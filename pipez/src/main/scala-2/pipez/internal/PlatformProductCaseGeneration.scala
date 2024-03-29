package pipez.internal

import pipez.internal.Definitions.{ Context, Result }
import pipez.internal.ProductCaseGeneration.inputNameMatchesOutputName

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.*
import scala.language.existentials

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait PlatformProductCaseGeneration[Pipe[_, _], In, Out]
    extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  final def isTuple[A: Type]: Boolean =
    typeOf[A].typeSymbol.fullName.startsWith("scala.Tuple")

  final def isCaseClass[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    val mem = typeOf[A].members
    sym.isClass && sym.asClass.isCaseClass && !sym.isAbstract && sym.asClass.primaryConstructor.isPublic
  }
  final def isCaseObject[A: Type]: Boolean = {
    val sym          = typeOf[A].typeSymbol
    def isScala2Enum = sym.asClass.isCaseClass
    def isScala3Enum = sym.isStatic && sym.isFinal // paramless case in S3 cannot be checked for "case"
    sym.isPublic && sym.isModuleClass && (isScala2Enum || isScala3Enum)
  }
  final def isJavaBean[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    val mem = typeOf[A].members
    sym.isClass && !sym.isAbstract && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetterOrVar)
  }

  private def isDefaultConstructor(ctor: Symbol): Boolean =
    ctor.isPublic && ctor.isConstructor && ctor.asMethod.paramLists.flatten.isEmpty

  private def isCaseClassField(field: Symbol): Boolean =
    field.isMethod && field.asMethod.isGetter && field.asMethod.isCaseAccessor

  private def isJavaGetter(getter: Symbol): Boolean =
    getter.isPublic &&
      getter.isMethod &&
      getter.asMethod.paramLists.flatten.isEmpty &&
      ProductCaseGeneration.isGetterName(getter.asMethod.name.toString)

  private def isJavaSetter(setter: Symbol): Boolean =
    setter.isPublic &&
      setter.isMethod &&
      setter.asMethod.paramLists.flatten.size == 1 &&
      ProductCaseGeneration.isSetterName(setter.asMethod.name.toString)

  private def isVar(setter: Symbol): Boolean =
    setter.isPublic && setter.isTerm && setter.asTerm.name.toString.endsWith("_$eq")

  private def isJavaSetterOrVar(setter: Symbol): Boolean =
    isJavaSetter(setter) || isVar(setter)

  final private case class Getter[Extracted, ExtractedField](
    tpe: Type[ExtractedField],
    get: Expr[Extracted] => Expr[ExtractedField]
  )
  private def extractGetters[Extracted: Type]: ListMap[String, Getter[Extracted, Any]] =
    typeOf[Extracted].decls
      .to(List)
      .filterNot(isGarbage)
      .filter(m => isCaseClassField(m) || isJavaGetter(m))
      .map { getter =>
        val name     = getter.name.toString
        val termName = getter.asMethod.name.toTermName
        name -> Getter[Extracted, Any](
          tpe = returnTypeOf(typeOf[Extracted], getter).asInstanceOf[Type[Any]],
          get =
            // macros distinct obj.method and obj.method()
            if (getter.asMethod.paramLists.isEmpty) (in: Expr[Extracted]) => c.Expr[Any](q"$in.$termName")
            else (in: Expr[Extracted]) => c.Expr[Any](q"$in.$termName()")
        )
      }
      .to(ListMap)

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] =
    extractGetters[In]
      .map { case (name, Getter(tpe, get)) =>
        name -> ProductInData.Getter(name, tpe, get, Path.Field(Path.Root, name))
      }
      .pipe(ProductInData(_))
      .pipe(DerivationResult.pure(_))
      .logSuccess(data => s"Resolved input: $data")

  private def providedFallbackValueByFields(settings: Settings): Map[String, Vector[FieldFallback[?]]] =
    settings.providedFallbackValues
      .collect { case ConfigEntry.AddFallbackValue(fallbackType, fallbackValue) =>
        extractGetters(fallbackType).map { case (name, Getter(fallbackFieldType, extractFallbackValue)) =>
          name -> FieldFallback.Value(fallbackFieldType, extractFallbackValue(fallbackValue))
        }.toVector
      }
      .flatten
      .groupMapReduce(_._1)(p => Vector(p._2))(_ ++ _)

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] =
    if (isJavaBean[Out]) {
      // Java Bean case

      val defaultConstructor = DerivationResult.fromOption(
        Out.decls.filterNot(isGarbage).find(isDefaultConstructor).map(_ => c.Expr[Out](q"new $Out()"))
      )(DerivationError.MissingPublicConstructor)

      val fallbackValues = providedFallbackValueByFields(settings) // Java Beans don't have fallbacks to defaults

      val setters = Out.decls
        .to(List)
        .filterNot(isGarbage)
        .filter(s => isJavaSetter(s) || isVar(s))
        .map { setter =>
          val name = setter.name.toString.pipe { n =>
            // Scala 3's JB setters _are_ methods ending with _= due to change in @BeanProperty behavior.
            // We have to drop that suffix to align names, so that comparing is possible.
            if (isVar(setter)) n.substring(0, n.length - "_$eq".length) else n
          }
          val termName = setter.asTerm.name.toTermName
          name -> ProductOutData.Setter(
            name = name,
            tpe = paramListsOf(Out, setter).flatten.head.typeSignature.asInstanceOf[Type[Any]],
            set = (out: Expr[Out], value: Expr[Any]) => c.Expr[Unit](q"$out.$termName($value)"),
            fallback = fallbackValues.view
              .collect {
                case (fallbackName, fallback)
                    if inputNameMatchesOutputName(fallbackName, name, settings.isFieldCaseInsensitive) =>
                  fallback
              }
              .flatten
              .toVector
          )
        }
        .to(ListMap)
        .pipe(DerivationResult.pure(_))

      defaultConstructor
        .map2(setters)(ProductOutData.JavaBean(_, _))
        .logSuccess(data => s"Resolved Java Bean output: $data")
    } else if (isCaseObject[Out]) {
      // case object case

      ProductOutData
        .CaseClass(
          caller = params => c.Expr(q"${Out.typeSymbol.asClass.module}"),
          params = List.empty
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case object output: $data")
    } else {
      // case class case

      (for {
        primaryConstructor <- DerivationResult.fromOption(
          Option(Out.typeSymbol).filter(_.isClass).map(_.asClass.primaryConstructor).filter(_.isPublic)
        )(DerivationError.MissingPublicConstructor)
        providedFallbackValues = providedFallbackValueByFields(settings)
          .asInstanceOf[Map[String, Vector[FieldFallback[Any]]]]
        // default value for case class field n (1 indexed) is obtained from Companion.apply$default$n
        defaultFallbackValues =
          primaryConstructor.typeSignature.paramLists.headOption.toList.flatten.zipWithIndex.collect {
            case (param, idx) if param.asTerm.isParamWithDefault =>
              param.name.toString -> FieldFallback.Default(
                c.Expr[Any](q"${Out.typeSymbol.companion}.${TermName("apply$default$" + (idx + 1))}")
              )
          }.toMap
        fallbackValues = (providedFallbackValues.keySet ++ defaultFallbackValues.keySet).map { fieldName =>
          // we want defaults to have lower priority than provided values
          fieldName -> (providedFallbackValues
            .getOrElse(fieldName, Vector.empty) ++ defaultFallbackValues.get(fieldName).toVector)
        }
      } yield ProductOutData.CaseClass(
        caller = params => c.Expr(q"new $Out(...$params)"),
        params = paramListsOf(Out, primaryConstructor).map { params =>
          params
            .map { param =>
              val name = param.name.toString
              name -> ProductOutData.ConstructorParam(
                name = name,
                tpe = param.typeSignature.asInstanceOf[Type[Any]],
                fallback = fallbackValues.view
                  .collect {
                    case (fallbackName, fallback)
                        if inputNameMatchesOutputName(fallbackName, name, settings.isFieldCaseInsensitive) =>
                      fallback
                  }
                  .flatten
                  .toVector
              )
            }
            .to(ListMap)
        }
      )).logSuccess(data => s"Resolved case class output: $data")
    }

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    generatorData match {
      case ProductGeneratorData.CaseClass(caller, results)            => generateCaseClass(caller, results)
      case ProductGeneratorData.JavaBean(defaultConstructor, results) => generateJavaBean(defaultConstructor, results)
    }

  private def generateCaseClass(
    constructor:          Constructor,
    outputParameterLists: List[List[ProductGeneratorData.OutputValue]]
  ): DerivationResult[Expr[Pipe[In, Out]]] = {
    val paramToIdx: Map[ProductGeneratorData.OutputValue.Result[?], Constant] = outputParameterLists.flatten
      .collect { case result: ProductGeneratorData.OutputValue.Result[?] => result }
      .zipWithIndex
      .map { case (result, idx) => result -> Constant(idx) }
      .toMap

    def constructorParams(in: Expr[In], ctx: Expr[Context], arr: Expr[Array[Any]]): List[List[Expr[?]]] =
      outputParameterLists.map(
        _.map {
          case ProductGeneratorData.OutputValue.Pure(_, caller) =>
            caller(in, ctx)
          case r @ ProductGeneratorData.OutputValue.Result(tpe, _) =>
            c.Expr(q"""$arr(${paramToIdx(r)}).asInstanceOf[$tpe]""")
        }
      )

    val arrSize = Constant(paramToIdx.size)
    val initialValue: Expr[Result[Array[Any]]] = pureResult(c.Expr(q"scala.Array.ofDim[scala.Any]($arrSize)"))

    @scala.annotation.tailrec
    def generateBody(
      in:          Expr[In],
      ctx:         Expr[Context],
      arrayResult: Expr[Result[Array[Any]]],
      params:      List[(ProductGeneratorData.OutputValue.Result[?], Constant)]
    ): Expr[Result[Out]] =
      params match {
        // all values are taken directly from input and wrapped in Result
        case Nil =>
          pureResult(constructor(constructorParams(in, ctx, null)))

        // last param - after adding the last value to array we extract all values from it into constructor
        case (param, idx) :: Nil =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]

          val left  = c.freshName(TermName("left"))
          val right = c.freshName(TermName("right"))
          val fun: Expr[(Array[Any], Any) => Out] = c.Expr[(Array[Any], Any) => Out](
            q"""
             ($left : scala.Array[scala.Any], $right : ${param.tpe}) => {
               $left($idx) = $right
               ${constructor(constructorParams(in, ctx, c.Expr[Array[Any]](q"$left")))}
             }
             """
          )

          mergeResults(ctx, arrayResult, rightCode, fun)

        // we combine Array's Result with a param's Result, store param in array and iterate further
        case (param, idx) :: tail =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]

          val left  = c.freshName(TermName("left"))
          val right = c.freshName(TermName("right"))
          val fun = c.Expr[(Array[Any], Any) => Array[Any]](
            q"""
            ($left : scala.Array[scala.Any], $right : ${param.tpe}) => {
              $left($idx) = $right
              $left
            }
            """
          )

          generateBody(in, ctx, mergeResults(ctx, arrayResult, rightCode, fun), tail)
      }

    val body: Expr[Pipe[In, Out]] = {
      val in  = c.freshName(TermName("in"))
      val ctx = c.freshName(TermName("ctx"))
      lift[In, Out](
        c.Expr[(In, Context) => Result[Out]](
          q"""
          ($in : $In, $ctx : $Context) =>
            ${generateBody(c.Expr[In](q"$in"), c.Expr[Context](q"$ctx"), initialValue, paramToIdx.toList)}
          """
        )
      )
    }

    DerivationResult
      .pure(body)
      .log(s"Case class derivation, constructor params: $outputParameterLists")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }

  private def generateJavaBean(
    defaultConstructor: Expr[Out],
    outputSettersList:  List[(ProductGeneratorData.OutputValue, ProductOutData.Setter[?])]
  ): DerivationResult[Expr[Pipe[In, Out]]] = {
    def pureValues(in: Expr[In], ctx: Expr[Context], result: Expr[Out]): List[Expr[Unit]] =
      outputSettersList.collect { case (ProductGeneratorData.OutputValue.Pure(_, caller), setter) =>
        setter.asInstanceOf[ProductOutData.Setter[Any]].set(result, caller(in, ctx))
      }

    val resultValues: List[(ProductGeneratorData.OutputValue.Result[?], ProductOutData.Setter[?])] =
      outputSettersList.collect { case (r: ProductGeneratorData.OutputValue.Result[?], s: ProductOutData.Setter[?]) =>
        r -> s
      }

    def initialValue(in: Expr[In], ctx: Expr[Context]): Expr[Result[Out]] = {
      val result = c.freshName(TermName("result"))
      pureResult(
        c.Expr[Out](
          q"""
          {
            val $result: $Out = $defaultConstructor
            ..${pureValues(in, ctx, c.Expr[Out](q"$result"))}
            $result
          }
          """
        )
      )
    }

    @scala.annotation.tailrec
    def generateBody(
      in:        Expr[In],
      ctx:       Expr[Context],
      outResult: Expr[Result[Out]],
      params:    List[(ProductGeneratorData.OutputValue.Result[?], ProductOutData.Setter[?])]
    ): Expr[Result[Out]] =
      params match {
        // all values are taken directly from input and wrapped in Result
        case Nil =>
          outResult

        // we have Out object on left and value to put into setter on right
        case (param, setter) :: tail =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]

          val left  = c.freshName(TermName("left"))
          val right = c.freshName(TermName("right"))
          val fun = c.Expr[(Out, Any) => Out](
            q"""
            ($left : $Out, $right : ${param.tpe}) => {
              ${setter.asInstanceOf[ProductOutData.Setter[Any]].set(c.Expr(q"$left"), c.Expr(q"$right"))}
              $left
            }
            """
          )

          generateBody(in, ctx, mergeResults(ctx, outResult, rightCode, fun), tail)
      }

    val body: Expr[Pipe[In, Out]] = {
      val in  = c.freshName(TermName("in"))
      val ctx = c.freshName(TermName("ctx"))
      lift[In, Out](
        c.Expr[(In, Context) => Result[Out]](
          q"""
          ($in : $In, $ctx : $Context) =>
            ${generateBody(c.Expr[In](q"$in"),
                           c.Expr[Context](q"$ctx"),
                           initialValue(c.Expr[In](q"$in"), c.Expr[Context](q"$ctx")),
                           resultValues
            )}
          """
        )
      )
    }

    DerivationResult
      .pure(body)
      .log(s"Java Beans derivation, setters: $outputSettersList")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }
}
