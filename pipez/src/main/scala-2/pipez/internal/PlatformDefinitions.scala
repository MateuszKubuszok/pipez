package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.{ nowarn, unused }
import scala.reflect.macros.blackbox

trait PlatformDefinitions[Pipe[_, _], In, Out] extends Definitions[Pipe, In, Out] {

  val c: blackbox.Context

  import c.universe.*

  override type Type[@unused A] = c.Type

  override type Argument[@unused A] = TermName
  override type CodeOf[A]           = Expr[A]

  final val inCode: Argument[In] => CodeOf[In] =
    id => c.Expr[In](q"$id")

  final def previewCode[A](code: c.universe.Expr[A]): String = showCode(code.tree)

  final def summonPipe[Input, Output](
    inputType:  Type[Input],
    outputType: Type[Output]
  ): DerivationResult[CodeOf[Pipe[Input, Output]]] =
    DerivationResult
      .unsafe(c.Expr[Pipe[Input, Output]](c.inferImplicitValue(pipeType(inputType, outputType), silent = false)))(_ =>
        DerivationError.RequiredImplicitNotFound(inputType, outputType)
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  final def singleAbstractMethodExpansion[SAM](tpe: Type[SAM], code: CodeOf[SAM]): CodeOf[SAM] =
    c.Expr(q"_root_.scala.Predef.identity[$tpe]($code)")

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] = {
    @nowarn("cat=unused")
    def extractPath(in: Tree): Either[String, Path] = in match {
      case Function(_, expr)                          => extractPath(expr)
      case Select(expr, TermName(field))              => extractPath(expr).map(Path.Field(_, field)) // extract .field
      case Apply(Select(expr, TermName(get)), List()) => extractPath(expr).map(Path.Field(_, get)) // extract .getField
      case Ident(TermName(_))                         => Right(Path.Root) // drop argName from before .field
      case _                                          => Left(s"Path ${showCode(in)} is not in format _.field1.field2")
    }

    def extract(tree: Tree, acc: List[ConfigEntry]): Either[String, Settings] = tree match {
      // matches PipeDerivationConfig[Pipe, In, Out]
      case TypeApply(Select(Ident(cfg), TermName("apply")), _) if cfg.decodedName.toString == "PipeDerivationConfig" =>
        Right(new Settings(acc))
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, TermName("enableDiagnostics")) =>
        extract(expr, ConfigEntry.EnableDiagnostics :: acc)
      // matches {cfg}.addField(in, out)
      case Apply(TypeApply(Select(expr, TermName("addField")), _), List(outputField, pipe)) =>
        for {
          outFieldPath <- extractPath(outputField)
          TypeRef(_, _, List(_, outFieldType)) = outputField.tpe
          result <- extract(
            expr,
            ConfigEntry.AddField(outFieldPath,
                                 outFieldType,
                                 singleAbstractMethodExpansion(pipeType[In, Any](inType, outFieldType), c.Expr(pipe))
            ) :: acc
          )
        } yield result
      // matches {cfg}.renameField(in, out)
      case Apply(TypeApply(Select(expr, TermName("renameField")), _), List(inputField, outputField)) =>
        for {
          inPath <- extractPath(inputField)
          outPath <- extractPath(outputField)
          result <- extract(
            expr,
            ConfigEntry.RenameField(inPath, inputField.tpe.resultType, outPath, outputField.tpe.resultType) :: acc
          )
        } yield result
      // matches {cfg}.plugIn(in, out)
      case Apply(TypeApply(Select(expr, TermName("plugInField")), _), List(inputField, outputField, pipe)) =>
        for {
          inFieldPath <- extractPath(inputField)
          outFieldPath <- extractPath(outputField)
          TypeRef(_, _, List(_, inFieldType))  = inputField.tpe
          TypeRef(_, _, List(_, outFieldType)) = outputField.tpe
          result <- extract(
            expr,
            ConfigEntry.PlugInField(
              inFieldPath,
              inFieldType,
              outFieldPath,
              outFieldType,
              singleAbstractMethodExpansion(pipeType[Any, Any](inFieldType, outFieldType), c.Expr(pipe))
            ) :: acc
          )
        } yield result
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, TermName("fieldMatchingCaseInsensitive")) =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      // TODO: removeSubtype
      // TODO: renameSubtype
      // TODO: enumMatchingCaseInsensitive
      case _ =>
        Left(s"${previewCode(code)} is not a right PipeDerivationConfig")
    }

    DerivationResult.fromEither(extract(code.tree, Nil))(DerivationError.InvalidConfiguration(_))
  }
}
