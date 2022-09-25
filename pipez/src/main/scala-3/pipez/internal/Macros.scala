package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }
import pipez.internal.Definitions.{ Context, Result }

import scala.quoted.{ Type as _, * }

// The way we are dispatching things here is a workaround that methods expanded in `inline` should be `object`

class MacrosImpl[Pipe[_, _], In, Out](q: Quotes)(
  override val Pipe:                     scala.quoted.Type[Pipe],
  override val In:                       scala.quoted.Type[In],
  override val Out:                      scala.quoted.Type[Out],
  pd:                                    Expr[PipeDerivation[Pipe]]
) extends PlatformDefinitions[Pipe, In, Out](using q)
    with PlatformGenerators[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] =
    TypeRepr.of(using Pipe).appliedTo(List(TypeRepr.of[I], TypeRepr.of[O])).asType.asInstanceOf[Type[Pipe[I, O]]]

  def derivePipe[Input: Type, Output: Type](settings: Settings): DerivationResult[Expr[Pipe[Input, Output]]] = {
    val m = new MacrosImpl(q)(
      Pipe = Pipe,
      In = typeOf[Input],
      Out = typeOf[Output],
      pd = pd
    )
    val filteredSettings = settings.stripSpecificsToCurrentDerivation.asInstanceOf[m.Settings]
    val result           = m.derive(filteredSettings).asInstanceOf[DerivationResult[Expr[Pipe[Input, Output]]]]
    result.fold(DerivationResult.pure)(errors =>
      DerivationResult.fail(DerivationError.RecursiveDerivationFailed(typeOf[Input], typeOf[Output], errors))
    )
  }

  val pipeDerivation: Expr[PipeDerivation.Aux[Pipe, Context, Result]] = {
    given p: scala.quoted.Type[Pipe] = Pipe
    '{ ${ pd }.asInstanceOf[PipeDerivation.Aux[Pipe, Context, Result]] }
  }
  val previewPipeDerivation: String = previewCode(pd)

  // Scala 3-macro specific instances, required because code-generation needs these types

  implicit override val Context: scala.quoted.Type[Context] = {
    given p: scala.quoted.Type[Pipe] = Pipe
    val tpe = '{ ${ pd }.updateContext(???, ???) }.asTerm.tpe
    tpe.asType.asInstanceOf[scala.quoted.Type[Context]]
  }
  implicit override val Result: scala.quoted.Type[Result] = {
    given p: scala.quoted.Type[Pipe] = Pipe
    val AppliedType(tpe, _) = '{ ${ pd }.pureResult(1) }.asTerm.tpe: @unchecked
    tpe.asType.asInstanceOf[scala.quoted.Type[Result]]
  }
}

object Macros {

  import scala.quoted.Type

  private def macros[Pipe[_, _], In, Out](
    pd:      Expr[PipeDerivation[Pipe]]
  )(using q: Quotes, pipeTypeConstructor: Type[Pipe], In: Type[In], Out: Type[Out]): MacrosImpl[Pipe, In, Out] =
    new MacrosImpl[Pipe, In, Out](q)(pipeTypeConstructor, In, Out, pd)

  /** Called with `${ pipez.internal.Macro.deriveDefault[Pipe, In, Out]('pd) }` */
  def deriveDefault[Pipe[_, _], In, Out](
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): Expr[Pipe[In, Out]] = macros[Pipe, In, Out](pd).deriveDefault

  /** Called with `macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]('config)('pd)` */
  def deriveConfigured[Pipe[_, _], In, Out](
    config: Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): Expr[Pipe[In, Out]] =
    macros[Pipe, In, Out](pd).deriveConfigured(config)
}
