package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.quoted.{ Type as _, * }

// The way we are dispatching things here is a workaround that methods expanded in `inline` should be `object`

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
class MacrosImpl[Pipe[_, _], In, Out](q: Quotes)(
  pd:                                    Expr[PipeDerivation[Pipe]],
  override val Pipe:                     scala.quoted.Type[Pipe],
  override val In:                       scala.quoted.Type[In],
  override val Out:                      scala.quoted.Type[Out]
) extends PlatformDefinitions[Pipe, In, Out](using q)
    with PlatformGenerators[Pipe, In, Out] { self =>

  import quotes.*
  import quotes.reflect.*

  def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] =
    TypeRepr.of(using Pipe).appliedTo(List(TypeRepr.of[I], TypeRepr.of[O])).asType.asInstanceOf[Type[Pipe[I, O]]]

  val pipeDerivation: CodeOf[PipeDerivation[Pipe] { type Context = self.Context; type Result[O] = self.Result[O] }] =
    pd.asInstanceOf[CodeOf[PipeDerivation[Pipe] { type Context = self.Context; type Result[O] = self.Result[O] }]]
}

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
object Macros {

  import scala.quoted.Type

  private def macros[Pipe[_, _], In, Out](
    pd:      Expr[PipeDerivation[Pipe]]
  )(using q: Quotes, pipeTypeConstructor: Type[Pipe], In: Type[In], Out: Type[Out]): MacrosImpl[Pipe, In, Out] =
    new MacrosImpl[Pipe, In, Out](q)(pd, pipeTypeConstructor, In, Out)

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