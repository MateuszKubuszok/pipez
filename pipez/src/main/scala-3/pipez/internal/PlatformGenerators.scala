package pipez.internal

import pipez.PipeDerivation
import pipez.internal.Definitions.{ Context, Result }

import scala.quoted.{ Type as _, * }

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
trait PlatformGenerators[Pipe[_, _], In, Out]
    extends Generators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] { self: PlatformDefinitions[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isSubtype[A: Type, B: Type]: Boolean =
    TypeRepr.of[A] <:< TypeRepr.of[B]

  final def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    report.info(diagnosticsMessage(result))

  final def reportError(errors: List[DerivationError]): Nothing =
    report.errorAndAbort(errorMessage(errors))

  final def lift[I: Type, O: Type](
    call: CodeOf[(I, Context) => Result[O]]
  ): CodeOf[Pipe[I, O]] = '{
    ${ pipeDerivation }.asInstanceOf[PipeDerivation.Aux[Pipe, Context, Result]].lift(${ call })
  }
  
  final def unlift[I: Type, O: Type](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  CodeOf[Context]
  ): CodeOf[Result[O]] = '{
    ${ pipeDerivation }.asInstanceOf[PipeDerivation.Aux[Pipe, Context, Result]].unlift(${ pipe }, ${ in }, ${ ctx })
  }

  final def updateContext(
    ctx:  CodeOf[Context],
    path: CodeOf[pipez.Path]
  ): CodeOf[Context] = '{
    ${ pipeDerivation }.asInstanceOf[PipeDerivation.Aux[Pipe, Context, Result]].updateContext(${ ctx }, ${ path })
  }

  final def pureResult[A: Type](a: CodeOf[A]): CodeOf[Result[A]] = '{
    ${ pipeDerivation }.asInstanceOf[PipeDerivation.Aux[Pipe, Context, Result]].pureResult(${ a })
  }

  final def mergeResults[A: Type, B: Type, C: Type](
    ra: CodeOf[Result[A]],
    rb: CodeOf[Result[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[Result[C]] = '{
    ${ pipeDerivation }.asInstanceOf[PipeDerivation.Aux[Pipe, Context, Result]].mergeResults(${ ra }, ${ rb }, ${ f })
  }
}
