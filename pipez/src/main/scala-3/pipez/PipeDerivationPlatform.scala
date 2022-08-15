package pipez

trait PipeDerivationPlatform { self: PipeDerivation.type =>

  inline def derive[Pipe[_, _], In, Out](using
    inline pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pd }) }

  inline def derive[Pipe[_, _], In, Out](
    inline config: PipeDerivationConfig[Pipe, In, Out]
  )(using
    inline pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveConfigured[Pipe, In, Out]('{ config })('{ pd }) }
}
