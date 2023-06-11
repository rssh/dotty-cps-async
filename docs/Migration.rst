Migration from previous versions.
=================================

0.9.17
-------


1. Custom CpsMonad-s:  `CpsMonadInstanceContext` is deprecated. Use instead one of more specific instances: `CpsTryMonadInstanceContext` for monads with full try/catch support,
`CpsThrowMonadInstanceContext` for throw only or `CosPureMonadInstanceContext` for flat/FlatMaps.
`CpsMonadInstanceContext` now is an alias for `CpsTryMonadInstanceContext`.  This will cause compiletime error, when your monad is not derived from `CpsTryMonad`.

2.  For custom contexts:  'adoptAwait' method is deprecated.
Instead, context should provide `monad` method, which returns CpsMonad instance,  which can be wrapped into the context-specific action.
This is needed to simplicity of await handling and unify API with context-encoding direct style (where we have no awaits at all).


0.9.16
-------


