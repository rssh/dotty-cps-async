Runtime Await.
==============


Starting from JDK-19,  the JVM provides lightweight concurrency constructions for representing non-blocking computations in 
synchronous form. 

We can use those constructions for some monads, to escape the need to write shifted variants of high-order functions and,
in some cases, work without the cps transformation of the code inside the `async` macro.

To utilize these possibilities for your monad,  you should implement 
`CpsRuntimeAwait[F] <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsRuntimeAwait.scala>`_ typeclass  
(see also a  `LoomRuntimeAwait[F] <https://github.com/rssh/dotty-cps-async/blob/master/jvm/src/main/scala/cps/runtime/LoomRuntimeAwait.scala>`_ trait which can be helpful).  
If you want to escape cps transformations entirely, mark your implementation as 
`CpsFastRuntimeAwait[F] <https://github.com/rssh/dotty-cps-async/blob/c9d2ca09f1a456c6a27f5bc34287269de5672e2b/shared/src/main/scala/cps/CpsRuntimeAwait.scala#L26>`_. 
Also,  declare  `given cps.macro.flags.UseLoom.type`  value to enable this feature.

Note that for most effect systems, the cps transformation is still beneficial because of performance reasons. 
When processing runtime awaits,  an effect interpreter should start all code thunks, including synchronous API usage,  
in the new virtual thread. 
Therefore, an hybrid mode, where effect interpretation is based on non-blocking code, and the system starts a virtual thread
only for arguments of high-order functions, can be more performant.


