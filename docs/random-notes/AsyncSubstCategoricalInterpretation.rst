
.. _categorical-interpretation-for-CallChainAsyncSubst:

Categorical interpretation for substitutions in async call chains:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Substitutions in async call chains is described in :ref:`substitutions-in-call-chains`.


Let's look at the folowing diagram:

.. image:: diagrams/AsyncSubst3.png

Let we have hight-order function :math:`f: A\to B` (example: :math:`f = withFilter: Iterable \to WithFilter`); then direct async transform will give us an async variant of :math:`f` -- :math:`f_{async}`  (example:  naive implementation of `withFilter_async`), and substituted is :math:`f'_{async}` (example - our delayed implementation of `withFiter_async`). [#]_.

.. [#] Here we write `AsyncSubst` instead `CallChainAsyncSubst` to save space on the picture.
  

For any function :math:`h: B->C`  from `B` to any `C`,  we have an image of `h` in `F[B]`: :math:`h_F = F.map(h)` and in AsyncSubst: :math:`h_{AsyncShift}`.
The diagram is commutative: 
* :math:`h_{F} * f_{async} = h_{AsyncSubst} * f'_{async}` . (Example - `h` is `flatMap`, our implementation performs the same operation as usual flatMap, but in one batch with filtering).
* :math:`\_finishChain * h'_{AsyncSubst}  = h_{AsyncSubst}` . (Example - `h` is `withFilter`  :math:`h'_{AsyncSubst}` is an implementation construct an next delayed instance with two predicates ).  

Moving to relations between functors, shown on the next diagram:

.. image:: diagrams/AsyncSubstFunctors.png

We can notice that `_finishChain` is a left Kan extension of `F` along `AsyncSubst`.  

This technique can be applied for cases where direct cps transform is impossible.  
  Example: cps transform of functional expression: `cps[S=>T]`, will give us not `F[S=>T]`,  but `S=>F[T]`.  We can't receive `F[T=>S]` from `T=>F[S]`, but can define an `AsynSubst[T=>S]` with implementation of `apply`, `andThen` and `compose`.


  

