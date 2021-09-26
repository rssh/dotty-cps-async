package cps.runtime

import cps._


/**
 * wrapper which can be used for translations of call-chains with
 *  given substitution methods.
 * The conventions, that shifted method can return such wrapper from
 * shifted function instead F[T] and then cps engine substitute calls of
 * chanined functions if they are implemented.
 **/
trait CallChainAsyncShiftSubst[F[_], T, FT]:

   /**
    * called when we have no calls in futher chain.
    **/
   def _finishChain:  FT 

   // and we assume, that for each method T which need substitution we will heve
   // appropriative method here, which accept same args and produce argument in monad.
   
