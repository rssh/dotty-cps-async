package cps

object X {

val r = ComputationBoundAsyncMonad.flatMap(T1.cbBool(true))(((v: Boolean) => {
  val x: Boolean = v
  ();
  val y: Int = 3
  ();
  ComputationBoundAsyncMonad.map(T1.cbi(2))(((v: Int) => {
    val z = v
    y+z
  }))
}))



}
