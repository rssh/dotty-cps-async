package cps

object X {

val r = ComputationBoundAsyncMonad.flatMap[Boolean, Int](T1.cbBool(true))(((v: Boolean) => {
  val x: Boolean = v
  val y: Int = 3
  ComputationBoundAsyncMonad.map[Int, Int](if (x) T1.cbi(2) else T1.cbi(3))(((v: Int) => {
    val z: Int = v
    y.+(z)
  }))
}))



}
