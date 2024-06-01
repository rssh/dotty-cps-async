package injection

@main
def main(): Unit = {
  given Int = 2
  val t: ((Int => Boolean) => Boolean) = injectfull {
    inject[Int => Boolean](inject[Int])
  }
  println(t(_ % 2 == 0))

  def test[A](using A): Unit = injectfull {
    val a = inject[A]
    println(a)
  }
  print(test[Int])

}
