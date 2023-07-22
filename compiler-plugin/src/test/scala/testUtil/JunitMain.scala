package testUtil

import org.junit.internal.TextListener
import org.junit.runner.*

object JunitMain {

  def main(args: Array[String]): Unit = {
    println(s"junitMain, args=${args.toList}")
    val junit = new JUnitCore
    val classNameToRun = args(0)
    junit.addListener(new TextListener(System.out))
    val result = junit.run(Class.forName(classNameToRun))
    val failures = result.getFailures
    if (failures.isEmpty) {
      println("Ok")
    } else {
      println("Some tests failed")
      failures.forEach(failure => println(failure.toString))
    }
  }

}
