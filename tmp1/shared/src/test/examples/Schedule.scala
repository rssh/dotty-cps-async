

object Schedule {

  val rooms = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  val days = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  val times = List("9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00")

  case class Course(name: String, duration: Int, room: String, day: String, time: String)



}