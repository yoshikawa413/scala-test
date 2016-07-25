object StatusTest extends App {
  val status01 = 1
  val status02 = 0 << 1
  val status03 = 1 << 2
  val status = status01 | status02 | status03

  println((status & 1) > 0)
  println((status & 1 << 1) > 0)
  println((status & 1 << 2) > 0)
}
