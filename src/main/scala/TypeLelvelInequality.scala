import scala.annotation.implicitNotFound

/** A somewhat mistrious file that implements typelevel inequality
  *
  * pretty much copy pasted from shapeless, why it works is actually a bit mysterious to me (Luca)
  *
  * @tparam A A type that should not be B
  * @tparam B A tyep that should not be A
  */

@implicitNotFound("Could not prove that type ${A} is different from ${B}")
sealed class =!=[A,B]

sealed trait LowerPriorityImplicits {
  /** do not call explicitly! */
  implicit def equal[A]: =!=[A, A] = sys.error(s"Should never happen")
}
object =!= extends LowerPriorityImplicits {
  /** do not call explicitly! */
  implicit def nequal[A,B]: =!=[A,B] = new =!=[A,B]
}




object TestTyepInequality extends App {
  // testing type inequality
  private def neqType[A, B](a:A, b:B)(implicit neq: A =!= B) = println(s"$a is different from $b")

  neqType(1, 1.0)
  neqType(1, "one")
  //neqType(1, 2) //does not compile

}