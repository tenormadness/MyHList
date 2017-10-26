import scala.annotation.implicitNotFound
import scala.language.higherKinds


object TestConstratints extends App {
  import Constraints._

  //testing type is contained
  implicit class ContainChecker[H <: MyHList](in: H) {
    def contains[U](implicit ev: HContains[H, U]) =
      println(s"$in contains the selected type!")

    def notContain[U](implicit ev: HDoesNotContain[H, U]) =
      println(s"$in does not contain the selected type!")

    def containsOnce[U](implicit ev: HContainsOnlyOnce[H, U]) =
      println(s"$in contains the selected type once and only once!")
  }

  (1 #: HNil).contains[Int]

  ("!" #: 1 #: HNil).contains[Int]

  //("!" #: 1.0 #: HNil).contains[Int] //rightfully does not compile!


  (1 #: HNil).notContain[String]

  ("!" #: 1 #: HNil).notContain[Double]

  //("!" #: 1.0 #: HNil).notContain[Double] //rightfully does not compile!

  /**
    * Test extract
    */
  val h1 = 1 #: HNil
  val easyInt = h1.extract[Int]
  println(s"I extracted $easyInt from $h1 !")

  val h2 = "!" #: 9.99 #: 5 #: 1 #: HNil
  val harderInt = h2.extract[Int]
  println(s"I extracted $harderInt from $h2 !")
  val harderDouble = h2.extract[Double]
  println(s"I extracted $harderDouble from $h2 !")

  //h2.extract[Boolean] //rightfully does not compile - no Boolean in list

  /**
    * Test containsOnce
    */
  (1 #: HNil).containsOnce[Int]
  ("foo" #: 1 #: HNil).containsOnce[Int]
  (1 #: "foo" #: HNil).containsOnce[Int]
  (1 #: 2 #: "hello" #: 5 #: 8 #: HNil).containsOnce[String]

  //(1 #: 2 #: HNil).containsOnce[Int] //rightfully does not compile - too many
  //(5.0 #: 1 #: "a" #: true #: 2 #: HNil).containsOnce[Int] //rightfully does not compile - too many
  //(1 #: 2 #: "b" #: HNil).containsOnce[Double] //rightfully does not compile - not found

  /**
    * Test extractSafe
    */
  val string1 = (1 #: 2 #: "hello" #: 5 #: 8 #: HNil).extractSafe[String]
  println(s"I safely extracted $string1!")

  val int1 = (9.9 #: 1 #: "foo" #: HNil).extractSafe[Int]
  println(s"I safely extracted $int1!")

  //(1 #: 2 #: "hello" #: 5 #: 8 #: HNil).extractSafe[Int] //rightfully does not compile - too many to decide what to extract
}