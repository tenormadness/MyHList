import scala.annotation.implicitNotFound
import scala.language.higherKinds

sealed trait HlistMayContain[H <: MyHList, U]

@implicitNotFound("Could not prove that the Hlist ${H} contains ${U}")
trait HContains[H <: MyHList, U]         extends HlistMayContain[H, U] { def extract(in: H): U }
@implicitNotFound("Could not prove that the Hlist ${H} does not contain ${U}")
trait HDoesNotContain[H <: MyHList, U]   extends HlistMayContain[H, U]
@implicitNotFound("Could not prove that the Hlist ${H} contains ${U} only once")
trait HContainsOnlyOnce[H <: MyHList, U] extends HlistMayContain[H, U]

object Constraints {

  // bolier plate for context bound notation
  type With[U] = {
    type NotContained[H <: MyHList] = HDoesNotContain[H, U]
    type Contained[H <: MyHList] = HContains[H, U]
    //  type ContainedOnce[H <: MyHList] = HContainsOnce[H, U]  not yet implemented
  }


  //HContains:

  // trivial case
  implicit def UIsFound[U, T <: MyHList]: HContains[Hhead[U, T], U] =
    new HContains[Hhead[U, T], U] {
      override def extract(in: Hhead[U, T]): U = in.head
    }

  // recursive case
  implicit def UWasFound[U, H, T <: MyHList](implicit ev: HContains[T, U]): HContains[Hhead[H, T], U] =
    new HContains[Hhead[H, T], U] {
      override def extract(in: Hhead[H, T]): U = ev.extract(in.tail)
    }

  // HDoesNotContain

  // trivial case
  implicit def NilDoesNotContain[U]: HDoesNotContain[HNil, U] =
    new HDoesNotContain[HNil, U] {}

  // recursive case
  implicit def extendingNotContain[U, A, T <: MyHList](implicit ev: HDoesNotContain[T, U], neq: U =!= A): HDoesNotContain[Hhead[A, T], U] =
    new HDoesNotContain[Hhead[A, T], U] {}

  // extractor interface
  implicit class Extractor[T <: MyHList](in: T) {
    def extract[A](implicit extractor: HContains[T, A]): A = extractor.extract(in)
  }

  //HContainsOnlyOnce:
  // trivial case
  implicit def UIsAloneWithNil[U, T <: MyHList]: HContainsOnlyOnce[Hhead[U, HNil], U] = {
    new HContainsOnlyOnce[Hhead[U, HNil], U] {}
  }

  // recursive case, current head not U
  implicit def UWasFoundOnce[U, H, T <: MyHList](implicit ev: HContainsOnlyOnce[T, U], neq: U =!= H): HContainsOnlyOnce[Hhead[H, T], U] =
    new HContainsOnlyOnce[Hhead[H, T], U] {}

  // current head is U
  implicit def UIsFoundOnce[U, H, T <: MyHList](implicit ev: HDoesNotContain[T, U]): HContainsOnlyOnce[Hhead[U, T], U] =
    new HContainsOnlyOnce[Hhead[U, T], U] {}

}


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

}