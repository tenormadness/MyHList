import scala.annotation.implicitNotFound
import scala.language.higherKinds

sealed trait HlistMayContain[H <: MyHList, U]

@implicitNotFound("Could not prove that the Hlist ${H} contains ${U}")
trait HContains[H <: MyHList, U]         extends HlistMayContain[H, U]
@implicitNotFound("Could not prove that the Hlist ${H} does not contain ${U}")
trait HDoesNotContain[H <: MyHList, U]   extends HlistMayContain[H, U]
//trait HContainsOnlyOnce[H <: MyHList, U] extends HlistMayContain[H, U] To be implemented... peter you want to give a crack to this?

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
    new HContains[Hhead[U, T], U] {}

  // recursive case
  implicit def UWasFound[U, H, T <: MyHList](implicit ev: HContains[T, U]) =
    new HContains[Hhead[H, T], U] {}

  // HDoesNotContain

  // trivial case
  implicit def NilDoesNotContain[U]: HDoesNotContain[HNil, U] =
    new HDoesNotContain[HNil, U] {}

  // recursive case
  implicit def extendingNotContain[U, A, T <: MyHList](implicit ev: HDoesNotContain[T, U], neq: U =!= A): HDoesNotContain[Hhead[A, T], U] =
    new HDoesNotContain[Hhead[A, T], U] {}

}



object TestConstratints extends App {
  import Constraints._

  //testing type is contained
  implicit class ContainChecker[H <: MyHList](in: H) {
    def contains[U](implicit ev: HContains[H, U]) =
      println(s"$in contains the selected type!")

    def notContain[U](implicit ev: HDoesNotContain[H, U]) =
      println(s"$in does not contain the selected type!")
  }
  def contains[H <: MyHList, U](in: H)(implicit ev: HContains[H, U]) =
    println(s"$in contains the selected type!")


  (1 #: HNil).contains[Int]

  ("!" #: 1 #: HNil).contains[Int]

  //("!" #: 1.0 #: HNil).contains[Int] //rightfully does not compile!


  (1 #: HNil).notContain[String]

  ("!" #: 1 #: HNil).notContain[Double]

  //("!" #: 1.0 #: HNil).notContain[Double] //rightfully does not compile!

}