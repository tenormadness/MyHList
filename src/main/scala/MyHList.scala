import scala.language.implicitConversions

sealed abstract class MyHList {
  // def #: deferred beacuse I want very fine detail about the result type
}

sealed trait HNil extends MyHList {

  def #:[TT](first: TT): Hhead[TT, HNil] = Hhead(first, HNil)
}
case object HNil extends HNil

final case class Hhead[A, T <: MyHList](head:A, tail: T) extends MyHList {
  type elem = A

  def #:[TT](first: TT): Hhead[TT, Hhead[A, T]] = Hhead(first, Hhead(head, tail))
}

object #: {
  def unapply[A, T <: MyHList](hlist: Hhead[A, T]): Option[(A, T)] =
    Some((hlist.head, hlist.tail))
}


object Test extends App {

  val foo = "one" #: 1 #: HNil
  val bar = "one" #: "two" #: HNil

  println(foo)

  foo match {
    case head #: tail => println(s"head = $head, tail= $tail")
    case _ => println("found nothing :(")
  }

  bar match {
    case head #: mid #: tail => println(s"head = $head, mid = $mid, tail= $tail")
    case _ => println("found nothing :(")
  }
}
