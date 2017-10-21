import scala.language.implicitConversions

sealed abstract class MyList[+T] {
  // def #: deferred beacuse I want very fine detail about the result type
}

sealed trait MyNil extends MyList[Nothing] {

  def :+:[TT](first: TT): MyHead[TT] = MyHead(first, MyNil)
}
case object MyNil extends MyNil

final case class MyHead[T](head:T, tail: MyList[T]) extends MyList[T] {

  def :+:[X >: T](first: X) = MyHead(first, MyHead(head, tail))

  override def toString = head.toString + " :# " + tail.toString
}

object :+: {
  def unapply[T](hlist: MyHead[T]): Option[(T, MyList[T])] =
    Some((hlist.head, hlist.tail))
}


object Test extends App {

  val sametype = 0 :: 1 :: Nil
  val foo = "one" :+: 1 :+: MyNil
  val bar = "one" :+: "two" :+: MyNil

  println(foo)

  foo match {
    case head :+: tail => println(s"head = $head, tail= $tail")
    case _ => println("found nothing :(")
  }

  bar match {
    case head :+: mid :+: tail => println(s"head = $head, mid = $mid, tail= $tail")
    case _ => println("found nothing :(")
  }


  // NOTE THAT THIS DOES NOT WORK!
  //
  //val baz = 1 :: "one" :: Nil
  def intOnlyFunction(i: Int) = i + 1
  //val bazp1 = intOnlyFunction(bazz.head)
  //assert(bazp1 == 2)

  // .. but this does
  assert(1 == intOnlyFunction(sametype.head))

}
