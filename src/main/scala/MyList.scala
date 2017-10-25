import scala.language.implicitConversions

sealed abstract class MyList[+T] {

  // We need to have a method to concatenate elements and build the list
  // note: methods that start with : are "magic"
  // def :+: deferred because ... because otherwise it will be hard later

  def head: T = this match {
    case MyNil => sys.error(s"empty list")
    case MyHead(h, t) => h
  }
}

sealed trait MyNil extends MyList[Nothing] {

  def :+:[TT](first: TT): MyHead[TT] = MyHead(first, MyNil)
}
case object MyNil extends MyNil

final case class MyHead[T](headd: T, tail: MyList[T]) extends MyList[T] {

  def :+:[X >: T](first: X): MyList[X] = MyHead(first, MyHead(headd, tail))

  override def toString = headd.toString + " :# " + tail.toString
}

object :+: {
  def unapply[T](hlist: MyHead[T]): Option[(T, MyList[T])] =
    Some((hlist.head, hlist.tail))
}


object Test extends App {

  // what is the type of these?
  val sametype = 0 :+: 1 :+: MyNil
  val foo = "one" :+: 1 :+: MyNil
  val bar = "one" :+: "two" :+: MyNil

  println(foo)

  foo match {
    case head :+: tail => println(s"head = $head, tail= $tail") //What is the type of head?
    case _ => sys.error(s"foo should have a head and a tail")
  }

  bar match {
    case head :+: mid :+: tail => println(s"head = $head, mid = $mid, tail= $tail") //what is the type of head? and Mid?
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
