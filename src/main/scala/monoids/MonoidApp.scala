package monoids

import monoids.MonoidOps._

/**
 * Created by richardgibson on 19/11/2016.
 */
object MonoidApp extends App {

  import MonoidImplicits._
  //  import AltMonoidImplicits._
  import MapMonoidImplicit._

  println(Monoid[String].combine("foo", "bar"))
  println(Monoid[Boolean].combine(true, false))
  println(Monoid[Int].combine(100, 50))
  val m1 = Map("foo" → 1, "bar" → 5)
  val m2 = Map("foo" → 2, "bar" → 3, "baz" → 9)

  println(Monoid[Map[String, Int]].combine(m1, m2))

  def add[A](items: List[A])(implicit m: Monoid[A]): A =
    items.foldLeft(m.empty)(m.combine)

  println(add(List(1, 2, 3, 4, 5, 6)))
  println(add(List("foo", "bar", "baz")))

  val m3 = Map("foo" → 20, "bar" → 1000, "bizz" → 100)

  println(add(List(m1, m2, m3)))

}
