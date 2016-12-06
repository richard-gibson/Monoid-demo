import monoids.MonoidOps._

import MonoidImplicits._
//import AltMonoidImplicits._


Monoid[String].combine("foo", "bar")

Monoid[Int].combine(100, 50)
val intEmpty = Monoid[Int].empty
Monoid[Int].combine(100, intEmpty)


Monoid[Boolean].combine(true, false)
val boolEmpty = Monoid[Boolean].empty
Monoid[Boolean].combine(false, boolEmpty)


import MapMonoidImplicit._

val m1 = Map("foo" -> 1, "bar" -> 5)
val m2 = Map("foo" -> 2, "bar" -> 3, "baz" -> 9)
val m3 = Map("foo" -> 0, "bar" -> 1000, "bizz" -> 100)

Monoid[Map[String, Int]].combine(m1, m2)

def add[A](items: List[A])(implicit m: Monoid[A]): A =
  items.foldLeft(m.empty)(m.combine)


add(List(1,2,3,4,5,6))
add(List("foo","bar","baz"))
add(List(m1, m2, m3))


//combine any case class (Product) using shapeless Generic!!
import CaseClassImplicits._

case class Order(totalCost: Double, quantity: Int)
add(List(Order(1.0, 10), Order(34.0, 5), Order(1.0, 2)))


//nested case classes
case class Foo(id: Int, desc:String)
case class Bar(id:Int, foo: Foo)

add(List(Bar(10,Foo(1, "desc1")),Bar(20, Foo(2, "desc2"))))