val m1 = Map("foo" -> 1, "bar" -> 5)
val m2 = Map("foo" -> 2, "bar" -> 3, "baz" -> 9)
val m3 = Map("foo" -> 0, "bar" -> 1000, "bizz" -> 100)


import cats._
import cats.implicits._


Monoid[String].combine("foo", "bar")

Monoid[Int].combine(100, 50)
val intEmpty = Monoid[Int].empty
Monoid[Int].combine(100, intEmpty)

100 |+| 50

"foo" |+| "bar" |+| "baz"

m1 |+| m2 |+| m3

Monoid[Option[Int]].combine(20.some, 30.some)

20.some |+| 30.some |+| none |+| 10.some

def add[A](items: List[A])(implicit m: Monoid[A]): A =
  items.foldLeft(m.empty)(m.combine)

add(List(1.some, 1.some, none, 7.some))

List(1.some, 1.some, none, 7.some).combineAll


case class Order(totalCost: Double, quantity: Int)

implicit object OrderMonoid extends  Monoid[Order] {
  override def empty: Order = Order(0.0,0)
  override def combine(x: Order, y: Order): Order =
    Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
}

implicit object OrderEq extends Eq[Order] {
  override def eqv(x: Order, y: Order): Boolean =
    x.totalCost == y.totalCost && x.quantity == y.quantity
}

Order(1.0, 10) |+| Order(34.0, 5) |+| Order(1.0, 2)


import cats.kernel.laws.GroupLaws
import org.scalacheck.Arbitrary

implicit def arbOrder(implicit ev: Arbitrary[(Double, Int)]): Arbitrary[Order] =
  Arbitrary(ev.arbitrary map(e => Order(e._1, e._2)))




GroupLaws[Order].monoid.all.check()