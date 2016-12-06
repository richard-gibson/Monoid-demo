package monoids

import shapeless._

object MonoidOps {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) = monoid
    def instance[A](id: A)(cbn: (A, A) ⇒ A): Monoid[A] = new Monoid[A] {
      override def empty: A = id
      override def combine(x: A, y: A): A = cbn(x, y)
    }
  }

  case class Min[A](value: A)

  implicit def MaxMonoid[A: Ordering](implicit min: Min[A]) = new Monoid[A] {
    val ord = implicitly[Ordering[A]]
    def empty: A = min.value
    def combine(x: A, y: A): A =
      ord.compare(x, y) match {
        case i if i < 0 ⇒ y
        case i if i > 0 ⇒ x
        case _          ⇒ x
      }

  }

  object MapMonoidImplicit {

    implicit def MapMonoid[K, V: Monoid]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      val m = implicitly[Monoid[V]]
      override def empty: Map[K, V] = Map.empty

      override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] =
        x.toList ++ y.toList groupBy (_._1) map {
          case (k, v) ⇒ k → v.map(_._2)
            .foldLeft(m.empty)(m.combine)
        }
    }
  }

  object MonoidImplicits {

    implicit val BooleanAndMonoid = Monoid.instance[Boolean](false)(_ && _)
    implicit val AggStringMonoid = Monoid.instance[String]("")(_ + _)
    implicit val AggIntMonoid = Monoid.instance[Int](0)(_ + _)
    implicit val AggDoubleMonoid = Monoid.instance[Double](0.0)(_ + _)

  }

  object AltMonoidImplicits {

    implicit val BooleanOrMonoid = Monoid.instance[Boolean](false)(_ || _)
    implicit val AggStringMonoid = Monoid.instance[String]("")(_ + _)
    //Min[Int] will cause MaxMonoid[Int] to be used to combine ints
    implicit val minInt = Min(Int.MinValue)
    implicit val minDouble = Min(Double.MinValue)

  }

  object CaseClassImplicits {

    implicit def genMonoid[T, R](implicit
      gen: Generic.Aux[T, R],
      repr: Lazy[Monoid[R]]): Monoid[T] =
      Monoid.instance[T](gen.from(repr.value.empty))(
        (x, y) ⇒
          gen.from(repr.value.combine(gen.to(x), gen.to(y)))
      )

    implicit val hNilMonoid: Monoid[HNil] =
      Monoid.instance[HNil](HNil)((x, y) ⇒ HNil)

    implicit def hConsMonoid[H, T <: HList](implicit
      hdMonoid: Lazy[Monoid[H]],
      tlMonoid: Lazy[Monoid[T]]): Monoid[H :: T] =
      Monoid.instance[H :: T](hdMonoid.value.empty :: tlMonoid.value.empty)(
        (x, y) ⇒
          hdMonoid.value.combine(x.head, y.head) :: tlMonoid.value.combine(x.tail, y.tail)
      )

  }

}
