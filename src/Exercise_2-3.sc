trait Semigroup[A] {
  def combine(x:A,y:A):A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}

implicit val booleanAndMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    override def empty = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

implicit val booleanOrMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    override def empty = false

    override def combine(x: Boolean, y: Boolean):Boolean = x || y
  }

implicit val booleanEitherMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    override def empty = false

    override def combine(x: Boolean, y: Boolean):Boolean = (x && !y) || (!x && y)
  }

implicit val booleanXnorMonoid: Monoid[Boolean] =
  new Monoid[Boolean] {
    override def empty = true

    override def combine(x: Boolean, y: Boolean):Boolean = (x || !y) && (!x || y)
  }

booleanAndMonoid.combine(true,true) // true
booleanAndMonoid.combine(true,false) // false
booleanAndMonoid.combine(false,false) // false

booleanOrMonoid.combine(true,true) // true
booleanOrMonoid.combine(true,false) // true
booleanOrMonoid.combine(false,false) // false

booleanEitherMonoid.combine(true,true) // false
booleanEitherMonoid.combine(true,false) // true
booleanEitherMonoid.combine(false,false) // false

booleanXnorMonoid.combine(true,true) // true
booleanXnorMonoid.combine(true,false)  //false
booleanXnorMonoid.combine(false,false) // true
