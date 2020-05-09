import cats.Show
import cats.implicits._

final case class Cat(name: String, age: Int, color: String)

implicit val showCat = Show.show[Cat] { cat =>
  val name = cat.name.show
  val age = cat.age.show
  val color = cat.color.show
  s"$name is a $age year old $color cat."
}

val c = Cat("Hilary",10,"black and white")
c.show