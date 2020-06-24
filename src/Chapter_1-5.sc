import cats.Eq
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import cats.syntax.eq._

/*
=== compares two objects for equality
=!= compares two objects for inequality
 */
final case class Cat(name: String, age: Int, color: String)

implicit val catEq: Eq[Cat] =
  Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name === cat2.name) && (cat2.age === cat2.age) && (cat1.color === cat2.color)
  }

val cat1 = Cat("Hilary",10,"Black & white")
val cat2 = Cat("Cyril",8,"Ginger")
val cat3 = Cat("Cyril",8,"Ginger")

cat1 === cat2
cat2 === cat3

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]
optionCat1 === optionCat2
optionCat1 =!= optionCat2
