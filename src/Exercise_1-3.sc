trait Printable[A] {
  def format(a: A): String
}

final case class Cat(name: String, age: Int, color: String)

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)
  def print[A](a: A)(implicit p: Printable[A]): Unit = println(p.format(a))
}

object PrintableInstances {

  implicit val printableString: Printable[String] = new Printable[String] {
    override def format(a: String): String = a
  }

  implicit val printableInt: Printable[Int] = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }

  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(c: Cat): String = {
      val name = Printable.format(c.name)
      val age = Printable.format(c.age)
      val color = Printable.format(c.color)
      s"$name is a $age year old $color cat."
    }
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit p: Printable[A]):String = Printable.format(a)
    def print(implicit p: Printable[A]):Unit = Printable.print(a)
  }
}

import PrintableInstances._
import PrintableSyntax._

Printable.format(2)
Printable.format("hello")

val c = Cat("Hilary",10,"black and white")
Printable.print(c)
c.print









