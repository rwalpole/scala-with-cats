trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      override def format(value: B): String = self.format(func(value))
    }
}

def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

implicit val stringPrintable: Printable[String] = new Printable[String] {
  override def format(value: String): String = "\"" + value + "\""
}

implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
  override def format(value: Boolean): String = if(value) "yes" else "no"
}

format("hello")

format(true)

final case class Box[A](value: A)

implicit def boxPrintable[A](implicit p: Printable[A]) =
  p.contramap[Box[A]](_.value)

format(Box("hello world"))
