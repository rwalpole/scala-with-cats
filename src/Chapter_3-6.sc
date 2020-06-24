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

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      override def encode(value: B) = self.encode(enc(value))

      override def decode(value: String) = dec(self.decode(value))
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

implicit  val stringCodec: Codec[String] = new Codec[String] {
  override def encode(value: String) = value
  override def decode(value: String) = value
}

implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_),_.value)

encode(123.4)
decode[Double]("123.4")
encode(Box(123.4))
decode[Box[Double]]("123.4")