// Contravariant in Cats
//trait Contravariant[F[_]] {
//  def contramap[A,B](fa: F[A])(f: B => A): F[B]
//}
//
//trait Invariant[F[_]] {
//  def imap[A,B](fa: F[A])(f: A => B): F[B]
//}



import cats.Contravariant
import cats.Show
import cats.instances.string._

val showString = Show[String]

val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"${sym.name}")

showSymbol.show('dave)

import cats.syntax.contravariant._
showString.contramap[Symbol](_.name).show('dave)

import cats.Monoid
import cats.syntax.invariant._
import

