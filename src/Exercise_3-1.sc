import cats.Functor

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]
object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}

object TreeFunctor {
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
        tree match {
          case Branch(left,right) =>
            Branch(map(left)(func),map(right)(func))
          case Leaf(value) =>
            Leaf(func(value))
        }
    }
}

import TreeFunctor.treeFunctor



Tree.leaf(200).map  //.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)