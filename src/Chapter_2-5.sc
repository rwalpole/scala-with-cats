import cats.Monoid
import cats.syntax.semigroup._

case class Order(totalCost: Double, quantity: Double)

object SuperAdder {
  def add(items: List[Int]): Int = {
    items.sum
  }
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)
}

implicit val orderMonoid: Monoid[Order] =
  new Monoid[Order] {
    override def empty = Order(0,0)

    override def combine(order1: Order, order2: Order):Order =
      Order(order1.totalCost + order2.totalCost,
        order1.quantity + order2.quantity)
  }

SuperAdder.add(List(1,2,3))

import cats.instances.int._
import cats.instances.option._
SuperAdder.add(List(Some(1),None,Some(2),None,Some(3)))
SuperAdder.add(List(Order(100,10),Order(200,5)))