package game.engine

import cats.kernel.Monoid

case class Result[A,B] (
  item: A,
  result:B
)

object Result {
  def apply[A, B](item: A)(implicit monoid: Monoid[B]) = new Result[A, B](item,  monoid.empty)  
}