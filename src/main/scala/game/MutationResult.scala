package game

import cats.kernel.Monoid

case class MutationResult(
  messages:  List[String],
  mutations: List[Mutation]
  )

object MutationResult {

  def apply(messages: List[String]) = new MutationResult(messages, List())
  def apply() = new MutationResult(List(), List())
  
  implicit val monoid = new Monoid[MutationResult] {
    def combine(a: MutationResult, b: MutationResult) =
      MutationResult(a.messages ++ b.messages, a.mutations ++ b.mutations)

    def empty = MutationResult(List(), List())
  }
}
