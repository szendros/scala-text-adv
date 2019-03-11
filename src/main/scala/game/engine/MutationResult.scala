package game.engine

import cats._

case class MutationResult(
  messages:  List[String] = List(),
  mutations: List[Mutation] = List(),
  currentLocation: Option[SubjectID] = None,
  state: Option[GameState] = None
  )

object MutationResult {

  implicit val monoid = new Monoid[MutationResult] {
    def combine(a: MutationResult, b: MutationResult) =
      MutationResult(a.messages ++ b.messages, a.mutations ++ b.mutations, b.currentLocation.orElse(a.currentLocation), b.state.orElse(a.state))

    def empty = MutationResult(List(), List())
  }
}
