package game.engine

import scala.Right

import game.WithError

sealed trait Mutation {
  val subject : Option[SubjectID]
}

case class TestMutation(subject: Option[SubjectID]) extends Mutation
case class RemoveMutation(subject: Option[SubjectID], target: SubjectID) extends Mutation
case class AddMutation(subject: Option[SubjectID], target: SubjectID) extends Mutation
case class RelocateMutation(subject: Option[SubjectID]) extends Mutation
case class GameOverMutation(subject: Option[SubjectID]) extends Mutation
case class EventMutation(subject: Option[SubjectID]) extends Mutation

object Mutation {
  
  def msg(message: String*): WithError[MutationResult] =
    Right(MutationResult(message.toList))

  def mut(mutation: Mutation*): WithError[MutationResult] =
    Right(MutationResult(List(), mutation.toList))

  def state(state: GameState): WithError[MutationResult] =
    Right(MutationResult(state = Some(state)))

  def loc(currentLocation: SubjectID): WithError[MutationResult] =
    Right(MutationResult(currentLocation = Some(currentLocation)))
  
}