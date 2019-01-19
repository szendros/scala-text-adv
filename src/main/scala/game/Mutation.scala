package game

trait Mutation {
  val subject : Option[SubjectID]
}

case class TestMutation(subject: Option[SubjectID]) extends Mutation
case class RemoveMutation(subject: Option[SubjectID], target: SubjectID) extends Mutation
case class AddMutation(subject: Option[SubjectID], target: SubjectID) extends Mutation
case class RelocateMutation(subject: Option[SubjectID]) extends Mutation
case class GameOverMutation(subject: Option[SubjectID]) extends Mutation