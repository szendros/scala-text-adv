package game.subjects

import game._
import game.Subject
import cats.implicits._

case object LeltarID extends SubjectID

case class Leltar(
  items: Set[SubjectID]) extends Subject {

  val id = LeltarID
  val info = SubjectInfo(None, "")

  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      case AddMutation(_, x)    => Result(this.copy(items + x))
      case RemoveMutation(_, x) => Result(this.copy(items - x))
      case _                    => Result(this)
    }
}

object Leltar {
  def apply() = new Leltar(Set())
}