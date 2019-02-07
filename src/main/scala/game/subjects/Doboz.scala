package game.subjects

import game._
import game.Subject
import cats.implicits._
import game.Mutation._

case object DobozID extends SubjectID

case class Doboz(
  items:    Set[SubjectID],
  kinyitva: Boolean) extends Subject {

  val id = DobozID
  val info = SubjectInfo(None, "doboz")

  override val build = List(Kulcs())

  override val visibleItems = items filter (_ => kinyitva)

  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      case AddMutation(_, x)    => Result(this.copy(items + x))
      case RemoveMutation(_, x) => Result(this.copy(items - x), msg("alma") |+| state(Finished))
      case _                    => Result(this)
    }
}

object Doboz {
  def apply() = new Doboz(Set(KulcsID), kinyitva = true)
}