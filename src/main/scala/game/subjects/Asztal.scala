package game.subjects

import game._
import cats.implicits._
import game.Mutation._

case object AsztalID extends SubjectID

case class Asztal(
  items:   Set[SubjectID],
  eltolva: Boolean) extends Subject {

  val id = AsztalID
  val info = SubjectInfo(Some("kis"), "asztal")

  override def build() = List(Doboz())

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("tedd") if cmd.subjectIDs.contains(KulcsID) => Result(
        this.copy(eltolva = true, items = items + KulcsID),
        msg("Leteszed a kulcsot az asztalra.") |+| mut(RemoveMutation(None, KulcsID)))                
      case Some("told") => Result(this.copy(eltolva = true), msg("Sikeresen eltoltad az asztalt.") |+| mut(TestMutation(Some(AblakID))))
      case _            => Result(this)
    }

  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      case AddMutation(_, x)    => Result(this.copy(items + x))
      case RemoveMutation(_, x) => Result(this.copy(items - x))
      case _                    => Result(this)
    }
}

object Asztal {
  def apply() = new Asztal(Set(DobozID), eltolva = true)
}