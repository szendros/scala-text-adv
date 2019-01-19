package game.subjects

import game._
import game.Subject
import cats.implicits._
import game.MutationOps._

case object KulcsID extends SubjectID

case class Kulcs(
  items: Set[SubjectID]) extends Subject {

  val id = KulcsID
  val info = SubjectInfo(None, "kulcs")

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("nézd") =>
        Result(this, msg("Egy kis kulcs, nincs rajta semmi jel."))
      case Some("vedd") => Result(this, mut("Felvetted a kulcsot.", 
          RemoveMutation(None, id), AddMutation(Some(LeltarID), KulcsID)))
      case _            => Result(this)
    }
}

object Kulcs {
  def apply() = new Kulcs(Set())
}