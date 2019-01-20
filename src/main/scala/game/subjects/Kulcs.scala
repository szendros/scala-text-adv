package game.subjects

import game._
import game.Subject
import cats.implicits._
import game.Mutation._

case object KulcsID extends SubjectID

case class Kulcs(
  items: Set[SubjectID]) extends Subject {

  val id = KulcsID
  val info = SubjectInfo(None, "kulcs")

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("nézd") if cmd.hasOnly(id) => Result(this, msg("Egy kis kulcs, nincs rajta semmi jel."))
      case Some("vedd") => Result(this, msg("Felvetted a kulcsot.") |+| mut(RemoveMutation(None, id), AddMutation(Some(LeltarID), KulcsID)))
      case Some("tedd") if cmd.hasOnly(id) => Result(this, msg("Leteszed a kulcsot a földre.") |+| mut(RemoveMutation(Some(LeltarID), id), AddMutation(Some(data.currentLocation), KulcsID)))
      case _ => Result(this)
    }
}

object Kulcs {
  def apply() = new Kulcs(Set())
}