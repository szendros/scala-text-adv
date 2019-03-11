package game.subjects

import cats.implicits._

import game.engine._
import game.engine.Mutation._

case object KulcsID extends SubjectID

case class Kulcs(
  items: Set[SubjectID]) extends Subject {

  val id = KulcsID
  val info = SubjectInfo(None, "kulcs")

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("nézd") => Result(this, msg("Egy kis kulcs, nincs rajta semmi jel."))
      case Some("vedd") if cmd.hasOnly(id) => Result(this, msg("Felvetted a kulcsot.") |+| mut(RemoveMutation(None, id), AddMutation(Some(AvatarID), KulcsID)))
      case Some("tedd") if cmd.hasOnly(id) => Result(this, msg("Leteszed a kulcsot a földre.") |+| mut(RemoveMutation(Some(AvatarID), id), AddMutation(Some(data.currentLocation), KulcsID)))
      case _                               => Result(this)
    }

  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      //case EventMutation(_) => Result(this, msg("Telik az idő."))
      case _                => Result(this)
    }
}

object Kulcs {
  def apply() = new Kulcs(Set())
}