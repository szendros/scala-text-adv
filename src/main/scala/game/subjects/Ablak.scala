package game.subjects

import cats.implicits._

import game.cond
import game.engine._
import game.engine.Mutation._

object AblakID extends SubjectID

case class Ablak(
  items:  Set[SubjectID],
  nyitva: Boolean) extends Subject {

  val id = AblakID
  val info = SubjectInfo(None, "ablak")

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("nézd") => Result(this, msg(description(data): _*))
      case _                               => Result(this)
    }

  def description(game: GameData) =
    "Egy kis ablak." :: Nil ++ cond(nyitva, "Ki van nyitva.")
}

object Ablak {
  def apply() = new Ablak(Set(), nyitva = false)
}