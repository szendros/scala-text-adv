package game.subjects

import game._
import game.Subject
import cats.implicits._
import game.MutationOps._

case object NappaliID extends SubjectID

case class Nappali(
  items: Set[SubjectID],
  lampa: Boolean) extends Subject {

  val id = NappaliID
  val info = SubjectInfo(None, "szoba")

  override def build() = List(Asztal(), Ablak())

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("nézd") =>
        Result(this, msg(description))
      case Some("é") =>
        Result(this, Left(MutationError("Nem tudsz északra menni.")))
      case _ => Result(this.copy())
    }

  val description = "A nappaliban vagy, a szobában áll egy asztal és a szekrény"
}

object Nappali {
  def apply() = new Nappali(Set(AsztalID, AblakID), false)
}