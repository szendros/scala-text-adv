package game.subjects

import game._
import game.Subject
import cats.implicits._
import game.Mutation._

case object NappaliID extends SubjectID

case class Nappali(
  items: Set[SubjectID],
  lampa: Boolean) extends Subject {

  val id = NappaliID
  val info = SubjectInfo(None, "szoba")

  override def build() = List(Asztal(), Ablak())

  override def handleCommand(cmd: Command, data: GameData) =
    cmd.action match {
      case Some("nézd") => Result(this, msg(description))
      case Some("é")    => Result(this, Left(MutationError("Nem tudsz északra menni.")))
      case _            => Result(this.copy())
    }

  override def handleMutation(mutation: Mutation, data: GameData) = {   
    mutation match {      
      case RelocateMutation(id)     => Result(this, msg(description))
      //case AddMutation(id, KulcsID) => Result(this, Left(MutationError("Itt nem tudod letenni.")))
      case AddMutation(_, x)        => Result(this.copy(items + x))
      case RemoveMutation(_, x)     => Result(this.copy(items - x))
      case _                        => Result(this)
    }
  }
  val description = "A nappaliban vagy, a szobában áll egy asztal és a szekrény."
}

object Nappali {
  def apply() = new Nappali(Set(AsztalID, AblakID), false)
}