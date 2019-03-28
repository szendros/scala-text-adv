package game.subjects

import cats.implicits._

import game.cond
import game.engine._
import game.engine.Mutation._
import game.MutationError

case object NappaliID extends SubjectID

case class Nappali(
  items:          Set[SubjectID],
  containedItems: Set[SubjectID],
  lampa:          Boolean) extends Subject {

  val id = NappaliID
  val info = SubjectInfo(None, "szoba")

  override val build = {

    List(Szekreny(), Ablak())
  }

  override val visibleItems = items ++ containedItems

  def szekreny(game: GameData): Szekreny = game.get(SzekrenyID)

  override def handleCommand(cmd: Command, game: GameData) =
    cmd.action match {
      case Some("nézd") if cmd.hasNoSubject() => Result(this, msg(description(game): _*))
      case Some("é")                          => Result(this, Left(MutationError("Nem tudsz északra menni.")))
      case _                                  => Result(this.copy())
    }

  override def handleMutation(mutation: Mutation, data: GameData) = {
    mutation match {
      case RelocateMutation(id) => Result(this, msg(description(data): _*))
      case AddMutation(_, x)    => Result(this.copy(containedItems = containedItems + x))
      case RemoveMutation(_, x) => Result(this.copy(containedItems = containedItems - x))
      case _                    => Result(this)
    }
  }

  def description(game: GameData) =
    "A nappaliban vagy, a szobában áll egy szekrény." :: Nil ++
      cond(szekreny(game).eltolva, "Az asztal el van tolva.") ++
      cond(containedItems.size > 0, "A földön a következők vannak:" ::
        Nil ++ containedItems.map(x => game.get[Subject](x).info.noun))
}

object Nappali {
  def apply() = new Nappali(Set(SzekrenyID, AblakID), Set(), false)
}