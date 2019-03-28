package game.subjects

import cats.implicits._

import game.engine._
import game.engine.Mutation._

case object SzekrenyID extends SubjectID

case class Szekreny(
  items:    Set[SubjectID],  
  kinyitva: Boolean,
  eltolva:  Boolean) extends Subject {

  val id = SzekrenyID
  val info = SubjectInfo(Some("kis"), "szekrény")

  override val build = List(Doboz())
  
  override val visibleItems = items

  override def handleCommand(cmd: Command, game: GameData) =
    cmd.action match {
      case Some("nézd") => Result(this, msg(description(game): _*))
      case Some("nyisd") => Result(this.copy(kinyitva = true, items = items + KulcsID),
        msg("Kinyitod a szekrényt."))
      case Some("told") => Result(this.copy(eltolva = true),
        msg("Sikeresen eltoltad a szekrényt. Mögötte van egy ablak."))
      case _ => Result(this)
    }

  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      case AddMutation(_, x)    => Result(this.copy(items + x))
      case RemoveMutation(_, x) => Result(this.copy(items - x))
      case _                    => Result(this)
    }   

  def description(game: GameData) : Seq[String] = {
    if (!kinyitva) return "A szekrény zárva van." :: Nil
    return "A szekrény nyitva van, ezek vannak benne:" :: Nil ++ items.map(x => game.get[Subject](x).info.noun)
  }

}

object Szekreny {
  def apply() = new Szekreny(Set(DobozID), kinyitva = false, eltolva = false)
}