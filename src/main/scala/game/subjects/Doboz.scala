package game.subjects

import cats.implicits._

import game.engine._
import game.engine.Mutation._

case object DobozID extends SubjectID

case class Doboz(
  items:          Set[SubjectID],  
  kinyitva:       Boolean) extends Subject {

  val id = DobozID
  val info = SubjectInfo(None, "doboz")

  override val build = List(Kulcs())

  override val visibleItems = (items filter (_ => kinyitva))

  override def handleCommand(cmd: Command, game: GameData) =
    cmd.action match {
      case Some("nézd") => Result(this, msg(description(game): _*))
      case Some("nyisd") => Result(
        this.copy(kinyitva = true), msg("Kinyitod a dobozt."))
      case _ => Result(this)
    }

  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      case AddMutation(_, x)    => Result(this.copy(items + x))
      case RemoveMutation(_, x) => Result(this.copy(items - x))
      case _                    => Result(this)
    }

  def description(game: GameData): Seq[String] = {
    if (!kinyitva) return "A doboz zárva van." :: Nil
    return "A doboz nyitva van, ezek vannak benne:" :: Nil ++ items.map(x => game.get[Subject](x).info.noun)
  }
}

object Doboz {
  def apply() = new Doboz(Set(KulcsID), kinyitva = false)
}