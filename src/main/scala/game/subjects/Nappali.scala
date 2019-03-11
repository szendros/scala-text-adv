package game.subjects

import cats.implicits._

import game.cond
import game.engine._
import game.engine.Mutation._
import game.MutationError

case object NappaliID extends SubjectID

case class Nappali(
  items: Set[SubjectID],
  lampa: Boolean) extends Subject {

  val id = NappaliID
  val info = SubjectInfo(None, "szoba") 
  
  override val build = {
    
    List(Szekreny(), Ablak())
  }
  
  def szekreny(game: GameData) : Szekreny = game.get(SzekrenyID)

  override def handleCommand(cmd: Command, game: GameData) =
    cmd.action match {
      case Some("nézd") if cmd.hasNoSubject() => Result(this, msg(description(game): _*))
      case Some("é")    => Result(this, Left(MutationError("Nem tudsz északra menni.")))
      case _            => Result(this.copy())
    }

  override def handleMutation(mutation: Mutation, data: GameData) = {   
    mutation match {      
      case RelocateMutation(id)     => Result(this, msg(description(data): _*))
      case AddMutation(_, x)        => Result(this.copy(items + x))
      case RemoveMutation(_, x)     => Result(this.copy(items - x))
      case _                        => Result(this)
    }
  }
       
  def description(game: GameData) = 
    "A nappaliban vagy, a szobában áll egy szekrény." :: Nil ++ 
    cond(szekreny(game).eltolva, "Az asztal el van tolva.")    
}

object Nappali {
  def apply() = new Nappali(Set(SzekrenyID, AblakID), false)
}