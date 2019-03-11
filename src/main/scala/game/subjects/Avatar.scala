package game.subjects

import cats.implicits._

import game.engine._
import game.engine.Mutation._

case object AvatarID extends SubjectID

case class Avatar(
  items: Set[SubjectID]) extends Subject {

  val id = AvatarID
  val info = SubjectInfo(None, "")

    override def handleCommand(cmd: Command, game: GameData) =
    cmd.action match {
      case Some("leltár") => Result(this,  msg(inventory(game): _*))
      case _                               => Result(this)
    }
  
  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {
      case AddMutation(_, x)    => Result(this.copy(items + x))
      case RemoveMutation(_, x) => Result(this.copy(items - x))
      case _                    => Result(this)
    }
  
   def inventory(game: GameData) : Seq[String] = {    
    if (items.isEmpty) return "Nincs nálad semmi." :: Nil
    return "Ezek vannak nálad:" :: Nil ++ items.map(x => game.get[Subject](x).info.noun)
  }
}

object Avatar {
  def apply() = new Avatar(Set())
}