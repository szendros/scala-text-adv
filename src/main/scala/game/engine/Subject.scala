package game.engine

import cats.implicits._
import game.Error

trait SubjectID {
}

case class SubjectInfo(
  adjective: Option[String],
  noun:      String)
  
trait Subject {

  val items: Set[SubjectID]  
 
  val id: SubjectID
  
  val info: SubjectInfo

  def visibleItems: Set[SubjectID] = items

  val build: List[Subject] = List() 
  
  def handleCommand(cmd: Command, data: GameData): Result[Subject, Either[Error, MutationResult]] =
    Result(this)

  def handleMutation(mutation: Mutation, data: GameData): Result[Subject, Either[Error, MutationResult]] =
    Result(this)        
}