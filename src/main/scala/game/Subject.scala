package game

import cats.implicits._

sealed trait SubjectID {
}

case class SubjectInfo(
  adjective: Option[String],
  noun:      String)
  
sealed trait Subject {

  val items: Set[SubjectID]

  val id: SubjectID
  
  val info: SubjectInfo

  def visibleItems: Set[SubjectID] = items

  def build(): List[Subject] = List() 
  
  def handleCommand(cmd: Command, data: GameData): Result[Subject, Either[Error, MutationResult]] =
    Result(this)

  def handleMutation(mutation: Mutation, data: GameData): Result[Subject, Either[Error, MutationResult]] =
    Result(this)        
}