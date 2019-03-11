package game.operations

import cats.implicits._
import cats.data.EitherT
import cats.data._
import cats.kernel.Monoid
import scala.Left
import scala.Right

import game.WithError
import game.engine._
import game.operations.MutationOps._
import game.operations.SubjectOps._
import game.subjects._
import game.words.Actions._
import game.Error
import game.CommandError

object CommandOps {

  def change[A](fn: GameData => (GameData, Either[Error, A])) =
    EitherT(State[GameData, Either[Error, A]](x => fn(x)))

  def getCommandWithIds(cmd: Command, availableSubjects: Set[SubjectID], s: GameData) = {           
    (s, cmd.subjectInfos.toList.traverse { info =>
      findSubject(info, availableSubjects, s.scene) 
    } map { x => Command(cmd.action, Set(), x.toSet) })
  }  
    
  def handleCommand(cmd: Command, availableSubjects: Set[SubjectID], s: GameData) = {    
    val res = (s.scene filter (x => (
        cmd.subjectIDs ++ Set(AvatarID, s.currentLocation)).contains(x._1))).map { _._2.handleCommand(cmd, s) }        
    val cmdRes = res.foldLeft(Monoid[WithError[MutationResult]].empty)((acc, item) =>
      Monoid[WithError[MutationResult]].combine(acc, item.result))        
    val newState = saveMutationInState(s.copy(s.scene ++ (res map { x => x.item.id -> x.item }).toMap), cmdRes)       
    (newState, cmdRes match {
      case Right(x) if x.messages.isEmpty => Left(CommandError(unhandledActionMessage(cmd.action.getOrElse("")))) 
      case x => x
    })
    
  }
  
   def saveMutationInState(state: GameData, mutation: WithError[MutationResult]) = 
    mutation.map {m => state.copy(        
        currentLocation = m.currentLocation.getOrElse(state.currentLocation),
        state = m.state.getOrElse(state.state),
        )} getOrElse(state)    

  def processCommand(commandText: String, data: GameData) =
    for {      
      cmdParsed <- change { (_, Parser.parse(commandText)) }      
      availableSubjects <- change { s => (s,  Right(getAvailableSubjects(s.scene, data.currentLocation, AvatarID))) }
      cmdWithIds <- change { s => getCommandWithIds(cmdParsed, availableSubjects, s) }
      cmdRes <- change { s => handleCommand(cmdWithIds, availableSubjects, s) }      
      eventRes <- change {  s => (s, Right(cmdRes.copy(mutations = cmdRes.mutations ++ List(EventMutation(None))))) }
      mutRes <- change { s =>
        {                                    
          val res = processMutations(s, MutationResult(eventRes.messages, eventRes.mutations))
          val newState = saveMutationInState(res.item, res.result)          
          (newState, res.result)
        }
      }
    } yield mutRes          
    
  def processInitialCommand(data: GameData) = {    
    val initialMutation = RelocateMutation(Some(NappaliID));    
     change { s =>
        {                          
          val res = processMutations(s, MutationResult(List(), List(initialMutation)))
          val newState = saveMutationInState(res.item, res.result)          
          (newState, res.result)
        }
     }
    }
}


