package game

import MutationOps._
import SubjectOps._

import cats.implicits._
import cats.kernel.Monoid
import cats.data.EitherT
import cats.data._
import game.subjects.LeltarID
import game.subjects.NappaliID
import game.words.Actions._

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
      if (cmd.subjectIDs.isEmpty) availableSubjects else cmd.subjectIDs).contains(x._1))).map { _._2.handleCommand(cmd, s) }        
    val cmdRes = res.foldLeft(Monoid[WithError[MutationResult]].empty)((acc, item) =>
      Monoid[WithError[MutationResult]].combine(acc, item.result))        
    val newState = saveMutationInState(s.copy(s.scene ++ (res map { x => x.item.id -> x.item }).toMap), cmdRes)       
    (newState, cmdRes match {
      case Right(x) if x.messages.isEmpty => Left(CommandError(unhandleActionMessage(cmd.action.getOrElse("")))) 
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
      availableSubjects <- change { s => (s,  Right(getAvailableSubjects(s.scene, data.currentLocation, LeltarID))) }
      cmdWithIds <- change { s => getCommandWithIds(cmdParsed, availableSubjects, s) }
      cmdRes <- change { s => handleCommand(cmdWithIds, availableSubjects, s) }      
      mutRes <- change { s =>
        {                          
          val res = processMutations(s, MutationResult(cmdRes.messages, cmdRes.mutations))
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


