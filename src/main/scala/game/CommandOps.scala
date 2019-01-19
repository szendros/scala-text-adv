package game

import MutationOps._
import SubjectOps._

import cats.implicits._
import cats.kernel.Monoid
import cats.data.EitherT
import cats.data._

object CommandOps {

  def change[A](fn: GameData => (GameData, Either[Error, A])) =
    EitherT(State[GameData, Either[Error, A]](x => fn(x)))

  def getCommandWithIds(cmd: Command, s: GameData) = {
    val availableSubjects = getAvailableSubjects(s.currentLocation, s.scene)
    (s, cmd.subjectInfos.toList.traverse { info =>
      findSubject(info, availableSubjects, s.scene) 
    } map { x => Command(cmd.action, Set(), x.toSet) })
  }

  def handleCommand(cmd: Command, availableSubjects: Set[SubjectID], s: GameData) = {    
    val res = (s.scene filter (x => (
      if (cmd.subjectIDs.isEmpty) availableSubjects else cmd.subjectIDs).contains(x._1))).map { _._2.handleCommand(cmd, s) }
    val newState = s.copy(s.scene ++ (res map { x => x.item.id -> x.item }).toMap)
    val cmdRes = res.foldLeft(Monoid[WithError[MutationResult]].empty)((acc, item) =>
      Monoid[WithError[MutationResult]].combine(acc, item.result))
    (newState, cmdRes)
  }

  def processCommand(commandText: String, data: GameData) =
    for {      
      cmdParsed <- change { (_, Parser.parse(commandText)) }      
      availableSubjects <- change { s => (s, Right(getAvailableSubjects(data.currentLocation, s.scene))) }
      cmdWithIds <- change { s => getCommandWithIds(cmdParsed, s) }
      cmdRes <- change { s => handleCommand(cmdWithIds, availableSubjects, s) }      
      mutRes <- change { s =>
        {          
          val res = processMutations(s, cmdRes.messages, cmdRes.mutations)
          (res.item, res.result)
        }
      }
    } yield mutRes
}


