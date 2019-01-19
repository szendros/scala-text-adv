package game

import MutationOps._
import SceneOps._

import cats.implicits._
import cats.kernel.Monoid
import cats.data.EitherT
import cats.data._

object CommandOps {

  def change[A](fn: GameData => (GameData, Either[Error, A])) =
    EitherT(State[GameData, Either[Error, A]](x => fn(x)))
  
  def processCommand(commandText: String, data: GameData) =
    for {
      //parse command
      cmd <- change { (_,Parser.parse(commandText)) }
      //expand command with available ids
      availableSubjects <- change { s =>
        (s, Right(getAvailableSubjects(data.currentLocation, s.scene)))
      }
      cmdWithIds <- change { s =>
        val availableSubjects = getAvailableSubjects(data.currentLocation, s.scene)
        (s, cmd.subjectInfos.toList.traverse { info =>
          findSubject(info, availableSubjects, data.scene) match {
            case Some(x) => Right(x)
            case None    => Left(TooManySubjectsError(info.noun))
          }
        } map { x => Command(cmd.action, Set(), x.toSet) })
      }
      //process command
      cmdRes <- change { s =>
        {
          val res = (data.scene filter (x => (
            if (cmdWithIds.subjectIDs.isEmpty) availableSubjects else cmdWithIds.subjectIDs)
            .contains(x._1))) map { _._2.handleCommand(cmdWithIds, data) }
          val newState = data.copy(data.scene ++ (res map { x => x.item.id -> x.item }).toMap)
          val cmdRes = res.foldLeft(Monoid[WithError[MutationResult]].empty)((acc, item) =>
            Monoid[WithError[MutationResult]].combine(acc, item.result))
          (newState, cmdRes)
        }
      }
      //process all mutations
      mutRes <- change { s => {       
              val res = processMutations(s, cmdRes.messages, cmdRes.mutations)
              (res.item, Right(res.result))                            
        }
      }
    } yield mutRes
}


