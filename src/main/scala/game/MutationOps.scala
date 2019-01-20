package game

import cats.implicits._
import scala.annotation.tailrec

object MutationOps {

  @tailrec
  def processMutations(game: GameData, mutationResults: MutationResult): Result[GameData, WithError[MutationResult]] = {
    mutationResults.mutations match {
      case head :: tail => {
        val res = (game.scene filterKeys { x => x == head.subject.getOrElse(x) }) map { _._2.handleMutation(head, game) }
        val newState = game.copy(
          scene = game.scene ++ (res map { x => x.item.id -> x.item }).toMap)
        val newRes = res.foldLeft(Right(mutationResults.copy(mutations = Nil)): WithError[MutationResult]) { (acc, item) => acc |+| item.result
        }        
        newRes match {
          case Right(x) =>
            processMutations(newState, (x ).copy(mutations = tail ++ x.mutations))
          case Left(x) => Result(game, Left(x))
        }
      }
      case Nil => Result(game, Right(mutationResults.copy(mutations = Nil)))
    }
  }  
}