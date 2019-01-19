package game

import cats.kernel.Monoid
import cats.implicits._

object MutationOps {

  def processMutations(data: GameData, messages: List[String], mutations: List[Mutation]): Result[GameData, WithError[MutationResult]] = {
    mutations match {
      case head :: tail => {
        val res = (data.scene filterKeys { x => x == head.subject.getOrElse(x) }) map { _._2.handleMutation(head, data) }
        val currLoc = head match {
          case RelocateMutation(Some(x)) => x
          case _                         => data.currentLocation
        }
        val gameState = head match {
          case GameOverMutation(None) => Finished
          case _                         => data.state
        }
        val newState = data.copy(
          scene = data.scene ++ (res map { x => x.item.id -> x.item }).toMap, currentLocation = currLoc, state = gameState)
        val newRes = res.foldLeft(Monoid[WithError[MutationResult]].empty) { (acc, item) =>
          Monoid[WithError[MutationResult]].combine(acc, item.result)
        }
        newRes match {
          case Right(x) =>
            processMutations(newState, messages ++ x.messages, tail ++ x.mutations)
          case Left(x) => Result(data, Left(x))       
        }
      }
      case Nil => Result(data, Right(MutationResult(messages, List())))
    }
  }
  
   def msg(message: String) =
    Right(MutationResult(List(message), List()))
  
  def mut(mutation: Mutation) =
    Right(MutationResult(List(), List(mutation)))
 
  def mut(message: String, mutation: Mutation) =
    Right(MutationResult(List(message), List(mutation)))
    
  def mut(message: String, mutation: Mutation*) =
    Right(MutationResult(List(message), mutation.toList))
  
}