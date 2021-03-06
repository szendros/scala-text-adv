package game.engine

import cats.effect._

import game.operations.CommandOps._
import game.Error

object GameRunner {

  def readln = scala.io.StdIn.readLine;

  def gameStart(game: GameData): IO[GameData] =
    for {
      initial <- IO(processInitialCommand(game).value.run(game).value)
      _ <- initial match {
        case (s, Right(x)) => IO { x.messages.foreach { println(_) } }
        case _             => IO((): Unit)
      }
      gameover <- gameLoop(game)
    } yield initial._1

  def gameLoop(game: GameData): IO[GameData] = {

    for {
      input <- IO { readln }
      processed <- IO(processCommand(input, game).value.run(game).value)
      _ <- processed match {
        case (s, Left(x: Error)) => IO { println(x.msg) }
        case (s, Right(x))       => IO { x.messages.foreach { println(_) } }
      }
      last <- processed._1.state match {
        case Finished => IO(processed)
        case _        => gameLoop(processed._1)
      }
    } yield processed._1
  }

}
