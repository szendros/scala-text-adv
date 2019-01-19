package game

//import cats.implicits._
import game.CommandOps._

import cats.effect.IO

object GameRunner {

  def readln = scala.io.StdIn.readLine;

  def gameLoop(game: GameData): IO[GameData] = {
    println(game) 
    for {
      input <- IO { readln }
      processed <- IO(processCommand(input, game).value.run(game).value)             
      _ <- processed match {
        case (s, Left(x: Error)) => IO { println(x.msg) }
        case (s, Right(x)) => IO { x.messages.foreach { println(_) } }
      }
      last <- processed._1.state match {
        case Finished => IO(processed)
        case _        => gameLoop(processed._1)
      }      
    } yield processed._1
  }

}
