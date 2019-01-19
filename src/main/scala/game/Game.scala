package game

//import cats.implicits._
import cats.effect.IO

trait GameState
case object Running extends GameState
case object Finished extends GameState

case class GeneralError(msg: String) extends Error

case class Game(
  state:     GameState,
  messages:  Either[Error, List[String]] = Right(List()),
  mutations: List[String]                = Nil
  )

class GameRunner() {

  def readln = scala.io.StdIn.readLine;

  def processInput(game: Game) = (input: String) => input match {
    case "exit"  => game.copy(state = Finished)
    case "error" => game.copy(messages = Left(GeneralError("error")))
    case _       => game.copy(messages = Right(List("alma", "korte")))
  }

  def processMutations(game: Game) =
    game

  def gameLoop(game: Game): IO[Game] =
    for {
      input <- game.mutations match {
        case Nil => IO { readln }.map(processInput(game))
        case _   => IO(game)
      }
      mutated <- IO(processMutations(input))
      _ <- input.messages match {
        case Left(x: Error) => IO { println(x.msg) }
        case Right(x)         => IO { x.foreach { println(_) } }
      }
      last <- mutated.state match {
        case Finished => IO(mutated)
        case _        => gameLoop(mutated)
      }
    } yield last

}
