package main

import game._
import cats.data.EitherT
import cats.data._
import game.subjects._
import game.SubjectOps._

//import State._

object Main extends App {
    
  def change[A](fn: Scene => (Scene, Either[Object, A])) =
    EitherT(State[Scene, Either[Object, A]](x => fn(x)))
          
  val game = GameData(buildScene(Nappali(), Leltar()), NappaliID, Running)

  //val commandText = "tedd a kulcsot az asztalra";
  //val commandText = "vedd fel a kulcsot";
  val commandText = "észak"
  
  val state = CommandOps.processCommand(commandText, game)  
  val r = state.value.run(game).value  
  //println(r) 
  
  GameRunner.gameLoop(game).unsafeRunSync()
}


