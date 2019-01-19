package main

import game._
import cats.data.EitherT
import cats.data._
import game.subjects._
import game.SceneOps._

//import State._

object Main extends App {
    
  def change[A](fn: Scene => (Scene, Either[Object, A])) =
    EitherT(State[Scene, Either[Object, A]](x => fn(x)))
          
  val scene = GameData(buildScene(Nappali(), Leltar()), NappaliID)

  //val commandText = "tedd a kulcsot az asztalra";
  val commandText = "vedd fel a kulcsot";
  //val commandText = "észak"
  
  val state = CommandOps.processCommand(commandText, scene)
  
  println(state.value.run(scene).value) 
}


