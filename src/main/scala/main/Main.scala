package main
import game.subjects._
import game.operations.SubjectOps._
import game.engine.Running
import game.engine.GameData
import game.engine.GameRunner

//import State._

object Main extends App {
      
  val game = GameData(buildScene(Nappali(), Avatar()), NappaliID, Running)

  //val commandText = "tedd a kulcsot az asztalra";
  //val commandText = "vedd fel a kulcsot";
  //val commandText = "észak"
  
  //val state = CommandOps.processCommand(commandText, game)  
  //val r = state.value.run(game).value  
  //println(r) 
  
  GameRunner.gameStart(game).unsafeRunSync()
}


