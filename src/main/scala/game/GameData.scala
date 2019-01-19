package game

trait GameState
case object Running extends GameState
case object Finished extends GameState

case class GameData (
  scene: Scene,
  currentLocation: SubjectID,
  state: GameState
)