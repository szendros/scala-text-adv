package game.subjects

import game._
import game.Subject

object AblakID extends SubjectID

case class Ablak(
  items: Set[SubjectID],
  nyitva: Boolean
) extends Subject {
        
  val id = AblakID
  val info = SubjectInfo(None, "ablak")           
}

object Ablak {
   def apply() = new Ablak(Set(), nyitva = false)
}