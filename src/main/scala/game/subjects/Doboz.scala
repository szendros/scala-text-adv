package game.subjects

import game._
import game.Subject
import cats.implicits._
import game.MutationOps._

case object DobozID extends SubjectID

case class Doboz(
  items:   Set[SubjectID],
  kinyitva: Boolean) extends Subject {

  val id = DobozID
  val info = SubjectInfo(None, "doboz")  
  
  override def build() = List(Kulcs())  
  
  override val visibleItems =
    items filter (_ => kinyitva)
  
  override def handleMutation(mutation: Mutation, data: GameData) =
    mutation match {    
       case RemoveMutation(_, KulcsID) =>
        Result(this.copy(items - KulcsID), mut(RelocateMutation(Some(AblakID))))        
      case _ => Result(this.copy())
    }
}

object Doboz {
  def apply() = new Doboz(Set(KulcsID), kinyitva = true)
}