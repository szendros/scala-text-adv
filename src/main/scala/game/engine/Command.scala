package game.engine

case class Command(
  action:       Option[String],
  subjectInfos: Set[SubjectInfo],
  subjectIDs:   Set[SubjectID]) {

  def has(id: SubjectID) =
    subjectIDs.contains(id)

  def hasOnly(id: SubjectID) =
    subjectIDs.contains(id) && subjectIDs.size == 1

  def hasNoSubject() = 
    subjectIDs.isEmpty
    
}