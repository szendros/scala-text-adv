package game

import cats.implicits._

object SceneOps {

  //build scene with subjects
  def buildScene(items: Subject*): Map[SubjectID, Subject] =
    items.foldLeft(Map[SubjectID, Subject]())((acc, item) => acc ++ Map(item.id -> item) ++ buildScene(item.build(): _*))

  //get all subjects visible from the subject
  def getAvailableSubjects(subject: SubjectID, scene: Map[SubjectID, Subject]): Set[SubjectID] =
    ((scene get subject) map ((x: Subject) => Set(subject) ++ x.visibleItems ++
      x.visibleItems.foldLeft(List.empty[SubjectID])(_ ++ getAvailableSubjects(_, scene))))
      .getOrElse(Set())

  //find subject from subjectinfo
  def findSubject(info: SubjectInfo, availableSubjects: Set[SubjectID], scene: Map[SubjectID, Subject]) = {
    val subjects = (availableSubjects.toList traverse { scene get _ }) map (
      _.filter(
        x =>
          info match {
            case SubjectInfo(None, b) if x.info.noun == b => true
            case a if x.info == a                         => true
            case _                                        => false
          }))
    
    subjects match {
      case Some(head :: Nil) => Some(head.id)
      case _ => None      
    }       
  }
}

