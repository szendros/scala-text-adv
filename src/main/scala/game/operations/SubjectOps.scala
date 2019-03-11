package game.operations

import cats.implicits._

import scala.Left
import scala.Right

import game.engine._
import game.TooManySubjectsError
import game.SubjectNotFoundError

object SubjectOps {

  //build scene with subjects
  def buildScene(items: Subject*): Map[SubjectID, Subject] =
    items.foldLeft(Map[SubjectID, Subject]())((acc, item) => acc ++ Map(item.id -> item) ++ buildScene(item.build: _*))

  def getAvailableSubjects(scene: Map[SubjectID, Subject], subject: SubjectID*): Set[SubjectID] =
    subject.foldLeft(Set[SubjectID]()) {_ ++ getAvailableSubjects(scene, _)}
    
  //get all subjects visible from the subject  
  def getAvailableSubjects(scene: Map[SubjectID, Subject], subject: SubjectID): Set[SubjectID] =
    ((scene get subject) map ((x: Subject) => Set(subject) ++ x.visibleItems ++
      x.visibleItems.foldLeft(List.empty[SubjectID])(_ ++ getAvailableSubjects(scene, _))))
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
      case Some(head :: Nil) => Right(head.id)
      case Some(head :: item :: Nil) => Left(TooManySubjectsError("Több is van, melyikre gondolsz?"))
      case _ => Left(SubjectNotFoundError(s"Itt nincsen ${info.noun}."))
    }       
  }
}

