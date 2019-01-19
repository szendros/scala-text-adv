package game

case class Command (
  action: Option[String],
  subjectInfos: Set[SubjectInfo],  
  subjectIDs: Set[SubjectID]
)