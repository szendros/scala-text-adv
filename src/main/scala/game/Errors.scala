package game

sealed trait Error {
  val msg: String
}
case class GeneralError(msg: String) extends Error

case class ParseError(msg: String) extends Error
case class CommandError(msg: String) extends Error
case class SubjectNotFoundError(msg: String) extends Error
case class TooManySubjectsError(msg: String) extends Error
case class MutationError(msg: String) extends Error