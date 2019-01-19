
package object game {
  
  type Scene = Map[SubjectID, Subject]
  
  type WithError[A] = Either[Error, A]
  
  type GameDataResultOrError[A] = Result[GameData, Either[Error, A]]
  type SubjectResultOrError[A] = Result[Subject, Either[Error, A]]
}