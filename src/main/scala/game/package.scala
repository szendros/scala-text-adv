
package object game {
  
  type Scene = Map[SubjectID, Subject]
  
  type WithError[A] = Either[Error, A]
   
   implicit class Pipes[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  } 
}