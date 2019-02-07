
package object game {
  
  type Scene = Map[SubjectID, Subject]
  
  type WithError[A] = Either[Error, A]
   
  @inline def cond[T](p: => Boolean, v: T): List[T] = if (p) v :: Nil else Nil 
  
   implicit class Pipes[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  } 
}