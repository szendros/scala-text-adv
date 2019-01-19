package util;

object Utils {
  
  implicit class Pipes[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  } 
}