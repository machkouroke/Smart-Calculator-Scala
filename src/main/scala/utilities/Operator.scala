package utilities

import scala.annotation.targetName

implicit class AnyEx[T](val v: T) extends AnyVal {
    @targetName("Pipe")
    def |>[U](f: T ⇒ U): U = f(v)
}