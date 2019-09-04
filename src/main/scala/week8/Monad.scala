package week8

trait M[T]{
  def flatMap[U](f:T => M[U]): M[U]
}
/*
abstract class Option[+T]{
  def flatMap[U](f: T =>Option[U]): Option[U] = this match {
    case Some(x) =>f(x)
    case None =>None
  }
}*/