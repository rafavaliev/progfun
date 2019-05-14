package week3

import java.util.NoSuchElementException

class Hey

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}


class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
