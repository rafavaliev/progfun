import java.util.NoSuchElementException

List(1, 2, 3)

List(List(true, false), List(3))

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: Listt[T]
}


class Cons[T](val head: T, val tail: Listt[T]) extends Listt[T] {
  override def isEmpty: Boolean = false

  override def toString: String = head + "," + tail
}

class Nil[T] extends Listt[T] {
  override def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "."
}
def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton[Boolean](true)


def nth[T](n: Int, xs: Listt[T]): T = {
  if (n < 0 || xs.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) xs.head
  else nth(n-1, xs.tail)
}

nth(0, singleton(1))

val c1 = new Cons(1, new Nil)
val c2 = new Cons(2, c1)
c2
nth(1, c2)
nth(-1, c2)