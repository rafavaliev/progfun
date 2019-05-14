def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)


removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)


def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]) : List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x,y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val someList = List(1, 2, 3, 4, 5, 6, 7, 8, 112, 2, 3, 5, 11, 23, -1, -5)
msort(someList)

val strList = List("orange", "apple", "scala", "Moscow")
msort(strList)

1 :: 3 :: 2 :: Nil
val pair = ("answer", 42)

val (answer, value) = pair


def flatten(xs: List[Any]): List[Any] = {
  xs match {
    case Nil => Nil
    case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
    case (head: Any) :: tail => head :: flatten(tail)
  }
}

flatten(List(List(1,2,3),3,List(3,2,1,4)))