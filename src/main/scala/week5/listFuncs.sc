def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => y*y :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)

val list = List(-1,0,1,1,2,3,4,5,6)

squareList(list)
squareList2(list)

val cond: (Int => Boolean) = x => x> 0

list filter cond

list filterNot cond

list partition cond

list takeWhile (x => x <2)

list dropWhile (x => x <2)

list span (x => x < 2)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case head :: tail =>
    val (first, second) = xs span (x => x == head)
    first :: pack(second)
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map(x => (x.head, x.length))

val charsList = List("a", "a", "a", "b", "c", "c", "a")
pack(charsList)

encode(charsList)