val someList = List(1, 2, 3, 4, 5, 6, 7, 8, 112, 2, 3, 5, 11, 23, -1, -5)

def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x+y)
def sum2(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)
def product2(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

def sum3(xs: List[Int]) = (xs foldLeft 0)(_ + _)
def product3(xs: List[Int]) = (xs foldLeft 1)(_ * _)


def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _)

concat(List(1,2,3), List(0,1,2,5))


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, y) => 1 + y)


mapFun(List(1,2,3,4), (x => x * 2): Int => Int)
lengthFun(someList)
lengthFun(List())
lengthFun(List(12323))
