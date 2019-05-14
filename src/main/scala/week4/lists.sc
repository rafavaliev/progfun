val fruit = List("apples", "oranges", "pears")
val fruits = "apples" :: "oranges" :: "pears" :: Nil
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

val empty: List[Nothing] = List()


Nil.::(4).::(3).::(2).::(1)

def insert(x: Int, xs:List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y :: ys =>
    if (x > y) x :: xs
    else y :: insert(x, ys)
}

def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => List()
  case y :: ys => insert(y, isort(ys))
}

isort(List(1,2,3,4,6,1,9,7,8))