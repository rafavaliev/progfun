val nums = Set(1,2,3,-83)
val people = Set("Aigul", "Nikita", "Rafael", "Rafael")
val s = (1 until 6).toSet

nums map (_ + 2)
people filter (_.startsWith("Ra"))
s.nonEmpty

s contains 5
s contains 6

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens
  queensWithRow forall {
    case(r ,c) => col != c && math.abs(col - c) != row - r
  }
}



def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines = for {
      col <- queens.reverse
    } yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(4) map show) mkString "\n"

(queens(15) take 3 map show) mkString "\n"