import scala.language.postfixOps

val nums = Vector(1,2,3,-83)
val people = Vector("Aigul", "Nikita", "Rafael")

25 +: nums
people :+ "Andry"

val xs: Array[Int] = Array(0,1,2,3,4,5,6,-7, 55)
xs map (x => x * 2)

val s = "Hello World!"
s filter(_.isUpper)


val range: Range = 1 until 5
val range2: Range = 6 to 10
1 to 10 by 2
6 to 1 by -1


val sq = Seq(1,2,3,4,5)

nums contains 1
nums filter(_>2)
s forall(c => c.isUpper)

val pairs = nums zip people
pairs unzip


s flatMap (c => List('.', c))

nums sum

xs.maxBy(x => -x)


range flatMap(x => range2 map (y => (x,y)))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case(x, y) => x * y}.sum

