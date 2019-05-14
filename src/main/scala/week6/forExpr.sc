def isPrime(n: Int): Boolean =
  (2 until n) forall (d => n % d != 0)

isPrime(11)
isPrime(12)

val n = 11

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)




def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum


scalarProduct(Vector(1,2,3), Vector(4,5,6))