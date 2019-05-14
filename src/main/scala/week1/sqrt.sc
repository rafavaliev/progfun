1 + 2
def abs(x: Double): Double = if (x > 0) x else -x
abs(-2)


def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double): Boolean =
  abs(guess * guess - x)/x < 0.0010000

def improve(guess: Double, x: Double): Double =
  (guess + x/guess) / 2

def sqrt(x: Double): Double = sqrtIter(1.0, x)



sqrt(2)
sqrt(4)
sqrt(0.0001)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)

def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n==0) acc
    else loop(acc*n, n-1)
  loop(1, n)
}

factorial(1)
