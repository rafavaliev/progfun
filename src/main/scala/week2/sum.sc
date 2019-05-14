def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, acc + f(a))
  }
  loop(a, 0)
}

def sumInts(a: Int, b: Int): Int = sum(x => x)(a: Int, b: Int)
sumInts(0,1000)

sum(x => x*x)(0,10)

def sum2(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a>b) 0
    else f(a) + sumF(a+1, b)
  sumF
}

def sumInts2 =  sum2(x => x)
def sumCubes2 = sum2(x => x*x*x)
sumInts2(0,1000)

sumCubes2(0,5) + sumInts2(0,5)

def cubes(x: Int): Int = x*x*x
sum(cubes)(0,10)


def sum3(f: Int => Int)(a: Int, b: Int): Int =
  if (a>b) 0 else f(a) + sum(f)(a+1, b)

sum3(cubes)(0,10)