def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

def fact(n: Int): Int = product(x => x)(1, n)
product(x => x * x)(3, 4)
fact(5)

def apply(f: Int => Int)(f2: (Int, Int) => Int)(entity: Int)(a: Int, b: Int): Int = {
  if (a > b) entity
  else f2(f(a), apply(f)(f2)(entity)(a + 1, b))
}

apply(x => x)((x, y) => x + y)(0)(0, 10)
apply(x => x)((x, y) => x * y)(1)(1, 10)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

mapReduce(x => x * 2, (x, y) => x + y, 0)(0, 10)
def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

product2(x => x)(1,10)t