class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  val numer = x / g

  val denom = y / g

  def +(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational): Rational = this + -that

  def * (that: Rational): Rational =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def <(that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  def max(that: Rational): Rational =
    if (this < that) that else this

  override def toString: String =
    numer + "/" + denom
}

def addRational(r: Rational, s: Rational): Rational =
  new Rational(
    r.numer * s.denom + s.numer * r.denom, r.denom * s.denom
  )

def makeString(r: Rational) =
  r.numer + "/" + r.denom

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

makeString(x)
y
makeString(addRational(x, y))
makeString(x + y)
x + y

val xy = x - y
val xyz = x - y - z

xy

xyz

x < y
x.max(y)

x + new Rational(1) +  x + x

new Rational(1,2).numer

new Rational(1,2) < new Rational(2,3)

x + y
-x