val romanNumbers = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCoutry = Map("US" -> "Washington", "Russia" -> "Moscow")
capitalOfCoutry("US")
capitalOfCoutry get "US"
capitalOfCoutry get "Andorra"


def showCapitalOfCountry(country: String) = capitalOfCoutry get country match {
  case Some(capital) => capital
  case None => "Missing data"
}


showCapitalOfCountry("US")
showCapitalOfCountry("Andorra")


val fruits = List("orange", "apple", "pineapple", "pear")

fruits sortWith (_.length < _.length)
fruits.sorted
fruits groupBy (_.head)

Map(0 -> 5, 1 -> -2, 3 -> 1)

class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def +++(other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val terms2 = terms withDefaultValue (0.0)
    val (exp, coef) = term
    terms + (exp -> (coef + terms(exp)))
  }
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coef) = term
    exp -> (coef + terms(exp))
  }

  override def toString: String =
    (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + "x^" + exp) mkString " + "
}

val p1 = new Poly(Map(0 -> 5.0, 1 -> -2.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
val p3 = new Poly(0 -> 3, 3 -> 7)
p1 + p2 + p3