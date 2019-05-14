
trait Expr
case class Number(n: Int) extends Expr
case class Var(x: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr



object Number {
  def apply(n: Int): Number = new Number(n)
}

object Sum {
  def apply(e1: Expr, e2: Expr): Sum = new Sum(e1, e2)
  def apply(e1: Expr,n: Int): Sum = new Sum(e1, Number(n))
  def apply(n: Int, e2: Expr): Sum = new Sum(Number(n), e2)
  def apply(n: Int, n2: Int): Sum = new Sum(Number(n), Number(n2))
}

object Prod {
  def apply(e1: Expr, e2: Expr): Prod = new Prod(e1, e2)
  def apply(e1: Expr,n: Int): Prod = new Prod(e1, Number(n))
  def apply(n: Int, e2: Expr): Prod = new Prod(Number(n), e2)
  def apply(n: Int, n2: Int): Prod = new Prod(Number(n), Number(n2))
}

object Var {
  def apply(x: String): Var = new Var(x)
}
def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Var(x) => throw new Error("eval var")
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Prod(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n toString
  case Var(x) => x
  case Sum(l, r) => show(l) + " + " + show(r)
  case Prod(l, r) => (l,r) match {
    case (Sum(l1, r1), Sum(l2, r2)) => "(" + show(Sum(l1, r1)) + ") * (" + show(Sum(l2, r2)) + ")"
    case (Sum(l1, r1), e) => "(" + show(Sum(l1, r1)) + ") * " + show(e)
    case (e, Sum(l1, r1)) => show(e) + " * (" + show(Sum(l1, r1)) + ")"
    case (e1, e2) => show(e1) + " * " + show(e2)

  }
}

show(Sum(Number(1), Number(44)))

show(Sum(Prod(2, Var("x")), Var("y")))

show(Prod(Sum(2, Var("x")), Var("y")))
show(Prod(Sum(2, Var("x")), Sum(Var("y"), 12)))
show(Prod(1,2))