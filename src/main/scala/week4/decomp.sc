trait Expr {
  def eval: Int
  def show: String = eval toString
}

class Number(n: Int) extends Expr {
  def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval + e2.eval
}

def eval(e: Expr): Int = {
  e.eval
}

class Prod(e1: Expr, e2: Expr) extends { //Expr {
    def eval:Int = e1.eval * e2.eval
}
eval(new Sum(new Number(1), new Sum(new Number(2), new Number(5))))
