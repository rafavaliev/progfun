// Peano numbers

abstract class Nat { //Natural
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor: Nat = throw new Error("0.predecessor No predecessor for Zero")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat =
    if (that.isZero) Zero
    else throw new Error("0.- ")

  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat =
    if (that.isZero) this
    else n - that.predecessor

  override def toString: String = "successor("+n+")"
}

Zero
Zero.successor.successor.successor
