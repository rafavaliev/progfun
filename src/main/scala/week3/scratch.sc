
import week3.{Hello}


trait Planar {
  def height: Int
  def width: Int
  def surface = height * width
}

List()
Set()
Seq()

def error(msg: String) = throw new Error(msg)
def heello(msg: Nothing) = 1

val x = null
val y: String = x
val xy = Nil
val z: Int = 0

if (false) 1 else false
