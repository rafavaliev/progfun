import java.util

object first {
  println("Hi!")

  val f: String => String = { case "ping" => "pong" }
  f("ping")


  val pF: PartialFunction[String, String] = { case "ping" => "pong" }
  pF.isDefinedAt("ping")
  if(pF.isDefinedAt("pong")) {
    f("pong")
  }

  val f2: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
  }

  val list = List(1,2,3)
  f2.isDefinedAt(list)
  f2(list)

  var singleList = List(1)
  if (f2.isDefinedAt(singleList))
    f2(singleList)

  val g: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest =>
      rest match {
        case Nil => "two"
      }
  }

  if(g.isDefinedAt(singleList)) {
    g(singleList)
  }

  if(g.isDefinedAt(list)) {
    g(list)
  }

}