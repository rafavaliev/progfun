val words = List("7225247386")
val mnemonics0: Map[Char, String] = Map(
  '2' -> "ABC",
  '3' -> "DEF",
  '4' -> "GHI",
  '5' -> "JKL",
  '6' -> "MNO",
  '7' -> "PQRS",
  '8' -> "TUV",
  '9' -> "WXYZ"
)
val mnemonics: Map[Char, String] = mnemonics0 withDefaultValue ""

val charCode: Map[Char, Char] =
  for {
    (code, str) <- mnemonics
    char <- str
  } yield char -> code

def translateNumber(num: String): List[String] = {
  num map (ch => mnemonics(ch)) toList
}

def wordCode(word: String): String =
  word.toUpperCase map charCode

def wordForNum()
wordCode("Java")
wordCode("Kotlin")
translateNumber("7225247386")