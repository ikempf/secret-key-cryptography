package ciphers.bazeries

def bazeriesType4(
    alphabet: Map[Char, Char],
    transpositionKey: List[Int],
    plaintext: String
): String =
  transposition(transpositionKey, substitution(alphabet, plaintext))

private def substitution(alphabet: Map[Char, Char], plaintext: String): String =
  plaintext.map(c => alphabet(c))

private def transposition(
    transpositionKey: List[Int],
    substitution: String
): String =
  continuously(transpositionKey)
    .scanLeft((0, Option(""))) { case ((start, _), pieceSize) =>
      val end = start + pieceSize
      (end, nextPiece(substitution, start, end))
    }
    .map(_._2)
    .takeWhile(_.isDefined)
    .map(_.getOrElse(""))
    .mkString

private def continuously(transpositionKey: List[Int]) =
  LazyList.continually(transpositionKey).flatten

private def nextPiece(substitution: String, start: Int, end: Int) =
  val piece = substitution.slice(start, end)
  if piece.isEmpty then None
  else Some(piece.reverse)

private val TranspositionKey = List(4, 2, 3)
private val Alphabet =
  Map(
    'A' -> 'H',
    'B' -> 'G',
    'C' -> 'F',
    'D' -> 'D',
    'E' -> 'C',
    'F' -> 'B',
    'G' -> 'A',
    'H' -> 'Z',
    'I' -> 'E',
    'J' -> 'R',
    'K' -> 'I',
    'L' -> 'S',
    'M' -> 'Y',
    'N' -> 'X',
    'O' -> 'W',
    'P' -> 'V',
    'Q' -> 'U',
    'R' -> 'T',
    'S' -> 'Q',
    'T' -> 'P',
    'U' -> 'O',
    'V' -> 'N',
    'W' -> 'M',
    'X' -> 'L',
    'Y' -> 'K',
    'Z' -> 'J'
  )

def demo(): Unit =
  println(s"-- 4.6.1 Bazeries Type 4")

  val plaintext = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG"
  val ciphertext = bazeriesType4(Alphabet, TranspositionKey, plaintext)
  println(s"Bazeries Type 4 plaintext:    $plaintext")
  println(s"Bazeries Type 4 substitution: ${substitution(Alphabet, plaintext)}")
  println(s"Bazeries Type 4 ciphertext:   $ciphertext")
