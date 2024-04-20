package rng.alphabetmixing

import scala.util.chaining.*

private val Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
private val Len = Alphabet.length

def basicMixForward(keyword: String, startPos: Int): String =
  basicMix(keyword, startPos, identity)

def basicMixBackward(keyword: String, startPos: Int): String =
  basicMix(keyword, startPos, _.reverse)

private def basicMix(keyword: String, startPos: Int, op: String => String) =
  val unusedAlphabet = Alphabet.filterNot(keyword.contains).pipe(op)
  unusedAlphabet.takeRight(startPos) ++ keyword + unusedAlphabet.take(Len - startPos - keyword.length)

def columnarMixingDown(keyword: String): String =
  columnarMixing(keyword).transpose.map(_.mkString).mkString(" ")

private def columnarMixing(keyword: String) =
  val cols = keyword.length
  val rows = math.ceil(Len.toDouble / cols).toInt // Rounded up
  val matrix = Array.ofDim[Char](rows, cols)

  val unusedAlphabetGroups = Alphabet.filterNot(keyword.contains).grouped(cols).toList

  matrix(0) = keyword.toArray
  Range(1, rows).foreach(row => matrix(row) = unusedAlphabetGroups(row - 1).toArray)

  matrix

def demo(): Unit =
  println(s"-- 5.2 Mixing the alphabet")

  println(s"Mixing the alphabet witness:  $Alphabet")
  println(s"Mixing the alphabet forward:  ${basicMixForward("SAMPLE", 6)}")
  println(s"Mixing the alphabet backward: ${basicMixBackward("SAMPLE", 6)}")
  println()
  println(s"Mixing the alphabet witness:   $Alphabet")
  println(s"Mixing the alphabet columnar:  ${columnarMixingDown("SAMPLE")}")
