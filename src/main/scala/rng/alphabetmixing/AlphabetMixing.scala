package rng.alphabetmixing

import scala.util.chaining._

private val Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

def basicMixForward(keyword: String, startPos: Int) =
  basicMix(keyword, startPos, identity)

def basicMixBackward(keyword: String, startPos: Int) =
  basicMix(keyword, startPos, _.reverse)

private def basicMix(keyword: String, startPos: Int, op: String => String) =
  val unusedAlphabet = Alphabet.filterNot(keyword.contains).pipe(op)
  unusedAlphabet.takeRight(startPos) ++ keyword + unusedAlphabet.take(26 - startPos - keyword.length)


def demo(): Unit =
  println(s"-- 5.2 Mixing the alphabet")

  println(s"Mixing the alphabet witness:  $Alphabet")
  println(s"Mixing the alphabet forward:  ${basicMixForward("SAMPLE", 6)}")
  println(s"Mixing the alphabet backward: ${basicMixBackward("SAMPLE", 6)}")
