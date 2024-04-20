package ciphers.belazo

private val Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
private val PolyAlphabet =
  List
    .iterate(Alphabet, 26)(alphabet => alphabet.tail :+ alphabet.head)
    .groupBy(_.head)
    .view
    .mapValues(_.head)
    .toMap

private val CipherText =
  "ZVZPVTOGGEKHXSNLRYRPZHZIORZHZAZCOAFPNOHF" +
    "VEYHCILCVSMGRYRSYXYRYSIEKRGBYXYRRCRIIVYH" +
    "CIYBAGZSWEKDMIJRTHVXZIKG"

def encrypt(key: String, plaintext: String) =
  lengthened(key, plaintext.length).zip(plaintext)
    .map { case (k, p) => PolyAlphabet(k)(Alphabet.indexOf(p)) }
    .mkString

private def lengthened(key: String, length: Int) =
  LazyList.continually(key.toList).flatten.take(length).mkString

def trigramCountFormat(cipherText: String): String =
  trigramCount(cipherText)
    .toList.sortBy(_._2)
    .map { case (str, count) => s"$count $str" }
    .mkString("\n")

def trigramCount(cipherText: String) =
  Range.inclusive(2, cipherText.length / 2)
    .map(size => trigramCountSize(cipherText, size))
    .fold(Map.empty)(_ ++ _)

//    .map { case ls@h :: l => (ls.map(_._1).mkString) -> h._2 }

private def trigramCountSize(cipherText: String, size: Int) =
  cipherText.sliding(size)
    .toList.groupBy(identity)
    .view.mapValues(_.size).toMap
    .filter(_._2 > 1)

def demo(): Unit =
  println(s"-- 5.5 The Belaso cipher")
  println(s"PolyAlphabet")
  PolyAlphabet.toList.sortBy(_._1).foreach { case (_, alphabet) => println(alphabet) }
  println(s"Encrypt SAMPLE with CAB             : ${encrypt("CAB", "SAMPLE")}")
  println(s"Encrypt PLAINTEXT with MYVERYLONGKEY: ${encrypt("MYVERYLONGKEY", "PLAINTEXT")}")
  println()
  println(s"-- 5.6 The Kasiski method")
  println(trigramCountFormat(CipherText))
