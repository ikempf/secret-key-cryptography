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
    .toList.sortBy(_._2._1)
    .map { case (str, (count, positions)) => s"$count $str ${positions.mkString(" ")}" }
    .mkString("\n")

def trigramCount(cipherText: String): Map[String, (Int, List[Int])] =
  Range.inclusive(2, cipherText.length / 2)
    .map(size => trigramCountSize(cipherText, size))
    .fold(Map.empty)(_ ++ _)

private def trigramCountSize(cipherText: String, size: Int) =
  cipherText.zipWithIndex.toList.sliding(size)
    .map(l => l.map(_._1).mkString -> l.head._2)
    .toList.groupBy(_._1)
    .view.mapValues(l => l.size -> l.map(_._2)).toMap
    .filter(_._2._1 > 1)

def demo(): Unit =
  println(s"-- 5.5 The Belaso cipher")
  println(s"PolyAlphabet")
  PolyAlphabet.toList.sortBy(_._1).foreach { case (_, alphabet) => println(alphabet) }
  println(s"Encrypt SAMPLE with CAB             : ${encrypt("CAB", "SAMPLE")}")
  println(s"Encrypt PLAINTEXT with MYVERYLONGKEY: ${encrypt("MYVERYLONGKEY", "PLAINTEXT")}")
  println()
  println(s"-- 5.6 The Kasiski method")
  println(trigramCountFormat(CipherText))
