package ciphers.belazo

private val Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
private val PolyAlphabet =
  List
    .iterate(Alphabet, 26)(alphabet => alphabet.tail :+ alphabet.head)
    .groupBy(_.head)
    .view
    .mapValues(_.head)
    .toMap

def encrypt(key: String, plaintext: String) =
  lengthened(key, plaintext.length).zip(plaintext)
    .map { case (k, p) => PolyAlphabet(k)(Alphabet.indexOf(p)) }
    .mkString

private def lengthened(key: String, length: Int) =
  LazyList.continually(key.toList).flatten.take(length).mkString

def demo(): Unit =
  println(s"-- 5.5 The Belaso cipher")

  println(s"PolyAlphabet")
  PolyAlphabet.toList.sortBy(_._1).foreach { case (_, alphabet) => println(alphabet) }
  println(s"Encrypt SAMPLE with CAB             : ${encrypt("CAB", "SAMPLE")}")
  println(s"Encrypt PLAINTEXT with MYVERYLONGKEY: ${encrypt("MYVERYLONGKEY", "PLAINTEXT")}")
