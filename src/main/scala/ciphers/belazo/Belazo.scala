package ciphers.belazo

import scala.util.chaining.*

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

private val Periods = List(4, 5, 6, 7, 8)

case class TrigramInfo(count: Int, positions: List[Int])
case class PeriodsModulos(periods: List[PeriodModulos])
case class PeriodModulos(period: Int, modulos: List[Int])
case class PeriodRepeats(period: Int, repeats: Int)

private def polyAlphabetFormat =
  PolyAlphabet.toList.sortBy(_._1)
    .map { case (name, alphabet) => s"$name - $alphabet" }

def encrypt(key: String, plaintext: String) =
  lengthened(key, plaintext.length).zip(plaintext)
    .map { case (k, p) => PolyAlphabet(k)(Alphabet.indexOf(p)) }
    .mkString

private def lengthened(key: String, length: Int) =
  LazyList.continually(key.toList).flatten.take(length).mkString

private def trigramCountFormat(counts: List[(String, TrigramInfo)]): String =
  counts
    .map { case (trigram, info) => s"${info.count} ${trigram} ${info.positions.mkString(" ")}" }
    .mkString("\n")

def trigramCount(cipherText: String): Map[String, TrigramInfo] =
  Range.inclusive(2, cipherText.length / 2)
    .map(size => trigramCountSize(cipherText, size))
    .fold(Map.empty)(_ ++ _)

private def trigramSorted(counts: Map[String, TrigramInfo]) =
  counts.toList.sortBy(info => info._2.count).reverse

private def trigramCountSize(cipherText: String, size: Int): Map[String, TrigramInfo] =
  cipherText.zipWithIndex.sliding(size)
    .map(chars => chars.map(_._1).mkString -> chars.head._2)
    .toList.groupBy(_._1)
    .view
    .mapValues(l => l.map(_._2))
    .mapValues(l => TrigramInfo(l.size, l))
    .toMap
    .filter(_._2._1 > 1)

private def computePeriodModulos(counts: List[(String, TrigramInfo)], periods: List[Int]) =
  counts
    .map { case (trigram, info) =>
      (trigram, info, periodModulos(periods, info))
    }

private def periodModulos(periods: List[Int], info: TrigramInfo) =
  PeriodsModulos(periods.map(period =>
    PeriodModulos(period, info.positions.map(pos => pos % period))
  ))

private def periodModulosFormat(counts: List[(String, TrigramInfo, PeriodsModulos)], periods: List[Int]) =
  counts
    .take(3)
    .map { case (trigram, info, periodsModulos) =>
      val periodLines =
        periodsModulos.periods
          .map(periodModulos => s"${periodModulos.period} - ${periodModulos.modulos.mkString(" ")}")
          .mkString("\n")

      s"$trigram ${info.positions.mkString(" ")}\n$periodLines"
    }
    .mkString("\n")

def inferPeriod(counts: List[(String, TrigramInfo, PeriodsModulos)]) =
  counts
    .map { case (trigram, info, periodsModulos) =>
      periodsModulos.periods
        .map(periodModulo => periodModulo.period -> periodModulo.modulos.toSet.size)
        .toMap
    }
    .fold(Map.empty)((a, b) =>
      (a.keySet ++ b.keySet).map { key =>
        key -> (a.getOrElse(key, 0) + b.getOrElse(key, 0))
      }.toMap
    )
    .toList
    .sortBy(_._2)
    .map{ case (period, repeats) => PeriodRepeats(period, repeats)}

private def inferPeriodFormat(periods: List[PeriodRepeats]) =
  periods
    .map(periodRepeats => s"${periodRepeats.period} -> ${periodRepeats.repeats}")
    .mkString("\n")


def demo(): Unit =
  println(s"-- 5.5 The Belaso cipher")
  println(s"PolyAlphabet")
  println(polyAlphabetFormat.mkString("\n"))
  println(s"Encrypt SAMPLE with CAB             : ${encrypt("CAB", "SAMPLE")}")
  println(s"Encrypt PLAINTEXT with MYVERYLONGKEY: ${encrypt("MYVERYLONGKEY", "PLAINTEXT")}")
  println()
  println(s"-- 5.6 The Kasiski method")
  val trigramCounts = trigramCount(CipherText).pipe(trigramSorted)
  println("Trigram counts and positions")
  println(trigramCountFormat(trigramCounts))
  println()
  println("Frequent trigram period modulos")
  val periodModulos = computePeriodModulos(trigramCounts, Periods)
  println(periodModulosFormat(periodModulos, Periods))
  println()
  println("Inferred key period")
  val inferredPeriod = inferPeriod(periodModulos)
  println(inferPeriodFormat(inferredPeriod))
