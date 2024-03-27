package rng

// Not thread safe
class ChainedDigitGenerator(seed: List[Int]):
  private var window = Array.from(seed)

  def nextInt(): Int =
    val rn = (window.head + window.last) % 10
    window = window.tail :+ rn
    rn

class ChainedDigitGeneratorRolling(seed: List[Int]):
  private val window = Array.from(seed)
  private var index = 0

  private def head = index % seed.size
  private def last = (index + seed.size - 1) % seed.size

  def nextInt(): Int =
    val rn = (window(head) + window(last)) % 10
    window((last + 1) % seed.size) = rn
    index = index + 1
    rn

def chainedDigitGenerator(seed: List[Int]): LazyList[Int] =
  LazyList.unfold(seed)(seed => {
    val rn = (seed.head + seed.last) % 10
    Some((rn, seed.tail :+ rn))
  })


def demo(): Unit =
  val Count = 100
  val seed = List(3, 9, 2, 0, 5, 1, 6)
  val generator = ChainedDigitGenerator(seed)
  val firstHundred = Range(0, Count).map(_ => generator.nextInt())
  val generatorRolling = ChainedDigitGeneratorRolling(seed)
  val firstHundredRolling = Range(0, Count).map(_ => generatorRolling.nextInt())
  val firstHundredStream = chainedDigitGenerator(seed).take(Count)
  println(s"Chained digit generator       : ${firstHundred.mkString(", ")}")
  println(s"Chained digit generator atomic: ${firstHundredRolling.mkString(", ")}")
  println(s"Chained digit generator stream: ${firstHundredStream.mkString(", ")}")
