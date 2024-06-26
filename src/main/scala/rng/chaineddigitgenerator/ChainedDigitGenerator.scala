package rng.chaineddigitgenerator

// Not thread safe
private class ChainedDigitGenerator(seed: List[Int]):
  private var window = Array.from(seed)

  def nextInt(): Int =
    val rn = (window.head + window.last) % 10
    window = window.tail :+ rn
    rn

private class ChainedDigitGeneratorRolling(seed: List[Int]):
  private val window = Array.from(seed)
  private var index = 0

  private def head = index % seed.size
  private def last = (index + seed.size - 1) % seed.size

  def nextInt(): Int =
    synchronized {
      val rn = (window(head) + window(last)) % 10
      window((last + 1) % seed.size) = rn
      index = index + 1
      rn
    }

private def chainedDigitGenerator(seed: List[Int]) =
  LazyList.unfold(seed)(seed => 
    val rn = (seed.head + seed.last) % 10
    Some((rn, seed.tail :+ rn))
  )

private val Count = 100
private val seed = List(3, 9, 2, 0, 5, 1, 6)

def basic(max: Int): Seq[Int] =
  val generator = ChainedDigitGenerator(seed)
  Range(0, max).map(_ => generator.nextInt())

def rolling(max: Int): Seq[Int] =
  val generatorRolling = ChainedDigitGeneratorRolling(seed)
  Range(0, max).map(_ => generatorRolling.nextInt())

def stream(max: Int): Seq[Int] =
  chainedDigitGenerator(seed).take(Count)

def demo(): Unit =
  println(s"-- 4.5.1 Chained digit generator")
  println(s"Chained digit generator basic:   ${basic(Count).mkString(", ")}")
  println(s"Chained digit generator rolling: ${rolling(Count).mkString(", ")}")
  println(s"Chained digit generator stream:  ${stream(Count).mkString(", ")}")
