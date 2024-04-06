import ciphers.bazeries
import rng.{alphabetmixing, chaineddigitgenerator}

@main def hello(): Unit =
  chaineddigitgenerator.demo()
  println()
  bazeries.demo()
  println()
  alphabetmixing.demo()
  println()
