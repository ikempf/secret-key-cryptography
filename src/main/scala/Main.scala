import ciphers.bazeries
import ciphers.belazo
import rng.{alphabetmixing, chaineddigitgenerator}

@main def hello(): Unit =
  chaineddigitgenerator.demo()
  println()
  bazeries.demo()
  println()
  alphabetmixing.demo()
  println()
  belazo.demo()
