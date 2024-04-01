
import ciphers.bazeries
import rng.chaineddigitgenerator

@main def hello(): Unit =
  chaineddigitgenerator.demo()
  println()
  bazeries.demo()
  println()
