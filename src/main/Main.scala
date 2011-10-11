package main

object Main extends App {
  override def main(args: Array[String]) {
    Calculation.
      calculate(List("ax ax", "kx kx"), "xx xx xx xx xx", 10000)
  }

}