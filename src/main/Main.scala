package main

object Main extends App {
  override def main(args: Array[String]) {
    Calculation.
      calculate(player("ax ax") vs "kx kx" on "xx xx xx xx xx", 10000)
  }

}