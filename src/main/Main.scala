package main

/**
 * This is where the execution happens.
 */

object Main extends App {
  override def main(args: Array[String]) {
    Deck.shuffle()
    val cpu = Deck.deal(2)
    val player = Deck.deal(2)
    val flop = Deck.deal(3)
    val turn = Deck.deal(1)
    val river = Deck.deal(1)

    val cpuString = cpu.mkString(" ")
    val playerString = player.mkString(" ")
    val flopString = flop.mkString(" ")
    val turnString = turn.mkString(" ")
    val riverString = river.mkString(" ")

    println("cpu: "+cpuString)
    println("player: "+playerString)
    println()

    Calculate(
      List(cpuString, "xx xx"),
      "xx xx xx xx xx",
      10000)
    println()

    Calculate(
      List(cpuString, "xx xx"),
      flopString + " xx xx",
      10000)
    println()

    Calculate(
      List(cpuString, "xx xx"),
      flopString + " " + turnString + " xx",
      10000)
    println()

    Calculate(
      List(cpuString, "xx xx"),
      flopString + " " + turnString + " " + riverString,
      10000)
    println()
  }

}
