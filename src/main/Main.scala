package main

/**
 * This is where the execution happens.
 */


object Main extends App {

  def calculateShare(pot: Int, odds: Float): Int = {
    val unrounded = pot / odds
    (unrounded / 10).floor.intValue() * 10
  }

  override def main(args: Array[String]) {
    val numtrial = 5000
    var roundnum = 1
    var programEnd = false
    while (!programEnd) {
      println("(round "+roundnum+")")
      Deck.shuffle()

      val cpu = Deck.dealString(2)
      val human = Deck.dealString(2)
      val flop = Deck.dealString(3)
      val turn = Deck.dealString(1)
      val river = Deck.dealString(1)

      val preflopOdds = Calculate(List(cpu, "xx xx"), "xx xx xx xx xx", numtrial)
      val flopOdds = Calculate(List(cpu, "xx xx"), flop + " xx xx", numtrial)
      val turnOdds = Calculate(List(cpu, "xx xx"), flop + " " + turn + " xx", numtrial)
      val riverOdds = Calculate(List(cpu, "xx xx"), flop + " " + turn + " " + river, numtrial)

      val oddsList = List(preflopOdds, flopOdds, turnOdds, riverOdds)
      val roundList = List("preflop", "flop", "turn", "river")
      val boardList = List("", flop, turn, river)

      var roundEnded = false

      println("player cards: " + human)
      println("cpu cards: " + cpu)
      for (i <- 0 to 3) {

        println("[" + roundList(i) + "]" + boardList.slice(0, i+1).mkString(" "))
        println("--> cpu odds: "+oddsList(i))
        var nextPhase = false
        if (!roundEnded) {
          while (!nextPhase) {
            print(">")
            val command = Console.readLine()
            if (command.contains("end")) {
              roundEnded = true
              nextPhase = true
            } else if (command.contains("pot")) {
              val pot = command.split(" ").apply(1).toInt
              println("CPU's pot share is " + calculateShare(pot, oddsList(i)))
            } else if (command.contains("next")) {
              nextPhase = true
            } else if (command.contains("quit")) {
              roundEnded = true
              nextPhase = true
              programEnd = true
            }
          }
        }
      }
      roundnum += 1
    }
  }

}
