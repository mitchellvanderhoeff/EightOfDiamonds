package main

import java.io.{FileWriter, File}


/**
 * This is where the execution happens.
 */


object Main extends App {

  var stringBuffer: StringBuilder = null


  /*
   * How much of the pot is the CPU permitted to bet?
   * This is calculated by dividing the pot amount by the odds permitted by the CPU.
   * After this, the value is rounded to tens.
   */
  def calculateShare(pot: Int, odds: Float): Int = {
    val unrounded = pot / odds
    (unrounded / 10).floor.intValue() * 10
  }

  /*
   * This function prints the message as well as appends the message to the string buffer, i.e. prepare for it to be written to the file.
   */

  def log(msg: String) {
    println(msg)
    stringBuffer.append(msg+"\n")
  }

  override def main(args: Array[String]) {
    stringBuffer = new StringBuilder()
    /*
    The filename has to be unique, so I've chosen to have it based on the currentTimeMillis, which is always unique.
     */
    val filename = "EoD-" + (System.currentTimeMillis()) + ".txt"
    val file: File = new File(filename)
    file.createNewFile()
    val fileWriter: FileWriter = new FileWriter(file)

    val numtrial = 5000
    var roundnum = 1
    var programEnd = false
    while (!programEnd) {
      log("(round " + roundnum + ")")
      Deck.shuffle()

      /*
       * All the cards are pre-dealt here...
       */

      val cpu = Deck.dealString(2)
      val human = Deck.dealString(2)
      val flop = Deck.dealString(3)
      val turn = Deck.dealString(1)
      val river = Deck.dealString(1)

      Deck.shuffle()

      /*
       * ...and all the odds precalculated
       */

      val preflopOdds = Calculate(List(cpu, "xx xx"), "xx xx xx xx xx", numtrial)
      val flopOdds = Calculate(List(cpu, "xx xx"), flop + " xx xx", numtrial)
      val turnOdds = Calculate(List(cpu, "xx xx"), flop + " " + turn + " xx", numtrial)
      val riverOdds = Calculate(List(cpu, "xx xx"), flop + " " + turn + " " + river, numtrial)

      /*
       * Lists for every phase: the CPU's odds for each round, the name of each round and the board(which cards are shown) for each round.
       */
      val oddsList = List(preflopOdds, flopOdds, turnOdds, riverOdds)
      val roundList = List("preflop", "flop", "turn", "river")
      val boardList = List("", flop, turn, river)

      var roundEnded = false

      log("player cards: " + human)
      stringBuffer.append("cpu cards: " + cpu + "\n")
      /*
       * Loop from 0 to 3, meaning 4 phases: preflop, flop, turn and river
       */
      for (i <- 0 to 3) {
        /*
         * This takes the string of all the previous phase and this one, and joins them.
         */
        log("[" + roundList(i) + "]" + boardList.slice(0, i + 1).mkString(" "))
        //        println("--> cpu odds: "+oddsList(i))
        var nextPhase = false
        /*
         * if roundEnded is true, this will be ignored
         */
        if (!roundEnded) {
          /*
           * if nextPhase is true once, the while loop will be skipped but at the beginning of the next phase, nextPhase will be set to false again.
           */
          while (!nextPhase) {
            print(">")
            val command = Console.readLine()
            /*
             * The command 'end' is used to end the current round.
             */
            if (command.contains("end")) {
              roundEnded = true
              nextPhase = true
            /*
             * The command 'write' is used to write text to the file.
             */
            } else if (command.contains("write")){
              val msg = command.replace("write ", "")
              stringBuffer.append(msg + "\n")
            /*
             * The command 'pot' is used to calculate the allowed bet/raise for the CPU. The calculateShare() function is used for this purpose.
             */
            } else if (command.contains("pot")) {
              val pot = command.split(" ").apply(1).toInt
              println("CPU's pot share is " + calculateShare(pot, oddsList(i)))
            /*
             * The command 'next' is used to skip to the next round, e.g. when both players check.
             */
            } else if (command.contains("next")) {
              nextPhase = true
            /*
             * The command 'quit' is used to exit the program.
             */
            } else if (command.contains("quit")) {
              roundEnded = true
              nextPhase = true
              programEnd = true
            }
          }
        }
      }
      /*
       * Now, to compare the scores and to see which of the two wins.
       */
      Deck.shuffle()
      val humanscore = Score(Deck.searchRaw(human + " " + flop + " " + turn + " " + river))
      Deck.shuffle()
      val cpuscore = Score(Deck.searchRaw(cpu + " " + flop + " " + turn + " " + river))
      /*
       * Stolen from ScoreOrdering
       */
      val winner = {
        var result = 0
        for (i <- 0 to (List(humanscore.length, cpuscore.length).max - 1)) {
          if (result == 0) {
            val comparison = humanscore(i).compare(cpuscore(i))
            if (comparison != 0)
              result = comparison
          }
        }
        result
      }
      /*
       * Reveal the cards to the player, and reveal the winner of the round.
       */
      println("player cards: " + human)
      println("cpu cards: " + cpu)
      log(
        winner match {
          case 1 => "player wins!"
          case 0 => "tie!"
          case -1 => "cpu wins!"
          case _ => "error"
        })
      println()
      stringBuffer.append("")
      roundnum += 1

    }
    /*
     * Finally, the entire stringBuffer, that has been built up throughout the game, is dumped to the file here.
     */
    fileWriter.write(stringBuffer.toString())
    fileWriter.close()
  }

}
