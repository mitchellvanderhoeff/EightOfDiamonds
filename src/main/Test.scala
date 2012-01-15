package main

/**
 * Very simple case class to keep count of a certain object
 */
class Countable[T](val content:T = null, var count:Int = 0) {
  def plusone() {
    count += 1
  }
  override def toString = content.toString + ": " + count.toString
}

/**
 * This class is very important, because it can spot errors in the code.
 * I have used this class many times when stumbling upon a problem that cannot be solved by looking at the code.
 */

object Test{

  /**
   * This is to check whether my check functions are a 100% consistent. The proportions are precalculated.
   */

  def calculateScoreDistribution(numtrial:Int) {
    val checkproportions:Map[Int, Double] = Map(
    8 -> 0.000311,
    7 -> 0.00168,
    6 -> 0.0260,
    5 -> 0.0303,
    4 -> 0.0462,
    3 -> 0.0483,
    2 -> 0.235,
    1 -> 0.438,
    0 -> 0.174)

    val distribution:Array[Countable[Int]] = (0 to 8).map(new Countable[Int](_)).toArray
    print("running "+numtrial+" tests |")
    for(i <- 1 to numtrial) {
      Deck.shuffle()
      val cards = Deck.deal(7)
      distribution(Score(cards)(0)).plusone()
      if (i % (numtrial / 10) == 0)
        print("-")
    }
    println("|")
    println("[distribution]")
    for(kv <- distribution.reverse) {
      val proportion = (kv.count.toDouble) / (numtrial.toDouble)
      val discrepancy = proportion / checkproportions(kv.content)
      println("score "+kv.content+" => "+"%1.6f".format(proportion)+" (discrepancy: "+"%1.3f".format(100*(discrepancy-1))+"%)")
    }
    println("total ("+(distribution.map(countable => countable.count).sum)+")")
  }


  /**
   * Test every kind of combination separately, and print the results.
   */
  def testScores() {
    val pairtostraightflush = List(
      "4d 6d 2h tc jd 3d qs",
      "3d 3c 5h th jc qs 2c",
      "5h 5d ks 2d kh 2s 6h",
      "7s 7h 7d ah 3s kh 8c",
      "ad 2c 3h 4h 5s 9s 9c",
      "kc tc qc ac 2c as ad",
      "6h 6s 6d 4s 4d 5c 7c",
      "9s 9c 9h 9d as ks 4c",
      "4c 5c 6c 7c 8c 9s ts"
    )
    pairtostraightflush.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.searchRaw(request)
        val score = Score(cards)
        println(cards + "  Score: " + score)
    }
  }

  /**
   * Test several kinds of straights
   */

  def testStraights() {
    val straights = List(
      "3h 3d 4c 5s 6s 7c 8h",
      "4c jh td 4s qh kh as",
      "4c 5h 6d 7c 8h kh as",
      "4c 5h 6d 6c 7h 8h as",
      "2c th 3d 4c 5h kh as")
    straights.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.searchRaw(request)
        val score = Score(cards)
        println(cards + " => " + score)
    }
  }

  /**
   * Test several kinds of flushes
   */

def testFlushes() {
    val flushes = List(
      "ad 2d 3d 4d 5d kh as",
      "6h 5h td jh qh kh as",
      "4s 5h 6s ts 8s ks as",
      "2c tc 3c 4c 5c kc jc")
    flushes.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.searchRaw(request)
        val score = Score(cards)
        println(cards + " => " + score)
  }
}

  /**
   * Test several kinds of straight flushes
   */

  def testStraightFlushes() {
    val straightFlushes = List(
      "ad 2d 3d 4d 5d kh as",
      "6h 5h 6d 7h 8h 9h ah",
      "4s 5s 6s 7s 8s 9s ts",
      "2c tc 3c 4c 5c kc ac")
    straightFlushes.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.searchRaw(request)
        val score = Score(cards)
        println(cards + " => " + score)
    }
  }
}