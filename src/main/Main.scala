package main

import util.Random

object Main extends App {
  override def main(args: Array[String]) {
    calculate(
      "as;ac",
      "jd;td",
      "xd;xd;ad;xx;xx",
      100000)
  }

  def calculate(playerstring:String, foestring:String, communitystring:String, totaltests:Int):Float = {
    var playerpoints = 0;
    var foepoints = 0;
    var testsdone = 0;
    print("running "+totaltests+" tests |")
    while (testsdone < totaltests) {
      Deck.shuffle()
      val player = Deck.search(playerstring)
      val foe = Deck.search(foestring)
      val community = Deck.search(communitystring)
      val playerscore = Score(player ::: community)
      val foescore = Score(foe ::: community)
      comparelists(playerscore, foescore) match {
        case -1 => foepoints += 1
        case 1 => playerpoints += 1
        case _ =>
      }
      if (testsdone % (totaltests / 10) == 0)
        print("-")
      testsdone += 1
    }
    println("|")
    val playerwinprob = "%1.5f".format(playerpoints.toFloat / testsdone.toFloat)
    val odds = "%1.4f".format(foepoints.toFloat / playerpoints.toFloat)
    println("player("+playerstring+") - community cards("+communitystring+") - foe("+foestring+")")
    println("p="+playerwinprob+", odds: "+odds+" to 1")
    foepoints.toFloat / playerpoints.toFloat
  }

  def calculateByScore(f:List[Int] => Boolean, tests:Int) {
    var n = 0;
    var total = 0;
    print("running "+tests+" totaltests |")
    while (total < tests) {
      Deck.shuffle()
      val cards = Deck.deal(7)
      val score = Score(cards)
      if (f(score))
        n += 1
      if (total % (tests / 10) == 0)
        print("-")
      total += 1
    }
    println("|")
    println("result: " + n + "/" + total + ", p=" + (n.toFloat / total.toFloat) * 100 + "%")
  }

  private def comparelists(one:List[Int], other:List[Int]):Int = {
    for(i <- 0 to (List(one.length, other.length).max - 1)) {
      if(one(i) < other(i))
        return -1
      else if(one(i) > other(i))
        return 1
    }
    0
  }

  def testScores() {
    val pairtostraightflush = List(
      "4d;6d;2h;tc;jd;3d;qs",
      "3d;3c;5h;th;jc;qs;2c",
      "5h;5d;ks;2d;kh;2s;6h",
      "7s;7h;7d;ah;3s;kh;8c",
      "ad;2c;3h;4h;5s;9s;9c",
      "kc;tc;qc;ac;2c;as;ad",
      "6h;6s;6d;4s;4d;5c;7c",
      "9s;9c;9h;9d;as;ks;4c",
      "4c;5c;6c;7c;8c;9s;ts")
    pairtostraightflush.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.search(request)
        val score = Score(cards)
        println(cards + "; Score: " + score)
    }
  }

  def testStraights() {
    val straights = List(
      "ad;2c;3h;4h;5c;kh;as",
      "4c;5h;td;jc;qh;kh;as",
      "4c;5h;6d;7c;8h;kh;as",
      "2c;th;3d;4c;5h;kh;as")
    straights.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.search(request)
        val score = Score(cards)
        println(cards + "; Score: " + score)
    }
  }
}