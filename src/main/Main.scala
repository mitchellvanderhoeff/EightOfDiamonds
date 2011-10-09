package main


class Countable[T](val element:T, var count:Int) {
  def this(element:T) = this(element, 0)
  def plusone() {
    count += 1
  }
  override def toString = element.toString + ": " + count.toString
}


object Main extends App {
  override def main(args: Array[String]) {
//    calculateScoreDistribution(400000)
//    testFlushes()
//    println
    testStraights()
  }



  def calculate(playerstring:String, foestring:String, communitystring:String, numtrial:Int):Float = {
    var playerpoints = 0;
    var foepoints = 0;
    print("running "+numtrial+" tests |")
    for(i <- 0 to numtrial) {
      Deck.shuffle()
      val player = Deck.search(playerstring)
      val foe = Deck.search(foestring)
      val community = Deck.search(communitystring)
      val playerscore = Score(player ::: community)
      val foescore = Score(foe ::: community)
//      println(ListDisplay(player)+" - "+ListDisplay(community)+" - "+ListDisplay(foe))
      MList.compareByElements[Int](playerscore, foescore) match {
        case -1 =>
          foepoints += 1
//          println("player "+ListDisplay(playerscore)+", foe* "+ListDisplay(foescore))
        case 1 =>
          playerpoints += 1
//          println("player* "+ListDisplay(playerscore)+", foe "+ListDisplay(foescore))
        case 0=>
//          println("player "+ListDisplay(playerscore)+", foe "+ListDisplay(foescore))
      }
      if (i % (numtrial / 10) == 0)
        print("-")
    }
    println("|")
    val playerwinprob = "%1.5f".format(playerpoints.toFloat / numtrial.toFloat)
    val odds = "%1.4f".format(foepoints.toFloat / playerpoints.toFloat)
    println("player("+playerstring+") - community cards("+communitystring+") - foe("+foestring+")")
    println("p="+playerwinprob+", odds: "+odds+" to 1")
    foepoints.toFloat / playerpoints.toFloat
  }

  def calculateScore(f:List[Int] => Boolean, tests:Int) {
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

  def calculateScoreDistribution(numtrial:Int) {
    val checkpercentages:Map[Int, Double] = Map(
    8 -> 0.0311,
    7 -> 0.168,
    6 -> 2.60,
    5 -> 3.03,
    4 -> 4.62,
    3 -> 4.82,
    2 -> 23.5,
    1 -> 43.8,
    0 -> 17.4)

    var distribution:Array[Countable[Int]] = (0 to 8).map(new Countable[Int](_)).toArray
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
      val percentage = (kv.count.toFloat) / (numtrial.toFloat) * 100
      val percentagediff = percentage - checkpercentages(kv.element)
      val sign = if(percentagediff > 0) "+" else ""
      println("score "+kv.element+" => "+"%1.3f".format(percentage)+"% ("+sign+"%1.3f".format(percentagediff)+"%)")
    }
    println("total ("+(distribution.map(countable => countable.count).sum)+")")
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
      "3x;3x;4x;5x;6x;7x;8x",
      "4c;5h;td;jc;qh;kh;as",
      "4c;5h;6d;7c;8h;kh;as",
      "2c;th;3d;4c;5h;kh;as")
    straights.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.search(request)
        val score = Score(cards)
        println(ListDisplay(cards) + " => " + ListDisplay(score))
    }
  }

def testFlushes() {
    val flushes = List(
      "ad;2d;3d;4d;5d;kh;as",
      "6h;5h;td;jh;qh;kh;as",
      "4s;5h;6s;ts;8s;ks;as",
      "2c;tc;3c;4c;5c;kc;jc")
    flushes.foreach {
      request =>
        Deck.shuffle()
        val cards = Deck.search(request)
        val score = Score(cards)
        println(ListDisplay(cards) + " => " + ListDisplay(score))
    }
  }
}