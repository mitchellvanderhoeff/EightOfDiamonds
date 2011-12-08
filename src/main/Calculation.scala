package main


object ScoreOrdering extends Ordering[Player] {
  def compare(a: Player, b: Player):Int = {
    val x = a.score; val y = b.score
    for(i <- 0 to (List(x.length, y.length).max - 1)) {
      val comparison = x(i).compare(y(i))
      if(comparison != 0)
        return comparison
    }
    0
  }
}

class Player(val handrequest:String, var community:List[Card]=null, var points:Int = 0) {
  def score:List[Int] =
    Score(Deck.search(handrequest) ++ community)
  def addpoint() {
    points += 1
  }
  override def toString:String = handrequest
}

object Calculate {
  def apply(playerrequests:List[String], communityrequest:String, numtrial:Int) = {
    val players:List[Player] = playerrequests
      .map(hs => new Player(hs))
    print("running "+numtrial+" tests |")
    for(i <- 0 to numtrial) {
      Deck.shuffle()
      try {
        val communitycards = Deck.search(communityrequest)
        players.foreach(_.community = communitycards)
        players.max(ScoreOrdering).addpoint()
      } catch {
        case _ =>
      }
      if (i % (numtrial / 10) == 0)
        print("-")
    }
    println("|")
    val playerwinprob = "%1.5f".format(players.head.points.toFloat / numtrial.toFloat)
    val odds = "%1.4f".format((numtrial - players.head.points).toFloat / players.head.points.toFloat)
    println("player ("+players.head+")  - community "+communityrequest+" - enemies ("+players.drop(1).mkString(", ")+")")
    println("p="+playerwinprob+", odds: "+odds+" to 1")
  }
}