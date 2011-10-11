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

object CardSlotOrdering extends Ordering[CardSlot] {
  def compare(a: CardSlot, b: CardSlot):Int = {
    val astr = a.rawrequest.toLowerCase
    val bstr = b.rawrequest.toLowerCase
    bstr.count(_ == 'x').compare(astr.count(_ == 'x'))
  }
}

class Player(val handrequest:String, val communityslots:Array[CardSlot], var points:Int = 0) {
  val handreqlist = handrequest.split(" ")
  val handslots:Array[CardSlot] = Array.tabulate[CardSlot](handreqlist.length)(i => new CardSlot(handreqlist(i)))
  def score:List[Int] =
    Score((handslots ++ communityslots)
      .map(_.content)
      .toList)
  def addpoint {
    points += 1
  }
  override def toString:String = handrequest + ": " + points
}


object Calculation {
  def calculate(playerrequests:List[String], communityrequest:String, numtrial:Int) = {
    val communityreqlist = communityrequest.split(" ")
    val communityslots:Array[CardSlot] = Array.tabulate[CardSlot](communityreqlist.length)(i => new CardSlot(communityreqlist(i)))
    val players:List[Player] = playerrequests
      .map(hs => new Player(hs, communityslots))
    val playerslots:Array[CardSlot] = players.flatMap(player => player.handslots).toArray
    val totalslotssorted:Array[CardSlot] = (playerslots ++ communityslots).sorted(CardSlotOrdering)
    print("running "+numtrial+" tests |")
    for(i <- 0 to numtrial) {
      Deck.shuffle()
      totalslotssorted.foreach(cardslot => Deck.fillslot(cardslot))
      players.max(ScoreOrdering).addpoint
      if (i % (numtrial / 10) == 0)
        print("-")
    }
    println("|")
    val playerwinprob = "%1.5f".format(players.head.points.toFloat / numtrial.toFloat)
    val odds = "%1.4f".format((numtrial - players.head.points).toFloat / players.head.points.toFloat)
    print("player "+playerrequests.head+"  - community "+communityrequest+" - enemies "+players.drop(1).mkString(" "))
    println("p="+playerwinprob+", odds: "+odds+" to 1")
  }
}