package main


class ScoreOrdering extends Ordering[Player] {
  def compare(a: Player, b: Player):Int = {
    val x = a.scorecards; val y = b.scorecards
    for(i <- 0 to (List(x.length, y.length).max - 1)) {
      val comparison = x(i).compare(y(i))
      if(comparison != 0)
        return comparison
    }
    0
  }
}

class CardString(val string:String, val tail:CardString = null) {
  def toList:List[CardString] = if(tail == null) List(this) else tail.toList ++ List(this)

  def vs(newstring:String):CardString = new HandString(newstring, this)
  def on(newstring:String):CardString = new CommunityString(newstring, this)

  implicit def string2handstring(req: String): HandString = {
    new HandString(req)
  }

  override def toString:String = string
}

class Player(val handrequest:String, var community:List[Card] = List.empty, var points:Int = 0) {
  def scorecards:List[Int] =
    Score(Deck.search(handrequest) ::: community)
  def addpoint {
    points += 1
  }
  override def toString:String = handrequest
}
class HandString(string:String, tail:CardString = null) extends CardString(string, tail)
class CommunityString(string:String, tail:CardString = null) extends CardString(string, tail)


object Calculation {
  def player(string:String):CardString = new CardString(string)

  val scoreordering = new ScoreOrdering

  def calculate(request:CardString, numtrial:Int) = {
    val requestlist = request.toList
    val players:List[Player] = requestlist
      .filter(cs => cs.isInstanceOf[HandString])
      .map(hs => new Player(hs.toString))
    val communityrequest = requestlist.find(cs => cs.isInstanceOf[CommunityString]).get
    print("running "+numtrial+" tests |")
    for(i <- 0 to numtrial) {
      Deck.shuffle()
      val communitycards = Deck.search(communityrequest.toString)
      players.foreach(_.community = communitycards)
      players.max(scoreordering).addpoint
      if (i % (numtrial / 10) == 0)
        print("-")
    }
    println("|")
    val playerwinprob = "%1.5f".format(players.head.points.toFloat / numtrial.toFloat)
    val odds = "%1.4f".format((numtrial - players.head.points).toFloat / players.head.points.toFloat)
    print("player "+requestlist.head+"  - community "+communityrequest+" - enemies "+players.drop(1).mkString(" "))
    println("p="+playerwinprob+", odds: "+odds+" to 1")
  }
}