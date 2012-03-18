package main

/**
 * This is necessary to compare scores to each other. It compares two scores element by element until it finds one higher than the other.
 *
 */
object ScoreOrdering extends Ordering[PlayerHand] {
  def compare(a: PlayerHand, b: PlayerHand):Int = {
    val x = a.score; val y = b.score
    for(i <- 0 to (List(x.length, y.length).max - 1)) {
      val comparison = x(i).compare(y(i))
      if(comparison != 0)
        return comparison
    }
    0
  }
}

/**
 * This object stores the player hand, the community cards and the amount of times won from the other players.
 */

class PlayerHand(val handRequest:String, var hand:List[Option[Card]]=List.empty, var community:List[Option[Card]]=List.empty, var score:List[Int] = List.empty, var points:Int = 0) {
  def setCommunity(newCommunity:List[Option[Card]]) {
    community = newCommunity
  }

  def fetchHand() {
    hand = Deck.search(handRequest)                                         // Search for your hand in the deck
  }

  def calculateScore() {
    score = Score(hand.map(op => op.get) ++ community.map(op => op.get))
  }

  def addpoint() {
    points += 1
  }
  override def toString:String = handRequest
}

object Calculate {
  def apply(playerrequests:List[String], communityrequest:String, numtrial:Int) = {
    val players:List[PlayerHand] = playerrequests
      .map(request => new PlayerHand(request))                              // Make a new player for each playerrequest, a string describing the requested hand for each player.
    var ties:Int = 0
//    println("community ("+communityrequest+")")
//    print("running "+numtrial+" tests |")
    for(i <- 0 to numtrial) {
      var communitycards:List[Option[Card]] = List.empty
      while(communitycards.isEmpty ||                                   // While at least one of the cards has not been found because it has been taken already..
            communitycards.exists(op => op.isEmpty) ||
            players.exists(player => player.hand.exists(op => op.isEmpty))) {
        Deck.shuffle()
        communitycards = Deck.search(communityrequest)                  // ..search for the cards again
        players.foreach{player =>
          player.fetchHand()
          player.setCommunity(communitycards)
        }
      }
      players.foreach(player => player.calculateScore())
      val bestplayer = players.max(ScoreOrdering)                       // The player with the highest score
      val playerscores = players.map(player => player.score)
      if(playerscores.count(score => score == bestplayer.score) > 1) {  // If this player shares the same score as at least one other player..
            ties += 1                                                   // ..it evidently becomes a tie
      } else {
        bestplayer.addpoint()                                           // Otherwise, the best player earns a point
      }
//      if (i % (numtrial / 30) == 0)
//        print("-")
    }
//    println("|")
//    val playerwinprob = "%1.5f".format(players.head.points.toFloat / numtrial.toFloat)     // The amount of times won for our player divided by the total number of trials
//    val tieprob = "%1.5f".format(ties.toFloat / numtrial.toFloat)                          // The amount of ties divided by the total number of trials
//    val odds = "%1.4f".format((numtrial - players.head.points).toFloat / players.head.points.toFloat)    // Odds to compare with the pot odds
//    println("odds "+odds+" to 1, Pwin="+playerwinprob+", Ptie="+tieprob+"")
    (numtrial - players.head.points).toFloat / players.head.points.toFloat   // The odds that are returned after all the calculations, so that Main may calculate the CPU's pot share.
  }
}