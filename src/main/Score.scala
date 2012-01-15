package main

/**
 * This is the class where a combination of five to seven cards are scored, so that we may compare several hands
 */

object Score {
  /**
   * A list of functions here is necessary, for every check has to happen in order as to not accidentally estimate a hand too low
   */
  val checkfunctions = List(
    checkstraightflush _,
    checkquads _,
    checkfullhouse _,
    checkflush _,
    checkstraight _,
    checktrips _,
    checktwopair _,
    checkpair _,
    checkhighcard _
  )
  /**
   *  This is where the actual scoring happens.
   *  The hand is run through a list of checking functions
   *  as an end result that one of them tests positive.
   */
  def apply(cards:List[Card]):List[Int] = {
    val hand = new Hand(cards.sortBy(card => card.index))
    checkfunctions.foreach { function =>
      val score = function(hand)
      if(score.isDefined) return score.get
    }
    /**
     * It should never be possible to reach this point, as the last function always returns the hand itself
    */
    throw new RuntimeException("illegal card combination: "+cards)
    List.empty
  }

  /**
   * All the check functions abide the following model
   * - Check Hand to see if it contains your combination
   * - If it does, return the monad Some(combination plus kickers), so that we may compare several hands of the same combination
   * - If it does not, return the monad None
   * This ensures that I never have to return 'null' if I cannot find the combination.
   */

  def checkstraightflush(hand:Hand):Option[List[Int]] = {
    if(!hand.straightflushes.isEmpty) {
      val bestStraightFlush = hand
        .straightflushes.map(sf => sf.head.lowaceindex)
        .max
      Some(List(8, bestStraightFlush + 1))
    } else
      None
  }

  def checkquads(hand:Hand):Option[List[Int]] = {
    val quads:Option[(Int, Int)] = hand.frequencies
      .find(_._2 == 4)                                                // i.e. if the hand contains four equally ranked cards
    if (quads.isDefined) {
      val value:Int = quads.get._1
      Some(List(7, value, hand.kickers.filterNot(_ == value).head))
    } else None
  }

  /**
   * This is a tricky one. I have to find the three cards with equal and highest ranks,
   * and after that the pair with highest rank, to create a full house.
   */

  def checkfullhouse(hand:Hand):Option[List[Int]] = {
    var trips = -1; var pair = -1           // minus one means not found yet
    hand.frequencies.foreach { vf =>
        if (vf._2 == 3) {                   // if we find three cards with equal ranks
          trips = List(trips, vf._1).max    // we keep our current trips or the new ones depending on which is higher
          /**
           * if we did find trips earlier, but not the ones we found this time
           * (which are evidently lower than our current ones, see previous line)
           * then these trips must be regarded as the additional pair to the full house
          */
          if(trips != -1 && vf._1 != trips)
            pair =  List(trips, vf._1).min
        }
        else if (vf._2 == 2)                 // now looking for pairs
          pair = List(pair, vf._1).max       // idem
    }
    if (trips >= 0 && pair >= 0)             // if we found both trips and a pair
      Some(List(6, trips, pair) ++ hand.kickers
        .filterNot(List(trips, pair).contains(_))
        .take(3))
    else
      None
  }

  /**
   * hand.flushes is simply a list of all the flushes present in the hand.
   * If it is empty, this function returns none immediately. If it is not, it returns the highest flush present.
   */

  def checkflush(hand:Hand):Option[List[Int]] = {
    if(!hand.flushes.isEmpty) {
      val sortedFlushes = hand.flushes.map(fivecards => fivecards.sortBy(_.index).reverse)
      Some(List(5) ++ sortedFlushes
        .maxBy(fivecards => fivecards.head.index)
        .map(card => card.index))
    }
    else None
  }

  /**
   * Same with straights as with flushes
   */

  def checkstraight(hand:Hand):Option[List[Int]] = {
    if(!hand.straights.isEmpty) {
     val bestStraight = hand.straights.sortBy(fivecards => fivecards.head.lowaceindex).head
     Some(List(4, bestStraight.head.lowaceindex + 1))
    }
    else None
  }

  /**
   * Same as full house, but without the pair
   */
  def checktrips(hand:Hand):Option[List[Int]] = {
    val trips:List[Int] = hand.frequencies
      .filter(_._2 == 3)
      .map(_._1)
    if (trips.size > 0) {
      val maxtrips = trips.max
      Some(List(3, maxtrips) ++ hand.kickers
        .filterNot(_ == maxtrips)
        .take(2))
    } else None
  }

  /**
   * Makes a list of pairs, then takes the two highest pairs and returns them.
   */

  def checktwopair(hand:Hand):Option[List[Int]] = {
    val pairs:List[Int] = hand.frequencies
      .filter(_._2 == 2)
      .map(_._1)
      .sorted
      .reverse
    if (pairs.size >= 2) {
      val twopairs = pairs.take(2)
      Some(List(2) ++ twopairs ++ hand.kickers
        .filterNot(twopairs.contains(_))
        .take(1))
    } else None
  }


  def checkpair(hand:Hand):Option[List[Int]] = {
    val pairs:List[Int] = hand.frequencies
      .filter(_._2 == 2)
      .map(_._1)
    if (pairs.size == 1) {
      val pair = pairs.max
      return Some(List(1, pair) ++ hand.kickers
        .filterNot(_ == pair)
        .take(4))
    }
    if(pairs.size > 1)

    /**
     * This point should not ever be reached, as more than one pair should have been
     * picked up by checktwopair() or the likes
     */
        throw new RuntimeException("Score check failed, twopair/trips check not executed")
    None
  }

  /**
   * This just returns the hand itself as kickers, seeing as all the previous checks for higher hands failed.
   */

  def checkhighcard(hand:Hand):Option[List[Int]] = {
    Some(List(0) ++ hand.kickers.take(5))
  }
}