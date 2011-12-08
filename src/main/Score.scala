package main

object Score {
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

  def apply(cards:List[Card]):List[Int] = {
    val hand = new Hand(cards.sortBy(card => card.index))
    checkfunctions.foreach { function =>
      val score = function(hand)
      if(score.isDefined) return score.get
    }
    throw new RuntimeException("illegal card combination: "+cards)
    List.empty
  }

  def checkstraightflush(hand:Hand):Option[List[Int]] = {
    if(!hand.straightflushes.isEmpty)
      Some(List(8, hand.straightflushes.last.head.lowaceindex + 1))
    else
      None
  }

  def checkquads(hand:Hand):Option[List[Int]] = {
    val quads:Option[(Int, Int)] = hand.frequencies
      .find(_._2 == 4)
    if (quads.isDefined) {
      val value:Int = quads.get._1
      Some(List(7, value) ::: hand.values.filterNot(_ == value))
    } else None
  }


  def checkfullhouse(hand:Hand):Option[List[Int]] = {
    var trips = -1; var pair = -1
    hand.frequencies.foreach { vf =>
        if (vf._2 == 3) {
          trips = List(trips, vf._1).max
          if(trips != -1 && vf._1 != trips)
            pair =  List(trips, vf._1).min
        }
        else if (vf._2 == 2)
          pair = List(pair, vf._1).max
    }
    if (trips >= 0 && pair >= 0)
      Some(List(6, trips, pair) ::: hand.kickers.diff(List(trips, pair)))
    else
      None
  }

  def checkflush(hand:Hand):Option[List[Int]] = {
    if(!hand.flushes.isEmpty) Some(List(5) ::: hand.flushes.last.reverse.map(card => card.index)) else None
  }

  def checkstraight(hand:Hand):Option[List[Int]] = {
    if(!hand.straights.isEmpty) Some(List(4, hand.straights.last.head.lowaceindex + 1)) else None
  }

  def checktrips(hand:Hand):Option[List[Int]] = {
    val trips:List[Int] = hand.frequencies
      .filter(_._2 == 3)
      .map(_._1)
    if (trips.size > 0) {
      val maxtrips = trips.max
      Some(List(3, maxtrips) ::: hand.kickers.filterNot(_ == maxtrips))
    } else None
  }

  def checktwopair(hand:Hand):Option[List[Int]] = {
    val pairs:List[Int] = hand.frequencies
      .filter(_._2 == 2)
      .map(_._1)
      .sorted
      .reverse
    if (pairs.size >= 2) {
      val twopairs = pairs.take(2)
      Some(List(2) ++ twopairs ++ hand.kickers.filterNot(twopairs.contains(_)))
    } else None
  }

  def checkpair(hand:Hand):Option[List[Int]] = {
    val pairs:List[Int] = hand.frequencies
      .filter(_._2 == 2)
      .map(_._1)
    if (pairs.size == 1) {
      val pair = pairs.max
      return Some(List(1, pair) ++ hand.kickers.filterNot(_ == pair))
    }
    if(pairs.size > 1)
        throw new RuntimeException("Score check failed, twopair/trips check not executed")
    None
  }

  def checkhighcard(hand:Hand):Option[List[Int]] = {
    Some(List(0) ::: hand.kickers)
  }
}