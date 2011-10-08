package main

object Score {
  val functions =  List(
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
    val hand = new Hand(cards.sortWith(c1, c2 => c1.rank.value > c2.rank.value))
    for(i <- 0 to 8) {
      val score = functions(i)(hand)
      if(score.isDefined) return score
    }
  }

  def checkstraightflush(hand:Hand):Option[List[Int]] = {
    for()
    None
  }

  def checkquads(hand:Hand):Option[List[Int]] = {
    val quads:List[Int] = hand.frequencies
      .filter(_._2 == 4)
      .map(_._1)
    if (quads.size > 0) {
      val value:Int = quads.max
      Some(List(7, value) ::: hand.values.diff(List(value)))
    } else None
  }


  def checkfullhouse(hand:Hand):Option[List[Int]] = {
    var trips = -1; var pair = -1
    hand.frequencies.foreach { vf =>
        if (vf._2 == 3) {
          trips = List(trips, vf._1).max
          if(trips >= 0 && vf._1 != trips)
            pair =  List(trips, vf._1).min
        }
        else if (vf._2 == 2)
          pair = List(pair, vf._1).max
    }
    if (trips >= 0 && pair >= 0)
      Some(List(6, trips, pair) ::: hand.values.diff(List(trips, pair)))
    else
      None
  }

  def checkflush(hand:Hand):Option[List[Int]] = {
    if(hand.samesuits.isDefined) Some(List(5) ::: hand.values) else None
  }

  def checkstraight(hand:Hand):Option[List[Int]] = {
    if(hand.sequence.isDefined) Some(List(4, hand.sequence.get)) else None
  }

  def checktrips(hand:Hand):Option[List[Int]] = {
    val trips:List[Int] = hand.frequencies
      .filter(_._2 == 3)
      .map(_._1)
    if (trips.size > 0) {
      val value = trips.max
      Some(List(3, value) ::: hand.values.filter(_ != value))
    } else None
  }

  def checktwopair(hand:Hand):Option[List[Int]] = {
    val pairs:List[Int] = hand.frequencies
      .filter(_._2 == 2)
      .map(_._1)
      .sortWith(_ > _)
    if (pairs.size >= 2) {
      val twopairs = pairs.take(2)
      Some(List(2) ::: twopairs ::: hand.values.filter(!twopairs.contains(_)))
    } else None
  }

  def checkpair(hand:Hand):Option[List[Int]] = {
    val pairs:List[Int] = hand.frequencies
      .filter(_._2 == 2)
      .map(_._1)
    if (pairs.size == 1) {
      val value = pairs(0)
      Some(List(1, value) ::: hand.values.filter(_ != value))
    } else None
  }

  def checkhighcard(hand:Hand):Option[List[Int]] = {
    Some(List(0) ::: hand.values)
  }
}