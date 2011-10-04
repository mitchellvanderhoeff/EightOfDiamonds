package main

case class CardsData(values:List[Int], frequencies:List[(Int, Int)], sequence:Option[Int], samesuits:Option[Suit])

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
    val cardsdata = buildcardsdata(cards)
    val results:List[List[Int]] = functions.flatMap(_(cardsdata))
    results(results.map(_(0)).indexOf(results.map(_(0)).max))
  }

  def getfrequencies(values:List[Any]):List[(Any, Int)] = {
    values.distinct.map((value) => (value, values.count(_ == value)))
  }

  def getvalues(cards:List[Card]):List[Int] = cards.map(_.rank.index).sortWith(_ > _)

  def getsequence(values:List[Int]):Option[Int] = {
    val revvalues = values.reverse
    val length = values.length
    val identifiers = (0.to(length-5))
      .map(checkindex => revvalues
        .slice(checkindex, 5+checkindex)
        .foldLeft(revvalues(checkindex)-1)
        ((a, b) => if(a + 1 == b) b else Int.MinValue)
    )
//    println("     revvalues = "+revvalues+" length="+length+", identifiers = "+identifiers.map(x => if(x < 0) -1 else x))
    val foundindex = identifiers.lastIndexWhere(_ >= 0)
    if(foundindex != -1) {
      Some(revvalues(foundindex)+1)
    } else {
      if(values.contains(12))
        getsequence(values
          .map(x => if(x == 12) -1 else x)
          .sortWith(_ > _)
        )
      else
        None
    }
  }

  def getsamesuits(cards:List[Card]):Option[Suit] = {
    val flushsuit = getfrequencies(cards.map(_.suit)).find(_._2 == 5)
    if(flushsuit.isDefined)
      Some(flushsuit.get._1.asInstanceOf[Suit])
    else
      None
  }

  def buildcardsdata(cards:List[Card]):CardsData = {
    val values:List[Int] = getvalues(cards)
    val frequencies:List[(Int, Int)] = getfrequencies(values).asInstanceOf[List[(Int, Int)]]
    val sequence:Option[Int] = getsequence(values)
    val samesuits:Option[Suit] = getsamesuits(cards)
    CardsData(values, frequencies, sequence, samesuits)
  }

  def checkstraightflush(cd:CardsData):Option[List[Int]] = {
     if(cd.samesuits.isDefined && cd.sequence.isDefined) Some(List(8, cd.sequence.get)) else None
  }

  def checkquads(cd:CardsData):Option[List[Int]] = {
    val quads:List[Int] = cd.frequencies
      .filter(_._2 == 4)
      .map(_._1)
    if (quads.size > 0) {
      val value:Int = quads.max
      Some(List(7, value) ::: cd.values.filter(_ != value))
    } else None
  }


  def checkfullhouse(cd:CardsData):Option[List[Int]] = {
    var trips = -1; var pair = -1
    cd.frequencies.foreach { vf =>
        if (vf._2 == 3) trips = List(trips, vf._1).max
        if (vf._2 == 2) pair = List(pair, vf._1).max
    }
    if (trips >= 0 && pair >= 0)
      Some(List(6, trips, pair) ::: cd.values.filter(!List(trips, pair).contains(_)))
    else
      None
  }

  def checkflush(cd:CardsData):Option[List[Int]] = {
    if(cd.samesuits.isDefined) Some(List(5) ::: cd.values) else None
  }

  def checkstraight(cd:CardsData):Option[List[Int]] = {
    if(cd.sequence.isDefined) Some(List(4, cd.sequence.get)) else None
  }

  def checktrips(cd:CardsData):Option[List[Int]] = {
    val trips:List[Int] = cd.frequencies
      .filter(_._2 == 3)
      .map(_._1)
    if (trips.size > 0) {
      val value = trips.max
      Some(List(3, value) ::: cd.values.filter(_ != value))
    } else None
  }

  def checktwopair(cd:CardsData):Option[List[Int]] = {
    val pairs:List[Int] = cd.frequencies
      .filter(_._2 == 2)
      .map(_._1)
      .sortWith(_ > _)
    if (pairs.size >= 2) {
      val twopairs = pairs.take(2)
      Some(List(2) ::: twopairs ::: cd.values.filter(!twopairs.contains(_)))
    } else None
  }

  def checkpair(cd:CardsData):Option[List[Int]] = {
    val pairs:List[Int] = cd.frequencies
      .filter(_._2 == 2)
      .map(_._1)
    if (pairs.size == 1) {
      val value = pairs(0)
      Some(List(1, value) ::: cd.values.filter(_ != value))
    } else None
  }

  def checkhighcard(cd:CardsData):Option[List[Int]] = {
    Some(List(0) ::: cd.values)
  }
}