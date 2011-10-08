package main

case class Hand(cards:List[Card]) {
  val possiblestraights = (-1 to 9).map(
    a => (0 to 4).map (
       b => a + b
    )
  )
  
  val values:List[Int] = cards.map(_.rank.index).sortWith(_ > _)

  val frequencies:List[Int, Int] = FrequencyList[Int](values)

  private def slicethrice(values:List[Int]):List[List[Card]] = {
     (0 to (values.length-5)).map(checkindex => ordered
        .slice(checkindex, 5+checkindex))
  }

  private def isstraight(fivecards:List[Card]):Boolean = {

  }

  val straights:List[List[Card]] = {
    val ordered = values.reverse
    val substraights = slicethrice(ordered).filter(possiblestraights.contains(_))
    if(values.contains(12)) {
      slicethrice(
        ordered
          .map(x => if(x == 12) -1 else x)
          .sortWith(_ > _))
        .filter(list => list.count(_ == -1) == 1)
        .filter(possiblestraights.contains(_)) ::: substraights
    } else
      substraights
  }

  val flushes:List[Card] = {
    val flushsuit = FrequencyList[Suit](cards.map(_.suit)).find(_._2 >= 5)
    if(flushsuit.isDefined)
      cards
        .filter(_.suit == flushsuit)
        .map(_.rank.value)
        .sortWith(_ > _)
    else
      List.empty
  }
}