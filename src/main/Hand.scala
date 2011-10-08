package main

case class Hand(cards:List[Card]) {
  val possiblestraights = (-1 to 9).map(
    a => (0 to 4).map (
       b => a + b
    )
  )
  
  val values:List[Int] = cards.map(_.rank.index).sortWith(_ > _)

  val straights:List[List[Card]] = {
    val ordered = values.reverse
    val identifiers = (0 to 2)
      .map(checkindex => ordered
        .slice(checkindex, 5+checkindex)
      ).filter(possiblestraights.contains(_))
    val foundindex = identifiers.lastIndexWhere(boolean => boolean)
    if(foundindex != -1) {
      Some(ordered(foundindex)+1)
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

  val flushes:List[Card] = {
    val flushsuit = FrequencyList[Suit](cards.map(_.suit)).find(_._2 >= 5)
    if(flushsuit.isDefined)
      cards.filter(_.suit == flushsuit)
    else
      List.empty
  }
}