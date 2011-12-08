package main

object Hand {
  val possiblestraights = (0 to 9).map(
    a => (0 to 4).map (
       b => a + b
    )
  )

  val lowacestraight = -1 to 3
}

case class Hand(cards:List[Card]) {

  val values:List[Int] = cards.map(_.index).sorted

  val kickers:List[Int] = values.reverse

  val frequencies:List[(Int, Int)] = {
    cards
      .map(card => card.index)
      .distinct
      .map(index => (index, cards.count(_.index == index)))
  }

  private def sliceByFives(manycards:List[Card]):List[List[Card]] = {
     (0 to (manycards.length-5))
       .map(index => manycards
        .slice(index, 5+index))
       .toList
  }

  private def returnstraight(fivecards:List[Card]):List[Card] = {
    if(Hand.possiblestraights.contains(fivecards.map(_.index).sorted))
      fivecards
    else
      List.empty
  }

  private def returnlowacestraight(fivecards:List[Card]):List[Card] = {
    if(fivecards.map(_.lowaceindex).sorted == Hand.lowacestraight)
      fivecards
    else
      List.empty
  }

  val straights:List[List[Card]] = {
    val slices = sliceByFives(cards)
    val lowaceslices = sliceByFives(cards.sortBy(_.lowaceindex))
    val result = lowaceslices.map(returnlowacestraight(_)) ++ slices.map(returnstraight(_))
    result.filter(!_.isEmpty)
  }

  private def returnflush(fivecards:List[Card]):List[Card] = {
    if(fivecards.forall(card => card.suit == fivecards.head.suit))
      fivecards
    else
      List.empty
  }

  val flushes:List[List[Card]] = {
      sliceByFives(cards.sortBy(card => card.suit.value))
        .map(fivecards => fivecards
          .sortBy(card => card.index))
        .map(fivecards => returnflush(fivecards))
        .filter(fivecards => !fivecards.isEmpty).sortBy(_(0).index)
  }

  val straightflushes:List[List[Card]] = {
    (flushes.map(returnlowacestraight(_)) ++ flushes.map(returnstraight(_))).filter(!_.isEmpty)
  }
}