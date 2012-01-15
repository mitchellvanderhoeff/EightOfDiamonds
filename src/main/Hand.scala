package main


/**
 * A collection of all the possible straights by card index
 */
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
  /**
   * A simple function so slice seven cards into fives,
   * e.g. [1, 2, 3, 4, 5, 6, 7] becomes [1, 2, 3, 4, 5], [2, 3, 4, 5, 6], and [3, 4, 5, 6, 7]
   */
  private def sliceByFives(manycards:List[Card]):List[List[Card]] = {
     (0 to (manycards.length-5))
       .map(index => manycards
        .slice(index, 5+index))
       .toList
  }

  private def makeLowAceSlices(manycards:List[Card]):List[List[Card]] = {
    sliceByFives(manycards.sortBy(card => card.lowaceindex))
  }

  private def makeHighAceSlices(manycards:List[Card]):List[List[Card]] = {
    sliceByFives(manycards.sortBy(card => card.index))
  }

  private def isStraight(fivecards:List[Card]):Boolean = {
    /**
     * Simple algorithm, just check if these five cards are with regard to index equal to the possible straights
     */
    Hand.possiblestraights.contains(fivecards.map(_.index).sorted) || fivecards.map(_.lowaceindex).sorted == Hand.lowacestraight
  }

  val straights:List[List[Card]] = {
    var uniques = cards.map(card => Card(card.rank, Suit(""))).distinct   // So that no duplicate ranks are present
    val slices = makeHighAceSlices(uniques) ++ makeLowAceSlices(uniques)  // Both the high ace and low ace 5 card hands
    slices.filter(fivecards => isStraight(fivecards))                     // Filter out the ones that aren't straights
  }

  private def isFlush(fivecards:List[Card]):Boolean = {
    fivecards.forall(card => card.suit == fivecards.head.suit)            // Check whether the first card has the same suit as all the other cards
  }

  val flushes:List[List[Card]] = {
      val suitSlices = sliceByFives(cards.sortBy(card => card.suit.value))    // Sort by suits, then check each slice for flush
      suitSlices.filter(fivecards => isFlush(fivecards))
  }

  val straightflushes:List[List[Card]] = {
    val suitList = Deck.suits.map(suit => cards.filter(card => card.suit.value == suit))   // For each suit, filter out the cards that are not that suit
    val totalFlush = suitList.find(list => list.length >= 5)
    if(totalFlush.isDefined) {                                               // If one of these lists is five or longer i.e. five of the same suits
      val highAceSorted = totalFlush.get.sortBy(card => card.index)          // Now check whether one or more of these contains a straight
      val lowAceSorted = totalFlush.get.sortBy(card => card.lowaceindex)
      val slices = sliceByFives(highAceSorted) ++ sliceByFives(lowAceSorted)
      slices.filter(slice => isStraight(slice))
    } else {
      List.empty
    }
  }
}