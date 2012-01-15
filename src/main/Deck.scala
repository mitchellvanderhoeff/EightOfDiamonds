package main

import util.Random

/**
 * The simple case classes for Card, Rank and Suit objects
 */

case class Rank(value:String, index:Int) {
  override def toString = value
  def ==(other:Rank) = (value == other.value)
  def ==(str:String) = (value == str)
}
case class Suit(value:String) {
  override def toString = value
  def ==(other:Suit) = (value == other.value)
  def ==(str:String) = (value == str)
}
case class Card(rank:Rank, suit:Suit) {
  override def toString = {
    rank.toString + suit.toString
  }
  def index = rank.index
  def lowaceindex:Int = if(index == 12) -1 else index            // Need this to check for low ace straights (A 2 3 4 5)
  def ==(other:Card) = (suit == other.suit && rank == other.rank)
}

/**
 * This is where cards are shuffled, searched and dealt. It is a static, singleton object that contains the cards as an immutable variable.
 */

object Deck {
  val ranks = List("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  val suits = List("s","h","c","d")
  val zippedranks = ranks.zipWithIndex
  val zippedsuits = suits.zipWithIndex
  val ordereddeck = suits.flatMap((suit) =>
      zippedranks.map(ri => Card(Rank(ri._1, ri._2), Suit(suit)))    // So that we may find card indexes easily
    )
  var cards:List[Card] = ordereddeck
  var lastlookup:(String, String) = ("","")
  var lookinguprandom = false

  /**
   * Each time Deck.shuffle() is called, the function takes the prototype ordered deck, shuffles it and assigns the shuffled version to the variable cards.
   */

  def shuffle() {
    cards = Random.shuffle(ordereddeck)
  }

  def deal(amount:Int):List[Card] = {
    val result = cards.take(amount)
    cards = cards.drop(amount)
    result
  }

  private def removecard(card:Card):Card = {
    cards = cards.filterNot(card == _)
    card
  }

  /**
   * This is basically a List.find, but then for cards
   */

  def lookup(condition:(Card => Boolean), rs:(String, String)):Option[Card] = {
    val fetched = cards.filter(condition(_))
    if(fetched.isEmpty) return None                                             // If this happens, the Calculation class will re-search all the cards again
    val fetchedcard = fetched(Random.nextInt(fetched.length))
    Some(removecard(fetchedcard))
  }

  private def findcard(rs:(String, String)):Option[Card] = {
    rs match {
      case ("X", "x") =>                                                // Nothing is specified
        Some(Deck.deal(1)(0))
      case ("X", checksuit) =>                                          // Only suit is specified
        lookup((c => c.suit == checksuit), rs)
      case (checkrank, "x") =>                                          // Only rank is specified
        lookup((c => c.rank == checkrank), rs)
      case (checkrank, checksuit) =>                                    // Both rank and suit are specified
        lookup((c => c.rank == checkrank
                 && c.suit == checksuit), rs)
    }
  }

  /**
   * Takes a request (e.g. "Kh 5d"), splits it up into individual cards, then gives it to the findcard function.
   */

  def search(request:String):List[Option[Card]] = {
    val requestedcards = request
      .split(" ")
      .map(req => req.splitAt(1))
      .map(rs => (rs._1.toUpperCase, rs._2.toLowerCase))
      .map(rs => findcard(rs))
    cards = cards.diff(requestedcards)
    requestedcards.toList
  }


  /**
   * Necessary for testing, Option[Card] is not needed there
   */
  def searchRaw(request:String):List[Card] = {
    search(request).map(op => op.get)
  }

  override def toString = {
    cards.toString()
  }
}

