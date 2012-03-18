package main
/*
 This class is strictly for the deck
 */
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
 * This is where cards are shuffled, searched for and dealt. There is only one instance of Deck and it is used each time cards are needed.
 * There is no way to access a Card, Rank or Suit without the object Deck.
 */

object Deck {
  val ranks = List("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  val suits = List("s","h","c","d")

  /*
   * list.zipWithIndex pairs the original elements with their respective indices
   */
  val zippedranks = ranks.zipWithIndex
  val zippedsuits = suits.zipWithIndex
  /*
   * list.flatMap applies a function to every element in the list and flattens it afterwards witch means the list becomes a list of elements,
    * not a list containing lists
   */
  val ordereddeck = suits.flatMap((suit) =>
      zippedranks.map(ri => Card(Rank(ri._1, ri._2), Suit(suit)))    // So that we may find card indices easily
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

  def dealString(amount:Int):String = {
    deal(amount).mkString(" ")
  }

  private def removecard(card:Card):Card = {
    cards = cards.filterNot(card == _)
    card
  }

  /**
   * This looks in the deck for a random card that satisfies the given condition
   */

  def lookup(condition:(Card => Boolean)):Option[Card] = {
    val fetched = cards.filter(condition(_))
    if(fetched.isEmpty) return None                                             // If the card is not found, the Calculation class will re-search all the cards again
    val fetchedcard = fetched(Random.nextInt(fetched.length))
    Some(removecard(fetchedcard))
  }
  /*
  A tuple is given to describe the requested card. The first element the rank, the second describing the suit.
  'x' signifies a random value.
   */
  private def findcard(rs:(String, String)):Option[Card] = {
    rs match {
      case ("X", "x") =>                                                // Nothing is specified
        Some(Deck.deal(1)(0))
      case ("X", checksuit) =>                                          // Only suit is specified
        lookup((c => c.suit == checksuit))
      case (checkrank, "x") =>                                          // Only rank is specified
        lookup((c => c.rank == checkrank))
      case (checkrank, checksuit) =>                                    // Both rank and suit are specified
        lookup((c => c.rank == checkrank
                 && c.suit == checksuit))
    }
  }

  /**
   * This function
   * 1) takes a request (e.g. "Kh 5d Tx"),
   * 2) splits it up into individual cards("Kh", "5d", "Tx"),
   * 3) splits the cards into rank and suit(("K", "h"), ("5", "d"), ("T", "x")),
   * 4) then passes it to the findcard() function.
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

