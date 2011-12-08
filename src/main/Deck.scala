package main

import util.Random

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
  def lowaceindex:Int = if(index == 12) -1 else index
  def ==(other:Card) = (suit == other.suit && rank == other.rank)
}


object Deck {
  val ranks = List("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  val suits = List("s","h","c","d")
  val zippedranks = ranks.zipWithIndex
  val zippedsuits = suits.zipWithIndex
  val ordereddeck = suits.flatMap((suit) =>
      zippedranks.map(ri => Card(Rank(ri._1, ri._2), Suit(suit)))
    )
  var cards:List[Card] = ordereddeck
  var lastlookup:(String, String) = ("","")
  var lookinguprandom = false

  def shuffle() {
    cards = Random.shuffle(Random.shuffle(ordereddeck))
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

  def lookup(f:(Card => Boolean)):Card = {
    val fetched = cards.filter(f(_))
    if(fetched.isEmpty)
      throw new RuntimeException("card not found")
    val fetchedcard = fetched(Random.nextInt(fetched.length))
    removecard(fetchedcard)
  }

  private def findcard(rs:(String, String)):Card = {
    rs match {
      case ("X", "x") =>
        Deck.deal(1)(0)
      case ("X", checksuit) =>
        lookup(c => c.suit == checksuit)
      case (checkrank, "x") =>
        lookup(c => c.rank == checkrank)
      case (checkrank, checksuit) =>
        lookup(c => c.rank == checkrank
                 && c.suit == checksuit)
    }
  }

  def search(request:String):List[Card] = {
    val requestedcards = request
      .split(" ")
      .map(req => req.splitAt(1))
      .map(rs => (rs._1.toUpperCase, rs._2.toLowerCase))
      .map(rs => findcard(rs))
    cards = cards.diff(requestedcards)
    requestedcards.toList
  }

  override def toString = {
    cards.toString()
  }
}

