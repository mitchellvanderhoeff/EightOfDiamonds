package main

import util.Random

case class Rank(value:String, index:Int) {
  override def toString = value
  def ==(other:Rank) = (value == other.value)
}
case class Suit(value:String) {
  override def toString = value
  def ==(other:Suit) = (value == other.value)
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
    cards = Random.shuffle(cards.diff(List(card)))
    card
  }

  private def lookup(f:(Card => Boolean)):Card = {
    val fetched = cards.find(c => f(c))
    if(fetched.isEmpty)
      throw new IllegalArgumentException("card not found")
    removecard(fetched.get)
  }

  private def findcard(rs:(String, String)):Card = {
    rs match {
      case ("X", "x") =>
        lookup(c => true)
      case ("X", _) =>
        lookup(c => c.suit.value == rs._2)
      case (_, "x") =>
        lookup(c => c.rank.value == rs._1)
      case (_, _) =>
        lookup(c => c.rank.value == rs._1 && c.suit.value == rs._2)
    }
  }

  def search(requests:String):List[Card] = {
    val requestedcards = requests
      .split(";")
      .map(req => req.splitAt(1))
      .map(rs => (rs._1.toUpperCase, rs._2.toLowerCase))
    val nonrandoms = requestedcards
      .filter(rs => rs._1 != "X" && rs._2 != "x")
      .map(findcard(_))
      .toList
    val randoms = requestedcards
      .filter(rs => rs._1 == "X" || rs._2 == "x")
      .map(findcard(_))
      .toList
    val allcards = nonrandoms ::: randoms
    cards = cards.diff(allcards)
    allcards
  }

  override def toString = {
    cards.toString()
  }
}

