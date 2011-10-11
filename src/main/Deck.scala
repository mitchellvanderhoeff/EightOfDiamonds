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



class CardSlot(val rawrequest:String, var content:Card=null) {
  val request = rawrequest.splitAt(1)
  def fill(card:Card) {content = card}
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

  def shuffle() {
    cards = Random.shuffle(ordereddeck)
  }

  def deal(amount:Int):List[Card] = {
    val result = cards.take(amount)
    cards = cards.drop(amount)
    result
  }

  private def removecard(card:Card):Card = {
    cards = cards.filterNot(_ == card)
    card
  }

  private def lookup(f:(Card => Boolean)):Card = {
    val fetched = cards.filter(c => f(c))
    if(fetched.isEmpty)
      throw new IllegalArgumentException("card not found, request was "+lastlookup+", cards="+ListDisplay(cards))
    removecard(Random.shuffle(fetched).head)
  }

  def fillslot(cardslot:CardSlot):CardSlot =  {
    val rs = cardslot.request
    lastlookup = rs
    cardslot.fill(
      rs match {
      case ("X", "x") =>
        Deck.deal(1)(0)
      case ("X", _) =>
        lookup(c => c.suit == rs._2)
      case (_, "x") =>
        lookup(c => c.rank == rs._1)
      case (_, _) =>
        lookup(c => c.rank == rs._1
                 && c.suit == rs._2)
    })
    cardslot
  }

  override def toString = {
    cards.toString()
  }
}

