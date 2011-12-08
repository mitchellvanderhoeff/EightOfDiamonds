package unittests

import main._
import org.scalatest.Spec
import util.Random

class TestStraights extends Spec {

  def recursivefindstraight(straightindices: List[Int]): List[Card] = {
    val purestraight = straightindices.map {
      lowaceindex =>
       try {
        Deck.lookup{card =>
            card.lowaceindex == lowaceindex}
         } catch {
          case e:RuntimeException =>
            throw new RuntimeException("card not found. lowaceindex: "+lowaceindex+"\ndeck: "+Deck.toString)
        }
    }
    if (purestraight.forall(card => card.suit == purestraight.head.suit))
    {
      Deck.shuffle()
      recursivefindstraight(straightindices)
    }
    else
      purestraight
  }

  def recursivefindlastcards(straight: List[Card]): List[Card] = {
    val lastcards = List.fill(2) {
      Deck.lookup(card =>
        !straight
        .map(_.lowaceindex)
        .contains(card.lowaceindex))
    }
    if (FrequencyList[Suit]((straight ++ lastcards).map(_.suit)).exists(_._2 >= 5))
    {
      Deck.shuffle()
      recursivefindlastcards(straight)
    }
    else
      lastcards
  }

  describe(Constants.numtrial + " pure straight hands") {
    it("should contain " + Constants.numtrial + " 'straights'") {
      for (i <- 1 to Constants.numtrial) {
        val startindex = Random.nextInt(8)
        val straightindices = startindex to (startindex + 4)
        val purestraight = recursivefindstraight(straightindices.toList)
        val restcards = recursivefindlastcards(purestraight)
        val allcards = Random.shuffle(purestraight ++ restcards)
        println
        val score = Score(allcards)
        assert((score(0) == 4), "cards: "+allcards+", not a straight, actually "+score+" found, i="+i)
        Deck.shuffle()
      }
    }
  }
}