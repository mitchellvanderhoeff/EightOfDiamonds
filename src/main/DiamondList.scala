package main

import collection.immutable.LinearSeq

class MList[T] extends LinearSeq[T] {
  def frequencylist:List[(T, Int)] = {
    this.distinct.map((value) => (value, this.count(_ == value)))
  }

  def compareByElements(other:MList[T]):Int = {
    for(i <- 0 to (List(length, other.length).max - 1)) {
      if(this(i) < other(i))
        return -1
      else if(this(i) > other(i))
        return 1
    }
    0
  }

  override def toString:String = {
    this.foldLeft("[")((a, b) => a + (if(a.length!=1) " " else "") + b.toString) + "]"
  }
}