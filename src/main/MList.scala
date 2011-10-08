package main

import collection.LinearSeq

object MList {
  def compareByElements[A](one:List[A], other:List[A])(implicit numeric:Numeric[A]):Int = {
    for(i <- 0 to (List(one.length, other.length).max - 1)) {
      val difference = numeric.compare(one(i), other(i))
      if(difference != 0)
        return difference
    }
    0
  }
}

object ListDisplay {
  def apply(list:List[Any]):String = list.mkString("["," ","]")
}

object FrequencyList {
  def apply[T](list:List[T]):List[(T, Int)] = list.distinct.map((value) => (value, list.count(_ == value)))
}



