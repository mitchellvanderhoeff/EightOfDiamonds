package main

import collection.LinearSeq

object MList {

}

object ListDisplay {
  def apply(list:List[Any]):String = list.mkString("["," ","]")
}

object FrequencyList {
  def apply[T](list:List[T]):List[(T, Int)] = list.distinct.map((value) => (value, list.count(_ == value)))
}



