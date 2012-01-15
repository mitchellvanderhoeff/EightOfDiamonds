package main

/**
 * Simple object to quickly calculate the frequency of elements in a list.
 */

object FrequencyList {
  def apply[T](list:List[T]):List[(T, Int)] = list.distinct.map((value) => (value, list.count(_ == value)))
}



