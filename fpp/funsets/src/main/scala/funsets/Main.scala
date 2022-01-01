package funsets

object Main extends App:
  import FunSets.*
  println(contains(singletonSet(1), 1))
  printSet(singletonSet(1))
  printSet(
    map(singletonSet(1), (x: Int) => 2 * x))
  //println(contains(map(singletonSet(1), (x: Int) => 2 * x), 2)
