package recfun

import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  import scala.util.control.TailCalls._
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def pascalTail(c: Int, r: Int): TailRec[Int] = {
      if c == 0 || c == r then done(1)
      else
        for {
          left <- tailcall(pascalTail(c-1, r-1))
          right <- tailcall(pascalTail(c, r-1))
        } yield left + right
    }
    pascalTail(c, r).result
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], parenthesesTrace: Int): Boolean = {
      if parenthesesTrace < 0 then false
      else if chars.isEmpty then parenthesesTrace == 0
      else
        val pointMap = Map(
          '(' -> 1,
          ')' -> -1
        )
        val updatedParenthesesTrace = parenthesesTrace + pointMap.getOrElse(chars.head, 0)
        balanceIter(chars.tail, updatedParenthesesTrace)
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(money: Int, coins: List[Int]): TailRec[Int] = {
      if money == 0 then done(1)
      else if money < 0 || coins.isEmpty then done(0)
      else
        for {
          noUseHead <- tailcall(countChange(money, coins.tail))
          useHead <- tailcall(countChange(money - coins.head, coins))
        } yield noUseHead + useHead
    }
    countChange(money, coins).result
  }

