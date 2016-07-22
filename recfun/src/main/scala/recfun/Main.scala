package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    var balanceAcc = 0

    def parenthesesDetector(char: Char): Int = {
      if (char == '(') 1 else if (char == ')') -1 else 0
    }

    def parenthesesBalancer(chars: List[Char]): Boolean = {
      if (chars.isEmpty) true
      else {
        balanceAcc = balanceAcc + parenthesesDetector(chars.head)
        balanceAcc >= 0 && parenthesesBalancer(chars.tail)
      }
    }

    parenthesesBalancer(chars) && balanceAcc == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countLoop(money: Int, coins: List[Int], i: Int): Int = {
      if (money == 0) 1
      else if (money < 0 || (i == coins.length && money > 0)) 0
      else countLoop(money - coins(i), coins, i) + countLoop(money, coins, i + 1)
    }

    if (money <= 0 || coins.isEmpty) 0
    else countLoop(money, coins, 0)
  }
}
