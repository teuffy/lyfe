package one.week

object lecture5 {
  /**
   * Calculates the square root of parameter using Newton's method
   */

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double): Boolean =
      Math.abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double, x: Double): Double =
      (guess + x / guess) / 2
      
    sqrtIter(1.0, x)
  }

}