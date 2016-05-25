package one.week

object lecture2 {
  34 + 65
  def radius = 10
  def pi = 3.14159

  (2 * pi) * radius

  def square(x: Double) = x * x
  def sumOfSquares(x: Double, y: Double) =
    square(x) + square(y)

//infinite loop
def loop: Int = loop

}