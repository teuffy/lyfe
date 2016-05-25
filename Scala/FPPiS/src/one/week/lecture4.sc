package one.week

object lecture4 {
  //and(x, y) == x && y
  def and(x: Boolean, y: => Boolean) = if (x) y else x
                                                  //> and: (x: Boolean, y: => Boolean)Boolean
  //or(x, y) == x || y
  def or(x: Boolean, y: Boolean) = if (x) x else y//> or: (x: Boolean, y: Boolean)Boolean
}