package models

object AdType extends Enumeration {
  type AdType = Value
  val Flat = Value("flat")
  val House = Value("house")
  val Rooms = Value("rooms")
}

object PricePeriod extends Enumeration {
  type PricePeriod = Value
  val Daily = Value("daily")
  val Monthly = Value("monthly")
  val Weekly = Value("weekly")
}

object SellerType extends Enumeration {
  type SellerType = Value
  val Direct = Value("direct")
  val Agency = Value ("agency")
}
