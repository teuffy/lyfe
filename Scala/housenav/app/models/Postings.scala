package models


import models.SellerType._
import models.PricePeriod._
import models.AdType._

case class Advertisement(id: Option[Long], Address: String, adType: AdType, price: Double, pricePeriod: PricePeriod, noOfRooms: Int, sellerType: SellerType, size: Int)

case class AdExtraData(id: Option[Long], isKitchenSeperate: Option[Boolean], noOfBathrooms: Option[Int], hasBalcony: Option[Boolean], hasTerrace: Option[Boolean], floor: Option[Int], floorsNo: Option[Int], advertisemntId: Long)



