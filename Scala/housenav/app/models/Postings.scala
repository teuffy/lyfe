package models

import models.SellerType._
import models.PricePeriod._
import models.AdType._

case class Advertisement(id: Option[Long], address: String, adType: AdType, price: Double, pricePeriod: PricePeriod, noOfRooms: Int, sellerType: SellerType, size: Int, userId: Option[Long])
object AdvertisementExtra {
  def applyFromForm(id: Option[Long], address: String, adType: String, price: Double, pricePeriod: String, noOfRooms: Int, sellerType: String, size: Int, userId: Option[Long]): Advertisement =
    Advertisement(id, address, AdType.withName(adType), price, PricePeriod.withName(pricePeriod), noOfRooms, SellerType.withName(sellerType), size, userId)
  def unapplyFromForm(a: Advertisement): Option[(Option[Long], String, String, Double, String, Int, String, Int, Option[Long])] =
    Option((a.id, a.address, a.adType.toString, a.price, a.pricePeriod.toString, a.noOfRooms, a.sellerType.toString, a.size, a.userId))
}

case class AdExtraData(id: Option[Long], isKitchenSeperate: Option[Boolean], noOfBathrooms: Option[Int], hasBalcony: Option[Boolean], hasTerrace: Option[Boolean], floor: Option[Int], floorsNo: Option[Int], advertisemntId: Long)

case class Location(id: Option[Long], lat: Double, long: Double, locationIds: Option[Seq[Double]])