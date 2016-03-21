package models

case class Location(id: Option[Long], lat: Double, long: Double)

case class Advertisement(id: Option[Long], locationId: Option[Long], name: String, description: String)

case class AdvertisementDetails(id: Option[Long], advertisementId: Option[Long], roomCount: Option[Int], bathNo: Option[Int], areaSize: Option[Int])