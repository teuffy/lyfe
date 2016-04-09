package dao

import play.api.db.slick.HasDatabaseConfigProvider
import slick.lifted.Tag
import slick.driver.H2Driver
import models.AdExtraData
import play.api.db.slick.DatabaseConfigProvider
import javax.inject. {Inject, Singleton}
import slick.lifted.TableQuery

trait AdExtraComponent { self: HasDatabaseConfigProvider[H2Driver] =>
  import driver.api._
  class AdExtraDatas(tag: Tag) extends Table[AdExtraData](tag, "advertisements") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def isKitchenSeperate = column[Option[Boolean]]("kitchenSeperate")
    def noOfBathrooms = column[Option[Int]]("noOfBathrooms")
    def hasBalcony = column[Option[Boolean]]("balcony")
    def hasTerrace = column[Option[Boolean]]("terrace")
    def floor = column[Option[Int]]("floor")
    def floorsNo = column[Option[Int]]("floorNo")
    def advertisementId = column[Long]("advertisementId")
    def * = (id.?, isKitchenSeperate, noOfBathrooms, hasBalcony, hasTerrace, floor, floorsNo, advertisementId) <> (AdExtraData.tupled, AdExtraData.unapply _)

  }
}
