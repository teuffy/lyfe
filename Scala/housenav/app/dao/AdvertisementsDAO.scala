package dao

import javax.inject.{ Inject, Singleton }
import models.{ AdType, Advertisement, PricePeriod, SellerType }
import models.AdType._
import models.PricePeriod._
import models.SellerType._
import play.api.db.slick.{ DatabaseConfigProvider, HasDatabaseConfigProvider }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import slick.driver.H2Driver
import slick.lifted.ProvenShape.proveShapeOf
import scala.concurrent.Future
import models.User

trait AdvertisementComponent { self: HasDatabaseConfigProvider[H2Driver] =>
  import driver.api._
  class Advertisements(tag: Tag) extends Table[Advertisement](tag, "advertisements") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def address = column[String]("address")
    def adType = column[AdType]("adType")
    def price = column[Double]("price")
    def pricePeriod = column[PricePeriod]("pricePeriod")
    def noOfRooms = column[Int]("noOfRooms")
    def sellerType = column[SellerType]("sellerType")
    def size = column[Int]("size")
    def userId = column[Long]("userId")
    def * = (id.?, address, adType, price, pricePeriod, noOfRooms, sellerType, size, userId.?) <> (Advertisement.tupled, Advertisement.unapply _)

    implicit val AdTypeMapper = MappedColumnType.base[AdType, String](
      a => a.toString,
      s => AdType.withName(s))

    implicit val PricePeriodMapper = MappedColumnType.base[PricePeriod, String](
      a => a.toString,
      s => PricePeriod.withName(s))

    implicit val SellerTypeMapper = MappedColumnType.base[SellerType, String](
      a => a.toString,
      s => SellerType.withName(s))
  }
}

@Singleton
class AdvertisementsDAO @Inject() (protected val dbConfigProvider: DatabaseConfigProvider) extends AdvertisementComponent
    with HasDatabaseConfigProvider[H2Driver] {
  import driver.api._
  
  private val Advertisements = TableQuery[Advertisements]
  db.run((Advertisements.schema).create)
  
  def insert(advertisement: Advertisement): Future[Int] = {
    db.run(Advertisements += advertisement)
  }
  def insertForUser(ad: Advertisement, user: User): Future[Int] = {
    val adWithUser = Advertisement(ad.id, ad.address, ad.adType, ad.price, ad.pricePeriod, ad.noOfRooms, ad.sellerType, ad.size, user.id)
    db.run(Advertisements += adWithUser)
  }
}