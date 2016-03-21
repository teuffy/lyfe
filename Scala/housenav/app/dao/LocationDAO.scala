package dao

import javax.inject.{ Singleton, Inject }
import scala.concurrent.Future
import models.Location
import play.api.db.slick.{ HasDatabaseConfigProvider, DatabaseConfigProvider }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import slick.driver.H2Driver

trait CompaniesComponent { self: HasDatabaseConfigProvider[H2Driver] =>
  import driver.api._
  class Locations(tag: Tag) extends Table[Location](tag, "LOCATION") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def lat = column[Double]("LATITUDE")
    def long = column[Double]("LONGITUDE")
    def * = (id.?, lat, long) <> (Location.tupled, Location.unapply _ )
  }
}

class LocationDAO {

}