package models

case class User(id: Option[Long], email: String, password: String, name: Option[String])