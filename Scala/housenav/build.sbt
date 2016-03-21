name := "tradenav"
version := "0.1"
scalaVersion := "2.11.7"
lazy val root = (project in file(".")).enablePlugins(PlayScala)
libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % "3.1.1",
    "com.typesafe.play" %% "play-slick" % "1.1.1",
    "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0" % Test,
    "org.scalacheck" %% "scalacheck" % "1.12.5" % Test,
    "com.h2database" % "h2" % "1.4.191"

)
