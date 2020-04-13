scalaVersion := "2.13.1"

val CatsVersion = "2.1.1"
val ScalaTestVersion = "3.1.2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-laws" % CatsVersion % Test,
  "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % s"${ScalaTestVersion}.0" % Test
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
  "-Wvalue-discard"
)

initialCommands in console := "import parser._, ParseResult._, Parser._, Person._, cats.implicits._"
