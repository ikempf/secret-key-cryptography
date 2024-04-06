lazy val root = project
  .in(file("."))
  .settings(
    name := "secret-key-cryptography",
    version := "1.0.0-SNAPSHOT",
    scalaVersion := "3.4.0"
  )
  .settings(
    scalacOptions ++= List(
      "-new-syntax",
      "-indent"
    )
  )
  .settings(
    libraryDependencies ++= List(
//      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
    )
  )
