lazy val root = project
  .in(file("."))
  .settings(
    name := "secret-key-cryptography",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.4.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
