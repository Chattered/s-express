name := "s-express"
lazy val root = project.in(file(".")).
  aggregate(sexpressJS, sexpressJVM).
  settings(
    publish := {},
    publishLocal := {}
  )
lazy val sexpress = crossProject.in(file(".")).
  settings(
    name := "s-express",
    organization := "org.ensime",
    version := "2.0.0-SNAPSHOT",
    scalaVersion in ThisBuild := "2.11.8",
    scalacOptions += "-feature",
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.1.2",
      "com.typesafe.akka" %% "akka-slf4j" % "2.3.15",
      "org.scalaz" %% "scalaz-core" % "latest.integration")
    )
lazy val sexpressJVM = sexpress.jvm
lazy val sexpressJS  = sexpress.js
