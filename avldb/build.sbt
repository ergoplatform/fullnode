import sbt.Keys.testFrameworks

name := "avldb"

libraryDependencies ++= Seq(
  "javax.xml.bind" % "jaxb-api" % "2.4.0-b180830.0359",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scorexfoundation" %% "scrypto" % "2.1.7"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "com.storm-enroute" %% "scalameter" % "0.9" % "test"
)

libraryDependencies ++= Seq(
  "org.ethereum" % "leveldbjni-all"     % "1.18.3"
)

testOptions in Test := Seq(Tests.Filter(t => !t.matches(".*Benchmark$")))
javaOptions in run += "-Xmx12G"

//scalacOptions ++= Seq("-Xdisable-assertions")

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation")
