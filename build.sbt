import sbt.Keys.{licenses, _}
import sbt._

lazy val commonSettings = Seq(
  organization := "org.ergoplatform",
  name := "ergo",
  version := "3.1.4",
  scalaVersion := "2.12.10",
  resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "SonaType" at "https://oss.sonatype.org/content/groups/public",
    "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"),
  homepage := Some(url("http://ergoplatform.org/")),
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))
)

val scorexVersion = "4ca3e400-SNAPSHOT"
val sigmaStateVersion = "3.1.0"

// for testing current sigmastate build (see sigmastate-ergo-it jenkins job)
val effectiveSigmaStateVersion = Option(System.getenv().get("SIGMASTATE_VERSION")).getOrElse(sigmaStateVersion)

libraryDependencies ++= Seq(
  ("org.scorexfoundation" %% "sigma-state" % effectiveSigmaStateVersion).force()
    .exclude("ch.qos.logback", "logback-classic")
    .exclude("org.scorexfoundation", "scrypto"),
  "org.scala-lang.modules" %% "scala-async" % "0.9.7",

  "org.scorexfoundation" %% "iodb" % "0.3.2",

  ("org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8").exclude("org.iq80.leveldb", "leveldb"),
  "org.iq80.leveldb" % "leveldb" % "0.12",
  ("org.scorexfoundation" %% "scorex-core" % scorexVersion).exclude("ch.qos.logback", "logback-classic"),

  "org.typelevel" %% "cats-free" % "1.6.0",
  "javax.xml.bind" % "jaxb-api" % "2.+",
  "com.iheart" %% "ficus" % "1.4.+",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.google.guava" % "guava" % "21.0",

  "com.storm-enroute" %% "scalameter" % "0.8.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test,it",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",

  "org.scorexfoundation" %% "scorex-testkit" % scorexVersion % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.24" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.1.9" % "test",
  "org.asynchttpclient" % "async-http-client" % "2.6.+" % "test",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.2" % "test",
  "com.spotify" % "docker-client" % "8.14.5" % "test" classifier "shaded"
)

updateOptions := updateOptions.value.withLatestSnapshots(false)

fork := true

val opts = Seq(
  "-server",
  // JVM memory tuning for 2g ram
  "-Xms128m",
  "-Xmx2G",
  //64M for stack, reduce after optimizations
  "-Xss64m",
  "-XX:+ExitOnOutOfMemoryError",
  // Java 9 support
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",

  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-XX:+UseG1GC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",

  // probably can't use these with jstack and others tools
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication"
)

// -J prefix is required by the bash script
javaOptions in run ++= opts
scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation")

// set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
// see https://github.com/eclipse/jetty.project/issues/3244
// these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
// see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
scalacOptions in(Compile, compile) ++= Seq("-release", "8")

sourceGenerators in Compile += Def.task {
  val versionFile = (sourceManaged in Compile).value / "org" / "ergoplatform" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val versionExtractor(major, minor, bugfix) = version.value
  IO.write(versionFile,
    s"""package org.ergoplatform
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $bugfix)
       |}
       |""".stripMargin)
  Seq(versionFile)
}

mainClass in assembly := Some("org.ergoplatform.ErgoApp")

test in assembly := {}

assemblyJarName in assembly := s"ergo-${version.value}.jar"

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case "reference.conf" => CustomMergeStrategy.concatReversed
  case PathList("org", "iq80", "leveldb", xs @ _*) => MergeStrategy.first
  case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
  case PathList("javax", "annotation", xs @ _*) => MergeStrategy.last
  case other => (assemblyMergeStrategy in assembly).value(other)
}

enablePlugins(sbtdocker.DockerPlugin)
enablePlugins(JavaAppPackaging)

mappings in Universal += {
  val sampleFile = (resourceDirectory in Compile).value / "samples" / "local.conf.sample"
  sampleFile -> "conf/local.conf"
}

// removes all jar mappings in universal and appends the fat jar
mappings in Universal ++= {
  // universalMappings: Seq[(File,String)]
  val universalMappings = (mappings in Universal).value
  val fatJar = (assembly in Compile).value
  // removing means filtering
  val filtered = universalMappings filter {
    case (_, name) => !name.endsWith(".jar")
  }
  // add the fat jar
  filtered :+ (fatJar -> ("lib/" + fatJar.getName))
}

// add jvm parameter for typesafe config
bashScriptExtraDefines += """addJava "-Dconfig.file=${app_home}/../conf/local.conf""""

inConfig(Linux)(
  Seq(
    maintainer := "ergoplatform.org",
    packageSummary := "Ergo node",
    packageDescription := "Ergo node"
  )
)

Defaults.itSettings
configs(IntegrationTest extend Test)
inConfig(IntegrationTest)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
))

dockerfile in docker := {
  val configDevNet = (resourceDirectory in IntegrationTest).value / "devnetTemplate.conf"
  val configTestNet = (resourceDirectory in IntegrationTest).value / "testnetTemplate.conf"
  val configMainNet = (resourceDirectory in IntegrationTest).value / "mainnetTemplate.conf"

  new Dockerfile {
    from("openjdk:9-jre-slim")
    label("ergo-integration-tests", "ergo-integration-tests")
    add(assembly.value, "/opt/ergo/ergo.jar")
    add(Seq(configDevNet), "/opt/ergo")
    add(Seq(configTestNet), "/opt/ergo")
    add(Seq(configMainNet), "/opt/ergo")
  }
}

buildOptions in docker := BuildOptions(
  removeIntermediateContainers = BuildOptions.Remove.OnSuccess
)

//FindBugs settings

findbugsReportType := Some(FindbugsReport.Xml)
findbugsExcludeFilters := Some(scala.xml.XML.loadFile(baseDirectory.value / "findbugs-exclude.xml"))

//Scapegoat settings

scapegoatVersion in ThisBuild := "1.3.3"

scapegoatDisabledInspections := Seq("FinalModifierOnCaseClass")

Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("Bench")))

lazy val avldb = (project in file("avldb"))
  .settings(
    commonSettings,
    name := "avldb"
  )

lazy val avldb_benchmarks = (project in file("avldb/benchmarks"))
  .settings(
    commonSettings,
    name := "avldb-benchmarks",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.9" % "test"
    ),
    publishArtifact := false,
    resolvers ++= Seq("Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    logBuffered := false
  )
  .dependsOn(avldb)
  .enablePlugins(JmhPlugin)

lazy val ergoWallet = (project in file("ergo-wallet"))
  .settings(
    commonSettings,
    name := "ergo-wallet",
    libraryDependencies += ("org.scorexfoundation" %% "sigma-state" % effectiveSigmaStateVersion)
  )

lazy val It2Test = config("it2") extend (IntegrationTest, Test)
configs(It2Test)
inConfig(It2Test)(Defaults.testSettings ++ Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
))

lazy val ergo = (project in file("."))
  .settings(commonSettings, name := "ergo")
  .dependsOn(ergoWallet % "compile->compile")
  .dependsOn(avldb % "compile->compile")
  .configs(It2Test)

lazy val benchmarks = (project in file("benchmarks"))
  .settings(commonSettings, name := "ergo-benchmarks")
  .dependsOn(ergo % "test->test")
  .enablePlugins(JmhPlugin)