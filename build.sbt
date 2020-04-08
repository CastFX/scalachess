name := "scalachess"
organization in ThisBuild := "it.scalachess"
scalaVersion in ThisBuild := "2.12.10"

// PROJECTS

lazy val global = project
  .in(file("."))
  .settings(settings)
  .aggregate(
    core,
    client,
    server,
    util
  )

lazy val core = project
  .settings(
    name := "core",
    settings,
    libraryDependencies ++= commonDependencies
  )

lazy val client = project
  .settings(
    name := "client",
    mainClass := Some("it.scalachess.client.remote_client.ClientMain"),
    settings,
    assemblySettings,
    libraryDependencies ++= (commonDependencies ++ akkaDependencies)
  )
  .dependsOn(
    core,
    util
  )

lazy val server = project
  .settings(
    name := "server",
    mainClass := Some("it.scalachess.server.ServerMain"),
    settings,
    assemblySettings,
    libraryDependencies ++= (commonDependencies ++ akkaDependencies)
  )
  .dependsOn(
    core,
    util
  )
// DEPENDENCIES

lazy val util = project
  .settings(
    name := "util",
    settings,
    libraryDependencies += dependencies.akkaTyped
  )
  .dependsOn(
    core
  )

lazy val dependencies =
  new {
    val logbackV        = "1.2.3"
    val logstashV       = "4.11"
    val scalaLoggingV   = "3.7.2"
    val slf4jV          = "1.7.25"
    val typesafeConfigV = "1.3.1"
    val pureconfigV     = "0.8.0"
    val monocleV        = "1.4.0"
    val akkaV           = "2.6.4"
    val scalatestV      = "3.0.4"
    val scalacheckV     = "1.13.5"
    val scalazV         = "7.2.30"

    val logback        = "ch.qos.logback"             % "logback-classic"             % logbackV
    val logstash       = "net.logstash.logback"       % "logstash-logback-encoder"    % logstashV
    val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging"              % scalaLoggingV
    val slf4j          = "org.slf4j"                  % "jcl-over-slf4j"              % slf4jV
    val typesafeConfig = "com.typesafe"               % "config"                      % typesafeConfigV
    val akkaTyped      = "com.typesafe.akka"          %% "akka-actor-typed"           % akkaV
    val akkaTest       = "com.typesafe.akka"          %% "akka-actor-testkit-typed"   % akkaV
    val akkaSerialize  = "com.typesafe.akka"          %% "akka-serialization-jackson" % akkaV
    val akkaRemote     = "com.typesafe.akka"          %% "akka-remote"                % akkaV
    val akkaCluster    = "com.typesafe.akka"          %% "akka-cluster-typed"         % akkaV
    val monocleCore    = "com.github.julien-truffaut" %% "monocle-core"               % monocleV
    val monocleMacro   = "com.github.julien-truffaut" %% "monocle-macro"              % monocleV
    val pureconfig     = "com.github.pureconfig"      %% "pureconfig"                 % pureconfigV
    val scalatest      = "org.scalatest"              %% "scalatest"                  % scalatestV
    val scalacheck     = "org.scalacheck"             %% "scalacheck"                 % scalacheckV
    val scalaz         = "org.scalaz"                 %% "scalaz-core"                % scalazV
  }

lazy val commonDependencies = Seq(
  dependencies.logback,
  dependencies.logstash,
  dependencies.scalaLogging,
  dependencies.slf4j,
  dependencies.typesafeConfig,
  dependencies.scalatest  % "test",
  dependencies.scalacheck % "test",
  dependencies.scalaz
)

lazy val akkaDependencies = Seq(
  dependencies.akkaTyped,
//  dependencies.akkaCluster,
  dependencies.akkaRemote,
  dependencies.akkaSerialize,
  dependencies.akkaTest % Test
)

// SETTINGS

lazy val settings =
commonSettings ++
wartremoverSettings ++
scalafmtSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

lazy val wartremoverSettings = Seq(
  wartremoverWarnings in (Compile, compile) ++= Warts.unsafe
)

lazy val scalafmtSettings =
  Seq(
    scalafmtOnCompile := true,
    scalafmtTestOnCompile := true,
    scalafmtVersion := "1.2.0"
  )

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := name.value + ".jar",
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case PathList("application.conf")  => MergeStrategy.concat
    case PathList("reference.conf")    => MergeStrategy.concat
    case x                             => MergeStrategy.first
  }
)
