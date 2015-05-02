lazy val commonSettings = Seq(
	organization := "com.kjcondron",
	version := "0.1.0",
	scalaVersion := "2.11.2")


lazy val root = (project in file("./src/com/kjcondron/web")).
  settings(commonSettings: _*).
  settings(
    name := "SpotifyAlt18Gen",
	libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2")


