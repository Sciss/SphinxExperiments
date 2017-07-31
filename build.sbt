name            := "SphinxExperiments"
version         := "0.1.0-SNAPSHOT"
organization    := "de.sciss"
licenses        := Seq("GNU General Public License v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

scalaVersion    := "2.12.3"
scalacOptions  ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint")

libraryDependencies ++= Seq(
  "de.sciss" %  "sphinx4-core"      % "1.0.0",
  "de.sciss" %  "sphinx4-data"      % "1.0.0",
  "de.sciss" %% "fileutil"          % "1.1.2",
  "de.sciss" %% "audiowidgets-core" % "1.11.0",
  "de.sciss" %% "scalaaudiofile"    % "1.4.6",
  "de.sciss" %% "processor"         % "0.4.1",
  "de.sciss" %% "pdflitz"           % "1.2.2",
  "de.sciss" %% "play-json-sealed"  % "0.4.1",
  "de.sciss" %% "lucre"             % "3.4.1",
  "de.sciss" %% "kollflitz"         % "0.2.1",
  "de.sciss" %  "shapeinterpolator" % "0.1.0"
)

initialCommands in console :=
  """import de.sciss.sphinxex._
    |import de.sciss.kollflitz.Ops._
    |import de.sciss.numbers.Implicits._
    |import de.sciss.file._""".stripMargin

resolvers += "Oracle Repository" at "http://download.oracle.com/maven"/  // required for sleepycat
