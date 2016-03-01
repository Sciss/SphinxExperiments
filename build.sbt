name            := "SphinxExperiments"
version         := "0.1.0-SNAPSHOT"
organization    := "de.sciss"
licenses        := Seq("GNU General Public License v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

scalaVersion    := "2.11.7"
scalacOptions  ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint")

libraryDependencies ++= Seq(
  "de.sciss" %  "sphinx4-core"      % "1.0.0",
  "de.sciss" %  "sphinx4-data"      % "1.0.0",
  "de.sciss" %% "fileutil"          % "1.1.1",
  "de.sciss" %% "audiowidgets-core" % "1.9.1",
  "de.sciss" %% "scalaaudiofile"    % "1.4.5",
  "de.sciss" %% "processor"         % "0.4.0",
  "de.sciss" %% "pdflitz"           % "1.2.1",
  "de.sciss" %% "play-json-sealed"  % "0.4.0",
  "de.sciss" %% "lucre"             % "3.3.0",
  "de.sciss" %% "kollflitz"         % "0.2.0",
  "de.sciss" %  "shapeinterpolator" % "0.1.0"
)
