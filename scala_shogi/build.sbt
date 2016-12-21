name := "scala_shogi"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"

lazy val root = (project in file(".")).
  settings(
    //その他の設定
    unmanagedJars in Compile += {
      val ps = new sys.SystemProperties
      val jh = ps("java.home")
      Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
    }
  )
