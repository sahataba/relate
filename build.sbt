import org.scalajs.linker.interface.ModuleSplitStyle

val publicDev = taskKey[String]("output directory for `npm run dev`")
val publicProd = taskKey[String]("output directory for `npm run build`")

lazy val `test-vite` = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    scalaVersion := "3.3.1",
    scalacOptions ++= Seq("-encoding", "utf-8", "-deprecation", "-feature"),

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("testvite")))
    },

    externalNpm := {
      //scala.sys.process.Process(List("npm", "install", "--silent", "--no-audit", "--no-fund"), baseDirectory.value).!
      baseDirectory.value
    },

    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "16.0.0",
      "com.raquo" %%% "waypoint" % "7.0.0",
      "dev.zio" %%% "zio-json" % "0.6.2",
      "com.github.japgolly.scalacss" %%% "core" % "1.0.0",
      "com.lihaoyi" %%% "pprint" % "0.8.1",
      "com.lihaoyi" %%% "upickle" % "3.1.2",
      ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
      "be.doeraene" %%% "web-components-ui5" % "1.10.0"
    ),

    publicDev := linkerOutputDirectory((Compile / fastLinkJS).value).getAbsolutePath(),
    publicProd := linkerOutputDirectory((Compile / fullLinkJS).value).getAbsolutePath(),
  )

def linkerOutputDirectory(v: Attributed[org.scalajs.linker.interface.Report]): File = {
  v.get(scalaJSLinkerOutputDirectory.key).getOrElse {
    throw new MessageOnlyException(
        "Linking report was not attributed with output directory. " +
        "Please report this as a Scala.js bug.")
  }
}
