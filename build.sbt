import org.scalajs.linker.interface.ModuleSplitStyle

val publicDev = taskKey[String]("output directory for `npm run dev`")
val publicProd = taskKey[String]("output directory for `npm run build`")

lazy val `test-vite` = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.3.0",
    scalacOptions ++= Seq("-encoding", "utf-8", "-deprecation", "-feature"),

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("testvite")))
    },

    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "16.0.0",
      "com.raquo" %%% "waypoint" % "7.0.0",
      "dev.zio" %%% "zio-json" % "0.6.0",
      "com.github.japgolly.scalacss" %%% "core" % "1.0.0",
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
