import Dependencies._

ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val zioDeps = List(
  "dev.zio" %% "zio"                % "1.0.9",
  "dev.zio" %% "zio-metrics"        % "1.0.12",
  "dev.zio" %% "zio-metrics-statsd" % "1.0.4",
)

lazy val catsDeps = List(
  "org.typelevel"     %% "cats-effect"          % "3.2.2",
  "org.typelevel"     %% "cats-core"            % "2.3.1",
  "org.typelevel"     %% "alleycats-core"       % "2.3.1",
  "org.typelevel"     %% "cats-laws"            % "2.6.1" % Test,
  "org.typelevel"     %% "discipline-scalatest" % "2.1.5" % Test,
  "com.avast.cloud"   %% "datadog4s-statsd"     % "0.31.0",
  "co.fs2"            %% "fs2-core"             % "3.1.3",
  "co.fs2"            %% "fs2-io"               % "3.1.3",
  "de.ruedigermoeller" % "fst"                  % "2.56"
)

lazy val slickDeps = List(
  "com.typesafe.slick" %% "slick"          % "3.3.3",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
  "org.postgresql"      % "postgresql"     % "42.2.23",
  "com.kubukoz"        %% "slick-effect"   % "0.4.0",
)

lazy val root = (project in file("."))
  .settings(
    name := "fp",
    libraryDependencies ++= List(
      scalaTest                     % Test,
      "org.scalacheck"             %% "scalacheck"                % "1.15.4" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0"  % Test,
      "org.scalameta"              %% "munit"                     % "0.7.28" % Test,
      "com.lihaoyi"                %% "pprint"                    % "0.6.6",
      "com.chuusai"                %% "shapeless"                 % "2.3.3",
      "org.apache.commons"          % "commons-collections4"      % "4.4",
    )
      .++(catsDeps)
      .++(slickDeps),
    scalacOptions ++= Seq(
      "-deprecation",                              // Emit warning and location for usages of deprecated APIs.
      "-encoding",
      "utf-8",                                     // Specify character encoding used by source files.
      "-explaintypes",                             // Explain type errors in more detail.
      "-feature",                                  // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials",                    // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros",             // Allow macro definition (besides implementation and application). Disabled, as this will significantly change in Scala 3
      "-language:higherKinds",                     // Allow higher-kinded types
      "-language:implicitConversions",             // Allow definition of implicit functions called views. Disabled, as it might be dropped in Scala 3. Instead use extension methods (implemented as implicit class Wrapper(val inner: Foo) extends AnyVal {}
      "-unchecked",                                // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit",                               // Wrap field accessors to throw an exception on uninitialized access.
      "-Xlint:adapted-args",                       // Warn if an argument list is modified to match the receiver.
      "-Xlint:constant",                           // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select",                 // Selecting member of DelayedInit.
      "-Xlint:doc-detached",                       // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible",                       // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any",                          // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator",               // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-unit",                       // Warn when nullary methods return Unit.
      "-Xlint:option-implicit",                    // Option.apply used implicit view.
      "-Xlint:package-object-classes",             // Class or object defined in package object.
      "-Xlint:poly-implicit-overload",             // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow",                     // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align",                        // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow",              // A local type parameter shadows a type already in scope.
      "-Xlint:nonlocal-return",                    // A return statement used an exception for flow control.
      "-Xlint:implicit-not-found",                 // Check @implicitNotFound and @implicitAmbiguous messages.
      "-Xlint:implicit-recursion",                 // Warn when an implicit resolves to an enclosing self-definition.
      "-Xlint:serial",                             // @SerialVersionUID on traits and non-serializable classes.
      "-Xlint:valpattern",                         // Enable pattern checks in val definitions.
      "-Xlint:eta-zero",                           // Warn on eta-expansion (rather than auto-application) of zero-ary method.
      "-Xlint:eta-sam",                            // Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly annotated with @FunctionalInterface.
      "-Xlint:deprecation",                        // Enable linted deprecations.
      "-Wextra-implicit",                          // Warn when more than one implicit parameter section is defined.
      "-Wmacros:both",                             // Lints code before and after applying a macro
      "-Woctal-literal",                           // Warn on obsolete octal syntax.
      "-Wunused:locals",                           // Warn if a local definition is unused.
      "-Wunused:linted",
      "-Wvalue-discard",                           // Warn when non-Unit expression results are unused.
      "-Ybackend-parallelism",
      "8",                                         // Enable paralellisation — change to desired number!
      "-Ycache-plugin-class-loader:last-modified", // Enables caching of classloaders for compiler plugins
      "-Ycache-macro-class-loader:last-modified",  // and macro definitions. This can lead to performance improvements.
      // "-Xfatal-warnings",                          // Fail the compilation if there are any warnings.
      // "-Xlint:nullary-override",                   // Warn when non-nullary `def f()' overrides nullary `def f'.
      // "-Xlint:unused",                             // TODO check if we still need -Wunused below
      // "-Wdead-code",                               // Warn when dead code is identified.
      // "-Wnumeric-widen",                           // Warn when numerics are widened.
      // "-Wunused:imports",                          // Warn if an import selector is not referenced.
      // "-Wunused:patvars",                          // Warn if a variable bound in a pattern is unused.
      // "-Wunused:privates",                         // Warn if a private member is unused.
      // "-Wunused:explicits",                        // Warn if an explicit parameter is unused.
      // "-Wunused:implicits",                        // Warn if an implicit parameter is unused.
      // "-Wunused:params",                           // Enable -Wunused:explicits,implicits.
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
