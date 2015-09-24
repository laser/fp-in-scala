lazy val root = (project in file(".")).
  settings(
    name := "fp-in-scala",
    version := "0.1.0",
    scalaVersion := "2.11.7",

    // add a test dependency on ScalaCheck
    libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4",
    libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.6.4",

    // reduce the maximum number of errors shown by the Scala compiler
    maxErrors := 20,

    // increase the time between polling for file changes when using continuous execution
    pollInterval := 100,

    // append -deprecation to the options passed to the Scala compiler
    scalacOptions += "-deprecation",

    // set Ivy logging to be at the highest level
    ivyLoggingLevel := UpdateLogging.Full,

    // disable printing timing information, but still print [success]
    showTiming := false,

    // disable printing a message indicating the success or failure of running a task
    showSuccess := false,

    // only show 10 lines of stack traces
    traceLevel := 20
  )
