package mill.testrunner

import mill.api.{DummyTestReporter, TestProgressReporter, internal}
import sbt.testing.Status

@internal object TestRunnerMain0 {
  def main0(args: Array[String], classLoader: ClassLoader): Unit = {
    try {
      val testArgs = upickle.default.read[mill.testrunner.TestArgs](os.read(os.Path(args(1))))
      testArgs.sysProps.foreach { case (k, v) => System.setProperty(k, v) }
      val progressReporter = new ProgressReporter(testArgs.progressDir)

      val result = testArgs.globSelectors match {
        case Left(selectors) =>
          val filter = TestRunnerUtils.globFilter(selectors)
          TestRunnerUtils.runTestFramework0(
            frameworkInstances = Framework.framework(testArgs.framework),
            testClassfilePath = Seq.from(testArgs.testCp),
            args = testArgs.arguments,
            classFilter = cls => filter(cls.getName),
            cl = classLoader,
            testReporter = DummyTestReporter,
            progressReporter = progressReporter
          )
        case Right((startingTestClass, testClassQueueFolder, claimFolder)) =>
          TestRunnerUtils.queueTestFramework0(
            frameworkInstances = Framework.framework(testArgs.framework),
            testClassfilePath = Seq.from(testArgs.testCp),
            args = testArgs.arguments,
            startingTestClass = startingTestClass,
            testClassQueueFolder = testClassQueueFolder,
            claimFolder = claimFolder,
            cl = classLoader,
            testReporter = DummyTestReporter,
            progressReporter = progressReporter
          )
      }

      // Clear interrupted state in case some badly-behaved test suite
      // dirtied the thread-interrupted flag and forgot to clean up. Otherwise,
      // that flag causes writing the results to disk to fail
      Thread.interrupted()
      os.write(testArgs.outputPath, upickle.default.stream(result))
    } catch {
      case e: Throwable =>
        println(e)
        e.printStackTrace()
    }
    // Tests are over, kill the JVM whether or not anyone's threads are still running
    // Always return 0, even if tests fail. The caller can pick up the detailed test
    // results from the outputPath
    System.exit(0)
  }

  /**
   * Logs test progress notifications in files under `progressDir`.
   */
  class ProgressReporter(progressDir: os.Path) extends TestProgressReporter {
    def logStart(fullyQualifiedName: String): Unit = {
      os.write(progressDir / fullyQualifiedName, Array.emptyByteArray)
    }
    def logFinish(fullyQualifiedName: String, status: Status): Unit = {
      os.write.over(progressDir / fullyQualifiedName, status.name())
    }
  }
}
