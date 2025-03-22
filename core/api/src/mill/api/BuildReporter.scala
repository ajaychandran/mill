package mill.api

import sbt.testing.{Event, Status}

/**
 * Test reporter class that can be
 * injected into the test task and
 * report information upon the start
 * and the finish of testing events
 */
trait TestReporter {
  def logStart(event: Event): Unit

  def logFinish(event: Event): Unit

}

/**
 * Dummy Test Reporter that doesn't report
 * anything for any testing event.
 */
object DummyTestReporter extends TestReporter {
  override def logStart(event: Event): Unit = {}
  override def logFinish(event: Event): Unit = {}
}

/**
 * Callback for test progress notifications.
 */
trait TestProgressReporter {
  def logStart(fullyQualifiedName: String): Unit
  def logFinish(fullyQualifiedName: String, status: Status): Unit
}
