package mill.testrunner

import mill.api.{Logger, TestProgressReporter, internal}
import sbt.testing.Status

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

/**
 * Logs test progress notifications to the `logger`'s prompt.
 * If the number of tests is not known beforehand, pass `0` for `total`.
 */
@internal class PromptProgressReporter(logger: Logger, total: Int)
    extends TestProgressReporter {
  private type State = (Seq[String], Option[Status])
  private val states = new ConcurrentHashMap[String, State]()
  private val count = new AtomicInteger()
  updatePrompt()

  private def updatePrompt() = {
    import scala.jdk.CollectionConverters.given
    var success, failure, elided = 0
    states.asScala.foreach {
      case (_, (_, status)) =>
        status.foreach {
          case Status.Success => success += 1
          case Status.Failure => failure += 1
          case _ => elided += 1
        }
    }
    def suffixPositive(value: Int, name: String) = if (value > 0) s", $value $name" else ""
    val started = count.get()
    val prefixStarted = if (total > 0) s"[$started/$total]" else started.toString
    val detail = new StringBuilder(128)
      .append(s"$prefixStarted started")
      .append(suffixPositive(success, "succeeded"))
      .append(suffixPositive(failure, "failed"))
      .append(suffixPositive(elided, "elided"))
      .result()
    logger.ticker(detail)
  }

  private def nextKey() = {
    val last0 = count.getAndIncrement().toString
    val last = "0" * (total.toString.length - last0.length) + last0
    logger.logKey :+ last
  }

  def logStart(fullyQualifiedName: String): Unit = {
    if (!states.containsKey(fullyQualifiedName)) {
      val key = nextKey()
      states.put(fullyQualifiedName, (key, None))
      logger.prompt.setPromptLine(key, "", fullyQualifiedName)
      updatePrompt()
    }
  }

  def logFinish(fullyQualifiedName: String, status: Status): Unit = {
    val (key, status0) =
      if (states.containsKey(fullyQualifiedName)) states.get(fullyQualifiedName)
      else (nextKey(), None)
    if (status0.isEmpty) {
      states.put(fullyQualifiedName, (key, Some(status)))
      logger.prompt.removePromptLine(key)
      updatePrompt()
    }
  }
}
