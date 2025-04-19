package mill.client

import java.io.{BufferedReader, File, IOException, PrintStream}
import java.nio.file.Files

class FileToStreamTailer(file: File, stream: PrintStream, intervalMsec: Int) extends Thread("Tail"),
      AutoCloseable {
  setDaemon(true)

  // if true, we won't read the whole file, but only new lines
  private var ignoreHead = true

  @volatile var keepReading = true
  @volatile var _flush = false

  override def run(): Unit = {
    if (isInterrupted) {
      keepReading = false
    }
    var reader: BufferedReader = null
    try {
      while (keepReading || _flush) {
        _flush = false
        try {
          // Init reader, if not already done
          if (null == reader) {
            try {
              reader = Files.newBufferedReader(file.toPath)
            } catch {
              case _: IOException =>
                // nothing to ignore if file is initially missing
                ignoreHead = false
            }
          }
          if (null != reader) {
            // read lines
            try {
              var line = reader.readLine()
              while (line != null) {
                if (!ignoreHead) {
                  stream.println(line)
                }
                line = reader.readLine()
              }
              // we ignored once
              this.ignoreHead = false
            } catch {
              case _: IOException =>
              // could not read line or file vanished
            }
          }
        } finally {
          if (keepReading) {
            // wait
            try {
              Thread.sleep(intervalMsec)
            } catch {
              case _: InterruptedException =>
              // can't handle anyway
            }
          }
        }
      }
    } finally {
      if (null != reader) {
        try {
          reader.close()
        } catch {
          case _: IOException =>
          // could not close but also can't do anything about it
        }
      }
    }
  }

  override def interrupt(): Unit = {
    this.keepReading = false
    super.interrupt()
  }

  /**
   * Force a next read, even if we interrupt the thread.
   */
  def flush(): Unit = {
    this._flush = true
  }

  @throws[Exception]
  def close(): Unit = {
    flush()
    interrupt()
  }
}
