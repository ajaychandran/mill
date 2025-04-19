package mill.client

import utest.*

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.file.Files
import scala.util.Using

object FileToStreamTailerTest extends TestSuite with TestSuite.Retries {

  def utestRetryCount = 3

  def tests = Tests {
    test("handleNonExistingFile") {
      val bas = new ByteArrayOutputStream()
      val ps = new PrintStream(bas)

      val file = File.createTempFile("tailer", "")
      assert(file.delete())

      Using(new FileToStreamTailer(file, ps, 10)) { tailer =>
        tailer.start()
        Thread.sleep(200)
        assert(bas.toString.isEmpty)
      }.get
    }
    test("handleNoExistingFileThatAppearsLater") {
      val bas = new ByteArrayOutputStream()
      val ps = new PrintStream(bas)

      val file = File.createTempFile("tailer", "")
      assert(file.delete())

      Using.Manager { use =>
        val tailer = use(new FileToStreamTailer(file, ps, 10))
        tailer.start()
        Thread.sleep(100)
        assert(bas.toString.isEmpty)

        val out = use(new PrintStream(Files.newOutputStream(file.toPath)))
        out.println("log line")
        assert(file.exists())
        Thread.sleep(100)
        assert(bas.toString == "log line" + System.lineSeparator())
      }.get
    }
    test("handleExistingInitiallyEmptyFile") {
      val bas = new ByteArrayOutputStream()
      val ps = new PrintStream(bas)

      val file = File.createTempFile("tailer", "")
      assert(file.exists())

      Using.Manager { use =>
        val tailer = use(new FileToStreamTailer(file, ps, 10))
        tailer.start()
        Thread.sleep(100)

        assert(bas.toString.isEmpty)

        val out = use(new PrintStream(Files.newOutputStream(file.toPath)))
        out.println("log line")
        assert(file.exists())
        Thread.sleep(100)
        assert(bas.toString == "log line" + System.lineSeparator())
      }.get
    }
    test("handleExistingFileWithOldContent") {
      val bas = new ByteArrayOutputStream()
      val ps = new PrintStream(bas)

      val file = File.createTempFile("tailer", "")
      assert(file.exists())

      Using.Manager { use =>
        val out = use(new PrintStream(Files.newOutputStream(file.toPath)))
        out.println("old line 1")
        out.println("old line 2")
        val tailer = use(new FileToStreamTailer(file, ps, 10))
        tailer.start()
        Thread.sleep(500)
        assert(bas.toString.isEmpty)
        out.println("log line")
        assert(file.exists())
        Thread.sleep(500)
        assert(bas.toString.trim == "log line")
      }.get
    }

    /* ignore
    test("handleExistingEmptyFileWhichDisappearsAndComesBack") {
      val bas = new ByteArrayOutputStream()
      val ps = new PrintStream(bas)

      val file = File.createTempFile("tailer", "")
      assert(file.exists())

      Using.Manager {
        use =>
          val tailer = use(new FileToStreamTailer(file, ps, 10))
          tailer.start()
          Thread.sleep(100)

          assert(bas.toString.isEmpty)

          val out = use(new PrintStream(Files.newOutputStream(file.toPath)))
          out.println("log line 1")
          out.println("log line 2")
          assert(file.exists())
          Thread.sleep(100)
          assert(
            bas.toString ==
              "log line 1" + System.lineSeparator() +
              "log line 2" + System.lineSeparator()
          )

        // Now delete file and give some time, then append new lines

        assert(file.delete())
        Thread.sleep(100)

        val out = use(new PrintStream(Files.newOutputStream(file.toPath)))
        out.println("new line")
        assert(file.exists())
        Thread.sleep(100)
        assert(
          bas.toString ==
            "log line 1" + System.lineSeparator() +
            "log line 2" + System.lineSeparator() +
            "new line" + System.lineSeparator()
        )
      }.get
    }
     */
  }
}
