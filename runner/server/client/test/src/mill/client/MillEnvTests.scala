package mill.client

import utest.*

import java.nio.file.Paths

object MillEnvTests extends TestSuite {

  def tests = Tests {
    test("readOptsFileLinesWithoutFInalNewline") {
      val file = Paths.get(
        getClass.getClassLoader.getResource("file-wo-final-newline.txt").toURI
      )
      val lines = ClientUtil.readOptsFileLines(file)
      assert(lines == List(
        "-DPROPERTY_PROPERLY_SET_VIA_JVM_OPTS=value-from-file",
        "-Xss120m"
      ))
    }
  }
}
