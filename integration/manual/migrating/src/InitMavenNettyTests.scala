package mill.integration
import utest.*
object InitMavenNettyTests extends InitTestSuite(
  "https://github.com/netty/netty",
  "netty-4.2.10.Final",
  Seq("--mill-jvm-id", "17")
) {
  def tests = Tests {
    test("compile") - assert(
      eval("buffer.compile").isSuccess
    )
    test("test") - assert(
      eval("buffer.test").isSuccess
    )
    test("issues") {
      test("missing generated sources") - assert(
        !eval("codec-dns.compile").isSuccess
      )
      test("javadocGenerated fails") - assert(
        !eval("buffer.javadocGenerated").isSuccess
      )
      test("checkstyle fails") - assert(
        tester.eval("buffer.checkstyle").err.contains("Files to process must be specified, found 0.")
      )
      test("revapi requires JPMS support") - assert(
        !eval("buffer.revapi").isSuccess
      )
    }
  }
}