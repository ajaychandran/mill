package mill.integration
import utest.*
object InitMavenJansiTests extends InitTestSuite(
      "https://github.com/fusesource/jansi",
      "jansi-2.4.2",
      Seq("--mill-jvm-id", "17")
    ) {
  def tests = Tests {
    test("compile") - assert(
      eval("compile").isSuccess
    )
    test("test") - assert(
      eval("test").isSuccess
    )
    test("publish") - assert(
      eval(("publishLocal", "--local-ivy-repo", ".ivy2local")).isSuccess
    )
    test("issues") {
      test("spotless formatter configurations must be mapped manually") {
        tester.eval("spotless").err.contains("lint errors in 30 files must be fixed/suppressed")
      }
    }
  }
}
