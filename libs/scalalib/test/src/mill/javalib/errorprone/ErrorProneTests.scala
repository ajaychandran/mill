package mill.javalib.errorprone

import mill.T
import mill.define.Discover
import mill.scalalib.JavaModule
import mill.testkit.{TestBaseModule, UnitTester}
import os.Path
import utest.*
import mill.util.TokenReaders._
object ErrorProneTests extends TestSuite {

  object noErrorProne extends TestBaseModule with JavaModule {
    lazy val millDiscover = Discover[this.type]
  }
  object errorProne extends TestBaseModule with JavaModule with ErrorProneModule {
    lazy val millDiscover = Discover[this.type]
  }
  object errorProneCustom extends TestBaseModule with JavaModule with ErrorProneModule {
    override def errorProneOptions: T[Seq[String]] = T(Seq(
      "-XepAllErrorsAsWarnings"
    ))
    lazy val millDiscover = Discover[this.type]
  }

  val testModuleSourcesPath: Path = os.Path(sys.env("MILL_TEST_RESOURCE_DIR")) / "errorprone"

  def tests = Tests {
    test("reference") {
      test("compile") {
        UnitTester(noErrorProne, testModuleSourcesPath).scoped { eval =>

          val res = eval(noErrorProne.compile)
          assert(res.isRight)
        }
      }
    }
    test("errorprone") {
      test("compileFail") {
        UnitTester(errorProne, testModuleSourcesPath).scoped { eval =>
          val res = eval(errorProne.compile)
          assert(res.isLeft)
        }
      }
      test("compileWarn") {
        UnitTester(errorProneCustom, testModuleSourcesPath, debugEnabled = true).scoped { eval =>
          val Right(opts) = eval(errorProneCustom.mandatoryJavacOptions): @unchecked
          assert(opts.value.exists(_.contains("-XepAllErrorsAsWarnings")))
          val res = eval(errorProneCustom.compile)
          assert(res.isRight)
        }
      }
    }
  }
}
