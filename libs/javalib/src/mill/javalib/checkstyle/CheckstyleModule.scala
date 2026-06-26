package mill.javalib.checkstyle

import mill.*
import mill.api.PathRef
import mill.javalib.{Dep, DepSyntax, JavaModule}
import mill.util.Jvm
import mill.api.BuildCtx

import scala.util.Properties.isWin

/**
 * Performs quality checks on Java source files using [[https://checkstyle.org/ Checkstyle]].
 */
trait CheckstyleModule extends JavaModule {

  /**
   * Runs [[https://checkstyle.org/cmdline.html#Command_line_usage Checkstyle]] and returns one of
   *  - number of violations found
   *  - program exit code
   *
   * @note [[sources]] are processed when no [[CheckstyleArgs.sources]] are specified.
   */
  def checkstyle(@mainargs.arg checkstyleArgs: CheckstyleArgs): Command[Int] = Task.Command {
    val (output, exitCode) = checkstyle0(checkstyleArgs.stdout, checkstyleArgs.sources)()

    checkstyleHandleErrors(checkstyleArgs.stdout, checkstyleArgs.check, exitCode, output)
  }

  protected def checkstyle0(stdout: Boolean, leftover: mainargs.Leftover[String]) = Task.Anon {

    val output = checkstyleOutput().path
    val checkstylePropertiesFileExists = os.exists(checkstylePropertiesFile().path)
    val args = checkstyleOptions() ++
      (if (!checkstyleOptions().contains("-c") && os.exists(checkstyleConfig().path)) {
         Seq("-c", checkstyleConfig().path.toString)
       } else Nil) ++
      (if (!checkstyleOptions().contains("-f")) {
         Seq("-f", checkstyleFormat())
       } else Nil) ++
      (if (!checkstyleOptions().contains("-o") && !stdout) {
         Seq("-o", output.toString())
       } else Nil) ++
      (if (!checkstyleOptions().contains("-p") && checkstylePropertiesFileExists)
         Seq("-p", checkstylePropertiesFile().path.toString)
       else Nil) ++
      (if (leftover.value.nonEmpty) leftover.value else sources().map(PathRef.realAbs))
    val jvmArgs =
      // CLI system properties are ignored if properties file exists
      if (checkstylePropertiesFileExists) Nil
      else {
        // On Windows, CLI system properties should be wrapped in double quotes.
        val encodeProp = if (isWin) (kv: (String, String)) => s"-D\"${kv._1}=${kv._2}\""
        else (kv: (String, String)) => s"-D${kv._1}=${kv._2}"
        checkstyleProperties().toSeq.map(encodeProp)
      }

    Task.log.info("running checkstyle ...")
    Task.log.debug(s"with $args")
    Task.log.debug(s"with jvmArgs: $jvmArgs")

    Task.log.info(s"pwd: ${os.pwd}")
    val exitCode = Jvm.callProcess(
      mainClass = "com.puppycrawl.tools.checkstyle.Main",
      classPath = checkstyleClasspath().map(_.path).toVector,
      mainArgs = args,
      cwd = moduleDir,
      stdin = os.Inherit,
      stdout = os.Inherit,
      check = false,
      jvmArgs = jvmArgs
    ).exitCode

    (output, exitCode)
  }

  protected def checkstyleHandleErrors(
      stdout: Boolean,
      check: Boolean,
      exitCode: Int,
      output: os.Path
  )(using ctx: mill.api.TaskCtx): Int = {

    val reported = os.exists(output)
    if (reported) {
      Task.log.info(s"checkstyle output report at $output")
    }

    if (exitCode == 0) {} // do nothing
    else if (exitCode < 0 || !(reported || stdout)) {
      Task.log.error(
        s"checkstyle exit($exitCode); please check command arguments, plugin settings or try with another version"
      )
      throw UnsupportedOperationException(s"checkstyle exit($exitCode)")
    } else if (check) {
      throw RuntimeException(s"checkstyle found $exitCode violation(s)")
    } else {
      Task.log.error(s"checkstyle found $exitCode violation(s)")
    }

    exitCode
  }

  /**
   * Classpath for running Checkstyle.
   */
  def checkstyleClasspath: T[Seq[PathRef]] = Task {
    defaultResolver().classpath[Dep](checkstyleMvnDeps())
  }

  /**
   * Checkstyle configuration file. Defaults to `checkstyle-config.xml`.
   * To specify a classpath  resource within [[checkstyleMvnDeps]] like `/google_checks.xml`, add
   * the path to [[checkstyleOptions]] with the `-c` option.
   */
  def checkstyleConfig: T[PathRef] = Task.Source {
    BuildCtx.workspaceRoot / "checkstyle-config.xml"
  }

  /**
   * Checkstyle output format (` plain | sarif | xml `). Defaults to `plain`.
   */
  def checkstyleFormat: T[String] = Task {
    "plain"
  }

  /**
   * Additional arguments for Checkstyle.
   */
  def checkstyleOptions: T[Seq[String]] = Task {
    Seq.empty[String]
  }

  /**
   * User language of the JVM running checkstyle.
   *
   * This can affect the messages in the checkstyle output file.
   */
  def checkstyleLanguage: T[Option[String]] = Task.Input {
    sys.props.get("user.language")
  }

  /**
   * Checkstyle output report.
   */
  def checkstyleOutput: T[PathRef] = Task {
    PathRef(Task.dest / s"checkstyle-output.${checkstyleFormat()}")
  }

  /**
   * Checkstyle version.
   */
  def checkstyleVersion: T[String] = Task {
    "10.18.1"
  }

  def checkstyleMvnDeps: T[Seq[Dep]] = Task {
    Seq(mvn"com.puppycrawl.tools:checkstyle:${checkstyleVersion()}")
  }

  /**
   * System properties for Checkstyle.
   *
   * @see [[https://checkstyle.sourceforge.io/config_system_properties.html]]
   */
  def checkstyleProperties: T[Map[String, String]] = Task {
    checkstyleLanguage().map("user.language" -> _).toMap
  }

  /**
   * File containing system properties for Checkstyle.
   *
   * @see [[https://checkstyle.sourceforge.io/cmdline.html#Using_a_Properties_File]]
   */
  def checkstylePropertiesFile: T[PathRef] =
    Task.Source(BuildCtx.workspaceRoot / "checkstyle.properties")
}
