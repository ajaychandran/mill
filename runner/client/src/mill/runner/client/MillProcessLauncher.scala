package mill.runner.client

import io.github.alexarchambault.nativeterm.NativeTerminal
import mill.client.ClientUtil
import mill.constants.OutFiles.*
import mill.constants.{EnvVars, ServerFiles, Util}

import java.io.{File, IOException}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicReference
import java.util.{Comparator, Properties, UUID}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object MillProcessLauncher {

  @throws[Exception]
  def launchMillNoServer(args: Array[String]): Int = {
    val setJnaNoSys = System.getProperty("jna.nosys") == null
    val sig = "%08x".format(UUID.randomUUID().hashCode())
    val processDir = Paths.get(".").resolve(out).resolve(millNoServer).resolve(sig)

    val cmd = Seq.newBuilder[String]
      .addAll(millLaunchJvmCommand(setJnaNoSys))
      .addOne("mill.runner.MillMain")
      .addOne(processDir.toAbsolutePath.toString)
      .addAll(ClientUtil.readOptsFileLines(millOptsFile()))
      .addAll(args)
      .result()
      .asJava
    val builder = new ProcessBuilder().command(cmd).inheritIO()
    var interrupted = false
    try {
      prepareMillRunFolder(processDir)
      val p = configureRunMillProcess(builder, processDir)
      p.waitFor()
    } catch {
      case e: InterruptedException =>
        interrupted = true
        throw e
    } finally {
      if (!interrupted && Files.exists(processDir)) {
        // cleanup if process terminated for sure
        Using(Files.walk(processDir)) { stream =>
          stream.sorted(Comparator.reverseOrder()).forEach(p => p.toFile.delete())
        }.get
      }
    }
  }

  @throws[Exception]
  def launchMillServer(serverDir: Path, setJnaNoSys: Boolean): Unit = {
    val cmd = Seq.newBuilder[String]
      .addAll(millLaunchJvmCommand(setJnaNoSys))
      .addOne("mill.runner.MillServerMain")
      .addOne(serverDir.toFile.getCanonicalPath)
      .result()
      .asJava
    val builder = new ProcessBuilder()
      .command(cmd)
      .redirectOutput(serverDir.resolve(ServerFiles.stdout).toFile)
      .redirectError(serverDir.resolve(ServerFiles.stderr).toFile)
    configureRunMillProcess(builder, serverDir)
  }

  @throws[Exception]
  def configureRunMillProcess(builder: ProcessBuilder, serverDir: Path): Process = {
    val sandbox = serverDir.resolve(ServerFiles.sandbox)
    Files.createDirectories(sandbox)
    builder.directory(sandbox.toFile)

    builder.environment().put(EnvVars.MILL_WORKSPACE_ROOT, new File("").getCanonicalPath)
    val jdkJavaOptions = Option(System.getenv("JDK_JAVA_OPTIONS")).getOrElse("")
    val javaOpts = Option(System.getenv("JAVA_OPTS")).getOrElse("")
    val opts = (jdkJavaOptions + " " + javaOpts).trim
    if (opts.nonEmpty) {
      builder.environment().put("JDK_JAVA_OPTIONS", opts)
    }

    builder.start()
  }

  def millJvmVersionFile(): Path = {
    val path = Option(System.getenv(EnvVars.MILL_JVM_VERSION_PATH)).getOrElse(".mill-jvm-version")
    Paths.get(path).toAbsolutePath
  }

  def millJvmOptsFile(): Path = {
    val path = Option(System.getenv(EnvVars.MILL_JVM_OPTS_PATH)).getOrElse(".mill-jvm-opts")
    Paths.get(path).toAbsolutePath
  }

  def millOptsFile(): Path = {
    val path = Option(System.getenv(EnvVars.MILL_OPTS_PATH)).getOrElse(".mill-opts")
    Paths.get(path).toAbsolutePath
  }

  def millServerTimeout: Option[String] = {
    Option(System.getenv(EnvVars.MILL_SERVER_TIMEOUT_MILLIS))
  }

  @throws[Exception]
  def javaHome(): Option[String] = {
    def millJvmId(millJvmVersionFile: Path) = {
      Option.when(Files.exists(millJvmVersionFile))(Files.readString(millJvmVersionFile).trim)
    }
    def defaultJvmId = {
      val ignoreSystemJava = System.getenv("MILL_TEST_SUITE_IGNORE_SYSTEM_JAVA") == null
      def systemJavaExists =
        new ProcessBuilder(
          if Util.isWindows then "where" else "which",
          "java"
        ).start().waitFor() == 0
      Option.when(!(ignoreSystemJava && systemJavaExists))(mill.client.BuildInfo.defaultJvmId)
    }
    def savedJavaHome(millJavaHomeFile: Path, jvmId: String) = {
      if (Files.exists(millJavaHomeFile)) {
        Files.readString(millJavaHomeFile).split(" ") match
          case Array(`jvmId`, javaHome) =>
            // Make sure we check to see if the saved java home exists before using
            // it, since it may have been since uninstalled, or the `out/` folder
            // may have been transferred to a different machine
            Option.when(Files.exists(Paths.get(javaHome)))(javaHome)
          case _ =>
            None
      } else None
    }
    def resolveAndSaveJavaHome(jvmId: String, millJavaHomeFile: Path) = {
      val javaHome = CoursierClient.resolveJavaHome(jvmId).getAbsolutePath
      Files.createDirectories(millJavaHomeFile.getParent)
      Files.write(millJavaHomeFile, (jvmId + " " + javaHome).getBytes())
      javaHome
    }

    millJvmId(millJvmVersionFile())
      .orElse(defaultJvmId)
      .map { jvmId =>
        // Fast path to avoid calling `CoursierClient` and paying the classloading cost
        // when the `javaHome` JVM has already been initialized for the configured `jvmId`
        // and is ready to use directly
        val millJavaHomeFile = Paths.get(".").resolve(out).resolve(millJavaHome)
        savedJavaHome(millJavaHomeFile, jvmId)
          .getOrElse(resolveAndSaveJavaHome(jvmId, millJavaHomeFile))
      }
      .orElse(Option(System.getProperty("java.home")))
      .orElse(Option(System.getenv("JAVA_HOME")))
  }

  @throws[Exception]
  def javaExe(): String = {
    javaHome() match
      case Some(home) =>
        val exePath = Paths.get(
          home + File.separator + "bin" + File.separator + "java" +
            (if Util.isWindows then ".exe" else "")
        )
        exePath.toAbsolutePath.toString
      case None =>
        "java"
  }

  @throws[Exception]
  def millClasspath(): Seq[String] = {
    var selfJars = ""
    val millOptionsPath = System.getProperty("MILL_OPTIONS_PATH")
    if (millOptionsPath != null) {

      // read MILL_CLASSPATH from file MILL_OPTIONS_PATH
      val millProps = new Properties()
      val is = Files.newInputStream(Paths.get(millOptionsPath))
      try {
        millProps.load(is)
      } catch {
        case e: IOException =>
          throw new RuntimeException("Could not load '" + millOptionsPath + "'", e)
      } finally is.close()

      selfJars = millProps.getProperty("MILL_CLASSPATH", "")
    } else {
      // read MILL_CLASSPATH from file sys props
      selfJars = System.getProperty("MILL_CLASSPATH")
    }

    if (selfJars == null || selfJars.trim.isEmpty) {
      // We try to use the currently local classpath as MILL_CLASSPATH
      selfJars = System.getProperty("java.class.path").replace(File.pathSeparator, ",")
    }

    if (selfJars == null || selfJars.trim.isEmpty) {
      // Assuming native assembly run
      selfJars = getClass
        .getProtectionDomain
        .getCodeSource
        .getLocation
        .getPath
    }

    if (selfJars == null || selfJars.trim().isEmpty) {
      throw new RuntimeException("MILL_CLASSPATH is empty!")
    }

    selfJars.split(",").iterator
      .map(new java.io.File(_).getCanonicalPath)
      .toSeq
  }

  @throws[Exception]
  def millLaunchJvmCommand(setJnaNoSys: Boolean): Seq[String] = {
    val vmOptions = Seq.newBuilder[String]

    // Java executable
    vmOptions.addOne(javaExe())

    // jna
    if (setJnaNoSys) {
      vmOptions.addOne("-Djna.nosys=true")
    }

    // sys props
    vmOptions.addAll(
      sys.props.iterator
        .collect {
          case (k, v) if k.startsWith("MILL_") && k != "MILL_CLASSPATH" => s"-D$k=$v"
        }
    )

    vmOptions.addAll(
      millServerTimeout.map("-Dmill.server_timeout=" + _)
    )

    // extra opts
    val _millJvmOptsFile = millJvmOptsFile()
    if (Files.exists(_millJvmOptsFile)) {
      vmOptions.addAll(ClientUtil.readOptsFileLines(_millJvmOptsFile))
    }

    vmOptions.addOne("-XX:+HeapDumpOnOutOfMemoryError")
    vmOptions.addOne("-cp")
    vmOptions.addOne(millClasspath().mkString(File.pathSeparator))

    vmOptions.result()
  }

  @throws[Exception]
  def getTerminalDim(s: String, inheritError: Boolean): Int = {
    val proc = new ProcessBuilder()
      .command("tput", s)
      .redirectOutput(ProcessBuilder.Redirect.PIPE)
      .redirectInput(ProcessBuilder.Redirect.INHERIT)
      // We cannot redirect error to PIPE, because `tput` needs at least one of the
      // outputstreams inherited so it can inspect the stream to get the console
      // dimensions. Instead, we check up-front that `tput cols` and `tput lines` do
      // not raise errors, and hope that means it continues to work going forward
      .redirectError(
        if inheritError then ProcessBuilder.Redirect.INHERIT else ProcessBuilder.Redirect.PIPE
      )
      .start()

    val exitCode = proc.waitFor()
    if (exitCode != 0) throw new Exception("tput failed")
    Integer.parseInt(new String(proc.getInputStream.readAllBytes()).trim())
  }

  private val memoizedTerminalDims = new AtomicReference[String]()

  private lazy val canUseNativeTerminal = {
    JLineNativeLoader.initJLineNative()
    if (mill.constants.Util.hasConsole) {
      try {
        NativeTerminal.getSize
        true
      } catch {
        case _: Throwable =>
          false
      }
    } else false
  }

  @throws[Exception]
  def writeTerminalDims(tputExists: Boolean, serverDir: Path): Unit = {
    var str: String = null

    try {
      if (!mill.constants.Util.hasConsole) str = "0 0"
      else {
        if (canUseNativeTerminal) {

          val size = NativeTerminal.getSize
          val width = size.getWidth
          val height = size.getHeight
          str = width + " " + height
        } else if (!tputExists) {
          // Hardcoded size of a quarter screen terminal on 13" windows laptop
          str = "78 24"
        } else {
          str = getTerminalDim("cols", true) + " " + getTerminalDim("lines", true)
        }
      }
    } catch {
      case _: Exception =>
        str = "0 0"
    }

    // We memoize previously seen values to avoid causing lots
    // of upstream work if the value hasn't actually changed.
    // The upstream work could cause significant load, see
    //
    //    https://github.com/com-lihaoyi/mill/discussions/4092
    //
    // The cause is currently unknown, but this fixes the symptoms at least.
    //
    val oldValue = memoizedTerminalDims.getAndSet(str)
    if ((oldValue == null) || oldValue != str) {
      Files.write(serverDir.resolve(ServerFiles.terminfo), str.getBytes())
    }
  }

  def checkTputExists(): Boolean = {
    try {
      getTerminalDim("cols", false)
      getTerminalDim("lines", false)
      true
    } catch {
      case _: Exception =>
        false
    }
  }

  @throws[Exception]
  def prepareMillRunFolder(serverDir: Path): Unit = {
    // Clear out run-related files from the server folder to make sure we
    // never hit issues where we are reading the files from a previous run
    Files.deleteIfExists(serverDir.resolve(ServerFiles.exitCode))
    Files.deleteIfExists(serverDir.resolve(ServerFiles.terminfo))
    Files.deleteIfExists(serverDir.resolve(ServerFiles.runArgs))

    val sandbox = serverDir.resolve(ServerFiles.sandbox)
    Files.createDirectories(sandbox)
    val tputExists = checkTputExists()

    writeTerminalDims(tputExists, serverDir)
    val termInfoPropagatorThread = new Thread(
      () => {
        try {
          while (true) {
            writeTerminalDims(tputExists, serverDir)
            Thread.sleep(100)
          }
        } catch {
          case _: Exception =>
        }
      },
      "TermInfoPropagatorThread"
    )
    termInfoPropagatorThread.setDaemon(true)
    termInfoPropagatorThread.start()
  }
}
