package mill.runner.client

import mill.client.lock.Locks
import mill.client.{ClientUtil, ServerCouldNotBeStarted, ServerLauncher}
import mill.constants.{OutFiles, Util}

import java.nio.file.{Path, Paths}

object MillClientMain {

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    var runNoServer = false
    if (args.length > 0) {
      val firstArg = args(0)
      runNoServer = Seq("--interactive", "--no-server", "--repl", "--bsp", "--help")
        .contains(firstArg)
        || firstArg.startsWith("-i")
    }
    if (!runNoServer) {
      // WSL2 has the directory /run/WSL/ and WSL1 not.
      val osVersion = System.getProperty("os.version")
      if (osVersion != null && (osVersion.contains("icrosoft") || osVersion.contains("WSL"))) {
        // Server-Mode not supported under WSL1
        runNoServer = true
      }
    }

    if (runNoServer) {
      // start in no-server mode
      System.exit(MillProcessLauncher.launchMillNoServer(args))
    } else
      try {
        // start in client-server mode
        val optsArgs = ClientUtil.readOptsFileLines(MillProcessLauncher.millOptsFile()) ++ args

        val launcher =
          new ServerLauncher(
            System.in,
            System.out,
            System.err,
            sys.env,
            optsArgs.toArray,
            null,
            -1
          ) {
            def initServer(serverDir: Path, setJnaNoSys: Boolean, locks: Locks): Unit = {
              MillProcessLauncher.launchMillServer(serverDir, setJnaNoSys)
            }

            def preRun(serverDir: Path): Unit = {
              MillProcessLauncher.prepareMillRunFolder(serverDir)
            }
          }

        val versionAndJvmHomeEncoding = Util.md5hex(
          mill.client.BuildInfo.millVersion + MillProcessLauncher.javaHome().getOrElse("")
        )
        val serverDir0 = Paths.get(OutFiles.out, OutFiles.millServer, versionAndJvmHomeEncoding)
        var exitCode = launcher.acquireLocksAndRun(serverDir0).exitCode
        if (exitCode == ClientUtil.ExitServerCodeWhenVersionMismatch()) {
          exitCode = launcher.acquireLocksAndRun(serverDir0).exitCode
        }
        System.exit(exitCode)
      } catch {
        case e: ServerCouldNotBeStarted =>
          // TODO: try to run in-process
          System.err.println("Could not start a Mill server process.\n"
            + "This could be caused by too many already running Mill instances "
            + "or by an unsupported platform.\n"
            + e.getMessage + "\n")

          System.err.println(
            "Loading Mill in-process isn't possible.\n" + "Please check your Mill installation!"
          )
          throw e
        case e: Exception =>
          System.err.println("Mill client failed with unknown exception")
          e.printStackTrace()
          System.exit(1)
      }
  }
}
