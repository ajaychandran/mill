package mill.client

import mill.client.lock.Locks
import mill.constants.{InputPumper, ProxyStream, ServerFiles, Util}

import java.io.{InputStream, OutputStream, PrintStream}
import java.net.{InetAddress, Socket}
import java.nio.file.{Files, Path}
import scala.util.Using

/**
 * Client side code that interacts with `Server.scala` in order to launch a generic
 * long-lived background server.
 *
 * The protocol is as follows:
 *
 * - Client:
 *   - Take clientLock
 *   - If processLock is not yet taken, it means server is not running, so spawn a server
 *   - Wait for server socket to be available for connection
 *
 * - Server:
 *   - Take processLock.
 *     - If already taken, it means another server was running
 *       (e.g. spawned by a different client) so exit immediately
 *
 * - Server: loop:
 *   - Listen for incoming client requests on serverSocket
 *   - Execute client request
 *   - If clientLock is released during execution, terminate server (otherwise
 *     we have no safe way of terminating the in-process request, so the server
 *     may continue running for arbitrarily long with no client attached)
 *   - Send `ProxyStream.END` packet and call `clientSocket.close()`
 *
 * - Client:
 *   - Wait for `ProxyStream.END` packet or `clientSocket.close()`,
 *     indicating server has finished execution and all data has been received
 */
abstract class ServerLauncher(
    stdin: InputStream,
    stdout: PrintStream,
    stderr: PrintStream,
    env: Map[String, String],
    args: Array[String],
    // For testing in memory, we need to pass in the locks separately, so that the
    // locks can be shared between the different instances of `ServerLauncher` the
    // same way file locks are shared between different Mill client/server processes
    memoryLocks: Array[Locks],
    forceFailureForTestingMillisDelay: Int,
    serverProcessesLimit: Int = 5,
    serverInitWaitMillis: Int = 10000
) {

  @throws[Exception]
  def initServer(serverDir: Path, b: Boolean, locks: Locks): Unit

  @throws[Exception]
  def preRun(serverDir: Path): Unit

  @throws[Exception]
  def acquireLocksAndRun(serverDir0: Path): ServerLaunchResult = {

    val setJnaNoSys = System.getProperty("jna.nosys") == null
    if (setJnaNoSys) {
      System.setProperty("jna.nosys", "true")
    }

    var serverIndex = 0
    var result: ServerLaunchResult = null
    while (null == result && serverIndex < serverProcessesLimit) { // Try each possible server process (-1 to -5)
      serverIndex += 1
      val serverDir =
        serverDir0.getParent.resolve(serverDir0.getFileName.toString + "-" + serverIndex)

      Files.createDirectories(serverDir)

      Using.Manager { use =>
        val locks = use(
          if memoryLocks == null then Locks.files(serverDir.toString)
          else memoryLocks(serverIndex - 1)
        )
        val clientLocked = use(locks.clientLock.tryLock())

        if (clientLocked.isLocked()) {
          preRun(serverDir)
          val exitCode = run(serverDir, setJnaNoSys, locks)
          result = ServerLaunchResult(exitCode, serverDir)
        }
      }.get
    }
    if (null == result)
      throw new ServerCouldNotBeStarted(
        "Reached max server processes limit: " + serverProcessesLimit
      )
    else result
  }

  @throws[Exception]
  def run(serverDir: Path, setJnaNoSys: Boolean, locks: Locks): Int = {

    Using(Files.newOutputStream(serverDir.resolve(ServerFiles.runArgs))) { f =>
      f.write(if (Util.hasConsole) 1 else 0)
      ClientUtil.writeString(f, BuildInfo.millVersion)
      ClientUtil.writeArgs(args, f)
      ClientUtil.writeMap(env, f)
    }.get

    if (locks.processLock.probe()) initServer(serverDir, setJnaNoSys, locks)

    while (locks.processLock.probe()) Thread.sleep(1)

    val retryStart = System.currentTimeMillis()
    var ioSocket: Socket = null
    var socketThrowable: Throwable = null
    while (ioSocket == null && System.currentTimeMillis() - retryStart < serverInitWaitMillis) {
      try {
        val port = Integer.parseInt(Files.readString(serverDir.resolve(ServerFiles.socketPort)))
        ioSocket = new java.net.Socket(InetAddress.getLoopbackAddress, port)
      } catch {
        case e: Throwable =>
          socketThrowable = e
          Thread.sleep(1)
      }
    }

    if (ioSocket == null) {
      throw new Exception("Failed to connect to server", socketThrowable)
    }

    val outErr = ioSocket.getInputStream
    val in = ioSocket.getOutputStream
    val outPumper = new ProxyStream.Pumper(outErr, stdout, stderr)
    val inPump = new InputPumper(() => stdin, () => in, true)
    val outPumperThread = new Thread(outPumper, "outPump")
    outPumperThread.setDaemon(true)
    val inThread = new Thread(inPump, "inPump")
    inThread.setDaemon(true)
    outPumperThread.start()
    inThread.start()

    if (forceFailureForTestingMillisDelay > 0) {
      Thread.sleep(forceFailureForTestingMillisDelay)
      throw new Exception("Force failure for testing: " + serverDir)
    }
    outPumperThread.join()

    try {
      val exitCodeFile = serverDir.resolve(ServerFiles.exitCode)
      if (Files.exists(exitCodeFile)) {
        Integer.parseInt(Files.readAllLines(exitCodeFile).get(0))
      } else {
        System.err.println("mill-server/ exitCode file not found")
        1
      }
    } finally {
      ioSocket.close()
    }
  }
}
