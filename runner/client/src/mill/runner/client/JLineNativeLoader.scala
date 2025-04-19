package mill.runner.client

import mill.constants.Util
import org.jline.nativ.OSInfo

import java.io.IOException
import java.nio.file.*
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Using

/**
 * Helper to load the jline-native native library before jline-native itself attempts to do so.
 *
 * We load it from a location that doesn't change. That way, if the library is already
 * there, we can load it straightaway. That's the "fast path".
 *
 * If the library isn't there already, we write it there first. That's the "slow path".
 * This should need to run only once on the user's machine. Once the library is there,
 * we can go through the fast path above every time.
 *
 * If we don't do that, jline-native loads its library on its own, and always does things slowly,
 * by writing its library in a new temporary location upon each run.
 */
class JLineNativeLoader private (
    val jlineNativeVersion: String,
    val millJLineNativeDir: Path,
    val millJLineNativeLibLocation: Path
) {

  def tryLoadFast(): Boolean = {
    Files.exists(millJLineNativeLibLocation)
  }

  // If the jlinenative native library isn't in cache (tryLoadFast returns null), loadSlow
  // reads it from the resources and writes it on disk, which is more heavyweight.
  // That's the slow path of our jlinenative-loading logic, that we try to avoid when we can.
  def loadSlow(): Unit = {
    val tmpLocation = millJLineNativeLibLocation
      .getParent
      .resolve(millJLineNativeLibLocation.getFileName.toString + "-" + UUID.randomUUID())
    try {
      Files.createDirectories(millJLineNativeLibLocation.getParent)
      Using.Manager { use =>
        val is = use(Thread.currentThread().getContextClassLoader.getResourceAsStream(
          JLineNativeLoader.jlineNativeLibResourcePath()
        ))
        val os = use(Files.newOutputStream(tmpLocation))
        is.transferTo(os)
      }.get
      // Concurrent Mill processes might try to create millJLineNativeLibLocation too, so we ignore
      // errors if the file has been written by another process in the mean time.
      // Also, we move it atomically to its final location, so that if another Mill process finds
      // it, it can use it fine straightaway.
      if (!Files.exists(millJLineNativeLibLocation))
        try {
          Files.move(tmpLocation, millJLineNativeLibLocation, StandardCopyOption.ATOMIC_MOVE)
        } catch {
          case _: FileAlreadyExistsException =>
          // Ignored, file should have been created by another Mill process
          case _: AtomicMoveNotSupportedException =>
            try {
              Files.move(tmpLocation, millJLineNativeLibLocation)
            } catch {
              case _: FileAlreadyExistsException =>
              // Ignored, file should have been created by another Mill process
            }
        }
    } catch {
      case ex: IOException =>
        throw new RuntimeException(ex)
    } finally {
      try {
        Files.deleteIfExists(tmpLocation)
      } catch {
        case _: IOException =>
        // ignored
      }
    }
  }
}

object JLineNativeLoader {

  def apply(jlineNativeVersion: String): JLineNativeLoader = {
    val baseDir =
      if Util.isWindows then Paths.get(System.getenv("UserProfile")).resolve(".mill/cache/")
      else Paths.get(System.getProperty("user.home")).resolve(".cache/mill/")
    val millJLineNativeDir = baseDir.resolve("jline/" + jlineNativeVersion)
    val millJLineNativeLibLocation =
      millJLineNativeDir.resolve(OSInfo.getNativeLibFolderPathForCurrentOS + "/"
        + System.mapLibraryName("jlinenative").replace(".dylib", ".jnilib"))
    new JLineNativeLoader(jlineNativeVersion, millJLineNativeDir, millJLineNativeLibLocation)
  }

  def jlineNativeLibResourcePath(): String = {
    "org/jline/nativ/" + OSInfo.getNativeLibFolderPathForCurrentOS + "/" +
      // Replacing '.dylib' by '.jnilib' is necessary, as jlinenative uses the latter extension on
      // macOS, rather than '.dylib', which is the default. The call to replace has no effect on
      // other platforms.
      System.mapLibraryName("jlinenative").replace(".dylib", ".jnilib")
  }

  def initJLineNative(): Unit = {
    if (initialized.compareAndSet(false, true)) doInitJLineNative()
  }

  private val initialized = new AtomicBoolean(false)

  private def doInitJLineNative(): Unit = {
    val loader = JLineNativeLoader(mill.runner.client.Versions.jlineNativeVersion)
    if (!loader.tryLoadFast()) loader.loadSlow()

    // In theory, this should be enough for org.jline.nativ.JLineNativeLoader
    // to use the JAR we cached ourselves.
    // But org.jline.nativ.JLineNativeLoader.initialize() also starts a "clean-up" thread,
    // which slows things down too apparently. So we keep using reflection, so that
    // org.jline.nativ.JLineNativeLoader.initialize() doesn't try to do anything.
    System.setProperty("library.jline.path", loader.millJLineNativeDir.toString)
    System.setProperty(
      "library.jline.name",
      loader.millJLineNativeLibLocation.getFileName.toString
    )

    System.load(loader.millJLineNativeLibLocation.toString)

    val cls = classOf[org.jline.nativ.JLineNativeLoader]
    val fld =
      try {
        cls.getDeclaredField("loaded")
      } catch {
        case ex: NoSuchFieldException =>
          throw new RuntimeException(ex)
      }
    fld.setAccessible(true)
    try {
      fld.setBoolean(null, true)
    } catch {
      case ex: IllegalAccessException =>
        throw new RuntimeException(ex)
    }
  }
}
