package mill.client

import java.io.{FileNotFoundException, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Scanner
import java.util.regex.Pattern
import scala.util.Using

object ClientUtil {
  // use methods instead of constants to avoid inlining by compiler
  def ExitClientCodeCannotReadFromExitCodeFile(): Int = {
    1
  }

  def ExitServerCodeWhenIdle(): Int = {
    0
  }

  def ExitServerCodeWhenVersionMismatch(): Int = {
    101
  }

  @throws[IOException]
  def parseArgs(argStream: InputStream): Array[String] = {
    val argsLength = readInt(argStream)
    Array.fill(argsLength)(readString(argStream))
  }

  @throws[IOException]
  def writeArgs(args: Array[String], argStream: OutputStream): Unit = {
    writeInt(argStream, args.length)
    for (arg <- args) {
      writeString(argStream, arg)
    }
  }

  /**
   * This allows the mill client to pass the environment as it sees it to the
   * server (as the server remains alive over the course of several runs and
   * does not see the environment changes the client would)
   */
  @throws[IOException]
  def writeMap(map: Map[String, String], argStream: OutputStream): Unit = {
    writeInt(argStream, map.size)
    for (kv <- map) {
      writeString(argStream, kv._1)
      writeString(argStream, kv._2)
    }
  }

  @throws[IOException]
  def parseMap(argStream: InputStream): Map[String, String] = {
    val env = Map.newBuilder[String, String]
    var i = readInt(argStream)
    while (i > 0) {
      val key = readString(argStream)
      val value = readString(argStream)
      env.addOne(key -> value)
      i -= 1
    }
    env.result()
  }

  @throws[IOException]
  def readString(inputStream: InputStream): String = {
    // Result is between 0 and 255, hence the loop.
    val length = readInt(inputStream)
    val arr = Array.ofDim[Byte](length)
    var total = 0
    while (total < length) {
      val res = inputStream.read(arr, total, length - total)
      if (res == -1) throw new IOException("Incomplete String")
      else {
        total += res
      }
    }
    new String(arr, StandardCharsets.UTF_8)
  }

  @throws[IOException]
  def writeString(outputStream: OutputStream, string: String): Unit = {
    val bytes = string.getBytes(StandardCharsets.UTF_8)
    writeInt(outputStream, bytes.length)
    outputStream.write(bytes)
  }

  @throws[IOException]
  def writeInt(out: OutputStream, i: Int): Unit = {
    out.write((i >>> 24).toByte)
    out.write((i >>> 16).toByte)
    out.write((i >>> 8).toByte)
    out.write(i.toByte)
  }

  @throws[IOException]
  def readInt(in: InputStream): Int = {
    ((in.read() & 0xff) << 24)
      + ((in.read() & 0xff) << 16)
      + ((in.read() & 0xff) << 8)
      + (in.read() & 0xff)
  }

  /**
   * Reads a file, ignoring empty or comment lines, interpolating env variables.
   *
   * @return The non-empty lines of the files or an empty list, if the file does not exist
   */
  @throws[IOException]
  def readOptsFileLines(file: Path): Seq[String] = {
    val vmOptions = Seq.newBuilder[String]
    Using(new Scanner(file.toFile, StandardCharsets.UTF_8)) { sc =>
      val env = sys.env
      while (sc.hasNextLine) {
        val arg = sc.nextLine()
        val trimmed = arg.trim()
        if (trimmed.nonEmpty && !trimmed.startsWith("#")) {
          vmOptions.addOne(interpolateEnvVars(arg, env))
        }
      }
    }.recover {
      case _: FileNotFoundException =>
      // ignored
    }.get
    vmOptions.result()
  }

  /**
   * Interpolate variables in the form of `${VARIABLE}` based on the given `env`.
   * Missing vars will be replaced by the empty string.
   */
  @throws[IOException]
  private def interpolateEnvVars(input: String, env: Map[String, String]): String = {
    val matcher = envInterpolatorPattern.matcher(input)
    // StringBuilder to store the result after replacing
    val result = new StringBuffer()

    while (matcher.find()) {
      val matched = matcher.group(1)
      if (matched.equals("$")) {
        matcher.appendReplacement(result, "\\$")
      } else {
        val envVarValue =
          // Hardcode support for PWD because the graal native launcher has it set to the
          // working dir of the enclosing process, when we want it to be set to the working
          // dir of the current process
          if (matched.equals("PWD"))
            new java.io.File(".").getAbsoluteFile.getCanonicalPath
          else env.getOrElse(matched, "")
        matcher.appendReplacement(result, envVarValue)
      }
    }

    matcher.appendTail(result) // Append the remaining part of the string
    result.toString
  }

  private val envInterpolatorPattern =
    Pattern.compile("\\$\\{(\\$|[A-Z_][A-Z0-9_]*)\\}")
}
