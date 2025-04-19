package mill.client.lock

import java.io.RandomAccessFile
import java.nio.channels.{FileChannel, OverlappingFileLockException}

class FileLock private (raf: RandomAccessFile, chan: FileChannel) extends Lock {

  @throws[Exception]
  def lock(): Locked = {
    new FileLocked(chan.lock())
  }

  @throws[Exception]
  def tryLock(): TryLocked = {
    var lock: java.nio.channels.FileLock = null
    try {
      lock = chan.tryLock()
    } catch {
      case _: OverlappingFileLockException =>
      // file already locked by this JVM
    }
    new FileTryLocked(lock)
  }

  @throws[Exception]
  def probe(): Boolean = {
    val l = chan.tryLock()
    if (l == null) false
    else {
      l.release()
      true
    }
  }

  @throws[Exception]
  def close(): Unit = {
    chan.close()
    raf.close()
  }

  @throws[Exception]
  override def delete(): Unit = {
    close()
  }
}

object FileLock {

  @throws[Exception]
  def apply(path: String): FileLock = {
    val raf = new RandomAccessFile(path, "rw")
    new FileLock(raf, raf.getChannel)
  }
}
