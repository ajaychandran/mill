package mill.client.lock

import mill.constants.ServerFiles

final class Locks(val clientLock: Lock, val processLock: Lock) extends AutoCloseable {

  @throws[Exception]
  def close(): Unit = {
    clientLock.delete()
    processLock.delete()
  }
}

object Locks {

  @throws[Exception]
  def files(serverDir: String): Locks = {
    new Locks(
      FileLock(serverDir + "/" + ServerFiles.clientLock),
      FileLock(serverDir + "/" + ServerFiles.processLock)
    )
  }

  def memory(): Locks = {
    new Locks(new MemoryLock(), new MemoryLock())
  }
}
