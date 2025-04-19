package mill.client.lock

import java.util.concurrent.locks.ReentrantLock

class MemoryLock extends Lock {

  private val innerLock = new ReentrantLock()

  def probe(): Boolean = {
    !innerLock.isLocked
  }

  def lock(): Locked = {
    innerLock.lock()
    new MemoryLocked(innerLock)
  }

  def tryLock(): MemoryTryLocked = {
    if (innerLock.tryLock()) new MemoryTryLocked(innerLock)
    else new MemoryTryLocked(null)
  }

  @throws[Exception]
  def close(): Unit = {
    innerLock.unlock()
  }
}
