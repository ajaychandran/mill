package mill.client.lock

class MemoryLocked(lock: java.util.concurrent.locks.Lock) extends Locked {

  @throws[Exception]
  def release(): Unit = {
    lock.unlock()
  }
}
