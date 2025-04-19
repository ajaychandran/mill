package mill.client.lock

class MemoryTryLocked(lock: java.util.concurrent.locks.Lock) extends MemoryLocked(lock), TryLocked {

  def isLocked(): Boolean = {
    lock != null;
  }

  @throws[Exception]
  override def release(): Unit = {
    if (isLocked()) super.release();
  }
}
