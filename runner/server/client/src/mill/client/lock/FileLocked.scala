package mill.client.lock

class FileLocked(lock: java.nio.channels.FileLock) extends Locked {

  @throws[Exception]
  def release(): Unit = {
    this.lock.release();
  }
}
