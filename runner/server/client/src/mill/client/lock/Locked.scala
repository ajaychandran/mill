package mill.client.lock

trait Locked extends AutoCloseable {

  @throws[Exception]
  def release(): Unit

  @throws[Exception]
  def close(): Unit = {
    release()
  }
}
