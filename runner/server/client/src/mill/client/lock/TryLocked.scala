package mill.client.lock

trait TryLocked extends Locked {
  def isLocked(): Boolean
}
