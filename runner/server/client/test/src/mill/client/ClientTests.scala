package mill.client

import mill.constants.ProxyStream
import utest.*

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.util.Random

object ClientTests extends TestSuite with TestSuite.Retries {

  def utestRetryCount = 3

  def tests = Tests {
    test("readWriteInt") {
      val examples = Array(
        0,
        1,
        126,
        127,
        128,
        254,
        255,
        256,
        1024,
        99999,
        1234567,
        Int.MaxValue,
        Int.MaxValue / 2,
        Int.MinValue
      )
      for (example <- examples) {
        val o = new ByteArrayOutputStream()
        ClientUtil.writeInt(o, example)
        val i = new ByteArrayInputStream(o.toByteArray)
        val s = ClientUtil.readInt(i)
        assert(example == s)
        assert(i.available() == 0)
      }
    }

    test("readWriteString") {
      val examples = Array(
        "",
        "hello",
        "i am cow",
        "i am cow\nhear me moo\ni weight twice as much as you",
        "我是一个叉烧包"
      )
      for (example <- examples) {
        checkStringRoundTrip(example)
      }
    }

    test("readWriteBigString") {
      val lengths = Array(0, 1, 126, 127, 128, 254, 255, 256, 1024, 99999, 1234567)
      for (length <- lengths) {
        val bigChars = Array.fill(length)('X')
        checkStringRoundTrip(new String(bigChars))
      }
    }

    test("tinyProxyInputOutputStream") {
      proxyInputOutputStreams(Array.copyOf(readSamples("/bandung.jpg"), 30), readSamples(), 10)
    }

    test("leftProxyInputOutputStream") {
      proxyInputOutputStreams(
        readSamples("/bandung.jpg", "/akanon.mid", "/gettysburg.txt", "/pip.tar.gz"),
        readSamples(),
        2950
      )
    }

    test("rightProxyInputOutputStream") {
      proxyInputOutputStreams(
        readSamples(),
        readSamples("/bandung.jpg", "/akanon.mid", "/gettysburg.txt", "/pip.tar.gz"),
        3000
      )
    }

    test("mixedProxyInputOutputStream") {
      proxyInputOutputStreams(
        readSamples("/bandung.jpg", "/gettysburg.txt"),
        readSamples("/akanon.mid", "/pip.tar.gz"),
        3050
      )
    }
  }

  @throws[Exception]
  def checkStringRoundTrip(example: String): Unit = {
    val o = new ByteArrayOutputStream()
    ClientUtil.writeString(o, example)
    val i = new ByteArrayInputStream(o.toByteArray)
    val s = ClientUtil.readString(i)
    assert(example == s)
    assert(i.available() == 0)
  }

  @throws[Exception]
  def readSamples(samples: String*): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    for (sample <- samples) {
      val bytes = java.nio.file.Files.readAllBytes(
        java.nio.file.Paths.get(getClass.getResource(sample).toURI)
      )
      out.write(bytes)
    }
    out.toByteArray
  }

  /**
   * Make sure that when we shove data through both ProxyOutputStreams in
   * variously sized chunks, we get the exact same bytes back out from the
   * ProxyStreamPumper.
   */
  @throws[Exception]
  def proxyInputOutputStreams(samples1: Array[Byte], samples2: Array[Byte], chunkMax: Int): Unit = {

    val pipe = new ByteArrayOutputStream()
    val src1 = new ProxyStream.Output(pipe, ProxyStream.OUT)
    val src2 = new ProxyStream.Output(pipe, ProxyStream.ERR)

    val random = new Random(31337)

    var i1 = 0
    var i2 = 0
    while (i1 < samples1.length || i2 < samples2.length) {
      val chunk = random.nextInt(chunkMax)
      if (random.nextBoolean() && i1 < samples1.length) {
        src1.write(samples1, i1, Math.min(samples1.length - i1, chunk))
        src1.flush()
        i1 += chunk
      } else if (i2 < samples2.length) {
        src2.write(samples2, i2, Math.min(samples2.length - i2, chunk))
        src2.flush()
        i2 += chunk
      }
    }

    val bytes = pipe.toByteArray

    val dest1 = new ByteArrayOutputStream()
    val dest2 = new ByteArrayOutputStream()
    val pumper = new ProxyStream.Pumper(new ByteArrayInputStream(bytes), dest1, dest2)
    pumper.run()
    assert(samples1.sameElements(dest1.toByteArray))
    assert(samples2.sameElements(dest2.toByteArray))
  }
}
