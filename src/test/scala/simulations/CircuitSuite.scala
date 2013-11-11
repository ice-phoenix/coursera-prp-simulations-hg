package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)

    run
    assert(out.getSignal === false, "0 & 0 = 0")

    in1.setSignal(true)
    run
    assert(out.getSignal === false, "1 & 0 = 0")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "1 & 1 = 1")

    in1.setSignal(false)
    run
    assert(out.getSignal === false, "0 & 1 = 0")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)

    run
    assert(out.getSignal === false, "0 | 0 = 0")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "1 | 0 = 1")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "1 | 1 = 1")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "0 | 1 = 1")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)

    run
    assert(out.getSignal === false, "0 | 0 = 0")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "1 | 0 = 1")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "1 | 1 = 1")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "0 | 1 = 1")
  }

  test("demux example") {
    val in = new Wire
    val c = Range(0, 3).map(_ => new Wire).toList
    val out = Range(0, 8).map(_ => new Wire).toList
    demux(in, c.reverse, out.reverse)

    run
    assert(out.forall(_.getSignal == false),
      "false(0) => false[*]")

    in.setSignal(true)
    run
    assert(out.filter(_.getSignal == true).size == 1 &&
      out(0).getSignal == true,
      "true(0) => true[0]")

    c(2).setSignal(true)
    c(1).setSignal(false)
    c(0).setSignal(true)
    run
    assert(out.filter(_.getSignal == true).size == 1 &&
      out(5).getSignal == true,
      "true(5) => true[5]")

    c(2).setSignal(true)
    c(1).setSignal(false)
    c(0).setSignal(false)
    run
    assert(out.filter(_.getSignal == true).size == 1 &&
      out(4).getSignal == true,
      "true(4) => true[4]")

    in.setSignal(false)
    run
    assert(out.forall(_.getSignal == false),
      "false(4) => false[*]")
  }

}
