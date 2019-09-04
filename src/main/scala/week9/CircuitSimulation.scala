package week9

abstract class CircuitSimulation
  extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
    val d, e = new Wire
    a.orGate(a, b, d)
    a.andGate(a, b, c)
    c.inverter(c, e)
    d.andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire,
                sum: Wire, cout: Wire) {

    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    c1.orGate(c1, c2, cout)
  }
}

