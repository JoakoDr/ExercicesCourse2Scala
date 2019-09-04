package week10

import scala.util.DynamicVariable

class Signal[T](expr: => T) {
  import Signal._
  private var myExpr: () => T = _
  private var myVal: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)


  protected def update(expr: T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if(myVal != newValue) {
      myVal = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply() = {
    observers += caller.value
    assert(!caller.value.observers.contains(this),"cyclic signal definition")
    myVal
  }
}


object noSignal extends Signal[Nothing] (???){
  override def computeValue() = ()
}
object Signal {
  private val caller = new DynamicVariable[Signal[_]](noSignal)
  def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  def update(expr: => T):Unit = super.update(expr)
}


//How to access: val caller = new StackableVariable(initialSig) caller.withValue(otherSig)
class StackableVariable[T](init: T){
  private var values: List[T] = List(init)
  def value: T = values.head
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

class BankAccountSignals {
   val balance = Var(0)

  def deposit(amount: Int ): Unit = {
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }
  def withdraw(amount: Int ): Unit = {
    if (amount < 0 && amount <=balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("Insuficientes funds")
  }

}
