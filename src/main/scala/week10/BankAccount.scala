package week10

class BankAccount extends Publisher {
    private var balance = 0
    def currentBalance = balance

  def deposit(amount: Int ): Unit = {
    if (amount > 0) {
      balance = balance +amount
      publish()
    }
  }
  def withdraw(amount: Int ): Unit = {
    if (amount < 0 && amount <=balance) {
      balance = balance - amount
      publish()
    } else throw new Error("Insuficientes funds")
  }

}

object observers {
  def main(args: Array[String]): Unit = {
    val bank1 = new BankAccount
    val bank2 = new BankAccount
    val bank3 = new BankAccount

    val cons = new Consolidator(List(bank1,bank2,bank3))

    println(cons.totalBalance)
    bank1 deposit 40
    bank2 deposit 20
    println(cons.totalBalance)
  }
}
