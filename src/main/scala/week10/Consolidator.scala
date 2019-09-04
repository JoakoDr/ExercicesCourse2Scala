package week10

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  //Recorro la lista de bankAccount y subscribo
    observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute() =
    total = observed.map(_.currentBalance).sum

   def handler(publisher: Publisher): Unit = compute

  def totalBalance = total

}
