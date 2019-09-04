package week8

object randtest {

  val integers = new Generator[Int] {
     def generate: Int = scala.util.Random.nextInt()
  }

  def single[T](x: T) = new Generator[T] {
     def generate: T = x
  }

  val booleans = integers.map(_ >= 0)

  def pairs[T,U](t: Generator[T],u:Generator[U]):Generator[(T,U)] = for {
    x <- t
    y <- u
  } yield (x,y)

  def emptyLists = single(Nil)

  def noEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  def lists: Generator[List[Int]] = for {
    cutoff <- booleans
    list <- if (cutoff) emptyLists else noEmptyLists
  } yield list

  def test[T](r: Generator[T], noTimes:Int = 100)(test:T => Boolean ): Unit ={
    for(_ <- 0 until noTimes){
      val value = r.generate
      assert(test(value),"Test failed for:" +value)
      println(new Error("No ha pasado el test"))
    }
    println("Test Passed "+noTimes+ " times")
  }

  def main(args: Array[String]): Unit = {
    test(pairs(lists,lists)){
      case (xs,ys) => (xs ++ ys).length > xs.length
    }
  }
}
