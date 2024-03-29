package week8

class Pouring(capacity: Vector[Int]) {

  //States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  //Moves
  trait Moves {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Moves {
    def change(state: State) = state updated(glass, 0)
  }

  case class Pour(from: Int, to: Int) extends Moves {
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  case class Fill(glass: Int) extends Moves {
    def change(state: State) =
      state updated(glass, capacity(glass))
  }

  val glasses = 1 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses) yield Pour(from, to))

  //Paths
  class Path(history: List[Moves], val endState: State) {
    /* def endState:State = (history foldRight initialState)(_ change _)*/
    def extend(moves: Moves) = new Path(moves :: history, moves change endState)

    override def toString: String = (history.reverse mkString " ") + "-->" + endState

    /* private def trackState(xs: List[Moves]):State = xs match {
     case Nil => initialState
     case move :: xs1 => move change trackState(xs1)
   }*/
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

object Pouring {
  def main(args: Array[String]): Unit = {
    val problem = new Pouring(Vector(4, 9, 19))
    println(problem.moves)
    println(problem.solution(16))
  }
}

