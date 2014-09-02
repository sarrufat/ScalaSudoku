import org.junit.runner.RunWith
import org.sarrufat.sudoku.Board
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

trait Output {
  def print(s: String) = Console.println(s)
}

@RunWith(classOf[JUnitRunner])
class TestLS extends FlatSpec with Output {
  val board1 = Board.mainBoard("Sudoku1.txt")
  val board2 = Board.mainBoard("Sudoku2.txt")
  val board3 = Board.mainBoard("Sudoku3.txt")

  "Board1" should " be solved" in {

    assert(board1.solveBoard(Board.BADTRACKING) != None)

  }
  "Board2" should " be solved" in {

    assert(board2.solveBoard(Board.BADTRACKING) != None)

  }
  "Board3" should " be solved" in {

    assert(board3.solveBoard(Board.BADTRACKING) != None)

  }
}