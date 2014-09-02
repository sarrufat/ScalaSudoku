import org.junit.runner.RunWith
import org.sarrufat.sudoku.Board
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

trait Output {
  def print(s: String) = Console.println(s)
}

@RunWith(classOf[JUnitRunner])
class TestLS extends FlatSpec with Output {
  val board = Board.mainBoard("Sudoku3.txt")

  "Mainboard" should " be solved" in {

    assert(board.solveBoard(Board.BADTRACKING) != None)

  }
}