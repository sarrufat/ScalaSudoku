import org.junit.runner.RunWith
import org.sarrufat.sudoku.Board
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

trait Output {
  def print(s: String) = Console.println(s)
}

@RunWith(classOf[JUnitRunner])
class TestLS extends FlatSpec with Output {
  val board = Board.mainBoard("Sudoku2.txt")

  "Mainboard" should "look" in {
    print(board toString ())
    print("...........\n")
    print(board.solveBoard toString ())
  }
  "Mainboard" should " be solved" in {
    
    assert(board.solveBoard.isSolved == true)
    
  }
}