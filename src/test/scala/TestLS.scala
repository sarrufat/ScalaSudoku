import org.junit.runner.RunWith
import org.sarrufat.sudoku.Board
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import com.sun.xml.internal.bind.v2.schemagen.xmlschema.List

trait Output {
  def print(s: String) = Console.println(s)
}

@RunWith(classOf[JUnitRunner])
class TestLS extends FlatSpec with Output {
  val board1 = Board.mainBoard("Sudoku1.txt")
  val board2 = Board.mainBoard("Sudoku2.txt")
  val board3 = Board.mainBoard("Sudoku3.txt")

  "Regions basic tests" should " past " in {

    assert(board1.charAt(7, 8) == '1')
    assert(board1.charAt(8, 7) == '7')
    assert(board1.vRegion(2).contains('1') == false)
    assert(board1.hRegion(0).contains('1') == false)
    assert(board1.zRegion(2, 0).contains('1') == false)
    print(board2 toString)
    board2.heuristicSolver
    print(board2 toString)
    print(board2.findNakedSingles.toString)
  }

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