package org.sarrufat.sudoku

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Map
import scala.io.Source

class Board() {

  implicit def intToDigitChar(x: Int): Char = ('0'.toInt + x).toChar

  type RowT = Array[Char]
  // Inicialzamos el tablero a ' 's donde ' ' = no resuelto
  val rows: Array[RowT] = Array.fill(9, 9) { '.' }
  // Vertical region
  def vRegion(x: Int): Array[Char] = for { row ← rows } yield { row(x) }
  // Horizontal region
  def hRegion(y: Int): Array[Char] = rows(y)
  // Zone region 
  def zRegion(x: Int, y: Int): Array[Char] = {
    val zx = x / 3
    val zy = y / 3
    val cells = for {
      ix ← Board.BOX_RANGE
      iy ← Board.BOX_RANGE
    } yield { rows(iy + zy * 3)(ix + zx * 3) }
    cells.toArray
  }

  def charAt(x: Int, y: Int) = rows(y)(x)

  override def toString() = {
    def rowsToString = {
      val hline = "---+---+---\n"
      val arrS = for { row <- rows } yield {
        rowToString(row)
      }
      ((arrS.slice(0, 3) :+ hline) ++ (arrS.slice(3, 6) :+ hline) ++ arrS.slice(6, 9)).mkString
    }
    def rowToString(row: RowT) = {
      val res = for { idx <- Board.RANGE } yield {
        row(idx) + (if (idx == 2 || idx == 5) "|" else "")
      }
      res.mkString + "\n"
    }
    rowsToString
  }
  private def readFile(fileName: String) {
    val filenes = (for { line <- Source.fromFile(fileName).getLines if !line.startsWith("---") } yield line.replace("|", "")).toIndexedSeq
    assert(filenes.length == 9)
    for (idx <- Board.RANGE) asignRow(filenes(idx), rows(idx))
    def asignRow(line: String, row: RowT) = {
      for { idx <- Board.RANGE } { row(idx) = line(idx) }
    }
  }
  private def rowConstraint(x: Int, value: Char): Boolean = !vRegion(x).contains(value)
  private def colConstraint(y: Int, value: Char): Boolean = !hRegion(y).contains(value)
  private def zoneConstraint(x: Int, y: Int, value: Char): Boolean = !zRegion(x, y).contains(value);
  private def attempValue(x: Int, y: Int, value: Char): Boolean = {
    rowConstraint(x, value) && colConstraint(y, value) && zoneConstraint(x, y, value)
  }

  // next Blanck cell
  private def nextBlank(x: Int, y: Int): (Int, Int) = {
    def nextXY(x: Int, y: Int): (Int, Int) = {
      var cxy = x + y * 9 + 1
      if (cxy < 81) (cxy % 9, cxy / 9) else (-1, -1)
    }
    val retXY = nextXY(x, y)
    if (retXY._1 >= 0 && retXY._2 >= 0 && charAt(retXY._1, retXY._2) != '.') nextBlank(retXY._1, retXY._2)
    else retXY
  }

  private def copy(): Board = {
    val retB = new Board
    val copia = rows.map(_.clone())
    copia.copyToArray(retB.rows)
    retB
  }

  def isSolved(): Boolean = {
    rows.forall(!_.contains('.'))
  }

  private def internalSolveBoard(x: Int, y: Int): Board = {

    if (x >= 0 && y >= 0) {
      var cb = copy
      val (nx, ny) = nextBlank(x, y)
      charAt(x, y) match {
        // Blank cell
        case '.' => {
          // Recursive Attemp
          def recAttem(ch: Int): Unit = {
            if (ch <= 9) {
              if (cb.attempValue(x, y, ch)) {
                cb.rows(y)(x) = ch
                // Solve next 
                val solvedB = cb.internalSolveBoard(nx, ny)
                if (solvedB.isSolved) cb = solvedB else recAttem(ch + 1)
              } else recAttem(ch + 1)
            }
          }
          recAttem(1)
        }
        // Clue => solve next
        case _ => cb = internalSolveBoard(nx, ny)
      }
      cb
    } else this

  }
  /* 
  * Heuristics algorithms
  */
  def getCandidatesAt(x: Int, y: Int) = {
    val allSet = { for { ch <- ('1' to '9') } yield { ch } }.toSet
    val imposibleSet = (vRegion(x) ++ hRegion(y) ++ zRegion(x, y)).toSet
    allSet diff imposibleSet
  }
  // Find clues
  def getCluesCoord() = {
    for {
      x <- Board.RANGE
      y <- Board.RANGE
      if (charAt(x, y) == '.')
    } yield {
      (x, y)
    }
  }
  // Fin naked singles
  def findNakedSingles() = {
    for {
      co <- getCluesCoord; cantidates = (getCandidatesAt(co._1, co._2))
      if (cantidates.size == 1)
    } yield {
      (co, cantidates.head)
    }
  }
  // Find hidden singles
  def findHiddenSingles() = {
    def findUniquesInMap(map: Map[Int, IndexedSeq[((Int, Int), Set[Char])]]) = {
      val result = for {
        entry <- map
      } yield {
        val allCandiChars = (for { es <- entry._2 } yield { es._2 }).flatten.groupBy(x => x)
        (for { charOcur <- allCandiChars if (charOcur._2.size == 1) } yield {
          (entry._2.find(tpl => tpl._2.find(ch => ch == charOcur._1) != None).get._1, charOcur._1)
        }).toList
      }
      result.toList.flatten
    }
    val allCanditates = getAllCandidates
    val xCandidates = allCanditates.groupBy(_._1._1)
    val yCandidates = allCanditates.groupBy(_._1._2)
    val zCandidates = allCanditates.groupBy(z => Board.zRegionNum(z._1._1, z._1._2))
    val ret = (findUniquesInMap(xCandidates) ::: findUniquesInMap(yCandidates) ::: findUniquesInMap(zCandidates)).toIndexedSeq
    println(this + "\nfindHiddenSingles = " + findUniquesInMap(xCandidates) + "\n")
    ret
  }
  private def getAllCandidates() = {
    for {
      co <- getCluesCoord; cantidates = (getCandidatesAt(co._1, co._2))
      if (cantidates.nonEmpty)
    } yield {
      (co, cantidates)
    }
  }
  // Find unique posible: If eight of the nine elements in any row, column or block
  def uniqueMissing() = {
    val allChars = Board.allCharSet
    def uniqueMiss(f: (Int) => Array[Char]) = {
      (for {
        co <- getCluesCoord
        if (f(co._1).count(_ == '.') == 1)
      } yield {
        (co, allChars.diff(f(co._1).toSet).head)
      }).toList
    }
    val zUniqs = (for {
      co <- getCluesCoord
      if (zRegion(co._1, co._2).count(_ == '.') == 1)
    } yield {
      (co, allChars.diff(zRegion(co._1, co._2).toSet).head)
    }).toList
    (uniqueMiss(vRegion) ::: uniqueMiss(hRegion) ::: zUniqs).toIndexedSeq
  }
  def heuristicSolver(): Board = {
    def genericProcess(finder: () => IndexedSeq[((Int, Int), Char)]) = {
      val uniques = finder()
      for (un <- uniques) { rows(un._1._2)(un._1._1) = un._2 }
      uniques
    }
    var end = false
    while (!end) {
      end = true
      while (!genericProcess(findNakedSingles).isEmpty) { end = false }
      while (!genericProcess(findHiddenSingles).isEmpty) { end = false }
      println("\nuniqueMissing = " + uniqueMissing)
      while (!genericProcess(uniqueMissing).isEmpty) { end = false }
    }
    this
  }
  def solveBoard(mode: Symbol): Option[Board] = {
    mode match {
      case 'BadTracking => {
        val solb = internalSolveBoard(0, 0)
        if (solb.isSolved) Some(solb) else None
      }
      case 'Heuristic => {
        val solb = heuristicSolver
        if (solb.isSolved) Some(solb) else None
      }
      case _ => None
    }
  }
}

object Board {
  private val RANGE = 0 until 9
  private val BOX_RANGE = 0 until 3
  private def allCharSet = { for { ch <- ('1' to '9') } yield { ch } }.toSet
  val BADTRACKING = 'BadTracking
  def mainBoard(fileName: String) = {
    val retB = new Board
    retB.readFile(fileName)
    retB
  }
  def zRegionNum(x: Int, y: Int) = x / 3 + y / 3 * 3
}