package org.sarrufat.sudoku

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.io.Source

class Board() {

  implicit def intToDigitChar(x: Int): Char = ('0'.toInt + x).toChar

  type RowT = Array[Char]
  // Inicialzamos el tablero a ' 's donde ' ' = no resuelto
  val rows: Array[RowT] = Array.fill(9, 9) { '.' }

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
    println("readFile")
    val filenes = (for { line <- Source.fromFile(fileName).getLines if !line.startsWith("---") } yield line.replace("|", "")).toIndexedSeq
    assert(filenes.length == 9)
    for (idx <- Board.RANGE) asignRow(filenes(idx), rows(idx))
    def asignRow(line: String, row: RowT) = {
      for { idx <- Board.RANGE } { row(idx) = line(idx) }
    }
  }
  private def rowConstraint(x: Int, value: Char): Boolean = !rows(x).contains(value)
  private def colConstraint(y: Int, value: Char): Boolean = {
    val colV = for { row <- rows } yield { row(y) }
    !colV.contains(value)
  }
  private def zoneConstraint(x: Int, y: Int, value: Char): Boolean = {
    val zx = x / 3
    val zy = y / 3
    val cells = for {
      ix <- Board.BOX_RANGE
      iy <- Board.BOX_RANGE
    } yield { rows(ix + zx * 3)(iy + zy * 3) }
    !cells.contains(value)
  }
  private def attempValue(x: Int, y: Int, value: Char): Boolean = {
    rowConstraint(x, value) && colConstraint(y, value) && zoneConstraint(x, y, value)
  }

  // next Blanck cell
  private def nextBlank(x: Int, y: Int): (Int, Int) = {
    def nextXY(x: Int, y: Int): (Int, Int) = {
      var cxy = x * 9 + y + 1
      if (cxy < 81) (cxy / 9, cxy % 9) else (-1, -1)
    }
    val retXY = nextXY(x, y)
    if (retXY._1 >= 0 && retXY._2 >= 0 && rows(retXY._1)(retXY._2) != '.') nextBlank(retXY._1, retXY._2)
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
      rows(x)(y) match {
        // Blank cell
        case '.' => {
          // Recursive Attemp
          def recAttem(ch: Int): Unit = {
            if (ch <= 9) {
              if (cb.attempValue(x, y, ch)) {
                cb.rows(x)(y) = ch
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
  def solveBoard(mode: Symbol): Option[Board] = {
    mode match {
      case 'BadTracking => {
        val solb = internalSolveBoard(0, 0)
        if (solb.isSolved) Some(solb) else None
      }
      case _ => None
    }
  }
}

object Board {
  private val RANGE = 0 until 9
  private val BOX_RANGE = 0 until 3
  val BADTRACKING = 'BadTracking
  def mainBoard(fileName: String) = {
    val retB = new Board
    retB.readFile(fileName)
    retB
  }
}