package org.sarrufat.sudoku

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.io.Source

class Board() {

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
      val res = for { idx <- 0 until 9 } yield {
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
    for (idx <- 0 until 9) asignRow(filenes(idx), rows(idx))
    def asignRow(line: String, row: RowT) = {
      for { idx <- 0 until 9 } { row(idx) = line(idx) }
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
      ix <- 0 until 3
      iy <- 0 until 3
    } yield { rows(ix + zx * 3)(iy + zy * 3) }
    !cells.contains(value)
  }
  def attempValue(x: Int, y: Int, value: Char): Boolean = {
    rowConstraint(x, value) && colConstraint(y, value) && zoneConstraint(x, y, value)
  }

  private def copy(): Board = {
    val retB = new Board
    val copia = rows.map(_.clone())
    copia.copyToArray(retB.rows)
    retB
  }

  def isSolved(): Boolean = {
    def isSolved(row: RowT): Boolean = !row.contains('.')
    rows.forall(isSolved(_))
  }
  private def solveBoard(x: Int, y: Int): Board = {
    def nextXY(x: Int, y: Int): (Int, Int) = {
      var cxy = x * 9 + y + 1
      if (cxy < 81) (cxy / 9, cxy % 9) else (-1, -1)
    }
    if (x >= 0 && y >= 0) {
      val (nx, ny) = nextXY(x, y)
      var cb = copy
      rows(x)(y) match {
        case '.' => {
          def recAttem(ch: Char): Unit = {
            if (ch <= '9') {
              if (cb.attempValue(x, y, ch)) {
                cb.rows(x)(y) = ch
                val solvedB = cb.solveBoard(nx, ny)
                if (solvedB.isSolved) cb = solvedB else recAttem((ch + 1).toChar)
              } else recAttem((ch + 1).toChar)
            }
          }
          recAttem('1')
        }
        case _ => cb = solveBoard(nx, ny)
      }
      cb
    } else this

  }
  def solveBoard(): Board = {
    solveBoard(0, 0)
  }
}

object Board {
  def mainBoard(fileName: String) = {
    val retB = new Board
    retB.readFile(fileName)
    retB
  }

}