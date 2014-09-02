package org.sarrufat.sudoku.latinsqr

class LatinSquare( size: Integer) {
  
  override def toString() = {
    normalized.toString
  }
  def normalized = {
    def normalizedRow(idx: Integer) = for { i <- 0 until size } yield { (i + idx) % size }
    for { r <- 0 until size } yield normalizedRow(r)
  }
}

object LatinSquare {
  val sudokuLatinSqr = new LatinSquare(9)
}