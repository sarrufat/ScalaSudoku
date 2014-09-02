package test

import org.sarrufat.sudoku.latinsqr.LatinSquare

object TestWS {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(123); 
  println("Welcome to the Scala worksheet");$skip(30); 
  val ls = new LatinSquare(9);System.out.println("""ls  : org.sarrufat.sudoku.latinsqr.LatinSquare = """ + $show(ls ))}
  
}
