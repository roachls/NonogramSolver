package org.roach.nonogramsolver

import scala.io.Source
import org.apache.logging.log4j.Logger
import org.apache.logging.log4j.LogManager

object NonogramSolver {
  val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {
    val (hsize, vsize, cols, rows) = {
      val lines = Source.fromFile(args(0)).getLines().filter(l => !l.startsWith("#") && l.length != 0).toList
      val hsize = lines(0).toInt
      val vsize = lines(1).toInt
      val cols = lines(2).replace("\",\"", ":").replace("\"","").split(":").map(c => Requirement(c.split(",").map(l => l.toInt).toList)).toList
      val rows = lines(3).replace("\",\"", ":").replace("\"","").split(":").map(r => Requirement(r.split(",").map(l => l.toInt).toList)).toList
      logger.debug(s"hsize=$hsize")
      logger.debug(s"vsize=$vsize")
      logger.debug(s"cols=$cols")
      logger.debug(s"rows=$rows")
      (hsize, vsize, cols, rows)
    }
//    val ui = new NonogramUI(args(0), hsize, vsize, rows, cols)
//    ui.setVisible(true)

    val filterGrid = Grid(for (col <- cols) yield col.generateFilter(vsize)).inverse

    // Generate list of all possible potential rows
    logger.debug("Generating potential rows...")
    val potentialRowList = for ((row, filter) <- rows zip filterGrid.contents) yield row.generateRows(hsize).filter(r => (r compatibleWith filter) && !r.isErrorRow)
    logger.debug("Generated " + potentialRowList.length + " rows")

    // Generate list of all possible potential columns
    logger.debug("Generating potential columns...")
    val potentialColList = for (col <- cols) yield col.generateRows(vsize).filter(c => !c.isErrorRow)
    logger.debug("Generated " + potentialColList.length + " columns")

    val orderToTryRows: List[Int] = (0 to vsize - 1).toList

    /*
     * The main engine that solves the given puzzle
     * Substitutes one row into the startGrid and then recursively calls itself
     * with the new startGrid and tries the next row.
     * @param startGrid The solution so far, which starts as all empty cells
     * @param rowList A list of integer lists for each row
     * @param cd The column difference so far.
     * @param rowsToTryNext A list of rows to try for this iteration
     * @param filter The current filter grid
     * @return Some(Grid) which is the solution of the puzzle, or None
     */
    var done = false;
    def solve(
      startGrid: Grid,
      rowList: List[Requirement],
      cd: Int,
      rowsToTryNext: List[Int],
      filter: Grid): Option[Grid] = {
      if (filter.hasErrors) return None
      if (rowsToTryNext != Nil) {
        logger.trace("In solve with " + rowsToTryNext.head)
      }
      rowList match {
        case Nil => Some(startGrid) // If we got this far, then we've solved the puzzle
        case row :: tail =>
          val whichRow = rowsToTryNext.head
          val rowsToTry = potentialRowList(whichRow).filter(r => r compatibleWith filter(whichRow))
          if (rowsToTry == Nil) {
            logger.debug("Stopping because none of " + potentialRowList(whichRow) + " matched " + filter(whichRow) + " for row " + whichRow)
          }
          rowsToTry.foreach { r =>
            // Builds a new grid with r substituted for row #whichRow
            val newGrid = startGrid.updated(whichRow, r)
//            ui.updateGame(newGrid)
            val newColumnDiff = newGrid.columnDiff(cols)
            if (cd - newColumnDiff == r.countOs) { // Only try rows where every O counts
              val tGrid = newGrid.inverse
              val allColumnsPossible = (potentialColList, tGrid.contents).zipped.forall(
                (p, filter) => p exists (_ compatibleWith filter))
              val newFilter = (newGrid & filter).applyRules(cols)
              // Only try rows where all columns are possible
              if (allColumnsPossible) {
                val solveTail = solve(newGrid, tail, newColumnDiff, rowsToTryNext.tail, newFilter)
                if (solveTail != None) {
                  return solveTail
                }
              }
            }
          }
          // Only reach here if all tries are exhausted and didn't work
          logger.trace("\nFinal filter:\n" + filter)
          None
      }
    }

     val t1 = System.currentTimeMillis()
    solve(Grid.emptyGrid(hsize, vsize), rows, Grid.emptyGrid(hsize, vsize).columnDiff(cols), orderToTryRows, filterGrid) match {
      case Some(solution) =>
         println("\nSolved!")
         println(s"Rows: $rows")
         println(s"Cols: $cols")
        println(solution)
//        ui.updateGame(solution)
      case None =>
        println("\n\nRows: " + rows)
        println("Cols: " + cols)
        println("After an exhaustive search I couldn't solve it. :-(")
    }
    lazy val t2 = System.currentTimeMillis()
    val time = (t2 - t1).asInstanceOf[Float]
    if (time <= 1000.0f) println("\n" + time + " msecs")
    else if (time <= 1000000.0f) println("\n" + time / 1000.0f + " secs")
    else println("\n" + time / 60000.0f + " min")
  }
}
