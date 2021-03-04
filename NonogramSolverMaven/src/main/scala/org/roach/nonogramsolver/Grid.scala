package org.roach.nonogramsolver
import collection.LinearSeq
/**
 * The Grid is a list of rows.
 */
case class Grid private (val contents: List[Row]) extends LinearSeq[Row] {
  /**
   * Get the row at the specified index
   * @param index The row number to get
   */
  def apply(idx: Int) = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException
    contents(idx)
  }
  /**
   * Get the column at the specified index
   * @param index The column number to get
   */
  def getCol(index: Int): Row = Row((for(row <- contents) yield row(index)): _*)

  /**
   * Converts the Grid to a string for printing
   */
  override def toString = contents.mkString("\n")

  /**
   * Retrieves the length of the Grid.
   */
  val length = contents.length

  /**
   * Returns a new Grid with the specified row replacing the row at the specified index.
   * @param index The position to place the new row
   * @param insertRow The row to patch in.
   */
  def updated(index: Int, insertRow: Row) = Grid(contents.updated(index, insertRow))

  /**
   * Returns a new Grid with rows and columns reversed.
   */
  lazy val inverse: Grid = Grid(
    (for (c <- 0 until contents(0).length) yield getCol(c)).toList
  )

  /**
   * Returns a heuristic number indicating approximately how different this grid
   * is from the given list of column Requirements
   * @param colReqs The list of column Requirements
   * @return The sum of the absolute values of diffs for each column
   */
  def columnDiff(colReqs: List[Requirement]) = (for ((col, req) <- this.inverse.contents zip colReqs) yield col.diff(req)).sum

  /**
   * Returns a new grid merged with this one to be used as a filter.
   * Works as such:
   * O + O = O
   * X + X = X
   * O + U = U + O = O
   * X + U = U + X = X
   * U + U = U
   * O + X = X + O = E (throw new ImpossibleException)
   * E + ? = E
   */
  def & (that: Grid): Grid = new Grid((contents, that.contents).zipped.map((r, t) => r & t))

  /**
   * Applies rules to the grid to make a new filter
   * <ol>
   *  <li>Invert the grid so that the columns are now rows</li>
   *  <li>Apply rules to each row using map</li>
   *  <li>Create a new Grid from the applied rows</li>
   *  <li>Re-invert the grid and return it
   * </ol>
   */
  def applyRules(reqs: List[Requirement]): Grid = {
    val rowList = for ((row, req) <- inverse.contents zip reqs) yield Row.applyRules(row, req)
    Grid(rowList).inverse
  }
  
  override def equals(obj: Any) = obj match {
    case otherGrid: Grid => contents.equals(otherGrid.contents)
    case _ => false
  }
  
  val hasErrors = {
    var b = false
    for (i <- 0 to size - 1) {
      b |= contents(i).isErrorRow
    }
    b
  }
}

object Grid {
  /**
   * Creates an empty grid with all characters unknown
   * @param size The width and length of the new Grid
   */
  def emptyGrid(hsize:Int, vsize:Int) = new Grid(List.fill(vsize)(Row.emptyRow(hsize)))
  
  def fromSeq(buf: Seq[Row]): Grid = {
    new Grid(buf.toList)
  }

  
  def apply(rows: Row*) = fromSeq(rows)
}
