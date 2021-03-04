/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.roach.nonogramsolver
import collection.SeqLike
import collection.mutable.{Builder, ListBuffer}
import collection.generic.CanBuildFrom

class Requirement(private val list: List[Int]) extends Seq[Int] with SeqLike[Int, Requirement] {
  override protected[this] def newBuilder: Builder[Int, Requirement] =
    Requirement.newBuilder
  val sum = list.sum
  val length = list.length
  val gamesum = sum + length - 1
  override val toString = list.mkString(" ")
  def apply(idx: Int): Int = {
    if (idx < 0 || list.length <= idx)
      throw new IndexOutOfBoundsException
    list(idx)
  }

  /**
   * Generates a list of all rows that meet the requirements given.
   * For example, given list = List(1,1) and size = 5, the output will be:
   * List(
   *   Row(List('O', 'x', 'O', 'x', 'x')),
   *   Row(List('O', 'x', 'x', 'O', 'x')),
   *   Row(List('O', 'x', 'x', 'x', 'O')),
   *   Row(List('x', 'O', 'x', 'O', 'x')),
   *   Row(List('x', 'O', 'x', 'x', 'O')),
   *   Row(List('x', 'x', 'O', 'x', 'O')),
   * )
   * @param list The list of requirements
   * @param size The size of the grid
   */
  def generateRows(vsize: Int) : List[Row] = {
    if (vsize < gamesum)
      throw new IllegalArgumentException("vsize " + vsize + " < gamesum " + gamesum) 
    /**
     * Returns the number of spaces each block may be moved within the
     * row/column. For example, given the list List(1, 2) and a grid size of
     * 5x5, each block may be in one of two positions, so this function would
     * return 2. A block of 8 in a grid of 15 would return 8.
     */
    def degreesOfFreedom(blockSize: Int, remainder: List[Int], width: Int) : Int =
      width - (0 /: remainder)(_ + _) - blockSize - remainder.length + 1

    def generateRowsR(_list: List[Int], _size: Int) : List[Row] = {
      if (_size >= 1) {
        _list match {
          case Nil => Nil
          case head::tail => {
            val headlist:List[Row] = for (position <- List.range(0, degreesOfFreedom(head, tail, _size)))
              yield Row((List.fill(position)(X):::List.fill(head)(O):::(if(_size>=head) List(X) else Nil)):_*)
            var returnList: List[Row] = Nil
            for (l:Row <- headlist) {
              val generatedList:List[Row] = generateRowsR(tail, _size - l.size)
              if (generatedList == Nil) {
                returnList :::= headlist;
              } else {
                returnList :::= (for(g <- generatedList) yield (l ++ g))
              }
            }
            returnList
          }
        }
      } else return Nil
    }
    generateRowsR(list, vsize).map(r => r.padTo(vsize, X)).map(r => r.take(vsize)).distinct.asInstanceOf[List[Row]]
  }

  def generateFilter(size: Int) : Row = {
    if (size < gamesum)
      return Row.ErrorRow(size)
    var retRow = Row.emptyRow(size)
    val sum = this.sum + this.length - 1
    var rowIndex = 0
    for (num <- list) {
      val spaceRemaining = size - (sum - num)
      val midsum = 2 * num - spaceRemaining
      if (midsum > 0) {
        rowIndex += (num - midsum)
        for (pos <- 0 until midsum) {
          retRow = retRow.updated(rowIndex, O)
          rowIndex += 1
        }
        if (num == midsum && rowIndex < size) retRow = retRow.updated(rowIndex, X)
        rowIndex += 1
      } else {
        rowIndex += (num + 1)
      }
    }
    retRow
  }

  override def iterator = list.iterator
}

object Requirement {
  def newBuilder: Builder[Int, Requirement] = new ListBuffer mapResult fromSeq
  def fromSeq(buf: Seq[Int]): Requirement = new Requirement(List(buf:_*))
  implicit def canBuildFrom: CanBuildFrom[Requirement, Int, Requirement] =
    new CanBuildFrom[Requirement, Int, Requirement] {
      def apply(): Builder[Int, Requirement] = newBuilder
      def apply(from: Requirement): Builder[Int, Requirement] = newBuilder
    }
  def apply(vals: Int*) = fromSeq(vals)
  def apply(lb:ListBuffer[Int]) = fromSeq(lb)
  def apply(l:List[Int]) = fromSeq(l)
}