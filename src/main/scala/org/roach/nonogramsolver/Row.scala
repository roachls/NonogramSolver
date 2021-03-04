package org.roach.nonogramsolver
import collection.mutable.ListBuffer
import collection.SeqLike

abstract class Cell {
  def &(other: Cell): Cell = Cell.fromInt(Cell.toInt(this) & Cell.toInt(other))
  def &&(other: Cell): Cell = if (this == other) this else U
}
// Error
case object E extends Cell
// Cell is definitely marked
case object X extends Cell {
  override val toString = " "
}
// Cell is definitely unmarked
case object O extends Cell {
  override val toString = "*"
}
// Cell is unknown
case object U extends Cell

object Cell {
  val fromInt: Int => Cell = Array(E, O, X, U)
  val toInt: Cell => Int = Map(E -> 0, O -> 1, X -> 2, U -> 3)
}

import collection.IndexedSeqLike
import collection.mutable.{ Builder, ArrayBuffer }
import collection.generic.CanBuildFrom
import collection.immutable.LinearSeq

class Row(
  val _cellList: List[Cell]) extends LinearSeq[Cell] with SeqLike[Cell, Row] {
  implicit def listToRow(l: List[Cell]) = new Row(l)
  import Row._
  override protected[this] def newBuilder: Builder[Cell, Row] =
    Row.newBuilder
  override def reverse = Row(_cellList.reverse)
  def contains(c: Cell) = _cellList.contains(c)
  def indexOf(c: Cell) = _cellList.indexOf(c)
  override def drop(num: Int) = Row(_cellList drop num)
  override def take(num: Int) = Row(_cellList take num)
  def updated(index: Int, elem: Cell) = Row(_cellList.updated(index, elem))
  override def lengthCompare(len: Int): Int = _cellList.lengthCompare(len)
  override def isEmpty = _cellList.isEmpty
  override def head = _cellList.head
  override def tail = _cellList.tail
  override def length = _cellList.length
  def apply(idx: Int): Cell = _cellList(idx)
  override def foreach[T](f: Cell => T): Unit = _cellList.foreach { f }

  val countOs = _cellList.count(c => c == O)

  val isErrorRow = (_cellList.count(c => c == E)) > 0

  def compatibleWith(filter: Row) = (this.groups, filter.groups).zipped.forall((r, f) => (r | f) == f)

  def diff(req: Requirement) = scala.math.abs(countOs - req.sum)

  override lazy val toString: String = (for (idx <- 0 until length) yield this(idx).toString).mkString

  def &(that: Row): Row = Row((this._cellList, that._cellList).zipped.map((r, o) => r & o))
  def &&(that: Row): Row = Row((this._cellList, that._cellList).zipped.map((r, o) => r && o))

  def actual: Requirement = {
    if (!_cellList.contains(O))
      return Requirement()

    var lb = _actual(this)
    return Requirement(lb)
  }

  def groups(): Array[Int] = {
    val g = new Array[Int]((_cellList.length + N - 1) / N)
    for (i <- 0 until _cellList.length) {
      g(i / N) |= Cell.toInt(_cellList(i)) << (i % N * S)
    }
    g
  }

  def ++(otherRow: Row): Row = Row(this._cellList ++ otherRow._cellList)
}

object Row {
  private val S = 2
  private val N = 32 / S
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Cell]): Row =
    new Row(buf.toList)

  def apply(cells: Cell*) = fromSeq(cells)
  def apply(ab: ArrayBuffer[Cell]) = fromSeq(ab)
  def apply(ab: List[Cell]) = fromSeq(ab)

  def newBuilder: Builder[Cell, Row] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[Row, Cell, Row] =
    new CanBuildFrom[Row, Cell, Row] {
      def apply(): Builder[Cell, Row] = newBuilder
      def apply(from: Row): Builder[Cell, Row] = newBuilder
    }
  def emptyRow(length: Int) = Row(Array.fill(length)(U): _*)
  def ErrorRow(length: Int) = Row(Array.fill(length)(E): _*)

  def applyRules(row: Row, req: Requirement): Row = {
    val retforwards = rule3(row, req)
    val retbackwards = rule3(row.reverse, req.reverse).reverse
    val ret = retforwards & retbackwards
    if (ret contains E) ErrorRow(row.length) else ret
  }

  private def _actual(row: Row): ListBuffer[Int] = {
    var lb: ListBuffer[Int] = ListBuffer[Int]()
    if (!row.contains(O)) return lb
    var pos = row.indexOf(O)
    var count = 0
    while (pos < row.length && row(pos) == O) {
      count += 1
      pos += 1
    }
    lb += count
    if (pos + 1 < row.length)
      lb ++= _actual(row drop pos + 1)
    lb
  }

  def rule3(row: Row, req: Requirement): Row = {
    var isProblematic = false
    //    println(s"in= $row, req=$req")

    if (req.gamesum > row.length) return ErrorRow(row.length)
    if (req.gamesum == row.length) {
      // requirements exactly met
      return row & req.generateFilter(row.size)
    }
    if (req isEmpty) {
      return row & Row.fill(row.length)(X)
    }
    if (row.forall(_ == U)) { // ab is only U
      return req.generateFilter(row.length)
    }
    val firstO = First(O)

    val ret = row._cellList match {
      case Nil => Row()
      case r if r contains E => row
      case X :: tail => (row take 1) ++ rule3(Row(tail), req)
      case O :: tail => {
        val req1 = req.head
        var ab = ArrayBuffer[Cell](row: _*)
        ab = fill(ab, 0, req1 - 1, O)
        if (req1 < ab.length) ab(req1) &= X
        Row(ab.take(req1 + 1)) ++ rule3(row.drop(req1 + 1), req.tail)
      }
      case _ :: O :: _ :: tail if req.head == 1 =>
        ((row take 3) & Row(X, O, X)) ++ rule3(Row(tail), req.tail)
      case list if list contains O => {
        var ab = ArrayBuffer[Cell](row: _*)
        val pos = list.indexOf(O)
        val req1 = req.head
        if (list contains X) {
          val xPos = list.indexOf(X)
          if (xPos == pos + 1) {
            val start = pos - req1 + 1
            if (start < 0) return ErrorRow(row.length)
            if (req.length > 1) {
              // partition req into parts that fit and try each
              val leftHalf = row take xPos
              val rightHalf = row drop xPos + 1
              var tryRow: Row = null
              for (i <- 0 to req.length) {
                val (lpart, rpart) = req.splitAt(i)
                val tryPartRow = applyRules(leftHalf, lpart) ++ Row(X) ++ applyRules(rightHalf, rpart)
                tryRow = tryRow match {
                  case null => if (!(tryPartRow contains E)) tryPartRow else null
                  case _ => if (!(tryPartRow contains E)) tryRow && tryPartRow else tryRow
                }
              }
              return tryRow
            } else if (req.length == 1) {
              if (start > 0)
                ab = fill(ab, 0, start - 1, X)
              if (start >= 0)
                ab = fill(ab, start, pos - 1, O)
              if (pos + 1 < ab.length)
                ab = fill(ab, pos + 1, ab.length - 1, X)
            }
          }
        } else if (pos <= req1 - 1) {
          ab = fill(ab, pos, req1 - 1, O)
          if (pos == 0 && req1 < row.length) {
            ab(req1) &= X
          }
          if (req.size > 1) {
            //         TODO problematic for 10
            //            //          println("About to call rule3 recursively")
            //            ab = ab.take(req1) ++ ArrayBuffer(rule3(row drop req1, req.tail): _*)
          } else
            ab = fill(ab, req1 + pos, row.length - 1, X)
        }
        if (isProblematic) println(s"ab = $ab")
        Row(ab)
      }
      case list if list contains X => {
        val xPos = list.indexOf(X)
        // partition req into parts that fit and try each
        val leftHalf = row take xPos
        val rightHalf = row drop xPos + 1
        var tryRow: Row = null
        for (i <- 0 to req.length) {
          val (lpart, rpart) = req.splitAt(i)
          val tryPartRow = applyRules(leftHalf, lpart) ++ Row(X) ++ applyRules(rightHalf, rpart)
          tryRow = tryRow match {
            case null => if (!(tryPartRow contains E)) tryPartRow else null
            case _ => if (!(tryPartRow contains E)) tryRow && tryPartRow else tryRow
          }
        }
        if (tryRow == null) ErrorRow(row.length) else tryRow
      }
      case _ => row
    }

    if (isProblematic) println(s"Returning $ret")
    ret
  }

  def fill(ab: ArrayBuffer[Cell], start: Int, end: Int, c: Cell): ArrayBuffer[Cell] = {
    var fillret: ArrayBuffer[Cell] = ab.clone
    if (start > end) ab
    else if (start == end) {
      fillret(start) &= c
    } else {
      for (i <- start to end) {
        fillret(i) &= c
      }
    }
    fillret
  }

  def fill(size: Int)(cell: Cell) = Row(ArrayBuffer.fill(size)(cell))
}

case class First(cell: Cell) {
  def unapplySeq(row: Row): Option[Seq[Cell]] = {
    if (row.head == cell) Some(row.takeWhile(_ == cell)) else None
  }
}