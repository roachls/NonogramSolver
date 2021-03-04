package org.roach.nonogramsolver

/* Unit tests */
import org.scalatest._
import collection.mutable.ArrayBuffer

class RowTest extends FlatSpec {
  behavior of "Cell"
  it should "apply the & operator correctly" in {
    assert((O & O) == O)
    assert((O & E) == E)
    assert((O & X) == E)
    assert((O & U) == O)
    assert((U & U) == U)
    assert((U & E) == E)
    assert((U & X) == X)
    assert((U & O) == O)
    assert((X & X) == X)
    assert((X & E) == E)
    assert((X & O) == E)
    assert((X & U) == X)
    assert((E & E) == E)
    assert((E & O) == E)
    assert((E & X) == E)
    assert((E & U) == E)
  }

  behavior of "Row"

  it should "have a length equal to the constructor parameter" in {
    assert(Row.emptyRow(5).length === 5)
  }

  it should "be properly updatable" in {
    assert(Row.emptyRow(5).updated(0, O) === Row(O, U, U, U, U))
  }

  it should "have a number of Os equal to the number of O cells in it" in {
    assert(Row.emptyRow(5).countOs === 0)
  }

  it should "return the proper cell with the apply method" in {
    val row2 = Row(X, O, U, X, O)
    assert(row2(0) === X)
    assert(row2(1) === O)
    assert(row2(2) === U)
  }

  it should "return true for isErrorRow if there are any errors" in {
    assert(Row(X, O).isErrorRow == false)
    assert(Row(E, O).isErrorRow == true)
  }

  behavior of "Row take method"

  it should "return the correct slice" in {
    val row2 = Row(X, O, X, X, O)
    assert((row2 take 3) === Row(X, O, X))
  }

  behavior of "Row padTo method"
  it should "pad a row correctly" in {
    val row2 = Row(X, O, X, X, O)
    assert(row2.padTo(10, X) === Row(X, O, X, X, O, X, X, X, X, X))
  }

  behavior of "Row ++ operator"
  it should "append one row to another" in {
    assert(Row(X) ++ Row(O) === Row(X, O))
    assert(Row(X, O, X, X, O) ++ Row(O, X, O, O, X) === Row(X, O, X, X, O, O, X, O, O, X))
  }

  behavior of "Row compatibleWith method"
  it should "show proper behavior" in {
    assert(Row(X) compatibleWith Row(X))
    assert(Row(X) compatibleWith Row(U))
    assert(!(Row(X) compatibleWith Row(O)))
    assert(!(Row(X) compatibleWith Row(E)))

    assert(!(Row(O) compatibleWith Row(X)))
    assert(Row(O) compatibleWith Row(U))
    assert(Row(O) compatibleWith Row(O))
    assert(!(Row(O) compatibleWith Row(E)))

    assert(!(Row(U) compatibleWith Row(X)))
    assert(Row(U) compatibleWith Row(U))
    assert(!(Row(U) compatibleWith Row(O)))
    assert(!(Row(U) compatibleWith Row(E)))

    assert(Row(E) compatibleWith Row(X))
    assert(Row(E) compatibleWith Row(U))
    assert(Row(E) compatibleWith Row(O))
    assert(Row(E) compatibleWith Row(E))

    val row1 = Row(X, O, X, X, O)
    assert(row1 compatibleWith Row(X, O, X, X, O))
    assert(row1 compatibleWith Row(U, U, U, U, U))
    assert(row1 compatibleWith Row(U, O, U, U, O))
    assert(row1 compatibleWith Row(X, U, X, X, U))
    assert(!(row1 compatibleWith Row(X, X, X, X, X)))
    assert(!(row1 compatibleWith Row(O, O, O, O, O)))
    assert(!(row1 compatibleWith Row(E, U, U, U, U)))
    assert(!(row1 compatibleWith Row(U, E, U, U, U)))
  }

  behavior of "Row diff method"
  it should "show proper behavior" in {
    val row1 = Row(X, X, O, O, X, O, O, O, X, O)
    val req1 = Requirement(2, 3, 1)
    val req2 = Requirement(3, 2, 1)
    val req3 = Requirement(1, 1, 1)
    val req4 = Requirement(6)
    val req5 = Requirement(3, 3, 3)
    assert((row1 diff req1) == 0)
    assert((row1 diff req2) == 0)
    assert((row1 diff req3) == 3)
    assert((row1 diff req4) == 0)
    assert((row1 diff req5) == 3)
  }

  behavior of "Row & operator"
  it should "show proper behavior" in {
    val row1 = Row(X, O, U, E, X, O, U, E, X, O, U, E, X, O, U, E)
    val fil1 = Row(X, X, X, X, O, O, O, O, U, U, U, U, E, E, E, E)
    assert((row1 & fil1) === Row(X, E, X, E, E, O, O, E, X, O, U, E, E, E, E, E))
  }

  behavior of "Row reverse method"
  it should "reverse the order of elements of a Row" in {
    assert(Row(O).reverse === Row(O))
    assert(Row(O, X, U, O, X, X).reverse === Row(X, X, O, U, X, O))
  }

  behavior of "actual method"
  it should "return correct actuals" in {
    assert(Row(U, U, U, U).actual === Requirement())
    assert(Row(O, U, U, U).actual === Requirement(1))
    assert(Row(O, O, U, U).actual === Requirement(2))
    assert(Row(O, O, O, U).actual === Requirement(3))
    assert(Row(O, O, O, O).actual === Requirement(4))
    assert(Row(X, X, X, X).actual === Requirement())
    assert(Row(E, E, E, E).actual === Requirement())
    assert(Row(E, X, U, X).actual === Requirement())
    assert(Row(O, X, O, O).actual === Requirement(1, 2))
    assert(Row(X, O, O, O).actual === Requirement(3))
    assert(Row(U, O, O, O).actual === Requirement(3))
    assert(Row(O, U, X, O).actual === Requirement(1, 1))
    assert(Row(U, O, O, U).actual === Requirement(2))
  }

  behavior of "fill method"
  it should "correctly fill an ArrayBuffer" in {
    assert(Row.fill(ArrayBuffer(U, U, U, U), 0, 1, O) === ArrayBuffer(O, O, U, U))
    assert(Row.fill(ArrayBuffer(U, U, U, U), 1, 2, O) === ArrayBuffer(U, O, O, U))
    assert(Row.fill(ArrayBuffer(U, U, U, U), 0, 3, O) === ArrayBuffer(O, O, O, O))
    assert(Row.fill(ArrayBuffer(X, O, E, U), 0, 1, O) === ArrayBuffer(E, O, E, U))
    assert(Row.fill(ArrayBuffer(X, X, X, X), 1, 2, O) === ArrayBuffer(X, E, E, X))
  }

  behavior of "rules"
  //  it should "apply rule1 correctly" in {
  //    assert(Row.rule1(Row(O,U,U,U,U), Requirement(1)) === ArrayBuffer(O,X,X,X,X))
  //    assert(Row.rule1(Row(E,O,U,U,U), Requirement(1)) === ArrayBuffer(E,O,X,X,X))
  //    assert(Row.rule1(Row(O,O,U,U,O), Requirement(2,1)) === ArrayBuffer(O,O,X,X,O))
  //    assert(Row.rule1(Row(U,O,O,O,U,U,U,O,U), Requirement(3, 2)) === ArrayBuffer(X,O,O,O,X,X,U,O,U))
  //  }

  //  it should "apply rule2 correctly" in {
  //    assert(Row.rule2(Row(U,U,U,U,U))(Requirement(3,1)) === ArrayBuffer(O,O,O,X,O))
  //    assert(Row.rule2(Row(U,U,U,U,U))(Requirement(2,2)) === ArrayBuffer(O,O,X,O,O))
  //    assert(Row.rule2(Row(U,U,O,U,U))(Requirement(2,2)) === ArrayBuffer(O,O,E,O,O))
  //    assert(Row.rule2(Row(U,U,U,U,U))(Requirement(5)) === ArrayBuffer(O,O,O,O,O))
  //    assert(Row.rule2(Row(U,U,U,X,U))(Requirement(5)) === ArrayBuffer(O,O,O,E,O))
  //    assert(Row.rule2(Row(U,O,U,U,U))(Requirement(3)) === ArrayBuffer(U,O,O,U,U))
  //    assert(Row.rule2(Row(U,O,U,U,U))(Requirement(4)) === ArrayBuffer(U,O,O,O,U))
  //    assert(Row.rule2(Row(O,U,U,U,U))(Requirement(3)) === ArrayBuffer(O,O,O,X,U))
  //    assert(Row.rule2(Row(U,U,O,U,U))(Requirement(3,1)) === ArrayBuffer(O,O,O,X,O))
  //    assert(Row.rule2(Row(U,O,U,U,U))(Requirement(2,2)) === ArrayBuffer(O,O,X,O,O))
  //    assert(Row.rule2(Row(U,O,U,U,U))(Requirement(2,1)) === ArrayBuffer(U,O,U,U,U))
  //    assert(Row.rule2(Row(U,O,U,U,U))(Requirement(1,2)) === ArrayBuffer(U,O,U,O,U))
  //    assert(Row.rule2(Row(U,U,U,U,U))(Requirement(3)) === ArrayBuffer(U,U,O,U,U))
  //
  //    assert(Row.rule2(Row(U,X,U,U,U,U,U))(Requirement(5)) === ArrayBuffer(X,X,O,O,O,O,O))
  //    assert(Row.rule2(Row(U,X,U,U,U,U,U))(Requirement(4)) === ArrayBuffer(X,X,U,O,O,O,U))
  //  }

  //  it should "apply rule3 correctly" in {
  //    assert(Row.rule3(Row(O,U,U,U,U))(Requirement(3)) === ArrayBuffer(O,O,O,X,X))
  //    assert(Row.rule3(Row(O,O,U,U,U))(Requirement(3)) === ArrayBuffer(O,O,O,X,X))
  //    assert(Row.rule3(Row(O,O,U,U,U))(Requirement(2)) === ArrayBuffer(O,O,X,X,X))
  //    assert(Row.rule3(Row(O,U,U,U,U))(Requirement(3,1)) === ArrayBuffer(O,U,U,U,U))
  //    assert(Row.rule3(Row(U,O,U,U,U))(Requirement(3)) === ArrayBuffer(U,O,O,U,X))
  //    assert(Row.rule3(Row(O,U,U,E,U))(Requirement(3)) === ArrayBuffer(O,U,U,E,U))
  //  }
  //
  //  it should "method applyRules should work" in {
  //    assert(Row(O,U,U,U,U).applyRules(Requirement(3)) === Row(O,O,O,X,X))
  //    assert(Row(U,O,U,U,U).applyRules(Requirement(3)) === Row(U,O,O,U,X))
  //    assert(Row(U,U,O,U,U).applyRules(Requirement(3)) === Row(U,U,O,U,U))
  //    assert(Row(U,U,U,O,U).applyRules(Requirement(3)) === Row(U,U,O,O,U))
  //    assert(Row(U,U,U,U,O).applyRules(Requirement(3)) === Row(U,U,O,O,O))
  //    assert(Row(U,U,U,U,U).applyRules(Requirement(5)) === Row(O,O,O,O,O))
  //    assert(Row(U,U,U,U,U).applyRules(Requirement(4)) === Row(U,O,O,O,U))
  //    assert(Row(U,U,U,U,U).applyRules(Requirement(3)) === Row(U,U,O,U,U))
  //    assert(Row(O,U,U,U,U).applyRules(Requirement(4)) === Row(O,O,O,O,X))
  //    assert(Row(O,O,U,U,U).applyRules(Requirement(4)) === Row(O,O,O,O,X))
  //    assert(Row(O,O,O,U,U).applyRules(Requirement(4)) === Row(O,O,O,O,X))
  //    assert(Row(O,O,O,X,U).applyRules(Requirement(4)) === Row(O,O,O,E,X))
  //    assert(Row(U,U,U,U,U).applyRules(Requirement(2,1)) === Row(U,O,U,U,U))
  //    assert(Row(U,U,U,U,U).applyRules(Requirement(3,1)) === Row(O,O,O,X,O))
  //    assert(Row(U,U,U,U,U).applyRules(Requirement(1,1)) === Row(U,U,U,U,U))
  //    assert(Row(U,O,U,U,U).applyRules(Requirement(3)) === Row(U,O,O,U,X))
  //    assert(Row(U,O,U,U,U).applyRules(Requirement(4)) === Row(U,O,O,O,U))
  //    assert(Row(X,X,X,X,U).applyRules(Requirement(1)) === Row(X,X,X,X,O))
  //    assert(Row(U,X,X,X,U).applyRules(Requirement(1)) === Row(U,X,X,X,U))
  //    assert(Row(X,U,U,X,X).applyRules(Requirement(2)) === Row(X,O,O,X,X))
  //  }

  it should "object method applyRules should work" in {
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(1)) === Row(O, X, X, X, X))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(2)) === Row(O, O, X, X, X))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(3)) === Row(O, O, O, X, X))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(4)) === Row(O, O, O, O, X))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(5)) === Row(O, O, O, O, O))

    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(1, 1)) === Row(O, X, U, U, U))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(1, 2)) === Row(O, X, U, O, U))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(1, 3)) === Row(O, X, O, O, O))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(2, 1)) === Row(O, O, X, U, U))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(2, 2)) === Row(O, O, X, O, O))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(3, 1)) === Row(O, O, O, X, O))
    assert(Row.applyRules(Row(O, U, U, U, U), Requirement(1, 1, 1)) === Row(O, X, O, X, O))

    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(1)) === Row(O, X, X, X, X))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(2)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(3)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(4)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(5)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(1, 1)) === Row(O, X, U, U, U))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(1, 2)) === Row(O, X, U, O, U))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(1, 3)) === Row(O, X, O, O, O))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(2, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(2, 2)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(3, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(O, X, U, U, U), Requirement(1, 1, 1)) === Row(O, X, O, X, O))

    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(1)) === Row(X, U, U, U, U))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(2)) === Row(X, U, U, U, U))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(3)) === Row(X, U, O, O, U))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(4)) === Row(X, O, O, O, O))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(5)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(1, 1)) === Row(X, U, U, U, U))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(1, 2)) === Row(X, O, X, O, O))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(1, 3)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(2, 1)) === Row(X, O, O, X, O))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(2, 2)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(3, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(X, U, U, U, U), Requirement(1, 1, 1)) === Row(E, E, E, E, E))

    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(1)) === Row(U, X, U, U, U))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(2)) === Row(X, X, U, O, U))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(3)) === Row(X, X, O, O, O))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(4)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(5)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(1, 2)) === Row(O, X, U, O, U))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(1, 3)) === Row(O, X, O, O, O))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(2, 2)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(3, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(2, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, X, U, U, U), Requirement(1, 1, 1)) === Row(O, X, O, X, O))

    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(1)) === Row(U, U, X, U, U))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(2)) === Row(U, U, X, U, U))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(3)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(4)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(5)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(1, 1)) === Row(U, U, X, U, U))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(1, 2)) === Row(U, U, X, O, O))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(1, 3)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(2, 1)) === Row(O, O, X, U, U))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(2, 2)) === Row(O, O, X, O, O))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(3, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, U, X, U, U), Requirement(1, 1, 1)) === Row(E, E, E, E, E))

    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(1)) === Row(X, O, X, X, X))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(2)) === Row(O, O, X, X, X))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(3)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(4)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(5)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, U, U, U, O, X, U, U), Requirement(2)) === Row(X, X, X, O, O, X, X, X))
    assert(Row.applyRules(Row(U, U, U, O, X, U, U, U), Requirement(2, 2)) === Row(X, X, O, O, X, U, O, U))
    assert(Row.applyRules(Row(U, U, U, O, X, U, U, U), Requirement(1, 1, 2)) === Row(U, U, X, O, X, U, O, U))
    assert(Row.applyRules(Row(U, U, U, U, O, X, U, U), Requirement(2, 2)) === Row(U, U, X, O, O, X, U, U))
    assert(Row.applyRules(Row(U, U, U, U, U, O, X, U, X, U, U, U, U, U, U), Requirement(1, 1, 1)) === Row(U, U, U, U, X, O, X, U, X, U, U, U, U, U, U))
    assert(Row.applyRules(Row(U, U, U, U, U, O, X, U, U, U, O, U, X, U, U, U, O, U, U, U), Requirement(3, 2, 1, 3, 2, 1)) === Row(O, O, O, X, O, O, X, U, U, U, O, U, X, U, U, U, O, U, U, U))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(1, 1)) === Row(X, O, X, U, U))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(1, 2)) === Row(X, O, X, O, O))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(1, 3)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(2, 1)) === Row(O, O, X, U, U))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(2, 2)) === Row(O, O, X, O, O))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(3, 1)) === Row(E, E, E, E, E))
    assert(Row.applyRules(Row(U, O, X, U, U), Requirement(1, 1, 1)) === Row(E, E, E, E, E))

    assert(Row.applyRules(Row(U, U, O, O, U), Requirement(3)) === Row(X, U, O, O, U))
    assert(Row.applyRules(Row(U, O, O, U, U), Requirement(3)) === Row(U, O, O, U, X))

    assert(Row.applyRules(Row(U, U, O, U, U, U, U, U, U, U), Requirement(3)) === Row(U, U, O, U, U, X, X, X, X, X))
    assert(Row.applyRules(Row(U, U, U, U, U, U, O, O, U, U), Requirement(2, 4)) === Row(U, U, U, U, U, U, O, O, U, U))
//        assert(Row.applyRules(Row(U,O,O,U,U,X,X,X,X,X), Requirement(3)) === Row(U,O,O,U,X,X,X,X,X,X)) // TODO
    //    assert(Row.applyRules(Row(X,X,X,X,X,U,U,O,O,U), Requirement(3)) === Row(X,X,X,X,X,X,U,O,O,U)) // TODO
//        assert(Row.applyRules(Row(U,O,X,U,U,O,U,U,U,U), Requirement(1,1,1)) === Row(X,O,X,U,X,O,X,U,U,U)) // TODO
  }

  behavior of "unapplySeq"
  it should "behave correctly" in {
    val row = Row(O, O, X, E, U)
    val firstO = new First(O)
    assert(firstO.unapplySeq(row) === Some(Row(O, O)))
    assert(firstO.unapplySeq(Row(U, X, U, U, U)) === None)
    val firstX = new First(X)
    assert(firstX.unapplySeq(row) === None)
  }
}
