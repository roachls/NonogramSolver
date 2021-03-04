package org.roach.nonogramsolver

/* Unit tests */
import org.scalatest._

class RulesTest extends FlatSpec {
  behavior of "Row applyRules"
  
  it should "show proper behavior" in {
    val row1 = Row(O, U, U)
    val req1a = Requirement(3)
    val req1b = Requirement(2)
    val req1c = Requirement(1)
    assert(Row.applyRules(row1, req1a) === Row(O, O, O))
    assert(Row.applyRules(row1, req1b) === Row(O, O, X))
    assert(Row.applyRules(row1, req1c) === Row(O, X, X))

    val row2 = Row(U, U, O)
    val req2a = Requirement(3)
    val req2b = Requirement(2)
    val req2c = Requirement(1)
    assert(Row.applyRules(row2, req2a) === Row(O, O, O))
    assert(Row.applyRules(row2, req2b) === Row(X, O, O))
    assert(Row.applyRules(row2, req2c) === Row(X, X, O))

    val row3 = Row(O, X, U)
    val req3a = Requirement(3)
    val req3b = Requirement(2)
    val req3c = Requirement(1)
    val errorRow3 = Row.ErrorRow(3)
    assert(Row.applyRules(row3, req3a) === errorRow3)
    assert(Row.applyRules(row3, req3b) === errorRow3)
    assert(Row.applyRules(row3, req3c) === Row(O, X, X))

    val row4 = Row(U, X, O)
    val req4a = Requirement(3)
    val req4b = Requirement(2)
    val req4c = Requirement(1)
    assert(Row.applyRules(row4, req4a) === errorRow3)
    assert(Row.applyRules(row4, req4b) === errorRow3)
    assert(Row.applyRules(row4, req4c) === Row(X, X, O))

    val row5 = Row(O, U, U, U, O)
    val req5a = Requirement(1, 1)
    val req5b = Requirement(1, 2)
    val req5c = Requirement(2, 2)
    val req5d = Requirement(5)
    val req5e = Requirement(4)
    val errorRow5 = Row.ErrorRow(5)
    assert(Row.applyRules(row5, req5a) === Row(O, X, U, X, O))
    assert(Row.applyRules(row5, req5b) === Row(O, X, X, O, O))
    assert(Row.applyRules(row5, req5c) === Row(O, O, X, O, O))
    assert(Row.applyRules(row5, req5d) === Row(O, O, O, O, O))
    assert(Row.applyRules(row5, req5e) === errorRow5)
    assert(Row.applyRules(row5, req5e) === errorRow5)

    val row6 = Row(U, O, U, U)
    val req6a = Requirement(2)
    val req6b = Requirement(3)
    val req6c = Requirement(4)
    assert(Row.applyRules(row6, req6a) === Row(U, O, U, X))
    assert(Row.applyRules(row6, req6b) === Row(U, O, O, U))
    assert(Row.applyRules(row6, req6c) === Row(O, O, O, O))

    val row7 = Row(U, O, U, U, O, U, U, U, U, O)
    val req7a = Requirement(2, 4, 1)
    assert(Row.applyRules(row7, req7a) === Row(U, O, U, U, O, O, O, U, X, O))

    val row8 = Row(U,U,U,U,O,U,U,U,U,U)
    assert(Row.applyRules(row8, Requirement(6)) === Row(U,U,U,U,O,O,U,U,U,U))
    assert(Row.applyRules(row8, Requirement(1,5)) === Row(U,U,U,U,O,O,O,U,U,U))
    assert(Row.applyRules(row8, Requirement(2,5)) === Row(U,U,U,U,O,O,O,O,U,U))
    assert(Row.applyRules(row8, Requirement(5,1)) === Row(U,U,U,O,O,U,U,U,U,U))
    assert(Row.applyRules(row8, Requirement(1,5,1)) === Row(U,U,U,O,O,O,O,U,U,U))

    val row9 = Row(U,U,U,O,U)
    assert(Row.applyRules(row9, Requirement(1, 3)) === Row(O,X,O,O,O))

    val row10 = Row(U,X,U,O,U,U,U,U,U)
    val req10a = Requirement(3, 2)
    assert(Row.applyRules(row10, req10a) === Row(U,X,U,O,O,U,U,U,U))

    val row11 = Row(U,U,X,U,U,O,U,U,U,U)
    val req1 = Requirement(1, 3)
    assert(Row.applyRules(row11, req1) === row11) // Tricky!

    val row12 = Row(U,U,U,U,O,U,U,O,U,U)
    val req2 = Requirement(3, 3)
    assert(Row.applyRules(row12, req2) === Row(U,U,U,U,O,U,U,O,U,U))

    val row13 = Row(U,U,U,U,X,U,U,O,U,U)
    val req3 = Requirement(2, 4)
    assert(Row.applyRules(row13, req3) === Row(U,U,U,U,X,U,O,O,O,U))
  }
}
