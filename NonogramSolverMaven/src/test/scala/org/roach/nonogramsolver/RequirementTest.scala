package org.roach.nonogramsolver

/* Unit tests */
import org.scalatest._
class RequirementTest extends FlatSpec {
  behavior of "Requirement"
  
  it should "have length equal to the number of arguments passed to the constructor" in {
    assert(Requirement(1,2).length == 2)
    assert(Requirement(1).length == 1)
    assert(Requirement(1,2,3).length == 3)
    assert(Requirement().length == 0)
//    assert(req1.sum == 3)
  }
  
  it should "have sum equal to the sum of its arguments" in {
    assert(Requirement().sum == 0)
    assert(Requirement(1).sum == 1)
    assert(Requirement(1, 2).sum == 3)
    assert(Requirement(1, 2, 3).sum == 6)
  }
  
  it should "produces rows that match expected Row production" in {
    assert(Requirement(1).generateRows(1) === List(Row(O)))
    assert(Requirement(1).generateRows(2) === List(Row(O,X), Row(X, O)))
    assert(Requirement(1, 2).generateRows(4) === List(Row(O,X,O,O)))
    assert(Requirement(1, 2).generateRows(5) === List(
        Row(X,O,X,O,O),
        Row(O,X,O,O,X),
        Row(O,X,X,O,O)
      )
    )
  }
  
  it should "throw an IllegalArgumentException when trying to produce too small a Row" in {
    assertThrows[IllegalArgumentException] {
      Requirement(1, 2).generateRows(3)
    }
  }

  it should "generate appropriate filters" in {
    assert((Requirement(5) generateFilter 5) === Row(O,O,O,O,O))
    assert((Requirement(5) generateFilter 6) === Row(U,O,O,O,O,U))
    assert((Requirement(5) generateFilter 9) === Row(U,U,U,U,O,U,U,U,U))
    assert((Requirement(5) generateFilter 10) === Row(U,U,U,U,U,U,U,U,U,U))

    assert((Requirement(2, 4, 3) generateFilter 11) === Row(O,O,X,O,O,O,O,X,O,O,O))
    assert((Requirement(2, 4, 3) generateFilter 12) === Row(U,O,U,U,O,O,O,U,U,O,O,U))
    assert((Requirement(2, 4, 3) generateFilter 13) === Row(U,U,U,U,U,O,O,U,U,U,O,U,U))
    assert((Requirement(2, 4, 3) generateFilter 14) === Row(U,U,U,U,U,U,O,U,U,U,U,U,U,U))
    assert((Requirement(2, 4, 3) generateFilter 15) === Row(U,U,U,U,U,U,U,U,U,U,U,U,U,U,U))
  }
  
  it should "produce an error row when trying to produce a filter for too small a Row" in {
    assert(Requirement(2, 4, 3).generateFilter(10) === Row(E,E,E,E,E,E,E,E,E,E))
  }

  it should "have a gamesum equal to the sum of the arguments plus the number of 'gaps' between arguments" in {
    assert(Requirement(1).gamesum === 1)
    assert(Requirement(1, 2).gamesum === 4)
    assert(Requirement(1, 2, 3).gamesum === 8)
  }
}
