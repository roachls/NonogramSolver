package org.roach.nonogramsolver

/* Unit tests */
import org.scalatest._

class GridTest extends FlatSpec {
  behavior of "Grid"
  
  it should "retrieve the correct row using apply" in {
    val grid1 = Grid(Row(O,X), Row(X,O))
    assert(grid1(0) === Row(O,X))
    assert(grid1(1) === Row(X,O))
  }
  
  it should "not allow retrieval of invalid rows" in {
    val grid1 = Grid(Row(O,X), Row(X,O))
    assertThrows[IndexOutOfBoundsException] {
      grid1(-1)
    }
    assertThrows[IndexOutOfBoundsException] {
      grid1(2)
    }
  }
  
  it should "retrieve the correct column using getCol" in {
    val grid1 = Grid(Row(O,X,U), Row(O,O,O), Row(U,X,O))
    assert(grid1.getCol(0) === Row(O,O,U))
    assert(grid1.getCol(1) === Row(X,O,X))
    assert(grid1.getCol(2) === Row(U,O,O))
  }
  
  it should "not allow retrieval of invalid columns" in {
    val grid1 = Grid(Row(O,X), Row(X,O))
    assertThrows[IndexOutOfBoundsException] {
      grid1.getCol(-1)
    }
    assertThrows[IndexOutOfBoundsException] {
      grid1.getCol(2)
    }
  }
  
  it should "update itself properly with the updated method" in {
    val grid = Grid(Row(O,O), Row(X,X))
    assert(grid(0) === Row(O,O))
    val grid2 = grid.updated(0, Row(U,E))
    assert(grid2(0) === Row(U,E))
    assert(grid2.getCol(0) === Row(U,X))
    val grid3 = grid.updated(1, Row(E,E))
    assert(grid3 === Grid(Row(O,O), Row(E,E)))
    assert(grid3.getCol(1) === Row(O,E))
  }
  
  it should "invert properly" in {
    val grid = Grid(Row(O,X), Row(U,E))
    assert(grid.getCol(0) === Row(O,U))
    assert(grid.getCol(1) === Row(X,E))
    val invertedGrid = Grid(Row(O,U), Row(X,E))
    val ig = grid.inverse
    assert(ig(0) === Row(O,U))
    assert(grid.inverse === invertedGrid)
    assert(invertedGrid.inverse === grid)
  }
  
  it should "correctly calculate heuristic differences from requirements" in {
    assert(Grid(Row(O)).columnDiff(List(Requirement(1))) == 0)
    assert(Grid(Row(X)).columnDiff(List(Requirement(1))) == 1)
    assert(Grid(Row(U)).columnDiff(List(Requirement(1))) == 1)
    assert(Grid(Row(E)).columnDiff(List(Requirement(1))) == 1)
    
    val grid = Grid(Row(U,U), Row(U,U))
    assert(grid.columnDiff(List(Requirement(1), Requirement(2))) == 3)
    assert(grid.columnDiff(List(Requirement(2), Requirement(2))) == 4)
    
    val grid2 = Grid(Row(O,X), Row(X,O))
    assert(grid2.getCol(0).countOs == 1)
    assert(grid2.getCol(1).countOs == 1)
    assert(Requirement(1).sum == 1)
    assert(grid2.columnDiff(List(Requirement(1), Requirement(1))) == 0)
    assert(grid2.columnDiff(List(Requirement(0), Requirement(1))) == 1)
  }
  
  it should "merge correctly with &" in {
    assert((Grid(Row(O)) & Grid(Row(O))) === Grid(Row(O)))
    assert((Grid(Row(O)) & Grid(Row(U))) === Grid(Row(O)))
    assert((Grid(Row(O)) & Grid(Row(X))) === Grid(Row(E)))
    assert((Grid(Row(O)) & Grid(Row(E))) === Grid(Row(E)))
    assert((Grid(Row(U)) & Grid(Row(O))) === Grid(Row(O)))
    assert((Grid(Row(U)) & Grid(Row(X))) === Grid(Row(X)))
    assert((Grid(Row(U)) & Grid(Row(U))) === Grid(Row(U)))
    assert((Grid(Row(U)) & Grid(Row(E))) === Grid(Row(E)))
    assert((Grid(Row(X)) & Grid(Row(O))) === Grid(Row(E)))
    assert((Grid(Row(X)) & Grid(Row(U))) === Grid(Row(X)))
    assert((Grid(Row(X)) & Grid(Row(X))) === Grid(Row(X)))
    assert((Grid(Row(X)) & Grid(Row(E))) === Grid(Row(E)))
    assert((Grid(Row(E)) & Grid(Row(X))) === Grid(Row(E)))
    assert((Grid(Row(E)) & Grid(Row(O))) === Grid(Row(E)))
    assert((Grid(Row(E)) & Grid(Row(U))) === Grid(Row(E)))
    assert((Grid(Row(E)) & Grid(Row(E))) === Grid(Row(E)))
    
    assert((Grid(Row(O,X,U,E), Row(O,X,U,E), Row(O,X,U,E), Row(O,X,U,E)) 
          & Grid(Row(O,O,O,O), Row(X,X,X,X), Row(U,U,U,U), Row(E,E,E,E)))
        === Grid(Row(O,E,O,E), Row(E,X,X,E), Row(O,X,U,E), Row(E,E,E,E)))
  }
  
  it should "produce a properly-sized empty grid" in {
    assert(Grid.emptyGrid(1, 2).length == 2)
    assert(Grid.emptyGrid(1, 1) === Grid(Row(U)))
    assert(Grid.emptyGrid(2, 3).length == 3)
    assert(Grid.emptyGrid(2, 4) === Grid(Row(U,U), Row(U,U), Row(U,U), Row(U,U)))
  }
}
