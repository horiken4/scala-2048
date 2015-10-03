import scala.util.Random

object Move extends Enumeration {
  val Up, Down, Left, Right = Value
}

class Board(s: Int) {
  val size = s

  // Initialize grid
  var grid = Array.ofDim[Int](size, size)
  grid(Random.nextInt(size))(Random.nextInt(size)) = 2
  grid(Random.nextInt(size))(Random.nextInt(size)) = 2

  def print() {
    println("---")
    for (r <- grid) {
      printf("|")
      for (v <- r) {
        if (v != 0) {
          printf("% 4d", v)
        } else {
          printf("    ")
        }
        printf("|")
      }
      println
    }
    println("---")
  }

  def move(m: Move.Value) = {
    var board = copy

    m match {
      case Move.Up =>
        board.moveToUp
      case Move.Down =>
        board.moveToDown
      case Move.Left =>
        board.moveToLeft
      case Move.Right =>
        board.moveToRight
    }

    board
  }

  def putNumber() = {
    // Put 2 or 4 to empty cell
    var emptyCells = List.empty[Tuple2[Int, Int]]
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (grid(i)(j) == 0) {
          emptyCells = (i, j) +: emptyCells
        }
      }
    }
    if (emptyCells.length > 0) {
      val cell = emptyCells(Random.nextInt(emptyCells.length))
      val v = List(2, 4)(Random.nextInt(2))
      grid(cell._1)(cell._2) = v
    }

    this
  }

  def canMove() = {
    if (canMoveToUp || canMoveToDown || canMoveToLeft || canMoveToRight) {
      true
    } else {
      false
    }
  }

  def canMove(m: Move.Value) = {
    m match {
      case Move.Up =>
        canMoveToUp
      case Move.Down =>
        canMoveToDown
      case Move.Left =>
        canMoveToLeft
      case Move.Right =>
        canMoveToRight
    }
  }

  private def copy() = {
    val board = new Board(size)
    board.grid = grid.map(_.clone)

    board
  }

  private def canMoveToUp() = {
    val board = copy
    board.moveToUp

    if (board.grid.deep == grid.deep) {
      false
    } else {
      true
    }
  }

  private def canMoveToDown() = {
    val board = copy
    board.moveToDown

    if (board.grid.deep == grid.deep) {
      false
    } else {
      true
    }
  }

  private def canMoveToLeft() = {
    val board = copy
    board.moveToLeft

    if (board.grid.deep == grid.deep) {
      false
    } else {
      true
    }
  }

  private def canMoveToRight() = {
    val board = copy
    board.moveToRight

    if (board.grid.deep == grid.deep) {
      false
    } else {
      true
    }
  }

  private def moveToUp() {
    for (j <- 0 until size) {
      moveColumnToUp(j)
    }
  }

  private def moveToDown() {
    for (j <- 0 until size) {
      moveColumnToDown(j)
    }
  }

  private def moveToLeft() {
    for (i <- 0 until size) {
      moveRowToLeft(i)
    }
  }

  private def moveToRight() {
    for (i <- 0 until size) {
      moveRowToRight(i)
    }
  }

  private def extractColumn(j: Int) = {
    var column = Array.fill(size)(0)
    for (i <- 0 until size) {
      column(i) = grid(i)(j)
    }
    column
  }

  private def extractRow(i: Int) = {
    var row = Array.fill(size)(0)
    for (j <- 0 until size) {
      row(j) = grid(i)(j)
    }
    row
  }

  private def moveColumnToUp(j: Int) {
    var column = extractColumn(j)

    // Move
    column = column.filter(_ > 0)

    // Merge
    for (i <- 0 until column.length - 1) {
      if (column(i) == column(i + 1)) {
        column(i) += column(i + 1)
        column(i + 1) = 0
      }
    }

    // Move
    column = column.filter(_ > 0)

    // Padding
    while (column.length < size) {
      column = column :+ 0
    }

    // Update column
    for (i <- 0 until size) {
      grid(i)(j) = column(i)
    }
  }

  private def moveColumnToDown(j: Int) {
    var column = extractColumn(j).reverse

    // Move
    column = column.filter(_ > 0)

    // Merge
    for (i <- 0 until column.length - 1) {
      if (column(i) == column(i + 1)) {
        column(i) += column(i + 1)
        column(i + 1) = 0
      }
    }

    // Move
    column = column.filter(_ > 0)

    // Padding
    while (column.length < size) {
      column = column :+ 0
    }

    // Update column
    column = column.reverse
    for (i <- 0 until size) {
      grid(i)(j) = column(i)
    }
  }

  private def moveRowToLeft(i: Int) {
    var row = extractRow(i)

    // Move
    row = row.filter(_ > 0)

    // Merge
    for (j <- 0 until row.length - 1) {
      if (row(j) == row(j + 1)) {
        row(j) += row(j + 1)
        row(j + 1) = 0
      }
    }

    // Move
    row = row.filter(_ > 0)

    // Padding
    while (row.length < size) {
      row = row :+ 0
    }

    // Update column
    for (j <- 0 until size) {
      grid(i)(j) = row(j)
    }
  }

  private def moveRowToRight(i: Int) {
    var row = extractRow(i).reverse

    // Move
    row = row.filter(_ > 0)

    // Merge
    for (j <- 0 until row.length - 1) {
      if (row(j) == row(j + 1)) {
        row(j) += row(j + 1)
        row(j + 1) = 0
      }
    }

    // Move
    row = row.filter(_ > 0)

    // Padding
    while (row.length < size) {
      row = row :+ 0
    }

    // Update column
    row = row.reverse
    for (j <- 0 until size) {
      grid(i)(j) = row(j)
    }
  }
}
