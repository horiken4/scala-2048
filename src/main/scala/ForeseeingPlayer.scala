class ForeseeingPlayer(depthForeseeing: Int) {

  def decideMove(board: Board) = {
    var bestMove: Option[Move.Value] = Option(null)
    var bestScore = 0.0

    for (m <- Move.values) {
      if (board.canMove(m)) {
        var score = findBestScore(board.move(m).putNumber, 0, depthForeseeing)
        if (bestScore < score) {
          bestScore = score
          bestMove = Option(m)
        }
      }
    }

    println("bestScore", bestScore)
    bestMove
  }

  private def findBestScore(board: Board, depth: Int, maxDepth: Int): Double = {
    if (depth == maxDepth) {
      calcBoardScore(board)
    } else {
      var bestScore = 0.0
      var bestMove: Option[Move.Value] = Option(null)

      for (m <- Move.values) {
        if (board.canMove(m)) {
          var score = findBestScore(board.move(m).putNumber, depth + 1, maxDepth)
          if (bestScore < score) {
            bestScore = score
            bestMove = Option(m)
          }
        }
      }

      bestMove match {
        case Some(move) => bestScore
        case None => calcBoardScore(board) // Can not move
      }

      bestScore
    }
  }

  private def calcBoardScore(board: Board) = {
    // TODO: You should enhance score function

    // Small island is better
    var minCol = board.size
    var maxCol = -1
    var minRow = board.size
    var maxRow = -1
    for ((r, i) <- board.grid.zipWithIndex) {
      for ((v, j) <- r.zipWithIndex) {
        if (v > 0) {
          if (minRow > i) {
            minRow = i
          }
          if (maxRow < i) {
            maxRow = i
          }
          if (minCol > j) {
            minCol = j
          }
          if (maxCol < j) {
            maxCol = j
          }
        }
      }
    }
    val islandScore = 2.0 / ((maxRow - minRow) * (maxCol - minCol))

    islandScore
  }
}
