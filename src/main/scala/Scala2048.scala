object Scala2048 {
  def main(args: Array[String]) {
    var board = new Board(4)
    // val player = new RandomPlayer
    val player = new ForeseeingPlayer(depthForeseeing=5)
    var numMoves = 0

    board.print
    while (board.canMove) {
      val move = player.decideMove(board)
      move match {
        case Some(move) =>
          println(move)
          board = board.move(move).putNumber
          board.print
          numMoves += 1
        case None =>
          println("Can not move!!")
      }
    }

    println("numMoves", numMoves)
  }
}
