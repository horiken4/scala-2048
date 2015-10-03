import scala.util.Random

class RandomPlayer extends Player {

  def decideMove(board: Board) = {
    // Choose move randomly
    val movable = List(
      (board.canMove(Move.Up), Move.Up),
      (board.canMove(Move.Down), Move.Down),
      (board.canMove(Move.Left), Move.Left),
      (board.canMove(Move.Right), Move.Right)).filter(_._1)

    if (movable.length <= 0) {
      Option(null)
    } else {
      Option(movable(Random.nextInt(movable.length))._2)
    }
  }
}
