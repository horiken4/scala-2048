trait Player {

  def decideMove(board: Board): Option[Move.Value]
}
