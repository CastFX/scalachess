package it.scalachess.core.logic.moves.generators

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.PieceType
import scalaz.{ Failure, Success, Validation }

/*
 * why use Validation[...,...] if the method apply returns Set[ValidMove]?
 * Failure is useful for declarate errors
 *
 * magari in futuro oltre al match delle mosse giuste, si fa anche il match delle mosse sbagliate,
 * nel caso la mossa inserita in input dall'utente matcha una mossa sbagliata, si restituisce
 * il messaggio di errore (con la notazione algebrica è impossibile matchare mosse sbagliate,
 * ma magari con la notazione mossa(from, to) sì
 */

case class GeneratePawnMoves(pieceType: PieceType, color: Color, board: Board, from: Position)
    extends GeneratePieceMoves {

  override def apply(): List[ValidMove] =
    color match {
      case White => generatePawnMoves(1)
      case Black => generatePawnMoves(-1)
    }

  def generatePawnMoves(forwardRowMod: Int): List[ValidMove] = {
    val moveOnePosWithoutCapture =
      generatePawnMovement(Position.of(from.col, from.row + forwardRowMod))
    val moveTwoPosWithoutCapture =
      from.row match {
        case Board.whiteMinorPiecesStartingRow =>
          moveOnePosWithoutCapture match {
            case Failure(errorMsg) => Failure(errorMsg)
            case Success(_) =>
              generatePawnMovement(Position.of(from.col, from.row + forwardRowMod + forwardRowMod))
          }
        case _ =>
          Failure("Pawn's movement: can't move two position forward, because it's not located in the starting row")
      }
    val leftAttack =
      generatePawnAttack(Position.of(from.col - 1, from.row + forwardRowMod))
    val rightAttack =
      generatePawnAttack(Position.of(from.col + 1, from.row + forwardRowMod))
    List(moveOnePosWithoutCapture, moveTwoPosWithoutCapture, leftAttack, rightAttack)
      .filter(_.toOption.nonEmpty)
      .map(_.toOption.get)
  }

  private def generatePawnMovement(to: Option[Position]): Validation[String, ValidSimpleMove] =
    to match {
      case None => Failure("Pawn's movement: the end position doesn't exist in the board")
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None    => Success(ValidSimpleMove(pieceType, color, from, to, None))
          case Some(_) => Failure("Pawn's movement: can't pass over pieces")
        }
    }

  private def generatePawnAttack(to: Option[Position]): Validation[String, ValidSimpleMove] =
    to match {
      case None => Failure("Pawn's attack: the end position doesn't exist in the board")
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None => Failure("Pawn's attack: there's no piece in the end position")
          case Some(pieceToCapture) =>
            pieceToCapture.color match {
              case color.other => Success(ValidSimpleMove(pieceType, color, from, to, Some(pieceToCapture)))
              case _           => Failure("Pawn's attack: can't attack an ally piece")
            }
        }
    }

}
