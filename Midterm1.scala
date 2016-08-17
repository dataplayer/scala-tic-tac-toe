package main

import scala.annotation.tailrec

object Main {
  case class Row[A](top: A, middle: A, bottom: A)
  case class Col[A](left: Row[A], middle: Row[A], right: Row[A])

  case class Board(matrix: Col[Option[PlayerChoice]])

  sealed trait PlayerChoice
  case object Xs extends PlayerChoice
  case object Os extends PlayerChoice

  case class GameState(board: Board)

  sealed trait UserError
  case object InvalidMove extends UserError
  case object GameOver extends UserError
  case object InvalidInput extends UserError
  
  sealed trait UserInput
  case class Symbol(sign: PlayerChoice) extends UserInput
  case class Move(b: Board, s: PlayerChoice) extends UserInput
  //case object InvalidInput extends UserInput
  
  def combineRowEntries(a: Option[PlayerChoice], b: Option[PlayerChoice]): Either[UserError,Option[PlayerChoice]] = {
    (a,b) match {
      case (Some(a),Some(b)) => Left(InvalidMove)
      case (Some(a),None)    => Right(Some(a))
      case (None,Some(b))    => Right(Some(b))
      case (None,None)       => Right(None)
    }
  }
  
  def addRows(r1: Row[Option[PlayerChoice]], r2: Row[Option[PlayerChoice]]): Either[UserError,Row[Option[PlayerChoice]]] = {
    val (t1,m1,b1) = r1 match {
      case Row(a,b,c) => (a,b,c)
    }
    val (t2,m2,b2) = r2 match {
      case Row(a,b,c) => (a,b,c)
    }
    val t3 = combineRowEntries(t1,t2)
    val m3 = combineRowEntries(m1,m2)
    val b3 = combineRowEntries(b1,b2)
    (t3,m3,b3) match {
      case (Right(a),Right(b),Right(c)) => Right(Row(a,b,c))
      case (Left(a),_,_) => Left(InvalidMove)
      case (_,Left(b),_) => Left(InvalidMove)
      case (_,_,Left(c)) => Left(InvalidMove)
    }
  }
  
  def addBoards(b1: Board, b2: Board, p1: PlayerChoice): Either[UserError,GameState] = {
    val p2: PlayerChoice = {
      p1 match {
        case Xs => Os
        case Os => Xs
      }
    }
    (b1,b2) match {
      case (Board(a),Board(b)) => (a,b) match {
        case (Col(a1,a2,a3),Col(b1,b2,b3)) => {
          val n1 = addRows(a1,b1)
          val n2 = addRows(a2,b2)
          val n3 = addRows(a3,b3)
          (n1,n2,n3) match {
            case (Right(a),Right(b),Right(c)) => Right(GameState(Board(Col(a,b,c))))
            case (Left(a),_,_) => Left(InvalidMove)
            case (_,Left(b),_) => Left(InvalidMove)
            case (_,_,Left(c)) => Left(InvalidMove)
          }
        }
      }
    }
    
  }
  
  
  def update(userInput: UserInput, state: GameState): Either[UserError, GameState] = {
    (userInput,state) match {
      case (Move(b1,userSymbol),GameState(b2)) => addBoards(b1,b2,userSymbol)
      case (_,_) => Left(InvalidMove)
    }
  }

  
  def getPlayerSymbol(symbol: String): Option[PlayerChoice] = {
    symbol match {
      case "x" => Some(Xs)
      case "o" => Some(Os)
      case _ => Some(Xs)
    }
  }
  
  def getInput = {
    val symbol = scala.io.StdIn.readLine("enter sign: ").toLowerCase
    val userSymbol = getPlayerSymbol(symbol)
    val row = scala.io.StdIn.readLine("enter row: ").toLowerCase
    val col = scala.io.StdIn.readLine("enter col: ").toLowerCase
    val emptyRow = Row[Option[PlayerChoice]](None,None,None)
    val emtpyBoard = Board(Col(emptyRow,emptyRow,emptyRow))
    
    val rowMove = row match {
      case "top"    => Row(userSymbol,None,None)
      case "middle" => Row(None,userSymbol,None)
      case "bottom" => Row(None,None,userSymbol)
      case _ => emptyRow
    }
    
    val newMove = col match {
      case "left"     => Board(Col(rowMove,emptyRow,emptyRow))
      case "middle"   => Board(Col(emptyRow,rowMove,emptyRow))
      case "right"    => Board(Col(emptyRow,emptyRow,rowMove))
      case _ => Board(Col(emptyRow,emptyRow,emptyRow))
    }

    Move(newMove,userSymbol.get)
  }

  def showError(err: UserError): Unit = {
    
  }

  def printBoard(state: GameState): Unit = {
    println("current state: " + state.toString())
  }

  @tailrec
  def loop(state: GameState): Unit = {
    val userInput = getInput
    update(userInput, state) match {
      case Left(error) => showError(error); loop(state)
      case Right(state) => printBoard(state); loop(state)
    }
  }

  def main(args: Array[String]): Unit = {
    val emptyRow = Row[Option[PlayerChoice]](None,None,None)
    val initialState = GameState(Board(Col(emptyRow,emptyRow,emptyRow)))
    printBoard(initialState)
    loop(initialState)
  }
}