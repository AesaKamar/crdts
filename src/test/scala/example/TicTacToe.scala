package example

object TicTacToe {


  // Implicit search!

  trait Exists

  val          iExist              : Exists = new Exists {}
  implicit val iExistToTheCompiler : Exists = new Exists {}



  implicitly[Exists] // ⌘⇧P








  def summon[A](implicit evidenceIHaveExactlyOneA: A): A =
    evidenceIHaveExactlyOneA

  summon[Exists]






















  // TODO
  // Describe how to:
  //  show who the winner of a Game of TicTacToe is to your compiler

  trait Player
  trait X extends Player
  trait O extends Player

  sealed trait Winner[P <: Player]




















//    implicit val aWinnerIsX = new Winner[X] {}













  // TODO
  // Make some types to hold onto information about:
  //  Where a player made a move


  sealed trait PlayerMove







  trait `↖`[A <: Player] extends PlayerMove; trait `↑`[A <: Player] extends PlayerMove; trait `↗`[A <: Player]extends PlayerMove;

  trait `←`[A <: Player] extends PlayerMove; trait `△`[A <: Player] extends PlayerMove; trait `→`[A <: Player]extends PlayerMove;

  trait `↙`[A <: Player] extends PlayerMove; trait `↓`[A <: Player] extends PlayerMove; trait `↘`[A <: Player]extends PlayerMove;




  // TODO
  // Find a way to logically connect a bunch of PlayerMoves with a Winner?












  // TODO
  // Find a way to logically connect a bunch of PlayerMoves with a Winner?
  //  Give some examples of ways to win

  sealed trait ThreeConnect[A <: Player]
  trait DiagonalConnect    [A <: Player] extends ThreeConnect[A]
  trait HorizontalConnect  [A <: Player] extends ThreeConnect[A]
  trait VerticalConnect    [A <: Player] extends ThreeConnect[A]
















  // TODO
  // Make one of the win conditions imply the Winner
  //
  // If I find evidence of one,
  //   should imply evidence of the other
//  implicit val xConnected3Diagonally : ThreeConnect[X] = new DiagonalConnect[X] {}


  implicit def ifThreeConnectThenWinner[P <: Player](implicit evidence: ThreeConnect[P]) : Winner[P] =
    new Winner[P] {}


  // ^^^^^^^^^^^^^^^^^^^^^^ TYPECHECK
















//    implicit def youWinIf3Connect[xo](implicit ev: ThreeConnect[xo]): Winner[xo] = ???







  // These two are isomorphic!
  //  implicit def youWinIf3Connect[xo <: Player : ThreeConnect]: Winner[xo] = ???







  // TODO
  // Make Bottom 3 imply horizontal connection
  //  HorizontalConnect => ThreeConnect => Winner[You!]
  //
  // `↖`[_] `↑`[_] `↗`[_]
  // `←`[_] `△`[_] `→`[_]
  // `↙`[X] `↓`[X] `↘`[X]









  // ^^^^^^^^^^^^^^^^^^^^^^^ TYPECHECK













  // How do we make an example of a Diagonal Connect?
    implicit def `becauseConnect⋱`[xo <: Player: `↖`: `△`: `↘`]: DiagonalConnect[xo] = ???
    implicit def `becauseConnect⋰`[xo <: Player: `↙`: `△`: `↗`]: DiagonalConnect[xo] = ???
  //
    implicit def `becauseConnect⠇  `[xo <: Player: `↖`: `←`: `↙`]: VerticalConnect[xo] = ???
    implicit def `becauseConnect ⠇ `[xo <: Player: `↑`: `△`: `↓`]: VerticalConnect[xo] = ???
    implicit def `becauseConnect  ⠇`[xo <: Player: `↗`: `→`: `↘`]: VerticalConnect[xo] = ???
  //
    implicit def `becauseConnect˙ ˙ ˙`[xo <: Player: `↖`: `↑`: `↗`]: HorizontalConnect[xo] = ???
    implicit def `becauseConnect・・・`[xo <: Player: `←`: `△`: `→`]: HorizontalConnect[xo] = ???
    implicit def `becauseConnect. . .`[xo <: Player: `↙`: `↓`: `↘`]: HorizontalConnect[xo] = ???




  //  // Let's Typecheck if we won yet?
  //
  implicit val mx1 = new `↖`[X] {}
  implicit val mx2 =   new `△`[X] {}
  implicit val mx3 =     new `↘`[X] {}




//  implicit val m4 = new `↖`[O] {}
//  implicit val m5 = new   `△`[O] {}
//  implicit val m6 = new     `↘`[O] {}

  // TYPE CHECKING HAPPENS HERE vvv
  implicitly[Winner[_]]
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


  implicitly[Winner[X]]
//  implicitly[Winner[O]]
  //---------------
  //- No overlaps
  //---------------
}
