open CS17SetupGame;
open Game; 

module Connect4 = {

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

type matrix = list(list(int));

type state =
    | State(status, matrix);

let rec makeList: (int, 'a) => list('a) = (num, item) => 
  switch(num){
  | 0 => []
  | n => [item, ...makeList(n-1, item)]
  }; 

let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims); //int
        let boardWidth = getBoardWidth(boardDims); //int
        State(Ongoing(P1), makeList(boardHeight, (makeList(boardWidth, 0))))
      }; 

  let gameStatus: state => status =
    inState => {
      let State(p, _) = inState;
      p;
    };

  type move =
    | Move(int);

  let otherPlayer = (player: whichPlayer): whichPlayer =>
    switch (player) {
    | P1 => P2
    | P2 => P1
    };

  let currentPlayer: state => whichPlayer =
    inState =>
      switch (inState) {
      | State(Draw, _) => failwith("")
      | State(Ongoing(p), _)
      | State(Win(p), _) => p
      };
  
  let makeMove: int => move = num => Move(num); 

/* generates the columns that are still unfilled (column numbed from right to 
  left, starting from 0)
  I/P: the first row of the board, where 0 indicates empty space 
  O/P: a list indicating the index of columns that are empty
*/
let rec makeMoveList: list(int) => list(int) = 
  lst => switch(lst){
    |[] => []
    |[hd, ...tl] => 
    if (hd == 0) {
      [List.length([hd, ...tl]), ...makeMoveList(tl)]
    } else {
      makeMoveList(tl)
    }
  }; 

checkExpect(makeMoveList([0, 1, 2, 0, 0]), 
            [5, 2, 1], 
            "check makeMoveList")

checkExpect(makeMoveList([0, 0, 0, 0, 0]), 
            [5, 4, 3, 2, 1], 
            "check makeMoveList all column open")

  let legalMoves: state => list(move) =
    inState =>
      switch (inState) {
      | State(_, [hd, ..._]) => 
        List.rev_map(makeMove, makeMoveList(hd))
      | State(_, []) => failwith("invalid game state")
      };

  checkExpect(legalMoves(State(Ongoing(P1), [[0, 0, 0, 0, 0], [1, 2, 3, 4, 5]])), 
              [Move(1), Move(2), Move(3), Move(4), Move(5)], 
              "legalMoves all column open")

//return the open row that can have a piece placed in; bottom row is row 0
  let rec checkOpenRow: (matrix, int) => int = (board, column) => 
    switch(board){
    |[hd, ...tl] => if (List.nth(hd, column) == 0){
      List.length(board)
    } else {
      checkOpenRow(tl, column)
    }; 
    |[] => failwith("checkOpenRow cannot find open row to drop piece")
    }; 
  
/* procedure to drop piece in board not yet implemented
  let dropPiece: (matrix, int, int) => matrix = (board, row, column) =>
    switch(List.nth(List.rev(board), row), column) => 1;
*/

  let nextState: (state, move) => state =
    (inState, inMove) =>
      switch (inState, inMove) {
      | (State(Win(_), _), _)
      | (State(Draw, _), _) => inState
      | (State(Ongoing(player), mat), Move(x)) => 
        if (player == P1) {
          failwith ("make the corresponding tile 1")
        } else {
          failwith("make the corresponding tile -1")
        }
      };

  let estimateValue: state => float =
    inState =>
      switch (inState) {
      | State(Win(P1), _) => 1.
      | State(Win(P2), _) => (-1.)
      | State(_, _) => 0.
      };

  let stringOfPlayer: whichPlayer => string =
    p =>
      switch (p) {
      | P1 => "P1"
      | P2 => "P2"
      };

  let stringOfState: state => string =
    inState =>
      switch (inState) {
      | State(Win(p), _) => stringOfPlayer(p) ++ " wins!"
      | State(Ongoing(p), _) => "It is " ++ stringOfPlayer(p) ++ "'s turn."
      | State(Draw, _) => "The game ends in a draw!"
      }; 

  let stringOfMove: move => string =
    inMove => {
      switch(inMove) {
        | Move(x) => string_of_int(x) 
      }
    };

     let moveOfString: (string, state) => move =
    (str, _) =>
      switch (str) {
      | _ => try(Move(int_of_string(str))) {
      | Failure(msg) => {
        failwith("please pick an integer corresponding to the column you want to 
                    drop your piece! (0~6)")
        };
      };}

//misc procedures that might come in handy

let rec mainDiagonal: matrix => list(int) = 
    ma => switch(ma) {
    |[] => []
    |[hd, ...tl] => [List.hd(hd), ...mainDiagonal(List.map(List.tl, tl))]
    }

};


/*
module MyGame : Game = Connect4;
open Connect4;*/

/* test cases */

