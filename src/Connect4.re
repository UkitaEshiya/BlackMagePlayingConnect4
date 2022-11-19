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

  let makeBoard: state => matrix = s => 
  switch(s) {
  |State(_, [hd, ...tl]) => [hd, ...tl]
  |_ => failwith("other")
  }; 


  let rec mainDiagonal: matrix => list(int) = 
    ma => switch(ma) {
    |[] => []
    |[hd, ...tl] => [List.hd(hd), ...mainDiagonal(List.map(List.tl, tl))]
    }

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
  
  let checkNthSlot: (int, list(int)) => bool = 
      (num, lst) =>
       if (List.nth(lst, num) == 0){
        true
      } else {
        false 
      }; 

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
        List.map(makeMove, makeMoveList(hd))
      | State(_, []) => failwith("invalid game state")
      };

  checkExpect(legalMoves(State(Ongoing(P1), [[0, 0, 0, 0, 0], [1, 2, 3, 4, 5]])), 
              [Move(5), Move(4), Move(3), Move(2), Move(1)], 
              "legalMoves all column open")
/*
    let legalMoves: state => list(move) =
    inState =>
      switch (inState) {
      | State(_, [hd, ...tl]) => 
        if(List.mem(0, List.hd(List.rev([hd, ...tl])))) {
         [Move(1)]
        } else {
          failwith ("board filled, game end in a tie")
        }
      | State(_, []) => failwith("invalid game state")
      };

  let nextState: (state, move) => state =
    (inState, inMove) =>
      switch (inState, inMove) {
      | (State(Win(_), _), _)
      | (State(Draw, _), _) => inState
      | (State(Ongoing(player), x), Move(1)) =>
        State(otherPlayer(player), (x - 1))
      | (State(Ongoing(player), x), Move(2)) =>
        State(otherPlayer(player), (x - 2))     
      | (State(Ongoing(player), x), Move(3)) =>
        State(otherPlayer(player), (x - 3))
      };*/

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


};
/*
module MyGame : Game = Connect4;
open Connect4;*/

/* test cases */

