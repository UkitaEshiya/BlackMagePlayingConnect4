

let width = 7;

let height = 6;

let maxDepth = 7;

let orangeWins = 1000000;

let yellowWins = - orangeWins;

let debug = ref(true);

/* July 28, 2012:
 *
 * I deviated from the blog post now (if you want to, checkout
 * earlier versions from GitHub to see the blog post version).
 *
 * Ocaml unfortunately doesn't support integer-based enums (like F#/C++).
 * I therefore removed this...
 *
 * type mycell =
 *     | Orange
 *     | Yellow
 *     | Barren
 *
 * ...and replaced it with direct usage of 1,0,-1
 * (wherever Orange,Barren,Yellow were used)
 *
 * This provided a speedup of 60% - just as integer-enums did for F# -
 * since there's no "mapping" required in scoreBoard anymore.
 */
let (|>) = (x, fn) => fn(x);

let inside = (y, x) => y >= 0 && y < height && x >= 0 && x < width;

let otherColor = (color) => - color;

let scoreBoard = (board) => {
  let counts = [|0, 0, 0, 0, 0, 0, 0, 0, 0|];
  let myincr = (arr, idx) => arr[idx] = arr[idx] + 1;
  /* Horizontal spans */
  for (y in 0 to height - 1) {
    let score = ref(board[y][0] + board[y][1] + board[y][2]);
    for (x in 3 to width - 1) {
      score := score^ + board[y][x];
      myincr(counts, score^ + 4);
      score := score^ - board[y][x - 3];
    };
  };
  /* Vertical spans */
  for (x in 0 to width - 1) {
    let score = ref(board[0][x] + board[1][x] + board[2][x]);
    for (y in 3 to height - 1) {
      score := score^ + board[y][x];
      myincr(counts, score^ + 4);
      score := score^ - board[y - 3][x];
    };
  };
  /* Down-right (and up-left) diagonals */
  for (y in 0 to height - 4) {
    for (x in 0 to width - 4) {
      let score = ref(0);
      for (idx in 0 to 3) {
        score := score^ + board[y + idx][x + idx];
      };
      myincr(counts, score^ + 4);
    };
  };
  /* up-right (and down-left) diagonals */
  for (y in 3 to height - 1) {
    for (x in 0 to width - 4) {
      let score = ref(0);
      for (idx in 0 to 3) {
        score := score^ + board[y - idx][x + idx];
      };
      myincr(counts, score^ + 4);
    };
  };
  /*
   For down-right and up-left diagonals, I also tried this incremental version
   of the diagonal scores calculations... It is doing less computation than
   the alternative above, but unfortunately, the use of the two tuple lists
   makes the overall results worse in my Celeron E3400... I suspect
   because the access to the list triggers cache misses.
   Outside, in global space:
       (* anchors to start calculating scores while moving down right *)
       let dr = [| (2,0);(1,0);(0,0);(0,1);(0,2);(0,3) |]
       (* anchors to start calculating scores while moving down left *)
       let dl = [| (0,3);(0,4);(0,5);(0,6);(1,6);(2,6) |]
   And in this function, using the anchors to do the calculation incrementally,
   just as we do for vertical and horizontal spaces:
       (* Down-right (and up-left) diagonals *)
       for idx=0 to 5 do
           let (yinit, xinit) = dr.(idx) in
           let y = ref yinit in
           let x = ref xinit in
           let score = ref (board.(!y).(!x) + board.(!y + 1).(!x + 1) + board.(!y + 2).(!x + 2)) in
           while !y+3<=height-1 && !x+3<=width-1 do
               score := !score + board.(!y+3).(!x+3) ;
               myincr counts (!score+4) ;
               score := !score - board.(!y).(!x) ;
               y := !y+1 ;
               x := !x+1 ;
           done
       done ;
       (* Down-left (and up-right) diagonals *)
       for idx=0 to 5 do
           let (yinit, xinit) = dl.(idx) in
           let y = ref yinit in
           let x = ref xinit in
           let score = ref (board.(!y).(!x) + board.(!y + 1).(!x - 1) + board.(!y + 2).(!x - 2)) in
           while !y+3<=height-1 && !x-3>=0 do
               score := !score + board.(!y+3).(!x-3) ;
               myincr counts (!score+4) ;
               score := !score - board.(!y).(!x) ;
               y := !y+1 ;
               x := !x-1 ;
           done
       done ;
   */
  if (counts[0] != 0) {
    yellowWins;
  } else if (counts[8] != 0) {
    orangeWins;
  } else {
    counts[5] + 2 * counts[6] + 5 * counts[7] - counts[3] - 2 * counts[2] - 5 * counts[1];
  };
};


/* This emulates the [X .. Y] construct of F# */
let (--) = (i, j) => {
  let rec aux = (n, acc) =>
    if (n < i) {
      acc;
    } else {
      aux(n - 1, [n, ...acc]);
    };
  aux(j, []);
};

/* This emulates the List.zip of F# */
let myzip = (a, b) => {
  let rec innermyzip = (a, b, accum) =>
    switch a {
    | [] => accum
    | _ =>
      let newList = [(List.hd(a), List.hd(b)), ...accum];
      innermyzip(List.tl(a), List.tl(b), newList);
    };
  List.rev(innermyzip(a, b, []));
};

exception NoMoreWork;

let dropDisk = (board, column, color) => {
  let arrNew = Array.make_matrix(height, width, 0);
  for (y in 0 to height - 1) {
    for (x in 0 to width - 1) {
      arrNew[y][x] = board[y][x];
    };
  };
  try {
    for (y in height - 1 downto 0) {
      if (arrNew[y][column] == 0) {
        arrNew[y][column] = color;
        raise(NoMoreWork);
      };
    };
    arrNew;
  } {
  | NoMoreWork => arrNew
  };
};

let rec abMinimax = (maximizeOrMinimize, color, depth, board) => {
  let validMoves = 0 -- (width - 1) |> List.filter((column) => board[0][column] == 0);
  switch validMoves {
  | [] => (None, scoreBoard(board))
  | _ =>
    let movesAndBoards =
      validMoves |> List.map((column) => (column, dropDisk(board, column, color)));
    let movesAndScores =
      movesAndBoards |> List.map(((column, board)) => (column, scoreBoard(board)));
    let killerMoves = {
      let targetScore = if (maximizeOrMinimize) {orangeWins} else {yellowWins};
      movesAndScores |> List.filter(((_, score)) => score == targetScore);
    };
    switch killerMoves {
    | [(killerMove, killerScore), ...rest] => (Some(killerMove), killerScore)
    | [] =>
      let bestScores =
        switch depth {
        | 1 => movesAndScores |> List.map(snd)
        | _ =>
          movesAndBoards
          |> List.map(snd)
          |> List.map(abMinimax(! maximizeOrMinimize, otherColor(color), depth - 1))
          /* when loss is certain, avoid forfeiting the match, by shifting scores by depth... */
          |> List.map(
               ((bmove, bscore)) => {
                 let shiftedScore =
                   switch bscore {
                   | 1000000
                   | (-1000000) => bscore - depth * color
                   | _ => bscore
                   };
                 shiftedScore;
               }
             )
        };
      let allData = myzip(validMoves, bestScores);
      if (debug^ && depth == maxDepth) {
        List.iter(
          ((column, score)) =>
            Printf.printf("Depth %d, placing on %d, Score:%d\n%!", depth, column, score),
          allData
        );
      };
      let best = ((_, s) as l, (_, s') as r) =>
        if (s > s') {
          l;
        } else {
          r;
        }
      and worst = ((_, s) as l, (_, s') as r) =>
        if (s < s') {
          l;
        } else {
          r;
        };
      let (bestMove, bestScore) =
        List.fold_left(
          if (maximizeOrMinimize) {best} else {worst},
          List.hd(allData),
          List.tl(allData)
        );
      (Some(bestMove), bestScore);
    };
  };
};

/* let any = List.fold_left (||) false
 * ..is slower than ... */
let rec any = (l) =>
  switch l {
  | [] => false
  | [true, ...xs] => true
  | [false, ...xs] => any(xs)
  };

let inArgs = (args, str) => any(Array.to_list(Array.map((x) => x == str, args)));

let loadBoard = (args) => {
  let board = Array.make_matrix(height, width, 0);
  for (y in 0 to height - 1) {
    for (x in 0 to width - 1) {
      let orange = Printf.sprintf("o%d%d", y, x);
      let yellow = Printf.sprintf("y%d%d", y, x);
      if (inArgs(args, orange)) {
        board[y][x] = 1;
      } else if (inArgs(args, yellow)) {
        board[y][x] = (-1);
      } else {
        board[y][x] = 0;
      };
    };
  };
  board;
};

{
  let board = loadBoard(Sys.argv);
  let scoreOrig = scoreBoard(board);
  debug := inArgs(Sys.argv, "-debug");
  if (debug^) {
    Printf.printf("Starting score: %d\n", scoreOrig);
  };
  if (scoreOrig == orangeWins) {
    Printf.printf("I win\n");
    (-1);
  } else if (scoreOrig == yellowWins) {
    Printf.printf("You win\n");
    (-1);
  } else {
    let (mv, score) = abMinimax(true, 1, maxDepth, board);
    switch mv {
    | Some(column) =>
      Printf.printf("%d\n", column);
      0;
    | _ => failwith("No move possible")
    };
  };
};
/* vim: set expandtab ts=8 sts=4 shiftwidth=4 */
