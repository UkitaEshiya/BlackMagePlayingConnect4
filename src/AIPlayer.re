/*open! CS17SetupGame;
open Game; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
 /* TODO */
  let nextMove: (PlayerGame.state => PlayerGame.move) = s => {
    failwith("not yet implemented")
  }
  
  /* put your team name here! */
  let playerName = "Windup Black Mage *batteries not included";
  
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

*/