       IDENTIFICATION DIVISION.
       PROGRAM-ID. TicTacToe.
       AUTHOR. ChatGPT with many fixes and comments by Cassandra Leder

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  GameBoard.
           05  Row1      PIC X(3) VALUE "   ".
           05  Row2      PIC X(3) VALUE "   ".
           05  Row3      PIC X(3) VALUE "   ".
       01  Player1      PIC X(1) VALUE "X".
       01  Player2      PIC X(1) VALUE "O".
       01  CurrentPlayer PIC X(1).
      * Move is a reserved word in GNUCOBOL, use xMove
       01  xMove         PIC 9.
       01  Winner       PIC X(1) VALUE SPACE.
       01  IsTaken      PIC X(3) VALUE "NO".
       01  GameOver     PIC X(3) VALUE "NO".
       01  TurnCount    PIC 9(2) VALUE 0.
       01  Win          PIC X(3) VALUE "NO".
       01  Elapsed-Time PIC 9(8) VALUE 0.
       01  Total-Time   PIC 9(8) VALUE 0.
      * define current date array
       01  CURRENT-DATE-DATA-START.
           05 WS-CURRENT-DATE-START.
              10 CURRENT-YEAR   	  PIC 9(4).
              10 CURRENT-MONTH  	  PIC 9(2).
              10 CURRENT-DAY    	  PIC 9(2).
           05 CURRENT-TIME-START.
              10 CURRENT-HOUR   	  PIC 9(2).
              10 CURRENT-MIN    	  PIC 9(2).
              10 CURRENT-SEC    	  PIC 9(2).
              10 CURRENT-MS-START     PIC 9(2).
           05 GMT-DIFF          	  PIC S9(4).
       01  CURRENT-DATE-DATA-END.
           05 WS-CURRENT-DATE-END.
              10 CURRENT-YEAR   	  PIC 9(4).
              10 CURRENT-MONTH  	  PIC 9(2).
              10 CURRENT-DAY    	  PIC 9(2).
           05 CURRENT-TIME-END.
              10 CURRENT-HOUR   	  PIC 9(2).
              10 CURRENT-MIN          PIC 9(2).
              10 CURRENT-SEC          PIC 9(2).
              10 CURRENT-MS-END       PIC 9(2).
           05 GMT-DIFF                PIC S9(4).
       PROCEDURE DIVISION.
      *> main function is here.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GAME
      *> example of a while-style loop
           PERFORM UNTIL GameOver = "YES"
               PERFORM DISPLAY-BOARD
               PERFORM SWITCH-PLAYER
               PERFORM GET-MOVE
               PERFORM TIME-GAME-START
               PERFORM VALIDATE-MOVE
               PERFORM UPDATE-BOARD
               PERFORM CHECK-WINNER
               PERFORM TIME-GAME-END
               PERFORM TIME-GAME-ELAPSE
               PERFORM TIME-GAME-TOTAL
           END-PERFORM
           PERFORM DISPLAY-BOARD
           PERFORM DISPLAY-WINNER
           DISPLAY Total-Time
           STOP RUN.

      *> set default values
       INITIALIZE-GAME.
           MOVE Player1 TO CurrentPlayer.
           MOVE 0 TO TurnCount.

      *> print current board
       DISPLAY-BOARD.
           DISPLAY "Current Board:"
           DISPLAY "Row 1:", Row1
           DISPLAY "Row 2:", Row2
           DISPLAY "Row 3:", Row3.

      *> time how long it takes to perform key parts of game
      * get current time
       TIME-GAME-START.
            MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-DATA-START.
			DISPLAY CURRENT-MS-START.
       TIME-GAME-END.
		    MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-DATA-END.
			DISPLAY CURRENT-MS-END.
       TIME-GAME-ELAPSE.
            COMPUTE Elapsed-Time = CURRENT-MS-END - CURRENT-MS-START.
       TIME-GAME-TOTAL.
            COMPUTE Total-Time = Total-Time + Elapsed-Time.


      * Shows COBOL syntax whereby '.' can be used instead of END-IF
       SWITCH-PLAYER.
           IF CurrentPlayer = Player1 THEN
               MOVE Player2 TO CurrentPlayer
           ELSE
               MOVE Player1 TO CurrentPlayer.

       GET-MOVE.
           DISPLAY "Player " CurrentPlayer ", enter your move (1-9): "
           ACCEPT xMove.

      * I cannot figure out how continuation lines work
      * for huge if statements, so I used else if chain...not ideal
       VALIDATE-MOVE.
           IF xMove < 1 OR xMove > 9 THEN 
            EVALUATE True
                WHEN Row1(1:3) = "X" OR Row1(1:3) = "O"
                    MOVE "YES" TO IsTaken
                WHEN Row2(1:3) = "X" OR Row2(1:3) = "O"
                    MOVE "YES" TO  IsTaken
                WHEN Row3(1:3) = "X" OR Row3(1:3) = "O" 
                    MOVE "YES" TO IsTaken
            END-EVALUATE
           END-IF

           IF IsTaken = "YES" THEN
              DISPLAY "Invalid move, try again"
              PERFORM GET-MOVE
           END-IF.

      * Example of a switch statement in COBOL
      * updates the varibles that represent the board
       UPDATE-BOARD.
           EVALUATE xMove
               WHEN 1 MOVE CurrentPlayer TO Row1(1:1)
               WHEN 2 MOVE CurrentPlayer TO Row1(2:1)
               WHEN 3 MOVE CurrentPlayer TO Row1(3:1)
               WHEN 4 MOVE CurrentPlayer TO Row2(1:1)
               WHEN 5 MOVE CurrentPlayer TO Row2(2:1)
               WHEN 6 MOVE CurrentPlayer TO Row2(3:1)
               WHEN 7 MOVE CurrentPlayer TO Row3(1:1)
               WHEN 8 MOVE CurrentPlayer TO Row3(2:1)
               WHEN 9 MOVE CurrentPlayer TO Row3(3:1)
           END-EVALUATE.
           ADD 1 TO TurnCount.

      * Thanks to how COBOL operates, this procedure is a mess.
       CHECK-WINNER.
      * Check if there's a horizontal, vertical, or diagonol win
        EVALUATE True
      *> when there's a horizontal win
             WHEN Row1(1:1) = CurrentPlayer AND Row1(2:1) =
                      CurrentPlayer AND Row1(3:1) = CurrentPlayer
                      MOVE "YES" TO Win
             WHEN Row2(1:1) = CurrentPlayer AND Row2(2:1) =
                     CurrentPlayer AND Row2(3:1) = CurrentPlayer
                      MOVE "YES" TO Win
		     WHEN Row3(1:1) = CurrentPlayer AND Row3(2:1) =
                      CurrentPlayer AND Row3(3:1) = CurrentPlayer
                      MOVE "YES" TO Win
      *> diagonal win
             WHEN Row1(1:1) = CurrentPlayer AND Row2(2:1) =
                      CurrentPlayer AND Row3(3:1) = CurrentPlayer
                      MOVE "YES" TO Win
		     WHEN Row1(3:1) = CurrentPlayer AND Row2(2:1) =
                      CurrentPlayer AND Row3(1:1) = CurrentPlayer
                      MOVE "YES" TO Win
      *> vertical 
             WHEN Row1(1:1) = CurrentPlayer AND Row2(1:1) =
                      CurrentPlayer AND Row3(1:1) = CurrentPlayer
                      MOVE "YES" TO Win
             WHEN Row1(2:1) = CurrentPlayer AND Row2(2:1) =
                      CurrentPlayer AND Row3(2:1) = CurrentPlayer
                      MOVE "YES" TO Win
             WHEN Row1(3:1) = CurrentPlayer AND Row2(3:1) =
                      CurrentPlayer AND Row3(3:1) = CurrentPlayer
                      MOVE "YES" TO Win
           END-EVALUATE

           IF Win = "YES" THEN
                MOVE CurrentPlayer TO Winner
                MOVE "YES" TO GameOver
           END-IF.

      *> output winner of the game 
       DISPLAY-WINNER.
           IF Winner = SPACE THEN
               DISPLAY "It's a draw!"
           ELSE
               DISPLAY "Player " Winner " wins!".
           
        END PROGRAM TicTacToe.
