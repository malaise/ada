-- The solitaire game, The Devil, for ANSI-standard displays
-- Demonstrates the use of PragmARC.Deck_Handler & PragmARC.US_card
--
with Ada.Text_Io;
with PragmARC.Ansi_Tty_Control;
with PragmARC.Us_Card;
with PragmARC.Us_Deck;
with PragmARC.Get_Line;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

use Ada;
use Ada.Characters;
use PragmARC;
procedure Devil is
   type Game_Result is (Quit, Again);

   procedure Prepare_Screen is
      -- null;
   begin -- Prepare_Screen
      Text_Io.Put (Ansi_Tty_Control.Clear_Screen);
      Text_Io.Put (Ansi_Tty_Control.Bold_Mode);
      Text_Io.Put ("Devil");
      Text_Io.Put (Ansi_Tty_Control.Normal_Mode);
      Text_Io.Put (Ansi_Tty_Control.Position (2, 6) );
      Text_Io.Put ("1  2  3  4");
      Text_Io.Put (Ansi_Tty_Control.Position (19, 1) );
      Text_Io.Put_Line ("Q    Quit Devil                         A      Deal Again");
      Text_Io.Put_Line ("<CR> Turn over three cards              PzFy   Move card pile Z => foundation Y");
      Text_Io.Put_Line ("HF#  Move card hand => foundation #     PxPw   Move pile X onto pile W");
      Text_Io.Put_Line ("HP#  Move card hand => pile #           FzPy   Move card foundation Z => pile Y");
      Text_Io.Put_Line ("SF#  Move card stock => foundation #    CnPzPy Move N cards pile Z => pile Y");
      Text_Io.Put_Line ("SP#  Move card stock => pile #");
   end Prepare_Screen;

   function Play_Game return Game_Result is
      subtype Index_Value is Integer range 1 .. 4;

      subtype Deck_52 is Us_Deck.Deck_52;

      type Stack_Set is array (Index_Value) of Deck_52;

      Foundation : Stack_Set;
      Stack : Stack_Set;
      Stock : Deck_52;
      Exposed : Deck_52;
      Hidden : Deck_52;
      Card : Us_Card.Card_Handle;
      Spare : Deck_52;
      Count : Positive;
      Index_From : Index_Value;
      Index_To : Index_Value;

      Rule_Violation : exception;
      Invalid_Command : exception;

      function "=" (Left : Us_Card.Rank_Id; Right : Us_Card.Rank_Id) return Boolean renames Us_Card."=";
      function "=" (Left : Us_Card.Suit_Id; Right : Us_Card.Suit_Id) return Boolean renames Us_Card."=";

      function Upper (Char : Character) return Character renames Characters.Handling.To_Upper;

      procedure Put (Card : in Us_Card.Card_Handle) is
         -- null;
      begin -- Put
         case Us_Card.Suit (Card) is
         when Us_Card.Diamond | Us_Card.Heart =>
            Text_Io.Put (Ansi_Tty_Control.Reverse_Video);
         when others =>
            null;
         end case;

         Text_Io.Put (Item => Us_Card.Image (Card) );
         Text_Io.Put (Ansi_Tty_Control.Normal_Mode);
      end Put;

      procedure Display (Foundation : in Stack_Set; Stack : in Stack_Set; Stock : in Deck_52; Exposed : in Deck_52) is
         package Integer_Io is new Text_Io.Integer_Io (Num => Integer);
      begin -- Display
         Text_Io.Put (Ansi_Tty_Control.Position (5, 1) );
         Clear_All : for Line in 5 .. 16 loop
            Text_Io.Put_Line (Ansi_Tty_Control.Clear_End_Of_Line);
         end loop Clear_All;
         
         Text_Io.Put (Ansi_Tty_Control.Clear_End_Of_Line);

         Text_Io.Put (Ansi_Tty_Control.Position (3, 5) );
         
         All_Foundations : for F in Foundation'range loop
            if Us_Deck.Deck.Is_Empty (Foundation (F) ) then
               Text_Io.Put ("   ");
            else
               Put (Card => Us_Deck.Deck.Value (Foundation (F), Us_Deck.Deck.Size (Foundation (F) ) ) );
               Text_Io.Put (' ');
            end if;
         end loop All_Foundations;

         Text_Io.Put (Ansi_Tty_Control.Position (5, 1) );
         
         Stock_Cards : for S in 1 .. Us_Deck.Deck.Size (Stock) loop
            Put (Card => Us_Deck.Deck.Value (Stock, S) );
            Text_Io.New_Line;
         end loop Stock_Cards;

         All_Stacks : for S in Stack'range loop
            All_Cards : for C in 1 .. Us_Deck.Deck.Size (Stack (S) ) loop
               Text_Io.Put (Ansi_Tty_Control.Position (C + 4, 5 + 3 * (S - 1) ) );
               Put (Card => Us_Deck.Deck.Value (Stack (S), C) );
            end loop All_Cards;
         end loop All_Stacks;

         Text_Io.Put (Ansi_Tty_Control.Position (5, 25) );
         Text_Io.Put ("Hand (");
         Integer_Io.Put (Us_Deck.Deck.Size (Exposed), Width => 2);
         Text_Io.Put ("):  ");

         if Us_Deck.Deck.Size (Exposed) >= 3 then
            Put (Card => Us_Deck.Deck.Value (Exposed, Us_Deck.Deck.Size (Exposed) - 2) );
            Text_Io.Put (' ');
         end if;

         if Us_Deck.Deck.Size (Exposed) >= 2 then
            Put (Card => Us_Deck.Deck.Value (Exposed, Us_Deck.Deck.Size (Exposed) - 1) );
            Text_Io.Put (' ');
         end if;

         if Us_Deck.Deck.Size (Exposed) >= 1 then
            Put (Card => Us_Deck.Deck.Value (Exposed, Us_Deck.Deck.Size (Exposed) ) );
         end if;

         Text_Io.Put (Ansi_Tty_Control.Position (7, 25) );
         Text_Io.Put ("Command:  ");
      end Display;

      procedure Check_To_Foundation (From_Card : in Us_Card.Card_Handle; Index_To : in Index_Value) is
         To_Card : Us_Card.Card_Handle;
      begin -- Check_To_Foundation
         if Us_Deck.Deck.Is_Empty (Foundation (Index_To) ) then
            if Us_Card.Rank (From_Card) /= Us_Card.Rank (Us_Deck.Deck.Value (Foundation (1), 1) ) then
               raise Rule_Violation;
            end if;

            return;
         end if;

         To_Card := Us_Deck.Deck.Value (Foundation (Index_To), Us_Deck.Deck.Size (Foundation (Index_To) ));

         if Us_Card.Suit (From_Card) /= Us_Card.Suit (To_Card) then
            raise Rule_Violation;
         end if;

         if Us_Card.Rank (To_Card) = Us_Card.Rank_Id'Last then
            if Us_Card.Rank (From_Card) /= Us_Card.Rank_Id'First then
               raise Rule_Violation;
            end if;
         elsif Us_Card.Rank (From_Card) /= Us_Card.Rank_Id'Succ (Us_Card.Rank (To_Card) ) then
            raise Rule_Violation;
         else
            null;
         end if;
      end Check_To_Foundation;

      procedure Check_To_Stack (From_Card : in Us_Card.Card_Handle; Index_To : in Index_Value) is
         To_Card : Us_Card.Card_Handle;
      begin -- Check_To_Stack
         if Us_Deck.Deck.Is_Empty (Stack (Index_To) ) then
            return;
         end if;

         To_Card := Us_Deck.Deck.Value (Stack (Index_To), Us_Deck.Deck.Size (Stack (Index_To) ));

         if ( (Us_Card.Suit (From_Card) = Us_Card.Diamond or else Us_Card.Suit (From_Card) = Us_Card.Heart) and then
              (Us_Card.Suit (To_Card) = Us_Card.Diamond or else Us_Card.Suit (To_Card) = Us_Card.Heart)
            ) or else
            ( (Us_Card.Suit (From_Card) = Us_Card.Club or else Us_Card.Suit (From_Card) = Us_Card.Spade) and then
              (Us_Card.Suit (To_Card) = Us_Card.Club or else Us_Card.Suit (To_Card) = Us_Card.Spade)
            )
         then
            raise Rule_Violation;
         end if;

         if Us_Card.Rank (To_Card) = Us_Card.Rank_Id'First then
            if Us_Card.Rank (From_Card) /= Us_Card.Rank_Id'Last then
               raise Rule_Violation;
            end if;
         elsif Us_Card.Rank (From_Card) /= Us_Card.Rank_Id'Pred (Us_Card.Rank (To_Card) ) then
            raise Rule_Violation;
         else
            null;
         end if;
      end Check_To_Stack;
   begin -- Play_Game
      Us_Deck.Standard_Deck (Item => Hidden);
      Us_Deck.Deck.Shuffle (Item => Hidden);

      Init_Stacks : for S in Stack'range loop
         Us_Deck.Deck.Deal (From => Hidden, To => Card);
         Us_Deck.Deck.Add (Item => Card, To => Stack (S) );
      end loop Init_Stacks;

      Us_Deck.Deck.Deal (From => Hidden, To => Card);
      Us_Deck.Deck.Add (Item => Card, To => Foundation (1) );

      Fill_Stock : for I in 1 .. 13 loop
         Us_Deck.Deck.Deal (From => Hidden, To => Card);
         Us_Deck.Deck.Add (Item => Card, To => Stock);
      end loop Fill_Stock;

      Text_Io.Put (Ansi_Tty_Control.Position (3, 1) ); -- Display foundations' base rank
      Put (Us_Deck.Deck.Value (Foundation (1), 1) );
      Text_Io.Put (Ansi_Tty_Control.Position (3, 2) & ' ');

      All_Actions : loop
         Display (Foundation => Foundation, Stack => Stack, Stock => Stock, Exposed => Exposed);

         Handle_Command : declare
            Command : constant String := Get_Line;
         begin -- handle_command
            if Command'Length < 1 then
               if Us_Deck.Deck.Is_Empty (Hidden) then
                  Us_Deck.Deck.Assign (To => Hidden, From => Exposed);
                  Us_Deck.Deck.Make_Empty (Item => Exposed);
               end if;

               Turn_3 : for I in 1 .. 3 loop
                  exit Turn_3 when Us_Deck.Deck.Is_Empty (Hidden);

                  Us_Deck.Deck.Deal (From => Hidden, To => Card);
                  Us_Deck.Deck.Add (Item => Card, To =>Exposed);
               end loop Turn_3;
            elsif Upper (Command (1) ) = 'T' then
               if Us_Deck.Deck.Is_Empty (Hidden) then
                  Us_Deck.Deck.Assign (To => Hidden, From => Exposed);
                  Us_Deck.Deck.Make_Empty (Item => Exposed);
               end if;

               if not Us_Deck.Deck.Is_Empty (Hidden) then
                  Us_Deck.Deck.Deal (From => Hidden, To => Card);
                  Us_Deck.Deck.Add (Item => Card, To =>Exposed);
               end if;
            elsif Upper (Command (1) ) = 'Q' then
               return Quit;
            elsif Upper (Command (1) ) = 'A' then
               return Again;
            elsif Command'Length >= 3 and then Upper (Command (1) ) = 'H' then
               Index_To := Integer'Value (Command (3 .. 3) );
               Card := Us_Deck.Deck.Value (Exposed, Us_Deck.Deck.Size (Exposed) );

               if Upper (Command (2) ) = 'F' then
                  Check_To_Foundation (Card, Index_To);
                  Us_Deck.Deck.Remove (From => Exposed, Position => Us_Deck.Deck.Size (Exposed), To => Card);
                  Us_Deck.Deck.Add (Item => Card, To => Foundation (Index_To) );
               elsif Upper (Command (2) ) = 'P' then
                  Check_To_Stack (Card, Index_To);
                  Us_Deck.Deck.Remove (From => Exposed, Position => Us_Deck.Deck.Size (Exposed), To => Card);
                  Us_Deck.Deck.Add (Item => Card, To =>Stack (Index_To) );
               else
                  raise Invalid_Command;
               end if;
            elsif Command'Length >= 3 and then Upper (Command (1) ) = 'S' then
               Index_To := Integer'Value (Command (3 .. 3) );
               Card := Us_Deck.Deck.Value (Stock, Us_Deck.Deck.Size (Stock) );

               if Upper (Command (2) ) = 'F' then
                  Check_To_Foundation (Card, Index_To);
                  Us_Deck.Deck.Remove (From => Stock, Position => Us_Deck.Deck.Size (Stock), To => Card);
                  Us_Deck.Deck.Add (Item => Card, To => Foundation (Index_To) );
               elsif Upper (Command (2) ) = 'P' then
                  Check_To_Stack (Card, Index_To);
                  Us_Deck.Deck.Remove (From => Stock, Position => Us_Deck.Deck.Size (Stock), To => Card);
                  Us_Deck.Deck.Add (Item => Card, To => Stack (Index_To) );
               else
                  raise Invalid_Command;
               end if;
            elsif Command'Length >= 4 and then Upper (Command (1) ) = 'P' then
               Index_From := Integer'Value (Command (2 .. 2) );
               Index_To := Integer'Value (Command (4 .. 4) );

               if Upper (Command (3) ) = 'F' then
                  Card := Us_Deck.Deck.Value (Stack (Index_From), Us_Deck.Deck.Size (Stack (Index_From) ));
                  Check_To_Foundation (Card, Index_To);
                  Us_Deck.Deck.Remove (From => Stack (Index_From),
                                       Position => Us_Deck.Deck.Size (Stack (Index_From) ),
                                       To => Card
                                      )
                  ;
                  Us_Deck.Deck.Add (Item => Card, To => Foundation (Index_To) );
               elsif Upper (Command (3) ) = 'P' then
                  Card := Us_Deck.Deck.Value (Stack (Index_From), 1);
                  Check_To_Stack (Card, Index_To);

                  Transfer : loop
                     exit Transfer when Us_Deck.Deck.Is_Empty (Stack (Index_From) );

                     Us_Deck.Deck.Deal (From => Stack (Index_From), To => Card);
                     Us_Deck.Deck.Add (Item => Card, To => Stack (Index_To) );
                  end loop Transfer;
               else
                  raise Invalid_Command;
               end if;

               if Us_Deck.Deck.Is_Empty (Stack (Index_From) ) and then not Us_Deck.Deck.Is_Empty (Stock) then
                  Us_Deck.Deck.Remove (From => Stock, Position => Us_Deck.Deck.Size (Stock), To => Card);
                  Us_Deck.Deck.Add (Item => Card, To => Stack (Index_From) );
               end if;
            elsif Command'Length >= 4 and then Upper (Command (1) ) = 'F' then
               Index_From := Integer'Value (Command (2 .. 2) );
               Index_To := Integer'Value (Command (4 .. 4) );

               if Upper (Command (3) ) /= 'P' then
                  raise Invalid_Command;
               end if;

               Card := Us_Deck.Deck.Value (Foundation (Index_From), Us_Deck.Deck.Size (Foundation (Index_From) ));
               Check_To_Stack (Card, Index_To);
               Us_Deck.Deck.Remove (From => Foundation (Index_From),
                                    Position => Us_Deck.Deck.Size (Foundation (Index_From) ),
                                    To => Card
                                   )
               ;
               Us_Deck.Deck.Add (Item => Card, To => Stack (Index_To) );
            elsif Command'Length >= 6 and then Upper (Command (1) ) = 'C' then
               Count := Integer'Value (Command (2 .. 2) );
               Index_From := Integer'Value (Command (4 .. 4) );
               Index_To := Integer'Value (Command (6 .. 6) );

               if Upper (Command (3) ) /= 'P' or else Upper (Command (5) ) /= 'P' then
                  raise Invalid_Command;
               end if;

               if Count > Us_Deck.Deck.Size (Stack (Index_From) ) then
                  raise Invalid_Command;
               end if;

               Card := Us_Deck.Deck.Value (Stack (Index_From), Us_Deck.Deck.Size (Stack (Index_From) ) - Count + 1);
               Check_To_Stack (Card, Index_To);
               Us_Deck.Deck.Make_Empty (Item => Spare);

               Remove : for I in 1 .. Count loop
                  Us_Deck.Deck.Remove (From => Stack (Index_From),
                                       Position => Us_Deck.Deck.Size (Stack (Index_From) ),
                                       To => Card
                                      )
                  ;
                  Us_Deck.Deck.Add (Item => Card, To => Spare);
               end loop Remove;

               Add : for I in 1 .. Count loop
                  Us_Deck.Deck.Remove (From => Spare, Position => Us_Deck.Deck.Size (Spare), To => Card);
                  Us_Deck.Deck.Add (Item => Card, To => Stack (Index_To) );
               end loop Add;
            else
               raise Invalid_Command;
            end if;
         exception -- Handle_Command
         when others =>
            Text_Io.Put (Latin_1.Bel);
         end Handle_Command;
      end loop All_Actions;
   end Play_Game;
begin -- Devil
   Prepare_Screen;

   All_Games : loop
      exit All_Games when Play_Game = Quit;
   end loop All_Games;

   Text_Io.Put (Ansi_Tty_Control.Clear_Screen);
end Devil;
--
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.