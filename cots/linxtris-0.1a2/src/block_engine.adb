-----------------------------------------------------------------------
--
--     Copyright (C) 2003 Dúlio Matos Leite de Carvalho e Silva
--
--     This file is part of LinXtris.
--
--     LinXtris is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
--
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
-----------------------------------------------------------------------

with Ada.Numerics;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

package body Block_Engine is
   -- Public Section
   function Piece_X(The: Object; i: integer) return Integer is
   begin
      Return The.Piece(i).X;
   end;

   function Piece_Y( The: Object; i: integer) return Integer is
   begin
      Return The.Piece(i).Y;
   end;

   function Ghost_X(The: Object; i: integer) return Integer is
   begin
      Return The.Ghost(i).X;
   end;

   function Ghost_Y( The: Object; i: integer) return Integer is
   begin
      Return The.Ghost(i).Y;
   end;


   package Random_Shape_Pkg is

      package Shape_Random is new Ada.Numerics.Discrete_Random(Piece_Shape_Type);

      Shape_Vector: array(Piece_Shape_Type'Range) of Piece_Shape_Type;
      Shape_Gen: Shape_Random.Generator;
      procedure Init;
      function Random_Shape return Piece_Shape_Type;
   end Random_Shape_Pkg;

   package body Random_Shape_Pkg is

      -- This will "shuffle" the results of the generator
      -- resulting in a better random sequence

      function Random_Shape return Piece_Shape_Type is
         Result, Index: Piece_Shape_Type;
      begin
         Index := Shape_Random.Random( Shape_Gen );
         Result := Shape_Vector( Index );
         Shape_Vector( Index ) := Shape_Random.Random( Shape_Gen );
         return Result;
      end;

      procedure Init is
      begin
         Shape_Random.Reset(Shape_Gen);
         for K in Piece_Shape_Type'Range loop
            Shape_Vector(K) := Shape_Random.Random( Shape_Gen );
         end loop;
      end;

   end Random_Shape_Pkg;

   function Random_Shape return Piece_Shape_Type Renames Random_Shape_Pkg.Random_Shape;

   procedure Init(The: in out Object) is
      t: Boolean;
   begin
      Random_Shape_Pkg.Init;
      The.Next_Piece_Shape := Random_Shape;
      The.Lines_Number := 0;
      The.Lines_Completed := 0;
      The.Level_Has_Changed := False;
      The.Game_Is_Over := False;
      The.Auto_Down := False;
      The.Next_Move := None;
      The.Pieces_Number := 0;
      The.Level := The.Initial_Level;
      for i in The.Block_Color'Range(1) loop
         for j in The.Block_Color'Range(2) loop
            The.Block_Color(i,j) := Blank;
         end loop;
      end loop;
      Put_Initial_Lines(The);
      Put_Piece(The, t);
      Set_Ghost( The );
      The.Score := 0;
      The.Bonus_Down := The.Initial_Level;
      The.Bonus_Completed := The.Initial_Lines;
   end;

   procedure Set_Next_Move(The: in out Object; To: Move) is
   begin
      The.Next_Move := To;
   end;

   procedure Set_Auto_Down(The: in out Object) is
   begin
      The.Auto_Down := True;
   end;

   function Get_Pieces_Number(The: Object) return Integer is
   begin
      Return The.Pieces_Number;
   end;

   function Get_Piece_Color(The: Object) return Color is
   begin
      Return The.Piece_Color;
   end;

   function Get_Next_Piece_Color(The: Object) return Color is
   begin
      return Shape_To_Color( The.Next_Piece_Shape );
   end;

   function Get_Score(The: Object) return Integer is
   begin
      Return The.Score;
   end;

   function Get_Level(The: Object) return Integer is
   begin
      Return The.Level;
   end;

   procedure Set_Has_Ghost( The: in out Object; To: Boolean ) is
   begin
      The.Has_Ghost := To;
   end;

   function Get_Has_Ghost( The: in Object ) return Boolean is
   begin
      return The.Has_Ghost;
   end;

   function Get_Initial_Level(The: Object) return Integer is
   begin
      Return The.Initial_Level;
   end;

   function Get_Lines_Completed(The: Object) return Integer is
   begin
      Return The.Lines_Completed;
   end;

   function Get_Lowest_Line_Completed(The: Object) return Integer is
   begin
      Return The.Lowest_Line_Completed;
   end;

   function Get_Lines_Number(The: Object) return Integer is
   begin
      Return The.Lines_Number;
   end;

   function Game_Over(The: Object) return Boolean is
   begin
      Return The.Game_Is_Over;
   end;

   function Level_Changed(The: Object) return Boolean is
   begin
      Return The.Level_has_Changed;
   end;


   procedure Set_Initial_Level(The: in out Object; To: Positive) is
   begin
      The.Initial_Level := To;
   end;

   procedure Set_Initial_Lines(The: in out Object; To: Natural) is
   begin
      The.Initial_Lines := To;
   end;

   function Get_Initial_Lines(The: Object ) return Integer is
   begin
      return The.Initial_Lines;
   end;


   function Get_Position_Color(The: Object; X: Positive; Y: Positive) return Color is
   begin
      Return The.Block_Color(X, Y);
   end;

   procedure Set_Random_Piece_Color( The: in out Object; To: Boolean ) is
   begin
      The.Random_Piece_Color := To;
   end;

   function Get_Random_Piece_Color( The: in Object ) return Boolean is
   begin
      return The.Random_Piece_Color;
   end;


   procedure Set_Ghost( The: in out Object  ) is
   begin
      if not The.Has_Ghost then
         return;
      end if;
      The.Ghost := The.Piece;

      Outer_Loop:
      loop
         for I in The.Ghost'Range loop
            exit Outer_Loop when ( The.Ghost(I).Y = 1 );
            exit Outer_Loop when ( The.Block_Color(The.Ghost(I).X, The.Ghost(i).Y-1) /= Blank );
         end loop;
         for I in The.Ghost'Range loop
            The.Ghost(I).Y := The.Ghost(I).Y - 1;
         end loop;
      end loop Outer_Loop;
   end;

   function Piece_Can_Move_Down( The: in Object ) return Boolean is
   begin
      for i in The.Piece'Range loop
         if The.Piece(i).Y - 1 not in The.Block_Color'Range(2) then
            return False;
         end if;
         if The.Block_Color(The.Piece(i).X, The.Piece(i).Y - 1) /= Blank then
            return False;
         end if;
      end loop;
      return True;
   end;

   procedure Process_Move(The: in out Object) is
      Can_Move: Boolean;
   begin
      if The.Game_Is_Over then
         return;
      end if;
      The.Lines_Completed := 0;
      The.Level_Has_Changed := False;
      if The.Next_Move = Drop then
         Drop(The);
         for i in 1..4 loop
            The.Block_Color( The.Piece(i).X, The.Piece(i).Y ) := The.Piece_Color;
         end loop;
         The.Score := The.Score + 3*The.Level + The.Bonus_Down;
         Put_Piece(The, Can_Move);
         if not Can_Move then
            The.Game_Is_Over := True;
         end if;
         Check_Completed_Lines(The);
      elsif The.Next_Move = Right then
         Move_Right(The);
      elsif The.Next_Move = Left then
         Move_Left(The);
      elsif The.Next_Move = Rotate then
         Rotate(The);
      elsif (The.Next_Move = Down) or (The.Auto_Down) then
         Move_Down(The, Can_Move);
         if not Can_Move then
            The.Score := The.Score + 3*The.Level + The.Bonus_Down;
            for i in 1..4 loop
               The.Block_Color( The.Piece(i).X, The.Piece(i).Y ) := The.Piece_Color;
            end loop;
            Put_Piece(The, Can_Move);
            if not Can_Move then
               The.Game_Is_Over := True;
            end if;
         end if;
         Check_Completed_Lines(The);
         The.Auto_Down := False;
         if The.Lines_Number > 10*(The.Level+1-The.Initial_Level) then
            The.Level := The.Level + 1;
            The.Level_Has_Changed := True;
         end if;
      end if;
      Set_Ghost( The );
      The.Next_Move := None;
   end;


   -- Private Section

   procedure Put_Initial_Lines(The: in out Object) is
      G: Generator;
      x: natural := 1;
      y: natural := 1;
      Line_Not_Completed: Boolean;
   begin
      Reset( G );
      while y <= The.Initial_Lines loop
         x := 1;
         Line_Not_Completed := False;
         while x in The.Block_Color'Range(1) loop
            if Random( G ) > 0.2 then
               The.Block_Color(x, y) := Shape_To_Color( Random_Shape );
            else
               The.Block_Color(x, y) := Blank;
               Line_Not_Completed := True;
            end if;
            x := x + 1;
         end loop;
         if Line_Not_Completed then
            y := y + 1;
         end if;
      end loop;
   end;

   procedure Move_Right(The: in out Object) is
   begin
      for i in The.Piece'Range loop
         if The.Piece(i).X + 1 not in The.Block_Color'Range(1) then
            Return;
         end if;
         if The.Block_Color(The.Piece(i).X + 1, The.Piece(i).Y) /= Blank then
            Return;
         end if;
      end loop;
      for i in The.Piece'Range loop
         The.Piece(i).X := The.Piece(i).X + 1;
      end loop;
   end;

   procedure Move_Left(The: in out Object) is
   begin
      for i in The.Piece'Range loop
         if The.Piece(i).X - 1 not in The.Block_Color'Range(1) then
            Return;
         end if;
         if The.Block_Color(The.Piece(i).X - 1, The.Piece(i).Y) /= Blank then
            Return;
         end if;
      end loop;
      for i in The.Piece'Range loop
         The.Piece(i).X := The.Piece(i).X - 1;
      end loop;
      Return;
   end;

   procedure Move_Down (The: in out Object; Success: out Boolean) is
   begin
      for i in The.Piece'Range loop
         if The.Piece(i).Y - 1 not in The.Block_Color'Range(2) then
            Success := False;
            Return;
         end if;
         if The.Block_Color(The.Piece(i).X, The.Piece(i).Y - 1) /= Blank then
            Success := False;
            Return;
         end if;
      end loop;
      for i in The.Piece'Range loop
         The.Piece(i).Y := The.Piece(i).Y-1;
      end loop;
      Success := True;
   end;

   procedure Drop(The: in out Object) is
   begin
      loop
         for i in The.Piece'Range loop
            if The.Piece(i).Y - 1 not in The.Block_Color'Range(2) then
               Return;
            end if;
            if The.Block_Color(The.Piece(i).X, The.Piece(i).Y - 1) /= Blank then
               for j in The.Piece'Range loop
                  Return;
               end loop;
            end if;
         end loop;
         for i in The.Piece'Range loop
            The.Piece(i).Y := The.Piece(i).Y - 1;
         end loop;
      end loop;
   end;

   procedure Rotate(The: in out Object) is
      type Pos is
         record
            X: Integer;
            Y: Integer;
         end record;
      Tmp: array(1..4) of Pos;
      X, Y: Integer;
   begin
      Tmp(1).X := The.Piece(1).X;
      Tmp(1).Y := The.Piece(1).Y;
      case The.Piece_Shape is
         when Piece_Q =>
            Return;
         when Piece_L | Piece_T | Piece_J =>
            for i in (The.Piece'First + 1)..(The.Piece'Last) loop
               X := The.Piece(1).Y - The.Piece(i).Y + The.Piece(1).X;
               Y := The.Piece(i).X - The.Piece(1).X + The.Piece(1).Y;
               if (X > 0) and (Y > 0) then
                  Tmp(i).X := X;
                  Tmp(i).Y := Y;
               else
                  return;
               end if;
            end loop;
         when Piece_I | Piece_S | Piece_Z =>
            if The.Piece(2).X = The.Piece(1).X then
               for i in (The.Piece'First + 1)..(The.Piece'Last) loop
                  Tmp(i).X := The.Piece(1).Y - The.Piece(i).Y + The.Piece(1).X;
                  Tmp(i).Y := The.Piece(i).X - The.Piece(1).X + The.Piece(1).Y;
               end loop;
            else
               for i in (The.Piece'First + 1)..(The.Piece'Last) loop
                  Tmp(i).X := -The.Piece(1).Y + The.Piece(i).Y + The.Piece(1).X;
                  Tmp(i).Y := -The.Piece(i).X + The.Piece(1).X + The.Piece(1).Y;
               end loop;
            end if;
      end case;
      for i in The.Piece'Range loop
         if ( Tmp(i).X not in The.Block_Color'Range(1) ) or
           ( Tmp(i).Y not in The.Block_Color'Range(2) ) then
            Return;
         end if;
         if The.Block_Color(Tmp(i).X, Tmp(i).Y) /= Blank then
            Return;
         end if;
      end loop;
      for i in The.Piece'Range loop
         The.Piece(i).X := Tmp(i).X;
         The.Piece(i).Y := Tmp(i).Y;
      end loop;
   end;

   procedure Check_Completed_Lines(The: in out Object) is
      i, j, y: positive;
   begin
      for I in The.Completed_Lines_List'Range loop
         The.Completed_Lines_List(I) := 0;
      end loop;
      The.Lines_Completed := 0;
      j := 1;
      while j <= The.Block_Color'Last(2) loop
         i := 1;
         while (The.Block_Color(i,j) /= Blank) and (i<=The.Block_Color'Last(1)) loop
            i := i + 1;
            if i = The.Block_Color'Last(1) + 1 then
               exit;
            end if;
         end loop;
         if i = The.Block_Color'Last(1)+1 then
            The.Lines_Completed := The.Lines_Completed + 1;
            if The.Lines_Completed = 1 then
               The.Lowest_Line_Completed := j;
            end if;
            The.Completed_Lines_List( The.Lines_Completed ) := J;
            y := j;

            while(y < The.Block_Color'Last(2)) loop
               for x in The.Block_Color'Range(1) loop
                  The.Block_Color(x, y) := The.Block_Color(x, y + 1);
               end loop;
               y := y + 1;
            end loop;
         else
            j := j + 1;
         end if;
      end loop;
      Case The.Lines_Completed is
         when 1 =>
            The.Score := The.Score + The.Level*30 + The.Bonus_Completed;
         when 2 =>
            The.Score := The.Score + The.Level*35*2 + The.Bonus_Completed;
         when 3 =>
            The.Score := The.Score + The.Level*40*3 + The.Bonus_Completed;
         when 4 =>
            The.Score := The.Score + The.Level*45*4 + The.Bonus_Completed;
         when others =>
            null;
      end case;
      The.Lines_Number := The.Lines_Number + The.Lines_Completed;
   end;

   function Get_Completed_Lines_Number( The: Object ) return Lines_Indication is
   begin
      return The.Completed_Lines_List;
   end;
   procedure Put_Piece(The: in out Object; Success: out Boolean) is
   begin

      if The.Next_Piece_Shape = Piece_I then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-1;

         The.Piece(2).X := The.Game_Width/2;
         The.Piece(2).Y := The.Game_Height;

         The.Piece(3).X := The.Game_Width/2;
         The.Piece(3).Y := The.Game_Height-2;

         The.Piece(4).X := The.Game_Width/2;
         The.Piece(4).Y := The.Game_Height-3;

      elsif The.Next_Piece_Shape = Piece_L then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-2;

         The.Piece(2).X := The.Game_Width/2;
         The.Piece(2).Y := The.Game_Height-1;

         The.Piece(3).X := The.Game_Width/2;
         The.Piece(3).Y := The.Game_Height-3;

         The.Piece(4).X := The.Game_Width/2+1;
         The.Piece(4).Y := The.Game_Height-3;

      elsif The.Next_Piece_Shape = Piece_J then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-2;

         The.Piece(2).X := The.Game_Width/2;
         The.Piece(2).Y := The.Game_Height-1;

         The.Piece(3).X := The.Game_Width/2;
         The.Piece(3).Y := The.Game_Height-3;

         The.Piece(4).X := The.Game_Width/2-1;
         The.Piece(4).Y := The.Game_Height-3;

      elsif The.Next_Piece_Shape = Piece_T then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-3;

         The.Piece(2).X := The.Game_Width/2;
         The.Piece(2).Y := The.Game_Height-2;

         The.Piece(3).X := The.Game_Width/2-1;
         The.Piece(3).Y := The.Game_Height-3;

         The.Piece(4).X := The.Game_Width/2+1;
         The.Piece(4).Y := The.Game_Height-3;

      elsif The.Next_Piece_Shape = Piece_S then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-2;

         The.Piece(2).X := The.Game_Width/2;
         The.Piece(2).Y := The.Game_Height-3;

         The.Piece(3).X := The.Game_Width/2-1;
         The.Piece(3).Y := The.Game_Height-3;

         The.Piece(4).X := The.Game_Width/2+1;
         The.Piece(4).Y := The.Game_Height-2;

      elsif The.Next_Piece_Shape = Piece_Z then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-3;

         The.Piece(2).X := The.Game_Width/2;
         The.Piece(2).Y := The.Game_Height-2;

         The.Piece(3).X := The.Game_Width/2-1;
         The.Piece(3).Y := The.Game_Height-2;

         The.Piece(4).X := The.Game_Width/2+1;
         The.Piece(4).Y := The.Game_Height-3;

      elsif The.Next_Piece_Shape = Piece_Q then

         The.Piece(1).X := The.Game_Width/2;
         The.Piece(1).Y := The.Game_Height-2;

         The.Piece(2).X := The.Game_Width/2+1;
         The.Piece(2).Y := The.Game_Height-2;

         The.Piece(3).X := The.Game_Width/2;
         The.Piece(3).Y := The.Game_Height-3;

         The.Piece(4).X := The.Game_Width/2+1;
         The.Piece(4).Y := The.Game_Height-3;

      end if;
      if not The.Random_Piece_Color then
         The.Piece_Color := Shape_To_Color(The.Next_Piece_Shape);
      else
         The.Piece_Color := Shape_To_Color(Random_Shape);
      end if;
         The.Piece_Shape := The.Next_Piece_Shape;

      The.Next_Piece_Shape := Random_Shape;

      for i in The.Piece'Range loop
         if The.Block_Color(The.Piece(i).X, The.Piece(i).Y) /= Blank then
            Success := False;
            Return;
         end if;
      end loop;
      The.Pieces_Number := The.Pieces_Number + 1;
      Success := True;
   end;

   function Shape_To_Color( Shape: Piece_Shape_Type ) return Color is
   begin
      Case Shape is
         when Piece_I =>
            Return Red;
         when Piece_J =>
            Return Magenta;
         when Piece_L =>
            Return Yellow;
         when Piece_T =>
            Return Grey;
         when Piece_Q =>
            Return Cyan;
         when Piece_Z =>
            Return Green;
         when Piece_S =>
            Return Blue;
      end case;
   end;

end Block_Engine;
