with Image;
separate (Screen)

package body Moves is

  Move_No : Positive;
  Move_Window : Con_Io.Window;

  Upper_Left  : constant Con_Io.Square := ( 4, 54);
  Lower_Right : constant Con_Io.Square := (23, 78);
  Height : constant Positive := Lower_Right.Row - Upper_Left.Row + 1;
  subtype Moves_Range is Positive range 1 .. Height;

  type Move is record
    Num : Positive;
    White_Move : Players.Action_Rec := (Valid => False);
    White_Result : Game.Move_Status_List;
    Black_Move : Players.Action_Rec := (Valid => False);
    Black_Result : Game.Move_Status_List;
  end record;
  Moves_Array : array (Moves_Range) of Move;
  Pos : Moves_Range := 1;
  Looped : Boolean := False;

  function Next (P : Moves_Range) return Moves_Range is
  begin
    if P /= Moves_Range'Last then
      return P + 1;
    else
      return Moves_Range'First;
    end if;
  end Next;
  function Prev (P : Moves_Range) return Moves_Range is
  begin
    if P /= Moves_Range'First then
      return P - 1;
    else
      return Moves_Range'Last;
    end if;
  end Prev;

  procedure Add_Move (Color  : in Space.Color_List;
                      Action : in Game.Valid_Action_Rec;
                      Result : in Game.Move_Status_List) is
    use type Space.Color_List;
  begin
    if Color = Space.White then
      Moves_Array(Pos).Num := Move_No;
      Moves_Array(Pos).White_Move := Action;
      Moves_Array(Pos).White_Result := Result;
      Moves_Array(Pos).Black_Move := (Valid => False);
    else
      Moves_Array(Pos).Black_Move := Action;
      Moves_Array(Pos).Black_Result := Result;
      Move_No := Move_No + 1;
      Pos := Next (Pos);
      if Pos = Moves_Range'First then
        Looped := True;
      end if;
    end if;
  end Add_Move;

  procedure Put_Moves is
    Curr, Last : Moves_Range;

    procedure Put (M : in Move) is
      Empty : constant Image.Move_Str := (others => ' ');
    begin
      if not M.White_Move.Valid then 
        Con_Io.Put ("   " & ' ' & Empty,
                    Name => Move_Window,
                    Foreground => Main_Fore);
      else
        Con_Io.Put (Normal (M.Num, 3) & ' ' &
                     Image.Move_Image (M.White_Move, M.White_Result),
                    Name => Move_Window,
                    Foreground => Fore(Space.White));
      end if;

      if not M.Black_Move.Valid then
        Con_Io.Put (' ' & Empty,
                    Name => Move_Window,
                    Foreground => Main_Fore);
      else
        Con_Io.Put (' ' & Image.Move_Image (M.Black_Move, M.Black_Result),
                    Name => Move_Window,
                    Foreground => Fore(Space.Black));
      end if;
    end Put;

  begin
    -- First call: init
    if not Con_Io.Is_Open (Move_Window) then
      Con_Io.Open (Move_Window, Upper_Left, Lower_Right);
      Con_Io.Set_Background (Main_Back, Move_Window);
      Move_No := 1;
    end if;

    Con_Io.Clear (Move_Window);

    if not Looped then
      -- From First to Pos included
      for I in Moves_Range'First .. Pos loop
        Put (Moves_Array(I));
      end loop;
    else
      -- From Curr to Last included
      if not Moves_Array(Pos).Black_Move.Valid then
        -- Current is a new partial move
        Curr := Next (Pos);
        Last := Pos;
      else
        -- Current is oldest
        Curr := Pos;
        Last := Prev (Pos);
      end if;
      loop
        Put (Moves_Array(Curr));
        exit when Curr = Last;
        Curr := Next (Curr);
      end loop;
    end if;

  end Put_Moves;

end Moves;

