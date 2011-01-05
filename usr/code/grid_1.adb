with Ada.Characters.Latin_1;
with As.B; use As.B;
with My_Io, Sorts;
package body Grid_1 is


  Data : array (Row_Coordinate, Col_Coordinate) of Character
       := (others => (others => Ada.Characters.Latin_1.Nul));
  Initialized : Boolean := False;

  package Char_Sort is new Sorts (Character, Positive, "<", String);

  -- Return a valid character for text
  function Filter (C : Character) return Character is
  begin
    if (C >= ' ' and then C <= '~') or else C = Ada.Characters.Latin_1.Cr then
      return C;
    elsif C = Ada.Characters.Latin_1.Ht then
      return ' ';
    else
      return Ada.Characters.Latin_1.Nul;
    end if;
  end Filter;

  -- Init Data
  procedure Initialize (Key : in String) is
    Row : Row_Coordinate;
    Col : Col_Coordinate;
    Char : Character;
    Stripped_Key : Asb_Bs(80);

    -- Store a char in data, checking if it is in Stripped_Key
    procedure Store (Char : in Character; Check : in Boolean) is
    begin
      if Check and then Stripped_Key.Locate (Char & "") /= 0 then
        return;
      end if;
      Data (Row, Col) := Char;
      if Col /= Col_Coordinate'Last then
        Col := Col_Coordinate'Succ(Col);
      else
        Col := Col_Coordinate'First;
        Row := Row_Coordinate'Succ(Row);
      end if;
    end Store;

  begin
    Stripped_Key.Set_Null;
    -- Store stripped key
    for I in Key'Range loop
      Char := Filter(Key(I));
      if Char /= Ada.Characters.Latin_1.Nul then
        if Stripped_Key.Locate (Char & "") = 0 then
          Stripped_Key.Append (Char);
        end if;
      end if;
    end loop;

    -- Sort characters of Key
    declare
      Sorted_Key : String(1 .. Stripped_Key.Length) := Stripped_Key.Image;
    begin
      Char_Sort.Bubble_Sort (Sorted_Key);

      -- Store stripped key then other chars in data
      Row := Row_Coordinate'First;
      Col := Col_Coordinate'First;
      for I in Sorted_Key'Range loop
        Store (Sorted_Key(I), False);
      end loop;
    end;
    Store (Ada.Characters.Latin_1.Cr, True);
    for C in Character'(' ') .. '~' loop
      Store (C, True);
    end loop;
    Initialized := True;
  end Initialize;


  -- C can be any char from ' ' to '~' or Ada.Characters.Latin_1.Cr
  function Encode (C : Character) return Coordinate_Rec is
    Sc : constant Character := Filter(C);
  begin
    if not Initialized then
      raise Grid_Not_Init;
    end if;
    if Sc = Ada.Characters.Latin_1.Nul then
      raise Invalid_Character;
    end if;
    for Row in Row_Coordinate loop
      for Col in Col_Coordinate loop
        if Data (Row, Col) = Sc then
          return (Row, Col);
        end if;
      end loop;
    end loop;
    raise Invalid_Character;
  end Encode;

  function Decode (Coordinate : Coordinate_Rec) return Character is
  begin
    if not Initialized then
      raise Grid_Not_Init;
    end if;
    return Data (Coordinate.Row, Coordinate.Col);
  end Decode;

  procedure Dump is
  begin
    if not Initialized then
      raise Grid_Not_Init;
    end if;
    for R in Row_Coordinate loop
      for C in Col_Coordinate loop
        if Data(R, C) = Ada.Characters.Latin_1.Cr then
          My_Io.Put ("Ret");
        elsif Data(R, C) /= Ada.Characters.Latin_1.Nul then
          My_Io.Put('>' & Data(R, C) & '<');
        elsif Data(R, C) = Ada.Characters.Latin_1.Nul then
          My_Io.Put ("Nul");
        else
          raise Constraint_Error;
        end if;
        My_Io.Put (' ');
      end loop;
      My_Io.New_Line;
    end loop;
  end Dump;

end Grid_1;

