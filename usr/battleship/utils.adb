with Environ, Basic_Proc;
package body Utils is


  -- Debug modes
  Dbg_Comm : Boolean := False;
  Dbg_Setup : Boolean := False;
  Dbg_Play : Boolean := False;
  procedure Init is
  begin
    Dbg_Comm := Environ.Is_Yes ("BATTLESHIP_DEBUG_COMM");
    Dbg_Setup := Environ.Is_Yes ("BATTLESHIP_DEBUG_SETUP");
    Dbg_Play := Environ.Is_Yes ("BATTLESHIP_DEBUG_PLAY");
  end Init;

  function Debug_Comm return Boolean is
  begin
    return Dbg_Comm;
  end Debug_Comm;

  function Debug_Setup return Boolean is
  begin
    return Dbg_Setup;
  end Debug_Setup;

  function Debug_Play return Boolean is
  begin
    return Dbg_Play;
  end Debug_Play;

  -- Debug message
  procedure Debug (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Output (Msg & ".");
  end Debug;

  -- Image in 2 chars (10 -> a)
  function Image (C : Coord) return Str2 is
    Char : Character;
  begin
   if C.Col = 10 then
     Char := 'a';
   else
     Char := Character'Val(Character'Pos('1') + Natural (C.Col) - 1);
   end if;
   return Character'Val(Character'Pos('A') + Row_Range'Pos(C.Row)) & Char;
  end Image;

  function Value (S : Str2) return Coord is
    Res : Coord;
  begin
    Res.Row := Row_Range'Val (Character'Pos(S(1)) - Character'Pos('A'));
    if S(2) = 'a' then
      Res.Col := 10;
    else
      Res.Col:= Col_Range (Character'Pos(S(2)) - Character'Pos('1') + 1);
    end if;
    return Res;
  end Value;

  -- Cell of a field
  function Fld2Coord (Start : Afpx.Field_Range; Fld : Afpx.Field_Range)
                     return Coord is
    use type Afpx.Absolute_Field_Range;
    Offset : constant Afpx.Absolute_Field_Range := Fld - Start;
  begin
    return (Row_Range'Val (Natural (Offset) / 10),
            Col_Range (Offset rem 10 + 1));
  end Fld2Coord;

  -- Field of a Cell
  function Coord2Fld (Start : Afpx.Field_Range; C : Coord)
                     return Afpx.Field_Range is
    use type Afpx.Absolute_Field_Range;
  begin
    return Start + Afpx.Absolute_Field_Range (Row_Range'Pos (C.Row) * 10)
                 + Afpx.Field_Range (C.Col) - 1;
  end Coord2Fld;

   -- Is a cell in grid
  function In_Grid (C : Coord; Row_Offset, Col_Offset : Integer)
                   return Boolean is
    Row, Col : Integer;
  begin
    Row := Row_Range'Pos(C.Row) + 1 + Row_Offset;
    Col := Integer(C.Col) + Col_Offset;
    return Row >= 1 and then Row <= 10
    and then Col >= 1 and then Col <= 10;
  end In_Grid;

end Utils;
