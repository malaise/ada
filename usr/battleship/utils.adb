with Trace.Loggers;
package body Utils is


  -- Debug modes
  Comm_Logger : Trace.Loggers.Logger;
  Setup_Logger : Trace.Loggers.Logger;
  Play_Logger : Trace.Loggers.Logger;
  procedure Init is
  begin
    Comm_Logger.Init ("Communication");
    Setup_Logger.Init ("Setup");
    Play_Logger.Init ("Play");
  end Init;

  -- Debug
  procedure Dbg_Comm  (Msg : in String) is
  begin
    Comm_Logger.Log_Debug (Msg);
  end Dbg_Comm;
  procedure Dbg_Setup  (Msg : in String) is
  begin
    Setup_Logger.Log_Debug (Msg);
  end Dbg_Setup;
  procedure Dbg_Play  (Msg : in String) is
  begin
    Play_Logger.Log_Debug (Msg);
  end Dbg_Play;
  procedure Err_Comm  (Msg : in String) is
  begin
    Comm_Logger.Log_Error (Msg);
  end Err_Comm;
  procedure Err_Setup  (Msg : in String) is
  begin
    Setup_Logger.Log_Error (Msg);
  end Err_Setup;
  procedure Err_Play  (Msg : in String) is
  begin
    Play_Logger.Log_Error (Msg);
  end Err_Play;

  function Dbg_Comm return Boolean is
  begin
    return Comm_Logger.Debug_On;
  end Dbg_Comm;
  function Dbg_Setup return Boolean is
  begin
    return Setup_Logger.Debug_On;
  end Dbg_Setup;
  function Dbg_Play return Boolean is
  begin
    return Play_Logger.Debug_On;
  end Dbg_Play;

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

