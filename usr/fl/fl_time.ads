package Fl_Time is

  type Hours_Range   is range 0 .. 999_999_999;
  type Minutes_Range is range 0 .. 59;

  type Time_Type is record
    Positiv : Boolean       := True;
    Hours   : Hours_Range   := 0;
    Minutes : Minutes_Range := 0;
  end record;

  function "+" (Left, Right : Time_Type) return Time_Type;

  function "-" (Left, Right : Time_Type) return Time_Type;

  Time_Overflow : exception;

end Fl_Time;

