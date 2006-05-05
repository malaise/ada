-- Common definitions
with Sys_Calls, Text_Line;
package body Common is

  Lfc : constant Character := Text_Line.Line_Feed;
  Lfs : constant String := Lfc & "";
  Lfu : constant Ada.String.Unbounded.Unbounded_String
      := Ada.String.Unbounded.To_Unbounded_String (Lfs);


  -- Line feed string
  function Line_Feed return Ada.String.Unbounded.Unbounded_String is
  begin
    return Lfu;
  end Line_Feed;

  function Line_Feed return String is
  begin
    return Lfs;
  end Line_Feed;

  function Line_Feed return Character is
  begin
    return Lfc;
  end Line_Feed;

  -- Put error message on stderr and raises Syntax_Error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error ("-->" & Msg & "<--");
    raise Syntax_Error;
  end Error;

end Common;

