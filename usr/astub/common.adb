-- Common definitions
with Sys_Calls, Text_Line;
with Words;
package body Common is

  Lfc : constant Character := Text_Line.Line_Feed_Char;
  Lfs : constant String := Text_Line.Line_Feed_Str;
  Lfu : constant Asu_Us := Tus (Lfs);


  -- Line feed string
  function Line_Feed return Asu_Us is
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

  procedure Dump_Words is
    Word : Words.Word_Rec;
  begin
    Sys_Calls.Put_Line_Error ("Dumping words, length "
                            & Natural'Image (Words.Length));
    for I in 1 .. Words.Length loop
      Word := Words.Read (I);
      Sys_Calls.Put_Line_Error (Word.Lexic'Img & ":" & Word.Text.Image);
    end loop;
  end Dump_Words;

end Common;

