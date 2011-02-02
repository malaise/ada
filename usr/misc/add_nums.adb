-- Read integers from stdin and add them
-- Any not digit character is a separator
with Ada.Text_Io, Ada.Exceptions;
with Sys_Calls, Text_Line, Parser, Integer_Image, Argument, Basic_Proc;
procedure Add_Nums is

  -- Text_Line file
  File : Text_Line.File_Type;
  -- Disable detection of negative number
  Force_Positive : Boolean;
  -- Sum (result)
  Sum : Integer;
  -- Iterator of Parser on current line
  Iter : Parser.Iterator;

  -- Separator for parsing line: any non digit
  function Separates (C : Character) return Boolean is
  begin
    return C <= '0' or else C >= '9';
  end Separates;

  -- Put usage
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: "
        & Argument.Get_Program_Name & " [ -p | --pos | -h | --help ]");
  end Usage;

  -- Put error
  procedure Error (Message : in String := "") is
  begin
    if Message /= "" then
      Basic_Proc.Put_Line_Error ("Error, " & Message & ".");
    end if;
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

begin

  -- Parse arguments: [ -p | --pos ] [ -h | --help ]
  if Argument.Get_Nbre_Arg = 0 then
    Force_Positive := False;
  elsif Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-p"
    or else Argument.Get_Parameter = "--pos") then
    Force_Positive := True;
  elsif Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-h"
    or else Argument.Get_Parameter = "--help") then
    Error;
    return;
  else
    Error ("invalid argument");
    return;
  end if;

  -- Open file on stdin and ini result
  Text_Line.Open (File, Text_Line.In_File, Sys_Calls.Stdin);
  Sum := 0;

  -- Loop on each line of input
  Line:
  loop
    declare
      -- Get line
      Str_Line : constant String := Text_Line.Get (File);
    begin
      -- End of file?
      exit Line when Str_Line = "";
      -- Parse line
      Parser.Set (Iter, Str_Line, Separates'Unrestricted_Access);
      -- Loop on each word of current line
      Word:
      loop
        declare
          -- Parse next word and get its (previous) separator
          Str_Word : constant String := Parser.Next_Word (Iter);
          Str_Sep : constant String := Parser.Prev_Separators (Iter);
        begin
          -- End of line?
          exit when Str_Word = "";
          -- Is it -ij?
          if not Force_Positive
          and then Str_Sep'Length > 0
          and then Str_Sep(Str_Sep'Last) = '-' then
            -- Sub value
            Sum := Sum - Natural'Value (Str_Word);
          else
            -- Add value
            Sum := Sum + Natural'Value (Str_Word);
          end if;
        end;
      end loop Word;
    end;
  end loop Line;

  -- Done, cleanup and put result
  Parser.Del (Iter);
  Text_Line.Close (File);
  Ada.Text_Io.Put_Line (Integer_Image (Sum));

exception
  when Err:others =>
    Error ("Exception "
        & Ada.Exceptions.Exception_Name (Err) & " raised.");
    raise;
end Add_Nums;

