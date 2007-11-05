with Ada.Text_Io;
with Argument_Parser;

procedure T_Arg_Parser is
  package Asu renames Argument_Parser.Asu;
  function Asu_Tus (Source : in String) return Argument_Parser.Asu_Us
                   renames Asu.To_Unbounded_String;

  -- The keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   (Key_Char => 's', Key_String => Asu_Tus ("simple"),
    Key_Can_Multiple => False, Key_Can_Option => False),
   (Key_Char => 'm', Key_String => Asu_Tus ("multi"),
    Key_Can_Multiple => True, Key_Can_Option => False),
   (Key_Char => 'o', Key_String => Asu_Tus ("opti"),
    Key_Can_Multiple => False, Key_Can_Option => True),
   (Key_Char => 'c', Key_String => Asu_Tus ("complex"),
    Key_Can_Multiple => True, Key_Can_Option => True) );

  Dscr : Argument_Parser.Parsed_Dscr;
begin

  Dscr :=  Argument_Parser.Parse (Keys);

  Ada.Text_Io.Put ("Parsing OK is "
     & Boolean'Image (Dscr.Is_Ok));
  Ada.Text_Io.Put (" and parsing error string is");
  if not Dscr.Is_Ok then
    Ada.Text_Io.New_Line;
  end if;
  Ada.Text_Io.Put_Line (" >" & Dscr.Get_Error & "<");

  Ada.Text_Io.Put_Line (
     "Number of keys found:" & Dscr.Get_Number_Keys'Img
     & ", Last key at pos:" & Dscr.Get_Last_Pos_Of_Keys'Img
     & ", First after at pos:" & Dscr.Get_First_Pos_After_Keys'Img
     & " and Nb embedded arguments:" & Dscr.Get_Nb_Embedded_Arguments'Img);

  for I in 0 .. Keys'Last loop
    if I = 0 then
      Ada.Text_Io.Put ("Arguments not key are");
    else
      Ada.Text_Io.Put ("Key " & Keys(I).Key_Char & " "
        & Asu.To_String (Keys(I).Key_String) & " is");
    end if;
    Ada.Text_Io.Put_Line (" found on" & Dscr.Get_Nb_Occurences (I)'Img
         & " occurences.");
    for J in 1 .. Dscr.Get_Nb_Occurences(I) loop
      Ada.Text_Io.Put_Line ("  Option >" & Dscr.Get_Option (I, J) & "<");
    end loop;
  end loop;

  Dscr.Reset;

exception
  when Argument_Parser.Parsing_Error =>
    Ada.Text_Io.Put_Line ("Exception Parsing_Error.");
end T_Arg_Parser;

