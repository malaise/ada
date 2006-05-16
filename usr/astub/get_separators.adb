with Ada.Strings.Unbounded;
with Ada_Parser;
with Common, Words;
-- Get the trailing separators (space or htab) of Words
function Get_Separators return String is
  Word : Words.Word_Rec;
  package Asu renames Ada.Strings.Unbounded;
  use type Ada_Parser.Lexical_Kind_List, Asu.Unbounded_String;
  Len : constant Natural := Words.Length;
  Start : Natural := Len;
  Result : Asu.Unbounded_String;
begin
  -- Locate last non separator (or Line_Feed)
  for I in reverse 1 .. Len loop
    Word := Words.Read (I);
    exit when Word.Lexic /= Ada_Parser.Separator
    or else Word.Text = String'(Common.Line_Feed);
    Start := I - 1;
  end loop;

  -- Empty Words or Words does not end by separators
  if Start =  Len then
    return "";
  end if;

  -- Start is the last char before the trailing spaces or htabs
  -- Get and append all what follows I
  for I in Start + 1 .. Words.Length loop
    Asu.Append (Result, Words.Get (Start + 1).Text);
  end loop;
  return Asu.To_String (Result);
end Get_Separators;

