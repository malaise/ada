with As.U; use As.U;
with Common, Words, Parser_Ada;
-- Get the trailing separators (space or htab) of Words
function Get_Separators return String is
  Word : Words.Word_Rec;
  use type Parser_Ada.Lexical_Kind_List, Asu_Us;
  Len : constant Natural := Words.Length;
  Start : Natural := Len;
  Result : Asu_Us;
begin
  -- Locate last non separator (or Line_Feed)
  for I in reverse 1 .. Len loop
    Word := Words.Read (I);
    exit when Word.Lexic /= Parser_Ada.Separator
    or else Word.Text = Asu_Us'(Common.Line_Feed);
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
  return Asu_Ts (Result);
end Get_Separators;

