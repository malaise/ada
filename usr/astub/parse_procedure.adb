with As.U;
with Output, Words, Parser_Ada, Parse_To_Ends, Parse_To_End, Parse_Name,
     Fix_Comment;

procedure Parse_Procedure (Level : in Natural;
                           Generated : in out Boolean) is
  Name : As.U.Asu_Us;
  Has_Aspect : Boolean;
begin

  -- Parse name
  Words.Add (Parser_Ada.Reserved_Word, "procedure");
  Parse_Name (Level, Name);

  -- Skip until last ';' or "with"
  Parse_To_Ends (
   (1 => (Parser_Ada.Delimiter, As.U.Tus (";")),
    2 => (Parser_Ada.Reserved_Word, As.U.Tus ("with"))),
   Level, True, False);

  -- If a renames or generic instanciation, put as comment
  if Words.Search (Parser_Ada.Reserved_Word, "renames") /= 0 then
    Fix_Comment (Level);
    Output.Put_Line (Words.Concat, True, Level, True);
    Words.Reset;
    return;
  end if;
  if Words.Search (Parser_Ada.Reserved_Word, "is") /= 0 then
    Fix_Comment (Level);
    Output.Put_Line (Words.Concat, True, Level, True);
    Words.Reset;
    return;
  end if;

  Has_Aspect := Words.Search (Parser_Ada.Reserved_Word, "with") /= 0;

  -- This is a "real" declaration: remove last ";" or " with"
  Generated := True;
  Words.Del;
  if Has_Aspect then
    Words.Del;
  end if;

  -- Output this and " is"
  Output.Put_Line (Words.Concat & " is", False, Level);
  -- Skip aspect if any
  if Has_Aspect then
    Parse_To_End (Parser_Ada.Delimiter, ";", Level);
  end if;
  Words.Reset;

  -- begin
  --   null;
  -- end <name>;
  Output.Put_Line ("begin", False, Level, True);
  Output.Put_Line ("null;", False, Level + 1, True);
  Output.Put_Line ("end " & Name.Image & ";", False, Level, True);
end Parse_Procedure;

