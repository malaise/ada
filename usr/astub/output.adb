with As.U; use As.U;
with Environ, Text_Handler, Text_Line, String_Mng.Regex, Ada_Words;
with Common, Files;
package body Output is

  -- Spaces per indent level
  Spaces_Name : constant String := "ASTUB_INDENT";
  Def_Nb_Spaces : constant := 2;
  Max_Nb_Spaces : constant := 10;
  Spaces : Text_Handler.Text(Max_Nb_Spaces);

  -- Line length
  Length_Name : constant String := "ASTUB_LENGTH";
  Def_Length : constant := 80;
  Min_Length : constant := 50;
  Max_Length : constant := 132;
  Length : Positive := Def_Length;
  -- Mini lenght for String_Mng.Truncate
  Mini_Len : constant := 40;

  -- get envir variables if first call
  procedure Getenv is
  begin
    -- Check if spaces is set
    if Spaces.Is_Empty then
      -- Getenv Nb_Spaces
      declare
        Nb_Spaces : Positive := Def_Nb_Spaces;
      begin
        Nb_Spaces := Environ.Get_Pos (Spaces_Name, Def_Nb_Spaces);
        if Nb_Spaces > Max_Nb_Spaces then
          -- If more than max, -> default
          Nb_Spaces := Def_Nb_Spaces;
        end if;
        -- Set Spaces
        for I in 1 .. Nb_Spaces loop
         Spaces.Append (' ');
        end loop;
      end;

      -- Also set line length
      Environ.Get_Pos (Length_Name, Length);
      if Length < Min_Length then
        Length := Min_Length;
      elsif Length > Max_Length then
        Length := Max_Length;
      end if;

    end if;
  end Getenv;

  -- Return the indentation of a given level
  function Get_Indent (Level : in Natural) return String is
    Result : Asu_Us;
  begin
    Getenv;
    for I in 1 .. Level loop
      Result.Append (Spaces.Value);
    end loop;
    return Result.Image;
  end Get_Indent;

  -- Is character a separator of Ada statement
  function Separates (Char : Character) return Boolean is
  begin
    return Ada_Words.Is_Separator (Char)
    or else Ada_Words.Is_Delimiter (Char);
  end Separates;

  -- Low level output procedure to filter successive Line_Feed
  Prev_Tail : Asu_Us;
  procedure Low_Put (Str : in String) is
    Ustr : Asu_Us;
    Found : Natural;
    Line_Feed_Char : constant Character := Common.Line_Feed;
  begin
    -- Prepend previous tail and replace any sequence of 3 or more
    -- line_feeds by only 2
    Ustr := Tus (
      String_Mng.Regex.Replace (Prev_Tail.Image & Str, "\n{3,}",
      Line_Feed_Char & Line_Feed_Char));
    Prev_Tail := Asu_Null;
    if Ustr.Is_Null then
      -- Nothing to put
      return;
    end if;
    -- Locate any tailing line feeds (1 or 2)
    Found := 0;
    for I in reverse 1 .. Ustr.Length loop
      if Ustr.Element (I) = Common.Line_Feed then
        Found := I;
      else
        exit;
      end if;
    end loop;
    -- Remove and save tailing line feeds if any
    if Found /= 0 then
      Prev_Tail := Ustr.Uslice (Found,  Ustr.Length);
      Ustr.Delete (Found, Ustr.Length);
    end if;
    -- Put remaining
    Text_Line.Put (Files.Out_File, Ustr.Image);
  end Low_Put;

  procedure Flush is
  begin
    Text_Line.Put (Files.Out_File, Prev_Tail.Image);
    Prev_Tail := Asu_Null;
  end Flush;

  -- Put one line (after flow has been cut according to Line_Feeds)
  procedure Format (Str : in String;
                    Comment : in Boolean;
                    Level : in Natural;
                    Indent : in Boolean) is
    -- Must we add the "-- "?
    Add_Comment : Boolean := Comment;
    -- Line to put
    Line2Put : Asu_Us;
    -- Index where to start / where to cut
    Index : Natural;
    -- Index of "--" in string
    Comment_Index : Natural;
  begin

    -- Check if this is a line feed (even with spaces before), put it
    Index := String_Mng.Parse_Spaces (Str);
    if Index /= 0
    and then Index = Str'Last
    and then Str(Str'Last) = Common.Line_Feed then
      Low_Put (Common.Line_Feed);
      return;
    end if;

    -- See there is a comment
    Comment_Index := String_Mng.Locate (Str, "--");

    -- Check if this is a comment to be put as a comment
    -- If yes, put Str at proper level
    if Comment then
      -- Check that comment is the significant start of Str
      if Comment_Index /= 0
      and then Comment_Index = String_Mng.Parse_Spaces (Str) then
        -- The significant start of Str is "--"
        -- so Str is already a comment
        Add_Comment := False;
      end if;
    end if;

    -- Indent
    if Indent then
      for I in 1 .. Level loop
        Line2Put.Append (Spaces.Value);
      end loop;
    end if;

    -- Append comment if needed
    if Add_Comment then
      Line2Put.Append ("-- ");
    end if;

    -- Append text
    Line2Put.Append (Str);

    -- Put comment without truncating
    if Comment or else Comment_Index /= 0 then
       Low_Put (Line2Put.Image);
      return;
    end if;

    -- Not a comment: split line if too long
    Index := String_Mng.Truncate (Line2Put.Image,
                                  Length, Mini_Len, Length,
                                  Separates'Access);

    -- Put the first chunk 1 .. Index
    Low_Put (Line2Put.Slice (1, Index));

    if Index /= Line2Put.Length then
      -- Line2Put is split. First chunk is Put_Line
      Low_Put (Common.Line_Feed);
      -- Format the remaining: Index + 1 .. Last
      Format (Line2Put.Slice (Index + 1, Line2Put.Length),
              False, Level, True);
    end if;

  end Format;


  -- Put --
  procedure Put (Str : in String;
                 Comment : in Boolean;
                 Level : in Natural := 0;
                 Indent : in Boolean := False) is
    Start, Lfi : Natural;
  begin
    Getenv;

    -- Split Str into several lines (look for Line_Feed)
    Lfi := 0;
    loop
      Start := Lfi + 1;
      Lfi := String_Mng.Locate (Str, Common.Line_Feed, Start);
      if Lfi = 0 or else Lfi = Str'Last then
        -- No Line_Feed or ends by Line_Feed => Last chunk, process up to end
        Format (Str (Start .. Str'Last), Comment, Level, Indent);
        exit;
      else
        -- Put this chunk
        Format (Str (Start .. Lfi), Comment, Level, Indent);
      end if;
    end loop;
  end Put;


  procedure Put_Line (Str : in String;
                      Comment : in Boolean;
                      Level : in Natural := 0;
                      Indent : in Boolean := False) is
  begin
    Put (Str & Character'(Common.Line_Feed), Comment, Level, Indent);
  end Put_Line;


  procedure New_Line is
  begin
    Put (Common.Line_Feed, False, 0, False);
  end New_Line;

end Output;

