with Ada.Strings.Unbounded;
with Environ, Text_Handler, Text_Line, String_Mng, Ada_Words;
with Common, Files;
package body Output is

  -- Output flow
  File : Text_Line.File_Type;

  -- Spaces per indent level
  Spaces_Name : constant String := "ASTUB_INDENT";
  Def_Nb_Spaces : constant := 2;
  Max_Nb_Spaces : constant := 10;
  Spaces : Text_Handler.Text (Max_Nb_Spaces);

  -- Line length
  Length_Name : constant String := "ASTUB_LENGTH";
  Def_Length : constant := 80;
  Min_Length : constant := 50;
  Max_Length : constant := 132;
  Length : Positive := Def_Length;
  -- Mini lenght for String_Mng.Truncate
  Mini_Len : constant := 40;


  -- Is character a separator of Ada statement
  function Separates (Char : Character) return Boolean is
  begin
    return Ada_Words.Is_Separator (Char)
    or else Ada_Words.Is_Delimiter (Char);
  end Separates;


  -- Put one line (after flow has been cut according to Line_Feeds)
  procedure Format (Str : in String;
                    Comment : in Boolean;
                    Level : in Natural := 0) is
    -- Must we add the "-- "?
    Add_Comment : Boolean := Comment;
    -- Line to put
    package Asu renames Ada.Strings.Unbounded;
    Line2Put : Asu.Unbounded_String;
    -- Index where to start / where to cut
    Index : Natural;
  begin

    -- Check if this is line feed, put it
    Index := String_Mng.Parse_Spaces (Str);
    if Index = Str'Last
    and then Str(Str'Last) = Common.Line_Feed then
      Text_Line.New_Line (File);
      return;
    end if;

    -- Check if this is a comment to be put as a comment
    -- If yes, put Str at proper level
    if Comment then
      -- Search "--" and check that it is the significant start of Str
      declare
        Comment_Index : Natural
                      := String_Mng.Locate (Str, Str'First, "--");
      begin
        if Comment_Index /= 0
        and then Comment_Index = String_Mng.Parse_Spaces (Str) then
          -- The significant start of Str is "--"
          -- so Str is already a comment
          Add_Comment := False;
        end if;
      end;
    end if;

    -- Indent
    for I in 1 .. Level loop
      Asu.Append (Line2Put, Text_Handler.Value (Spaces));
    end loop;

    -- Append comment if needed
    if Add_Comment then
      Asu.Append (Line2Put, "-- ");
    end if;

    -- Append text
    Asu.Append (Line2Put, Str);

    -- Put comment
    if Add_Comment then
      Text_Line.Put (File, Asu.To_String (Line2Put));
      return;
    end if;

    -- Not a comment: split line if too long
    Index := String_Mng.Truncate (Asu.To_String (Line2Put),
                                  Length, Mini_Len, Length,
                                  Separates'Access);

    -- Put the first chunk 1 .. Index
    Text_Line.Put (File, Asu.Slice (Line2Put, 1, Index) );

    if Index /= Asu.Length (Line2Put) then
      -- Format the remaining: Index + 1 .. Last
      Format (Asu.Slice (Line2Put, Index + 1, Asu.Length (Line2Put)),
              False, Level);
    end if;

  end Format;


  -- Put --
  procedure Put (Str : in String;
                 Comment : in Boolean;
                 Level : in Natural := 0) is
    Start, Lfi : Natural;
  begin
    -- Check if spaces is set
    if Text_Handler.Empty (Spaces) then
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
         Text_Handler.Append (Spaces, ' ');
        end loop;
      end;

      -- Also set line length
      Environ.Get_Pos (Length_Name, Length);

      -- Get file
      File := Files.Out_File;
    end if;

    -- Split Str into several lines (look for Line_Feed)
    Lfi := 0;
    loop
      Start := Lfi + 1;
      Lfi := String_Mng.Locate (Str, Start, Common.Line_Feed);
      if Lfi = 0 or else Lfi = Str'Last then
        -- No Line_Feed of ends by Line_Feed => Last chunk, process up to end
        Format (Str (Start .. Str'Last), Comment, Level);
        exit;
      else
        -- Put this chunk
        Format (Str (Start .. Lfi), Comment, Level);
      end if;
    end loop;
  end Put;


  procedure Put_Line (Str : in String;
                      Comment : in Boolean;
                      Level : in Natural := 0) is
  begin
    Put (Str & Character'(Common.Line_Feed), Comment, Level);
  end Put_Line;


  procedure New_Line is
  begin
    Put (Common.Line_Feed, False, 0);
  end New_Line;

end Output;

