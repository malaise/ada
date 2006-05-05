with Environ, Text_Handler, Text_Line, String_Mng;
with Common, Files;
package body Output is

  -- Spaces per indent level
  Spaces_Name : constant String := "ASTUB_INDENT";
  Def_Nb_Spaces : constant := 2;
  Max_Nb_Spaces : constant := 10;
  Spaces : Text_Handler.Text (Max_Nb_Spaces);

  -- Put --
  procedure Put (Str : in String;
                 Comment : in Boolean;
                 Level : in Natural := 0) is
    File : Text_Line.File_Type;
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
    end if;

    -- Get file
    File := Files.Out_File;

    -- Split into several lines
    declare
    begin
    end;


    if Comment then
      -- Detect comment of comment (no indent nor extra "--" in this case)
      declare
        Comment_Index : Natural
                      := String_Mng.Locate (Str, Str'First, "--");
      begin
        if Comment_Index = 0
        or else Comment_Index /= String_Mng.Parse_Spaces (Str) then
          -- This is not (only) a comment, so put whole as indented comment
          -- Indent
          for I in 1 .. Level loop
            Text_Line.Put (File, Text_Handler.Value (Spaces));
          end loop;
          -- Put comment
          Text_Line.Put (File, "-- ");
          -- Indent comment
          for I in 1 .. Comment_Level loop
            Text_Line.Put (File, Text_Handler.Value (Spaces));
          end loop;
        end if;
        -- Put text
        Text_Line.Put (File, Str(First_Char .. Str'Last));
      end if;
   else
      -- Indent
      for I in 1 .. Level loop
        Text_Line.Put (File, Text_Handler.Value (Spaces));
      end loop;
      -- Put Str
      Text_Line.Put (File, Str);
    end if;
  end Put;

  procedure Put_Line (Str : in String;
                      Comment : in Boolean;
                      Level : in Natural := 0) is
  begin
    Put (Str & Common.Line_Feed, Level, Comment, Comment_Level);
  end Put_Line;

  procedure New_Line is
  begin
    Put (Common.Line_Feed, 0, False);
  end New_Line;
end Output;

