with Environ, Text_Handler, Text_Line, String_Mng;
with Files;
package body Output is

  -- Spaces per indent level
  Spaces_Name : constant String := "ASTUB_INDENT";
  Def_Nb_Spaces : constant := 2;
  Max_Nb_Spaces : constant := 10;
  Spaces : Text_Handler.Text (Max_Nb_Spaces);

  -- Put --
  procedure Put (Str : in String; 
                 Level : in Natural;
                 Comment : in Boolean;
                 Comment_Level : in Natural := 0) is
    Nb_Spaces : Positive := Def_Nb_Spaces;
    File : Text_Line.File_Type;
    First_Char : Natural;
  begin
    -- Check if spaces is set
    if Text_Handler.Empty (Spaces) then
      -- Getenv Nb_Spaces
      Nb_Spaces := Environ.Get_Pos (Spaces_Name, Def_Nb_Spaces);
      if Nb_Spaces > Max_Nb_Spaces then
        -- If more than max, -> default
        Nb_Spaces := Def_Nb_Spaces;
      end if;
      -- Set Spaces
      for I in 1 .. Nb_Spaces loop
       Text_Handler.Append (Spaces, ' ');
      end loop;
    end if;

    -- Get file
    File := Files.Out_File;

    -- For comment, skip leading separators, "--", separators
    if Comment then
      -- Parse leading separators
      First_Char := String_Mng.Parse_Spaces (Str);

      if First_Char > 0
      and then First_Char <= Str'Last - 1
      and then Str(First_Char) = '-'
      and then Str(First_Char + 1) = '-' then
        -- First significant chars of Str are "--", so skip them
        First_Char := First_Char + 2;
        -- Skip following separators
        First_Char := String_Mng.Parse_Spaces (Str(First_Char .. Str'Last));
      end if;
      if First_Char /= 0 then
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
                      Level : in Natural;
                      Comment : in Boolean;
                      Comment_Level : in Natural := 0) is
  begin
    Put (Str, Level, Comment, Comment_Level);
    Text_Line.New_Line (Files.Out_File);
  end Put_Line;

end Output;

