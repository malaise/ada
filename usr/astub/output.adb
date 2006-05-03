with Environ, Text_Handler, Text_Line, String_Mng;
with Files;
package body Output is

  -- Spaces per indent level
  Spaces_Name : constant String := "ASTUB_INDENT";
  Def_Nb_Spaces : constant := 2;
  Max_Nb_Spaces : constant := 10;
  Spaces : Text_Handler.Text (Max_Nb_Spaces);

  -- Put_Line --
  procedure Put_Line (Str : in String; 
                      Level : in Natural;
                      Comment : in Boolean) is
    Nb_Spaces : Positive := Def_Nb_Spaces;
    File : Text_Line.File_Type;
    First_Char : Natural;
    Add_Comment : Boolean;
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

    -- Indent
    for I in 1 .. Level loop
      Text_Line.Put (File, Text_Handler.Value (Spaces));
    end loop;

    -- See if Str IS a comment
    Add_Comment := Comment;
    First_Char := String_Mng.Parse_Spaces (Str);
    if First_Char > 0
    and then First_Char <= Str'Last - 1
    and then Str(First_Char) = '''
    and then Str(First_Char+1) = ''' then
      -- First significant chars of Str are "--", so don't any new --
      Add_Comment := False;
    end if;

    -- Put comment is required
    if Add_Comment then
      Text_Line.Put (File, "-- ");
    end if;

    -- Put Str
    Text_Line.Put_Line (File, Str);
      
  end Put_Line;

end Output;

