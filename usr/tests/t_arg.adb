with Text_Handler, Argument, My_Io, Upper_Char;
use  My_Io;

procedure T_Arg is

  subtype Rep_Key_Range is Character;
  Rep_Key : Rep_Key_Range;

  Occ : Natural;

  Key : Text_Handler.Text(Argument.Max_Len_Arg);
  Arg : Text_Handler.Text(Argument.Max_Len_Arg);

  Pos : Natural;

  Prog_Path : String (1 .. Argument.Max_Len_Arg);
  Prog_Name : String (1 .. Argument.Max_Len_Arg);
  Len : Natural;

  procedure Get_Txt (Txt : in out Text_Handler.Text) is
    Str : String (1 .. Txt.Max_Len);
    Len : Natural;
  begin
    Skip_Line;
    Get_Line (Str, Len);
    Text_Handler.Set (Txt, Str(1..Len));
  exception
    when others => raise Constraint_Error;
  end Get_Txt;


begin
  Argument.Get_Program_Path (Prog_Path, Len);
  Put ('>' & Prog_Path (1 .. Len) & "< >");
  Argument.Get_Program_Name (Prog_Name, Len);
  Put_Line (Prog_Name (1 .. Len) & '<');

  Put_Line (Natural'Image(Argument.Get_Nbre_Arg) & " arguments.");
  loop
    loop
      begin
        Put_Line ("E : Exit");
        Put_Line ("K : Key");
        Put_Line ("Y : anY_key");
        Put_Line ("N : Not_key");
        Put_Line ("A : Any_arg");
        Put ("Enter the letter of your choice : ");
        Get (Rep_Key);
        Rep_Key := Upper_Char(Rep_Key);
        exit when Rep_Key = 'E'
        or else Rep_Key = 'K'
        or else Rep_Key = 'Y'
        or else Rep_Key = 'N'
        or else Rep_Key = 'A';
      exception
        when others => null;
      end;
    end loop;

    case Rep_Key is
      when 'E' =>
        exit;
      when 'K' =>
        loop
          begin
            Put ("Enter KEY ? ");
            Get_Txt (Key);
            exit;
          exception
            when others => null;
          end;
        end loop;
      when 'Y' =>
        Text_Handler.Set (Key, Argument.Any_Key);
      when 'N' =>
        Text_Handler.Set (Key, Argument.Not_Key);
      when 'A' =>
        Text_Handler.Set (Key, Argument.Any_Arg);
      when others =>
        null;
    end case;


    loop
      begin
        Put ("Enter OCCURENCE ? ");
        Get (Occ);
        exit;
      exception
        when others => null;
      end;
    end loop;
    begin
      Argument.Get_Param_And_Pos (Arg, Pos, Occ, Text_Handler.Value(Key));
      Put_Line ("Argument : >" & Text_Handler.Value(Arg) & "<");
      for I in 1 .. Text_Handler.Length(Arg) loop
        Put_Line ("Char ->" & Text_Handler.Value(Arg)(I) & "< " &
         Integer'Image (Character'Pos(Text_Handler.Value(Arg)(I)) ) );
      end loop;

      Put (" found at position "); Put_Line (Pos, 3);
    exception
      when Argument.Argument_Not_Found =>
        Put_Line ("Exception ARGUMENT_NOT_FOUND raised.");
      when Argument.Argument_Too_Long =>
        Put_Line ("Exception ARGUMENT_TOO_LONG raised.");
      when others =>
        Put_Line ("Exception raised.");
    end;
    New_Line;
  end loop;
end T_Arg;
