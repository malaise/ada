with As.U, Argument, Basic_Proc, Upper_Char, Gets, Normal;
use  Basic_Proc;
procedure T_Arg is

  subtype Rep_Key_Range is Character;
  Rep_Key : Rep_Key_Range;

  Occ : Natural;

  Max_Len_Arg : constant := 1024;
  Key : As.U.Asu_Us;
  Arg : As.U.Asu_Us;

  Pos : Natural;

  Prog_Path : String (1 .. Max_Len_Arg);
  Prog_Name : String (1 .. Max_Len_Arg);
  Len : Natural;

  procedure Get_Txt (Txt : in out As.U.Asu_Us) is
    Str : String (1 .. Max_Len_Arg);
    Len : Natural;
  begin
    Get_Line (Str, Len);
    Txt := As.U.Tus (Str(1..Len));
  exception
    when others => raise Constraint_Error;
  end Get_Txt;


  procedure Get (I : out Integer) is
    Str : As.U.Asu_Us;
  begin
    Get_Txt (Str);
    I := Gets.Get_Int (Str.Image);
  end Get;

begin
  Argument.Get_Program_Path (Prog_Path, Len);
  Put_Output ('>' & Prog_Path (1 .. Len) & "< >");
  Argument.Get_Program_Name (Prog_Name, Len);
  Put_Line_Output (Prog_Name (1 .. Len) & '<');

  Put_Line_Output (Natural'Image(Argument.Get_Nbre_Arg) & " arguments.");
  loop
    loop
      begin
        Put_Line_Output ("E : Exit");
        Put_Line_Output ("K : Key");
        Put_Line_Output ("Y : anY_key");
        Put_Line_Output ("N : Not_key");
        Put_Line_Output ("A : Any_arg");
        Put_Output ("Enter the letter of your choice : ");
        Get (Rep_Key);
        Skip_Line;
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
            Put_Output ("Enter KEY ? ");
            Get_Txt (Key);
            exit;
          exception
            when others => null;
          end;
        end loop;
      when 'Y' =>
        Key := As.U.Tus (Argument.Any_Key);
      when 'N' =>
        Key := As.U.Tus (Argument.Not_Key);
      when 'A' =>
        Key := As.U.Tus (Argument.Any_Arg);
      when others =>
        null;
    end case;


    loop
      begin
        Put_Output ("Enter OCCURENCE ? ");
        Get (Occ);
        exit;
      exception
        when others => null;
      end;
    end loop;
    begin
      Argument.Get_Param_And_Pos (Arg, Pos, Occ, Key.Image);
      Put_Line_Output ("Argument : >" & Arg.Image & "<");
      for I in 1 .. Arg.Length loop
        Put_Line_Output ("Char ->" & Arg.Element (I) & "< " &
         Integer'Image (Character'Pos(Arg.Element (I))) );
      end loop;

      Put_Output (" found at position ");
      Put_Line_Output (Normal (Pos, 3));
    exception
      when Argument.Argument_Not_Found =>
        Put_Line_Output ("Exception ARGUMENT_NOT_FOUND raised.");
      when Argument.Argument_Too_Long =>
        Put_Line_Output ("Exception ARGUMENT_TOO_LONG raised.");
      when others =>
        Put_Line_Output ("Exception raised.");
    end;
    New_Line_Output;
  end loop;
end T_Arg;

