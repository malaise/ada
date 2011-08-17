with Basic_Proc, Sys_Calls, Normal, Text_Line, As.U, Regular_Expressions,
     Argument, String_Mng;
procedure T_Str_Error is
  Flow : Text_Line.File_Type;
  Pat : Regular_Expressions.Compiled_Pattern;
  Ok : Boolean;
  Line : As.U.Asu_Us;
  Index : Natural;
  Code : Positive;
  Max_Len : constant := 15;
begin

  if Argument.Get_Nbre_Arg /= 0 then
    -- Any arg => Do not process inpout flow
    for I in 1 .. 256 loop
      exit when Sys_Calls.Str_Error(I) = "";
      Basic_Proc.Put_Line_Output (Normal(I, 3) & " -> "
                                & Sys_Calls.Str_Error(I) );
    end loop;
    return;
  end if;

  -- Open filow on stdin
  Flow.Open (Text_Line.In_File, Sys_Calls.Stdin);

  -- Compile regexp for expected text
  Regular_Expressions.Compile (Pat, Ok, "^[0-9]+ [A-Z]+$");
  if not Ok then
    Basic_Proc.Put_Line_Error ("Invalid regex: "
          & Regular_Expressions.Error (Pat) & '.');
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Read stdin, check if no data at all
  loop
    Line := Flow.Get;
    exit when Line.Is_Null;
    Text_Line.Trim (Line);
    Regular_Expressions.Exec (Pat, Line.Image, Index,
                              Regular_Expressions.No_Match_Array);
    if Index /= 0 then
      -- Valid line
      Index := Line.Locate (" ");
      begin
        Code := Positive'Value (Line.Slice (1, Index - 1));
      exception
        when Constraint_Error =>
          Index := 0;
      end;
      if Index /= 0 then
        Basic_Proc.Put_Line_Output (
            Normal(Code, 3) & " "
          & String_Mng.Procuste (Line.Slice (Index + 1, Line.Length), Max_Len)
          & " -> "
          & Sys_Calls.Str_Error(Code) );
      end if;
    end if;

  end loop;

  -- Close flow
  Flow.Close;

end T_Str_Error;
