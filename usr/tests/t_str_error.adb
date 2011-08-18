with Basic_Proc, Sys_Calls, Normal, Text_Line, As.U, Regular_Expressions,
     Argument, String_Mng, Unbounded_Arrays;
procedure T_Str_Error is
  -- Input flow and line read
  Flow : Text_Line.File_Type;
  Line : As.U.Asu_Us;
  -- Paterns to match
  Patc, Pata : Regular_Expressions.Compiled_Pattern;
  Ok : Boolean;
  -- Index to split line and mnemonic
  Index : Natural;

  -- Max len of mnemonic
  Max_Len : constant := 15;
  -- Max value of errno
  subtype Code_Range is Positive;
  Code : Code_Range;

  -- Unbounded array of aliases
  type Alias_Rec is record
    Mnemo, Target : As.U.Asu_Us;
  end record;
  type Alias_Array is array (Positive range <>) of Alias_Rec;
  package Alias_Mng is new Unbounded_Arrays (Alias_Rec, Alias_Array);
  Aliases : Alias_Mng.Unb_Array;

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

  -- Open flow on stdin
  Flow.Open (Text_Line.In_File, Sys_Calls.Stdin);

  -- Compile regexps for expected text
  -- code mnemonic
  Regular_Expressions.Compile (Patc, Ok, "^[0-9]+ [A-Z]+$");
  if not Ok then
    Basic_Proc.Put_Line_Error ("Invalid regex: "
          & Regular_Expressions.Error (Patc) & '.');
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  -- mnemonic mnemonic
  Regular_Expressions.Compile (Pata, Ok, "^[A-Z]+ [A-Z]+$");
  if not Ok then
    Basic_Proc.Put_Line_Error ("Invalid regex: "
          & Regular_Expressions.Error (Pata) & '.');
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Read stdin, put definitions on the flow and store aliases
  Basic_Proc.Put_Line_Output ("Errno:");
  loop
    Line := Flow.Get;
    exit when Line.Is_Null;
    Text_Line.Trim (Line);
    Regular_Expressions.Exec (Patc, Line.Image, Index,
                              Regular_Expressions.No_Match_Array);
    if Index /= 0 then
      -- Valid line "code mnemo"
      Index := Line.Locate (" ");
      begin
        Code := Positive'Value (Line.Slice (1, Index - 1));
      exception
        when Constraint_Error =>
          Index := 0;
      end;
      if Index /= 0 then
        -- Put code, menmo and text
        Basic_Proc.Put_Line_Output (
            Normal(Code, 3) & " "
          & String_Mng.Procuste (Line.Slice (Index + 1, Line.Length), Max_Len)
          & " -> "
          & Sys_Calls.Str_Error(Code) );
      end if;
    else
      Regular_Expressions.Exec (Pata, Line.Image, Index,
                                Regular_Expressions.No_Match_Array);
      if Index /= 0 then
        -- Valid line "mnemo mnemo"
        -- Insert alias definition
        Index := Line.Locate (" ");
        Aliases.Append ( Alias_Rec'(Line.Uslice (1, Index - 1),
                                    Line.Uslice (Index + 1, Line.Length)) );

      end if;
    end if;

  end loop;

  -- Put Aliases
  Basic_Proc.Put_Line_Output ("Aliases:");
  for I in 1 .. Aliases.Length loop
    Basic_Proc.Put_Line_Output (
      String_Mng.Procuste (Aliases.Element(I).Mnemo.Image, Max_Len)
     & " = " & Aliases.Element(I).Target.Image);
  end loop;

  -- Close flow
  Flow.Close;

end T_Str_Error;

