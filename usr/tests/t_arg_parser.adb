with Ada.Characters.Latin_1;
with As.U.Utils, Argument, Argument_Parser, Environ, Basic_Proc, Integer_Image,
     Sys_Calls, Command, Many_Strings, Parser, Event_Mng, Str_Util;

procedure T_Arg_Parser is

  -- ENV variables used internally
  -- The env var set for auto test
  Auto_Env_Name : constant String := "T_ARG_PARSER_AUTO_TEST";
  -- The env var to set for test of empty list of keys
  Empty_Env_Name : constant String := "T_ARG_PARSER_EMPTY_TEST";

  -- The keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   (False, 'f', As.U.Tus ("first"),  False),
   (False, 's', As.U.Tus ("second"), False),
   (False, 't', As.U.Tus ("third"),  False),
   (False, 'm', As.U.Tus ("multi"),  True),
   (True,  'o', As.U.Tus ("opt"),    False, True,  As.U.Asu_Null),
   (True,  'c', As.U.Tus ("combi"),  True,  False, As.U.Tus ("Opt")));

  -- The help message for each key
  Helps : constant As.U.Utils.Asu_Array (Keys'Range) := (
    As.U.Tus ("A first single key"),
    As.U.Tus ("A second single key"),
    As.U.Tus ("A third single key"),
    As.U.Tus ("Key that can appear several times"),
    As.U.Tus ("Key that can have options"),
    As.U.Tus ("Key that can appear several times and have options"));

  No_Keys : constant Argument_Parser.The_Keys_Type (1 .. 0)
          := (others => (False,
                         Argument_Parser.No_Key_Char,
                         Argument_Parser.No_Key_String,
                         False));
  Nb_Keys : Natural;

  Dscr : Argument_Parser.Parsed_Dscr;

  -- Put a string, an int
  procedure Put (S : String) renames Basic_Proc.Put_Output;
  procedure Put (I : Natural; Append_Sep : Boolean) is
  begin
    Put (Integer_Image (I));
    if Append_Sep then
      Put (" ");
    end if;
  end Put;

  -- When Try fails => stop
  Stop_Error : exception;

  -- Build Many_Strings command line
  function Build (Str : String) return Many_Strings.Many_String is
    Iter : Parser.Iterator;
    Res : Many_Strings.Many_String;
  begin
    Iter.Set (Str);
    loop
      declare
        Word : constant String := Iter.Next_Word;
      begin
        exit when Word = "";
        Res.Cat (Word);
      end;
    end loop;
    return Res;
  end Build;

  -- Check that Cmd produces Res
  procedure Try (Args : in String; Res : in String) is
    Cmd : constant Many_Strings.Many_String
        := Build ("./" & Argument.Get_Program_Name & " " & Args);
    Flow : aliased Command.Flow_Rec (Command.Str);
    Code : Command.Exit_Code_Range;
    Len : Natural;
  begin
    -- Execute command
    Command.Execute (Cmd, True, Command.Both,
                     Flow'Unrestricted_Access, Flow'Unrestricted_Access,
                     Code);
    if Event_Mng.Reset_Default_Signals_Policy then
       -- Command aborted
      Basic_Proc.Put_Line_Error ("Aborted");
      raise Stop_Error;
    end if;

    if Code = Command.Error then
      -- Command returns error
      Basic_Proc.Put_Line_Error ("ERROR: " & Flow.Str.Image);
      raise Stop_Error;
    end if;
    -- Raw mode appends [ a space ] and a line feed, remove them
    Len := Flow.Str.Length;
    if Len >= 1
    and then Flow.Str.Element (Len) = Ada.Characters.Latin_1.Lf then
      Flow.Str.Delete (Len, Len);
    end if;
    Len := Flow.Str.Length;
    if Len >= 1
    and then Flow.Str.Element (Len) = ' ' then
      Flow.Str.Delete (Len, Len);
    end if;
    -- Check result
    if Flow.Str.Image = Res then
      -- Expected result
      Basic_Proc.Put_Line_Output (Args & " -> " & Res);
    else
      Basic_Proc.Put_Line_Error ("ERROR: " & Args);
      Basic_Proc.Put_Line_Error (" ----->" & Flow.Str.Image & "<");
      Basic_Proc.Put_Line_Error (" i.o. >" & Res & "<");
      raise Stop_Error;
    end if;
  exception
    when Command.Terminate_Request =>
      Basic_Proc.Put_Line_Error ("Aborted");
      raise Stop_Error;
    when Command.Spawn_Error =>
      Basic_Proc.Put_Line_Error ("ERROR: Spawn error");
      raise Stop_Error;
  end Try;

begin
  if Argument.Get_Nbre_Arg /= 1 or else Argument.Get_Parameter /= "auto" then

    -- Not test mode: parse arguments and output result
    if Environ.Is_Yes (Empty_Env_Name) then
      -- Test with empty list of keys
      if not Environ.Is_Set (Auto_Env_Name)
      or else not Environ.Is_Yes (Auto_Env_Name) then
        Basic_Proc.Put_Line_Output ("Parsing with no keys");
      end if;
      Dscr :=  Argument_Parser.Parse (No_Keys);
      Nb_Keys := No_Keys'Length;
    else
      Dscr :=  Argument_Parser.Parse (Keys);
      Nb_Keys := Keys'Length;
    end if;

    if not Environ.Is_Set (Auto_Env_Name)
    or else not Environ.Is_Yes (Auto_Env_Name) then
      -- Verbose output
      for I in 1 .. Nb_Keys loop
        Basic_Proc.Put_Line_Output (
           Str_Util.Procuste (Argument_Parser.Image (Keys(I)), 28)
           & " " & Helps(I).Image);
      end loop;

      Basic_Proc.Put_Output ("Parsing OK is "
         & Boolean'Image (Dscr.Is_Ok));
      Basic_Proc.Put_Output (" and parsing error string is");
      if not Dscr.Is_Ok then
        Basic_Proc.New_Line_Output;
      end if;
      Basic_Proc.Put_Line_Output (" >" & Dscr.Get_Error & "<");

      Basic_Proc.Put_Line_Output (
         "Number of keys found:" & Dscr.Get_Number_Keys'Img
         & ", Last key at pos:" & Dscr.Get_Last_Pos_Of_Keys'Img
         & ", First after at pos:" & Dscr.Get_First_Pos_After_Keys'Img
         & " and Nb embedded arguments:" & Dscr.Get_Nb_Embedded_Arguments'Img);

      for I in 0 .. Nb_Keys loop
        if I = 0 then
          Basic_Proc.Put_Output ("Arguments not key are");
        else
          Basic_Proc.Put_Output ("Key " & Keys(I).Key_Char & " "
            & Keys(I).Key_String.Image & " is");
        end if;
        Basic_Proc.Put_Line_Output (" found on" & Dscr.Get_Nb_Occurences (I)'Img
             & " occurences.");
        for J in 1 .. Dscr.Get_Nb_Occurences(I) loop
          Basic_Proc.Put_Output ("  Kind: ");
          if Dscr.Is_Char (I, J) then
            Basic_Proc.Put_Output ("Chr");
          else
            Basic_Proc.Put_Output ("Str");
          end if;
          Basic_Proc.Put_Line_Output ("  Position: " &  Dscr.Get_Position (I, J)'Img
                              & "  Option >" & Dscr.Get_Option (I, J) & "<");
        end loop;
      end loop;
    else
      -- Raw output
      if not Dscr.Is_Ok then
        -- Parsing error
        Basic_Proc.Put_Line_Output (Dscr.Get_Error);
        Dscr.Reset;
        return;
      end if;

      -- Common data
      Put (Dscr.Get_Number_Keys, True);
      Put (Dscr.Get_Last_Pos_Of_Keys, True);
      Put (Dscr.Get_First_Pos_After_Keys, True);
      Put (Dscr.Get_Nb_Embedded_Arguments, True);

      -- All about keys
      for I in 1 .. Nb_Keys loop
        if Dscr.Get_Nb_Occurences (I) /= 0 then
          Put ("" & Keys(I).Key_Char);
          Put (Dscr.Get_Nb_Occurences (I), True);
          for J in 1 .. Dscr.Get_Nb_Occurences(I) loop
             Put (Dscr.Get_Position (I, J), True);
             if Keys(I).Key_Can_Option
             and then Dscr.Get_Option (I, J) /= "" then
               Put (Dscr.Get_Option (I, J) & " ");
             end if;
          end loop;
        end if;
      end loop;

      -- Other args
      for I in 1 .. Dscr.Get_Nb_Occurences(Argument_Parser.No_Key_Index) loop
        Put (Dscr.Get_Position (Argument_Parser.No_Key_Index, I), True);
        Put (Dscr.Get_Option (Argument_Parser.No_Key_Index, I) & " ");
      end loop;

      -- Done
      Basic_Proc.New_Line_Output;

    end if;

    Dscr.Reset;
    return;
  end if;

  -- Auto test
  -- Expected result is:
  -- 4 ints: Number_Keys Last_Pos_Of_Keys First_Pos_After_Keys Nb_Embedded
  -- For each key: <Char><Nb_Occurences> { <Position> [ <Option> ] }
  -- For each arg: <Position> <Arg>
  Sys_Calls.Setenv (Auto_Env_Name, "Yes");
  Basic_Proc.Put_Line_Output ("Test with no key");
  Sys_Calls.Setenv (Empty_Env_Name, "Yes");
  Try ("", "0 0 0 0");
  Try ("f1 f2", "0 0 1 0 1 f1 2 f2");
  Try ("f1 -o f2", "Argument -o at pos 2 is not expected");

  Basic_Proc.Put_Line_Output ("Test with keys");
  Sys_Calls.Setenv (Empty_Env_Name, "No");
  Try ("", "0 0 0 0");
  Try ("-o", "Argument -o at pos 1 must have an option");
  Try ("-o opt", "1 1 0 0 o1 1 opt");
  Try ("-o opt1 -o opt2", "Argument -o at pos 3 appears several times");
  Try ("-x -o opt", "Argument -x at pos 1 is not expected");
  Try ("-o opt -f f1 f2", "2 3 4 0 f1 3 o1 1 opt 4 f1 5 f2");
  Try ("-o opt f1 -- -f f1 f2", "1 1 3 0 o1 1 opt 3 f1 5 -f 6 f1 7 f2");
  Try ("-ftso opt f1 f2 f3", "4 1 3 0 f1 1 s1 1 t1 1 o1 1 opt 3 f1 4 f2 5 f3");
  Try ("-m arg1 -m arg2 -- -m arg3",
       "2 3 4 1 m2 1 3 2 arg1 4 arg2 6 -m 7 arg3");
  Try ("-m arg1 -m -- arg2 -m arg3",
       "2 3 5 1 m2 1 3 2 arg1 5 arg2 6 -m 7 arg3");
  Try ("-f arg1 -s arg2 -o opt -- -t arg3 arg4",
       "3 5 8 2 f1 1 s1 3 o1 5 opt 2 arg1 4 arg2 8 -t 9 arg3 10 arg4");
  Try ("-f arg1 -c opt1 -o opt2 -c opt3 -- -t arg3",
       "4 7 10 1 f1 1 o1 5 opt2 c2 3 opt1 7 opt3 2 arg1 10 -t 11 arg3");


  Basic_Proc.Put_Line_Output ("Done");
exception
  when Stop_Error =>
    Basic_Proc.Set_Error_Exit_Code;
  when Argument_Parser.Parsing_Error =>
    Basic_Proc.Put_Line_Error ("Exception Parsing_Error.");
    Basic_Proc.Set_Error_Exit_Code;
end T_Arg_Parser;

