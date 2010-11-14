with Ada.Text_Io, Ada.Characters.Latin_1;
with As.U; use As.U;
with Argument, Argument_Parser, Environ, Basic_Proc, Int_Image, Sys_Calls,
     Command, Many_Strings, Parser, Event_Mng;

procedure T_Arg_Parser is

  -- Image of Natural
  function Image is new Int_Image (Natural);

  -- The env var set on auto test
  Auto_Env_Name : constant String := "T_ARG_PARSER_AUTO_TEST";
  -- The env var to set vor test of empty list of keys
  Empty_Env_Name : constant String := "T_ARG_PARSER_EMPTY_TEST";

  -- The keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   (Key_Char => 'f', Key_String => Asu_Tus ("first"),
    Key_Can_Multiple => False, Key_Can_Option => False),
   (Key_Char => 's', Key_String => Asu_Tus ("second"),
    Key_Can_Multiple => False, Key_Can_Option => False),
   (Key_Char => 't', Key_String => Asu_Tus ("third"),
    Key_Can_Multiple => False, Key_Can_Option => False),
   (Key_Char => 'm', Key_String => Asu_Tus ("multi"),
    Key_Can_Multiple => True, Key_Can_Option => False),
   (Key_Char => 'o', Key_String => Asu_Tus ("opt"),
    Key_Can_Multiple => False, Key_Can_Option => True),
   (Key_Char => 'c', Key_String => Asu_Tus ("combine"),
    Key_Can_Multiple => True, Key_Can_Option => True) );

  No_Keys : constant Argument_Parser.The_Keys_Type (1 .. 0)
          := (others => (Argument_Parser.No_Key_Char,
                         Argument_Parser.No_Key_String,
                         False, False));
  Nb_Keys : Natural;

  Dscr : Argument_Parser.Parsed_Dscr;

  -- Put a string, an int
  procedure Put (S : String) renames Basic_Proc.Put_Output;
  procedure Put (I : Natural; Append_Sep : Boolean) is
  begin
    Put (Image (I));
    if Append_Sep then
      Put (" ");
    end if;
  end Put;

  -- When Try fails => stop
  Stop_Error : exception;

  -- Build Many_Strings command line
  function Build (Str : String) return Many_Strings.Many_String is
    Iter : Parser.Iterator;
    Res : Asu_Us;
  begin
    Iter.Set (Str);
    loop
      declare
        Word : constant String := Iter.Next_Word;
      begin
        exit when Word = "";
        Many_Strings.Cat (Res, Word);
      end;
    end loop;
    return Asu_Ts (Res);
  end Build;

  -- Check that Cmd produces Res
  procedure Try (Args : in String; Res : in String) is
    Cmd : constant String := Build ("./" & Argument.Get_Program_Name
                                  & " " & Args);
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
      Basic_Proc.Put_Line_Error ("ERROR: " & Asu_Ts (Flow.Str));
      raise Stop_Error;
    end if;
    -- Raw mode appends [ a space ] and a line feed, remove them
    Len := Asu.Length (Flow.Str);
    if Len >= 1
    and then Asu.Element (Flow.Str, Len) = Ada.Characters.Latin_1.Lf then
      Asu.Delete (Flow.Str, Len, Len);
    end if;
    Len := Asu.Length (Flow.Str);
    if Len >= 1
    and then Asu.Element (Flow.Str, Len) = ' ' then
      Asu.Delete (Flow.Str, Len, Len);
    end if;
    -- Check result
    if Asu_Ts (Flow.Str) = Res then
      -- Expected result
      Basic_Proc.Put_Line_Output (Args & " -> " & Res);
    else
      Basic_Proc.Put_Line_Error ("ERROR: " & Args);
      Basic_Proc.Put_Line_Error (" ----->" & Asu_Ts (Flow.Str) & "<");
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
        Ada.Text_Io.Put_Line ("Parsing with no keys");
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
      Ada.Text_Io.Put ("Parsing OK is "
         & Boolean'Image (Dscr.Is_Ok));
      Ada.Text_Io.Put (" and parsing error string is");
      if not Dscr.Is_Ok then
        Ada.Text_Io.New_Line;
      end if;
      Ada.Text_Io.Put_Line (" >" & Dscr.Get_Error & "<");

      Ada.Text_Io.Put_Line (
         "Number of keys found:" & Dscr.Get_Number_Keys'Img
         & ", Last key at pos:" & Dscr.Get_Last_Pos_Of_Keys'Img
         & ", First after at pos:" & Dscr.Get_First_Pos_After_Keys'Img
         & " and Nb embedded arguments:" & Dscr.Get_Nb_Embedded_Arguments'Img);

      for I in 0 .. Nb_Keys loop
        if I = 0 then
          Ada.Text_Io.Put ("Arguments not key are");
        else
          Ada.Text_Io.Put ("Key " & Keys(I).Key_Char & " "
            & Asu_Ts (Keys(I).Key_String) & " is");
        end if;
        Ada.Text_Io.Put_Line (" found on" & Dscr.Get_Nb_Occurences (I)'Img
             & " occurences.");
        for J in 1 .. Dscr.Get_Nb_Occurences(I) loop
          Ada.Text_Io.Put ("  Kind: ");
          if Dscr.Is_Char (I, J) then
            Ada.Text_Io.Put ("Chr");
          else
            Ada.Text_Io.Put ("Str");
          end if;
          Ada.Text_Io.Put_Line ("  Position: " &  Dscr.Get_Position (I, J)'Img
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
  Try ("-o", "1 1 0 0 o1 1");
  Try ("-o opt", "1 1 0 0 o1 1 opt");
  Try ("-o opt -o", "Argument -o at pos 3 appears several times");
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
    Ada.Text_Io.Put_Line ("Exception Parsing_Error.");
    Basic_Proc.Set_Error_Exit_Code;
end T_Arg_Parser;

