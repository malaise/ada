with Ada.Characters.Latin_1, Ada.Exceptions;
with As.U.Utils, Basic_Proc, Str_Util.Regex, Many_Strings, Gets,
     Upper_Str, Lower_Str, Mixed_Str, Upper_Char, Environ, Argument;
procedure T_String is

  Action : Natural;
  Str : String(1 .. 500);
  Str_Len : Natural;

  Pos1 : Positive;
  Pos2 : Positive;
  Pos3 : Positive;
  Nat1 : Natural;
  Nat2 : Natural;
  Nat3 : Natural;
  Nat4 : Natural;
  Nat5 : Natural;
  Char1 : Character;
  Bool1 : Boolean;
  Bool2 : Boolean;
  Bool3 : Boolean;
  Str1 : String(1 .. 500);
  Str2 : String(1 .. 500);

  Search_Result : Str_Util.Regex.Search_Result;

  procedure Nat_Get (V : out Natural; Allow_Zero : in Boolean) is
  begin
    loop
      begin
        V := Gets.Get_Int (Basic_Proc.Get_Line);
        if V = 0 and then not Allow_Zero then
          raise Constraint_Error;
        end if;
        return;
      exception
        when others => null;
      end;
      if Allow_Zero then
        Basic_Proc.Put_Output ("Enter a Natural ? ");
      else
        Basic_Proc.Put_Output ("Enter a Positive ? ");
      end if;
    end loop;
  end Nat_Get;

  procedure Bool_Get (V : out Boolean) is
    Str : constant String := Mixed_Str (Basic_Proc.Get_Line);
  begin
    loop
      if Str = "True" or else Str = "T"
      or else Str = "Yes" or else Str = "Y" then
        V := True;
        return;
      elsif Str = "False" or else Str = "F"
      or else Str = "No" or else Str = "N" then
        V := False;
        return;
      end if;
      Basic_Proc.Put_Output ("Yes or No ? ");
    end loop;
  end Bool_Get;

begin

  if Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter = "-a" then
    Str := (others => '#');
    Str(200 .. 209) := "0123456789";
    Str(301 .. 326) := "abcdefghijklmnopqrstuvwxyz";
    Str(401 .. 426) := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    -- Generate the same output as t_asu and t_asb for the operations
    --  that have no sense with strings
    Basic_Proc.Put_Line_Output ("Empty array:");
    Basic_Proc.Put_Line_Output ("Length 0");
    Basic_Proc.Put_Line_Output ("Image ");
    Basic_Proc.Put_Line_Output ("Array of 3:");
    Basic_Proc.Put_Line_Output ("Image 3");
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Array of 1, 3, 5:");
    Basic_Proc.Put_Line_Output ("Length 3");
    Basic_Proc.Put_Line_Output ("Image 135");
    Basic_Proc.Put_Line_Output ("Element 2: 3");
    Basic_Proc.Put_Line_Output ("Replaced by u: u");
    Basic_Proc.Put_Line_Output ("Image 1u5");
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Append a, b, c, d, e");
    Basic_Proc.Put_Line_Output ("Image 1u5abcde");
    Basic_Proc.Put_Line_Output ("Same with concat");
    Basic_Proc.Put_Line_Output ("Image 1u5abcde");
    Basic_Proc.Put_Line_Output ("Same with reverse concat");
    Basic_Proc.Put_Line_Output ("Image 1u5abcde");
    Basic_Proc.Put_Line_Output ("Same with prepend");
    Basic_Proc.Put_Line_Output ("Image 1u5abcde");
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Slice 4 .. 6");
    Basic_Proc.Put_Line_Output ("Slice string abc");
    Basic_Proc.Put_Line_Output ("Uslice Image abc");
    Basic_Proc.Put_Line_Output ("Check ""="" OK");
    Basic_Proc.New_Line_Output;
    -- Test Overwrite, Replace, Insert and Delete
    Basic_Proc.Put_Line_Output ("Replace from 4 to 7 with B, C");
    Basic_Proc.Put_Line_Output ("Replace from 1 to 3 with a, b, c, d");
    Basic_Proc.Put_Line_Output ("Replace from 5 to 7 with e");
    declare
      N1 : constant String := "1u5abcdE";
      N2 : constant String := Str_Util.Replace (N1, 4, 7, Str(402 .. 403));
      N3 : constant String := Str_Util.Replace (N2, 1, 3, Str(301 .. 304));
      N4 : constant String := Str_Util.Replace (N3, 5, 7, Str(305 .. 305));
    begin
      Basic_Proc.Put_Line_Output ("Image " & N4);
    end;
    Basic_Proc.Put_Line_Output ("Overwrite from 4 with 4, 5");
    Basic_Proc.Put_Line_Output ("Overwrite from 6 with 6, 7");
    Basic_Proc.Put_Line_Output ("Overwrite from 1 with 1, 2, 3");
    Basic_Proc.Put_Line_Output ("Overwrite from 8 with 8");
    declare
      N1 : constant String := "abcde";
      N2 : constant String := Str_Util.Overwrite (N1, 4, Str(204 .. 205));
      N3 : constant String := Str_Util.Overwrite (N2, 6, Str(206 .. 207));
      N4 : constant String := Str_Util.Overwrite (N3, 1, Str(201 .. 203));
      N5 : constant String := Str_Util.Overwrite (N4, 8, Str(208 .. 208));
    begin
      Basic_Proc.Put_Line_Output ("Image " & N5);
    end;
    Basic_Proc.Put_Line_Output ("Replace from 1 to 0 with 0");
    Basic_Proc.Put_Line_Output ("Replace from 3 to 1 with a, b");
    Basic_Proc.Put_Line_Output ("Replace from 12 to 11 with y, z");
    declare
      N1 : constant String := "12345678";
      N2 : constant String := Str_Util.Replace (N1, 1, 0, Str(200 .. 200));
      N3 : constant String := Str_Util.Replace (N2, 3, 1, Str(301 .. 302));
      N4 : constant String := Str_Util.Replace (N3, 12, 11, Str(325 .. 326));
    begin
      Basic_Proc.Put_Line_Output ("Image " & N4);
    end;
    Basic_Proc.Put_Line_Output ("Delete from 3 to 4, insert a, b, c, d before 4");
    Basic_Proc.Put_Line_Output ("Replace from 4 to 5 with B, C, D");
    Basic_Proc.Put_Line_Output ("Delete from 4 to 8");
    Basic_Proc.Put_Line_Output ("Trail 2");
    Basic_Proc.Put_Line_Output ("Insert 9 before 10");
    Basic_Proc.Put_Line_Output ("Replace from 11 to 10 with A, B, C");
    Basic_Proc.Put_Line_Output ("Overwrite from 14 with D, E, F");
    declare
      N1 : constant String := "01ab2345678yz";
      N2 : constant String := Str_Util.Delete (N1, 3, 4);
      N3 : constant String := Str_Util.Insert (N2, 4, Str(301 .. 304));
      N4 : constant String := Str_Util.Replace (N3, 4, 5, Str(402 .. 404));
      N5 : constant String := Str_Util.Delete (N4, 4, 8);
      N6 : constant String := Str_Util.Cut (N5, 2, False);
      N7 : constant String := Str_Util.Insert (N6, 10, Str(209 .. 209));
      N8 : constant String := Str_Util.Replace (N7, 11, 10, Str(401 .. 403));
      N9 : constant String := Str_Util.Overwrite (N8, 14, Str(404 .. 406));
    begin
      Basic_Proc.Put_Line_Output ("Image " & N9);
    end;
    Basic_Proc.Put_Line_Output ("Delete 2 from 2 then 5 from 11");
    declare
      N1 : constant String := "0123456789ABCDEF";
      N2 : constant String := Str_Util.Delete_Nb (N1, 2, 2);
      N3 : constant String := Str_Util.Delete_Nb (N2, 11, 5);
    begin
      Basic_Proc.Put_Line_Output ("Image " & N3);
    end;

    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Check Finalization");
    Basic_Proc.Put_Line_Output ("Array of B, C");
    Basic_Proc.Put_Line_Output ("Image BC");
    Basic_Proc.Put_Line_Output ("Done.");
    return;
  end if;

  loop
    Basic_Proc.Put_Output ("Str (String)? ");
    Basic_Proc.Get_Line (Str, Str_Len);

    loop
      Basic_Proc.New_Line_Output;
      Basic_Proc.Put_Line_Output (
       "String: |0        1         2         3         4         5         6");
      Basic_Proc.Put_Line_Output (
       "String: |123456789012345678901234567890123456789012345678901234567890");
      Basic_Proc.Put_Line_Output (
       "String: |" & Str(1 .. Str_Len) & "|   len: "
                    & Integer'Image(Str_Len));

      Basic_Proc.Put_Line_Output ("Main menu");
      Basic_Proc.Put_Line_Output (" 0 Exit to change String");
      Basic_Proc.Put_Line_Output (" 1 Case conversion");
      Basic_Proc.Put_Line_Output (" 2 Strip");
      Basic_Proc.Put_Line_Output (" 2 Parse spaces - DISCARDED");
      Basic_Proc.Put_Line_Output (" 3 Procuste");
      Basic_Proc.Put_Line_Output (" 4 Locate (fragment)");
      Basic_Proc.Put_Line_Output (" 5 Remove (substring)");
      Basic_Proc.Put_Line_Output (" 6 (Extract) Slice");
      Basic_Proc.Put_Line_Output (" 7 Cut (head or tail)");
      Basic_Proc.Put_Line_Output (" 8 Extract (head or tail)");
      Basic_Proc.Put_Line_Output (" 9 Swap");
      Basic_Proc.Put_Line_Output ("10 Unique (from head or tail)");
      Basic_Proc.Put_Line_Output ("11 Variable substitution");
      Basic_Proc.Put_Line_Output ("12 Escape location");
      Basic_Proc.Put_Line_Output ("13 Tuncate at best length");
      Basic_Proc.Put_Line_Output ("14 Copy");
      Basic_Proc.Put_Line_Output ("15 Substit");
      Basic_Proc.Put_Line_Output ("16 Normalize");
      Basic_Proc.Put_Line_Output ("17 Regex locate");
      Basic_Proc.Put_Line_Output ("18 Regex substit");
      Basic_Proc.Put_Line_Output ("19 Split");
      Basic_Proc.Put_Line_Output ("20 Regex split");
      Basic_Proc.Put_Line_Output ("21 Center");
      Basic_Proc.Put_Line_Output ("22 Regex split on sep");
      Basic_Proc.Put_Line_Output ("23 Overwrite");
      Basic_Proc.Put_Line_Output ("24 Replace");
      Basic_Proc.Put_Line_Output ("25 Insert");
      Basic_Proc.Put_Line_Output ("26 Delete");
      Basic_Proc.Put_Line_Output ("26 Delete_nb");
      Basic_Proc.Put_Line_Output ("28 Check char is backslashed");

      Basic_Proc.Put_Output ("Choice (0 .. 28) ? "); Nat_Get (Action, True);
      Basic_Proc.New_Line_Output;

      begin
        case Action is
          when  0 =>
            exit;
          when  1 =>
            Basic_Proc.Put_Output ("Case conversion (ULM) ? ");
            Basic_Proc.Get (Char1);
            Basic_Proc.Skip_Line;
            Char1 := Upper_Char (Char1);
            if Char1 = 'U' then
              Basic_Proc.Put_Line_Output (Upper_Str (Str(1 .. Str_Len)));
            elsif Char1 = 'L' then
              Basic_Proc.Put_Line_Output (Lower_Str (Str(1 .. Str_Len)));
            elsif Char1 = 'M' then
              Basic_Proc.Put_Line_Output (Mixed_Str (Str(1 .. Str_Len)));
            else
              Basic_Proc.Put_Line_Output ("Discarded.");
            end if;

          when  2 =>
            Basic_Proc.Put_Line_Output ("Strip");
            Basic_Proc.Put_Output ("From Head/Tail/Both (HTB)? "); Basic_Proc.Get(Char1);
            Basic_Proc.Skip_Line;
            Char1 := Upper_Char (Char1);
            if Char1 = 'H' then
              Basic_Proc.Put_Line_Output ("|"
                            & Str_Util.Strip (Str(1 .. Str_Len), Str_Util.Head)
                            & "|");
            elsif Char1 = 'T' then
              Basic_Proc.Put_Line_Output ("|"
                            & Str_Util.Strip (Str(1 .. Str_Len), Str_Util.Tail)
                            & "|");
            elsif Char1 = 'B' then
              Basic_Proc.Put_Line_Output ("|"
                            & Str_Util.Strip (Str(1 .. Str_Len), Str_Util.Both)
                            & "|");
            else
              Basic_Proc.Put_Line_Output ("Discarded.");
            end if;

          when  3 =>
            Basic_Proc.Put_Line_Output ("Procuste");
            Basic_Proc.Put_Output ("Len (Pos)? "); Nat_Get(Pos1, False);
            Basic_Proc.Put_Output ("Align_Left (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Output ("Gap (Char)? "); Basic_Proc.Get(Char1); Basic_Proc.Skip_Line;
            Basic_Proc.Put_Output ("Trunc_Head (YN)? "); Bool_Get(Bool2);
            Basic_Proc.Put_Output ("Show_Trunc (YN)? "); Bool_Get(Bool3);

            Basic_Proc.Put_Line_Output (
                "Procuste: |"
              & Str_Util.Procuste(Str(1 .. Str_Len),
                       Len => Pos1,
                       Align_Left => Bool1,
                       Gap => Char1,
                       Trunc_Head => Bool2,
                       Show_Trunc => Bool3)
              & "|" );

          when  4 =>
            Basic_Proc.Put_Line_Output ("Locate");
            Basic_Proc.Put_Output ("Fragment (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Output ("From_Index (Nat)? "); Nat_Get(Nat2, True);
            Basic_Proc.Put_Output ("Forward (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Output ("Occurence (Pos)? "); Nat_Get(Pos1, False);
            Basic_Proc.Put_Line_Output ("Occurence of fragment located at: " &
             Integer'Image (Str_Util.Locate (
               Str(1 .. Str_Len),
               Fragment => Str1(1 .. Nat1),
               From_Index => Nat2,
               Forward => Bool1,
               Occurence => Pos1)) );

          when  5 =>
            Basic_Proc.Put_Line_Output ("Remove (substring)");
            Basic_Proc.Put_Output ("At_Index (Pos)? "); Nat_Get(Pos1, False);
            Basic_Proc.Put_Output ("Nb_Char (Nat)? "); Nat_Get(Nat1, True);
            Basic_Proc.Put_Output ("Shift_Left (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Output ("Gap (Char, n for none)? ");
                       Basic_Proc.Get(Char1); Basic_Proc.Skip_Line;
            if Char1 = 'n' then Char1 := Ada.Characters.Latin_1.Nul; end if;
            Basic_Proc.Put_Line_Output ("Remaining string: |"
              & Str_Util.Remove (Str(1 .. Str_Len),
                 At_Index => Pos1,
                 Nb_Char => Nat1,
                 Shift_Left => Bool1,
                 Gap => Char1) & "|" );

          when  6 =>
            Basic_Proc.Put_Line_Output ("(Extract) Slice");
            Basic_Proc.Put_Output ("At_Index (Pos)? "); Nat_Get(Pos1, False);
            Basic_Proc.Put_Output ("Nb_Char (Nat)? "); Nat_Get(Nat1, True);
            Basic_Proc.Put_Output ("To_Right (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Line_Output ("Extracted slice: |"
              & Str_Util.Slice (Str(1 .. Str_Len),
                 At_Index => Pos1,
                 Nb_Char => Nat1,
                 To_Right => Bool1) & "|" );

          when  7 =>
            Basic_Proc.Put_Line_Output ("Cut (head or tail)");
            Basic_Proc.Put_Output ("Nb_Char (Nat)? "); Nat_Get(Nat1, True);
            Basic_Proc.Put_Output ("Head (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Line_Output ("Cut string: |"
              & Str_Util.Cut (Str(1 .. Str_Len),
                 Nb_Char => Nat1,
                 Head => Bool1) & "|" );

          when  8 =>
            Basic_Proc.Put_Line_Output ("Extract (head or tail)");
            Basic_Proc.Put_Output ("Nb_Char (Nat)? "); Nat_Get(Nat1, True);
            Basic_Proc.Put_Output ("Head (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Line_Output ("Extracted: |"
              & Str_Util.Extract (Str(1 .. Str_Len),
                 Nb_Char => Nat1,
                 Head => Bool1) & "|" );

          when  9 =>
            Basic_Proc.Put_Line_Output ("Swap");
            Basic_Proc.Put_Line_Output ("Swapped: |"
              & Str_Util.Swap (Str(1 .. Str_Len)) & "|" );

          when 10 =>
            Basic_Proc.Put_Line_Output ("Unique (from head or tail)");
            Basic_Proc.Put_Output ("From head (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Line_Output ("Uniqued: |"
              & Str_Util.Unique (Str(1 .. Str_Len),
                                   From_Head => Bool1) & "|" );

          when 11 =>
            Basic_Proc.Put_Line_Output ("Env variable substitution");
            Basic_Proc.Put_Output ("Start delimiter (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Output ("Stop delimiter (Str)? ");  Basic_Proc.Get_Line (Str2, Nat2);
            Basic_Proc.Put_Output ("Multiple passes (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Output ("No check of stop (YN)? "); Bool_Get(Bool2);
            Basic_Proc.Put_Output ("Skip backslashed (YN)? "); Bool_Get(Bool3);
            Basic_Proc.Put_Line_Output ("Substitued: |"
              & Str_Util.Eval_Variables (
                        Str(1 .. Str_Len),
                        Start_Delimiter => Str1(1 .. Nat1),
                        Stop_Delimiter => Str2(1 .. Nat2),
                        Resolv => Environ.Getenv'Access,
                        Muliple_Passes => Bool1,
                        No_Check_Stop => Bool2,
                        Skip_Backslashed => Bool3)
              & "|" );

          when 12 =>
            Basic_Proc.Put_Line_Output ("Escape sequence location");
            Basic_Proc.Put_Output ("From index (Pos)? "); Nat_Get(Pos1, False);
            Basic_Proc.Put_Output ("Escaped (Esc char first) (Str)? ");
                       Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Line_Output ("Located at: "
              & Natural'Image (Str_Util.Locate_Escape (
                        Str(1 .. Str_Len),
                        From_Index => Pos1,
                        Escape => Str1(1 .. Nat1))));

          when 13 =>
            Basic_Proc.Put_Line_Output ("13 Tuncation ot best length");
            Basic_Proc.Put_Output ("Length (Pos)? "); Nat_Get(Pos1, False);
            Basic_Proc.Put_Output ("Mini (Pos)? ");   Nat_Get(Pos2, False);
            Basic_Proc.Put_Output ("Maxi (Pos)? ");   Nat_Get(Pos3, False);
            Basic_Proc.Put_Line_Output ("Truncate result: "
              & Str_Util.Truncate (
                        Str(1 .. Str_Len),
                        Length => Pos1,
                        Mini => Pos2,
                        Maxi => Pos3,
                        Separating => Str_Util.Is_Separator'Access));

          when 14 =>
            Basic_Proc.Put_Line_Output ("14 Copy");
            Basic_Proc.Put_Output ("Val (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            declare
              Lstr : String (1 .. Str_Len) := Str (1 .. Str_Len);
            begin
              Str_Util.Copy (Val => Str1(1 .. Nat1),
                               To => Lstr);
              Basic_Proc.Put_Line_Output ("Copy result: " & Lstr);
            end;
          when 15 =>
            Basic_Proc.Put_Line_Output ("15 Replace");
            Basic_Proc.Put_Output ("What (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Output ("By (Str)? "); Basic_Proc.Get_Line (Str2, Nat2);
            Basic_Proc.Put_Output ("Skip_Backslashed (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Line_Output ("Replaced string: "
              & Str_Util.Substit (Str(1 .. Str_Len),
                                    What => Str1(1 .. Nat1),
                                    By => Str2(1 .. Nat2),
                                    Skip_Backslashed => Bool1) );

          when 16 =>
            Basic_Proc.Put_Line_Output ("16 Normalize");
            declare
              -- Copy Str in a non normalized string
              Lstr : constant String (2 .. Str_Len+1) := Str(1 .. Str_Len);
            begin
              Basic_Proc.Put_Line_Output ("Normalized string: "
                & Str_Util.Normalize (Lstr));
            end;

          when 17 =>
            Basic_Proc.Put_Line_Output ("17 Regex locate");
            Basic_Proc.Put_Output ("Criteria (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Output ("From_Index (Nat)? "); Nat_Get (Nat2, True);
            Basic_Proc.Put_Output ("To_Index (Nat)? "); Nat_Get (Nat3, True);
            Basic_Proc.Put_Output ("Forward (YN)? "); Bool_Get(Bool1);
            Basic_Proc.Put_Output ("Occurence (Pos)? "); Nat_Get (Pos1, False);
            Search_Result := Str_Util.Regex.Locate (Str(1 .. Str_Len),
                    Criteria => Str1(1 .. Nat1),
                    From_Index => Nat2,
                    To_Index => Nat3,
                    Forward => Bool1,
                    Occurence => Pos1);
            Basic_Proc.Put_Line_Output ("Match at: " & Search_Result.First_Offset'Img
                          & " -" & Search_Result.Last_Offset_Start'Img
                          & " /" & Search_Result.Last_Offset_Stop'Img);

          when 18 =>
            Basic_Proc.Put_Line_Output ("18 Regex replace");
            Basic_Proc.Put_Output ("Criteria (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Output ("By (Str)? "); Basic_Proc.Get_Line (Str2, Nat2);
            Basic_Proc.Put_Output ("From_Index (Nat)? "); Nat_Get (Nat3, True);
            Basic_Proc.Put_Output ("To_Index (Nat)? "); Nat_Get (Nat4, True);
            Basic_Proc.Put_Output ("Nb_Cycles (Nat)? "); Nat_Get (Nat5, True);
            Basic_Proc.Put_Line_Output ("Replaced string: "
              & Str_Util.Regex.Substit (Str(1 .. Str_Len),
                    Criteria => Str1(1 .. Nat1),
                    By => Str2(1 .. Nat2),
                    From_Index => Nat3,
                    To_Index => Nat4,
                    Nb_Cycles => Nat5));

          when 19 =>
            Basic_Proc.Put_Line_Output ("19 Split");
            Basic_Proc.Put_Output ("Separator (Char)? "); Basic_Proc.Get(Char1); Basic_Proc.Skip_Line;
            declare
              Lstr : constant Many_Strings.Many_String
                   := Str_Util.Split (Str(1 .. Str_Len), Char1);
            begin
              Basic_Proc.Put_Line_Output ("Split into: ");
              for I in 1 .. Many_Strings.Nb (Lstr) loop
                Basic_Proc.Put_Line_Output (">" & Many_Strings.Nth (Lstr, I) & "<");
              end loop;
            end;
          when 20 =>
            Basic_Proc.Put_Line_Output ("20 Regex split");
            Basic_Proc.Put_Output ("Criteria (String)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Output ("Max_Slices (Pos)? "); Nat_Get (Pos1, False);
            declare
              Lstr : constant As.U.Utils.Asu_Array
                   := Str_Util.Regex.Split (Str(1 .. Str_Len),
                                              Str1(1 .. Nat1), Pos1);
            begin
              Basic_Proc.Put_Line_Output ("Split into: ");
              for I in Lstr'Range loop
                Basic_Proc.Put_Line_Output (">" & Lstr(I).Image & "<");
              end loop;
            end;

          when 21 =>
            Basic_Proc.Put_Line_Output ("21 Center");
            Basic_Proc.Put_Output ("Len (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Output ("Gap (Char)? "); Basic_Proc.Get(Char1); Basic_Proc.Skip_Line;
            Basic_Proc.Put_Line_Output (
                "Center: |"
              & Str_Util.Center(Str(1 .. Str_Len), Len => Pos1, Gap => Char1)
              & "|" );

          when 22 =>
            Basic_Proc.Put_Line_Output ("22 Regex split on sep");
            Basic_Proc.Put_Output ("Separator (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            declare
              Slices : constant As.U.Utils.Asu_Array
                     := Str_Util.Regex.Split_Sep (Str(1 .. Str_Len),
                                                    Str1(1 .. Nat1));
            begin
              for I in 1 .. Slices'Length loop
                Basic_Proc.Put_Line_Output (">" & Slices(I).Image & "<");
              end loop;
            end;

          when 23 =>
            Basic_Proc.Put_Line_Output ("23 Overwrite");
            Basic_Proc.Put_Output ("Position (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Output ("New_Str (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Line_Output (
                "Overwritten: "
              & Str_Util.Overwrite (Str(1 .. Str_Len),
                                    Position => Pos1,
                                    New_Str => Str1(1 .. Nat1)));

          when 24 =>
            Basic_Proc.Put_Line_Output ("24 Replace");
            Basic_Proc.Put_Output ("Low (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Output ("High (Nat)? "); Nat_Get (Nat2, True);
            Basic_Proc.Put_Output ("By (Str)? "); Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Line_Output (
                "Replaced: "
              & Str_Util.Replace (Str(1 .. Str_Len),
                                  Low => Pos1,
                                  High => Nat2,
                                  By => Str1(1 .. Nat1)));

          when 25 =>
            Basic_Proc.Put_Line_Output ("25 Insert");
            Basic_Proc.Put_Output ("Before (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Output ("New_Str (Str)? ");
            Basic_Proc.Get_Line (Str1, Nat1);
            Basic_Proc.Put_Line_Output (
                "Inserted: "
              & Str_Util.Insert (Str(1 .. Str_Len),
                                 Before => Pos1,
                                 New_Str => Str1(1 .. Nat1)));

          when 26 =>
            Basic_Proc.Put_Line_Output ("26 Delete");
            Basic_Proc.Put_Output ("From (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Output ("Through (Nat)? "); Nat_Get (Nat1, True);
            Basic_Proc.Put_Line_Output (
                "Deleted: "
              & Str_Util.Delete (Str(1 .. Str_Len),
                                 From => Pos1,
                                 Through => Nat1));

          when 27 =>
            Basic_Proc.Put_Line_Output ("26 Delete_Nb");
            Basic_Proc.Put_Output ("From (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Output ("Number (Nat)? "); Nat_Get (Nat1, True);
            Basic_Proc.Put_Line_Output (
                "Deleted: "
              & Str_Util.Delete_Nb (Str(1 .. Str_Len),
                                    From => Pos1,
                                    Number => Nat1));

          when 28 =>
            Basic_Proc.Put_Line_Output ("27 Check char is backslashed");
            Basic_Proc.Put_Output ("Index (Pos)? "); Nat_Get (Pos1, False);
            Basic_Proc.Put_Line_Output (
                "Is backslashed: "
              & Mixed_Str(Boolean'Image (Str_Util.Is_Backslashed (
                   Str(1 .. Str_Len),
                   Index => Pos1))));

          when others => null;

        end case;
      exception
        when Error:Constraint_Error
             | Str_Util.Inv_Delimiter
             | Str_Util.Delimiter_Mismatch
             | Str_Util.Regex.Invalid_Regular_Expression
             | Str_Util.Regex.Invalid_Index =>
          Basic_Proc.Put_Line_Output ("Raised " & Ada.Exceptions.Exception_Name(Error)
                                    & "!");

      end;

    end loop;

    Basic_Proc.New_Line_Output;
  end loop;

end T_String;

