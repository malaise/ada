with Ada.Characters.Latin_1, Ada.Exceptions;
with My_Io, String_Mng, Upper_Str, Lower_Str, Mixed_Str, Upper_Char, Sys_Calls;
procedure T_String is

  Action : Natural;
  Str : String(1 .. 500);
  Str_Len : Natural;

  Pos1 : Positive;
  Pos2 : Positive;
  Pos3 : Positive;
  Nat1 : Natural;
  Nat2 : Natural;
  Char1 : Character;
  Bool1 : Boolean;
  Bool2 : Boolean;
  Bool3 : Boolean;
  Str1 : String(1 .. 500);
  Str2 : String(1 .. 500);

  procedure Nat_Get (V : out Natural; Allow_Zero : in Boolean) is
    Str : String (1 .. 80);
    Len : Integer;
  begin
    loop
      My_Io.Get_Line (Str, Len);
      begin
        V := Natural'Value(Str(1 .. Len));
        if V = 0 and then not Allow_Zero then
          raise Constraint_Error;
        end if;
        return;
      exception
        when others => null;
      end;
      if Allow_Zero then
        My_Io.Put ("Enter a Natural ? ");
      else
        My_Io.Put ("Enter a Positive ? ");
      end if;
    end loop;
  end Nat_Get;

  procedure Bool_Get (V : out Boolean) is
    Str : String (1 .. 80);
    Len : Integer;
  begin
    loop
      My_Io.Get_Line (Str, Len);
      Str(1 .. Len) := Mixed_Str (Str(1 .. Len));
      if Str(1 .. Len) = "True" or else Str(1 .. Len) = "T"
      or else Str(1 .. Len) = "Yes" or else Str(1 .. Len) = "Y" then
        V := True;
        return;
      elsif Str(1 .. Len) = "False" or else Str(1 .. Len) = "F"
      or else Str(1 .. Len) = "No" or else Str(1 .. Len) = "N" then
        V := False;
        return;
      end if;
      My_Io.Put ("Yes or No ? ");
    end loop;
  end Bool_Get;

begin

  loop
    My_Io.Put ("Str (String)? "); My_Io.Get_Line (Str, Str_Len);

    loop
      My_Io.New_Line;
      My_Io.Put_Line ("String: |" & Str(1 .. Str_Len) & "|   len: "
                    & Integer'Image(Str_Len));

      My_Io.Put_Line ("Main menu");
      My_Io.Put_Line (" 0 Exit");
      My_Io.Put_Line (" 1 Case conversion");
      My_Io.Put_Line (" 2 Parse spaces");
      My_Io.Put_Line (" 3 Procuste");
      My_Io.Put_Line (" 4 Locate (fragment)");
      My_Io.Put_Line (" 5 Remove (substring)");
      My_Io.Put_Line (" 6 (Extract) Slice");
      My_Io.Put_Line (" 7 Cut (head or tail)");
      My_Io.Put_Line (" 8 Extract (head or tail)");
      My_Io.Put_Line (" 9 Swap");
      My_Io.Put_Line ("10 Unique (from head or tail)");
      My_Io.Put_Line ("11 Variable substitution");
      My_Io.Put_Line ("12 Escape location");
      My_Io.Put_Line ("13 Tuncation ot best length");
      My_Io.Put_Line ("14 Copy");
      My_Io.Put_Line ("15 Replace");

      My_Io.Put ("Choice (0 .. 15) ? "); Nat_Get (Action, True);
      My_Io.New_Line;

      begin
        case Action is
          when  0 =>
            exit;
          when  1 =>
            My_Io.Put ("Case conversion (ULM) ? "); My_Io.Get(Char1);
            My_Io.Skip_Line;
            Char1 := Upper_Char (Char1);
            if Char1 = 'U' then
              My_Io.Put_Line (Upper_Str (Str(1 .. Str_Len)));
            elsif Char1 = 'L' then
              My_Io.Put_Line (Lower_Str (Str(1 .. Str_Len)));
            elsif Char1 = 'M' then
              My_Io.Put_Line (Mixed_Str (Str(1 .. Str_Len)));
            else
              My_Io.Put_Line ("Discarded.");
            end if;

          when  2 =>
            My_Io.Put_Line ("Parse spaces");
            My_Io.Put ("From_Head (YN)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Spaces parsed at: " &
             Integer'Image (String_Mng.Parse_Spaces (
               Str(1 .. Str_Len), From_Head => Bool1)) );

          when  3 =>
            My_Io.Put_Line ("Procuste");
            My_Io.Put ("Len (Pos)? "); Nat_Get(Pos1, False);
            My_Io.Put ("Align_Left (YN)? "); Bool_Get(Bool1);
            My_Io.Put ("Gap (Char)? "); My_Io.Get(Char1); My_Io.Skip_Line;
            My_Io.Put ("Trunc_Head (YN)? "); Bool_Get(Bool2);
            My_Io.Put ("Show_Trunc (YN)? "); Bool_Get(Bool3);

            My_Io.Put_Line (
                "Procuste: |"
              & String_Mng.Procuste(Str(1 .. Str_Len),
                       Len => Pos1,
                       Align_Left => Bool1,
                       Gap => Char1,
                       Trunc_Head => Bool2,
                       Show_Trunc => Bool3)
              & "|" );

          when  4 =>
            My_Io.Put_Line ("Locate");
            My_Io.Put ("From_Index (Pos)? "); Nat_Get(Pos1, False);
            My_Io.Put ("Fragment (Str)? "); My_Io.Get_Line (Str1, Nat1);
            My_Io.Put ("Occurence (Pos)? "); Nat_Get(Pos2, False);
            My_Io.Put_Line ("Occurence of fragment located at: " &
             Integer'Image (String_Mng.Locate (
               Str(1 .. Str_Len),
               From_Index => Pos1,
               Fragment => Str1(1 .. Nat1),
               Occurence => Pos2)) );

          when  5 =>
            My_Io.Put_Line ("Remove (substring)");
            My_Io.Put ("At_Index (Pos)? "); Nat_Get(Pos1, False);
            My_Io.Put ("Nb_Char (Nat)? "); Nat_Get(Nat1, False);
            My_Io.Put ("Shift_Left (Bool)? "); Bool_Get(Bool1);
            My_Io.Put ("Gap (Char, n for none)? ");
                       My_Io.Get(Char1); My_Io.Skip_Line;
            if Char1 = 'n' then Char1 := Ada.Characters.Latin_1.Nul; end if;
            My_Io.Put_Line ("Remaining string: |"
              & String_Mng.Remove (Str(1 .. Str_Len),
                 At_Index => Pos1,
                 Nb_Char => Nat1,
                 Shift_Left => Bool1,
                 Gap => Char1) & "|" );

          when  6 =>
            My_Io.Put_Line ("(Extract) Slice");
            My_Io.Put ("At_Index (Pos)? "); Nat_Get(Pos1, False);
            My_Io.Put ("Nb_Char (Nat)? "); Nat_Get(Nat1, False);
            My_Io.Put ("To_Right (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Extracted slice: |"
              & String_Mng.Slice (Str(1 .. Str_Len),
                 At_Index => Pos1,
                 Nb_Char => Nat1,
                 To_Right => Bool1) & "|" );

          when  7 =>
            My_Io.Put_Line ("Cut (head or tail)");
            My_Io.Put ("Nb_Char (Nat)? "); Nat_Get(Nat1, False);
            My_Io.Put ("Head (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Cut string: |"
              & String_Mng.Cut (Str(1 .. Str_Len),
                 Nb_Char => Nat1,
                 Head => Bool1) & "|" );

          when  8 =>
            My_Io.Put_Line ("Extract (head or tail)");
            My_Io.Put ("Nb_Char (Nat)? "); Nat_Get(Nat1, False);
            My_Io.Put ("Head (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Extracted: |"
              & String_Mng.Extract (Str(1 .. Str_Len),
                 Nb_Char => Nat1,
                 Head => Bool1) & "|" );

          when  9 =>
            My_Io.Put_Line ("Swap");
            My_Io.Put_Line ("Swapped: |"
              & String_Mng.Swap (Str(1 .. Str_Len)) & "|" );

          when 10 =>
            My_Io.Put_Line ("Unique (from head or tail)");
            My_Io.Put ("From head (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Uniqued: |"
              & String_Mng.Unique (Str(1 .. Str_Len),
                                   From_Head => Bool1) & "|" );
          when 11 =>
            My_Io.Put_Line ("Env variable substitution");
            My_Io.Put ("Start delimiter (Str)? "); My_Io.Get_Line (Str1, Nat1);
            My_Io.Put ("Stop delimiter (Str)? ");  My_Io.Get_Line (Str2, Nat2);
            My_Io.Put_Line ("Substitued: |"
              & String_Mng.Eval_Variables (
                        Str(1 .. Str_Len),
                        Start_Delimiter => Str1(1 .. Nat1),
                        Stop_Delimiter => Str2(1 .. Nat2),
                        Resolv => Sys_Calls.Getenv'Access)
              & "|" );
          when 12 =>
            My_Io.Put_Line ("Escape sequence location");
            My_Io.Put ("From index (Nat)? "); Nat_Get(Nat1, False);
            My_Io.Put ("Escaped (Esc char first) (Str)? ");
                       My_Io.Get_Line (Str2, Nat2);
            My_Io.Put_Line ("Located at: "
              & Natural'Image (String_Mng.Locate_Escape (
                        Str(1 .. Str_Len),
                        From_Index => Nat1,
                        Escape => Str2(1 .. Nat2))));
          when 13 =>
            My_Io.Put_Line ("13 Tuncation ot best length");
            My_Io.Put ("Length (Pos)? "); Nat_Get(Pos1, False);
            My_Io.Put ("Mini (Pos)? ");   Nat_Get(Pos2, False);
            My_Io.Put ("Maxi (Pos)? ");   Nat_Get(Pos3, False);
            My_Io.Put_Line ("Truncated at: "
              & Natural'Image (String_Mng.Truncate (
                        Str(1 .. Str_Len),
                        Length => Pos1,
                        Mini => Pos2,
                        Maxi => Pos3,
                        Separating => String_Mng.Is_Separator'Access)));
          when 14 =>
            My_Io.Put_Line ("14 Copy");
            My_Io.Put ("Val (Str)? "); My_Io.Get_Line (Str1, Nat1);
            declare
              Lstr : String (1 .. Str_Len) := Str (1 .. Str_Len);
            begin
              String_Mng.Copy (Str1(1 .. Nat1), Lstr);
              My_Io.Put_Line ("Copy result: " & Lstr);
            end;
          when 15 =>
            My_Io.Put_Line ("15 Replace");
            My_Io.Put ("What (Str)? "); My_Io.Get_Line (Str1, Nat1);
            My_Io.Put ("By (Str)? "); My_Io.Get_Line (Str2, Nat2);
            My_Io.Put_Line ("Replaced string: "
              & String_Mng.Replace (Str1(1 .. Nat1),
                                    Str2(1 .. Nat2),
                                    Str(1 .. Str_Len)));
          when others => null;
        end case;
      exception
        when Error:Constraint_Error
             | String_Mng.Inv_Delimiter
             | String_Mng.Delimiter_Mismatch
             | Sys_Calls.Env_Not_Set =>
          My_Io.Put_Line ("Raised " & Ada.Exceptions.Exception_Name(Error)
                                    & "!");

      end;

    end loop;

    My_Io.New_Line;
  end loop;

end T_String;

