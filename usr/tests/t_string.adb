with Ada.Characters.Latin_1;
with My_Io, String_Mng, Upper_Str, Lower_Str, Mixed_Str, Upper_Char;
procedure T_String is

  Action : Natural;
  Str : String(1 .. 500);
  Str_Len : Natural;

  Pos1 : Positive;
  Nat1 : Natural;
  Char1 : Character;
  Bool1 : Boolean;
  Bool2 : Boolean;
  Bool3 : Boolean;
  Str1 : String(1 .. 500);

  procedure Int_Get (V : out Natural; Allow_Zero : in Boolean) is
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
  end Int_Get;

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
      My_Io.Put_Line ("0 Exit");
      My_Io.Put_Line ("1 Case conversion");
      My_Io.Put_Line ("2 Parse spaces");
      My_Io.Put_Line ("3 Procuste");
      My_Io.Put_Line ("4 Locate (fragment)");
      My_Io.Put_Line ("5 Remove (substring)");
      My_Io.Put_Line ("6 (Extract) Slice");
      My_Io.Put_Line ("7 Cut (head or tail)");
      My_Io.Put_Line ("8 Extract (head or tail)");
      
      loop 
        My_Io.Put ("Choice (0 .. 8) ? "); Int_Get (Action, True);
        exit when Action <= 8;
      end loop;
      My_Io.New_Line;

      begin
        case Action is
          when 0 =>
            exit;
          when 1 =>
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

          when 2 =>
            My_Io.Put_Line ("Parse spaces");
            My_Io.Put ("From_Head (YN)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Spaces parsed at: " & 
             Integer'Image (String_Mng.Parse_Spaces (
               Str(1 .. Str_Len), From_Head => Bool1)) );

          when 3 =>
            My_Io.Put_Line ("Procuste");
            My_Io.Put ("Len (Pos)? "); Int_Get(Pos1, False);
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

          when 4 =>
            My_Io.Put_Line ("Locate");
            My_Io.Put ("Fragment (String)? ");
            My_Io.Get_Line (Str1, Nat1);
            My_Io.Put ("Occurence (Pos)? "); Int_Get(Pos1, False);
            My_Io.Put_Line ("Occurence of fragment located at: " & 
             Integer'Image (String_Mng.Locate (
               Str(1 .. Str_Len),
               Fragment => Str1(1 .. Nat1),
               Occurence => Pos1)) );

          when 5 =>
            My_Io.Put_Line ("Remove (substring)");
            My_Io.Put ("At_Index (Pos)? "); Int_Get(Pos1, False);
            My_Io.Put ("Nb_Char (Nat)? "); Int_Get(Nat1, False);
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

          when 6 =>
            My_Io.Put_Line ("(Extract) Slice");
            My_Io.Put ("At_Index (Pos)? "); Int_Get(Pos1, False);
            My_Io.Put ("Nb_Char (Nat)? "); Int_Get(Nat1, False);
            My_Io.Put ("To_Right (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Extracted slice: |"
              & String_Mng.Slice (Str(1 .. Str_Len),
                 At_Index => Pos1,
                 Nb_Char => Nat1,
                 To_Right => Bool1) & "|" );

          when 7 =>
            My_Io.Put_Line ("Cut (head or tail)");
            My_Io.Put ("Nb_Char (Nat)? "); Int_Get(Nat1, False);
            My_Io.Put ("Head (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Cut string: |"
              & String_Mng.Cut (Str(1 .. Str_Len),
                 Nb_Char => Nat1,
                 Head => Bool1) & "|" );

          when 8 =>
            My_Io.Put_Line ("Extract (head or tail)");
            My_Io.Put ("Nb_Char (Nat)? "); Int_Get(Nat1, False);
            My_Io.Put ("Head (Bool)? "); Bool_Get(Bool1);
            My_Io.Put_Line ("Extracted: |"
              & String_Mng.Extract (Str(1 .. Str_Len),
                 Nb_Char => Nat1,
                 Head => Bool1) & "|" );

          when others => null;
        end case;
      exception
        when Constraint_Error =>
          My_Io.Put_Line ("Raised Constraint_Error!");
      end;

    end loop;

    My_Io.New_Line;
  end loop;

end T_String;

