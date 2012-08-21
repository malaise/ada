with Ada.Strings.Wide_Unbounded;
with As.U, Environ, String_Mng, Lower_Str, Unbounded_Arrays;
package body Language is

  -- When ENV, UTF_8 is set if a Getenv on "LANG" gives a value
  --  containing "UTF-8". This is the default behaviour.
  -- type Language_List is (Lang_C, Lang_Utf_8, Get_Env);

  -- Language management
  Lang_Set : Boolean := False;
  Lang : Language_Set_List;

  -- Gess Lang from Environ
  procedure Getenv_Lang is
    Lang_Str : constant String := Lower_Str (Environ.Getenv ("LANG"));
  begin
    if String_Mng.Locate (Lang_Str, "utf8") /= 0 then
      Lang := Lang_Utf_8;
    elsif String_Mng.Locate (Lang_Str, "utf-8") /= 0 then
      Lang := Lang_Utf_8;
    else
      Lang := Lang_C;
    end if;
    Lang_Set := True;
  exception
    when others =>
      Lang := Lang_C;
      Lang_Set := True;
  end Getenv_Lang;

  procedure Set_Language (Language : in Language_List) is
  begin
    -- Reject changing Lang if already set or got
    if Lang_Set then
      raise Language_Already_Set;
    else
      Force_Language (Language);
    end if;
  end Set_Language;

  procedure Force_Language (Language : in Language_List) is
  begin
    if Language = Get_Env then
      Getenv_Lang;
    else
      Lang := Language;
      Lang_Set := True;
    end if;
  end Force_Language;

  function Get_Language return Language_Set_List is
  begin
    if not Lang_Set then
      Getenv_Lang;
    end if;
    return Lang;
  end Get_Language;

  -- Utf_8.Nb_Char or 1
  function Nb_Chars (Char : Character) return Positive is
  begin
    if Get_Language /= Lang_Utf_8 then
      return 1;
    else
      return Utf_8.Nb_Chars (Char);
    end if;
  end Nb_Chars;

  -- Raw translation from Wide_Character to and from Character
  function Is_Char (W : Wide_Character) return Boolean is
  begin
    return W <= Wide_Last_Char;
  end Is_Char;
  function Is_Char (U : Unicode_Number) return Boolean is
  begin
    return U <= Unicode_Last_Char;
  end Is_Char;
  function Is_Wide (U : Unicode_Number) return Boolean is
  begin
    return U <= Unicode_Last_Wide;
  end Is_Wide;

  function Char_To_Wide (C : Character) return Wide_Character is
  begin
    return Wide_Character'Val (Character'Pos (C));
  end Char_To_Wide;
  function Char_To_Unicode (C : Character) return Unicode_Number is
  begin
    return Character'Pos (C);
  end;
  function Wide_To_Unicode (W : Wide_Character) return Unicode_Number is
  begin
    return Wide_Character'Pos (W);
  end Wide_To_Unicode;

  function Wide_To_Char (W : Wide_Character) return Character is
  begin
    if W <= Wide_Last_Char then
      return Character'Val (Wide_Character'Pos (W));
    else
      return Default_Char;
    end if;
  end Wide_To_Char;
  function Unicode_To_Char (U : Unicode_Number) return Character is
  begin
    if U <= Unicode_Last_Char then
      return Character'Val (U);
    else
      return Default_Char;
    end if;
  end Unicode_To_Char;
  function Unicode_To_Wide (U : Unicode_Number) return Wide_Character is
  begin
    if U <= Unicode_Last_Wide then
      return Wide_Character'Val (U);
    else
      return Char_To_Wide (Default_Char);
    end if;
  end Unicode_To_Wide;

  function Copy (W : Wide_String) return String is
    S : String (W'Range);
  begin
    for I in W'Range loop
      S(I) := Wide_To_Char (W(I));
    end loop;
    return S;
  end Copy;
  function Copy (S : String) return Wide_String is
    W : Wide_String (S'Range);
  begin
    for I in S'Range loop
      W(I) := Char_To_Wide (S(I));
    end loop;
    return W;
  end Copy;
  function Copy (U : Unicode_Sequence) return String is
    S : String (U'Range);
  begin
    for I in U'Range loop
      S(I) := Unicode_To_Char (U(I));
    end loop;
    return S;
  end Copy;
  function Copy (S : String) return Unicode_Sequence is
    U : Unicode_Sequence (S'Range);
  begin
    for I in S'Range loop
      U(I) := Char_To_Unicode (S(I));
    end loop;
    return U;
  end Copy;
  function Copy (U : Unicode_Sequence) return Wide_String is
    W : Wide_String (U'Range);
  begin
    for I in U'Range loop
      W(I) := Unicode_To_Wide (U(I));
    end loop;
    return W;
  end Copy;
  function Copy (W : Wide_String) return Unicode_Sequence  is
    U : Unicode_Sequence (W'Range);
  begin
    for I in W'Range loop
      U(I) := Wide_To_Unicode (W(I));
    end loop;
    return U;
  end Copy;

  -- Convertion to and from wide string
  -- May raise Utf_8 exceptions
  function Wide_To_String (Str : Wide_String) return String is
    S : As.U.Asu_Us;
    W : Wide_Character;
  begin
    if Get_Language /= Lang_Utf_8 then
      for I in Str'Range loop
        S.Append (Wide_To_Char (Str(I)));
      end loop;
    else
      for I in Str'Range loop
        W := Str(I);
        if W <= Wide_Last_Char then
          -- Optim
          S.Append (Wide_To_Char (Str(I)));
        else
          S.Append (Utf_8.Encode (Str(I)));
        end if;
      end loop;
    end if;
    return S.Image;
  end Wide_To_String;

  function "&" (Left : String; Right : Wide_String) return String is
  begin
    return Left & Wide_To_String (Right);
  end "&";
  function "&" (Left : Wide_String; Right : String) return String is
  begin
    return Wide_To_String (Left) & Right;
  end "&";

  function String_To_Wide (Str : String) return Wide_String is
    Ws : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
    Wc : Wide_Character;
    Index : Natural;
    Nb : Utf_8.Len_Range;
  begin
    if Get_Language /= Lang_Utf_8 then
      for I in Str'Range loop
        Ada.Strings.Wide_Unbounded.Append (Ws, Char_To_Wide(Str(I)));
      end loop;
    else
      -- Encode Utf_8 sequences
      Index := Str'First;
      loop
        exit when Index > Str'Last;
        -- Encode the Nb_Chars of this sequence
        begin
          Nb := Utf_8.Nb_Chars (Str(Index));
          if Nb = 1 then
            -- Optim
            Wc := Char_To_Wide (Str(Index));
          elsif Index + Nb - 1 > Str'Last then
            -- Incomplete end of sequence
            raise Invalid_Utf_8_Sequence;
          else
            Wc := Utf_8.Decode (Str(Index .. Index + Nb - 1));
          end if;
        exception
          when Utf_8.Invalid_Sequence =>
            raise Invalid_Utf_8_Sequence;
        end;
        Ada.Strings.Wide_Unbounded.Append (Ws, Wc);
        Index := Index + Nb;
      end loop;
    end if;
    return Ada.Strings.Wide_Unbounded.To_Wide_String (Ws);
  end String_To_Wide;

  function "&" (Left : String; Right : Wide_String) return Wide_String is
  begin
    return String_To_Wide (Left) & Right;
  end "&";
  function "&" (Left : Wide_String; Right : String) return Wide_String is
  begin
    return Left & String_To_Wide (Right);
  end "&";
  function "=" (Left : String; Right : Wide_String) return Boolean is
  begin
    return String_To_Wide (Left) = Right;
  end "=";
  function "=" (Left : Wide_String; Right : String) return Boolean is
  begin
    return Left = String_To_Wide (Right);
  end "=";

  -- Conversion to and from unicode sequence according to language
  function Unicode_To_String (Str : Unicode_Sequence) return String is
    S : As.U.Asu_Us;
    U : Unicode_Number;
  begin
    if Get_Language /= Lang_Utf_8 then
      for I in Str'Range loop
        S.Append (Unicode_To_Char (Str(I)));
      end loop;
    else
      for I in Str'Range loop
        U := Str(I);
        if U <= Unicode_Last_Char then
          -- Optim
          S.Append (Unicode_To_Char (Str(I)));
        else
          S.Append (Utf_8.Encode (Str(I)));
        end if;
      end loop;
    end if;
    return S.Image;
  end Unicode_To_String;

  function "&" (Left : String; Right : Unicode_Sequence) return String is
  begin
    return Left & Unicode_To_String (Right);
  end "&";
  function "&" (Left : Unicode_Sequence; Right : String) return String is
  begin
    return Unicode_To_String (Left) & Right;
  end "&";

  package Unbounded_Unicode is new Unbounded_Arrays (Unicode_Number,
                                                     Unicode_Sequence);
  function String_To_Unicode (Str : String) return Unicode_Sequence is
    Us : Unbounded_Unicode.Unbounded_Array;
    U : Unicode_Number;
    Index : Natural;
    Nb : Utf_8.Len_Range;
  begin
    if Get_Language /= Lang_Utf_8 then
      for I in Str'Range loop
        Unbounded_Unicode.Append (Us, Char_To_Unicode (Str(I)));
      end loop;
    else
      -- Encode Utf_8 sequences
      Index := Str'First;
      loop
        exit when Index > Str'Last;
        -- Encode the Nb_Chars of this sequence
        begin
          Nb := Utf_8.Nb_Chars (Str(Index));
          if Nb = 1 then
            -- Optim
            U := Char_To_Unicode (Str(Index));
          else
            U := Utf_8.Decode (Str(Index .. Index + Nb - 1));
          end if;
        exception
          when Utf_8.Invalid_Sequence =>
            U := Char_To_Unicode (Default_Char);
        end;
        Unbounded_Unicode.Append (Us, U);
        Index := Index + Nb;
      end loop;
    end if;
    return Unbounded_Unicode.To_Array (Us);
  end String_To_Unicode;

  function "&" (Left : String; Right : Unicode_Sequence)
               return Unicode_Sequence is
    use type Unicode.Unicode_Sequence;
  begin
    return String_To_Unicode (Left) & Right;
  end "&";
  function "&" (Left : Unicode_Sequence; Right : String)
               return Unicode_Sequence is
    use type Unicode.Unicode_Sequence;
  begin
    return Left & String_To_Unicode (Right);
  end "&";
  function "=" (Left : String; Right : Unicode_Sequence) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return String_To_Unicode (Left) = Right;
  end "=";
  function "=" (Left : Unicode_Sequence; Right : String) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Left = String_To_Unicode (Right);
  end "=";

  -- Compute the number of slots to put Str
  function Put_Length (Str : String) return Natural is
    Len : Natural;
    Index : Natural;
  begin
    -- Count effective put len
    Index := Str'First;
    Len := 0;
    loop
      exit when Index > Str'Last;
      Len := Len + 1;
      -- Skip the Nb_Chars of this sequence
      Index := Index + Nb_Chars (Str(Index));
    end loop;
    return Len;
  end Put_Length;

  -- Compute the last index of Str to put a number of slots max
  function Last_Index_For (Str : String; Put_Pos : Natural) return Natural is
    Index : Natural;
  begin
    if Str'Length = 0 or else Put_Pos = 0 then
      return 0;
    end if;
    -- Move forward by steps of Nb_Chars
    Index := Str'First;
    for I in 1 .. Put_Pos loop
      Index := Index + (Nb_Chars (Str(Index)) - 1);
      if Index = Str'Last then
        -- End of string reached before Put_Pos => Str'last
        return Str'Last;
      elsif Index > Str'Last then
        -- Index + Nb_Chars is above Last + 1 => String is invalid
        raise Invalid_Utf_8_Sequence;
      end if;
      -- Cannot overflow because < Str'Last
      Index := Index + 1;
    end loop;
    return Index - 1;
  end Last_Index_For;

  -- Adjust String so that it contains only Len valid characters
  --  (result is Len or shorter than Len)
  function Adjust (Str : String; Len : Natural) return String is
    Indexes : constant Index_Array := All_Indexes_Of (Str);
    Last : Natural;
  begin
    for I in reverse Indexes'Range loop
      if Indexes(I) <= Len then
        -- Compute Last Str index for Indexes(I)
        Last := Indexes(I);
        Last := Last + Nb_Chars (Str(Last));
        if Last - Str'First + 1 <= Len then
          -- Last leads to a string shorter or equal to Len
          return Str (Str'First .. Last);
        end if;
      end if;
    end loop;
    return "";
  end Adjust;

  -- Compute all the indexes of Str corresponding to successive slots
  function All_Indexes_Of (Str : String) return Index_Array is
    Indexes : Index_Array (1 .. Str'Length);
    Index : Natural;
    Len : Natural;
  begin
    -- Store effective put indexes
    Index := Str'First;
    Len := 0;
    for I in Indexes'Range loop
      if Index > Str'Last then
        -- End of string reached? Len is OK then
        return Indexes (1 .. Len);
      end if;
      Indexes(I) := Index;
      Len := I;
      Index := Index + Nb_Chars (Str(Index));
    end loop;
    -- End of Indexes reached
    return Indexes;
  end All_Indexes_Of;

  -- Return slice of Str between 2 slots included
  function Slice (Str : String;
                  First_Pos : Positive;
                  Last_Pos  : Natural) return String is
    Indexes : constant Index_Array := All_Indexes_Of (Str);
    Last_Nbre_Char : Positive;
    Last_Char_Index : Positive;
  begin
    if Last_Pos < First_Pos then
      return "";
    end if;
    Last_Nbre_Char := Nb_Chars (Str(Indexes(Last_Pos)));
    Last_Char_Index := Indexes(Last_Pos) + Last_Nbre_Char - 1;
    return Str (Indexes(First_Pos) .. Last_Char_Index);
  end Slice;

end Language;

