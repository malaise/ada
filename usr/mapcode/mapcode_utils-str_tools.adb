-- Various utilities on strings
package body Mapcode_Utils.Str_Tools is

  -- Locate the Nth occurence of a fragment within a string,
  --  between a given index (first/last if 0) and the end/beginning of the
  --  string, searching forward or backward
  -- Return the index in Within of the char matching the start of Fragment
  -- Return 0 if Index not in Within, if Within or Fragment is empty,
  --  or if not found
-- Locate Nth occurence of a fragment within a string,
  --  between a given index (first/last if 0) and the end/beginning of string,
  --  searching forward or backward
  -- Returns index in Within of char matching start of Fragment
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within     : String;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1)
           return Natural is
    Index : Natural;
    Found_Occurence : Natural := 0;
  begin
    -- Fix Index
    Index := (if From_Index = 0 then
               (if Forward then Within'First else Within'Last)
              else From_Index);

    -- Handle limit or incorrect values
    if Within'Length = 0
    or else Fragment'Length = 0
    or else Index not in Within'First .. Within'Last then
      return 0;
    end if;
    if Forward then
      for I in Index .. Within'Last - Fragment'Length + 1 loop
        if Within(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    else
      for I in reverse Within'First .. Index - Fragment'Length + 1 loop
        if Within(I .. I + Fragment'Length - 1) = Fragment then
          Found_Occurence := Found_Occurence + 1;
          if Found_Occurence = Occurence then
            return I;
          end if;
        end if;
      end loop;
    end if;
    return 0;
  exception
    when Constraint_Error =>
      return 0;
  end Locate;

  -- Convert a character into upper char
  function Upper_Char (Char : Character) return Character is
    Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
  begin
    return (if Char not in 'a' .. 'z' then Char
            else Character'Val (Character'Pos(Char) + Offset));
  end Upper_Char;

  -- Convert a character into lower char
  function Lower_Char (Char : Character) return Character is
    Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
  begin
    return (if Char not in 'A' .. 'Z' then Char
            else Character'Val (Character'Pos(Char) - Offset));
  end Lower_Char;

  -- Convert the characters of Str into upper char
  function Upper_Str (Str : String) return String is
    Result : String := Str;
  begin

    for C of Result loop
      C := Upper_Char (C);
    end loop;
    return Result;
  end Upper_Str;

  -- Convert the characters of Str:
  -- Any letter that follows a letter is lower char
  -- Any other  letter (including the first letter) is UPPER char
  function Mixed_Str (Str : String) return String is
    Result : String := Str;
    Prev_Separator : Boolean := True;
  begin
    for C of Result loop
      if Prev_Separator and then C in 'a' .. 'z' then
        C := Upper_Char (C);
      elsif not Prev_Separator and then C in 'A' .. 'Z' then
        C := Lower_Char (C);
      end if;
      Prev_Separator := C not in 'a' .. 'z'
               and then C not in 'A' .. 'Z';
    end loop;
    return Result;
  end Mixed_Str;

end Mapcode_Utils.Str_Tools;

