with Trace.Loggers, As.U, Aski, Trilean, Gets, Mixed_Str, Regular_Expressions;
package body Scanner is

  -- Convert a string into a sequence of Anys, according to a given format
  -- An array and a sequence of Anys
  -- type Any_Array is array (Positive range <>) of Any_Def.Any;

  -- Logger
  Logger : Trace.Loggers.Logger;
  -- Init the trace logger if necessary
  procedure Init_Logger is
  begin
    if not Logger.Is_Init then
      Logger.Init ("Scanner");
    end if;
  end Init_Logger;

  -- Type descriptors
  type Dscr_Rec is record
    -- The character that denotes the type
    Char : Character;
    -- Is <Len> mandatory (True) or optional (False)
    Len_Needed : Boolean;
    -- Len value if it is set
    Len_Value : Natural := 0;
  end record;
  subtype Dscr_Range is Positive range 1 .. 8;
  Dscrs : constant array (Dscr_Range) of Dscr_Rec := (
      ('b', False, 5),
      ('t', False, 5),
      ('l', False, 0),
      ('d', False, 0),
      ('r', False, 0),
      ('s', True,  0),
      ('i', False, 0),
      ('w', False, 0) );

  -- Get descriptor of a char
  function Dscr_Of (Char : Character) return Dscr_Range is
  begin
    for I in Dscr_Range loop
      if Dscrs(I).Char = Char then
        return I;
      end if;
    end loop;
    Logger.Log_Debug ("Unknown Esc >" & Char & "<");
    raise Invalid_Format;
  end Dscr_Of;

  -- Is a char a digit
  function Is_Digit (Char : Character) return Boolean is
  begin
    return Char in '0' .. '9';
  end Is_Digit;

  -- Scan length of a type, update index to point to next char (the type)
  function Get_Length (Format : in String; Ind : in out Positive)
                      return Natural is
    Start : constant Positive := Ind;
  begin
    while Is_Digit (Format(Ind)) loop
      Ind := Ind + 1;
    end loop;
    Logger.Log_Debug ("  Got length " & Format(Start .. Ind - 1));
    return Gets.Get_Int (Format(Start .. Ind - 1));
  exception
    when Constraint_Error =>
      -- Out of Format of error in Get_Int
      Logger.Log_Debug ("Invalid len " & Format(Start .. Format'Last));
      raise Invalid_Format;
  end Get_Length;

  -- Check a Format, raises Invalid_Format
  -- If not Allow_Unknown then raise Unknown_Length is a field has unknown size
  function Check_Format (Format : String;
                         Allow_Unknown : Boolean) return Natural is
    Ind : Positive;
    Char : Character;
    Di : Dscr_Range;
    Len : Integer;
    Unknown : Boolean;
    Total : Natural;
  begin
    Logger.Log_Debug ("Check_Format >" & Format & "<");
    -- Look for Esc sequences
    Unknown := False;
    Total := 0;
    Ind := Format'First;
    while Ind <= Format'Last loop
      if Format(Ind) = Esc then
        if Ind = Format'Last then
          -- Tailing %
          Logger.Log_Debug ("Tailing Esc");
          raise Invalid_Format;
        end if;
        -- Check this is a known Esc sequence
        Ind := Ind + 1;
        Char := Format(Ind);
        if Char = Esc then
          Total := Total + 1;
        else
          -- Get and skip optional length
          Len := -1;
          if Is_Digit (Char) then
            Len := Get_Length (Format, Ind);
            Char := Format(Ind);
          end if;
          -- Get descriptor, check length
          Di := Dscr_Of (Char);
          if Dscrs(Di).Len_Needed
          and then Len = -1 and then Ind /= Format'Last then
            Logger.Log_Debug ("Missing len for " & Char);
            raise Invalid_Format;
          end if;
          if Dscrs(Di).Len_Value /= 0
          and then Len /= -1
          and then Len /= Dscrs(Di).Len_Value then
            Logger.Log_Debug ("Invalid len" & Len'Img & " for " & Char
                            & ", expecting" & Dscrs(Di).Len_Value'Img);
            raise Invalid_Format;
          end if;
          -- Check and sum length
          if Len <= 0 then
            Unknown := True;
            if not Allow_Unknown then
              Logger.Log_Debug ("Unknown length for " & Char);
            end if;
          else
            Total := Total + Len;
          end if;
        end if;
      else
        -- Normal char
        Total := Total + 1;
      end if;
      Ind := Ind + 1;
    end loop;
    if Unknown and then not Allow_Unknown then
      raise Unknown_Length;
    end if;
    return Total;
  end Check_Format;

  -- Compute the length of data induced by Format
  -- Raises Invalid_Format if Format is invalid
  -- Raises Unknown_Length if Format refers to a <type> without specifying
  --  the <len> (thus incluing any reference to a Boolean)
  function Length (Format : String) return Natural is
  begin
    Init_Logger;
    return Check_Format (Format, False);
  end Length;

  -- Scan one item of a given type (char) for a given length, update index
  --  to point to the last parsed char
  function Scan_One (Data : in String; Ind : in out Positive;
                     Char : in Character; Len : in Natural)
           return Any_Def.Any is
    -- First digit and last character to scan
    Start : Positive;
    Stop : Natural;
    -- The result of the scan
    Result : Any_Def.Any;

    -- Look for the integer part "[ ]*[+-]?[0-9]+"
    procedure Look_Int is
    begin
      -- Skip [ ]*
      while Data(Start) = ' ' loop
        Start := Start + 1;
      end loop;
      -- Get [+-]
      if Data(Start) = '+' or else Data(Start) = '-' then
        Start := Start + 1;
      end if;
      -- Look for digits
      Stop := Start - 1;
      for I in Start .. Data'Last loop
        exit when not Is_Digit (Data(I));
        Stop := I;
      end loop;
      -- If no sign nor digit at all, stop may remain before Data'First
      if Stop < Data'First then
        raise Constraint_Error;
      end if;
    exception
      when Constraint_Error =>
        -- Only spaces
        Logger.Log_Debug ("Invalid integer part >"
                        & Data(Ind .. Data'Last) & "<");
        raise Invalid_Data;
    end Look_Int;

    -- Look for the decimal part "(\.[0-9]+)?"
    procedure Look_Dec is
    begin
      Stop := Start;
      if Start = Data'Last then
        return;
      end if;
      -- Get '.'
      Start := Start + 1;
      if Data(Start) /= '.' then
        return;
      end if;
      Stop := Start;
      Start := Start + 1;
      -- Look for digits
      for I in Start .. Data'Last loop
        exit when not Is_Digit (Data(I));
        Stop := I;
      end loop;
    exception
      when Constraint_Error =>
        -- Only spaces
        Logger.Log_Debug ("Invalid decimal part >"
                        & Data(Ind .. Data'Last) & "<");
        raise Invalid_Data;
    end Look_Dec;

    -- Look for the exponent part "([eE][+-]?[0-9]+)?"
    procedure Look_Exp is
    begin
      Stop := Start;
      if Start = Data'Last then
        return;
      end if;
      -- Get 'e or E'
      Start := Start + 1;
      if Data(Start) /= 'e' and then Data(Start) /= 'E' then
        return;
      end if;
      -- Get [+-]
      Start := Start + 1;
      if Data(Start) = '+' or else Data(Start) = '-' then
        Start := Start + 1;
      end if;
      Stop := Start;
      Start := Start + 1;
      -- Look for digits
      for I in Start .. Data'Last loop
        exit when not Is_Digit (Data(I));
        Stop := I;
      end loop;
    exception
      when Constraint_Error =>
        -- Only spaces
        Logger.Log_Debug ("Invalid exponent part >"
                        & Data(Ind .. Data'Last) & "<");
        raise Invalid_Data;
    end Look_Exp;

  begin
    case Char is
      when 'b' =>
        -- Boolean: Scan True or False (any casing) and update index
        if Len = 0 and then Ind + 3 <= Data'Last
        and then Mixed_Str (Data(Ind .. Ind + 3)) = "True" then
          Result := (Kind => Any_Def.Bool_Kind, Bool => True);
          Ind := Ind + 3;
        elsif Len /= 0
        and then Ind + 4 <= Data'Last
        and then (Mixed_Str (Data(Ind .. Ind + 4)) = " True"
          or else Mixed_Str (Data(Ind .. Ind + 4)) = "True ") then
          Result := (Kind => Any_Def.Bool_Kind, Bool => True);
          Ind := Ind + 4;
        elsif Ind + 4 <= Data'Last
        and then Mixed_Str (Data(Ind .. Ind + 4)) = "False" then
          Result := (Kind => Any_Def.Bool_Kind, Bool => False);
          Ind := Ind + 4;
        else
          Logger.Log_Debug ("Invalid boolean >" & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        Logger.Log_Debug ("  Got a " & Any_Def.Image (Result));

      when 't' =>
        -- Trilean: Scan True or False or Other or Maybe (any casing)
        --  and update index
        if Len = 0 and then Ind + 3 <= Data'Last
        and then Mixed_Str (Data(Ind .. Ind + 3)) = "True" then
          Result := (Kind => Any_Def.Trilean_Kind, Tril => Trilean.True);
          Ind := Ind + 3;
        elsif Len /= 0
        and then Ind + 4 <= Data'Last
        and then (Mixed_Str (Data(Ind .. Ind + 4)) = " True"
          or else Mixed_Str (Data(Ind .. Ind + 4)) = "True ") then
          Result := (Kind => Any_Def.Trilean_Kind, Tril => Trilean.True);
          Ind := Ind + 4;
        elsif Ind + 4 <= Data'Last
        and then Mixed_Str (Data(Ind .. Ind + 4)) = "False" then
          Result := (Kind => Any_Def.Trilean_Kind, Tril => Trilean.False);
          Ind := Ind + 4;
        elsif Ind + 4 <= Data'Last
        and then (Mixed_Str (Data(Ind .. Ind + 4)) = "Other"
          or else Mixed_Str (Data(Ind .. Ind + 4)) = "Maybe") then
          Result := (Kind => Any_Def.Trilean_Kind, Tril => Trilean.Other);
          Ind := Ind + 4;
        else
          Logger.Log_Debug ("Invalid Trilean >" & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        Logger.Log_Debug ("  Got a " & Any_Def.Image (Result));

      when 'l' =>
        -- Long integer: Use Len or parse to get length
        if Len /= 0 then
          Stop := Ind + Len - 1;
        else
          Start := Ind;
          Look_Int;
        end if;
        -- Check length and validity
        if Stop > Data'Last then
          Logger.Log_Debug ("Data too short >" & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        if not Regular_Expressions.Match ("[ ]*[+-]?[0-9]+", Data(Ind .. Stop),
                                          True) then
          Logger.Log_Debug ("Invalid long int >"
                          & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        -- Get the int value
        Result := (Kind => Any_Def.Lint_Kind,
                   Lint => Gets.Get_Llint (Data(Ind .. Stop)));
        Ind := Stop;

      when 'd' =>
       -- Decimal: Use Len or parse to get length
        if Len /= 0 then
          Stop := Ind + Len - 1;
        else
          Start := Ind;
          Look_Int;
          Start := Stop;
          Look_Dec;
        end if;
        -- Check length and validity
        if Stop > Data'Last then
          Logger.Log_Debug ("Data too short >" & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        if not Regular_Expressions.Match ("[ ]*[+-]?[0-9]+(\.[0-9]+)?",
                                          Data(Ind .. Stop), True) then
          Logger.Log_Debug ("Invalid duration >"
                          & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        -- Get the duration value
        Result := (Kind => Any_Def.Duration_Kind,
                   Dur  => Gets.Get_Dur (Data(Ind .. Stop)));
        Ind := Stop;

      when 'r' =>
       -- Real: Use Len or parse to get length
        if Len /= 0 then
          Stop := Ind + Len - 1;
        else
          Start := Ind;
          Look_Int;
          Start := Stop;
          Look_Dec;
          Start := Stop;
          Look_Exp;
        end if;
        -- Check length and validity
        if Stop > Data'Last then
          Logger.Log_Debug ("Data too short >" & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        if not Regular_Expressions.Match (
            "[ ]*[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?",
            Data(Ind .. Stop), True) then
          Logger.Log_Debug ("Invalid real >"
                          & Data(Ind .. Data'Last) & "<");
          raise Invalid_Data;
        end if;
        -- Get the real value
        Result := (Kind => Any_Def.Real_Kind,
                   Real => Gets.Get_Real (Data(Ind .. Stop)));
        Ind := Stop;

      when 's' =>
        -- String: Use Len or up to Data'Last
        if Len /= 0 then
          Stop := Ind + Len - 1;
        else
          Stop := Data'Last;
        end if;
        Result := (Kind => Any_Def.Str_Kind,
                   Str => As.U.Tus (Data(Ind .. Stop)));
        Ind := Stop;

      when 'i' =>
        -- Look for digits
        if Len /= 0 then
          Stop := Ind + Len - 1;
        else
          Stop := Ind - 1;
          for I in Ind .. Data'Last loop
            exit when Data(I) not in '_' | '0'..'9' | 'a'..'z' | 'A'..'Z';
            Stop := I;
          end loop;
          if Stop < Data'First then
            Logger.Log_Debug ("Empty identifier >"
                            & Data(Ind .. Data'Last) & "<");
            raise Invalid_Data;
          end if;
        end if;
        Result := (Kind => Any_Def.Str_Kind,
                   Str => As.U.Tus (Data(Ind .. Stop)));
        Ind := Stop;

      when 'w' =>
        -- Look for non space
        if Len /= 0 then
          Stop := Ind + Len - 1;
        else
          Stop := Ind - 1;
          for I in Ind .. Data'Last loop
            exit when Data(I) in Aski.Spc | Aski.Ht | Aski.Cr | Aski.Lf
                               | Aski.Ff | Aski.Vt;
            Stop := I;
          end loop;
        end if;
        Result := (Kind => Any_Def.Str_Kind,
                   Str => As.U.Tus (Data(Ind .. Stop)));
        Ind := Stop;

      when others =>
        -- Already checked when checking the format...
        Logger.Log_Debug ("Unknown Esc >" & Char & "<");
        raise Invalid_Format;
    end case;
    return Result;
  exception
    when Constraint_Error =>
      Logger.Log_Debug ("Invalid data (too long?) >"
                      & Data(Ind .. Data'Last) & "<");
      raise Invalid_Data;
  end Scan_One;

  -- Scan the Data according to Format, return the sequence of refered
  --  data
  -- Raises Invalid_Format if Format is invalid
  -- Raises Invalid_Data if Data does not match Format
  function Scan (Data : String; Format : String) return Any_Sequence is
    Idat, Ifor : Positive;
    -- Is current char a dscr
    Is_Dscr : Boolean;
    -- Length to scan
    Len : Natural;
    -- The parsed element
    Word : Any_Def.Any;
    Result : Any_Sequence;
  begin
    -- Init
    Init_Logger;
    Logger.Log_Debug ("Scanning >" & Data & "< with format >" & Format & "<");
    Len := Check_Format (Format, True);
    -- Check empty format => emty string
    if Format = "" then
      if Data = "" then
        return Result;
      else
        Logger.Log_Debug ("Expecting empty data");
        raise Invalid_Data;
      end if;
    end if;
    -- Scan the format and the string
    Ifor := Format'First;
    Idat := Data'First;
    loop
      -- Check if this is supposed to be a field
      if Format(Ifor) = Esc then
        if Format(Ifor + 1) = Esc then
          -- %% in Format -> % in Str
          Ifor := Ifor + 1;
          Is_Dscr := False;
          Logger.Log_Debug ("  Parsed %%");
        else
          -- A type to scan
          Ifor := Ifor + 1;
          Is_Dscr := True;
        end if;
      else
        Is_Dscr := False;
      end if;
      if Is_Dscr then
        -- Get the optional length
        if Is_Digit (Format(Ifor)) then
          Len := Get_Length (Format, Ifor);
        else
          Len := 0;
        end if;
        -- Parse
        Word := Scan_One (Data, Idat, Format(Ifor), Len);
        Logger.Log_Debug ("  Parsed >" & Any_Def.Image (Word) & "<");
        Result.Append (Word);
      else
        -- Normal char
        if Data(Idat) /= Format(Ifor) then
          Logger.Log_Debug ("Got " & Data(Idat)
                          & " while expecting " & Format(Ifor));
          raise Invalid_Data;
        end if;
        Logger.Log_Debug ("  Parsed " & Data (Idat));
      end if;

      -- Check end conditions
      if Ifor = Format'Last then
        -- Check that end of Str
        if Idat = Data'Last then
          exit;
        else
          Logger.Log_Debug ("Data longer that Format");
          raise Invalid_Data;
        end if;
      end if;
      if Idat = Data'Last and then Ifor /= Format'Last then
        -- Check that end of format
        Logger.Log_Debug ("Data shorter that Format");
        raise Invalid_Data;
      end if;
      Ifor := Ifor + 1;
      Idat := Idat + 1;
    end loop;
    -- Done
    return Result;
  end Scan;

end Scanner;

