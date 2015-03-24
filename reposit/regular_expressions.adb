-- Posix regular expression
with Aski, C_Types, Bit_Ops, Utf_8, Str_Util, Gets;
package body Regular_Expressions is

  -- C interface --
  subtype C_Offset_Range is Integer;
  type C_Match_Cell is record
    Start_Offset : C_Offset_Range;
    Stop_Offset  : C_Offset_Range;
  end record;
  type C_Match_Array is array (Natural range <>) of C_Match_Cell;
  function String4C (Str : String) return String is
  begin
    return Str & Aski.Nul;
  end String4C;

  -- Flags defined in C (those in coment are not used)
  C_Icase    : constant Integer := 16#0001#;
  C_Newline  : constant Integer := 16#0002#;
  C_Notbol   : constant Integer := 16#0004#;
  C_Noteol   : constant Integer := 16#0008#;
  C_Dotall   : constant Integer := 16#0010#;
--C_Nosub    : constant Integer := 16#0020#;
  C_Utf8     : constant Integer := 16#0040#;
--C_Startend : constant Integer := 16#0080#;
  C_Notempty : constant Integer := 16#0100#;
--C_Ungreedy : constant Integer := 16#0200#;
--C_Ucp      : constant Integer := 16#0400#;

  function C_Regvers return System.Address;
  pragma Import (C, C_Regvers, "pcre_version");

  function C_Malloc_Regex return System.Address;
  pragma Import (C, C_Malloc_Regex, "malloc_regex");

  procedure C_Free_Regex (Ptr : in System.Address);
  pragma Import (C, C_Free_Regex, "free_regex");

  function C_Regcomp (Preg : in System.Address;
                      Regex : in System.Address;
                      Cflags : in C_Types.Int) return C_Types.Int;
  pragma Import (C, C_Regcomp, "regcomp");

  function C_Regexec (Preg : in System.Address;
                      Str : in System.Address;
                      Nmatch : in C_Types.Size_T;
                      Pmatch : in System.Address;
                      Eflags : in C_Types.Int) return C_Types.Int;
  pragma Import (C, C_Regexec, "regexec");

  function C_Regerror (Errcode : in C_Types.Int;
                       Preg : in System.Address;
                       Errbuf : in System.Address;
                       Errbuf_Size : in C_Types.Size_T) return C_Types.Size_T;
  pragma Import (C, C_Regerror, "regerror");

  procedure C_Regfree (Preg : in System.Address);
  pragma Import (C, C_Regfree, "regfree");

  -- Get PCRE version
  function Get_Pcre_Version return String is
    Addr, Dummy : System.Address;
    Str : String (1 .. 255);
    function C_Strncpy (Dest, Src : System.Address; Size : Integer)
             return System.Address;
    pragma Import (C, C_Strncpy, "strncpy");
  begin
    -- Returns a char*, make it a string
    Addr := C_Regvers;
    Dummy := C_Strncpy (Str(Str'First)'Address, Addr, Str'Length);
    -- Stop at first space if any
    for I in Str'Range loop
      if Str(I) = Aski.Nul
      or else (Str(I) = ' ' and then I /= Str'First) then
        return Str(Str'First .. I - 1);
      end if;
    end loop;
    return Str;
  end Get_Pcre_Version;

  -- Check PCRE version >= 7.8
  Version_Ok : Boolean := False;
  Invalid_Pcre_Version : exception;
  procedure Check_Pcre_Version is
    Vers : Float;
  begin
    if Version_Ok then
      return;
    end if;
    Vers := Gets.Get_Float (Get_Pcre_Version);
    if Vers < 7.8 then
      raise Invalid_Pcre_Version;
    end if;
    Version_Ok := True;
  exception
    when others =>
      raise Invalid_Pcre_Version;
  end Check_Pcre_Version;

  -- Ada binding
  procedure Compile (Compiled : in out Compiled_Pattern;
                     Ok : out Boolean;
                     Criteria : in String;
                     Case_Sensitive : in Boolean := True;
                     Multi_Line : in Boolean := False;
                     Dot_All : in Boolean := False) is
    Str4C : constant String := String4C (Criteria);
    Cflags : Integer := 0;
    use type System.Address, Language.Language_List;
    use Bit_Ops;
  begin
    Check_Pcre_Version;
    Compiled.Lang := Language.Get_Language;
    -- Set flags
    Cflags := 0;
    if Compiled.Lang = Language.Lang_Utf_8 then
      Cflags := Cflags or C_Utf8;
    end if;
    if not Case_Sensitive then
      Cflags := Cflags or C_Icase;
    end if;
    if Multi_Line then
      Cflags := Cflags or C_Newline;
    end if;
    if Dot_All then
      Cflags := Cflags or C_Dotall;
    end if;
    -- Free previous buffer if needed, allocate buffer if needed
    Compiled.Error := 0;
    if Compiled.Comp_Addr /= System.Null_Address then
      C_Regfree (Compiled.Comp_Addr);
    else
      -- Allocate regex buffer
      Compiled.Comp_Addr := C_Malloc_Regex;
    end if;
    -- Compile
    Compiled.Error := C_Regcomp (Compiled.Comp_Addr, Str4C'Address, Cflags);
    Ok := Compiled.Error = 0;
  end Compile;

  -- Check a regex, return True if OK
  function Check (Criteria : String) return Boolean is
    Compiled : Compiled_Pattern;
    Ok : Boolean;
  begin
    Compile (Compiled, Ok, Criteria);
    Free (Compiled);
    return Ok;
  end Check;

  -- Check if Char is the startup of a valid UTF-8 sequence
  --  and increment Offset accordingly
  procedure Adjust_Utf8 (Char : in Character;
                         Offset : in out Offset_Range) is
    Len : Utf_8.Len_Range;
  begin
    -- Get lenght of sequence
    Len := Utf_8.Nb_Chars (Char);
    -- Apply offset
    Offset := Offset + Len - 1;
  exception
    when Utf_8.Invalid_Sequence =>
      -- Leave Offset unchanged
      null;
  end Adjust_Utf8;

  -- Check that a Match_Cell can be used to extract matching (sub) string
  function Valid_Match (Cell : Match_Cell) return Boolean is
  begin
    return Cell.First_Offset /= 0
           and then Cell.Last_Offset_Stop >= Cell.First_Offset;
  end Valid_Match;

  -- Check that a Match_Cell or Match_Array (returned by Exec or Match)
  --  matches strictly the string Str
  -- Strict means that the complete Str matches the criteria, i.e.
  --  Cells(1).First_Offset = Str'First and
  --  Cells(1).Last_Offset_Stop = Str'Last
  -- Beware that a strict match is not necessarily valid (e.g. Any_Match
  --  strictly matches "" but is not valid)
  function Strict_Match (To_Check : String; Cell : Match_Cell)
           return Boolean is
  begin
    return Cell /= No_Match
           and then Cell.First_Offset = To_Check'First
           and then Cell.Last_Offset_Stop = To_Check'Last;
  end Strict_Match;

  function Strict_Match (To_Check : String; Cells : Match_Array)
           return Boolean is
  begin
    if Cells'Length = 0 then
      return False;
    else
      return Strict_Match (To_Check, Cells(Cells'First));
    end if;
  end Strict_Match;

  -- Exec regex
  procedure Exec (Compiled : in Compiled_Pattern;
                  To_Check : in String;
                  N_Matched : out Natural;
                  Match_Info : out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True) is
    C_Match_Info : C_Match_Array (1 .. Match_Info'Length);
    Eflags : Integer := 0;
    To_Check4C : constant String := String4C (To_Check);
    Cres : Integer;
    First : constant Integer := To_Check'First;
    J : Positive;
    use type System.Address;
    use type Language.Language_List;
    use Bit_Ops;
  begin
    -- Init results
    Match_Info := (others => No_Match);
    N_Matched := 0;
    -- Check criteria has compiled
    if Compiled.Error /= 0
    or else Compiled.Comp_Addr = System.Null_Address then
      raise No_Criteria;
    end if;
    -- Set flags
    Eflags := 0;
    if not Begin_Line_Match then
      Eflags := Eflags or C_Notbol;
    end if;
    if not End_Line_Match then
      Eflags := Eflags or C_Noteol;
    end if;
    if To_Check /= "" then
      -- Close to POSIX behavior
      Eflags := Eflags or C_Notempty;
    end if;
    -- Exec regex
    C_Match_Info := (others => (1, 0));
    Cres := C_Regexec (Compiled.Comp_Addr,
                       To_Check4C'Address,
                       C_Types.Size_T(Match_Info'Length),
                       C_Match_Info'Address,
                       Eflags);
    -- Return if no match
    if Cres /= 0 then
      return;
    end if;
    -- Set N_Matched to 1 even if empty Match_Info
    if Match_Info'Length = 0 then
      N_Matched := 1;
      return;
   end if;
    -- Set N_Matched to last non-empty Match_Info
    -- Update Match_Info so that it contains indexes in To_Check
    J := 1;
    for I in Match_Info'Range loop
      if C_Match_Info(J).Start_Offset /= -1 then
        Match_Info(I).First_Offset := C_Match_Info(J).Start_Offset + First;
        Match_Info(I).Last_Offset_Start :=
             C_Match_Info(J).Stop_Offset + First - 1;
        Match_Info(I).Last_Offset_Stop :=
             C_Match_Info(J).Stop_Offset + First - 1;
        -- Any adjustment due to Lang
        if Compiled.Lang = Language.Lang_Utf_8
        and then Match_Info(I).Last_Offset_Stop >= First then
          Adjust_Utf8 (To_Check(Match_Info(I).Last_Offset_Stop),
                       Match_Info(I).Last_Offset_Stop);
        end if;
        N_Matched := I;
      end if;
      J := J + 1;
    end loop;
  end Exec;

  -- Compare string Str to Criteria
  -- Return a Match_Array of size between 0 (no match) and Max_Match
  -- May raise No_Criteria if Criteria does not compile
  function Match (Criteria, To_Check : String; Max_Match : Positive := 10)
                  return Match_Array is
    Pattern : Compiled_Pattern;
    Ok : Boolean;
    Matched : Natural;
    Match_Info : Match_Array (1 .. Max_Match);
  begin
    Compile (Pattern, Ok, Criteria);
    if not Ok then
      raise No_Criteria;
    end if;
    Exec (Pattern, To_Check, Matched, Match_Info);
    Free (Pattern);
    return Match_Info(1 .. Matched);
  end Match;

  -- Compare string Str to Criteria
  -- Returns No_Match or a Match_Cell
  -- May raise No_Criteria if Criteria does not compile
  function Match (Criteria, To_Check : String) return Match_Cell is
    Match_Info : constant Match_Array := Match (Criteria, To_Check, 1);
  begin
    return (if Match_Info'Length = 0 then No_Match else Match_Info(1));
  end Match;

  -- Compare string Str to Criteria
  -- Returns True or False
  -- May raise No_Criteria if Criteria does not compile.
  function Match (Criteria, To_Check : String;
                  Strict : in Boolean) return Boolean is
    Result : Match_Cell;
  begin
    Result := Match (Criteria, To_Check);
    if not Strict then
      -- Ok if match
      return Result /= No_Match;
    else
      -- Ok if strict match
      return Strict_Match (To_Check, Result);
    end if;
  end Match;

  function Error (Compiled : in Compiled_Pattern) return String is
    Len : C_Types.Size_T;
  begin
    Len := C_Regerror (Compiled.Error, Compiled.Comp_Addr,
                       System.Null_Address, 0);
    if Len <= 0 then
      return "";
    end if;
    declare
      Str : String (1 .. Integer(Len));
    begin
      Len := C_Regerror (Compiled.Error, Compiled.Comp_Addr,
                         Str'Address, Len);
      return Str_Util.Strip (Str (1 .. Integer(Len) - 1));
    end;
  end Error;

  procedure Free (Compiled : in out Compiled_Pattern) is
    use type System.Address;
  begin
    if Compiled.Comp_Addr /= System.Null_Address then
      C_Regfree (Compiled.Comp_Addr);
      C_Free_Regex (Compiled.Comp_Addr);
      Compiled.Comp_Addr := System.Null_Address;
    end if;
    Compiled.Error := 0;
  end Free;

  -- Is compiled pattern free
  function Is_Free  (Compiled : in Compiled_Pattern) return Boolean  is
    use type System.Address;
  begin
    return Compiled.Comp_Addr = System.Null_Address
           and then Compiled.Error = 0;
  end Is_Free;


  overriding procedure Finalize (Criteria : in out Compiled_Pattern) is
  begin
    Free (Criteria);
  end Finalize;

end Regular_Expressions;

