-- Posix regular expression
with Ada.Characters.Latin_1;
with Bit_Ops, Utf_8;
package body Regular_Expressions is

  -- C interface --
  subtype C_Offset_Range is Integer;
  type C_Match_Cell is record
    Start_Offset : C_Offset_Range;
    Stop_Offset  : C_Offset_Range;
  end record;
  type C_Match_Array is array (Natural range <>) of C_Match_Cell;
    
  function Str4C (Str : String) return String is
  begin
    return Str & Ada.Characters.Latin_1.Nul;
  end Str4C;

  function C_Malloc_Regex return System.Address;
  pragma Import (C, C_Malloc_Regex, "malloc_regex");

  procedure C_Free_Regex (Ptr : in System.Address);
  pragma Import (C, C_Free_Regex, "free_regex");

  procedure C_Regexcpy (Dest, Source : in System.Address);
  pragma Import (C, C_Regexcpy, "regexcpy");

  function C_Regcomp (Preg : in System.Address;
                      Regex : in System.Address;
                      Cflags : in Integer) return Integer;
  pragma Import (C, C_Regcomp, "regcomp");
  C_Extended : constant Integer := 1;
  C_Icase    : constant Integer := 2;
  C_Newline  : constant Integer := 4;

  function C_Regexec (Preg : in System.Address;
                      Str : in System.Address;
                      Nmatch : in Long_Integer;
                      Pmatch : in System.Address;
                      Eflags : in Integer) return Integer;
  pragma Import (C, C_Regexec, "regexec");
  C_Notbol : constant Integer := 1;
  C_Noteol : constant Integer := 2;

  function C_Regerror (Errcode : in Integer;
                       Preg : in System.Address;
                       Errbuf : in System.Address;
                       Errbuf_Size : in Long_Integer) return Long_Integer;
  pragma Import (C, C_Regerror, "regerror");

  procedure C_Regfree (Preg : in System.Address);
  pragma Import (C, C_Regfree, "regfree");

  -- Ada binding
  procedure Compile (Result : in out Compiled_Pattern;
                     Ok : out Boolean;
                     Criteria : in String;
                     Extended : in Boolean := True;
                     Case_Sensitive : in Boolean := True;
                     Match_Newline : in Boolean := True) is
    Criteria4C : constant String := Str4C (Criteria);
    Cflags : Integer := 0;
    use type System.Address;
    use Bit_Ops;
  begin
    -- Set flags
    Cflags := 0;
    if Extended then
      Cflags := Cflags or C_Extended;
    end if;
    if not Case_Sensitive then
      Cflags := Cflags or C_Icase;
    end if;
    if not Match_Newline then
      Cflags := Cflags or C_Newline;
    end if;
    -- Free previous buffer if needed, allocate buffer if needed
    Result.Error := 0;
    if Result.Comp_Addr /= System.Null_Address then
      C_Regfree (Result.Comp_Addr);
    else
      -- Allocate regex buffer
      Result.Comp_Addr := C_Malloc_Regex;
    end if;
    -- Compile
    Result.Lang := Language.Get_Language;
    Result.Error := C_Regcomp (Result.Comp_Addr, Criteria4C'Address, Cflags);
    Ok := Result.Error = 0;
  end Compile;

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

  procedure Exec (Criteria : in Compiled_Pattern;
                  To_Check : in String;
                  N_Matched : out Natural;
                  Match_Info : in out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True) is
    C_Match_Info : C_Match_Array (1 .. Match_Info'Length);
    Eflags : Integer := 0;
    To_Check4C : constant String := Str4C (To_Check);
    Cres : Integer;
    First : constant Integer := To_Check'First;
    J : Positive;
    use type System.Address;
    use type Language.Language_Set_List;
    use Bit_Ops;
  begin
    -- Init results
    Match_Info := (others => No_Match);
    N_Matched := 0;
    -- Check criteria has compiled
    if Criteria.Error /= 0
    or else Criteria.Comp_Addr = System.Null_Address then
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
    -- Exec regex
    C_Match_Info := (others => (1, 0));
    Cres := C_Regexec (Criteria.Comp_Addr,
                       To_Check4C'Address,
                       Long_Integer(Match_Info'Length),
                       C_Match_Info'Address,
                       Eflags);
    -- Return if no match
    if Cres /= 0 then
      return;
    end if;
    -- Set N_Matched to 1 event if empty Match_Info
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
        Match_Info(I).Last_Offset_Start  := C_Match_Info(J).Stop_Offset + First - 1;
        Match_Info(I).Last_Offset_Stop   := C_Match_Info(J).Stop_Offset + First - 1;
        -- Any adjustment due to Lang
        if Criteria.Lang = Language.Lang_Utf_8 then
          Adjust_Utf8 (To_Check(Match_Info(I).Last_Offset_Stop),
                       Match_Info(I).Last_Offset_Stop);
        end if;
        N_Matched := I;
      end if;
      J := J + 1;
    end loop;
  end Exec;

  -- Compare string Str to Criteria
  -- Returns No_Match or a Match_Cell
  -- May raise No_Criteria is Criteria does not compile.
  function Match (Criteria, Str : String) return Match_Cell is
    Pattern : Compiled_Pattern;
    Ok : Boolean;
    Matched : Natural;
    Match_Info : One_Match_Array;
  begin
    Compile (Pattern, Ok, Criteria);
    if not Ok then
      raise No_Criteria;
    end if;
    Exec (Pattern, Str, Matched, Match_Info);
    Free (Pattern);
    return Match_Info(1);
  end Match;

  -- Compare string Str to Criteria
  -- Returns True or False
  -- May raise No_Criteria is Criteria does not compile.
  function Match (Criteria, Str : String;
                  Strict : in Boolean) return Boolean is
    Result : Match_Cell;
  begin
    Result := Match (Criteria, Str);
    if not Strict then
      -- Ok if match
      return Result /= No_Match;
    else
      -- Ok if match indexes are Str indexes
      return Result /= No_Match
      and then Result.First_Offset = Str'First
      and then Result.Last_Offset_Stop = Str'Last;
    end if;
  end Match;

  function Error (Criteria : in Compiled_Pattern) return String is
    Len : Long_Integer;
  begin
    Len := C_Regerror (Criteria.Error, Criteria.Comp_Addr,
                       System.Null_Address, 0);
    if Len <= 0 then
      return "";
    end if;
    declare
      Str : String (1 .. Integer(Len));
    begin
      Len := C_Regerror (Criteria.Error, Criteria.Comp_Addr,
                         Str'Address, Len);
      return Str (1 .. Integer(Len) - 1);
    end;
  end Error;

  procedure Free (Criteria : in out Compiled_Pattern) is
    use type System.Address;
  begin
    if Criteria.Comp_Addr /= System.Null_Address then
      C_Regfree (Criteria.Comp_Addr);
      C_Free_Regex (Criteria.Comp_Addr);
      Criteria.Comp_Addr := System.Null_Address;
    end if;
    Criteria.Error := 0;
  end Free;

  procedure Finalize (Criteria : in out Compiled_Pattern) is
  begin
    Free (Criteria);
  end Finalize;

end Regular_Expressions;

