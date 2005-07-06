-- Posix regular expression
with Bit_Ops;
package body Regular_Expressions is

  -- C interface --
  function Str4C (Str : String) return String is
  begin
    return Str & Ascii.Nul;
  end Str4C;

  function C_Malloc_Regex return System.Address;
  pragma Interface (C, C_Malloc_Regex);
  pragma Interface_Name (C_Malloc_Regex, "malloc_regex");

  procedure C_Free_Regex (Ptr : in System.Address);
  pragma Interface (C, C_Free_Regex);
  pragma Interface_Name (C_Free_Regex, "free_regex");

  function C_Regcomp (Preg : in System.Address;
                      Regex : in System.Address;
                      Cflags : in Integer) return Integer;
  pragma Interface (C, C_Regcomp);
  pragma Interface_Name (C_Regcomp, "regcomp");
  C_Extended : constant Integer := 1;
  C_Icase    : constant Integer := 2;
  C_Newline  : constant Integer := 4;

  function C_Regexec (Preg : in System.Address;
                      Str : in System.Address;
                      Nmatch : in Long_Integer;
                      Pmatch : in System.Address;
                      Eflags : in Integer) return Integer;
  pragma Interface (C, C_Regexec);
  pragma Interface_Name (C_Regexec, "regexec");
  C_Notbol : constant Integer := 1;
  C_Noteol : constant Integer := 2;

  function C_Regerror (Errcode : in Integer;
                       Preg : in System.Address;
                       Errbuf : in System.Address;
                       Errbuf_Size : in Long_Integer) return Long_Integer;
  pragma Interface (C, C_Regerror);
  pragma Interface_Name (C_Regerror, "regerror");

  procedure C_Regfree (Preg : in System.Address);
  pragma Interface (C, C_Regfree);
  pragma Interface_Name (C_Regfree, "regfree");

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
    Result.Error := C_Regcomp (Result.Comp_Addr, Criteria4C'Address, Cflags);
    Ok := Result.Error = 0;
  end Compile;

  procedure Exec (Criteria : in Compiled_Pattern;
                  To_Check : in String;
                  N_Matched : out Natural;
                  Mach_Info : in out Match_Array;
                  Begin_Line_Match : in Boolean := True;
                  End_Line_Match : in Boolean := True) is
    Eflags : Integer := 0;
    To_Check4C : constant String := Str4C (To_Check);
    Cres : Integer;
    First : constant Integer := To_Check'First;
    use type System.Address;
    use Bit_Ops;
  begin
    -- Init results
    Mach_Info := (others => (Start_Offset => 1, End_Offset => 0));
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
    Cres := C_Regexec (Criteria.Comp_Addr,
                       To_Check4C'Address,
                       Long_Integer(Mach_Info'Length),
                       Mach_Info'Address,
                       Eflags);
    -- Return if no match
    if Cres /= 0 then
      return;
    end if;
    -- Set N_Matched to 1 event if empty Mach_Info
    if Mach_Info'Length = 0 then
      N_Matched := 1;
      return;
   end if;
    -- Set N_Matched to last non-empty Match_Info
    -- Update Match_Info so that it contains indexes in To_Check
    for I in Mach_Info'Range loop
      if Mach_Info(I).Start_Offset /= -1 then
        Mach_Info(I).Start_Offset := Mach_Info(I).Start_Offset + First;
        Mach_Info(I).End_Offset   := Mach_Info(I).End_Offset   + First - 1;
        N_Matched := I;
      end if;
    end loop;
  end Exec;

  -- Compare string to criteria
  -- May raise No_Criteria is Criteria does not compile.
  function Match (Criteria, Str : String) return Boolean is
    Pattern : Compiled_Pattern;
    Ok : Boolean;
    Matched : Natural;
  begin
    Compile (Pattern, Ok, Criteria);
    if not Ok then
      raise No_Criteria;
    end if;
    Exec (Pattern, Str, Matched, No_Match_Array);
    return Matched /= 0;
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

