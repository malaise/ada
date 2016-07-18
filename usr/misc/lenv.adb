with Ada.Exceptions;
with Sys_Calls, Argument, As.U, Directory, Regular_Expressions, Str_Util;
procedure Lenv is

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error (
        "List environment variables whose name match a criteria.");
    Sys_Calls.Put_Line_Error (
        "Print only the names or the names and the values.");
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ -v | --value ] [ <criteria> ]");
    Sys_Calls.Put_Line_Error ("<criteria> ::= <pattern> | @<regex>");
  end Usage;

  Give_Up : exception;
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    raise Give_Up;
  end Error;

  -- Store the criteria and check if a name matches
  package Criteria is
    procedure Set (Crit : in String);
    function Match (Name : String) return Boolean;
  end Criteria;

  package body Criteria is

    Rcrit : constant Character := '@';
    Pattern : As.U.Asu_Us;
    Regex : Regular_Expressions.Compiled_Pattern;

    procedure Set (Crit : in String) is
      Result : Boolean;
    begin
      if Crit = "" then
        -- Empty pattern or regex
        Pattern := As.U.Tus ("*");
      elsif Crit(Crit'First) = Rcrit then
        -- A regex: Store and compile
        Pattern := As.U.Tus (Crit(Integer'Succ(Crit'First) .. Crit'Last));
        Regex.Compile (Result, Pattern.Image);
        if not Result then
          Error ("Invalid regex " & Pattern.Image & " => " & Regex.Error);
        end if;
      else
        -- A pattern: Store and check
        Pattern := As.U.Tus (Crit);
        begin
          Result := Directory.File_Match ("Toto", Pattern.Image);
        exception
          when Directory.Syntax_Error =>
            Error ("Invalid pattern " & Pattern.Image);
        end;
      end if;
    end Set;

    function Match (Name : String) return Boolean is
    begin
      if Regex.Is_Free then
        -- A pattern
        return Directory.File_Match (Name, Pattern.Image);
      else
        -- A Regex: strict match
        declare
          Cell : constant Regular_Expressions.Match_Cell
               := Regex.Match (Name);
        begin
          return Regular_Expressions.Strict_Match (Name, Cell);
        end;
      end if;
    end Match;
  end Criteria;

  -- Print variable value?
  Value_Option : Boolean;

  -- Max num of arguments
  Max_Arg : Positive;

  -- Variable Name and definition (Name=Value)
  Name, Var : As.U.Asu_Us;
begin

  -- Parse arguments
  ------------------
  -- Help
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-h"
    or else Argument.Get_Parameter = "--help") then
    Usage;
    raise Give_Up;
  end if;

  -- 'Value' option
  Value_Option := False;
  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter = "-v"
    or else Argument.Get_Parameter = "--value") then
    Value_Option := True;
  end if;

  -- Criteria
  Max_Arg := (if Value_Option then 2 else 1);
  if Argument.Get_Nbre_Arg > Max_Arg then
    Error ("Invalid argument(s)");
  elsif Argument.Get_Nbre_Arg = Max_Arg then
    -- One criteria
    Criteria.Set (Argument.Get_Parameter (Occurence =>
        (if Value_Option then 2 else 1) ));
  else
    -- No criteria
    Criteria.Set ("");
  end if;

  -- Check all environment variables
  ----------------------------------
  for I in 1 .. Sys_Calls.Environ_Len loop
    -- Get variable definition ("Name=Value" and extract name (up to '=')
    Var := As.U.Tus (Sys_Calls.Environ_Val (I));
    Name := Var.Uslice (1, Str_Util.Locate (Var.Image, "=") - 1);
    if Criteria.Match (Name.Image) then
      -- Put Name or Name=Value
      if Value_Option then
        Sys_Calls.Put_Line_Output (Var.Image);
      else
        Sys_Calls.Put_Line_Output (Name.Image);
      end if;
    end if;
  end loop;


exception
  when Give_Up =>
    Sys_Calls.Set_Error_Exit_Code;
  when Error: others =>
    Sys_Calls.Put_Line_Error ("ERROR: Exception "
        & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Sys_Calls.Set_Error_Exit_Code;
end Lenv;

