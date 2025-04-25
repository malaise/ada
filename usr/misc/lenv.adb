with Ada.Exceptions;
with Sys_Calls, Argument, As.U, Directory, Reg_Exp, Str_Util,
     Long_Long_Dynamic_List;
procedure Lenv is

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name &
        " [ <values> ] { [ <filter> ] }");
    Sys_Calls.Put_Line_Error (
        "List environment variables whose name match criteria.");
    Sys_Calls.Put_Line_Error (
        "  <values>   ::= -v | --values                      // Default: put only names");
    Sys_Calls.Put_Line_Error (
        "  <filter>   ::= [ <modifier> ] { <criteria> }");
    Sys_Calls.Put_Line_Error (
        "  <modifier> ::= -s | --strict | -n | --not-strict  // Default: strict");
    Sys_Calls.Put_Line_Error ("  <criteria> ::= <pattern> | @<regex>");
  end Usage;

  Give_Up : exception;
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    raise Give_Up;
  end Error;

  -- Store the criteria and check if a name matches a criteria
  package Criteria is
    procedure Store (Crit : in String; Strict : in Boolean);
    function Match (Name : String) return Boolean;
  end Criteria;

  package body Criteria is
    -- Regex indicator in the criteria
    Rcrit : constant Character := '@';

    -- List of stored criteria
    type Regex_Access is access all Reg_Exp.Compiled_Pattern;
    type Criteria_Rec is record
      Pattern : As.U.Asu_Us;
      Regex : Regex_Access;
      Strict : Boolean := False;
    end record;
    package Crit_List_Mng is new Long_Long_Dynamic_List (Criteria_Rec);
    package Crit_Mng renames Crit_List_Mng.Dyn_List;
    List : Crit_Mng.List_Type;

    procedure Store (Crit : in String; Strict : in Boolean) is
      Criteria : Criteria_Rec;
      Result : Boolean;
    begin
      if Crit = "" then
        -- Empty pattern or regex
        Criteria.Strict := False;
        Criteria.Pattern := As.U.Tus ("*");
      elsif Crit(Crit'First) = Rcrit then
        -- A regex: Store and compile
        Criteria.Strict := Strict;
        Criteria.Pattern := As.U.Tus (Crit(Integer'Succ(Crit'First)
                                           .. Crit'Last));
        Criteria.Regex := new Reg_Exp.Compiled_Pattern;
        Criteria.Regex.Compile (Result, Criteria.Pattern.Image);
        if not Result then
          Error ("Invalid regex " & Criteria.Pattern.Image
               & " => " & Criteria.Regex.Error);
        end if;
      else
        -- A pattern: Store and check
        Criteria.Strict := Strict;
        Criteria.Pattern := As.U.Tus (Crit);
        begin
          Result := Directory.File_Match ("Toto", Criteria.Pattern.Image);
        exception
          when Directory.Syntax_Error =>
            Error ("Invalid pattern " & Criteria.Pattern.Image);
        end;
      end if;
      List.Insert (Criteria);
    end Store;

    function Match (Name : String) return Boolean is
    begin
      -- If no pattern sotred
      if List.Is_Empty then
        Store ("", False);
      end if;
      -- Search the first match among the stored criteria
      List.Rewind;
      loop
        if List.Access_Current.Regex = null then
          -- A pattern
          if List.Access_Current.Strict then
            if Directory.File_Match (Name,
                List.Access_Current.Pattern.Image) then
              return True;
            end if;
          elsif Directory.File_Match (Name,
                "*" & List.Access_Current.Pattern.Image & "*") then
            return True;
          end if;
        else
          -- A Regex: match
          if List.Access_Current.Regex.Match (Name,
              List.Access_Current.Strict) then
            return True;
          end if;
        end if;
        exit when not List.Check_Move;
        List.Move_To;
      end loop;
      -- No match
      return False;
    end Match;
  end Criteria;

  -- Print variable value?
  Value_Option : Boolean;

  -- Strict or not
  Strict : Boolean;

  -- First argument after option
  First_Arg : Positive;

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
    or else Argument.Get_Parameter = "--values") then
    Value_Option := True;
  end if;

  -- Store criteria
  Strict := True;
  First_Arg := (if Value_Option then 2 else 1);
  for I in First_Arg .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (I) = "-s"
    or else Argument.Get_Parameter (I) = "--strict" then
      Strict := True;
    elsif Argument.Get_Parameter (I) = "-n"
    or else Argument.Get_Parameter (I) = "--not-strict" then
      Strict := False;
    else
      -- One criteria
      if Argument.Get_Parameter (Occurence => I)(1) = '-' then
        Error ("Invalid argument " & String'(Argument.Get_Parameter (I)));
      end if;
      Criteria.Store (Argument.Get_Parameter (I), Strict);
    end if;
  end loop;

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

