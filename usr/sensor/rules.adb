with Ada.Calendar;
with As.U, Socket, Environ, Images, Hashed_List.Unique, Computer, Aski;
package body Rules is

  -- Image of current time
  function Get_Time return String is
  begin
    return Images.Date_Image (Ada.Calendar.Clock);
  end Get_Time;

  -- Unique list of rules
  type Rule is record
    Name, Action : As.U.Asu_Us;
  end record;
  type Rule_Access is access all Rule;
  procedure Set (To : out Rule; Val : in Rule) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Rule; Criteria : Rule) return Boolean is
    use type As.U.Asu_Us;
  begin
   return Current.Name = Criteria.Name;
  end "=";
  function Key_Image (Element : Rule) return String is
  begin
    return Element.Name.Image;
  end Key_Image;
  package Rules_List_Mng is new Hashed_List (Rule, Rule_Access,
                                             Set, "=", Key_Image);
  package Unique_Rules_Mng is new Rules_List_Mng.Unique;
  Rules : Unique_Rules_Mng.Unique_List_Type;

  -- Memory of variables
  Memory : Computer.Memory_Type;

  -- Resolver that gets from ENV and saves the name of an unknown variable
  Variable : As.U.Asu_Us;
  function Resolver (Name : String) return String is
  begin
    return Environ.Getenv_If_Set (Name);
  exception
    when Environ.Name_Error =>
      Variable := As.U.Tus (Name);
      raise Unknown_Variable;
  end Resolver;

  -- Init done once
  Init_Done : Boolean := False;
  procedure Init is
  begin
    if Init_Done then
      return;
    end if;
    Memory.Set ("Host", Socket.Local_Host_Name, False, True);
    Memory.Set_External_Resolver (Resolver'Access);
    Init_Done := True;
  end Init;

  -- Store a rule by name
  -- Init host
  procedure Store (Name : in String; Action : in String) is
  begin
    Init;
    Rules.Insert ( (As.U.Tus (Name), As.U.Tus (Action) ) );
  end Store;

  -- Check that a rule exists
  -- Unknown_Rule : exception;
  function Exists (Name : in String) return Boolean is
    Crit : Rule;
  begin
    Crit.Name := As.U.Tus (Name);
    return Rules.Search (Crit);
  end Exists;

  -- Check rule
  -- Return the error  or empty
  function Check_Action (Action : String) return String is
    Dummy : As.U.Asu_Us;
  begin
    Init;
    Memory.Set ("Time", Get_Time, True, True);
    Memory.Set ("Match", "Line1" & Aski.lf & "Line2" & Aski.Lf, True, True);
    Dummy := As.U.Tus (Memory.Eval (Action));
    return "";
  exception
    when Computer.Unknown_Variable =>
      return "Unknown variable " & Variable.Image;
    when Computer.Invalid_Expression =>
      return "Invalid expression " & Action;
  end Check_Action;

  -- Read a rule and expand the action
  -- Init time and line
  function Expand (Name : String; Lines : String) return String is
    The_Rule : Rule;
  begin
    -- Set time ASAP
    Memory.Set ("Time", Get_Time, True, True);
    Memory.Set ("Match", Lines, True, True);
    -- Find rule
    The_Rule.Name := As.U.Tus (Name);
    Rules.Read (The_Rule);
    -- Expand
    return Memory.Eval (The_Rule.Action.Image);
  exception
    when Unique_Rules_Mng.Not_In_List =>
      raise Unknown_Rule;
    when Environ.Name_Error =>
      raise Unknown_Variable;
  end Expand;

end Rules;

