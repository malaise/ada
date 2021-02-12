with Ada.Calendar;
with As.U, Socket, Environ, Images, Hashed_List.Unique, Computer, Aski;
package body Actions is

  -- Image of current time
  function Get_Time return String is (Images.Date_Image (Ada.Calendar.Clock));

  -- Unique list of actions
  type Action is record
    Name, Command : As.U.Asu_Us;
  end record;
  procedure Set (To : out Action; Val : in Action) is
  begin
    To := Val;
  end Set;
  overriding function "=" (Current : Action; Criteria : Action)
                      return Boolean is
    use type As.U.Asu_Us;
  begin
   return Current.Name = Criteria.Name;
  end "=";
  function Key_Image (Element : Action) return String is (Element.Name.Image);
  package Actions_List_Mng is new Hashed_List (Action, Set, "=", Key_Image);
  package Unique_Actions_Mng is new Actions_List_Mng.Unique;
  Actions : Unique_Actions_Mng.Unique_List_Type;

  -- Memory of variables
  Memory : Computer.Memory_Type;

  -- Define a variable
  procedure Define_Variable (Name : in String; Value : in String) is
  begin
    if Name = "Host" or else Name = "Time" or else Name = "Match" then
      raise Invalid_Variable;
    end if;
    Memory.Set (Name, Value, False, True);
  exception
    when Computer.Constant_Exists =>
      raise Invalid_Variable;
  end Define_Variable;

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

  -- Resolve variables in a string
  function Expand_Variables (Text : String) return String renames Memory.Eval;

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
    Actions.Insert ( (As.U.Tus (Name), As.U.Tus (Action) ) );
  end Store;

  -- Check that an action exists
  -- Unknown_Rule : exception;
  function Exists (Name : in String) return Boolean is
    Crit : Action;
  begin
    Crit.Name := As.U.Tus (Name);
    return Actions.Search (Crit);
  end Exists;

  -- Check rule
  -- Return the error or empty
  function Check_Command (Command : String) return String is
    Dummy : As.U.Asu_Us;
  begin
    Init;
    Memory.Set ("Time", Get_Time, True, True);
    Memory.Set ("Match", "Line1" & Aski.Lf & "Line2" & Aski.Lf, True, True);
    Dummy := As.U.Tus (Memory.Eval (Command));
    return "";
  exception
    when Computer.Unknown_Variable =>
      return "Unknown variable " & Variable.Image;
    when Computer.Invalid_Expression =>
      return "Invalid expression " & Command;
  end Check_Command;

  -- Read a rule and expand the action
  -- Init time and line
  function Expand (Name : String; Lines : String) return String is
    The_Action : Action;
  begin
    -- Set time ASAP
    Memory.Set ("Time", Get_Time, True, True);
    Memory.Set ("Match", Lines, True, True);
    -- Find rule
    The_Action.Name := As.U.Tus (Name);
    Actions.Read (The_Action);
    -- Expand
    return Memory.Eval (The_Action.Command.Image);
  exception
    when Unique_Actions_Mng.Not_In_List =>
      raise Unknown_Action;
    when Environ.Name_Error =>
      raise Unknown_Variable;
  end Expand;

end Actions;

