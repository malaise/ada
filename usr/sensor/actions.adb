with Ada.Calendar;
with Socket, Environ, Images, Hashed_List.Unique, Long_Long_Limited_List,
     Computer, Aski, Long_Longs;
with Debug;
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

  -- List of repetitions
  --   list of occurences
  procedure Set (To : out Ada.Calendar.Time; Val : in Ada.Calendar.Time) is
  begin
    To := Val;
  end Set;
  package Occurences_Mng is new Long_Long_Limited_List (Ada.Calendar.Time, Set);
  type Occurences_Access is access Occurences_Mng.List_Type;
  --  a repetition
  type Repeat is record
    Repeated_Action : As.U.Asu_Us;
    Number : Positive;
    During : Duration;
    Launch_Action : As.U.Asu_Us;
    Occurences : Occurences_Access;
  end record;
  procedure Set (To : out Repeat; Val : in Repeat) is
  begin
    To := Val;
  end Set;
  package Repeats_Mng is new Long_Long_Limited_List (Repeat, Set);
  Repeats : Repeats_Mng.List_Type;

  -- Memory of variables
  Memory : Computer.Memory_Type;

  -- Define a variable
  procedure Define_Variable (Name : in String; Value : in String) is
  begin
    if Name = "Host" or else Name = "Time"
       or else Name = "Match" or else Name = "Action" then
      raise Invalid_Variable;
    end if;
    Memory.Set (Name, Value, False, True);
  exception
    when Computer.Constant_Exists =>
      raise Invalid_Variable;
  end Define_Variable;

  -- Define / Undefine the triggereing Action
  procedure Set_Action (Value : in String) is
  begin
    Memory.Set ("Action", Value, True, True);
  exception
    when Computer.Constant_Exists =>
      raise Invalid_Variable;
  end Set_Action;

  procedure Unset_Action is
  begin
    Memory.Unset ("Action");
  exception
    when Computer.Unknown_Variable | Computer.Constant_Exists =>
      raise Invalid_Variable;
  end Unset_Action;

  -- Resolver that gets from ENV and saves the name of an unknown variable
  Variable : As.U.Asu_Us;
  function Resolver (Name : String) return String is
  begin
    return Environ.Getenv_If_Set (Name);
  exception
    when Environ.Name_Error =>
      Variable := As.U.Tus (Name);
      Debug.Log ("Unknown variable " & Name);
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

  -- Resolve variables in a string
  function Expand_Variables (Text : String) return String is
  begin
    Init;
    return Memory.Eval (Text);
  end Expand_Variables;

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
    Memory.Set ("Action", "Action", True, True);
    Dummy := As.U.Tus (Memory.Eval (Command));
    return "";
  exception
    when Computer.Unknown_Variable =>
      -- Variable is set by the resolver
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

   -- Register a repetition
  procedure Add_Repeat (Name : in String;
                        Number : in Positive;
                        During : in Duration;
                        Triggers : in String) is
    Rep : Repeat;
  begin
    Rep.Repeated_Action := As.U.Tus (Name);
    Rep.Number := Number;
    if During < 1.0 then
      raise Constraint_Error;
    end if;
    Rep.During := During;
    Rep.Launch_Action := As.U.Tus (Triggers);
    Rep.Occurences := new Occurences_Mng.List_Type;
    Repeats.Insert (Rep);
  end Add_Repeat;

  -- Purge old occurences of one Repeat
  procedure Purge (Now : in Ada.Calendar.Time; Rep : in Repeat) is
    use type Ada.Calendar.Time;
    Start : constant Ada.Calendar.Time := Now - Rep.During;
    Moved : Boolean;
  begin
    if Rep.Occurences.Is_Empty then
      return;
    end if;
    -- Scan from older occurences
    Rep.Occurences.Rewind;
    loop
      if Rep.Occurences.Access_Current.all < Start then
        -- Purge this one
        Rep.Occurences.Delete (Moved => Moved);
        exit when not Moved;
      else
        -- No more occurence to purge
        exit;
      end if;
    end loop;
  end Purge;

  -- Purge old occurences of all repeats
  procedure Purge is
    Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
  begin
    if Repeats.Is_Empty then
      return;
    end if;
    -- Process all Repeats
    Repeats.Rewind;
    loop
      Purge (Now, Repeats.Access_Current.all);
      exit when not Repeats.Check_Move;
      Repeats.Move_To;
    end loop;
  end Purge;

  -- Declare one or several occurences
  -- Which may trigger the execution of several actions
  function Occurs (Action : in String; Nb : in Positive)
           return As.U.Utils.Asu_Array is
    Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    Rep : Repeat;
    Result : As.U.Utils.Asu_Ua.Unbounded_Array;
    use type As.U.Asu_Us, Long_Longs.Llu_Natural;
  begin
    if Repeats.Is_Empty then
      return As.U.Utils.Asu_Ua.To_Array (Result);
    end if;
    -- Check all Repeats
    Repeats.Rewind;
    loop
      -- For each Repeat on this action
      if Repeats.Access_Current.Repeated_Action = Action then
        Rep := Repeats.Access_Current.all;
        --  Purge "old" occurences (before Now - During)
        Purge (Now, Rep);
        --  Add Nb occurences at Now
        if not Rep.Occurences.Is_Empty then
          Rep.Occurences.Rewind (Occurences_Mng.Prev);
        end if;
        for I in 1 .. Nb loop
          Rep.Occurences.Insert (Now);
        end loop;
        --  If the number of occurences is above the max then
        --    append the launch to the result
        if Rep.Occurences.List_Length
           >= Long_Longs.Llu_Positive (Rep.Number) then
          Rep.Occurences.Delete_List;
          Result.Append (Rep.Launch_Action);
        end if;
      end if;
      exit when not Repeats.Check_Move;
      Repeats.Move_To;
    end loop;
    return As.U.Utils.Asu_Ua.To_Array (Result);
  end Occurs;

end Actions;

