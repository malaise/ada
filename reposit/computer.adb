-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
with Str_Util, Parser, Trace;
package body Computer is

  package Logger is new Trace.Basic_Logger ("Computer");
  procedure Log (Msg : in String) renames Logger.Log_Debug;

  -- Operation on stored variables
  procedure Set (To : out Var_Rec; Val : in Var_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Var_Rec) return String is
    (Element.Name.Image);
  overriding function "=" (Current : Var_Rec;
                           Criteria : Var_Rec ) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  -- Variable management
  ----------------------
  -- Reset volatile or all variables
  procedure Reset (Memory : in out Memory_Type; Only_Volatile : in Boolean) is
    Vol_List : As.U.Asu_Us;
    -- Iterator to build list of names of volatile variables
    procedure List_Iter (Current : in Var_Rec;
                         Go_On   : in out Boolean) is
      pragma Unreferenced (Go_On);
    begin
      if not Current.Persistent then
        Vol_List.Append (Current.Name.Image & " ");
      end if;
    end List_Iter;
    Iter : Parser.Iterator;
    Var : Var_Rec;
  begin
    if not Only_Volatile then
      -- Delete all
      Log ("Deleting all variables");
      Memory.Var_List.Delete_List;
      return;
    end if;
    -- Make list of names of volatile variables
    Memory.Var_List.Iterate (List_Iter'Access);
    -- Delete each volatile variable
    Iter.Set (Vol_List.Image, Parser.Is_Space_Or_Htab_Function'Access);
    loop
      declare
        Name : constant String := Iter.Next_Word;
      begin
        exit when Name = "";
        Var.Name := As.U.Tus (Name);
        Log ("Deleting volatile " & Image (Var));
        Memory.Var_List.Delete (Var);
      end;
    end loop;
    Iter.Del;
  end Reset;

  -- Internal check of Name validity
  procedure Check_Name (Name : in String; Caller : in String) is
  begin
    if Name = "" then
      Log (Caller & " raises Invalid_Variable on " & Name);
      raise Invalid_Variable;
    end if;
    for C of Name loop
      if Parser.Is_Space_Or_Htab_Function (C) then
        Log (Caller & " raises Invalid_Variable on " & Name);
        raise Invalid_Variable;
      end if;
    end loop;
  end Check_Name;

  -- Set (store), maybe overwrite a variable
  procedure Set (Memory : in out Memory_Type;
                 Name : in String;
                 Value : in String;
                 Modifiable : in Boolean;
                 Persistent : in Boolean) is
    Var : Var_Rec;
    Found : Boolean;
  begin
    Check_Name (Name, "Set");
    -- Check if this variable exists (persistent or not) and is modifiable
    Var.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Var, Found);
    if Found then
      Memory.Var_List.Read (Var);
      if not Var.Modifiable
      or else not Modifiable then
        -- One of the original or of the new (or both) is not modifiable
        Log ("Set raises Constant_Exists on " & Name);
        raise Constant_Exists;
      end if;
    end if;
    -- Insert or overwrite modifiable variable, or insert new constant
    Var.Value := As.U.Tus (Value);
    Var.Persistent := Persistent;
    Var.Modifiable := Modifiable;
    Memory.Var_List.Insert (Var);
    Log ("Inserted "
         & (if Persistent then "persistent " else "volatile ")
         & (if Modifiable then "variable " else "constant ")
         & Image (Var) & ", " & Value);
  end Set;

  -- Check if a variable is set
  function Is_Set (Memory : in out Memory_Type;
                   Name : in String) return Boolean is
    Crit : Var_Rec;
    Found : Boolean;
  begin
    Check_Name (Name, "Is_Set");
    Crit.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Crit, Found);
    return Found;
  end Is_Set;

  -- Unset a variable
  procedure Unset (Memory : in out Memory_Type;
                   Name : in String) is
    Var : Var_Rec;
    Found : Boolean;
  begin
    Check_Name (Name, "Unset");
    -- Check that this variable exists (persistent or not) and is modifiable
    Var.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Var, Found);
    if not Found then
      Log ("Unset raises Unknown_Variable on " & Name);
      raise Unknown_Variable;
    end if;
    Memory.Var_List.Read (Var);
    if not Var.Modifiable then
      -- This is a constant
      Log ("Unset raises Constant_Exists on " & Name);
      raise Constant_Exists;
    end if;
    Log ("Deleting volatile " & Image (Var));
    Memory.Var_List.Delete (Var);
  end Unset;

  -- Read a variable rec (internal)
  -- May raise Unknown_Variable
  function Read (Memory : in out Memory_Type;
                 Name : in String) return Var_Rec is
    Found : Boolean;
    Res : Var_Rec;
  begin
    Check_Name (Name, "Read");
    -- First check if variable is volatile
    Res.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Res, Found);
    if Found then
      Memory.Var_List.Read (Res);
      Log ("Read " & Name & " => " & Res.Value.Image);
      return Res;
    else
      Log ("Read raises Unknown_Variable on " & Name);
      raise Unknown_Variable;
    end if;
  end Read;

  -- Get a variable
  function Get (Memory : in out Memory_Type;
                Name : in String) return String is
    Var : Var_Rec;
  begin
    Log ("Getting >" & Name & "<");
    Var := Read (Memory, Name);
    return Var.Value.Image;
  end Get;

   -- Get characteristics
  function Is_Modifiable (Memory : in out Memory_Type;
                          Name : in String) return Boolean is
    Var : Var_Rec;
  begin
    Var := Read (Memory, Name);
    return Var.Modifiable;
  end Is_Modifiable;

  function Is_Persistent (Memory : in out Memory_Type;
                          Name : in String) return Boolean is
    Var : Var_Rec;
  begin
    Var := Read (Memory, Name);
    return Var.Persistent;
  end Is_Persistent;


  -- External resolver of variables:
  procedure Set_External_Resolver (Memory : in out Memory_Type;
                                   Resolver : in Resolver_Access) is
  begin
    Memory.External_Resolver := Resolver;
  end Set_External_Resolver;


  -- Resolv variables of an expresssion
  function Internal_Eval (Memory : in out Memory_Type;
                          Expression : in String;
                          Check : in Boolean) return String is
    -- Get a variable, invokes external resolver if needed
    function Ext_Get (Name : String) return String is
    begin
      begin
        -- Get internal variable if set
        return Get (Memory, Name);
      exception
        when Unknown_Variable =>
          if Memory.External_Resolver = null then
            -- Variable is not set and no external resolver
            Log ("Resolv raises Unknown_Variable on " & Name);
            raise;
          end if;
          -- Will go on trying external resolver
      end;
      begin
        return Memory.External_Resolver.all (Name);
      exception
        when others =>
          Log ("Resolv catches resolver exception on " & Name);
          raise Unknown_Variable;
      end;
    end Ext_Get;

    -- Get a variable, check if it is empty or contains spaces
    function Check_Get (Name : String) return String is
      Result : constant String := Ext_Get (Name);
    begin
      if Check and then
      (Result = "" or else Str_Util.Locate (Result, " ") /= 0) then
        Log ("Check_Get raises Invalid_Expression on " & Name);
        raise Invalid_Expression;
      end if;
      return Result;
    end Check_Get;

  begin
    return Str_Util.Eval_Variables (
              Expression, "${", "}", Check_Get'Access,
              Muliple_Passes   => True,
              No_Check_Stop    => False,
              Skip_Backslashed => True);
  exception
    when Str_Util.Inv_Delimiter | Str_Util.Delimiter_Mismatch =>
      Log ("Internal_Eval raises Invalid_Expression on " & Expression);
      raise Invalid_Expression;
  end Internal_Eval;

  function Eval (Memory : in out Memory_Type;
                 Expression : in String) return String is
    (Internal_Eval (Memory, Expression, Check => False));

end Computer;

