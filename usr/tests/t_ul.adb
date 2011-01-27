-- Stores ENV variables in a unique list
-- Modifies and retrieve list entries according to arguments
--  <name> | <name>=<value> | dump
with Ada.Text_Io;
with Hashed_List.Unique, Argument, As.B, String_Mng, Sys_Calls;
procedure T_Ul is

  -- A stored variable (name, value)
  -- Name is the "identifier"
  subtype Name_Txt is As.B.Asb_Bs(80);
  subtype Val_Txt is As.B.Asb_Bs(8024);
  type Var_Rec is record
    Name : Name_Txt;
    Val : Val_Txt;
  end record;
  type Var_Acc is access all Var_Rec;

  procedure Set (To : out Var_Rec; Val : in Var_Rec) is
  begin
    To.Name.Set (Val.Name);
    To.Val.Set (Val.Val);
  end Set;
  function Image (Element : Var_Rec) return String is
  begin
    return Element.Name.Image;
  end Image;
  function "=" (Current : Var_Rec; Criteria : Var_Rec) return Boolean is
    use type As.B.Asb_Bs;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  package H_Ul is new Hashed_List (Var_Rec, Var_Acc, Set, "=", Image);
  package My_Ul is new H_Ul.Unique;
  Ul : My_Ul.Unique_List_Type;

  -- Store a env var (from string "name=val")
  function Store_Env (Str : in String) return Boolean is
    L : Natural;
    Var : Var_Rec;
  begin
    L := String_Mng.Locate (Str, "=");
    if L = 0 then
      return False;
    end if;
    Var.Name.Set (Str (Str'First .. L - 1));
    Var.Val.Set (Str (L + 1 .. Str'Last));
    My_Ul.Insert (Ul, Var);
    return True;
  exception
    when Constraint_Error =>
      Sys_Calls.Put_Line_Error ("String too long for storage >" & Str & "<");
      return False;
  end Store_Env;

  procedure Put (Var : in Var_Rec) is
  begin
    Ada.Text_Io.Put (">" & Var.Name.Image & "<->" & Var.Val.Image & "<");
  end Put;
  procedure Iteration (Current : Var_Rec; Go_On : in out Boolean) is
    pragma Unreferenced (Go_On);
  begin
    Put (Current);
    Ada.Text_Io.New_Line;
  end Iteration;

  Var : Var_Rec;
  Go_On : Boolean;
begin
  -- Store env in unique list
  for I in 1 .. Sys_Calls.Environ_Len loop
    if not Store_Env (Sys_Calls.Environ_Val(I)) then
      Sys_Calls.Set_Error_Exit_Code;
      return;
    end if;
  end loop;

  -- Process argument: variable (re) definition or get
  Ada.Text_Io.Put_Line ("Processing arguments:");
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (Occurence => I) = "dump" then
      -- Dump the list
      Ada.Text_Io.Put_Line ("List dump:");
      My_Ul.Iterate (Ul, Iteration'Access);
      Ada.Text_Io.New_Line;
    elsif Store_Env (Argument.Get_Parameter (Occurence => I)) then
      -- Try to store variable (will work if string constains "=")
      Ada.Text_Io.Put_Line ("Inserted >"
                          & Argument.Get_Parameter (Occurence => I) & "<");
    else
      -- Try to fetch the variable
      begin
        Var.Name.Set (Argument.Get_Parameter (Occurence => I));
        Go_On := True;
      exception
        when others =>
          Sys_Calls.Put_Line_Error ("String too long for storage >"
            & Argument.Get_Parameter (Occurence => I) & "<");
          Go_On := False;
      end;
      if Go_On then
        begin
          Ul.Read (Var);
          Ada.Text_Io.Put ("Got ");
          Put (Var);
          Ada.Text_Io.New_Line;
        exception
          when My_Ul.Not_In_List =>
            Ada.Text_Io.Put_Line ("Var >" & Var.Name.Image &  "< is not set.");
        end;
      end if;
    end if;
  end loop;

end T_Ul;

