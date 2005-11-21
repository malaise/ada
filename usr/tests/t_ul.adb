with Ada.Text_Io;
with Unique_List, Argument, Text_Handler, String_Mng, Sys_Calls;
procedure T_Ul is

  -- A stored variable (name, value)
  -- Name is the "identifier"
  subtype Name_Txt is Text_Handler.Text (80);
  subtype Val_Txt is Text_Handler.Text (1024);
  type Var_Rec is record
    Name : Name_Txt;
    Val : Val_Txt;
  end record;

  procedure Set (To : out Var_Rec; Val : in Var_Rec) is
  begin
    Text_Handler.Set (To.Name, Val.Name);
    Text_Handler.Set (To.Val, Val.Val);
  end Set;
  function Image (Element : Var_Rec) return String is
  begin
    return Text_Handler.Value (Element.Name);
  end Image;
  function "=" (Current : Var_Rec; Criteria : Var_Rec) return Boolean is
    use type Text_Handler.Text;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  package My_Ul is new Unique_List (Var_Rec, Set, Image, "=");
  Ul : My_Ul.List_Type;

  -- Store a env var (from string "name=val")
  function Store_Env (Str : in String) return Boolean is
    L : Natural;
    Var : Var_Rec;
  begin
    L := String_Mng.Locate (Str, "=");
    if L = 0 then
      return False;
    end if;
    Text_Handler.Set (Var.Name, Str (Str'First .. L - 1));
    Text_Handler.Set (Var.Val, Str (L + 1 .. Str'Last));
    My_Ul.Insert (Ul, Var);
    return True;
  exception
    when Constraint_Error =>
      Sys_Calls.Put_Line_Error ("String too long for storage >" & Str & "<");
      return False;
  end Store_Env;

  procedure Put (Var : in Var_Rec) is
  begin
    Ada.Text_Io.Put (">" & Text_Handler.Value(Var.Name)
                 & "<->" & Text_Handler.Value(Var.Val) & "<");
  end Put;
  procedure Iteration (Current : Var_Rec; Go_On : in out Boolean) is
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

  -- Process argument: varialbe (re) definition or get
  Ada.Text_Io.Put_Line ("Processing arguments:");
  for I in 1 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (Occurence => I) = "dump" then
      -- Dump the list
      Ada.Text_Io.Put_Line ("List dump:");
      My_Ul.Iterate (Ul, Iteration'Unrestricted_Access);
      Ada.Text_Io.New_Line;
    elsif Store_Env (Argument.Get_Parameter (Occurence => I)) then
      -- Try to store variable (will work if string constains "=")
      Ada.Text_Io.Put_Line ("Inserted >"
                          & Argument.Get_Parameter (Occurence => I) & "<");
    else
      -- Try to fetch the variable
      begin
        Text_Handler.Set (Var.Name, Argument.Get_Parameter (Occurence => I));
        Go_On := True;
      exception
        when others =>
          Sys_Calls.Put_Line_Error ("String too long for storage >"
            & Argument.Get_Parameter (Occurence => I) & "<");
          Go_On := False;
      end;
      if Go_On then
        begin
          My_Ul.Read (Ul, Var, Var);
          Ada.Text_Io.Put ("Got ");
          Put (Var);
          Ada.Text_Io.New_Line;
        exception
          when My_Ul.Not_In_List =>
            Ada.Text_Io.Put_Line ("Var >" & Text_Handler.Value (Var.Name)
                               &  "< is not set.");
        end;
      end if;
    end if;
  end loop;
  Ada.Text_Io.New_Line;

end T_Ul;

