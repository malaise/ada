with Text_Io;
separate (Mcd_Mng)

package body Registers is

  Nb_Of_Registers : constant := 2 * 26;
  subtype Register_Range is Positive range 1 .. Nb_Of_Registers;

  Registers_Array : array (Register_Range) of Item_Rec :=
    (others => (Kind => Oper, Val_Oper => Operator_List'First));

  function Reg2Ind (Reg : in Item_Rec) return Register_Range is
  begin
    if Reg.Kind /= Regi then
      raise Invalid_Register;
    end if;
    if Reg.Val_Regi in 'a' .. 'z' then
      return Character'Pos(Reg.Val_Regi) -  Character'Pos('a') + 1;
    elsif Reg.Val_Regi in 'A' .. 'Z' then
      return Character'Pos(Reg.Val_Regi) -  Character'Pos('A') + 1
           + Character'Pos('z') -  Character'Pos('a') + 1;
    else
       raise Invalid_Register;
    end if;
  end Reg2Ind;

  procedure Store (Val : in Item_Rec; To_Reg : in Item_Rec) is
  begin
    if Val.Kind not in Register_Content_List then
      raise Invalid_Argument;
    end if;
    Registers_Array(Reg2Ind(To_Reg)) := Val;
    if Debug.Debug_Level_Array(Debug.Register) then
      Text_Io.Put ("Register: Storing in " & To_Reg.Val_Regi & ": ");
      Debug.Put (Val);
      Text_Io.New_Line;
    end if;
  end Store;
    
  function  Retrieve (From_Reg : in Item_Rec) return Item_Rec is
    Val : Item_Rec;
  begin
    Val := Registers_Array(Reg2Ind(From_Reg));
    if Val.Kind not in Register_Content_List then
      raise Emtpy_Register;
    end if;
    if Debug.Debug_Level_Array(Debug.Register) then
      Text_Io.Put ("Register: Retrieving from " & From_Reg.Val_Regi & ": ");
      Debug.Put (Val);
      Text_Io.New_Line;
    end if;
    return Val;
  end Retrieve;

end Registers;

