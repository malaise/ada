with Text_Io;
separate (Mcd_Mng)

package body Registers is

  Nb_Of_Registers : constant := 2 * 26;
  subtype Register_Range is Positive range 1 .. Nb_Of_Registers;

  Empty_Rec : constant Item_Rec
            := (Kind => Oper, Val_Oper => Operator_List'First);

  Registers_Array : array (Register_Range) of Item_Rec
                  := (others => Empty_Rec);

  function Reg2Ind (Reg : in Item_Rec) return Register_Range is
  begin
    if Reg.Kind /= Regi then
      raise Invalid_Register;
    end if;
    if Reg.Val_Regi in 'A' .. 'Z' then
      return Character'Pos(Reg.Val_Regi) -  Character'Pos('A') + 1;
    elsif Reg.Val_Regi in 'a' .. 'z' then
      return Character'Pos(Reg.Val_Regi) -  Character'Pos('a') + 1
           + Character'Pos('Z') -  Character'Pos('A') + 1;
    else
      raise Invalid_Register;
    end if;
  end Reg2Ind;

  procedure Check_Reg (Reg : in Item_Rec) is
  begin
    if Reg.Kind /= Regi then
      raise Invalid_Register;
    end if;
    if Reg.Val_Regi not in 'A' .. 'Z'
    and then Reg.Val_Regi not in 'a' .. 'z' then
      raise Invalid_Register;
    end if;
  end Check_Reg;

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

  procedure Next (Reg : in out Item_Rec) is
  begin
    Check_Reg (Reg);
    if Reg.Val_Regi in 'A' .. 'Y' or else Reg.Val_Regi in 'a' .. 'y' then
      Reg.Val_Regi := Character'Succ(Reg.Val_Regi);
    elsif Reg.Val_Regi = 'Z' then
      Reg.Val_Regi := 'a';
    else
      raise Invalid_Argument;
    end if;
  end Next;

  procedure Prev (Reg : in out Item_Rec) is
  begin
    Check_Reg (Reg);
    if Reg.Val_Regi in 'B' .. 'Z' or else Reg.Val_Regi in 'b' .. 'z' then
      Reg.Val_Regi := Character'Pred(Reg.Val_Regi);
    elsif Reg.Val_Regi = 'a' then
      Reg.Val_Regi := 'Z';
    else
      raise Invalid_Argument;
    end if;
  end Prev;


  procedure Clear_All is
  begin
    for R in Registers_Array'Range loop
      Registers_Array(R) := Empty_Rec;
    end loop;
  end Clear_All;

end Registers;

