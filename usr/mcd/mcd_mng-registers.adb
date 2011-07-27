with Hashed_List.Unique, Normal;
separate (Mcd_Mng)

package body Registers is

  Nb_Of_Registers : constant := 2 * 26;
  subtype Register_Range is Positive range 1 .. Nb_Of_Registers;

  Empty_Rec : constant Item_Rec
            := (Kind => Oper, Val_Oper => Operator_List'First);

  Registers_Array : array (Register_Range) of Item_Rec
                  := (others => Empty_Rec);

  function Reg2Ind (Reg : Item_Rec) return Register_Range is
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

  function Ind2Reg (Ind : Item_Rec) return Character is
  begin
    if Ind.Kind /= Inte
    or else Ind.Val_Inte < My_Math.Inte(Register_Range'First)
    or else Ind.Val_Inte > My_Math.Inte(Register_Range'Last) then
      raise Invalid_Argument;
    end if;
    if Ind.Val_Inte <= Nb_Of_Registers / 2 then
      return Character'Val(Character'Pos('A') + Ind.Val_Inte - 1);
    else
      return Character'Val(Character'Pos('a') + Ind.Val_Inte - 27);
    end if;
  end Ind2Reg;

  function Is_Register (C : in Character) return Boolean is
  begin
    return C in 'A' .. 'Z' or else C in 'a' .. 'z';
  end Is_Register;

  procedure Check_Reg (Reg : in Item_Rec) is
  begin
    if Reg.Kind /= Regi then
      raise Invalid_Register;
    end if;
    if not Is_Register(Reg.Val_Regi) then
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
      Async_Stdin.Put_Err ("Register: Storing in " & To_Reg.Val_Regi & ": ");
      Debug.Put (Val);
      Async_Stdin.New_Line_Err;
    end if;
  end Store;

  function  Retrieve (From_Reg : Item_Rec) return Item_Rec is
    Val : Item_Rec;
  begin
    Val := Registers_Array(Reg2Ind(From_Reg));
    if Val.Kind not in Register_Content_List then
      raise Empty_Register;
    end if;
    if Debug.Debug_Level_Array(Debug.Register) then
      Async_Stdin.Put_Err ("Register: Retrieving from " & From_Reg.Val_Regi & ": ");
      Debug.Put (Val);
      Async_Stdin.New_Line_Err;
    end if;
    return Val;
  end Retrieve;

  procedure Clear (Reg : in Item_Rec) is
  begin
    Registers_Array(Reg2Ind(Reg)) := Empty_Rec;
  end Clear;

  procedure Clear_All is
  begin
    for R in Registers_Array'Range loop
      Registers_Array(R) := Empty_Rec;
    end loop;
  end Clear_All;

  function Is_Empty (Reg : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool,
            Val_Bool => Registers_Array(Reg2Ind(Reg)) = Empty_Rec);
  end Is_Empty;

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

  function Index_Of (Reg : Item_Rec) return Item_Rec is
  begin
    return (Kind => Inte, Val_Inte => My_Math.Inte(Reg2Ind(Reg)));
  end Index_Of;

  function Register_At (Index : Item_Rec) return Item_Rec is
  begin
    return (Kind => Regi, Val_Regi => Ind2Reg(Index));
  end Register_At;

  -- Array: store / retrieve Var[Index]
  -- Var must be a register; Index must be Inte (otherwise Invalid_Argument)

  -- Key for accessing an array variable: string made of
  --  - the register
  --  - Plus or minus sign
  --  - index padded by zeros
  Key_Width : constant Positive := My_Math.Inte'Width + 1;
  subtype Key_Type is String (1 .. Key_Width);
  function Key_Of (Reg : Item_Rec; Index : Item_Rec) return Key_Type is
    I : Integer;
    Key : Key_Type;
  begin
    -- Check arguments
    Check_Reg (Reg);
    if Index.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    begin
      I := Integer (Index.Val_Inte);
    exception
      when Constraint_Error =>
        raise Invalid_Argument;
    end;
    -- Build key
    Key(1) := Reg.Val_Regi;
    if I >= 0 then
      Key(2) := '+';
    else
      Key(2) := '-';
      I := -I;
    end if;
    Key (3 .. Key_Width) := Normal (I, Key_Width - 2, Gap => '0');
    return Key;
  end Key_Of;

  -- What is stored in Unique_List
  type Storage_Rec is record
    Key : Key_Type;
    Data : Item_Rec;
  end record;
  type Storage_Access is access all Storage_Rec;
  procedure Set (To : out Storage_Rec; Val : in Storage_Rec) is
  begin
    To := Val;
  end Set;
  function Key_Image (Element : Storage_Rec) return String is
  begin
    return Element.Key;
  end Key_Image;
  function "=" (Current : Storage_Rec; Criteria : Storage_Rec)
               return Boolean is
  begin
    return Current.Key = Criteria.Key;
  end "=";
  package H_List_Mng is new Hashed_List (Storage_Rec, Storage_Access, Set,
                                       "=", Key_Image);
  package List_Mng is new H_List_Mng.Unique;
  Array_List : List_Mng.Unique_List_Type;

  procedure Store_Array (Val : in Item_Rec;
                         To_Reg : in Item_Rec; Index : in Item_Rec) is
    Storage : Storage_Rec;
  begin
    if Val.Kind not in Register_Content_List then
      raise Invalid_Argument;
    end if;
    Storage.Key := Key_Of (To_Reg, Index);
    Storage.Data := Val;
    Array_List.Insert (Storage);
  end Store_Array;

  function Retrieve_Array (From_Reg : Item_Rec; Index : Item_Rec)
                          return Item_Rec is
    Storage : Storage_Rec;
  begin
    Storage.Key := Key_Of (From_Reg, Index);
    Array_List.Read (Storage);
    return Storage.Data;
  exception
    when List_Mng.Not_In_List =>
      raise Empty_Register;
  end Retrieve_Array;

  procedure Clear_Array (Reg : in Item_Rec; Index : Item_Rec) is
    Storage : Storage_Rec;
  begin
    Storage.Key := Key_Of (Reg, Index);
    Array_List.Delete (Storage);
  exception
    when List_Mng.Not_In_List =>
      -- Clearing already cleared entry
      null;
  end Clear_Array;

  function Is_Empty_Array (Reg : Item_Rec; Index : Item_Rec)
                            return Item_Rec is
    Storage : Storage_Rec;
    Result : Item_Rec(Bool);
  begin
    Storage.Key := Key_Of (Reg, Index);
    Array_List.Search (Storage, Result.Val_Bool);
    Result.Val_Bool := not Result.Val_Bool;
    return Result;
  end Is_Empty_Array;

end Registers;

