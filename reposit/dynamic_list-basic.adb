package body Dynamic_List.Basic is

  -- Move to first element of list
  procedure Rewind (List : in out List_Type) is
  begin
    if Is_Empty (List) then
      return;
    end if;
    Move_To (List, Next, 0, False);
  end Rewind;

  -- Delete current item in list, moving to previous if possible
  procedure Delete_Current (List : in out List_Type) is
  begin
    if Is_Empty (List) then
      return;
    end if;
    if Get_Position (List) /= 1 then
      Delete (List, Prev);
    else
      Delete (List, Next);
    end if;
  end Delete_Current;

  -- Read current item and moves to next if possible
  -- May raise Empty_List
  procedure Read_Move (List        : in out List_Type;
                       Item        : out Element_Type;
                       End_Of_List : out Boolean) is
  begin
    if Get_Position (List) /= List_Length (List) then
      Read (List, Item, Next);
      End_Of_List := False;
    else
      Read (List, Item, Current);
      End_Of_List := True;
    end if;
  end Read_Move;


  -- Find first occurence from first of matching Item
  -- Return True if found, False otherwise
  procedure Find_First (List  : in out List_Type;
                        Item  : in Element_Type;
                        Found : out Boolean) is
  begin
    if Is_Empty (List) then
      Found := False;
      return;
    end if;
    Search (List, Item, From => Absolute);
    Found :=  True;
  exception
    when Not_In_List =>
      Found := False;
  end Find_First;

end Dynamic_List.Basic;

