with Text_Io;
with Dynamic_List, Normal, Rnd;
procedure T_Dl is
  package My_List is new Dynamic_List(Element_Type => Integer);
  procedure My_Search is new My_List.Search;   -- ("=" of Integer)
  procedure My_Safe_Search is new My_List.Safe_Search;   -- ("=" of Integer)
  procedure My_Sort is new My_List.Sort("<");  -- ("<" of Integer)

  List : My_List.List_Type;
  Item : Integer;
  Found : Boolean;
  Done : Boolean;

  procedure Put (I : in Integer; New_Line : in Boolean := False) is
  begin
    Text_Io.Put (Normal (I, 2, Gap => '0') & ' ');
    if New_Line then Text_Io.New_Line; end if;
  end Put;

  procedure Dump is
    Pos : Natural;
    Done : Boolean;
  begin
    if My_List.Is_Empty(List) then
      Text_Io.New_Line;
      return;
    end if;

    Pos := My_List.Get_Position (List);
    My_List.Rewind (List);
    loop
      My_List.Read (List, Item, Done => Done);
      Put (Item);
      exit when not Done;
    end loop;
    Text_Io.New_Line;
    My_List.Move_To (List, My_List.Next, Pos-1, False);
  end Dump;

begin

  -- add 10 elements to the list
  Text_Io.Put_Line("Adds 10 elements");
  for I in 1 .. 10 loop
    My_List.Insert(List, I, My_List.Next);
  end loop;

  -- read 5 elements from list in reverse
  Text_Io.Put("Reads 5 elements from the last one: ");
  My_List.Rewind(List, My_List.Prev);
  for I in 1 .. 5 loop
    My_List.Read(List, Item, My_List.Prev);
    Put(Item);
  end loop;
  Text_Io.New_Line;

  -- dump
  Text_Io.Put("List length: ");
  Put(My_List.List_Length(List), True);

  -- delete 5th
  Text_Io.Put_Line("Deletes the current");
  My_List.Delete(List, Done => Done);

  -- Pos and list length
  Text_Io.Put("Pos from first: ");
  Put(My_List.Get_Position (List), False);
  Text_Io.Put("List length: ");
  Put(My_List.List_Length(List), True);

  -- read 7 elements from first
  Text_Io.Put("Reads 7 elements from the first one: ");
  My_List.Move_To(List, My_List.Next, 0, False);
  for I in 1 .. 7 loop
    My_List.Read(List, Item);
    Put(Item);
  end loop;
  Text_Io.New_Line;

  -- add 50 before current
  Text_Io.Put_Line("Adds the element 50 before current position");
  My_List.Insert(List, 50, My_List.Prev);

  -- list length
  Text_Io.Put("List length: ");
  Put(My_List.List_Length(List), True);

  -- read 9 elements from the last
  Text_Io.Put("Reads 9 elements from the last one: ");
  My_List.Move_To(List, My_List.Prev, 0, False);
  for I in 1 .. 9 loop
    My_List.Read(List, Item, My_List.Prev);
    Put(Item);
  end loop;
  Text_Io.New_Line;

  -- permute 1st and 4th elements, then search 3 from last
  Text_Io.Put_Line("Permute 1st and 4th elements, then search 3 from last");
  My_List.Permute (List, 0, 3, My_List.Next, False);
  My_Search (List, 3, My_List.Prev, 1, My_List.Absolute);

  -- get pos from first and current item
  Text_Io.Put("Get current pos from first: ");
  Put(My_List.Get_Position (List));
  Text_Io.Put(" Get current item: ");
  My_List.Get (List, Item);
  Put(Item, True);

  -- dump
  Text_Io.Put("List (length: ");
  Put (My_List.List_Length(List), False);
  Text_Io.Put (") : ");
  Dump;

  -- search 50 from first
  Text_Io.Put_Line("Seach 50 from first");
  My_Safe_Search (List, Found, 50, From => My_List.Absolute);
  if not Found then
    Text_Io.Put_Line ("NOT FOUND");
    -- This is not normal. Abort.
    raise My_List.Not_In_List;
  end if;
  -- search 50 from current, skipping it
  Text_Io.Put_Line("Seach 50, skipping current");
  My_Safe_Search (List, Found, 50, From => My_List.Skip_Current);
  if not Found then
    Text_Io.Put_Line ("NOT FOUND");
  end if;

  -- dump the list
  begin
    loop
      Text_Io.Put("Pos from first: ");
      Put (My_List.Get_Position (List), False);
      Text_Io.Put("Pos from last: ");
      Put (My_List.Get_Position (List, My_List.From_Last), False);
      Text_Io.Put ("Can go to next: ");
      Text_Io.Put (Boolean'Image (My_List.Check_Move (List, My_List.Next))
                 & " ");
      Text_Io.Put("Current item, go to next: ");
      My_List.Read(List, Item);
      Put(Item, True);
    end loop;
  exception
    when My_List.Not_In_List =>
      Text_Io.Put_Line("NOT IN LIST");
  end;

  Text_Io.Put("Pos from first: ");
  Put(My_List.Get_Position (List), False);
  Text_Io.Put("Pos from last: ");
  Put(My_List.Get_Position (List, My_List.From_Last), False);
  Text_Io.Put("Current item, stay: ");
  My_List.Read(List, Item, My_List.Current);
  Put(Item, True);

  Text_Io.Put("Current item, stay: ");
  My_List.Read(List, Item, My_List.Current);
  Put(Item, True);

  Text_Io.Put_Line("Delete fully the list");
  My_List.Delete_List (List);

  Text_Io.Put("Get current pos from first: ");
  begin
    Put(My_List.Get_Position(List), True);
  exception
    when My_List.Empty_List => Text_Io.Put_Line("EMPTY LIST");
  end;

  -- list length
  Text_Io.Put("List length: ");
  Put(My_List.List_Length(List), True);

  Text_Io.Put ("Make the following random list: ");
  Rnd.Randomize;
  for I in 1 .. Rnd.Int_Random (0, 10) loop
    My_List.Insert (List, Rnd.Int_Random(0, 50));
  end loop;
  Dump;
  My_Sort(List);
  Text_Io.Put ("After sorting it: ");
  Dump;

end T_Dl;

