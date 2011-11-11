with Basic_Proc, Dynamic_List, Normal, Rnd;
procedure T_Dl is
  package My_Dyn_List is new Dynamic_List(Element_Type => Integer);
  package My_List renames My_Dyn_List.Dyn_List;
  procedure My_Search is new My_List.Search("=");   -- ("=" of Integer)
  procedure My_Search_Raise is new My_List.Search_Raise("=");
  procedure My_Sort is new My_List.Sort("<");  -- ("<" of Integer)

  List : My_List.List_Type;
  List1 : My_List.List_Type;
  Item : Integer;
  Acc : access Integer;
  Found : Boolean;
  Moved : Boolean;
  Count : Natural;

  procedure Put (I : in Integer; New_Line : in Boolean := False) is
  begin
    Basic_Proc.Put_Output (Normal (I, 2, Gap => '0') & ' ');
    if New_Line then Basic_Proc.New_Line_Output; end if;
  end Put;

  procedure Iteration (Current : in out Integer;
                       Go_On   : in out Boolean) is
  begin
    Put (Current);
    if Current rem 2 = 0 then
      Basic_Proc.Put_Output (" is odd");
    else
      Basic_Proc.Put_Output (" is even");
    end if;
    if Current > 10 then
      Basic_Proc.Put_Line_Output (" and stopping iteration");
      begin
        List.Read (Item);
      exception
        when My_List.In_Callback =>
          Basic_Proc.Put_Line_Output ("Read raises In_Callback, OK");
      end;
      Go_On := False;
    else
      Basic_Proc.New_Line_Output;
    end if;
  end Iteration;

  procedure Dump is
    Pos : Natural;
    Moved : Boolean;
  begin
    if List.Is_Empty then
      Basic_Proc.New_Line_Output;
      return;
    end if;

    Pos := List.Get_Position;
    List.Rewind;
    loop
      List.Read (Item, Moved => Moved);
      Put (Item);
      exit when not Moved;
    end loop;
    Basic_Proc.New_Line_Output;
    List.Move_At (Pos);
  end Dump;

begin

  -- Add 10 elements to the list
  Basic_Proc.Put_Line_Output("Adds 10 elements");
  for I in 1 .. 10 loop
    List.Insert(I, My_List.Next);
  end loop;

  -- Read 5 elements from list in reverse
  Basic_Proc.Put_Output("Reads 5 elements from the last one: ");
  List.Rewind(True, My_List.Prev);
  for I in 1 .. 5 loop
    List.Read(Item, My_List.Prev);
    Put(Item);
  end loop;
  Basic_Proc.New_Line_Output;

  -- Dump
  Basic_Proc.Put_Output("List length: ");
  Put(List.List_Length, True);

  -- Delete 5th
  Basic_Proc.Put_Line_Output("Deletes the current");
  List.Delete(Moved => Moved);

  -- Pos and list length
  Basic_Proc.Put_Output("Pos from first: ");
  Put(List.Get_Position, False);
  Basic_Proc.Put_Output("List length: ");
  Put(List.List_Length, True);

  -- Read 7 elements from first
  Basic_Proc.Put_Output("Reads 7 elements from the first one: ");
  List.Rewind;
  for I in 1 .. 7 loop
    List.Read(Item);
    Put(Item);
  end loop;
  Basic_Proc.New_Line_Output;

  -- Add 50 before current
  Basic_Proc.Put_Line_Output("Adds the element 50 before current position");
  List.Insert(50, My_List.Prev);

  Basic_Proc.Put_Output("Store current access and read: ");
  Acc := List.Access_Current;
  Put(Acc.all, True);

  -- List length
  Basic_Proc.Put_Output("List length: ");
  Put(List.List_Length, True);

  -- Read 9 elements from the last
  Basic_Proc.Put_Output("Reads 9 elements from the last one: ");
  List.Move_To(My_List.Prev, 0, False);
  for I in 1 .. 9 loop
    List.Read(Item, My_List.Prev);
    Put(Item);
  end loop;
  Basic_Proc.New_Line_Output;

  -- Move back to saved access and read
  Basic_Proc.Put_Output("Search stored access and read: ");
  List.Search_Access (Found, Acc);
  if not Found then
    Basic_Proc.Put_Line_Output ("NOT FOUND");
    -- This is not normal. Abort.
    raise My_List.Not_In_List;
  end if;
  List.Read(Item, My_List.Current);
  Put(Item, True);

  -- Permute 1st and 4th elements, then search 3 from last
  Basic_Proc.Put_Line_Output("Permute 1st and 4th elements, then search 3 from last");
  List.Permute (0, 3, My_List.Next, False);
  My_Search_Raise (List, 3, My_List.Prev, 1, My_List.Absolute);

  -- Get pos from first and current item
  Basic_Proc.Put_Output("Get current pos from first: ");
  Put(List.Get_Position);
  Basic_Proc.Put_Output(" Get current item: ");
  List.Get (Item);
  Put(Item, True);

  -- Dump
  Basic_Proc.Put_Output("List (length: ");
  Put (List.List_Length, False);
  Basic_Proc.Put_Output (") : ");
  Dump;

  -- Search 50 from first
  Basic_Proc.Put_Line_Output("Seach 50 from first");
  My_Search (List, Found, 50, From => My_List.Absolute);
  if not Found then
    Basic_Proc.Put_Line_Output ("NOT FOUND");
    -- This is not normal. Abort.
    raise My_List.Not_In_List;
  end if;
  -- Search 50 from current, skipping it
  Basic_Proc.Put_Line_Output("Seach 50, skipping current");
  My_Search (List, Found, 50, From => My_List.Skip_Current);
  if not Found then
    Basic_Proc.Put_Line_Output ("Returns not Found, OK");
  end if;

  -- Dump the list
  begin
    Count := 0;
    loop
      Basic_Proc.Put_Output("Pos from first: ");
      Put (List.Get_Position, False);
      Basic_Proc.Put_Output("Pos from last: ");
      Put (List.Get_Position (My_List.From_Last), False);
      Basic_Proc.Put_Output ("Can go to next: ");
      Basic_Proc.Put_Output (Boolean'Image (List.Check_Move (My_List.Next))
                 & " ");
      Basic_Proc.Put_Output("Current item, go to next: ");
      List.Read(Item);
      Put(Item, True);
      Count := Count + 1;
    end loop;
  exception
    when My_List.Not_In_List =>
      if Count = 2 then
        Basic_Proc.Put_Line_Output("raises Not_In_List, OK");
      else
        Basic_Proc.Put_Line_Output("==> NOT IN LIST");
      end if;
  end;

  Basic_Proc.Put_Output("Pos from first: ");
  Put(List.Get_Position, False);
  Basic_Proc.Put_Output("Pos from last: ");
  Put(List.Get_Position (My_List.From_Last), False);
  Basic_Proc.Put_Output("Current item, stay: ");
  List.Read(Item, My_List.Current);
  Put(Item, True);

  -- Read no move
  Basic_Proc.Put_Output("Current item, stay: ");
  List.Read(Item, My_List.Current);
  Put(Item, True);

  -- Iterator
  Basic_Proc.Put_Line_Output ("Iteration");
  List.Iterate (null, 1, My_List.Next, My_List.Absolute,
                   Iteration'Access);

  -- Complete delete
  Basic_Proc.Put_Line_Output("Delete fully the list");
  List.Delete_List;

  Basic_Proc.Put_Output("Get current pos from first: ");
  begin
    Put(List.Get_Position, True);
  exception
    when My_List.Empty_List =>
      Basic_Proc.Put_Line_Output("raises Empty_List, OK");
  end;

  -- List length
  Basic_Proc.Put_Output("List length: ");
  Put(List.List_Length, True);

  -- Sort fixed list
  Basic_Proc.Put_Output("Sort the list: 30 50 42 35: ");
  List.Insert (30);
  List.Insert (50);
  List.Insert (42);
  List.Insert (35);
  My_Sort(List);
  Dump;
  List.Delete_List;

  -- Sort random list
  Basic_Proc.Put_Output ("Make the following random list: ");
  Rnd.Randomize;
  for I in 1 .. Rnd.Int_Random (0, 10) loop
    List.Insert (Rnd.Int_Random(0, 50));
  end loop;
  Dump;
  My_Sort(List);
  Basic_Proc.Put_Output ("After sorting it: ");
  Dump;

  -- Insert a copy
  List1.Insert_Copy (List);
  List.Delete_List;
  List.Insert (21);
  List.Insert (12);
  List.Move_To (My_List.Prev);
  List.Insert_Copy (List1);
  Basic_Proc.Put_Output ("Copied/Inserted in a new list between 21 and 12: ");
  Dump;

end T_Dl;

