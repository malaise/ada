with Ada.Text_Io;
with Dynamic_List, Normal, Rnd;
procedure T_Dl is
  package My_Dyn_List is new Dynamic_List(Element_Type => Integer);
  package My_List renames My_Dyn_List.Dyn_List;
  procedure My_Search is new My_List.Search("=");   -- ("=" of Integer)
  procedure My_Unsafe_Search is new My_List.Unsafe_Search("=");
  procedure My_Sort is new My_List.Sort("<");  -- ("<" of Integer)

  List : My_List.List_Type;
  List1 : My_List.List_Type;
  Item : Integer;
  Acc : My_Dyn_List.Element_Access;
  Found : Boolean;
  Moved : Boolean;

  procedure Put (I : in Integer; New_Line : in Boolean := False) is
  begin
    Ada.Text_Io.Put (Normal (I, 2, Gap => '0') & ' ');
    if New_Line then Ada.Text_Io.New_Line; end if;
  end Put;

  procedure Iteration (Current : in out Integer;
                       Go_On   : in out Boolean) is
  begin
    Put (Current);
    if Current rem 2 = 0 then
      Ada.Text_Io.Put (" is odd");
    else
      Ada.Text_Io.Put (" is even");
    end if;
    if Current > 10 then
      Ada.Text_Io.Put_Line (" and stopping iteration.");
      begin
        List.Read (Item);
      exception
        when My_List.In_Callback =>
          Ada.Text_Io.Put_Line ("IN CALLBACK");
      end;
      Go_On := False;
    else
      Ada.Text_Io.New_Line;
    end if;
  end Iteration;

  procedure Dump is
    Pos : Natural;
    Moved : Boolean;
  begin
    if List.Is_Empty then
      Ada.Text_Io.New_Line;
      return;
    end if;

    Pos := List.Get_Position;
    List.Rewind;
    loop
      List.Read (Item, Moved => Moved);
      Put (Item);
      exit when not Moved;
    end loop;
    Ada.Text_Io.New_Line;
    List.Move_At (Pos);
  end Dump;

begin

  -- Add 10 elements to the list
  Ada.Text_Io.Put_Line("Adds 10 elements");
  for I in 1 .. 10 loop
    List.Insert(I, My_List.Next);
  end loop;

  -- Read 5 elements from list in reverse
  Ada.Text_Io.Put("Reads 5 elements from the last one: ");
  List.Rewind(True, My_List.Prev);
  for I in 1 .. 5 loop
    List.Read(Item, My_List.Prev);
    Put(Item);
  end loop;
  Ada.Text_Io.New_Line;

  -- Dump
  Ada.Text_Io.Put("List length: ");
  Put(List.List_Length, True);

  -- Delete 5th
  Ada.Text_Io.Put_Line("Deletes the current");
  List.Delete(Moved => Moved);

  -- Pos and list length
  Ada.Text_Io.Put("Pos from first: ");
  Put(List.Get_Position, False);
  Ada.Text_Io.Put("List length: ");
  Put(List.List_Length, True);

  -- Read 7 elements from first
  Ada.Text_Io.Put("Reads 7 elements from the first one: ");
  List.Rewind;
  for I in 1 .. 7 loop
    List.Read(Item);
    Put(Item);
  end loop;
  Ada.Text_Io.New_Line;

  -- Add 50 before current
  Ada.Text_Io.Put_Line("Adds the element 50 before current position");
  List.Insert(50, My_List.Prev);

  Ada.Text_Io.Put("Store current access and read: ");
  Acc := List.Access_Current;
  Put(Acc.all, True);

  -- List length
  Ada.Text_Io.Put("List length: ");
  Put(List.List_Length, True);

  -- Read 9 elements from the last
  Ada.Text_Io.Put("Reads 9 elements from the last one: ");
  List.Move_To(My_List.Prev, 0, False);
  for I in 1 .. 9 loop
    List.Read(Item, My_List.Prev);
    Put(Item);
  end loop;
  Ada.Text_Io.New_Line;

  -- Move back to saved access and read
  Ada.Text_Io.Put("Search stored access and read: ");
  List.Search_Access (Found, Acc);
  if not Found then
    Ada.Text_Io.Put_Line ("NOT FOUND");
    -- This is not normal. Abort.
    raise My_List.Not_In_List;
  end if;
  List.Read(Item, My_List.Current);
  Put(Item, True);

  -- Permute 1st and 4th elements, then search 3 from last
  Ada.Text_Io.Put_Line("Permute 1st and 4th elements, then search 3 from last");
  List.Permute (0, 3, My_List.Next, False);
  My_Unsafe_Search (List, 3, My_List.Prev, 1, My_List.Absolute);

  -- Get pos from first and current item
  Ada.Text_Io.Put("Get current pos from first: ");
  Put(List.Get_Position);
  Ada.Text_Io.Put(" Get current item: ");
  List.Get (Item);
  Put(Item, True);

  -- Dump
  Ada.Text_Io.Put("List (length: ");
  Put (List.List_Length, False);
  Ada.Text_Io.Put (") : ");
  Dump;

  -- Search 50 from first
  Ada.Text_Io.Put_Line("Seach 50 from first");
  My_Search (List, Found, 50, From => My_List.Absolute);
  if not Found then
    Ada.Text_Io.Put_Line ("NOT FOUND");
    -- This is not normal. Abort.
    raise My_List.Not_In_List;
  end if;
  -- Search 50 from current, skipping it
  Ada.Text_Io.Put_Line("Seach 50, skipping current");
  My_Search (List, Found, 50, From => My_List.Skip_Current);
  if not Found then
    Ada.Text_Io.Put_Line ("NOT FOUND");
  end if;

  -- Dump the list
  begin
    loop
      Ada.Text_Io.Put("Pos from first: ");
      Put (List.Get_Position, False);
      Ada.Text_Io.Put("Pos from last: ");
      Put (List.Get_Position (My_List.From_Last), False);
      Ada.Text_Io.Put ("Can go to next: ");
      Ada.Text_Io.Put (Boolean'Image (List.Check_Move (My_List.Next))
                 & " ");
      Ada.Text_Io.Put("Current item, go to next: ");
      List.Read(Item);
      Put(Item, True);
    end loop;
  exception
    when My_List.Not_In_List =>
      Ada.Text_Io.Put_Line("NOT IN LIST");
  end;

  Ada.Text_Io.Put("Pos from first: ");
  Put(List.Get_Position, False);
  Ada.Text_Io.Put("Pos from last: ");
  Put(List.Get_Position (My_List.From_Last), False);
  Ada.Text_Io.Put("Current item, stay: ");
  List.Read(Item, My_List.Current);
  Put(Item, True);

  -- Read no move
  Ada.Text_Io.Put("Current item, stay: ");
  List.Read(Item, My_List.Current);
  Put(Item, True);

  -- Iterator
  Ada.Text_Io.Put_Line ("Iteration");
  List.Iterate (null, 1, My_List.Next, My_List.Absolute,
                   Iteration'Access);

  -- Complete delete
  Ada.Text_Io.Put_Line("Delete fully the list");
  List.Delete_List;

  Ada.Text_Io.Put("Get current pos from first: ");
  begin
    Put(List.Get_Position, True);
  exception
    when My_List.Empty_List =>
      Ada.Text_Io.Put_Line("EMPTY LIST");
  end;

  -- List length
  Ada.Text_Io.Put("List length: ");
  Put(List.List_Length, True);

  -- Sort
  Ada.Text_Io.Put ("Make the following random list: ");
  Rnd.Randomize;
  for I in 1 .. Rnd.Int_Random (0, 10) loop
    List.Insert (Rnd.Int_Random(0, 50));
  end loop;
  Dump;
  My_Sort(List);
  Ada.Text_Io.Put ("After sorting it: ");
  Dump;

  -- Insert a copy
  List1.Insert_Copy (List);
  List.Delete_List;
  List.Insert (21);
  List.Insert (12);
  List.Move_To (My_List.Prev);
  List.Insert_Copy (List1);
  Ada.Text_Io.Put ("Copied/Inserted in a new list between 21 and 12: ");
  Dump;

end T_Dl;

