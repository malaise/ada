with Upper_Str;
with Str_Mng;
package body Pers_Mng is

  -- Search a person knowing its pid
  procedure Search (List : in out Pers_Def.Person_List;
                    Pid  : in Pers_Def.Pid_Range;
                    Pos  : out Natural) is
    List_Length : constant Natural
                := Pers_Def.Person_List_Mng.List_Length (List);
    Person : Pers_Def.Person_Rec;
    Loc_Pos : Natural;
    use Pers_Def;

  begin
    Pos := 0;
    if List_Length = 0 then
      return;
    end if;

    -- Move to beginning
    Pers_Def.Person_List_Mng.Rewind (List);
    -- Read persons, look for Pid
    for I in 1 .. List_Length loop
      if I /= List_Length then
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Next);
      else
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Current);
      end if;

      if Person.Pid = Pid then
        -- Return when Pid found
        Pos := I;
        Loc_Pos := I;
        -- Move to this pos
        Pers_Def.Person_List_Mng.Move_To (List, Pers_Def.Person_List_Mng.Next,
         Loc_Pos - 1, False);
        return;
      end if;
    end loop;

  end Search;

  -- Search a person knowing its name and activity
  procedure Search (List     : in out Pers_Def.Person_List;
                    Name     : in Pers_Def.Person_Name_Str;
                    Activity : in Pers_Def.Person_Activity_Str;
                    Pos      : out Natural) is
    List_Length : constant Natural
                := Pers_Def.Person_List_Mng.List_Length (List);
    Person : Pers_Def.Person_Rec;
    Loc_Pos : Natural;
  begin
    Pos := 0;
    if List_Length = 0 then
      return;
    end if;

    -- Move to beginning
    Pers_Def.Person_List_Mng.Rewind (List);
    -- Read persons, look for (Name, Activity)
    for I in 1 .. List_Length loop
      if I /= List_Length then
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Next);
      else
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Current);
      end if;

      if Person.Name = Name and then Person.Activity = Activity then
        -- Return when Name and Activity found
        Pos := I;
        Loc_Pos := I;
        -- Move to this pos
        Pers_Def.Person_List_Mng.Move_To (List, Pers_Def.Person_List_Mng.Next,
         Loc_Pos - 1, False);
        return;
      end if;
    end loop;


  end Search;

  -- The Expand call affects the current position in list
  -- Name must not be empty otherwise nothing is expanded
  -- Activity can be empty or partial
  -- If Activity is empty:
  --   If Name allows only one and only one name expansion then
  --     it is expanded if necessary and Pos is set to 0
  --   Else
  --     Pos is set to -1 (not found or not one possible expansion)
  -- Else (Activity is set)
  --   If Name and Activity allow one and only one expansion, then
  --     Name is expanded if necessary and Pos is set to its position
  --   Else
  --     Pos is set to -1
  procedure Expand (List     : in out Pers_Def.Person_List;
                    Name     : in out Pers_Def.Person_Name_Str;
                    Activity : in out Pers_Def.Person_Activity_Str;
                    Pos      : out Integer) is
    Empty_Name     : constant Pers_Def.Person_Name_Str     := (others => ' ');
    Empty_Activity : constant Pers_Def.Person_Activity_Str := (others => ' ');
    First_Name, Last_Name, Len_Name : Positive;
    First_Activity, Last_Activity, Len_Activity : Positive;
    List_Length : constant Natural
                := Pers_Def.Person_List_Mng.List_Length (List);
    Person : Pers_Def.Person_Rec;
    Matching_Person : Pers_Def.Person_Rec;
    Match_Activity : constant Boolean := Activity /= Empty_Activity;
    Loc_Pos : Integer;
  begin
    Pos := -1;
    Str_Mng.Parse (Name);
    Name := Upper_Str (Name);
    Str_Mng.Parse (Activity);
    Activity := Upper_Str (Activity);

    -- No empty list!
    if List_Length = 0 then
      return;
    end if;
    -- No empty name
    if Name = Empty_Name then
      return;
    end if;

    -- Locate end of Name
    Last_Name := Name'Last;
    for I in reverse Name'Range loop
      if Name(I) /= ' ' then
        Last_Name := I;
        exit;
      end if;
    end loop;
    First_Name := Name'First;
    Len_Name := Last_Name - First_Name + 1;
    -- Locate end of Activity
    Last_Activity := Activity'Last;
    for I in reverse Activity'Range loop
      if Activity(I) /= ' ' then
        Last_Activity := I;
        exit;
      end if;
    end loop;
    First_Activity := Activity'First;
    Len_Activity := Last_Activity - First_Activity + 1;

   -- Scan the list
   Loc_Pos := -1;
   Matching_Person.Name := Empty_Name;
   Matching_Person.Activity := Empty_Activity;
   Pers_Def.Person_List_Mng.Rewind (List);
   for I in 1 .. List_Length loop
      Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Current);
      -- This person name matches
      if Person.Name(1 .. Len_Name) = Name(First_Name .. Last_Name) then
        -- This person name matches
        if not Match_Activity
        or else Person.Activity(1 .. Len_Activity) =
                Activity(First_Activity .. Last_Activity) then
          -- This person matches
          if Loc_Pos = -1 then
            -- First matching person
            Matching_Person.Name := Person.Name;
            if Match_Activity then
              -- Same name, same activity, may be ok if sole
              Matching_Person.Activity := Person.Activity;
              Loc_Pos  := Pers_Def.Person_List_Mng.Get_Position (List);
            else
              Loc_Pos := 0;
            end if;
          else
            -- Not first matching : not sole
            Loc_Pos := 0;
            -- Another matching person was found. Same name or activity
            if Person.Name /= Matching_Person.Name
            or else (Match_Activity
                     and then Person.Activity /= Matching_Person.Activity) then
              -- Several different names matching : abort
              Loc_Pos := -1;
              return;
            end if;
          end if;
        end if;
      end if;
      if I /= List_Length then
        Pers_Def.Person_List_Mng.Move_To (List);
      end if;
    end loop;

    if Loc_Pos /= -1 then
      Name := Matching_Person.Name;
      if Loc_Pos /= 0 then
        Pers_Def.Person_List_Mng.Move_To (List, Pers_Def.Person_List_Mng.Next,
                                          Loc_Pos - 1, False);
        Activity := Matching_Person.Activity;
      end if;
      Pos := Loc_Pos;
    end if;
  end Expand;


  function Order_Name (Left, Right : Pers_Def.Person_Rec) return Boolean is
  begin
    return Left.Name < Right.Name;
  end Order_Name;
  procedure Sort_Name is new Pers_Def.Person_List_Mng.Sort (Order_Name);

  -- Get first and last index (in list) of persons with the provided name
  procedure Select_By_Name (List : in out Pers_Def.Person_List;
                            Name : in Pers_Def.Person_Name_Str;
                            First, Last : out Natural) is
    List_Length : constant Natural
                := Pers_Def.Person_List_Mng.List_Length (List);
    Person : Pers_Def.Person_Rec;
    Found : Boolean;
    Loc_Pos : Natural;
    Loc_First : Natural;
    Loc_Last : Natural;
  begin
    Loc_First := 0;
    Loc_Last := 0;
    if List_Length = 0 then
      First := Loc_First;
      Last := Loc_Last;
      return;
    end if;

    -- Sort list by names
    Sort_Name (List);

    -- Look for first and last persons with matching name
    Found := False;
    for I in 1 .. List_Length loop
      if I /= List_Length then
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Next);
      else
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Current);
      end if;
      if not Found then
        if Person.Name = Name then
          Loc_First := I;
          Loc_Pos := I;
          Found := True;
        end if;
      elsif Person.Name /= Name then
        Loc_Last := I - 1;
        exit;
      end if;
    end loop;
    if Found then
      -- Last is last of list
      if Loc_Last = 0 then
        Loc_Last := List_Length;
      end if;
      -- Move to first found
      Pers_Def.Person_List_Mng.Move_To (List, Pers_Def.Person_List_Mng.Next,
       Loc_Pos - 1, False);
    end if;
    First := Loc_First;
    Last := Loc_Last;
  end Select_By_Name;

  -- Get first and last index (in list) of persons with the provided activity
  procedure Select_By_Activity (List : in out Pers_Def.Person_List;
                                Activity : in Pers_Def.Person_Activity_Str;
                                First, Last : out Natural) is
    List_Length : constant Natural
                := Pers_Def.Person_List_Mng.List_Length (List);
    Person : Pers_Def.Person_Rec;
    Found : Boolean;
    Loc_Pos : Natural;
  begin
    First := 0;
    Last := 0;
    if List_Length = 0 then
      return;
    end if;

    -- Sort list by names
    Sort_Name (List);

    -- Look for first and last persons with matching name
    Found := False;
    for I in 1 .. List_Length loop
      if I /= List_Length then
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Next);
      else
        Pers_Def.Person_List_Mng.Read (List, Person,
         Pers_Def.Person_List_Mng.Current);
      end if;
      if not Found then
        if Person.Activity = Activity then
          First := I;
          Loc_Pos := I;
          Found := True;
        end if;
      elsif Person.Activity /= Activity then
        Last := I - 1;
        -- Move to first found
        Pers_Def.Person_List_Mng.Move_To (List, Pers_Def.Person_List_Mng.Next,
         Loc_Pos - 1, False);
        return;
      end if;
    end loop;


  end Select_By_Activity;

  -- The Insert call affects the order and current position in list
  -- Current pos in list becomes this person's one
  -- The person's pid is not significant in the In value,
  --  and set in the Out value
  -- It may raise Not_Sole if the (Name, Activity) already exists in list

  -- Insert a new person in the list. (Its Name+Activity must be sole)
  procedure Insert (List : in out Pers_Def.Person_List;
                    Person : in out Pers_Def.Person_Rec) is
    List_Length : constant Natural
                := Pers_Def.Person_List_Mng.List_Length (List);
    Curr_Person : Pers_Def.Person_Rec;
    Pid_Array : array (Pers_Def.Pid_Range) of Boolean := (others => False);
    Found : Boolean;
  begin
    if List_Length /= 0 then
      -- Search if this (Name, Activity) is already in List
      -- Mark used Pids
      Pers_Def.Person_List_Mng.Rewind (List);
      for I in 1 .. List_Length loop
        if I /= List_Length then
          Pers_Def.Person_List_Mng.Read (List, Curr_Person,
           Pers_Def.Person_List_Mng.Next);
        else
          Pers_Def.Person_List_Mng.Read (List, Curr_Person,
           Pers_Def.Person_List_Mng.Current);
        end if;
        if Curr_Person.Name = Person.Name and then
           Curr_Person.Activity = Person.Activity then
          raise Not_Sole_Error;
        end if;
        Pid_Array (Curr_Person.Pid) := True;
      end loop;
      -- Look for first free Pid
      Found := False;
      for I in Pers_Def.Pid_Range loop
        if not Pid_Array(I) then
          Person.Pid :=  I;
          Found := True;
          exit;
        end if;
      end loop;
      if not Found then
        raise List_Full_Error;
      end if;

    else
      Person.Pid := Pers_Def.Pid_Range'First;
    end if;

    -- Insert person in list
    Pers_Def.Person_List_Mng.Insert (List, Person);
  end Insert;

end Pers_Mng;

