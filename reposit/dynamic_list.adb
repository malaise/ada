with Unchecked_Deallocation;
package body Dynamic_List is

  type Element_Array is array (Positive range <>) of Element_Type;

  Free_List : Link := Null;

  function Is_Empty (List : List_Type) return Boolean is
  begin
    return List.First = null;
  end Is_Empty;

  procedure Check (List : in List_Type) is
  begin
    if Is_Empty(List) then
      raise Empty_List;
    end if;
  end Check;

  procedure Check_In(Pos : in Link) is
  begin
    if Pos = null then
      raise Not_In_List;
    end if;
  end Check_In;

  -- delete the full list
  procedure Delete_List (List : in out List_Type;
                         Deallocate : in Boolean := True) is
    Local : Link;
    procedure Deallocation_Of is new
     Unchecked_Deallocation(Object=>Cell, Name=>Link);
  begin
    if Deallocate then
      -- deallocate the list
      while List.First /= null loop
        Local := List.First;
        List.First := List.First.Next;
        Deallocation_Of(Local);
      end loop;
      -- deallocate the free list
      while Free_List /= null loop
        Local := Free_List;
        Free_List := Free_List.Next;
        Deallocation_Of(Local);
      end loop;
    else
      -- Insert the list in beginning of free list
      if List.First /= null then
        List.Last.Next := Free_List;
        if Free_List /= null then
          Free_List.Prev := List.Last;
        end if;
        Free_List := List.First;
      end if;
    end if;
    List := (Modified => True, Pos_First | Pos_Last => 0,
     Current => null, First => null, Last => null);
  end Delete_List;

  -- read the current item
  procedure Read (List : in out List_Type;
                  Item : out Element_Type;
                  Move : in Movement := Next) is
  begin
    Check(List);
    Item := List.Current.Value;
    if Move /= Current then
      Move_To (List, Move);
    end if;
    -- Modification done my MOVE_TO
  end Read;

  -- modify the current item
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next) is
  begin
    Check(List);
    List.Current.Value := Item;
    if Move /= Current then
      Move_To (List, Move);
    end if;
    List.Modified := True;
  end Modify;

  -- put a new element in the list
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next) is
    New_Cell : Link;
  begin
    List.Modified := True;
    if Free_List = null then
      -- create the first element of the list
      New_Cell := new Cell;
    else
      New_Cell := Free_List;
      Free_List := Free_List.Next;
    end if;
    -- fill new cell
    New_Cell.Value := Item;
    if Is_Empty (List) then
      New_Cell.Next := null;
      New_Cell.Prev := null;
      List.Pos_First := 1;
      List.Pos_Last := 1;
    else
      case Where is
        when Next =>
          New_Cell.Next := List.Current.Next;
          New_Cell.Prev := List.Current;
          List.Pos_First := List.Pos_First + 1;
        when Prev =>
          New_Cell.Next := List.Current;
          New_Cell.Prev := List.Current.Prev;
          List.Pos_Last := List.Pos_Last + 1;
      end case;
    end if;
    -- update neibours
    List.Current := New_Cell;
    if New_Cell.Prev /= null then
      New_Cell.Prev.Next := New_Cell;
    else
      List.First := New_Cell;
    end if;
    if New_Cell.Next /= null then
      New_Cell.Next.Prev := New_Cell;
    else
      List.Last := New_Cell;
    end if;

  exception
    when Storage_Error =>
      raise Full_List;
  end Insert;

  -- suppress the current element from the list
  procedure Delete (List : in out List_Type; Move : in Direction := Next) is
    Del_Cell : Link;
  begin
    Check(List);

    if List.Pos_First = 1 and then List.Pos_Last = 1 then
      -- Last item of the list
      null;
    else
      -- check movement
      if Move = Next then
        Check_In(List.Current.Next);
      elsif Move = Prev then
        Check_In (List.Current.Prev);
      end if;
    end if;

    List.Modified := True;
    -- disconnect
    if List.Current.Next /= null then
      List.Current.Next.Prev := List.Current.Prev;
    else
      List.Last := List.Current.Prev;
    end if;
    if List.Current.Prev /= null then
      List.Current.Prev.Next := List.Current.Next;
    else
      List.First := List.Current.Next;
    end if;
    -- move
    Del_Cell := List.Current;
    case Move is
      when Next =>
        List.Current := List.Current.Next;
        List.Pos_Last := List.Pos_Last - 1;
      when Prev =>
        List.Current := List.Current.Prev;
        List.Pos_First := List.Pos_First - 1;
    end case;
    -- insert in free list
    if Free_List /= null then
      Free_List.Prev := Del_Cell;
    end if;
    Del_Cell.Prev := null;
    Del_Cell.Next := Free_List;
    Free_List := Del_Cell;
    -- check the special case when list is empty
    --  (set pos_first AND pos_last to 0)
    if List.Current = null then
      List.Pos_First := 0;
      List.Pos_Last := 0;
    end if;
  end Delete;


  -- reads and deletes the current element
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next) is
  begin
    Read(List, Item, Current);
    Delete(List, Move);
    -- Modified flag changed by DELETE
  end Get;

  -- changes current position
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Natural := 1;
                     From_Current : in Boolean := True) is
    New_Pos                     : Link;
    New_Pos_First, New_Pos_Last : Natural;
  begin
    Check(List);
    -- start from
    if From_Current then
      New_Pos := List.Current;
      New_Pos_First := List.Pos_First;
      New_Pos_Last := List.Pos_Last;
    else
      case Where is
        when Next =>
          New_Pos := List.First;
          New_Pos_First := 1;
          New_Pos_Last := List_Length(List);
        when Prev =>
          New_Pos := List.Last;
          New_Pos_First := List_Length(List);
          New_Pos_Last := 1;
      end case;
    end if;
    -- move
    case Where is
      when Next =>
        for I in 1 .. Number loop
          Check_In (New_Pos.Next);
          New_Pos := New_Pos.Next;
          New_Pos_First := New_Pos_First + 1;
          New_Pos_Last := New_Pos_Last - 1;
        end loop;
      when Prev =>
        for I in 1 .. Number loop
          Check_In (New_Pos.Prev);
          New_Pos := New_Pos.Prev;
          New_Pos_First := New_Pos_First - 1;
          New_Pos_Last := New_Pos_Last + 1;
        end loop;
    end case;
    -- realy move if no problem
    List.Current := New_Pos;
    List.Pos_First := New_Pos_First;
    List.Pos_Last := New_Pos_Last;
    List.Modified := True;
  end Move_To;

  -- permute two elements knowing links to them
  -- (internal procedure for permute and sort)
  procedure Permute (List : in out List_Type; Left, Right : in Link) is
    Tmp_Next, Tmp_Prev : Link;
  begin

    Tmp_Prev := Left.Prev;
    Tmp_Next := Left.Next;
    if Left.Next /= Right and then Left.Prev /= Right then
      -- no adjacent cells
      -- exchange neighbours links
      if Left.Prev /= null then
        Left.Prev.Next := Right;
      else
        List.First := Right;
      end if;
      if Left.Next /= null then
        Left.Next.Prev := Right;
      else
        List.Last := Right;
      end if;
      if Right.Prev /= null then
        Right.Prev.Next := Left;
      else
        List.First := Left;
      end if;
      if Right.Next /= null then
        Right.Next.Prev := Left;
      else
        List.Last := Left;
      end if;

      -- exchange swapped cells links to neighbours
      Left.Prev := Right.Prev;
      Left.Next := Right.Next;
      Right.Prev := Tmp_Prev;
      Right.Next := Tmp_Next;
    elsif Left.Next = Right then
      -- left just before right
      -- exchange neighbours links
      if Left.Prev /= null then
        Left.Prev.Next := Right;
      else
        List.First := Right;
      end if;
      if Right.Next /= null then
        Right.Next.Prev := Left;
      else
        List.Last := Left;
      end if;

      -- exchange swapped cells links to neighbours
      Left.Prev := Right;
      Left.Next := Right.Next;
      Right.Prev := Tmp_Prev;
      Right.Next := Left;
    elsif Left.Prev = Right then
      -- left just after right
      -- exchange neighbours links
      if Left.Next /= null then
        Left.Next.Prev := Right;
      else
        List.Last := Right;
      end if;
      if Right.Prev /= null then
        Right.Prev.Next := Left;
      else
        List.First := Left;
      end if;

      -- exchange swapped cells links to neighbours
      Left.Prev := Right.Prev;
      Left.Next := Right;
      Right.Prev := Left;
      Right.Next := Tmp_Next;
    else
      raise Program_Error;
    end if;
    List.Modified := True;
  end Permute;

  -- permutes 2 elements
  procedure Permute (List      : in out List_Type;
                     Number1      : in Natural;
                     Number2      : in Natural;
                     Where        : in Direction := Next;
                     From_Current : in Boolean   := False) is
    Current_Position : constant Positive := Get_Position (List);
    Link1, Link2 : Link;
  begin
    -- move to elements and store links to them
    Move_To (List, Where, Number1, From_Current);
    Link1 := List.Current;
    Move_To (List, Where, Number2, From_Current);
    Link2 := List.Current;

    -- permute items
    Permute (List, Link1, Link2);

    -- Restore initial position
    Move_To (List, Next, Current_Position - 1, From_Current => False);
    List.Modified := True;
  exception
    when Not_In_List =>
      -- Restore initial position
      Move_To (List, Next, Current_Position - 1, From_Current => False);
      raise;
  end Permute;

  -- returns the number of elements in the list (0 if empty)
  function List_Length (List : List_Type) return Natural is
  begin
    if Is_Empty(List) then
      return 0;
    else
      return List.Pos_First + List.Pos_Last - 1;
    end if;
  end List_Length;

  -- get position from first or last item in list
  function Get_Position (List : List_Type;
                         From : Reference := From_First) return Positive is
  begin
    Check(List);
    case From is
      when From_First =>
        return List.Pos_First;
      when From_Last =>
        return List.Pos_Last;
    end case;
  end Get_Position;

  function Is_Modified (List : List_Type) return Boolean is
  begin
    return List.Modified;
  end Is_Modified;

  procedure Modification_Ack (List : in out List_Type)is
  begin
    List.Modified := False;
  end Modification_Ack;

  -- Copy the Val list to To list
  procedure Assign (To : in out List_Type; Val : in List_Type) is
  begin
    To := Val;
    To.Modified := True;
  end Assign;

  -- Access to current element
  function Access_Current (List : List_Type) return Element_Access is
  begin
    if Is_Empty (List) then
      return null;
    end if;
    return List.Current.Value'Unrestricted_Access;
  end;

  procedure Search (List         : in out List_Type;
                    Item         : in Element_Type;
                    Where        : in Direction := Next;
                    Occurence    : in Positive := 1;
                    From_Current : in Boolean := True) is
    New_Pos                     : Link;
    New_Pos_First, New_Pos_Last : Natural;

    procedure Next_Pos is
    begin
      Check_In(New_Pos.Next);
      New_Pos := New_Pos.Next;
      New_Pos_First := New_Pos_First + 1;
      New_Pos_Last := New_Pos_Last - 1;
    end Next_Pos;

    procedure Prev_Pos is
    begin
      Check_In(New_Pos.Prev);
      New_Pos := New_Pos.Prev;
      New_Pos_First := New_Pos_First - 1;
      New_Pos_Last := New_Pos_Last + 1;
    end Prev_Pos;

  begin
    if Is_Empty (List) then
      raise Not_In_List;
    end if;
    -- start from
    if From_Current then
      New_Pos := List.Current;
      New_Pos_First := List.Pos_First;
      New_Pos_Last := List.Pos_Last;
    else
      case Where is
        when Next =>
          New_Pos := List.First;
          New_Pos_First := 1;
          New_Pos_Last := List_Length(List);
        when Prev =>
          New_Pos := List.Last;
          New_Pos_First := List_Length(List);
          New_Pos_Last := 1;
      end case;
    end if;
    -- move
    case Where is
      when Next =>
        for I in 1 .. Occurence loop
          if I /= 1 then
            Next_Pos;
          end if;
          loop
            exit when Equal(New_Pos.Value, Item);
            Next_Pos;
          end loop;
        end loop;
      when Prev =>
        for I in 1 .. Occurence loop
          if I /= 1 then
            Prev_Pos;
          end if;
          loop
            exit when Equal(New_Pos.Value, Item);
            Prev_Pos;
          end loop;
        end loop;
    end case;

    List.Current := New_Pos;
    List.Pos_First := New_Pos_First;
    List.Pos_Last := New_Pos_Last;
    List.Modified := True;
  end Search;


  procedure Sort (List : in out List_Type) is
    Last : constant Natural := List_Length (List);
  begin
    if Last <= 1 then
      -- No or 1 element. No sort.
      return;
    end if;

    declare

      -- recursive procedure which sorts a slice of the list
      procedure Quick (Left, Right : in Positive) is
        -- middle of the slice
        I_Frontier : constant Positive := (Left + Right) / 2;
        L_Frontier : Link;
        -- indexes in both halfs of the slice
        I_Left, I_Right : Positive;
        L_Left, L_Right : Link;
     begin
        I_Left := Left;
        I_Right := Right;
        -- set link to frontier
        Move_To (List, Next, I_Frontier-1, False);
        L_Frontier := List.Current;

        loop

          -- first element at left of slice and not positioned ok
          --  regarding the frontier
          Move_To (List, Next, I_Left-1, False);
          while Less_Than (List.Current.Value, L_Frontier.Value) loop
            Move_To (List, Next, 1, True);
          end loop;
          L_Left := List.Current;
          I_Left := Get_Position (List);

          -- last  element a right of slice and not positioned ok
          --  regarding the frontier
          Move_To (List, Next, I_Right-1, False);
          while Less_Than (L_Frontier.Value, List.Current.Value) loop
            Move_To (List, Prev, 1, True);
          end loop;
          L_Right := List.Current;
          I_Right := Get_Position (List);

          -- exchange and go to next elements if not both in frontier
          if I_Left < I_Right then
            Permute (List, L_Left, L_Right);
            I_Left  := I_Left  + 1;
            I_Right := I_Right - 1;
          elsif I_Left = I_Right then
            -- go to next elements if not crossed
            if I_Left /= Right then
              I_Left  := I_Left  + 1;
            end if;
            if I_Right /= Left then
              I_Right := I_Right - 1;
            end if;
          end if;

          -- leave if crossed now
          exit when I_Left > I_Right or else
                   (I_Left = Right and then I_Right = Left);
        end loop;

        -- sort both new slices
        if Left   < I_Right then Quick(Left,   I_Right); end if;
        if I_Left < Right   then Quick(I_Left, Right);   end if;
      end Quick;

    begin
      Quick (1, Last);
    end;
    -- move to first item
    Move_To (List, Next, 0, False);
    List.Modified := True;
  end Sort;

end Dynamic_List;
