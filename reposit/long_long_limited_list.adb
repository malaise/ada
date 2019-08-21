with Ada.Unchecked_Deallocation;
with Mod_Utils;
package body Long_Long_Limited_List is
  use type Ll_Natural;

  Free_List : Link := null;

  -- Utilities
  function Is_Empty (List : List_Type) return Boolean is (List.First = null);

  procedure Check (List : in List_Type) is
  begin
    if Is_Empty (List) then
      raise Empty_List;
    end if;
  end Check;

  procedure Check_In (Pos : in Link) is
  begin
    if Pos = null then
      raise Not_In_List;
    end if;
  end Check_In;

  function Check_In (Pos : in Link) return Boolean is ( Pos /= null);

  procedure Check_Assigned (List : in List_Type) is
  begin
    if List.Assigned then
      raise List_Assigned;
    end if;
  end Check_Assigned;

  procedure Check_Cb (List : in List_Type) is
  begin
    if List.In_Cb then
      raise In_Callback;
    end if;
  end Check_Cb;

  -- Delete the full list
  procedure Deallocation_Of is new
    Ada. Unchecked_Deallocation (Object=>Cell, Name=>Link);
  procedure Delete_List (List : in out List_Type;
                         Deallocate : in Boolean := True) is
    Local : Link;
  begin
    Check_Cb (List);
    -- Don't delete the list if it is a copy
    if not List.Assigned then
      if Deallocate then
        -- Deallocate the list
        while List.First /= null loop
          Local := List.First;
          List.First := List.First.Next;
          Deallocation_Of (Local);
        end loop;
        -- Deallocate the free list
        while Free_List /= null loop
          Local := Free_List;
          Free_List := Free_List.Next;
          Deallocation_Of (Local);
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
    elsif Deallocate then
      -- If Assigned and Deallocate, we can still deallocate the free list
      while Free_List /= null loop
        Local := Free_List;
        Free_List := Free_List.Next;
        Deallocation_Of (Local);
      end loop;
    end if;
    List.Modified := True;
    List.Assigned := False;
    List.In_Cb := False;
    List.Pos_First := 0;
    List.Pos_Last := 0;
    List.Current := null;
    List.First := null;
    List.Last := null;
  end Delete_List;


  -- Next <-> Prev
  function Other_Way (Where : Direction) return Direction is
    (if Where = Next then Prev else Next);

  -- Check movement
  function Check_Move (List : in List_Type;
                       Where : Direction := Next;
                       Check_Empty : in Boolean := True) return Boolean is
  begin
    if Check_Empty then
      Check (List);
    elsif Is_Empty (List) then
      return False;
    end if;
    return (if Where = Next then List.Current.Next /= null
            else List.Current.Prev /= null);
  end Check_Move;


  -- Read the current item
  procedure Read (List : in out List_Type;
                  Item : out Element_Type;
                  Move : in Movement := Next) is
  begin
    Check_Cb (List);
    Check (List);
    Set (Item, List.Current.Value);
    if Move /= Current then
      Move_To (List, Move);
    end if;
  end Read;
  function  Read (List : in out List_Type;
                  Move : in Movement := Next) return Element_Type is
  begin
    return E : Element_Type do
      Read (List, E, Move);
    end return;
  end Read;

  procedure Read (List  : in out List_Type;
                  Item  : out Element_Type;
                  Move  : in Movement := Next;
                  Moved : out Boolean) is
  begin
    Check_Cb (List);
    Check (List);
    Set (Item, List.Current.Value);
    -- Modified is set by Move_To
    if Move = Current then
      Moved := True;
    elsif Check_Move (List, Move) then
      Move_To (List, Move);
      Moved := True;
    else
      Moved := False;
    end if;
  end Read;


  -- Modify the current item
  procedure Modify (List : in out List_Type;
                    Item : in Element_Type;
                    Move : in Movement := Next) is
  begin
    Check_Cb (List);
    Check_Assigned (List);
    Check (List);
    Set (List.Current.Value, Item);
    if Move /= Current then
      Move_To (List, Move);
    end if;
    List.Modified := True;
  end Modify;

  procedure Modify (List  : in out List_Type;
                    Item  : in Element_Type;
                    Move  : in Movement := Next;
                    Moved : out Boolean) is
  begin
    Check_Cb (List);
    Check_Assigned (List);
    Check (List);
    Set (List.Current.Value, Item);
    List.Modified := True;
    if Move = Current then
      Moved := True;
    elsif Check_Move (List, Move) then
      Move_To (List, Move);
      Moved := True;
    else
      Moved := False;
    end if;
  end Modify;


  -- Put a new element in the list
  procedure Insert (List  : in out List_Type;
                    Item  : in Element_Type;
                    Where : in Direction := Next) is
    New_Cell : Link;
  begin
    Check_Cb (List);
    Check_Assigned (List);
    List.Modified := True;
    if Free_List = null then
      -- Create the first element of the list
      New_Cell := new Cell;
    else
      New_Cell := Free_List;
      Free_List := Free_List.Next;
    end if;
    -- Fill new cell
    Set (New_Cell.Value, Item);
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
    -- Update neibours
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

  -- Suppress and possibly deallocate the current element from the list
  procedure Del (List       : in out List_Type;
                 Move       : in Direction;
                 Deallocate : in Boolean)  is
    Del_Cell : Link;
  begin
    Check_Cb (List);
    Check_Assigned (List);
    Check (List);

    if List.Pos_First = 1 and then List.Pos_Last = 1 then
      -- Last item of the list
      null;
    else
      -- Check movement
      if Move = Next then
        Check_In (List.Current.Next);
      elsif Move = Prev then
        Check_In (List.Current.Prev);
      end if;
    end if;

    List.Modified := True;
    -- Disconnect
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
    -- Move
    Del_Cell := List.Current;
    case Move is
      when Next =>
        List.Current := List.Current.Next;
        List.Pos_Last := List.Pos_Last - 1;
      when Prev =>
        List.Current := List.Current.Prev;
        List.Pos_First := List.Pos_First - 1;
    end case;
    -- Insert in free list or deallocate
    if Deallocate then
      Deallocation_Of (Del_Cell);
    else
      if Free_List /= null then
        Free_List.Prev := Del_Cell;
      end if;
      Del_Cell.Prev := null;
      Del_Cell.Next := Free_List;
      Free_List := Del_Cell;
    end if;
    -- Check the special case when list is empty
    --  (set pos_first and pos_last to 0)
    if List.Current = null then
      List.Pos_First := 0;
      List.Pos_Last := 0;
    end if;
  end Del;

  -- Suppress the current element from the list
  procedure Delete (List       : in out List_Type;
                    Move       : in Direction := Next) is
  begin
    Del (List, Move, False);
  end Delete;

  procedure Delete (List  : in out List_Type;
                    Move  : in Direction := Next;
                    Moved : out Boolean) is
  begin
    if Check_Move (List, Move) then
      Del (List, Move, False);
      Moved := True;
    else
      Del (List, Other_Way (Move), False);
      Moved := False;
    end if;
  end Delete;

  -- Suppress and deallocate the current element from the list
  procedure Deallocate (List : in out List_Type;
                        Move : in Direction := Next) is
  begin
    Del (List, Move, True);
  end Deallocate;

  procedure Deallocate (List  : in out List_Type;
                        Move  : in Direction := Next;
                        Moved : out Boolean) is
  begin
    if Check_Move (List, Move) then
      Del (List, Move, True);
      Moved := True;
    else
      Del (List, Other_Way (Move), True);
      Moved := False;
    end if;
  end Deallocate;


  -- Reads and deletes the current element
  procedure Get (List : in out List_Type;
                 Item : out Element_Type;
                 Move : in Direction := Next) is
  begin
    Read (List, Item, Current);
    Delete (List, Move);
    -- Modified flag changed by Delete
  end Get;
  function  Get (List : in out List_Type;
                 Move : in Direction := Next) return Element_Type is
  begin
    return E : Element_Type do
      Get (List, E, Move);
    end return;
  end Get;

  procedure Get (List  : in out List_Type;
                 Item  : out Element_Type;
                 Move  : in Direction := Next;
                 Moved : out Boolean) is
  begin
    if Check_Move (List, Move) then
      Get (List, Item, Move);
      Moved := True;
    else
      Get (List, Item, Other_Way (Move));
      Moved := False;
    end if;
  end Get;


  -- Changes current position
  procedure Move_To (List         : in out List_Type;
                     Where        : in Direction := Next;
                     Number       : in Ll_Natural := 1;
                     From_Current : in Boolean := True) is
    New_Pos                     : Link;
    New_Pos_First, New_Pos_Last : Ll_Natural;
  begin
    Check_Cb (List);
    Check (List);
    -- Start from current or first/last
    if From_Current then
      New_Pos := List.Current;
      New_Pos_First := List.Pos_First;
      New_Pos_Last := List.Pos_Last;
    else
      case Where is
        when Next =>
          if Mod_Utils.Dist (Number, List.Pos_First) > List.Pos_First / 2 then
            New_Pos := List.First;
            New_Pos_First := 1;
            New_Pos_Last := List_Length(List);
          elsif Number < List.Pos_First then
            -- Optim: Better move backwards from current
            Move_To (List, Prev, List.Pos_First - (Number + 1), True);
            return;
          else
            -- Optim: Better move forward from current
            Move_To (List, Next, (Number + 1) - List.Pos_First, True);
            return;
          end if;
        when Prev =>
          if Mod_Utils.Dist (Number, List.Pos_Last) > List.Pos_Last / 2 then
            New_Pos := List.Last;
            New_Pos_First := List_Length(List);
            New_Pos_Last := 1;
          elsif Number < List.Pos_Last then
            -- Optim: Better move forward from current
            Move_To (List, Next, List.Pos_Last - (Number + 1), True);
            return;
          else
            -- Optim: Better move backwards from current
            Move_To (List, Prev, (Number + 1) - List.Pos_First, True);
            return;
          end if;
      end case;
    end if;
    -- Move
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
    -- Realy move if no problem
    List.Current := New_Pos;
    List.Pos_First := New_Pos_First;
    List.Pos_Last := New_Pos_Last;
    List.Modified := True;
  end Move_To;

  -- Move at given position
  procedure Move_At (List     : in out List_Type;
                     Position : in Ll_Positive;
                     Where    : in Direction := Next) is
  begin
    Check(List);
    -- Optim when next/prev of current: Move relative
    if      (Where = Next and then Position = List.Pos_First + 1)
    or else (Where = Prev and then Position = List.Pos_Last  - 1) then
      -- Move 1 step forward
      Move_To (List, Next, +1, True);
    elsif   (Where = Next and then Position = List.Pos_First - 1)
    or else (Where = Prev and then Position = List.Pos_Last  + 1) then
      -- Move 1 step backwards
      Move_To (List, Prev, +1, True);
    else
      -- Move absolute
      Move_To (List, Where, Position - 1, False);
    end if;
  end Move_At;

  -- Move to beginning/end of list: Move_To (List, Where, 0, False);
  procedure Rewind (List        : in out List_Type;
                    Where       : in Direction := Next;
                    Check_Empty : in Boolean := True) is
  begin
    Check_Cb (List);
    if not Check_Empty and then Is_Empty (List) then
      return;
    end if;
    Check (List);
    -- Care here: List_Length reads Pos_First and Pos_Last!
    case Where is
      when Next =>
        List.Current := List.First;
        List.Pos_Last := List_Length(List);
        List.Pos_First := 1;
      when Prev =>
        List.Current := List.Last;
        List.Pos_First := List_Length(List);
        List.Pos_Last := 1;
    end case;
    List.Modified := True;
  end Rewind;


  -- Permute two elements knowing links to them
  --  (internal procedure for permute and sort)
  -- Does List.Current point to one of the permuted elements
  --  so does it need to be updated
  type Current_Status_List is (Is_Left, Is_Right, Is_None);
  procedure Permute (List : in out List_Type; Left, Right : in Link) is
    Tmp_Next, Tmp_Prev : Link;
    Current_Status : Current_Status_List;
  begin

    if Left = Right then
      return;
    end if;

    -- Check if current is one the swapped cells
    Current_Status := (if List.Current = Left then Is_Left
                       elsif List.Current = Right then Is_Right
                       else Is_None);

    Tmp_Prev := Left.Prev;
    Tmp_Next := Left.Next;
    if Left.Next /= Right and then Left.Prev /= Right then
      -- No adjacent cells
      -- Exchange neighbours links
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

      -- Exchange swapped cells links to neighbours
      Left.Prev := Right.Prev;
      Left.Next := Right.Next;
      Right.Prev := Tmp_Prev;
      Right.Next := Tmp_Next;
    elsif Left.Next = Right then
      -- Left just before right
      -- Exchange neighbours links
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

      -- Exchange swapped cells links to neighbours
      Left.Prev := Right;
      Left.Next := Right.Next;
      Right.Prev := Tmp_Prev;
      Right.Next := Left;
    elsif Left.Prev = Right then
      -- Left just after right
      -- Exchange neighbours links
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

      -- Exchange swapped cells links to neighbours
      Left.Prev := Right.Prev;
      Left.Next := Right;
      Right.Prev := Left;
      Right.Next := Tmp_Next;
    else
      -- Impossible
      raise Program_Error;
    end if;

    -- If necessary, update List.Current so that it still points
    --  consistently with Pos_First and Pos_Last
    if Current_Status = Is_Left then
      List.Current := Right;
    elsif Current_Status = Is_Right then
      List.Current := Left;
    end if;
    List.Modified := True;
  end Permute;

  -- Permutes 2 elements
  procedure Permute (List         : in out List_Type;
                     Number1      : in Ll_Natural;
                     Number2      : in Ll_Natural;
                     Where        : in Direction := Next;
                     From_Current : in Boolean   := False) is
    Current_Position : constant Ll_Positive
                     := Get_Position (List);
    Link1, Link2 : Link;
  begin
    Check_Assigned (List);
    -- Move to elements and store links to them
    Move_To (List, Where, Number1, From_Current);
    Link1 := List.Current;
    Move_To (List, Where, Number2, From_Current);
    Link2 := List.Current;

    -- Permute items
    Permute (List, Link1, Link2);

    -- Restore initial position
    Move_At (List, Current_Position);
    List.Modified := True;
  exception
    when Not_In_List =>
      -- Restore initial position
      Move_At (List, Current_Position);
      raise;
  end Permute;


  -- Returns the number of elements in the list (0 if empty)
  function List_Length (List : List_Type) return Ll_Natural is
    (if Is_Empty (List) then 0 else List.Pos_First + List.Pos_Last - 1);


  -- Get position from first or last item in list
  function Get_Position (List : List_Type;
                         From : Reference := From_First)
           return Ll_Positive is
  begin
    Check (List);
    return (case From is
              when From_First => List.Pos_First,
              when From_Last => List.Pos_Last);
  end Get_Position;


  -- Modification stuff
  procedure Set_Modified (List : in out List_Type) is
  begin
    List.Modified := False;
  end Set_Modified;

  function Is_Modified (List : List_Type) return Boolean is (List.Modified);

  procedure Modification_Ack (List : in out List_Type)is
  begin
    List.Modified := False;
  end Modification_Ack;


  -- Copy the Val list to To list
  procedure Unchecked_Assign (To : in out List_Type; Val : in List_Type) is
  begin
    Delete_List (To);
    To.Assigned := True;
    To.Modified := True;
    To.In_Cb := False;
    To.Pos_First := Val.Pos_First;
    To.Pos_Last := Val.Pos_Last;
    To.Current := Val.Current;
    To.First := Val.First;
    To.Last := Val.Last;
  end Unchecked_Assign;

  -- Completely insert a copy of Val list (data) after or before current
  procedure Insert_Copy (To    : in out List_Type;
                         Val   : in List_Type;
                         Where : in Direction := Next) is
    Lval : List_Type;
  begin
    Check_Assigned (To);
    if Is_Empty (Val) then
      -- Nothing if Val is empty
      return;
    end if;
    Unchecked_Assign (Lval, Val);
    Rewind (Lval, Where);
    loop
      -- Copy Elt
      Insert (To, Lval.Access_Current.all, Where);
      -- End of Lval?
      exit when not Check_Move (Lval, Where);
      Move_To (Lval, Where);
    end loop;
  end Insert_Copy;


  -- Access to current element
  function Access_Current (List : List_Type;
           return not null access Element_Type is
  begin
    return Cell_Access_Current (List).Value'Unrestricted_Access;
  end Access_Current;

  -- Search the element that is at the provided access (move to it)
  function Search_Access (List      : in out List_Type;
                          Criteria  : not null access Element_Type)
           return Boolean is
    New_Pos : Link;
    New_Pos_First : Ll_Natural;
    Found : Boolean;
  begin
    Check_Cb (List);
    Check (List);
    -- Forbid calls from application
    List.In_Cb := True;
    -- Optim: check if current is the one
    if List.Current.Value'Unrestricted_Access = Criteria then
      New_Pos := List.Current;
      New_Pos_First := List.Pos_First;
      Found := True;
    else
      -- Search from first
      New_Pos := List.First;
      New_Pos_First := 1;
      -- Search until end of list or found
      Found := False;
      while New_Pos /= null loop
        if New_Pos.Value'Unrestricted_Access = Criteria then
          -- Found
          Found := True;
          exit;
        end if;
        -- Next cell
        New_Pos := New_Pos.Next;
        New_Pos_First := New_Pos_First + 1;
      end loop;
    end if;
    if Found then
      -- Move to item found
      List.Current := New_Pos;
      List.Pos_Last := List.Pos_Last + List.Pos_First - New_Pos_First;
      List.Pos_First := New_Pos_First;
      List.Modified := True;
    end if;
    List.In_Cb := False;
    return Found;
  exception
    when others =>
      List.In_Cb := False;
      raise;
  end Search_Access;


  -- Access to the cell (that stores data) for deleting it without searching
  -- Get direct access to the current Cell (that stores the current Element)
  -- CARE: As soon as the element is deleted, the access becomes invalid
  --  and using it will lead to unpredictable results
  function Cell_Access_Current (List : List_Type;
           return not null access Cell is
  begin
    Check (List);
    return List.Current;
  end Cell_Access_Current;

  -- Delete current element and rewind the list
  -- Rewinding is necessary because the impact of this deletion on current
  --  position is unknown
  procedure Delete_Current_Rewind (List     : in out List_Type;
                                   Cell_Acc : not null access Cell;
                                   Where    : in Direction := Next) is
    Len : Ll_Natural;
  begin
    Check_Cb (List);
    Check_Assigned (List);
    Check (List);
    List.Modified := True;
    -- Disconnect Cell_Acc
    if Cell_Acc.Next /= null then
      Cell_Acc.Next.Prev := Cell_Acc.Prev;
    else
      List.Last := Cell_Acc.Prev;
    end if;
    if Cell_Acc.Prev /= null then
      Cell_Acc.Prev.Next := Cell_Acc.Next;
    else
      List.First := Cell_Acc.Next;
    end if;
    -- Move Cell_Acc into free list
    if Free_List /= null then
      Free_List.Prev := Link (Cell_Acc);
    end if;
    Cell_Acc.Prev := null;
    Cell_Acc.Next := Free_List;
    Free_List := Link (Cell_Acc);
    -- Rewind: Update Pos_xxx and Current
    Len := List.Pos_First + List.Pos_Last - 2;
    if Len = 0 then
      -- Check the special case when list becomes empty
      List.Pos_First := 0;
      List.Pos_Last := 0;
      List.Current := null;
    else
      case Where is
        when Next =>
          List.Current := List.First;
          List.Pos_First := 1;
          List.Pos_Last := Len;
        when Prev =>
          List.Current := List.Last;
          List.Pos_First := Len;
          List.Pos_Last := 1;
      end case;
    end if;
  end Delete_Current_Rewind;


  -- Searches
  -----------

  -- Generic search with a Criteria not of Item_Type
  function Search_Criteria (List      : in out List_Type;
                            Criteria  : in Criteria_Type;
                            Where     : in Direction := Next;
                            Occurence : in Ll_Positive := 1;
                            From      : in Search_Kind_List) return Boolean is
    New_Pos                     : Link;
    New_Pos_First, New_Pos_Last : Ll_Natural;
    Found : Boolean;

    function Next_Pos return Boolean is
    begin
      if not Check_In(New_Pos.Next) then
        return False;
      end if;
      New_Pos := New_Pos.Next;
      New_Pos_First := New_Pos_First + 1;
      New_Pos_Last := New_Pos_Last - 1;
      return True;
    end Next_Pos;

    function Prev_Pos return Boolean is
    begin
      if not Check_In(New_Pos.Prev) then
        return False;
      end if;
      New_Pos := New_Pos.Prev;
      New_Pos_First := New_Pos_First - 1;
      New_Pos_Last := New_Pos_Last + 1;
      return True;
    end Prev_Pos;

  begin
    Check_Cb (List);
    -- Default
    Found := False;
    if Is_Empty (List) then
      return Found;
    end if;
    -- Forbid calls from application
    List.In_Cb := True;

    -- Try Current_Absolute
    if From = Current_Absolute then
      if Occurence /= 1 then
        raise Constraint_Error;
      end if;
      if Match (List.Current.Value, Criteria) then
        -- Optim worked, done
        List.In_Cb := False;
        return True;
      end if;
    end if;

    -- Start from
    if From = From_Current or else From = Skip_Current then
      -- From current (keep or skip it)
      New_Pos := List.Current;
      New_Pos_First := List.Pos_First;
      New_Pos_Last := List.Pos_Last;
    else
      -- Absolute
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
    -- Move
    case Where is
      when Next =>
        for I in 1 .. Occurence loop
          Found := False;
          if I /= 1 or else From = Skip_Current then
            exit when not Next_Pos;
          end if;
          loop
            Found := Match (New_Pos.Value, Criteria);
            exit when Found;
            exit when not Next_Pos;
          end loop;
        end loop;
      when Prev =>
        for I in 1 .. Occurence loop
          Found := False;
          if I /= 1 or else From = Skip_Current then
            exit when not Prev_Pos;
          end if;
          loop
            Found := Match (New_Pos.Value, Criteria);
            exit when Found;
            exit when not Prev_Pos;
          end loop;
        end loop;
    end case;

    if Found then
      -- No change if not found
      List.Current := New_Pos;
      List.Pos_First := New_Pos_First;
      List.Pos_Last := New_Pos_Last;
      List.Modified := True;
    end if;
    List.In_Cb := False;
    return Found;
  exception
    when others =>
      List.In_Cb := False;
      raise;
  end Search_Criteria;

  -- Generic search with Criteria of Element_Type
  function Search (List      : in out List_Type;
                   Criteria  : in Element_Type;
                   Where     : in Direction := Next;
                   Occurence : in Ll_Positive := 1;
                   From      : in Search_Kind_List) return Boolean is

    function Item_Search is new Search_Criteria (Element_Type, Match);
  begin
    return Item_Search (List, Criteria, Where, Occurence, From);
  end Search;


  -- Search with Match passed by access
  function Search_Match (List      : in out List_Type;
                         Match     : not null access
                   function (Current, Criteria : Element_Type) return Boolean;
                         Criteria  : in Element_Type;
                         Where     : in Direction := Next;
                         Occurence : in Ll_Positive := 1;
                         From      : in Search_Kind_List) return Boolean is
    function Search_Element is new Search (Match.all);
  begin
    return Search_Element (List, Criteria, Where, Occurence, From);
  end Search_Match;

  -- Search -> exception
  procedure Search_Raise (List      : in out List_Type;
                          Criteria  : in Element_Type;
                          Where     : in Direction := Next;
                          Occurence : in Ll_Positive := 1;
                          From      : in Search_Kind_List) is
    function Loc_Match (Current, Criteria : Element_Type) return Boolean is
      (Match (Current, Criteria));
  begin
    if not Search_Match (List, Loc_Match'Unrestricted_Access,
                         Criteria, Where, Occurence, From) then
      raise Not_In_List;
    end if;
  end Search_Raise;

  -- Iterate
  procedure Iterate (List      : in out List_Type;
                     Match     : access
                function (Current, Criteria : Element_Type) return Boolean;
                     Criteria  : in Element_Type;
                     Where     : in Direction := Next;
                     From      : in Search_Kind_List;
                     Iteration : access
    procedure (Current : in Element_Type;
               Go_On   : in out Boolean)) is
    Found : Boolean;
    Go_On : Boolean;
  begin
    Check_Cb (List);
    if List.Is_Empty then
      return;
    end if;
    -- By default
    Go_On := True;
    -- Search first matching item
    if Match /= null then
      Found := Search_Match (List, Match, Criteria, Where, 1, From);
    else
      Rewind (List, Where => Where);
      Found := True;
    end if;
    while Found loop
      if Iteration /= null then
        -- Forbid calls from application
        List.In_Cb := True;
        -- Call cb
        Iteration (List.Current.Value, Go_On);
        List.In_Cb := False;
        List.Modified := True;
        -- Cb wants to stop processing now
        exit when not Go_On;
      end if;
      -- Search next matching item
      if Match /= null then
        Found := Search_Match (List, Match, Criteria, Where, 1, Skip_Current);
      else
        if Check_Move (List, Where) then
          Move_To (List, Where);
          Found := True;
        else
          Found := False;
        end if;
      end if;
    end loop;
  exception
    when others =>
      List.In_Cb := False;
      raise;
  end Iterate;


  -- Sort
  procedure Sort (List : in out List_Type) is
    Last : constant Ll_Natural := List_Length (List);
  begin
    Check_Cb (List);
    Check_Assigned (List);
    if Last <= 1 then
      -- No or 1 element. No sort.
      return;
    end if;

    declare

      -- Recursive procedure which sorts a slice of the list
      procedure Quick (Left, Right : in Ll_Positive) is
        -- Middle of the slice
        I_Frontier : constant Ll_Positive := (Left + Right) / 2;
        L_Frontier : Link;
        -- Indexes in both halfs of the slice
        I_Left, I_Right : Ll_Positive;
        L_Left, L_Right : Link;
     begin
        I_Left := Left;
        I_Right := Right;
        -- Set link to frontier
        Move_At (List, I_Frontier);
        L_Frontier := List.Current;

        loop

          -- First element at left of slice and not positioned ok
          --  regarding the frontier
          Move_At (List, I_Left);
          while Less_Than (List.Current.Value, L_Frontier.Value) loop
            Move_To (List, Next, 1);
          end loop;
          L_Left := List.Current;
          I_Left := Get_Position (List);

          -- Last  element a right of slice and not positioned ok
          --  regarding the frontier
          Move_At (List, I_Right);
          while Less_Than (L_Frontier.Value, List.Current.Value) loop
            Move_To (List, Prev, 1);
          end loop;
          L_Right := List.Current;
          I_Right := Get_Position (List);

          -- Exchange and go to next elements if not both in frontier
          if I_Left < I_Right then
            Permute (List, L_Left, L_Right);
            I_Left  := I_Left  + 1;
            I_Right := I_Right - 1;
          elsif I_Left = I_Right then
            -- Go to next elements if not crossed
            if I_Left /= Right then
              I_Left  := I_Left  + 1;
            end if;
            if I_Right /= Left then
              I_Right := I_Right - 1;
            end if;
          end if;

          -- Leave if crossed now
          exit when I_Left > I_Right or else
                   (I_Left = Right and then I_Right = Left);
        end loop;

        -- Sort both new slices
        if Left   < I_Right then Quick(Left,   I_Right); end if;
        if I_Left < Right   then Quick(I_Left, Right);   end if;
      end Quick;

    begin
      Quick (1, Last);
    end;
    -- Move to first item
    Rewind (List, Next);
    List.Modified := True;
  exception
    when others => raise Sort_Error;
  end Sort;

  overriding procedure Finalize (List : in out List_Type) is
  begin
    Delete_List (List, Deallocate => True);
  end Finalize;

end Long_Long_Limited_List;

