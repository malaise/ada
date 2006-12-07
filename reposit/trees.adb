with Dyn_Data;
package body Trees is


  -- A tree starts with a root (first branch)
  -- Each branch / cell has
  --  a data (Element_Type)
  --  some children (0 to N)
  --  some brothers (0 to N, root has no brother)
  --  one father (except root).
  package body Tree is

    -- Check The_Tree is not empty
    procedure Check_Empty (The_Tree : in Tree_Type) is
    begin
      -- No empty tree
      if The_Tree.Curr = null then
        raise No_Cell;
      end if;
    end Check_Empty;

    ----------------
    -- Insertions --
    ----------------
    -- All may raise Too_Many_Children if amount of children
    --  is Natural'Last

    -- Dynamic data management
    package Cell_Dyn is new Dyn_Data (Cell_Rec, Cell_Access);
    package Data_Dyn is new Dyn_Data (Element_Type, Element_Access);

    -- Allocate a new Cell storing Element
    function Allocate (Element  : Element_Type) return Cell_Access is
      Cell_Acc : Cell_Access;
    begin
      -- Create cell and store data
      Cell_Acc := Cell_Dyn.Allocate;
      Cell_Acc.Data := Data_Dyn.Allocate (Element);
      return Cell_Acc;
    end Allocate;


    -- Link my brothers to me
    procedure Link_Brothers (Me : in Cell_Access) is
    begin
      if Me.Brothers(Young) /= null then
        Me.Brothers(Young).Brothers(Old) := Me;
      end if;
      if Me.Brothers(Old) /= null then
        Me.Brothers(Old).Brothers(Young) := Me;
      end if;
    end Link_Brothers;

    -- Insert a father of current position (root if tree is empty)
    -- Move to it
    procedure Insert_Father (The_Tree : in out Tree_Type;
                             Element  : in Element_Type) is
      Cell_Acc, Prev_Curr : Cell_Access;
    begin
      -- Save current
      Prev_Curr := The_Tree.Curr;

      -- Create cell and move to it
      Cell_Acc := Allocate (Element);
      The_Tree.Curr := Cell_Acc;

      if Prev_Curr = null then
        -- Tree was empty
        The_Tree.Root := Cell_Acc;
        return;
      end if;

      -- Insert new cell
      Cell_Acc.Nb_Children := 1;
      Cell_Acc.Children(Old)  := Prev_Curr;
      Cell_Acc.Children(Young) := Prev_Curr;
      Cell_Acc.Father := Prev_Curr.Father;

      -- Link Root or previous father to new cell
      if Prev_Curr.Father = null then
        -- Inserting father of root (as root)
        The_Tree.Root := Cell_Acc;
      else
        if Prev_Curr.Brothers(Old) = null then
          -- Inserting instead of oldest, link father to it
          Prev_Curr.Father.Children(Old) := Cell_Acc;
        end if;
        if Prev_Curr.Brothers(Young) = null then
          -- Inserting instead of youngest, link father to it
          Prev_Curr.Father.Children(Young) := Cell_Acc;
        end if;
      end if;

      -- Inherit brothers of previous and link brothers to new cell
      Cell_Acc.Brothers := Prev_Curr.Brothers;
      Link_Brothers (Cell_Acc);

      -- Relink previous current
      Prev_Curr.Father := Cell_Acc;
      Prev_Curr.Brothers := (others => null);

    end Insert_Father;


    -- Insert a child of current position
    -- Append or prepend to the list of children
    -- Move to it
    -- May raise No_Cell if The_Tree is empty
    procedure Insert_Child (The_Tree : in out Tree_Type;
                            Element  : in Element_Type;
                            Eldest   : in Boolean := True) is
      Cell_Acc : Cell_Access;
      Child, Brother : Order;
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      -- Check number of children
      if The_Tree.Curr.Nb_Children = Child_Range'Last then
        raise Too_Many_Children;
      end if;

      -- Create
      Cell_Acc := Allocate (Element);

      -- Insert and move
      if Eldest then
        Child := Old;
        Brother := Young;
      else
        Child := Young;
        Brother := Old;
      end if;
      Cell_Acc.Father := The_Tree.Curr;
      The_Tree.Curr := Cell_Acc;
      Cell_Acc.Brothers(Brother) := Cell_Acc.Father.Children(Child);
      Cell_Acc.Father.Children(Child) := Cell_Acc;
      if Cell_Acc.Father.Nb_Children = 0 then
        Cell_Acc.Father.Children(Brother) := Cell_Acc;
      end if;
      Link_Brothers (Cell_Acc);

      -- Increment number of children
      Cell_Acc.Father.Nb_Children := Cell_Acc.Father.Nb_Children + 1;

    end Insert_Child;

    -- Insert a brother of current position
    -- Append or prepend to the list of brothers
    -- Move to it
    -- May raise No_Cell if The_Tree is empty or current is root
    procedure Insert_Brother (The_Tree : in out Tree_Type;
                              Element  : in Element_Type;
                              Elder   : in Boolean := True) is
      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      -- No root
      if The_Tree.Curr = The_Tree.Root then
        raise Is_Root;
      end if;

      -- Check number of children of father
      if The_Tree.Curr.Father.Nb_Children = Child_Range'Last then
        raise Too_Many_Children;
      end if;
      The_Tree.Curr.Father.Nb_Children := The_Tree.Curr.Father.Nb_Children + 1;

      -- Create
      Cell_Acc := Allocate (Element);

      -- Insert and move
      Cell_Acc.Father := The_Tree.Curr.Father;
      Cell_Acc.Brothers := The_Tree.Curr.Brothers;
      if Elder then
        if Cell_Acc.Brothers(Old) = null then
          -- Inserting eldest
          Cell_Acc.Father.Children(Old) := Cell_Acc;
        end if;
        Cell_Acc.Brothers(Young) := The_Tree.Curr;
      else
        if Cell_Acc.Brothers(Young) = null then
          -- Inserting youngest
          Cell_Acc.Father.Children(Young) := Cell_Acc;
        end if;
        Cell_Acc.Brothers(Old) := The_Tree.Curr;
      end if;
      Link_Brothers (Cell_Acc);
      The_Tree.Curr := Cell_Acc;

    end Insert_Brother;


    ---------------
    -- Deletions --
    ---------------
    -- Delete current cell if it has no children, moving to father
    -- May raise No_Cell if The_Tree is empty
    -- May raise Has_Children if current cell has children
    procedure Delete_Current (The_Tree : in out Tree_Type) is
      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      -- Check no children
      Cell_Acc := The_Tree.Curr;
      if Cell_Acc.Nb_Children /= 0 then
        raise Has_Children;
      end if;

      -- Clean saved if current
      if The_Tree.Save = The_Tree.Curr then
        The_Tree.Save := null;
      end if;

      -- Free data
      Data_Dyn.Free (Cell_Acc.Data);

      -- Check for root (no father)
      if Cell_Acc.Father = null then
        Cell_Dyn.Free (The_Tree.Curr);
        return;
      end if;

      -- Relink father and brothers
      if Cell_Acc.Brothers(Old) = null then
        -- deleting oldest
        Cell_Acc.Father.Children(Old) := Cell_Acc.Brothers(Young);
      else
        Cell_Acc.Brothers(Old).Brothers(Young) := Cell_Acc.Brothers(Young);
      end if;
      if Cell_Acc.Brothers(Young) = null then
        -- deleting oldest
        Cell_Acc.Father.Children(Young) := Cell_Acc.Brothers(Old);
      else
        Cell_Acc.Brothers(Young).Brothers(Old) := Cell_Acc.Brothers(Old);
      end if;

      -- Update father and move to it
      Cell_Acc.Father.Nb_Children := Cell_Acc.Father.Nb_Children - 1;
      The_Tree.Curr := Cell_Acc.Father;

      -- Free cell
      Cell_Dyn.Free (Cell_Acc);
    end Delete_Current;

    -- Clean children of current cell, tree is not updated
    procedure Clean_Children (The_Tree : in out Tree_Type;
                              Me : in Cell_Access);

    -- Clean a cell recursively, tree is not updated
    procedure Clean_Me (The_Tree : in out Tree_Type; Me : in out Cell_Access) is
    begin
      -- Clean my children
      Clean_Children (The_Tree, Me);

      -- Clean saved if current
      if The_Tree.Save = Me then
        The_Tree.Save := null;
      end if;
      Data_Dyn.Free (Me.Data);
      Cell_Dyn.Free (Me);
    end Clean_Me;

    procedure Clean_Children (The_Tree : in out Tree_Type;
                              Me : in Cell_Access) is
      Next : Cell_Access;
    begin
      Next := Me.Children(Old);
      while Next /= null loop
        Clean_Me(The_Tree, Next);
      end loop;
    end Clean_Children;

    -- Clear the whole sub-tree
    procedure Delete_Tree (The_Tree : in out Tree_Type;
                     Deallocate : in Boolean := True) is
    begin
      -- Check for empty tree
      if The_Tree.Curr /= null then
        -- Clean the children of current
        Clean_Children (The_Tree, The_Tree.Curr);
        -- Update current
        The_Tree.Curr.Children := (others => null);
        The_Tree.Curr.Nb_Children := 0;
        -- Remove current
        Delete_Current(The_Tree);
      end if;

      -- Deallocate cells and data if requested
      if Deallocate then
        Data_Dyn.Clear;
        Cell_Dyn.Clear;
      end if;
    end Delete_Tree;


    ------------------
    -- Save position --
    ------------------
    procedure Save_Position (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      The_Tree.Save := The_Tree.Curr;
    end Save_Position;


    --------------------
    -- Read / Replace --
    --------------------
    -- All may raise No_Cell if The_Tree is empty

    -- Read current element
    procedure Read (The_Tree : in Tree_Type;
                    Element  : out Element_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      Element := The_Tree.Curr.Data.all;
    end Read;

    function Read (The_Tree : Tree_Type) return Element_Type is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      return The_Tree.Curr.Data.all;

    end Read;

    -- Replace current element
    procedure Replace (The_Tree : in Tree_Type;
                       Element  : in Element_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      The_Tree.Curr.Data.all := Element;
    end Replace;

    -- Replace current element, returning previous
    procedure Swap (The_Tree : in Tree_Type;
                    Element  : in out Element_Type) is
      Tmp : Element_Type;
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      Tmp := The_Tree.Curr.Data.all;
      The_Tree.Curr.Data.all := Element;
      Element := Tmp;
    end Swap;

    -- Move saved pos and its sub tree at current position, return current
    procedure Swap_Saved (The_Tree : in Tree_Type) is

      -- Copy Children and data
      procedure Copy (Dst : in out Cell_Rec; Src : in Cell_Rec) is
      begin
        Dst.Children := Src.Children;
        Dst.Nb_Children := Src.Nb_Children;
        Dst.Data := Src.Data;
      end Copy;

      Tmp : Cell_Rec;
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      -- Check a pos is saved
      if The_Tree.Save = null then
        raise No_Cell;
      end if;

      -- Swap data and children between current and saved
      Copy (Tmp, The_Tree.Curr.all);
      Copy (The_Tree.Curr.all, The_Tree.Save.all);
      Copy (The_Tree.Save.all, Tmp);

    end Swap_Saved;

    -------------
    -- Look up --
    -------------
    -- Has current cell a father (alway true except for root)
    function Has_Father (The_Tree : Tree_Type) return Boolean is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      return The_Tree.Curr.Father /= null;
    end Has_Father;

    -- Has current cell and elder/younger brother
    function Has_Brother (The_Tree : Tree_Type;
                          Elder    : Boolean := True) return Boolean is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      if Elder then
        return The_Tree.Curr.Brothers(Old) /= null;
      else
        return The_Tree.Curr.Brothers(Young) /= null;
      end if;
    end Has_Brother;

    -- How many children has current cell
    function Children_Number (The_Tree : Tree_Type) return Child_Range is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      return The_Tree.Curr.Nb_Children;
    end Children_Number;


    ----------
    -- Move --
    ----------
    -- Move to root
    procedure Move_Root (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      The_Tree.Curr := The_Tree.Root;
    end Move_Root;

    -- Move to father
    -- May raise No_Cell if no father (root)
    procedure Move_Father (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      if The_Tree.Curr.Father = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Curr.Father;
    end Move_Father;

    -- Move to oldest/youngest child
    -- May raise No_Cell if no child
    procedure Move_Child (The_Tree : in out Tree_Type;
                          Elder    : in Boolean := True) is
      Child : Order;
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      if Elder then
        Child := Old;
      else
        Child := Young;
      end if;

      if The_Tree.Curr.Children(Child) = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Curr.Children(Child);
    end Move_Child;


    -- Move to older/younger brother
    -- May raise No_Cell if no such brother
    procedure Move_Brother (The_Tree : in out Tree_Type;
                            Elder    : in Boolean := True) is
      Brother : Order;
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      if Elder then
        Brother := Old;
      else
        Brother := Young;
      end if;

      if The_Tree.Curr.Brothers(Brother) = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Curr.Brothers(Brother);
    end Move_Brother;

    -- Move to saved position
    -- May raise No_Cell if no saved position
    procedure Move_Saved (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      -- Check a pos is saved
      if The_Tree.Save = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Save;
    end Move_Saved;


    ----------
    -- Dump --
    ----------
    procedure Put (Me        : in out Cell_Access;
                   Level     : in Natural;
                   Image_Acc : in Image_Access;
                   File      : in Ada.Text_Io.File_Type;
                   Elder     : in Boolean) is
      Next : Cell_Access;
    begin
      -- Put me at proper level
      Ada.Text_Io.Put_Line (Image_Acc (Me.Data.all, Level));

      -- Put children, oldest first if Elder
      if Elder then
        Next := Me.Children(Old);
      else
        Next := Me.Children(Young);
      end if;
      if Level /= Natural'Last then
        while Next /= null loop
          Put (Next, Level + 1, Image_Acc, File, Elder);
        end loop;
      end if;

      -- Move to brother, younger if Elder
      if Elder then
        Me := Me.Brothers(Young);
      else
        Me := Me.Brothers(Old);
      end if;
    end Put;

    procedure Dump (The_Tree  : in Tree_Type;
                    Image_Acc : in Image_Access;
                    File      : in Ada.Text_Io.File_Type;
                    Elder     : in Boolean := True) is
      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      if The_Tree.Root = null then
        return;
      end if;
      Cell_Acc := The_Tree.Curr;
      Put (Cell_Acc, 0, Image_Acc, File, Elder);
    end Dump;

    -- Iterate on current and children
    procedure Recurs (Me         : in out Cell_Access;
                      Do_One_Acc : in Do_One_Access;
                      Elder      : in Boolean) is
      Next : Cell_Access;
    begin
      -- Do_One on me, stop if it returns False
      if not Do_One_Acc (Me.Data.all) then
        return;
      end if;

      -- Iterate on children, oldest first if Elder
      if Elder then
        Next := Me.Children(Old);
      else
        Next := Me.Children(Young);
      end if;
      while Next /= null loop
        Recurs (Next, Do_One_Acc, Elder);
      end loop;

      -- Move to younger (if Eldest) older brother
      if Elder then
        Me := Me.Brothers(Young);
      else
        Me := Me.Brothers(Old);
      end if;
    end Recurs;

    procedure Iterate (The_Tree   : in Tree_Type;
                       Do_One_Acc : in Do_One_Access;
                       Elder      : in Boolean := True) is

      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      if The_Tree.Root = null then
        return;
      end if;
      Cell_Acc := The_Tree.Curr;
      Recurs (Cell_Acc, Do_One_Acc, Elder);
    end Iterate;

  end Tree;

end Trees;

