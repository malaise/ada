with Dyn_Data;
package body Trees is

  -- A tree starts with a root (first branch)
  -- Each branch / cell has
  --  a data (Element_Type)
  --  some children (0 to N)
  --  some brothers (0 to N, root has no brother)
  --  one father (except root).
  package body Tree is

    -------------------------
    -- Internal operations --
    -------------------------
    -- Check The_Tree is not empty
    procedure Check_Empty (The_Tree : in Tree_Type) is
    begin
      -- No empty tree
      if The_Tree.Curr = null then
        raise No_Cell;
      end if;
    end Check_Empty;

    -- Chek The_Tree is not empty and not in a callback
    procedure Check_Callback (The_Tree : in Tree_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      if The_Tree.In_Cb then
        raise In_Callback;
      end if;
    end Check_Callback;

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

    -- Initialise a Tree with a Cell
    procedure Init_Tree (The_Tree : in out Tree_Type;
                         Init_Cell : in Cell_Access) is
    begin
        -- Set Root and current
        The_Tree.Root := Init_Cell;
        The_Tree.Curr := Init_Cell;
        -- Create the pool of save position at first insertion of root
        if The_Tree.Save = null then
          The_Tree.Save := new Saved_Pool.Pool_Type;
        end if;
    end Init_Tree;

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

    -- Link my father to me
    -- Does not increment its number of children
    procedure Link_Father (Me : in Cell_Access) is
    begin
      -- No father => root
      if Me.Father = null then
        return;
      end if;
      -- Handle if I am youngest or eldest
      if Me.Brothers(Young) = null then
        Me.Father.Children(Young) := Me;
      end if;
      if Me.Brothers(Old) = null then
        Me.Father.Children(Old) := Me;
      end if;
    end Link_Father;

    -- Swap two cells A and B
    -- Does not update Root if A or B is root
    procedure Swap_Cells (A, B : in Cell_Access) is
      Tmp : Cell_Rec := B.all;
    begin
      -- Update B from A
      B.Father := A.Father;
      B.Brothers := A.Brothers;
      Link_Brothers (B);
      Link_Father (B);

      -- Update A from B (Tmp)
      A.Father := Tmp.Father;
      A.Brothers := Tmp.Brothers;
      Link_Brothers (A);
      Link_Father (A);
    end Swap_Cells;

    -- Detach my father and my brothers from me
    -- Does not update Nb_Children of father
    procedure Detach (Me : in Cell_Access) is
    begin
      if Me.Brothers(Old) = null then
        -- Deleting oldest
        Me.Father.Children(Old) := Me.Brothers(Young);
      else
        Me.Brothers(Old).Brothers(Young) := Me.Brothers(Young);
      end if;
      if Me.Brothers(Young) = null then
        -- Deleting youngest
        Me.Father.Children(Young) := Me.Brothers(Old);
      else
        Me.Brothers(Young).Brothers(Old) := Me.Brothers(Old);
      end if;
    end Detach;

    ----------------
    -- Insertions --
    ----------------
    -- All may raise Too_Many_Children if amount of children
    --  is Natural'Last

    -- Insert a father of current position
    --  (insert the root if the tree is empty)
    -- Move to it
    procedure Insert_Father (The_Tree : in out Tree_Type;
                             Element  : in Element_Type) is
      Cell_Acc, Prev_Curr : Cell_Access;
    begin
      -- Save current
      if The_Tree.In_Cb then
        raise In_Callback;
      end if;

      -- Save current
      Prev_Curr := The_Tree.Curr;

      -- Create cell and move to it
      Cell_Acc := Allocate (Element);

      if Prev_Curr = null then
        -- Tree was empty, init it
        Init_Tree (The_Tree, Cell_Acc);
        return;
      end if;

      -- Move to new cell and make it the father of previous
      The_Tree.Curr := Cell_Acc;
      Cell_Acc.Nb_Children := 1;
      Cell_Acc.Children(Old)  := Prev_Curr;
      Cell_Acc.Children(Young) := Prev_Curr;

      -- Inherit father of previous
      -- and link previous father (or root) to new cell
      Cell_Acc.Father := Prev_Curr.Father;
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
      Check_Callback (The_Tree);

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
    --  as older or younger brother of current position
    -- Move to it
    -- May raise No_Cell if The_Tree is empty or current is root
    -- May raise Is_Root if current is root
    procedure Insert_Brother (The_Tree : in out Tree_Type;
                              Element  : in Element_Type;
                              Elder   : in Boolean := True) is
      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      Check_Callback (The_Tree);

      -- Not root
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

      -- Link to father and brothers
      Cell_Acc.Father := The_Tree.Curr.Father;
      Cell_Acc.Brothers := The_Tree.Curr.Brothers;
      if Elder then
        Cell_Acc.Brothers(Young) := The_Tree.Curr;
      else
        Cell_Acc.Brothers(Old) := The_Tree.Curr;
      end if;
      -- Link father and brothers to me
      Link_Father (Cell_Acc);
      Link_Brothers (Cell_Acc);
      -- Move to inserted
      The_Tree.Curr := Cell_Acc;

    end Insert_Brother;


    -------------------
    -- Saved position --
    -------------------
    -- Push current position in a Lifo
    -- May raise No_Cell if The_Tree is empty
    procedure Save_Position (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Callback (The_Tree);
      Saved_Pool.Push (The_Tree.Save.all, The_Tree.Curr);
    end Save_Position;

    -- Pop last pushed position and move to it
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no (more) saved position in the Lifo
    procedure Restore_Position (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Callback (The_Tree);
      if Saved_Pool.Is_Empty (The_Tree.Save.all) then
        raise No_Saved_Position;
      end if;
      Saved_Pool.Pop (The_Tree.Save.all, The_Tree.Curr);
    end Restore_Position;

    -- Pop last pushed position but does not move to it
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no (more) saved position in the Lifo
    procedure Pop_Position (The_Tree : in out Tree_Type) is
      Tmp_Cell : Cell_Access;
    begin
      -- No empty tree
      Check_Callback (The_Tree);
      if Saved_Pool.Is_Empty (The_Tree.Save.all) then
        raise No_Saved_Position;
      end if;
      Saved_Pool.Pop (The_Tree.Save.all, Tmp_Cell);
    end Pop_Position;


    ---------------
    -- Deletions --
    ---------------
    -- Delete current cell if it has no children, moving to father
    -- There must be no saved position
    -- May raise No_Cell if The_Tree is empty
    -- May raise Has_Children if current cell has children
    -- May raise Saved_Position if some position is currently saved
    procedure Delete_Current (The_Tree : in out Tree_Type) is
      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      Check_Callback (The_Tree);

      -- No saved position
      if not Saved_Pool.Is_Empty (The_Tree.Save.all) then
        raise Saved_Position;
      end if;

      -- No child
      Cell_Acc := The_Tree.Curr;
      if Cell_Acc.Nb_Children /= 0 then
        raise Has_Children;
      end if;

      -- Free data
      Data_Dyn.Free (Cell_Acc.Data);

      -- Check for root (no father)
      if Cell_Acc.Father = null then
        Cell_Dyn.Free (The_Tree.Curr);
        return;
      end if;

      -- Detach Cell_Acc from its father and brothers
      Detach (Cell_Acc);

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

    -- Clear the whole sub-tree, moving to father
    -- There must be no saved position
    -- May raise No_Cell if The_Tree is empty
    -- May raise Saved_Position if some position is currently saved
    procedure Delete_Tree (The_Tree : in out Tree_Type;
                     Deallocate : in Boolean := True) is
    begin
      -- No empty tree
      Check_Callback (The_Tree);

      -- No saved position
      if not Saved_Pool.Is_Empty (The_Tree.Save.all) then
        raise Saved_Position;
      end if;

      -- Clean the children of current
      Clean_Children (The_Tree, The_Tree.Curr);
      -- Update current
      The_Tree.Curr.Children := (others => null);
      The_Tree.Curr.Nb_Children := 0;
      -- Remove current
      Delete_Current(The_Tree);

      -- Deallocate cells and data if requested
      if Deallocate then
        Data_Dyn.Clear;
        Cell_Dyn.Clear;
      end if;
    end Delete_Tree;


    --------------------
    -- Read / Replace --
    --------------------
    -- All may raise No_Cell if The_Tree is empty

    -- Read current element
    -- May raise No_Cell if The_Tree is empty
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
    -- May raise No_Cell if The_Tree is empty
    procedure Replace (The_Tree : in Tree_Type;
                       Element  : in Element_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      The_Tree.Curr.Data.all := Element;
    end Replace;

    -- Replace current element, returning previous
    -- May raise No_Cell if The_Tree is empty
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

    -- Internal operation checking that a cell is not ancestor of an other
    function Is_Ancestor (A_Cell, Of_A_Cell : in Cell_Access) return Boolean is
      Tmp: Cell_Access := Of_A_Cell;
    begin
      loop
        if Tmp = A_Cell then
          -- A_Cell is one ancestor of Of_A_Cell
          return True;
        elsif Tmp.Father = null then
          -- Reached root
          exit;
        else
          -- Move up
          Tmp := Tmp.Father;
        end if;
      end loop;
      return False;
    end Is_Ancestor;

    -- Swap saved pos and its sub tree with current position and its sub tree
    -- Saved position is poped.
    -- Current position remains the same cell (it follows the swapped cell)
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no position is saved
    procedure Swap_Saved (The_Tree : in out Tree_Type) is

      Saved : Cell_Access;
      Tmp : Cell_Rec;
    begin
      -- No empty tree
      Check_Callback (The_Tree);

      -- Check a pos is saved
      if Saved_Pool.Is_Empty (The_Tree.Save.all) then
        raise No_Saved_Position;
      end if;

      -- Get saved pos and check that current and saved are not ancestors
      Saved_Pool.Pop (The_Tree.Save.all, Saved);
      if Is_Ancestor (Saved, The_Tree.Curr)
      or else Is_Ancestor (The_Tree.Curr, Saved) then
        raise Swap_Ancestor;
      end if;
      -- Nothing if saved is current
      if Saved = The_Tree.Curr then
        return;
      end if;

      -- Swap cells. None of them can be root, it would have been
      --  detected as ancestor of the other
      Swap_Cells (The_Tree.Curr, Saved);
    end Swap_Saved;

    -------------
    -- Look up --
    -------------
    -- Has current cell a father (alway true except for root)
    -- May raise No_Cell if The_Tree is empty
    function Has_Father (The_Tree : Tree_Type) return Boolean is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      return The_Tree.Curr.Father /= null;
    end Has_Father;

    -- Has current cell and elder/younger brother
    -- May raise No_Cell if The_Tree is empty
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
    -- May raise No_Cell if The_Tree is empty
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
    -- May raise No_Cell if The_Tree is empty
    procedure Move_Root (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Callback (The_Tree);

      The_Tree.Curr := The_Tree.Root;
    end Move_Root;

    -- Move to father
    -- May raise No_Cell if no father (at root or tree is empty)
    procedure Move_Father (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Callback (The_Tree);

      if The_Tree.Curr.Father = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Curr.Father;
    end Move_Father;

    -- Move to oldest/youngest child
    -- May raise No_Cell if no child or if tree is empty
    procedure Move_Child (The_Tree : in out Tree_Type;
                          Elder    : in Boolean := True) is
      Child : Order;
    begin
      -- No empty tree
      Check_Callback (The_Tree);

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
    -- May raise No_Cell if no such brother or if tree is empty
    procedure Move_Brother (The_Tree : in out Tree_Type;
                            Elder    : in Boolean := True) is
      Brother : Order;
    begin
      -- No empty tree
      Check_Callback (The_Tree);

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

    ------------------------
    -- Multi-tree operations
    ------------------------
    -- Swap sub-trees (from current position) of two trees
    -- Any of the trees can be empty, in this case this is a move of a sub-tree
    -- Curent positions are updated
    -- Both trees must have no saved position
    procedure Swap_Trees (Tree_A, Tree_B : in out Tree_Type) is
      Tmp : Cell_Access;
      -- Move subtree From as root of To
      procedure Move_Tree (From, To : in out Tree_Type) is
        Cell_Acc : constant Cell_Access := From.Curr;
      begin
        -- Init tree To with current of From
        Init_Tree (To, Cell_Acc);
        -- Handle case when From was root
        if Cell_Acc.Father = null then
          From.Root := null;
          From.Curr := null;
          return;
        else
          -- Move current of From to its father
          From.Curr := Cell_Acc.Father;
          -- Detach father and brothers of From
          Cell_Acc.Father.Nb_Children := Cell_Acc.Father.Nb_Children - 1;
          Detach (Cell_Acc);
        end if;
        -- Reset links of new root
        Cell_Acc.Father := null;
        Cell_Acc.Brothers := (others => null);
      end Move_Tree;

    begin
      -- No saved position
      if Tree_A.Root /= null
      and then not Saved_Pool.Is_Empty (Tree_A.Save.all) then
        raise Saved_Position;
      end if;
      if Tree_B.Root /= null
      and then not Saved_Pool.Is_Empty (Tree_B.Save.all) then
        raise Saved_Position;
      end if;

      -- Nothing if same tree or both empty
      if Tree_A = Tree_B
      or else (Tree_A.Root = null and then Tree_B.Root = null) then
        return;
      end if;

      -- Handle case when A or B is empty
      if Tree_A.Root = null then
        Move_Tree (Tree_B, Tree_A);
        return;
      elsif Tree_B.Root = null then
        Move_Tree (Tree_A, Tree_B);
        return;
      end if;

      -- Swap cells
      Swap_Cells (Tree_A.Curr, Tree_B.Curr);
      -- Update Roots if necessary
      if Tree_A.Root = Tree_A.Curr then
        Tree_A.Root := Tree_B.Curr;
      end if;
      if Tree_B.Root = Tree_B.Curr then
        Tree_B.Root := Tree_A.Curr;
      end if;
      -- Swap current
      Tmp := Tree_A.Curr;
      Tree_A.Curr := Tree_B.Curr;
      Tree_B.Curr := Tmp;
    end Swap_Trees;

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
      -- Not in callback
      if The_Tree.In_Cb then
         raise In_Callback;
      end if;
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
      -- Not in callback
      if The_Tree.In_Cb then
         raise In_Callback;
      end if;
      -- No empty tree
      if The_Tree.Root = null then
        return;
      end if;
      Cell_Acc := The_Tree.Curr;
      Recurs (Cell_Acc, Do_One_Acc, Elder);
    end Iterate;

  end Tree;

end Trees;

