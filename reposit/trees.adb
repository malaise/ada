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
    package Cell_Dyn is new Dyn_Data (Cell, Cell_Access);
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
      if Me.Brothers(True) /= null then
        Me.Brothers(True).Brothers(False) := Me;
      end if;
      if Me.Brothers(False) /= null then
        Me.Brothers(False).Brothers(True) := Me;
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
      Cell_Acc.Children(True)  := Prev_Curr;
      Cell_Acc.Children(False) := Prev_Curr;
      Cell_Acc.Father := Prev_Curr.Father;

      -- Link Root or previous father to new cell
      if Prev_Curr.Father = null then
        -- Inserting father of root (as root)
        The_Tree.Root := Cell_Acc;
      else
        if Prev_Curr.Brothers(True) = null then
          -- Inserting instead of oldest, link father to it
          Prev_Curr.Father.Children(True) := Cell_Acc;
        end if;
        if Prev_Curr.Brothers(False) = null then
          -- Inserting instead of youngest, link father to it
          Prev_Curr.Father.Children(False) := Cell_Acc;
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
      Cell_Acc.Father := The_Tree.Curr;
      The_Tree.Curr := Cell_Acc;
      Cell_Acc.Brothers(not Eldest) := Cell_Acc.Father.Children(Eldest);
      Cell_Acc.Father.Children(Eldest) := Cell_Acc;
      if Cell_Acc.Father.Nb_Children = 0 then
        Cell_Acc.Father.Children(not Eldest) := Cell_Acc;
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

      -- Check number of children
      if The_Tree.Curr.Nb_Children = Child_Range'Last then
        raise Too_Many_Children;
      end if;

      -- Create
      Cell_Acc := Allocate (Element);

      -- Insert and move
      Cell_Acc.Father := The_Tree.Curr.Father;
      Cell_Acc.Brothers := The_Tree.Curr.Brothers;
      if Elder then
        if Cell_Acc.Brothers(True) = null then
          -- Inserting eldest
          Cell_Acc.Father.Children(True) := Cell_Acc;
        end if;
        Cell_Acc.Brothers(False) := The_Tree.Curr;
      else
        if Cell_Acc.Brothers(False) = null then
          -- Inserting youngest
          Cell_Acc.Father.Children(False) := Cell_Acc;
        end if;
        Cell_Acc.Brothers(True) := The_Tree.Curr;
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
        The_Tree.Root := null;
        Cell_Dyn.Free (The_Tree.Curr);
        return;
      end if;

      -- Relink father and brothers 
      if Cell_Acc.Brothers(True) = null then
        -- deleting oldest
        Cell_Acc.Father.Children(True) := Cell_Acc.Brothers(False);
      else
        Cell_Acc.Brothers(True).Brothers(False) := Cell_Acc.Brothers(False);
      end if;
      if Cell_Acc.Brothers(False) = null then
        -- deleting oldest
        Cell_Acc.Father.Children(False) := Cell_Acc.Brothers(True);
      else
        Cell_Acc.Brothers(False).Brothers(True) := Cell_Acc.Brothers(True);
      end if;

      -- Update father and move to it
      Cell_Acc.Father.Nb_Children := Cell_Acc.Father.Nb_Children - 1;
      The_Tree.Curr := Cell_Acc.Father;

      -- Free cell
      Cell_Dyn.Free (Cell_Acc);
    end Delete_Current;

    -- Clean a cell recursively
    procedure Clean (Me : in out Cell_Access) is
      Next : Cell_Access;
    begin
      -- Clean children
      Next := Me.Children(True);
      while Next /= null loop
        Clean (Next);
      end loop;

      -- No more child: free me and return next brother
      Next := Me.Brothers(True);
      Data_Dyn.Free (Me.Data);
      Cell_Dyn.Free (Me);
    end Clean;

    -- Clear the whole tree
    procedure Reset (The_Tree : in out Tree_Type;
                     Deallocate : in Boolean := True) is
    begin
      if The_Tree.Root /= null then
        -- Clean the tree from root
        Clean (The_Tree.Root);
        The_Tree.Curr := null;
      end if;
      -- Deallocate cells and data if requested
      if Deallocate then
        Data_Dyn.Clear;
        Cell_Dyn.Clear;
      end if;
    end Reset;

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
      return The_Tree.Curr.Brothers(Elder) /= null;
    end Has_Brother;

    -- How many children has current cell
    function Children_Number (The_Tree : Tree_Type) return Child_Range is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      return The_Tree.Curr.Nb_Children;
    end Children_Number;


    ------------------
    -- Save position --
    ------------------
    procedure Save_Position (The_Tree : in out Tree_Type) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);
      The_Tree.Save := The_Tree.Curr;
    end Save_Position;



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
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      if The_Tree.Curr.Children(Elder) = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Curr.Children(Elder);
    end Move_Child;


    -- Move to older/younger brother
    -- May raise No_Cell if no such brother
    procedure Move_Brother (The_Tree : in out Tree_Type;
                            Elder    : in Boolean := True) is
    begin
      -- No empty tree
      Check_Empty (The_Tree);

      if The_Tree.Curr.Brothers(Elder) = null then
        raise No_Cell;
      end if;

      The_Tree.Curr := The_Tree.Curr.Brothers(Elder);
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
    procedure Put (Me : in out Cell_Access;
                   Level : in Natural;
                   Image_Acc : in Image_Access;
                   File : in Ada.Text_Io.File_Type) is
      Next : Cell_Access;
    begin
      -- Put me at proper level
      Ada.Text_Io.Put_Line (Image_Acc (Me.Data.all, Level));

      -- Put children, youngest first
      Next := Me.Children(False);
      if Level /= Natural'Last then
        while Next /= null loop
          Put (Next, Level + 1, Image_Acc, File);
        end loop;
      end if;

      -- Move to older brother
      Me := Me.Brothers(True);
    end Put;

    procedure Dump (The_Tree : in Tree_Type;
                    Image_Acc : in Image_Access;
                    File     : in Ada.Text_Io.File_Type) is
      Cell_Acc : Cell_Access;
    begin
      -- No empty tree
      if The_Tree.Root = null then
        return;
      end if;
      Cell_Acc := The_Tree.Curr;
      Put (Cell_Acc, 0, Image_Acc, File);
    end Dump;

  end Tree;

end Trees;

