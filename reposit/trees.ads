with Ada.Text_Io;
package Trees is

  -- Amount of children of a father
  subtype Child_Range is Natural;

  -- A tree starts with a root (first branch)
  -- Each branch / cell has
  --  a data (Element_Type)
  --  some children (0 to N)
  --  some brothers (0 to N, root has no brother)
  --  one father (except root).
  generic
    -- Element to store in the tree
    type Element_Type is private;

  package Tree is
    -- A tree
    type Tree_Type is limited private;
    No_Tree : constant Tree_Type;

    -- Access to current data
    type Element_Access is access all Element_Type;


    -- Insertions --
    ----------------
    -- All may raise Too_Many_Children if amount of children
    --  is Natural'Last

    -- Insert a father of current position (root if tree is empty)
    -- Move to it
    procedure Insert_Father (The_Tree : in out Tree_Type;
                             Element  : in Element_Type);

    -- Insert a child of current position
    -- Append or prepend to the list of children
    -- Move to it
    -- May raise No_Cell if The_Tree is empty
    procedure Insert_Child (The_Tree : in out Tree_Type;
                            Element  : in Element_Type;
                            Eldest   : in Boolean := True);

    -- Insert a brother of current position
    --  as older or younger brother of current position
    -- Move to it
    -- May raise No_Cell if The_Tree is empty
    -- May raise Is_Root if current is root
    procedure Insert_Brother (The_Tree : in out Tree_Type;
                              Element  : in Element_Type;
                              Elder    : in Boolean := True);


    -- Deletions --
    ---------------

    -- Delete current cell if it has no children, moving to father
    -- May raise No_Cell if The_Tree is empty
    -- May raise Has_Children if current cell has children
    procedure Delete_Current (The_Tree : in out Tree_Type);

    -- Clear the whole sub-tree, moving to father
    procedure Delete_Tree (The_Tree   : in out Tree_Type;
                           Deallocate : in Boolean := True);


    -- Save position --
    ------------------
    -- Position points to the cell in the tree, so it points
    --  to the same cell (same data) when inserting fathers.
    -- It is reset when saved cell is deleted
    -- May raise No_Cell if The_Tree is empty
    procedure Save_Position (The_Tree : in out Tree_Type);


    -- Read / Replace --
    --------------------
    -- All may raise No_Cell if The_Tree is empty

    -- Read current element
    procedure Read (The_Tree : in Tree_Type;
                    Element  : out Element_Type);
    function Read (The_Tree : Tree_Type) return Element_Type;

    -- Replace current element
    procedure Replace (The_Tree : in Tree_Type;
                       Element  : in Element_Type);

    -- Replace current element, returning previous
    procedure Swap (The_Tree : in Tree_Type;
                    Element  : in out Element_Type);


    -- Swap saved pos and its sub tree with current position and its sub tree
    -- May raise No_Cell if no position is saved
    procedure Swap_Saved (The_Tree : in Tree_Type);


    -- Look up --
    -------------
    -- All may raise No_Cell if The_Tree is empty

    -- Has current cell a father (alway true except for root)
    function Has_Father (The_Tree : Tree_Type) return Boolean;

    -- Has current cell and elder/younger brother
    function Has_Brother (The_Tree : Tree_Type;
                          Elder    : Boolean := True) return Boolean;

    -- How many children has current cell
    function Children_Number (The_Tree : Tree_Type) return Child_Range;


    -- Move --
    ----------
    -- All may raise No_Cell if The_Tree is empty

    -- Move to root
    procedure Move_Root (The_Tree : in out Tree_Type);

    -- Move to father
    -- May raise No_Cell if no father (root)
    procedure Move_Father (The_Tree : in out Tree_Type);

    -- Move to oldest/youngest child
    -- May raise No_Cell if no child
    procedure Move_Child (The_Tree : in out Tree_Type;
                          Elder    : in Boolean := True);

    -- Move to older/younger brother
    -- May raise No_Cell if no such brother
    procedure Move_Brother (The_Tree : in out Tree_Type;
                            Elder    : in Boolean := True);

    -- Move to saved position
    -- May raise No_Cell if no saved position
    procedure Move_Saved (The_Tree : in out Tree_Type);


    -- Dump --
    ----------
    -- Image of an element at a depth (level)
    type Image_Access is access function (Element : Element_Type;
                                          Level   : Natural) return String;
    -- Dump current then children data (oldest first by default)
    procedure Dump (The_Tree  : in Tree_Type;
                    Image_Acc : in Image_Access;
                    File      : in Ada.Text_Io.File_Type;
                    Elder     : in Boolean := True);

    -- Iterate --
    -------------
    -- What to do on current item
    -- Iteration will continue as long as returning True
    type Do_One_Access is access function (Element : Element_Type)
                                          return Boolean;

    -- Iterate on current and children (old to young by default)
    procedure Iterate (The_Tree   : in Tree_Type;
                       Do_One_Acc : in Do_One_Access;
                       Elder      : in Boolean := True);

  private

    -- One cell of tree and access to it
    type Cell_Rec;
    type Cell_Access is access Cell_Rec;

    -- Eldest/Youngest children, or Elder/Younger brothers
    type Order is (Young, Old);
    -- Workaround of a gvd bug
    -- for Order use (1, 2);
    type Cell_Pair is array (Order) of Cell_Access;

    -- A cell of tree
    type Cell_Rec is record
      Father   : Cell_Access := null;
      Brothers : Cell_Pair := (others => null);
      Nb_Children : Child_Range := 0;
      Children : Cell_Pair := (others => null);
      Data : Element_Access := null;
    end record;

    -- A tree
    type Tree_Type is record
      Root : Cell_Access := null;
      Curr : Cell_Access := null;
      Save : Cell_Access := null;
      In_Cb : Boolean := False;
    end record;

    No_Tree : constant Tree_Type := (null, null, null, False);

  end Tree;

  -- When inserting too many children
  Too_Many_Children : exception;

  -- When inserting a brother or child in empty tree, or invalid move
  No_Cell : exception;

  -- When inserting a brother of root
  Is_Root : exception;

  -- When deleting current
  Has_Children : exception;

  -- When in callback (dump / iterate)
  In_Callback : exception;

end Trees;

