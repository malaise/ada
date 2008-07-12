with Ada.Text_Io;
with Unlimited_Pool;
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


    -- All operations that modify the tree, plus Dump and Iterate, may raise
    --  In_Callback if already in a callback (Dump or Iterate)

    -- All operations except Insert_Father, Dump and Iterate, may raise No_Cell
    --  if the tree is empty

    -- Check if tree is empty
    function Is_Empty (The_Tree : in Tree_Type) return Boolean;

    -- Insertions --
    ----------------
    -- All may raise Too_Many_Children if amount of children
    --  is Natural'Last

    -- Insert a father of current position
    --  (insert the root if the tree is empty)
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

    -- Saved position --
    -------------------
    -- Position points to the cell in the tree, so it points
    --  to the same cell (same data) when inserting fathers.

    -- Push current position in a Lifo
    -- May raise No_Cell if The_Tree is empty
    procedure Save_Position (The_Tree : in out Tree_Type);

    -- Pop last pushed position an move to it
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no (more) saved position in the Lifo
    procedure Restore_Position (The_Tree : in out Tree_Type);

    -- Pop last pushed position but does not move to it
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no (more) saved position in the Lifo
    procedure Pop_Position (The_Tree : in out Tree_Type);


    -- Deletions --
    ---------------
    -- Delete current cell if it has no children, moving to father
    -- There must be no saved position
    -- May raise No_Cell if The_Tree is empty
    -- May raise Has_Children if current cell has children
    -- May raise Saved_Position if some position is currently saved
    procedure Delete_Current (The_Tree : in out Tree_Type);

    -- Clear the whole sub-tree, moving to father
    -- There must be no saved position
    -- May raise No_Cell if The_Tree is empty
    -- May raise Saved_Position if some position is currently saved
    procedure Delete_Tree (The_Tree   : in out Tree_Type;
                           Deallocate : in Boolean := True);


    -- Read / Replace --
    --------------------
    -- Read current element
    -- May raise No_Cell if The_Tree is empty
    procedure Read (The_Tree : in Tree_Type;
                    Element  : out Element_Type);
    function Read (The_Tree : Tree_Type) return Element_Type;

    -- Replace current element
    -- May raise No_Cell if The_Tree is empty
    procedure Replace (The_Tree : in Tree_Type;
                       Element  : in Element_Type);

    -- Replace current element, returning previous
    -- May raise No_Cell if The_Tree is empty
    procedure Swap (The_Tree : in Tree_Type;
                    Element  : in out Element_Type);

    -- Swap saved pos and its sub tree with current position and its sub tree
    -- Saved position is poped.
    -- Current position remains the same cell (it follows the swapped cell)
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no position is saved
    -- May raise Is_Ancestor if one (current or saved) is ancestor of the other
    procedure Swap_Saved (The_Tree : in out Tree_Type);

    -- Copy saved pos and its sub tree as (elder or youger) son or brother of
    --  current position.
    -- Saved position is poped.
    -- Current position becomes the copied cell
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no position is saved
    -- May raise Is_Ancestor if saved is ancestor of current
    procedure Copy_Saved (The_Tree : in out Tree_Type;
                          Child    : in Boolean;
                          Elder    : in Boolean := True);


    -- Look up --
    -------------
    -- Has current cell a father (alway true except for root)
    -- May raise No_Cell if The_Tree is empty
    function Has_Father (The_Tree : Tree_Type) return Boolean;

    -- Has current cell and elder/younger brother
    -- May raise No_Cell if The_Tree is empty
    function Has_Brother (The_Tree : Tree_Type;
                          Elder    : Boolean := True) return Boolean;

    -- How many children has current cell
    -- May raise No_Cell if The_Tree is empty
    function Children_Number (The_Tree : Tree_Type) return Child_Range;


    -- Move --
    ----------
    -- Move to root
    -- May raise No_Cell if The_Tree is empty
    procedure Move_Root (The_Tree : in out Tree_Type);

    -- Move to father
    -- May raise No_Cell if no father (at root or tree is empty)
    procedure Move_Father (The_Tree : in out Tree_Type);

    -- Move to oldest/youngest child
    -- May raise No_Cell if no child or if tree is empty
    procedure Move_Child (The_Tree : in out Tree_Type;
                          Elder    : in Boolean := True);

    -- Move to older/younger brother
    -- May raise No_Cell if no such brother or if tree is empty
    procedure Move_Brother (The_Tree : in out Tree_Type;
                            Elder    : in Boolean := True);

    -- Access to Current --
    -----------------------
    -- WARNING:
    -- It is up to the user to ensure the validity of the access it may store
    -- versus changes of tree
    -- May raise No_Cell if The_Tree is empty
    type Position_Access is private;
    No_Position : constant Position_Access;
    function Get_Position (The_Tree : Tree_Type) return Position_Access;
    procedure Set_Position (The_Tree : in out Tree_Type;
                            Position : in Position_Access);

    -- Multi-tree operations
    ------------------------
    -- Swap sub-trees (from current position) of two trees
    -- Any of the trees can be empty, in this case this is a move of a sub-tree
    -- Curent positions are updated
    -- Both trees must have no saved position
    procedure Swap_Trees (Tree_A, Tree_B : in out Tree_Type);

    -- Copy cell and sub-tree (from current position) of a tree
    --  as (elder or youger) son or brother of current position in another
    --  tree.
    -- To can be empty but not From
    -- Curent position of To is updated
    -- May raise No_Cell if From_Tree is empty
    procedure Copy_Tree (To_Tree   : in out Tree_Type;
                         From_Tree : in Tree_Type;
                         Child     : in Boolean;
                         Elder     : in Boolean := True);
    -- Dump --
    ----------
    -- Image of an element at a depth (level)
    type Image_Access is access function (Element : Element_Type;
                                          Level   : Natural) return String;
    -- Dump current then children data (oldest first by default)
    -- Nothing if tree is empty
    procedure Dump (The_Tree  : in Tree_Type;
                    Image_Acc :  access
      function (Element : Element_Type;
                Level   : Natural) return String;
                    File      : in Ada.Text_Io.File_Type;
                    Elder     : in Boolean := True);

    -- Iterate --
    -------------
    -- What to do on current item
    -- Iteration will continue as long as returning True
    type Do_One_Access is access function (Element : Element_Type)
                                          return Boolean;

    -- Iterate on current and children (old to young by default)
    -- Nothing if tree is empty
    procedure Iterate (The_Tree   : in out Tree_Type;
                       Do_One_Acc : access
      function (Element : Element_Type) return Boolean;
                       Elder      : in Boolean := True);

  private

    -- One cell of tree and access to it
    type Cell_Rec;
    type Cell_Access is access Cell_Rec;
    type Position_Access is new Cell_Access;
    No_Position : constant Position_Access := null;

    -- The Lifo of saved position;
    package Saved_Pool is new Unlimited_Pool (Cell_Access, Lifo => True);
    type Saved_Pool_Access is access Saved_Pool.Pool_Type;

    -- Eldest/Youngest children, or Elder/Younger brothers
    type Order is (Young, Old);
    -- Workaround of a gvd bug
    for Order use (1, 2);
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
      Save : Saved_Pool_Access := null;
      In_Cb : Boolean := False;
    end record;

    No_Tree : constant Tree_Type := (null, null, null, False);

  end Tree;

  -- When inserting too many children
  Too_Many_Children : exception;

  -- When inserting a brother or child in empty tree, or when invalid move
  No_Cell : exception;

  -- When inserting a brother of root
  Is_Root : exception;

  -- When deleting current if it has children
  Has_Children : exception;

  -- When in callback (dump / iterate)
  In_Callback : exception;

  -- When popping a saved position if there is none
  No_Saved_Position : exception;

  -- When deleting current or tree if there is some saved position
  Saved_Position : exception;

  -- When swapping/copying cells and one is ancestor of the other
  Is_Ancestor : exception;

end Trees;

