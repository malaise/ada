-- Tree of elements
-- A tree starts with a root (first node)
-- Each node / cell has
--  a data (Element_Type)
--  some children (0 to N)
--  some brothers (0 to N)
--  one father.
-- Root has no brother and no father
private with Ada.Finalization;
private with Unlimited_Pool, Magic_Numbers;
with Trilean;
package Trees is

  -- Common definitions for all trees
  --  (exceptions are also common, see below)
  -- Amount of children of a father
  subtype Child_Range is Natural;

  -- What to do after current item when iterating in tree
  subtype Iteration_Policy is Trilean.Trilean;
  -- Go_On scanning the tree, Skip subtree of current element, Give_Up
  Go_On   : constant Iteration_Policy := Trilean.True;
  Skip    : constant Iteration_Policy := Trilean.Other;
  Give_Up : constant Iteration_Policy := Trilean.False;

  generic
    -- Element to store in the tree
    type Element_Type is private;

  package Tree is
    -- A tree
    type Tree_Type is tagged limited private;

    -- All operations that modify the tree, plus Dump and Iterate, may raise
    --  In_Callback if already in a callback (Dump or Iterate)

    -- All operations except Is_Empty, Insert_Father, Dump and Iterate, may
    --  raise No_Cell if the tree is empty

    -- Check if tree is empty
    function Is_Empty (The_Tree : in Tree_Type) return Boolean;

    -- Insertions --
    ----------------
    -- All insertions may raise Too_Many_Children if the amount of children
    --  is already Natural'Last

    -- Insert a father of current position
    --  (insert the root if the tree is empty)
    -- Move to it
    procedure Insert_Father (The_Tree : in out Tree_Type;
                             Element  : in Element_Type);

    -- Insert a child of current position
    --  either as eldest or youngest child
    -- Move to it
    -- May raise No_Cell if The_Tree is empty
    procedure Insert_Child (The_Tree : in out Tree_Type;
                            Element  : in Element_Type;
                            Eldest   : in Boolean := True);

    -- Insert a brother of current position
    --  either as older or younger brother of current position
    -- Move to it
    -- May raise No_Cell if The_Tree is empty
    -- May raise Is_Root if current cell is root
    procedure Insert_Brother (The_Tree : in out Tree_Type;
                              Element  : in Element_Type;
                              Elder    : in Boolean := True);

    -- Saved position --
    -------------------
    -- Position refers to a cell in the tree
    -- It is forbidden to delete cells while some positions are saved
    --  so the tree cannot be empty when restoring or popping a position
    --  (except if also there is no position)

    -- Push current position in a Lifo
    -- May raise No_Cell if The_Tree is empty
    procedure Save_Position (The_Tree : in out Tree_Type);

    -- Pop last pushed position an move to it
    -- May raise No_Saved_Position if no (more) saved position in the Lifo
    procedure Restore_Position (The_Tree : in out Tree_Type);

    -- Pop last pushed position but does not move to it
    -- May raise No_Saved_Position if no (more) saved position in the Lifo
    procedure Pop_Position (The_Tree : in out Tree_Type);


    -- Deletions --
    ---------------
    -- When Deallocate is set then the free lists of deleted elements
    --  are cleared

    -- Delete current cell if it has no children, moving to father
    -- There must be no saved position
    -- May raise No_Cell if The_Tree is empty
    -- May raise Has_Children if current cell has children
    -- May raise Saved_Position if some position is currently saved
    procedure Delete_Current (The_Tree : in out Tree_Type;
                              Deallocate : in Boolean := False);

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


    -- Use saved position --
    ------------------------
    -- Swap saved pos and its sub tree with current position and its sub tree
    -- Saved position is poped.
    -- Current position remains the same cell (it follows the swapped cell)
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no position is saved
    -- May raise Is_Ancestor if one (current or saved) pos is ancestor of the
    --  other
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

    -- Move saved pos and its sub tree as (elder or youger) son or brother of
    --  current position.
    -- Saved position is poped.
    -- Current position becomes the moved cell
    -- May raise No_Cell if The_Tree is empty
    -- May raise No_Saved_Position if no position is saved
    -- May raise Is_Ancestor if saved is ancestor of current
    procedure Move_Saved (The_Tree : in out Tree_Type;
                          Child    : in Boolean;
                          Elder    : in Boolean := True);


    -- Look up --
    -------------
    -- Has current cell a father (alway true except for root)
    -- May raise No_Cell if The_Tree is empty
    function Has_Father (The_Tree : Tree_Type) return Boolean;

    -- Has current cell an elder/younger brother
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
    -- May raise No_Cell if no father (pos is root or tree is empty)
    procedure Move_Father (The_Tree : in out Tree_Type);

    -- Move to oldest/youngest child
    -- May raise No_Cell if no child or if tree is empty
    procedure Move_Child (The_Tree : in out Tree_Type;
                          Eldest   : in Boolean := True);

    -- Move to older/younger brother
    -- May raise No_Cell if no such brother or if tree is empty
    procedure Move_Brother (The_Tree : in out Tree_Type;
                            Elder    : in Boolean := True);


    -- Access to Current --
    -----------------------
    -- WARNING:
    -- It is up to the user to ensure the validity of the access that he may
    --  store, versus changes of tree (deletion, swap...)
    -- May raise No_Cell if tree is empty
    type Position_Access is private;
    No_Position : constant Position_Access;
    -- Retrieve current position
    function Get_Position (The_Tree : Tree_Type) return Position_Access;
    -- Move to a position
    -- May raise No_Cell if The_Tree is empty or not the same as the one
    --  from which the position was got
    procedure Set_Position (The_Tree : in out Tree_Type;
                            Position : in Position_Access);


    -- Multi-tree operations
    ------------------------
    -- Swap sub-trees (from current position) of two trees
    -- Any of the trees can be empty, in this case this is a move of a sub-tree
    -- Curent positions are updated
    -- Both trees must have no saved position
    -- May raise Saved_Position if some position is currently saved for Tree_A
    -- or Tree_B
    procedure Swap_Trees (Tree_A, Tree_B : in out Tree_Type);

    -- Copy cell and sub-tree (from current position) of a tree
    --  as (elder or youger) son or brother of current position in another
    --  tree.
    -- To_Tree can be empty but not From_Tree
    -- Curent position of To_Tree is updated
    -- May raise No_Cell if From_Tree is empty
    procedure Copy_Tree (To_Tree   : in out Tree_Type;
                         From_Tree : in Tree_Type;
                         Child     : in Boolean;
                         Elder     : in Boolean := True);


    -- Iterate --
    -------------
    -- What to do on current item, and what to do next
    type Do_One_Access is access
         function (Element : in out Element_Type;
                   Level : Natural) return Iteration_Policy;

    -- Iterate on current and children (old to young by default)
    -- Nothing if tree is empty
    -- Level is 0 on current item
    procedure Iterate (The_Tree   : in out Tree_Type;
                       Do_One_Acc : access function
                           (Element : in out Element_Type;
                            Level   : Natural) return Iteration_Policy;
                       Elder      : in Boolean := True);

  private

    -- One cell of tree and access to it
    type Cell_Rec;
    type Cell_Access is access Cell_Rec;

    -- The Lifo of saved position;
    package Saved_Pool_Manager is new Unlimited_Pool (Cell_Access,
                                                      Lifo => True);
    package Saved_Pool renames Saved_Pool_Manager.Upool;
    type Saved_Pool_Access is access Saved_Pool.Pool_Type;

    -- Access to current data
    type Element_Access is access all Element_Type;

    -- A tree
    type Tree_Type is limited new Ada.Finalization.Limited_Controlled
    with record
      Magic : Magic_Numbers.Magic_Long := Magic_Numbers.Generate;
      Root : Cell_Access := null;
      Curr : Cell_Access := null;
      Save : Saved_Pool_Access := null;
      In_Cb : Boolean := False;
    end record;
    overriding procedure Finalize (Tree : in out Tree_Type);

    -- A position to get/set
    type Position_Access is record
      Magic : Magic_Numbers.Extended_Magic_Long := Magic_Numbers.Magic_Long0;
      Cell_Acc : Cell_Access;
    end record;
    No_Position : constant Position_Access := (others => <>);

  end Tree;

  -- Exceptions
  -------------
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

