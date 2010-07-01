with Trees;
with Sourcer;
package Tree_Mng is

  -- The source descriptor with an indicator of loop
  type Src_Dscr is record
    Dscr : Sourcer.Src_Dscr;
    Looping : Boolean := False;
  end record;

  -- The tree of sources
  package Tree_Mng is new Trees.Tree (Src_Dscr);
  Tree : Tree_Mng.Tree_Type;

  -- Build the tree of source dependencies of Origin
  procedure Build (Origin : in Sourcer.Src_Dscr; Revert : in Boolean);

end Tree_Mng;

