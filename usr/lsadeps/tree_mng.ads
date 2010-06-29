with Trees;
with Sourcer;
package Tree_Mng is

  -- The tree of sources
  package Tree_Mng is new Trees.Tree (Sourcer.Src_Dscr);
  Tree : Tree_Mng.Tree_Type;

  -- Build the tree of source dependencies of Origin
  procedure Build (Origin : in Sourcer.Src_Dscr);

  -- Dump the tree
  procedure Dump;

end Tree_Mng;

