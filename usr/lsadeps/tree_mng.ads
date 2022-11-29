-- Dependancy tree
-- In normal mode File_Spec -> Withed_files
--                          -> File_Body -> Withed_files
--                                       -> Subunit -> Withed_Files
-- In revert mode: File_Spec -> Withing_files
--                           -> File_Body -> File_Subunits
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
  procedure Build (Origin : in Sourcer.Src_Dscr;
                   -- Only consider dependencies within specs
                   Specs_Mode,
                   -- Tree of units recursively withing Origin
                   Revert_Mode,
                   -- Build the full tree
                   Tree_Mode,
                   -- Keep the shortest path
                   Shortest_Mode,
                   -- Scan only the first level (when not full tree)
                   Direct_Mode,
                   -- Show only one path between each unit
                   Once_Mode,
                   -- List the files (not the units)
                   File_Mode,
                   -- In revert mode, also consider bodies withing unit
                   Bodies_Mode,
                   -- Skip limited and private with
                   Restrict_Mode,
                   -- Show loops of with
                   Loop_Mode : in Boolean);

end Tree_Mng;

