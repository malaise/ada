with Sourcer;
package Output is

  -- Put list/tree, normal/revert of units/files
  procedure Put (Revert_Mode, Tree_Mode, Shortest_Mode, File_Mode : in Boolean;
                 Path_Unit : in Sourcer.Src_Dscr);

  -- List a unit or all
  Error_Raised : exception renames Sourcer.Error_Raised;
  procedure List (Target, Dir, Path: in String;
                  File_Mode, Subunits, Children : in Boolean);

end Output;

