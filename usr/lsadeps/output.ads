with Sourcer;
package Output is

  -- Put list/tree, normal/revert of units/files
  procedure Put (Revert_Mode, Tree_Mode, Direct_Mode, File_Mode : in Boolean;
                 Path_Unit : in Sourcer.Src_Dscr);

  -- List a unit or all
  Error_raised : exception renames Sourcer.Error_raised;
  procedure List (Target, Dir, Path: in String;
                  File_Mode, All_Mode : in Boolean);

end Output;

