with Sourcer;
package Output is

  -- Put list/tree, normal/revert of units/files
  procedure Put (Revert_Mode, Tree_Mode, File_Mode : in Boolean;
                 Path_Unit : in Sourcer.Src_Dscr);

end Output;

