with As.U.Utils;
package Sort is

  -- Set the priority level of a path (1 = Higest)
  -- So that the order of '-I' dirs is preserved in the output
  procedure Set_Prio (Path : As.U.Asu_Us; Prio : Positive);

  -- Sort entries ([<path>/]<file>)
  --  First the entries without path
  --  Then in order of prio
  --  Then the entries without prio
  --  At each level in alpha order, but .ads then .adb
  procedure Sort (List : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type);

end Sort;

