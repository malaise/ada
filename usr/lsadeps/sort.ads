with As.U.Utils; use As.U, As.U.Utils;
package Sort is

  -- Set the priority level of a path (1 = Higest)
  procedure Set_Prio (Path : Asu_Us; Prio : Positive);

  -- Sort entries ([<path>/]<file>)
  --  First the entries without path
  --  Then in order of prio
  --  Then the entries without prio
  --  At each level in alpha order, but .ads then .adb
  procedure Sort (List : in out Asu_Dyn_List_Mng.List_Type);

end Sort;

