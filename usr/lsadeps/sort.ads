with As.U.Utils;
package Sort is

  -- Add this path to list with decrescent prio
  -- So that the order of '-I' and -R dirs is preserved in the output
  procedure Add_Path (Path : As.U.Asu_Us);

  -- Sort then get all the paths
  function Get_Paths return As.U.Utils.Asu_Ua.Unb_Array;


  -- Sort entries ([<path>/]<file>)
  --  First the entries without path
  --  Then in order of prio
  --  Then the entries without prio
  --  At each level in alpha order, but .ads then .adb
  procedure Sort (List : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type);

end Sort;

