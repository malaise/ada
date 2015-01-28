with Argument_Parser, Trilean;
with Entities;
package Targets is

  -- List according to selection criteria
  -- Return True if a matching file dir was found
  function List (Dots : Entities.Dots_Kind_List;
                 Recursive : Boolean;
                 Depth : Natural;
                 Merge : Boolean;
                 Skip_Dirs : Boolean;
                 Put_Dir_Names : Trilean.Trilean;
                 Follow_Links : Boolean;
                 Args : Argument_Parser.Parsed_Dscr) return Boolean;

end Targets;

