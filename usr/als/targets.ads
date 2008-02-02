with Argument_Parser;
with Entities;
package Targets is

  -- List according to selection criteria
  -- Return True is a matching file dir was found
  function List (Dots : Entities.Dots_Kind_List;
                 Recursive : Boolean;
                 Merge : Boolean;
                 Args : Argument_Parser.Parsed_Dscr) return Boolean;


end Targets;

