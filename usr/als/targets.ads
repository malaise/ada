with Argument_Parser;
with Entities;
package Targets is

  -- List according to selection criteria
  procedure List (Dots : in Entities.Dots_Kind_List;
                  Recursive : in Boolean;
                  Merge : in Boolean;
                  Args : in Argument_Parser.Parsed_Dscr);


end Targets;

