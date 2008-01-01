with Argument_Parser;
with Entities;
package Targets is

  -- List according to selection criteria
  procedure List (Dots : in Entities.Dots_Kind_List;
                  Only_Dirs : in Boolean;
                  Date1, Date2 : in Entities.Date_Spec_Rec;
                  Recursive : in Boolean;
                  Merge : in Boolean;
                  Args : in Argument_Parser.Parsed_Dscr);


end Targets;

