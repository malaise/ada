with Pers_Def;
package Pers_Mng is

  -- All these calls make no assumption about the state of the list

  -- The Search calls affect the current position in list (move to this
  --  person if found, random otherwise)
  -- They return 0 if the person is not found

  -- Search a person knowing its pid
  procedure Search (List : in out Pers_Def.Person_List;
                    Pid  : in Pers_Def.Pid_Range;
                    Pos  : out Natural);

  -- Search a person knowing its name and activity
  procedure Search (List     : in out Pers_Def.Person_List;
                    Name     : in Pers_Def.Person_Name_Str;
                    Activity : in Pers_Def.Person_Activity_Str;
                    Pos      : out Natural);

  -- The Expand call affects the current position in list
  -- Name must not be empty otherwise nothing is expanded
  -- Activity can be empty or partial
  -- If Activity is empty:
  --   If Name allows only one and only one name expansion then
  --     it is expanded if necessary and Pos is set to 0
  --   Else
  --     Pos is set to -1 (not found or not one possible expansion)
  -- Else (Activity is set)
  --   If Name and Activity allow one and only one expansion, then
  --     Name is expanded if necessary and Pos is set to its position
  --   Else
  --     Pos is set to -1
  procedure Expand (List     : in out Pers_Def.Person_List;
                    Name     : in out Pers_Def.Person_Name_Str;
                    Activity : in out Pers_Def.Person_Activity_Str;
                    Pos      : out Integer);

  -- The Select calls affect the order and current position in list
  --  (position set to first if found)
  -- They return 0, if no person found

  -- Get first and last index (in list) of persons with the provided name
  procedure Select_By_Name (List : in out Pers_Def.Person_List;
                            Name : in Pers_Def.Person_Name_Str;
                            First, Last : out Natural);

  -- Get first and last index (in list) of persons with the provided activity
  procedure Select_By_Activity (List : in out Pers_Def.Person_List;
                                Activity : in Pers_Def.Person_Activity_Str;
                                First, Last : out Natural);

  -- The Insert call affects the order and current position in list
  -- Current pos in list becomes this person's one
  -- The person's pid is not significant in the in value,
  --  and set in the out value
  -- It may raise Not_Sole_Error if the (Name, Activity) already exists in list
  --  or raise List_Full_Error if no more Pid available

  -- Insert a new person in the list. (Its Name+Activity must be sole)
  procedure Insert (List : in out Pers_Def.Person_List;
                    Person : in out Pers_Def.Person_Rec);

  Not_Sole_Error : exception;
  List_Full_Error : exception;

end Pers_Mng;

