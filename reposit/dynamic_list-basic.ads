generic
package Dynamic_List.Basic is

  -- The following procedures implement usefull basic services
  -- Most of them avoid raisiing Empty_List ot Not_In_List

  -- Move to first element of list
  procedure Rewind (List : in out List_Type);

  -- Delete current item in list, moving to previous if possible
  procedure Delete_Current (List : in out List_Type);

  -- Read current item and moves to next if possible
  -- May raise Empty_List
  procedure Read_Move (List        : in out List_Type;
                       Item        : out Element_Type;
                       End_Of_List : out Boolean);



  -- Find first occurence from first of matching Item
  -- Return True if found, False otherwise
  generic
    with procedure Search (List         : in out List_Type;
                           Item         : in Element_Type;
                           Where        : in Direction := Next;
                           Occurence    : in Positive := 1;
                           From_Current : in Boolean := True);
  procedure Find_First (List  : in out List_Type;
                        Item  : in Element_Type;
                        Found : out Boolean);

end Dynamic_List.Basic;

