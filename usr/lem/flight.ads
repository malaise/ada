with Space;
package Flight is

  -- Get a valid init position for the LEM
  function Get_Init_Position return Space.Position_Rec;

  -- Kind of LEM flight status
  type Status_List is (Flying, Landed, Crashed, Lost);
  -- LEM status and pos when landed
  type Status_Rec (Status : Status_List := Flying) is record
    case Status is
      when Flying | Crashed | Lost => null;
      when Landed =>
        Landed_Pos : Space.Position_Rec;
    end case;
  end record;
  -- get current LEM status
  function Get_Status return Status_Rec;

end Flight;

