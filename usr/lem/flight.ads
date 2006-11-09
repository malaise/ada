with Space;
package Flight is

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

  function Get_Status return Status_Rec;

end Flight;

