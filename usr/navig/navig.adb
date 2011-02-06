with Nav_Screen, Nav_Data, Nav_Dialog;
procedure Navig is
  Data, Result : Nav_Data.T_Data;
  Action : Nav_Dialog.Action;
  Status : Nav_Data.T_Consistency;
begin
  Nav_Dialog.Init;
  loop

    -- get data and the action
    Nav_Dialog.Get (Data, Action);
    if Nav_Screen."=" (Action, Nav_Screen.Quit) then
      return;
    end if;

    -- compute
    Nav_Data.Resolution (Data, Status, Result);

    if Nav_Data."=" (Status, Nav_Data.Ok) then
      -- put the result
      Nav_Dialog.Put (Result);
    else
      -- put consistency error message during a while
      Nav_Dialog.Put (Status);
    end if;

  end loop;

end Navig;
