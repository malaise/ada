with Screen;
separate (Account)
procedure Quit is
begin
  if not Mng.Is_Saved then
    Mng.Save (Mng.Cancel);
  end if;
  if Mng.Is_Saved or else Screen.Confirm_Action(Screen.Quit_Unsaved) then 
    raise Quit_Program;
  end if;
end Quit;

