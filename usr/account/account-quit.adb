with Screen;
separate (Account)
procedure Quit is
begin
  if Mng.Is_Saved or else Screen.Confirm_Action(Screen.Quit_Unsaved) then 
    raise Quit_Program;
  end if;
end Quit;

