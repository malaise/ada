with Con_Io;
with My_Io;
procedure T_Key is

  Key : Natural;
  Char : Boolean;
  Ctrl, Shift : Boolean;

begin

  Con_Io.Init;
  Con_Io.Reset_Term;
  Key := 0;
  Char := True;
  Ctrl := False;
  Shift := False;
  loop
    if Key = 0 and then Char and then not Ctrl and then Not Shift then
      -- refresh
      Con_Io.Move;
      Con_Io.Put_Line ("Exit with Ctrl C");
    end if;
    Con_Io.Get_Key (Key, Char, Ctrl, Shift);
    My_Io.Put (Key, Base => 16, Width => 6);

    if Char then
      My_Io.Put (" CHAR ");
      if Key = 8 then
        My_Io.Put ("Backspace");
      elsif Key in
       Character'Pos(Character'First) .. Character'Pos(Character'Last) then
        My_Io.Put ('>' & Character'Val(Key) & '<');
      else
        My_Io.Put (" non ADA character");
      end if;
    end if;
    My_Io.Put (" " & Boolean'Image(Ctrl) & " " & Boolean'Image(Shift));
    My_Io.New_Line;
    -- Ctrl C in Window
    exit when Char and then Key = Character'Pos('c')
     and then Ctrl and then not Shift;
    -- SigInt C in Xterm
    exit when Char and then Key = 3
     and then not Ctrl and then not Shift;
  end loop;
end T_Key;

