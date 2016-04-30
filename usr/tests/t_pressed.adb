with Basic_Proc, Key_Pressed, Hexa_Utils;
procedure T_Pressed is
  Char : Character;
begin

  Key_Pressed.Open (True);

  loop
    Char := Key_Pressed.Get_Key;
    Basic_Proc.Put_Line_Output (
          Hexa_Utils.Image (Integer'(Character'Pos (Char)))
        & " ->" & Char & "<");
  end loop;

end T_Pressed;

