with My_Io, Convert;
procedure T_Conv is

  I : Integer;

begin

  I := 16#12345678#;
  My_Io.Put ("Host:       "); My_Io.Put (I, Base => 16); My_Io.New_Line;
  I := Convert.Hton (I);
  My_Io.Put ("to Network: "); My_Io.Put (I, Base => 16); My_Io.New_Line;
  I := Convert.Ntoh (I);
  My_Io.Put ("to Host:    "); My_Io.Put (I, Base => 16); My_Io.New_Line;

end T_Conv;

