with My_Io, C_Types, Convert;
procedure T_Conv is

  I : C_Types.Uint32;

  procedure Put (N : C_Types.Uint32) is
  begin
    My_Io.Put (Integer(N), Base => 16);
  end Put;

begin

  I := 16#12345678#;
  My_Io.Put ("Host:       "); Put (I); My_Io.New_Line;
  I := Convert.Hton (I);
  My_Io.Put ("to Network: "); Put (I); My_Io.New_Line;
  I := Convert.Ntoh (I);
  My_Io.Put ("to Host:    "); Put (I); My_Io.New_Line;

end T_Conv;

