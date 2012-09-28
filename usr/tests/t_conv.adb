with Basic_Proc, C_Types, Convert, Hexa_Utils, Upper_Str;
procedure T_Conv is

  I : C_Types.Uint32;

  procedure Put (N : C_Types.Uint32) is
  begin
    Basic_Proc.Put_Output ("16#" & Upper_Str (Hexa_Utils.Image (Natural(N)))
                         & "#");
  end Put;

begin

  I := 16#12345678#;
  Basic_Proc.Put_Output ("Host:       "); Put (I); Basic_Proc.New_Line_Output;
  I := Convert.Hton (I);
  Basic_Proc.Put_Output ("to Network: "); Put (I); Basic_Proc.New_Line_Output;
  I := Convert.Ntoh (I);
  Basic_Proc.Put_Output ("to Host:    "); Put (I); Basic_Proc.New_Line_Output;

end T_Conv;

