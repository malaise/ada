with Bit_Ops;

package body Forker is

  Stop_Mask : constant Integer := 16#7F#;  

  procedure Decode_Exit
     (Status : in Integer;
      Cause  : out Exit_Cause_List;
      Code   : out Natural) is
    use Bit_Ops;
  begin
    if (Status and Stop_Mask) = Stop_Mask then
      Cause := Stop;
      Code  := Shr (Status, 8) and Stop_Mask;
    elsif (Status and Stop_Mask) = 0 then
      Cause := Normal;
      Code  := Shr (Status, 8) and 16#FF#;
    else
      Cause := Signal;
      Code  := Status and Stop_Mask;
    end if;
  end Decode_Exit;

end Forker;

