with My_Math, Basic_Proc, Images;
procedure T_Math is

  function Inte_Image is new Images.Int_Image (My_Math.Inte);
  function Real_Image is new Images.Flo_Image (My_Math.Real);

  use My_Math, Basic_Proc;
  procedure Put (I : in My_Math.Inte) is
  begin
    Put_Output (Inte_Image (I));
  end Put;
  procedure Put (R : in My_Math.Real) is
  begin
    Put_Output (Real_Image (R));
  end Put;

  subtype Real is My_Math.Real;
  R, Rp : Real;

begin
  loop
    loop
      begin
        Put_Output ("Enter a real R1 : ? "); R := Get (Get_Line);
        exit;
      exception
        when Basic_Proc.End_Error => raise;
        when others => Skip_Line;
      end;
    end loop;
    begin
      Put_Output ("int   (R1) "); Put( Int  (R)); New_Line_Output;
      Put_Output ("frac  (R1) "); Put( Frac (R)); New_Line_Output;
      Put_Output ("round (R1) "); Put( Round (R)); New_Line_Output;
      Put_Output ("trunc (R1) "); Put( Trunc (R)); New_Line_Output;
      begin
        Put_Output ("sqrt (R1) "); Put( Sqrt (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("exp (R1) "); Put ( Exp(R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("ln  (R1) "); Put( Ln (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("lg  (R1) "); Put( Lg (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      Put_Line_Output ("Angles in radian");
      begin
        Put_Output ("sin (R1) "); Put( Sin (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("cos (R1) "); Put( Cos (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("tan (R1) "); Put( Tan (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("arc_sin (R1) "); Put( Arc_Sin (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("arc_cos (R1) "); Put( Arc_Cos (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("arc_tan (R1) "); Put( Arc_Tan (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("Enter another real R2 : ? "); Rp := Get (Get_Line);
        Put_Output (" R1 ** R2 "); Put (R**Rp); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
    exception
      when others => Put_Line_Output ("Exception.");
    end;
    New_Line_Output;
  end loop;
exception
  when Basic_Proc.End_Error =>
    null;
end T_Math;

