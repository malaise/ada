with Ada.Io_Exceptions;
with My_Math, Basic_Proc;
procedure T_Math is

  use My_Math, Basic_Proc, My_Math.Real_Io, My_Math.Inte_Io;

  subtype Real is My_Math.Real;
  R, Rp : Real;

begin
  loop
    loop
      begin
        Put_Output ("Enter a real R1 : ? "); Get (R);
        exit;
      exception
        when Ada.Io_Exceptions.End_Error => raise;
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
        Put_Output ("log_10 (R1) "); Put( Log_10 (R)); New_Line_Output;
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
        Put_Output ("tg  (R1) "); Put( Tg (R)); New_Line_Output;
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
        Put_Output ("arc_tg  (R1) "); Put( Arc_Tg (R)); New_Line_Output;
      exception
        when others=> Put_Line_Output ("Exception");
      end;
      begin
        Put_Output ("Enter another real R2 : ? "); Get (Rp);
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
  when Ada.Io_Exceptions.End_Error =>
    null;
end T_Math;

