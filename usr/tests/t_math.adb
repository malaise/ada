with Text_Io;
with My_Math, My_Io;
procedure T_Math is

  package Real_Io is new Text_Io.Float_Io (My_Math.Real);

  use My_Math, My_Io, Real_Io;

  subtype Real is My_Math.Real;
  R, Rp : Real;

begin
  loop
    loop
      begin
        Put ("Enter a real R1 : ? "); Get (R);
        exit;
      exception
        when others => Skip_Line;
      end;
    end loop;
    begin
      Put ("int   (R1) "); Put( Int  (R)); New_Line;
      Put ("frac  (R1) "); Put( Frac (R)); New_Line;
      Put ("round (R1) "); Put( Round (R)); New_Line;
      Put ("trunc (R1) "); Put( Trunc (R)); New_Line;
      begin
        Put ("sqrt (R1) "); Put( Sqrt (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("exp (R1) "); Put ( Exp(R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("ln  (R1) "); Put( Ln (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("log_10 (R1) "); Put( Log_10 (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      Put_Line ("Angles en radian");
      begin
        Put ("sin (R1) "); Put( Sin (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("cos (R1) "); Put( Cos (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("tg  (R1) "); Put( Tg (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("arc_sin (R1) "); Put( Arc_Sin (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("arc_cos (R1) "); Put( Arc_Cos (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("arc_tg  (R1) "); Put( Arc_Tg (R)); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
      begin
        Put ("Enter another real R2 : ? "); Get (Rp);
        Put (" R1 ** R2 "); Put (R**Rp); New_Line;
      exception
        when others=> Put_Line ("Exception");
      end;
    exception
      when others => Put_Line ("Exception.");
    end;
    New_Line;
  end loop;
end T_Math;
