with TEXT_IO;
with MY_MATH, MY_IO;
procedure T_MATH is

  package REAL_IO is new TEXT_IO.FLOAT_IO (MY_MATH.REAL);

  use MY_MATH, MY_IO, REAL_IO;

  subtype REAL is MY_MATH.REAL;
  R, RP : REAL;

begin
  loop
    loop
      begin
        PUT ("Enter a real R1 : ? "); GET (R);
        exit;
      exception
        when others => SKIP_LINE;
      end;
    end loop;
    begin
      PUT ("int   (R1) "); PUT( INT  (R)); NEW_LINE;
      PUT ("frac  (R1) "); PUT( FRAC (R)); NEW_LINE;
      PUT ("round (R1) "); PUT( ROUND (R)); NEW_LINE;
      PUT ("trunc (R1) "); PUT( TRUNC (R)); NEW_LINE;
      begin
        PUT ("sqrt (R1) "); PUT( SQRT (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("exp (R1) "); PUT ( EXP(R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("ln  (R1) "); PUT( LN (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("log_10 (R1) "); PUT( LOG_10 (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      PUT_LINE ("Angles en radian");
      begin
        PUT ("sin (R1) "); PUT( SIN (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("cos (R1) "); PUT( COS (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("tg  (R1) "); PUT( TG (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("arc_sin (R1) "); PUT( ARC_SIN (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("arc_cos (R1) "); PUT( ARC_COS (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("arc_tg  (R1) "); PUT( ARC_TG (R)); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
      begin
        PUT ("Enter another real R2 : ? "); GET (RP);
        PUT (" R1 ** R2 "); PUT (R**RP); NEW_LINE;
      exception
        when others=> PUT_LINE ("Exception");
      end;
    exception
      when others => PUT_LINE ("Exception.");
    end;
    NEW_LINE;
  end loop;
end T_MATH;
