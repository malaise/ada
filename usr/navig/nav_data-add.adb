-- When 1 vector is unknown compute it by addition or substraction of
--  the 2 others
with NBRES_C;
separate (NAV_DATA)
function ADD (X, Y : NAV_TYPES.T_VECTOR) return NAV_TYPES.T_VECTOR is
  CX, CY, CR : NBRES_C.COMPLEX;
begin
  CX := NBRES_C.CREATE_COMPLEX(NBRES_C.REAL(X.SPEED),
                               NBRES_C.DEGREE(TO_REAL(X.ANGLE)) );
  CY := NBRES_C.CREATE_COMPLEX(NBRES_C.REAL(Y.SPEED),
                               NBRES_C.DEGREE(TO_REAL(Y.ANGLE)) );
  CR := NBRES_C."+" (CX, CY);

  return (
   SPEED => NAV_TYPES.T_SPEED(NBRES_C.MODULE(CR)),
   ANGLE => TO_ANGLE(REAL(NBRES_C.ANGLE_DEGREE(CR))) );
end ADD;




