-- When 1 vector is unknown compute it by addition or substraction of
--  the 2 others
with C_NBRES;
separate (NAV_DATA)
function ADD (X, Y : NAV_TYPES.T_VECTOR) return NAV_TYPES.T_VECTOR is
  CX, CY, CR : C_NBRES.COMPLEX;
begin
  CX := C_NBRES.CREATE_COMPLEX(C_NBRES.REAL(X.SPEED),
                               C_NBRES.DEGREE(TO_REAL(X.ANGLE)) );
  CY := C_NBRES.CREATE_COMPLEX(C_NBRES.REAL(Y.SPEED),
                               C_NBRES.DEGREE(TO_REAL(Y.ANGLE)) );
  CR := C_NBRES."+" (CX, CY);

  return (
   SPEED => NAV_TYPES.T_SPEED(C_NBRES.MODULE(CR)),
   ANGLE => TO_ANGLE(REAL(C_NBRES.ANGLE_DEGREE(CR))) );
end ADD;

