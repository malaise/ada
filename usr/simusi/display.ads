with Common, Data;
package Display is

  procedure Print_Tittle (Kind : in Common.Cote_Kind);

  -- The way for a cote
  type Way_Vector is array (Data.Eff_Line_Range range <>)
                     of Data.Eff_Line_Range;

  procedure Print (Kind : in Common.Cote_Kind;
                   Cote : in Data.Eff_Cote_Range;
                   Way  : in Way_Vector);

  procedure Put_No_Way (Kind : in Common.Cote_Kind;
                        Cote : in Data.Eff_Cote_Range);

  procedure Print_Result;

end Display;

