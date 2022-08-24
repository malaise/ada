with Mesu_Def;
package Mesu_Imp is

  -- Select file, then:
  -- For TCX file: Import Date, and Samples according to Sampling_Delta
  -- For TXT file: Import all Samples
  procedure Import (Mesure   : in out Mesu_Def.Mesure_Rec;
                    Ok       : out Boolean;
                    Date_Set : out Boolean);
end Mesu_Imp;

