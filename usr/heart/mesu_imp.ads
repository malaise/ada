with Mesu_Def;
package Mesu_Imp is

  -- Import Date, and Samples according to Sampling_Delta, from .tcx file
  -- Import all Samples from .txt file
  procedure Import (File_Name : in String;
                    Mesure : in out Mesu_Def.Mesure_Rec;
                    Ok : out Boolean);
end Mesu_Imp;

