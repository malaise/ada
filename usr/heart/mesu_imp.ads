with Mesu_Def;
package Mesu_Imp is

  -- Select file, then:
  -- Import Date, and Samples according to Sampling_Delta, from .tcx file
  -- Import all Samples from .txt file
  procedure Import (Mesure : in out Mesu_Def.Mesure_Rec;
                    Ok : out Boolean);
end Mesu_Imp;

