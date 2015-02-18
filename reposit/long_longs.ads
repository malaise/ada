package Long_Longs is

  subtype Ll_Integer  is Long_Long_Integer;
  subtype Ll_Natural  is Ll_Integer range 0 .. Ll_Integer'Last;
  subtype Ll_Positive is Ll_Integer range 1 .. Ll_Integer'Last;

  type Ll_Mod is mod Ll_Integer'Last + 1;
  for Ll_Mod'Size use Ll_Integer'Size;
  subtype Ll_Mod_Natural  is Ll_Mod range 0 .. Ll_Mod'Last;
  subtype Ll_Mod_Positive is Ll_Mod range 1 .. Ll_Mod'Last;

end Long_Longs;

