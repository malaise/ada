package Long_Longs is

  subtype Ll_Integer  is Long_Long_Integer;
  subtype Ll_Natural  is Ll_Integer range 0 .. Ll_Integer'Last;
  subtype Ll_Positive is Ll_Integer range 1 .. Ll_Integer'Last;

end Long_Longs;

