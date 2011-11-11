with Basic_Proc, Directory;
procedure T_Dirname is
begin
  Basic_Proc.Put_Line_Output ("Dirname /tools/toto.ads: "
   & Directory.Dirname ("/tools/toto.ads"));
  Basic_Proc.Put_Line_Output ("Dirname toto.ads: "
   & Directory.Dirname ("toto.ads"));
  Basic_Proc.Put_Line_Output ("Dirname /toto.ads: "
   & Directory.Dirname ("/toto.ads"));

  Basic_Proc.Put_Line_Output ("Basename /tools/toto.ads: "
   & Directory.Basename ("/tools/toto.ads"));
  Basic_Proc.Put_Line_Output ("Basename toto.ads: "
   & Directory.Basename ("toto.ads"));
  Basic_Proc.Put_Line_Output ("Basename /toto.ads: "
   & Directory.Basename ("/toto.ads"));
  Basic_Proc.Put_Line_Output ("Basename /tools/toto.ads .ads: "
   & Directory.Basename ("/tools/toto.ads", ".ads"));
  Basic_Proc.Put_Line_Output ("Basename /tools/toto.a .ad: "
   & Directory.Basename ("/tools/toto.a", ".ad"));

  Basic_Proc.Put_Line_Output ("File_Prefix /tools/toto.ads: "
    & Directory.File_Prefix ("/tools/toto.ads"));
  Basic_Proc.Put_Line_Output ("File_Suffix /tools/toto.ads: "
    & Directory.File_Suffix ("/tools/toto.ads"));

  Basic_Proc.Put_Line_Output (
      "Normalize /root/dir1/../dirok/./dir3/../toto.ads: "
    & Directory.Normalize_Path ("/root/dir1/../dirok/./dir3/../toto.ads"));
  Basic_Proc.Put_Line_Output ("Make_Full """": "
    & Directory.Make_Full_Path (""));
  Basic_Proc.Put_Line_Output ("Make_Full ""."": "
    & Directory.Make_Full_Path ("."));
  Basic_Proc.Put_Line_Output ("Make_Full dir1/../dirok/./dir3/../toto.ads: "
    & Directory.Make_Full_Path ("dir1/../dirok/./dir3/../toto.ads"));

end T_Dirname;

