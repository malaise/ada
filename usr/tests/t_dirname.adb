with Ada.Text_Io;
with Directory;
procedure T_Dirname is
begin
  Ada.Text_Io.Put_Line ("Dirname /tools/toto.ads: "
   & Directory.Dirname ("/tools/toto.ads"));
  Ada.Text_Io.Put_Line ("Dirname toto.ads: "
   & Directory.Dirname ("toto.ads"));
  Ada.Text_Io.Put_Line ("Dirname /toto.ads: "
   & Directory.Dirname ("/toto.ads"));

  Ada.Text_Io.Put_Line ("Basename /tools/toto.ads: "
   & Directory.Basename ("/tools/toto.ads"));
  Ada.Text_Io.Put_Line ("Basename toto.ads: "
   & Directory.Basename ("toto.ads"));
  Ada.Text_Io.Put_Line ("Basename /toto.ads: "
   & Directory.Basename ("/toto.ads"));
  Ada.Text_Io.Put_Line ("Basename /tools/toto.ads .ads: "
   & Directory.Basename ("/tools/toto.ads", ".ads"));
  Ada.Text_Io.Put_Line ("Basename /tools/toto.a .ad: "
   & Directory.Basename ("/tools/toto.a", ".ad"));

  Ada.Text_Io.Put_Line ("File_Prefix /tools/toto.ads: "
    & Directory.File_Prefix ("/tools/toto.ads"));
  Ada.Text_Io.Put_Line ("File_Suffix /tools/toto.ads: "
    & Directory.File_Suffix ("/tools/toto.ads"));

  Ada.Text_Io.Put_Line ("Normalize /root/dir1/../dirok/./dir3/../toto.ads: "
    & Directory.Normalize_Path ("/root/dir1/../dirok/./dir3/../toto.ads"));
  Ada.Text_Io.Put_Line ("Make_Full """": "
    & Directory.Make_Full_Path (""));
  Ada.Text_Io.Put_Line ("Make_Full ""."": "
    & Directory.Make_Full_Path ("."));
  Ada.Text_Io.Put_Line ("Make_Full dir1/../dirok/./dir3/../toto.ads: "
    & Directory.Make_Full_Path ("dir1/../dirok/./dir3/../toto.ads"));

end T_Dirname;

