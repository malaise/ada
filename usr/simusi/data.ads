with Common, File;
pragma Elaborate(File);
package Data is
  Nb_Cote : constant Common.Cote_Range := File.Get_Nb_Cote;

  Manufas : constant Common.Manufa_Array := File.Get_Manufa;
  Designs : constant Common.Design_Array := File.Get_Design;

  subtype Eff_Cote_Range is Common.Cote_Range range 1 .. Nb_Cote;
  subtype Eff_Line_Range is Common.Line_Range range 1 .. Nb_Cote + 1;

end Data;

