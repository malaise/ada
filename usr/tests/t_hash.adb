with As.B, Console, Upper_Str, Normal, Hashing, Basic_Proc;
procedure T_Hash is

  subtype Data_Access is Positive;
  procedure Dump (I : in Data_Access);

  package Hash is new Hashing.Sized_Hash (512);
  package My_Hash is new Hash.Hash_Mng (Data_Access, Dump);
  Ht : My_Hash.Hash_Table;

  subtype Txt_P is As.B.Asb_Bs(500);
  Txt : Txt_P;
  Input : String (1 .. Txt.Max);
  Len : Natural;
  I : Data_Access := 1;

  Found : My_Hash.Found_Rec;

  function Str (Txt : Txt_P) return String is
  begin
    return Txt.Slice (3, Txt.Length);
  exception
    when others =>
      return "";
  end Str;

  function Image (I : Positive) return String is
  begin
    return Normal (I, 3, Gap => '0');
  end Image;

  procedure Dump (I : in Data_Access) is
  begin
    Basic_Proc.Put_Output(Image(I));
  end Dump;

begin

  loop
    Basic_Proc.New_Line_Output;

    Basic_Proc.Put_Output ("Store <>, Zreset <>, Find <>, Reread <>, Delete <>, dumP <>, Clear all, EXIT ? ");
    Basic_Proc.Get_Line (Input, Len);
    Txt.Set (Input(1 .. Len));
    if Txt.Length >= 3 and then Txt.Element (2) = ' ' then
      case Txt.Element (1) is
        when 'S' | 's' =>
          My_Hash.Store (Ht, Str(Txt), I);
          Basic_Proc.Put_Line_Output (Image(I) & " stored with key >" & Str(Txt) & "<.");
          I := I + 1;
        when 'Z' | 'z' =>
          My_Hash.Reset_Find (Ht, Str(Txt));
          Basic_Proc.Put_Line_Output ("Search reset for key >" & Str(Txt) & "<.");
        when 'F' | 'f' =>
          My_Hash.Find_Next (Ht, Str(Txt), Found);
          if Found.Found then
            Basic_Proc.Put_Line_Output ("Found " & Image(Found.Data) & " with key >" & Str(Txt) & "<.");
          else
            Basic_Proc.Put_Line_Output ("No data found for key >" & Str(Txt) & "<.");
          end if;
        when 'R' | 'r' =>
          My_Hash.Re_Read (Ht, Str(Txt), Found);
          if Found.Found then
            Basic_Proc.Put_Line_Output ("Re-read " & Image(Found.Data) & " with key >" & Str(Txt) & "<.");
          else
            Basic_Proc.Put_Line_Output ("No data re-read for key >" & Str(Txt) & "<.");
          end if;
        when 'D'| 'd' =>
          begin
            My_Hash.Remove (Ht, Str(Txt));
            Basic_Proc.Put_Line_Output ("Current data for key >" & Str(Txt) & "< deleted.");
          exception
            when Hash.Not_Found =>
              Basic_Proc.Put_Line_Output ("Exception NOT_FOUND raised when deleting data for key >"
                             & Str(Txt) & "<.");
          end;
        when 'P' | 'p' =>
          Basic_Proc.Put_Line_Output ("Dumping data for key >" & Str(Txt) & "<:");
          My_Hash.Dump(Ht, Str(Txt));
        when others =>
          Console.Sound;
      end case;
    elsif Upper_Str (Txt.Image) = "C" then
      My_Hash.Clear_All (Ht);
      Basic_Proc.Put_Line_Output ("Storage cleared.");
    elsif Upper_Str (Txt.Image) = "EXIT" then
      exit;
    else
      Console.Sound;
    end if;

  end loop;

  My_Hash.Clear_All (Ht);

end T_Hash;

