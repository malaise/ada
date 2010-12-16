with Console;
with Upper_Str;
with Normal;
with Hashing;
with My_Io;
with Text_Handler;
procedure T_Hash is

  subtype Data_Access is Positive;
  procedure Dump (I : in Data_Access);

  package Hash is new Hashing.Sized_Hash (512);
  package My_Hash is new Hash.Hash_Mng (Data_Access, Dump);
  Ht : My_Hash.Hash_Table;

  subtype Txt_P is Text_Handler.Text(500);
  Txt : Txt_P;
  Input : String (1 .. Txt.Max_Len);
  Len : Natural;
  I : Data_Access := 1;

  Found : My_Hash.Found_Rec;

  function Str (Txt : Txt_P) return String is
  begin
    return Txt.Value(3 .. Txt.Length);
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
    My_Io.Put(Image(I));
  end Dump;


begin

  loop
    My_Io.New_Line;

    My_Io.Put ("Store <>, Zreset <>, Find <>, Remove <>, Dump <>, Clear all, EXIT ? ");
    My_Io.Get_Line (Input, Len);
    Txt.Set (Input(1 .. Len));
    if Txt.Length >= 3 and then Txt.Value(2) = ' ' then
      case Txt.Value(1) is
        when 'S' | 's' =>
          My_Hash.Store (Ht, Str(Txt), I);
          My_Io.Put_Line (Image(I) & " stored with key >" & Str(Txt) & "<.");
          I := I + 1;
        when 'Z' | 'z' =>
          My_Hash.Reset_Find (Ht, Str(Txt));
          My_Io.Put_Line ("Search reset for key >" & Str(Txt) & "<.");
        when 'F' | 'f' =>
          My_Hash.Find_Next (Ht, Str(Txt), Found);
          if Found.Found then
            My_Io.Put_Line ("Found " & Image(Found.Data) & " with key >" & Str(Txt) & "<.");
          else
            My_Io.Put_Line ("No data found for key >" & Str(Txt) & "<.");
          end if;
        when 'R'| 'r' =>
          begin
            My_Hash.Remove (Ht, Str(Txt));
            My_Io.Put_Line ("Current data for key >" & Str(Txt) & "< removed.");
          exception
            when Hash.Not_Found =>
              My_Io.Put_Line ("Exception NOT_FOUND raised when removing data for key >"
                             & Str(Txt) & "<.");
          end;
        when 'D' | 'd' =>
          My_Io.Put_Line ("Dumping data for key >" & Str(Txt) & "<:");
          My_Hash.Dump(Ht, Str(Txt));
        when others =>
          Console.Sound;
      end case;
    elsif Upper_Str (Txt.Value) = "C" then
      My_Hash.Clear_All (Ht);
      My_Io.Put_Line ("Storage cleared.");
    elsif Upper_Str (Txt.Value) = "EXIT" then
      exit;
    else
      Console.Sound;
    end if;

  end loop;

  My_Hash.Clear_All (Ht);

end T_Hash;

