with Ada.Calendar, Ada.Text_Io;
with Directory, Sys_Calls, Bit_Ops, Normal, Int_Image, Date_Image, Upper_Str,
     Environ;
package body Output is

  -- Max amount of entries to sort
  Env_Max_To_Sort : constant String := "ALS_MAX_TO_SORT";
  Max_To_Sort : Natural := 5_000;

  -- Asu
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Tus (Source : in String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Ts (Source : in Asu_Us) return String renames Asu.To_String;

  -- Images
  function Nat_Image is new Int_Image (Natural);
  function Size_Image is new Int_Image (Sys_Calls.Size_T);
  function Total_Image is new Int_Image (Lister.Size_Type);

  -- Stored style
  Sort_Kind : Sort_Kind_List;
  Revert : Boolean;
  Format_Kind : Format_Kind_List;
  Put_Path : Boolean;
  Full_Path : Boolean;
  Separator : Asu.Unbounded_String;
  Classify : Boolean;
  Default_Separator : constant String := "  ";

  -- Current directory path
  Curdir : constant Asu_Us := Asu_Tus (Directory.Get_Current);

  -- Set (store) sorting and format style
  procedure Set_Style (
             Sort_Kind   : in Sort_Kind_List;
             Revert      : in Boolean;
             Format_Kind : in Format_Kind_List;
             Put_Path    : in Boolean;
             Full_Path   : in Boolean;
             Classify    : in Boolean;
             Separator   : in Ada.Strings.Unbounded.Unbounded_String) is
  begin
    Output.Sort_Kind := Sort_Kind;
    Output.Revert := Revert;
    Output.Format_Kind := Format_Kind;
    Output.Put_Path := Put_Path;
    Output.Full_Path := Full_Path;
    Output.Separator := Separator;
    Output.Classify := Classify;
    Environ.Get_Nat (Env_Max_To_Sort, Max_To_Sort);
  end Set_Style;

  -- Get full path of a path
  function Make_Full_Path (Path : String) return String is
  begin
    if Path = "" then
      return Directory.Normalize_Path (Asu_Ts (Curdir));
    elsif Path(Path'First) = '/' then
      -- Path is already absolute => Normalize
      return Directory. Normalize_Path (Path);
    else
      -- Path is relative, prepend current path & Normalize
      return Directory.Normalize_Path (Asu_Ts (Curdir) & "/" & Path);
    end if;
  end Make_Full_Path;

  -- Sorting function for 2 paths
  function "<" (S1, S2 : Asu_Us) return Boolean is
    Len : Natural;
    C1, C2 : Character;
  begin
    if Asu.Length (S1) <= Asu.Length (S2) then
      Len := Asu.Length (S1);
    else
      Len := Asu.Length (S2);
    end if;
    for I in 1 .. Len loop
      C1 := Asu.Element (S1, I);
      C2 := Asu.Element (S2, I);
      if C1 /= C2 then
        if C1 = '/' or else C1 = '.' then
          return True;
        elsif C2 = '/' or else C2 = '.' then
          return False;
        else
          return C1 < C2;
        end if;
      end if;
    end loop;
    return Asu.Length (S1) < Asu.Length (S2);
  end "<";

  -- Sorting function for 2 entities according to output format
  function Less_Than (El1, El2 : Entities.Entity) return Boolean is
    use type Ada.Calendar.Time, Asu_Us;
    use type Sys_Calls.Size_T;
    C1, C2 : Entities.Entity;
  begin
    if not Revert then
      C1 := El1;
      C2 := El2;
    else
      C1 := El2;
      C2 := El1;
    end if;
    -- Sort including full path if required
    if Full_Path then
      C1.Name := Asu.To_Unbounded_String (
        Directory.Build_File_Name (
           Make_Full_Path (Asu_Ts (C1.Path)),
           Asu_Ts (C1.Name), "") );
      C2.Name := Asu.To_Unbounded_String (
        Directory.Build_File_Name (
           Make_Full_Path (Asu_Ts (C2.Path)),
           Asu_Ts (C2.Name), "") );
    elsif Put_Path then
      C1.Name := Asu.To_Unbounded_String (
        Directory.Build_File_Name (
           Asu_Ts (C1.Path),
           Asu_Ts (C1.Name), "") );
      C2.Name := Asu.To_Unbounded_String (
        Directory.Build_File_Name (
           Asu_Ts (C2.Path),
           Asu_Ts (C2.Name), "") );
    end if;
    -- Alpha sort case insensitive, and letters before ponctuation
    C1.Name := Asu_Tus (Upper_Str (Asu_Ts (C1.Name)));
    C2.Name := Asu_Tus (Upper_Str (Asu_Ts (C2.Name)));
    case Sort_Kind is
      when None =>
        return True;
      when Alpha =>
        -- Name then Time then Size
        if C1.Name /= C2.Name then
          return C1.Name < C2.Name;
        elsif C1.Modif_Time /= C2.Modif_Time then
          return  C1.Modif_Time > C2.Modif_Time;
        else
          return C1.Size > C2.Size;
        end if;
      when Time =>
        -- Time then Size then Name
        if C1.Modif_Time /= C2.Modif_Time then
          return C1.Modif_Time > C2.Modif_Time;
        elsif C1.Size /= C2.Size then
          return C1.Size > C2.Size;
        else
          return C1.Name < C2.Name;
        end if;
      when Size =>
        -- Size then Time then Name
        if C1.Size /= C2.Size then
          return C1.Size > C2.Size;
        elsif C1.Modif_Time /= C2.Modif_Time then
          return  C1.Modif_Time > C2.Modif_Time;
        else
          return C1.Name < C2.Name;
        end if;
    end case;
  end Less_Than;

  procedure Sort is new Entities.Entity_List_Mng.Sort (Less_Than);

  -- Is separator explicitely set (or is it the default)
  function Separator_Set return Boolean is
    use type Asu.Unbounded_String;
  begin
    return Separator /= Asu.Null_Unbounded_String;
  end Separator_Set;

  -- Return current separator
  function Get_Separator return String is
  begin
    if Separator_Set then
      return Asu_Ts (Separator);
    else
      return Default_Separator;
    end if;
  end Get_Separator;

  -- Split a size in X.y thousands of it
  Kilo : constant Lister.Size_Type := 1024;
  procedure Split (Size : in Lister.Size_Type;
                   Int  : out Lister.Size_Type;
                   Frac : out Lister.Size_Type) is
    Tmp : Lister.Size_Type;
  begin
    -- Integer part
    Int := Size / Kilo;
    -- Thousands in base 10 -> xyz
    Tmp := ((Size rem Kilo) * 1000) / Kilo;
    -- "Round" at hundredths: x.yz, set x+1 if yz /= 0
    Frac := Tmp / 100;
    if Tmp rem 100 /= 0 then
      Frac := Frac + 1;
    end if;
    -- Propagate carry
    if Frac >= 10 then
      Int := Int + 1;
      Frac := 0;
    end if;
  end Split;

  -- First entry, and first entry after we put a name; don't need
  --  separator
  First_Entry : Boolean := True;

  -- Put an entity name (possibly with full path)
  function Name_Image (Entity : in Entities.Entity) return String is
  begin
   First_Entry := True;
   if Full_Path then
      return Directory.Build_File_Name (
           Make_Full_Path (Asu_Ts (Entity.Path)),
           Asu_Ts (Entity.Name), "");
    elsif Put_Path then
      return Directory.Build_File_Name (
           Asu_Ts (Entity.Path),
           Asu_Ts (Entity.Name), "");
    else
      return Asu_Ts (Entity.Name);
    end if;
  end Name_Image;

  -- Put an entity in with separator
  procedure Put_Raw (Entity : in Entities.Entity) is
  begin
    if not First_Entry then
      Ada.Text_Io.Put (Get_Separator);
    end if;
    Ada.Text_Io.Put (Name_Image (Entity));
    First_Entry := False;
  end Put_Raw;

  -- Char associated to Classify option
  Default_Char : constant Character := ' ';
  function Char_Of (Kind : Directory.File_Kind_List;
                    Rights : Natural) return Character is
    use Directory, Bit_Ops;
  begin
    case Kind is
      when File =>
        -- Executable file?
        if (Rights and Shl (1, 6)) /= 0
        or else (Rights and Shl (1, 3)) /= 0
        or else (Rights and Shl (1, 0)) /= 0 then
          return '*';
        else
          return Default_Char;
        end if;
      when Unknown => return Default_Char;
      when Character_Device | Block_Device => return Default_Char;
      when Socket => return Default_Char;
      when Dir => return '/';
      when Link => return '@';
      when Pipe => return '|';
    end case;
  end Char_Of;

  -- Put an entity in normal mode
  Max_Col : constant := 80;
  Current_Col : Natural := 0;
  procedure Put_Simple (Entity : in Entities.Entity) is
    Image : constant String := Name_Image (Entity);
    Name_Len : constant Natural := Image'Length;
    Len : Natural := Name_Len;
    Char : Character := Default_Char;
  begin
    -- Increase Len if Classify character is appended
    if Classify then
      Char := Char_Of (Entity.Kind, Entity.Rights);
      if Char /= Default_Char then
        Len := Len + 1;
      end if;
    end if;
    -- Check if need to New_Line or Space
    if Current_Col /= 0 then
      if Current_Col + Default_Separator'Length + Len > Max_Col then
        Ada.Text_Io.New_Line;
        Current_Col := 0;
      else
        Ada.Text_Io.Put (Default_Separator);
        Current_Col := Current_Col + Default_Separator'Length;
      end if;
    end if;
    Ada.Text_Io.Put (Image);
    if Char /= Default_Char then
      Ada.Text_Io.Put (Char);
    end if;
    Current_Col := Current_Col + Len;
  end Put_Simple;

  -- Put an entity in one row
  procedure Put_One_Row (Entity : in Entities.Entity) is
  begin
    Ada.Text_Io.Put (Name_Image (Entity));
    if Classify
    and then Char_Of (Entity.Kind, Entity.Rights) /= Default_Char then
      Ada.Text_Io.Put (Char_Of (Entity.Kind, Entity.Rights));
    end if;
    Ada.Text_Io.New_Line;
  end Put_One_Row;

  -- Put an entity in full mode
  procedure Put_Long (Entity : in Entities.Entity;
                      Human : in Boolean) is
   Suid, Sgid, Tbit : Boolean;
   Date : String (1 .. 23);
   -- Max length of fields user, group and size, for padding
   Max_Name_Len : constant := 8;
   Max_Group_Len : constant := 6;
   Max_Size_Len : constant := 9;
   use Bit_Ops;
   use type Directory.File_Kind_List;
   function Id_Image (Id : Natural) return String is
     Str : constant String := Nat_Image (Id);
   begin
     if Str'Length >= Max_Name_Len then
       return Str;
     else
       return Normal (Id, Max_Name_Len);
     end if;
   end Id_Image;

  -- Size on human readable format (xxxx or y.zM or yyM or yyyM)
  -- or on N digits or more
  function Size_Image (Size : Sys_Calls.Size_T; Human : Boolean) return String is
    Multipliers : constant array (1 .. 4) of Character := ('k', 'M', 'G', 'T');
    Kilos : Lister.Size_Type;
    Kilosi, Kilosf : Lister.Size_Type;
  begin
    if Human then
      if Size < 1024 then
        return Normal (Natural(Size), 4);
      end if;
      Kilos := Lister.Size_Type(Size);
      for I in Multipliers'Range loop
        Split (Kilos, Kilosi, Kilosf);
        if Kilosi < 10 then
          -- This is the proper multiplier
          -- y.zM
          return Total_Image (Kilosi) & '.'
               & Total_Image (Kilosf) & Multipliers(I);
        else
          if Kilosf > 5 then
            -- Round
            Kilosi := Kilosi + 1;
          end if;
          if Kilosi < 1000 then
            -- yyM or yyyM
            return Normal (Natural(Kilosi), 3) & Multipliers(I);
          end if;
        end if;
        Kilos := Kilosi;
      end loop;
      return Total_Image (Kilos) & Multipliers(Multipliers'Last);
    else
      declare
        Str : constant String := Size_Image (Entity.Size);
        Pad : constant String (1 .. Max_Size_Len - Str'Length)
            := (others => ' ');
      begin
        return Pad & Str;
      end;
    end if;
  end Size_Image;

  begin
    -- Put kind
    case Entity.Kind is
      when Directory.File => Ada.Text_Io.Put ('-');
      when Directory.Dir => Ada.Text_Io.Put ('d');
      when Directory.Link => Ada.Text_Io.Put ('l');
      when Directory.Block_Device => Ada.Text_Io.Put ('b');
      when Directory.Character_Device => Ada.Text_Io.Put ('c');
      when Directory.Pipe => Ada.Text_Io.Put ('p');
      when Directory.Socket => Ada.Text_Io.Put ('s');
      when Directory.Unknown => Ada.Text_Io.Put ('?');
    end case;

    -- Put Rights
    -- Extra bits
    Suid := (Entity.Rights and Shl (1, 11)) /= 0;
    Sgid := (Entity.Rights and Shl (1, 10)) /= 0;
    Tbit := (Entity.Rights and Shl (1, 09)) /= 0;
    -- User rights
    if (Entity.Rights and Shl (1, 8)) /= 0 then
      Ada.Text_Io.Put ('r');
    else
      Ada.Text_Io.Put ('-');
    end if;
    if (Entity.Rights and Shl (1, 7)) /= 0 then
      Ada.Text_Io.Put ('w');
    else
      Ada.Text_Io.Put ('-');
    end if;
    if (Entity.Rights and Shl (1, 6)) /= 0 then
      if Suid then
        Ada.Text_Io.Put ('s');
      else
        Ada.Text_Io.Put ('x');
      end if;
    else
      if Suid then
        Ada.Text_Io.Put ('S');
      else
        Ada.Text_Io.Put ('-');
      end if;
    end if;
    -- Group rights
    if (Entity.Rights and Shl (1, 5)) /= 0 then
      Ada.Text_Io.Put ('r');
    else
      Ada.Text_Io.Put ('-');
    end if;
    if (Entity.Rights and Shl (1, 4)) /= 0 then
      Ada.Text_Io.Put ('w');
    else
      Ada.Text_Io.Put ('-');
    end if;
    if (Entity.Rights and Shl (1, 3)) /= 0 then
      if Sgid then
        Ada.Text_Io.Put ('s');
      else
        Ada.Text_Io.Put ('x');
      end if;
    else
      if Sgid then
        Ada.Text_Io.Put ('S');
      else
        Ada.Text_Io.Put ('-');
      end if;
    end if;
    -- Others rights
    if (Entity.Rights and Shl (1, 2)) /= 0 then
      Ada.Text_Io.Put ('r');
    else
      Ada.Text_Io.Put ('-');
    end if;
    if (Entity.Rights and Shl (1, 1)) /= 0 then
      Ada.Text_Io.Put ('w');
    else
      Ada.Text_Io.Put ('-');
    end if;
    if (Entity.Rights and Shl (1, 0)) /= 0 then
      if Tbit then
        Ada.Text_Io.Put ('t');
      else
        Ada.Text_Io.Put ('x');
      end if;
    else
      if Tbit then
        Ada.Text_Io.Put ('T');
      else
        Ada.Text_Io.Put ('-');
      end if;
    end if;
    Ada.Text_Io.Put (' ');

    -- Owner and its group on N chars or more
    -- If resolution fails (Sys_Calls.System_Error) put Id in 10 chars
    begin
      declare
        User_Name : constant String
                  := Sys_Calls.Get_Name_Of_User_Id (Entity.User_Id);
        Pad : constant String (1 .. Max_Name_Len - User_Name'Length)
            := (others => ' ');
      begin
        Ada.Text_Io.Put (Pad & User_Name & ' ');
      end;
    exception
      when Sys_Calls.System_Error =>
        Ada.Text_Io.Put (Id_Image (Entity.User_Id) & ' ');
    end;
    begin
      declare
        Group_Name : constant String
                   := Sys_Calls.Get_Name_Of_Group_Id (Entity.Group_Id);
        Pad : constant String (1 .. Max_Group_Len - Group_Name'Length)
            := (others => ' ');
      begin
        Ada.Text_Io.Put (Pad & Group_Name & ' ');
      end;
    exception
      when Sys_Calls.System_Error =>
        Ada.Text_Io.Put (Id_Image (Entity.Group_Id) & ' ');
    end;

    -- Size human format or full size
    Ada.Text_Io.Put (Size_Image (Entity.Size, Human) & ' ');

    -- Modif time
    -- Date_Image is "YYyy/Mm/Dd Hh:Mm:Ss.mmm"
    -- Put "YYyy/Mm/Dd-Hh:Mm:Ss"
    Date := Date_Image (Entity.Modif_Time);
    Date(11) := '-';
    Ada.Text_Io.Put (Date(1 .. 19) & ' ');

    -- Entity name
    Ada.Text_Io.Put (Name_Image (Entity) );

    -- Link
    if Entity.Kind = Directory.Link then
      if Entity.Link_Ok then
        Ada.Text_Io.Put (" -> ");
      else
        Ada.Text_Io.Put (" => ");
      end if;
      Ada.Text_Io.Put (Asu_Ts (Entity.Link));
      if Classify and then Entity.Link_Ok
      and then Char_Of (Entity.Link_Kind, Entity.Link_Rights)
                       /= Default_Char then
        Ada.Text_Io.Put (Char_Of (Entity.Link_Kind, Entity.Link_Rights));
      end if;
    elsif Classify
    and then Char_Of (Entity.Kind, Entity.Rights) /= Default_Char then
      Ada.Text_Io.Put (Char_Of (Entity.Kind, Entity.Rights));
    end if;
    -- Done
    Ada.Text_Io.New_Line;
  end Put_Long;

  -- Sort list and put according to style
  procedure Put (List : in out Entities.Entity_List;
                 Append_New_Line : in Boolean) is
    Moved : Boolean;
    Ent : Entities.Entity;
  begin
    if List.Is_Empty then
      return;
    end if;
    -- Sort (rewinds) if less than a max
    if Sort_Kind = None or else List.List_Length > Max_To_Sort then
      List.Rewind;
    else
      Sort (List);
    end if;
    -- Init output
    Current_Col := 0;
    -- Put list
    loop
      List.Read (Ent, Done => Moved);
      if Separator_Set then
        -- Use separator whatever format requested
        Put_Raw (Ent);
      else
        -- Use format requested
        case Format_Kind is
          when Simple =>
            Put_Simple (Ent);
          when One_Row =>
            Put_One_Row (Ent);
          when Long =>
            Put_Long (Ent, False);
          when Long_Human =>
            Put_Long (Ent, True);
        end case;
      end if;
      exit when not Moved;
   end loop;
   if Append_New_Line and then Format_Kind = Simple then
     Ada.Text_Io.New_Line;
   end if;
  end Put;

  procedure Put_Dir (Name : in String) is
  begin
    Ada.Text_Io.Put_Line (Name & ":");
  end Put_Dir;

  procedure New_Line is
  begin
    Ada.Text_Io.New_Line;
  end New_Line;

  -- Put Total size, no new_line
  procedure Put_Size (Size : in Lister.Size_Type) is
    Kilos : Lister.Size_Type;
    Kilosi, Kilosf : Lister.Size_Type;

  begin
    -- Bytes
    Ada.Text_Io.Put ("Total size: " & Total_Image (Size) & "B");
    -- kBytes rounded
    Split (Size, Kilosi, Kilosf);
    if Kilosf < 5 then
      Kilos := Kilosi;
    else
      Kilos := Kilosi + 1;
    end if;
    Ada.Text_Io.Put (" " & Total_Image (Kilos) & "kB");

    -- MBytes and tenth
    Split (Kilos, Kilosi, Kilosf);
    if Kilosi = 0 and then Kilosf = 0 then
      return;
    end if;
    Ada.Text_Io.Put (" " & Total_Image (Kilosi)
                   & "." & Total_Image (Kilosf) & "MB");
    if Kilosf < 5 then
      Kilos := Kilosi;
    else
      Kilos := Kilosi + 1;
    end if;

    -- GBytes and tenth
    Split (Kilos, Kilosi, Kilosf);
    if Kilosi = 0 and then Kilosf = 0 then
      return;
    end if;
    Ada.Text_Io.Put (" " & Total_Image (Kilosi)
                   & "." & Total_Image (Kilosf) & "GB");
    if Kilosf < 5 then
      Kilos := Kilosi;
    else
      Kilos := Kilosi + 1;
    end if;

    -- TBytes and tenth
    Split (Kilos, Kilosi, Kilosf);
    if Kilosi = 0 and then Kilosf = 0 then
      return;
    end if;
    Ada.Text_Io.Put (" " & Total_Image (Kilosi)
                   & "." & Total_Image (Kilosf) & "TB");
  end Put_Size;

end Output;

