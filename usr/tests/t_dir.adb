with Ada.Calendar;
with My_Io, Int_Io, Directory, Text_Handler, Argument, Day_Mng, Normal;
procedure T_Dir is
  File_Name : Text_Handler.Text(Directory.Max_Dir_Name_Len);
  Dir_Name : Text_Handler.Text(Directory.Max_Dir_Name_Len);
  Kind : Directory.File_Kind_List;
  Rights : Natural;
  Mtime  : Directory.Time_T;
  Fsize  : Directory.Size_T;
  Max_Len : constant := 50;
  Pad : constant String (1 .. Max_Len) := (others => ' ');

  function Get_New_Dir return String is
    Str : String (1 .. 1024);
    Len : Natural;
  begin
    My_Io.Put ("Enter new directory: ");
    My_Io.Get_Line (Str, Len);
    return Str (1 .. Len);
  end  Get_New_Dir;

  procedure Put_Rights (Rights : in Natural) is
    Str : String(1 .. 7) := (others => ' ');
    Zstr : String(1 .. 4) := (others => '0');
    F,L : Natural;
  begin
    Int_Io.Put (Str, Rights, Base => 8);
    for I in Str'Range loop
      if Str(I) = '#' then
        F := I+1;
        exit;
      end if;
    end loop;
    for I in reverse Str'Range loop
      if Str(I) = '#' then
        L := I - 1;
        exit;
      end if;
    end loop;

    Zstr(4-L+F .. 4) := Str(F .. L);
    My_Io.Put(Zstr);
  end Put_Rights;

  procedure Put_Date (Mtime : in Directory.Time_T) is
    T : Ada.Calendar.Time;
    Year   : Ada.Calendar.Year_Number;
    Month  : Ada.Calendar.Month_Number;
    Day    : Ada.Calendar.Day_Number;
    Dur    : Ada.Calendar.Day_Duration;
    Hours    : Day_Mng.T_Hours;
    Minutes  : Day_Mng.T_Minutes;
    Seconds  : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;
  begin
    T := Directory.Time_Of(Mtime);
    Ada.Calendar.Split (T, Year, Month, Day, Dur);
    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisec);
    My_Io.Put(" " &
              Normal(Year, 4, Gap =>'0') & '/' &
              Normal(Month, 2, Gap =>'0') & '/' &
              Normal(Day, 2, Gap =>'0') & ' '  &
              Normal(Hours, 2, Gap =>'0') & ':'  &
              Normal(Minutes, 2, Gap =>'0') & ':'  &
              Normal(Seconds, 2, Gap =>'0') );
  end Put_Date;

  use Directory;
begin

  loop

    My_Io.Put_Line ("PWD ->" & Directory.Get_Current & "<");

    declare
      Dsc : Directory.Dir_Desc;
    begin
      if Argument.Get_Nbre_Arg /= 0 then
        Argument.Get_Parameter(Dir_Name);
      else
        Directory.Get_Current(Dir_Name);
      end if;
      Dsc := Directory.Open(Text_Handler.Value(Dir_Name));
      if Text_Handler.Value(Dir_Name) = "/" then
        Text_Handler.Empty(Dir_Name);
      end if;
      loop
        Directory.Next_Entry(Dsc, File_Name);
        My_Io.Put ("  ---->" & Text_Handler.Value (File_Name) & "< ");
        My_Io.Put (Pad(1 .. Max_Len - Text_Handler.Length(File_Name)));
        begin
          Directory.File_Stat (
             Text_Handler.Value (Dir_Name) & '/' &
             Text_Handler.Value (File_Name), Kind, Rights, Mtime, Fsize);
          Put_Rights (Rights);
          Put_Date (Mtime);
          My_Io.Put_Line (" " & Directory.File_Kind_List'Image(Kind));
          if Kind = Directory.Link then
            My_Io.Put_Line ("    ++++>" & Directory.Read_Link (
                Text_Handler.Value (Dir_Name) & '/' &
                Text_Handler.Value (File_Name)) & '<');
          end if;
        exception
          when Directory.Name_Error =>
            My_Io.Put_Line ("???? ???");
          when Directory.Access_Error =>
            My_Io.Put_Line ("!!!! !!!");
        end;
      end loop;
    exception
      when Directory.End_Error =>
        Directory.Close (Dsc);
    end;
    if Argument.Get_Nbre_Arg /= 0 then
      return;
    end if;

    My_Io.New_Line;
    loop
      begin
        Directory.Change_Current(Get_New_Dir);
        exit;
      exception
        when Directory.Name_Error =>
          My_Io.Put_Line ("-> Not found.");
        when Directory.Access_Error =>
          My_Io.Put_Line ("-> Permission.");
      end;
    end loop;

  end loop;

end T_Dir;

