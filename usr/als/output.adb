with Ada.Calendar, Ada.Strings.Unbounded, Ada.Text_Io;
with Directory, Sys_Calls, Bit_Ops, Normal, Int_Image, Date_Image, Upper_Str;
package body Output is
  package Asu renames Ada.Strings.Unbounded;
  function Nat_Image is new Int_Image (Natural);
  function Size_Image is new Int_Image (Sys_Calls.Size_T);

  -- Stored style
  Sort_Kind : Sort_Kind_List;
  Revert : Boolean;
  Format_Kind : Format_Kind_List;
  Put_Path : Boolean;

  -- Set (store) sorting and format style
  procedure Set_Style (Sort_Kind : in Sort_Kind_List;
                       Revert : in Boolean;
                       Format_Kind : in Format_Kind_List;
                       Put_Path : in Boolean) is
  begin
    Output.Sort_Kind := Sort_Kind;
    Output.Revert := Revert;
    Output.Format_Kind := Format_Kind;
    Output.Put_Path := Put_Path;
  end Set_Style;

  -- Sorting function
  function Less_Than (El1, El2 : Entities.Entity) return Boolean is
    use type Ada.Calendar.Time, Asu.Unbounded_String;
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
    if Put_Path then
      C1.Name := Asu.To_Unbounded_String (
        Directory.Build_File_Name (
           Asu.To_String (C1.Path),
           Asu.To_String (C1.Name), "") );
      C2.Name := Asu.To_Unbounded_String (
        Directory.Build_File_Name (
           Asu.To_String (C2.Path),
           Asu.To_String (C2.Name), "") );
    end if;
    -- Alpha sort case insensitive, and letters before ponctuation
    C1.Name := Asu.To_Unbounded_String (Upper_Str (Asu.To_String (C1.Name)));
    C2.Name := Asu.To_Unbounded_String (Upper_Str (Asu.To_String (C2.Name)));
    case Sort_Kind is
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

  -- Put an entity name (possibly with full path)
  procedure Put_Name (Entity : in Entities.Entity) is
  begin
    if Put_Path then
      Ada.Text_Io.Put (
        Directory.Build_File_Name (
           Asu.To_String (Entity.Path),
           Asu.To_String (Entity.Name), "") );
    else
      Ada.Text_Io.Put (Asu.To_String (Entity.Name));
    end if;
  end Put_Name;

  -- Put an entity in normal mode
  Max_Col : constant := 80;
  Current_Col : Natural := 0;
  Col_Separator : constant String := "  ";
  procedure Put_Simple (Entity : in Entities.Entity) is
    Len : constant Natural := Asu.Length (Entity.Name);
  begin
    -- Check if need to New_Line or Space
    if Current_Col /= 0 then
      if Current_Col + Len >= Max_Col then
        Ada.Text_Io.New_Line;
        Current_Col := 0;
      else
        Ada.Text_Io.Put (Col_Separator);
        Current_Col := Current_Col + Col_Separator'Length;
      end if;
    end if;
    Ada.Text_Io.Put (Asu.To_String (Entity.Name));
    Current_Col := Current_Col + Len;
  end Put_Simple;

  -- Put an entity in one row
  procedure Put_One_Row (Entity : in Entities.Entity) is
  begin
    Put_Name (Entity);
    Ada.Text_Io.New_Line;
  end Put_One_Row;

  -- Put an entity in full mode
  procedure Put_Long (Entity : in Entities.Entity) is
   Suid, Sgid, Tbit : Boolean;
   Date : String (1 .. 23);
   Max_Name_Len : constant := 10;
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

    -- Owner and its group on 10 chars or more
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
        Pad : constant String (1 .. Max_Name_Len - Group_Name'Length)
            := (others => ' ');
      begin
        Ada.Text_Io.Put (Pad & Group_Name & ' ');
      end;
    exception
      when Sys_Calls.System_Error =>
        Ada.Text_Io.Put (Id_Image (Entity.Group_Id) & ' ');
    end;

    -- Size on 10 digits
    declare
      Max_Size_Len : constant := 10;
      Str : constant String := Size_Image (Entity.Size);
      Pad : constant String (1 .. Max_Size_Len - Str'Length) := (others => ' ');
    begin
      Ada.Text_Io.Put (Pad & Str & ' ');
    end;

    -- Modif time
    -- Date_Image is "YYyy/Mm/Dd Hh:Mm:Ss.mmm"
    -- Put "YYyy/Mm/Dd-Hh:Mm"
    Date := Date_Image (Entity.Modif_Time);
    Date(11) := '-';
    Ada.Text_Io.Put (Date(1 .. 16) & ' ');

    -- Entity name
    Put_Name (Entity);

    -- Link
    if Entity.Kind = Directory.Link then
      Ada.Text_Io.Put (" -> " & Asu.To_String (Entity.Link));
    end if;
    -- Done
    Ada.Text_Io.New_Line;
  end Put_Long;

  -- Sort list and put according to style
  procedure Put (List : in out Entities.Entity_List) is
    Moved : Boolean;
    Ent : Entities.Entity;
  begin
    if List.Is_Empty then
      return;
    end if;
    -- Sort (rewinds)
    Sort (List);
    -- Init output
    Current_Col := 0;
    -- Put list
    loop
      List.Read (Ent, Done => Moved);
      case Format_Kind is
        when Simple =>
          Put_Simple (Ent);
        when One_Row =>
          Put_One_Row (Ent);
        when Long =>
          Put_Long (Ent);
      end case;
      exit when not Moved;
   end loop;
  end Put;

  procedure Put_Dir (Name : in String) is
  begin
    Ada.Text_Io.Put_Line (Name & ":");
  end Put_Dir;

  procedure New_Line is
  begin
    Ada.Text_Io.New_Line;
  end New_Line;

end Output;

