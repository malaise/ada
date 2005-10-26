with Ada.Text_Io, Ada.Exceptions;

with Bloc_Io;

procedure T_Bloc_Io is

  subtype Bloc is Integer;
  package Int_Io is new Bloc_Io(Bloc);

  File_Name : constant String := "data/bloc_io.blk";
  File : Int_Io.File_Type;
  Arr : Int_Io.Element_Array(1 .. 256);

  Ro : Boolean;
  Remain : Int_Io.Count;
  N_Arr : constant := 21;

  procedure Dump (Slice : in Int_Io.Element_Array) is
  begin
    Ada.Text_Io.Put("Dump: ");
    for I in Slice'Range loop
      Ada.Text_Io.Put(Integer'Image(Slice(I)) & " ");
    end loop;
    Ada.Text_Io.New_Line;
  end Dump;

  use type Int_Io.Count;

begin

  begin
    Int_Io.Open(File, Int_Io.In_File, File_Name);
    Ro := True;
  exception
    when Error:others =>
      Ada.Text_Io.Put_Line("Open => "
             & Ada.Exceptions.Exception_Name (Error));
      Int_Io.Create(File, File_Name);
      Ro := False;
  end;

  Remain := Int_Io.Size(File);
  Ada.Text_Io.Put_Line("Size: " & Int_Io.Count'Image(Remain));

  if Ro then
    while Remain > Arr'Length loop
      Int_Io.Read(File, Arr);
      Dump(Arr);
      Remain := Remain -  Arr'Length;
    end loop;
    Int_Io.Read(File, Arr(1 .. Remain));
    Dump(Arr(1 .. Remain));
    Remain := 0;
    Int_Io.Set_Index(File, N_Arr * Arr'Length - 5);
    Ada.Text_Io.Put_Line("Set index to " & Int_Io.Count'Image(Int_Io.Index(File)));
    Int_Io.Read(File, Arr(1 .. 4));
    Dump(Arr(1 .. 4));
    Int_Io.Close(File);
    return;
  end if;

  for I in 1 .. N_Arr loop
    Arr := (others => I);
    Int_Io.Write(File, Arr);
  end loop;
  Arr(1) := 42;
  Int_Io.Write(File, Arr(1 ..1));

  Arr(1 .. 2) := (-1, -2);
  Int_Io.Write (File, Arr(1 .. 2), N_Arr * Arr'Length - 4);

  Int_Io.Close(File);

end T_Bloc_Io;

