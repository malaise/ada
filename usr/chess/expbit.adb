with Ada.Text_Io;
with Get_Line, Argument, Text_Handler;

procedure ExpBit is

  Size : constant := 35;
  Size2 : constant := Size / 2 + 1;

  package MGL is new Get_Line (1, Size2, Size/2 * 2 + 1, '#');
  Count : MGL.Word_Count;
  Line : MGL.Line_Array;

  Str : String (1 .. Size2);
  Lines_Done : Natural := 0;

begin

  MGL.Open (Argument.Get_Parameter);

  loop
    -- Load left part
    Count := MGL.Get_Word_Number;
    MGL.Get_Words (Line);
    Str := (others => '0');
    for I in 1 .. Count loop
      Str(I) := Text_Handler.Value(Line(I))(1);
    end loop;
    
    for I in Str'Range loop
      Ada.Text_Io.Put (Str(Str'Last - I + 1) & ", ");
    end loop;
    for I in 2 .. Str'Last - 1 loop
      Ada.Text_Io.Put (Str(I) & ", ");
    end loop;
    Ada.Text_Io.Put_Line (Str(Str'Last) & ",");
    Lines_Done := Lines_Done + 1;
    
   MGL.Read_Next_Line;
 end loop;

exception
  when MGL.No_More_Line =>
    MGL.Close;
    Ada.Text_Io.Put_Line ("Done " & Natural'Image (Lines_Done));
end ExpBit;

