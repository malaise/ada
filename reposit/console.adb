with Ada.Text_Io, Ada.Characters.Latin_1;
-- From man console_codes
package body Console is

  Csi : constant String := Ada.Characters.Latin_1.Esc & '[';

  function Trans (N : Positive) return String is
    Str : constant String := N'Img;
  begin
    -- Skip first ' '
    return Str (2 .. Str'Last);
  end Trans;


  -- Clear screen
  procedure Clear is
  begin
    Ada.Text_Io.Put (Csi & "2J");
  end Clear;

  -- Move cursor left/right
  procedure Left is
  begin
    Ada.Text_Io.Put (Csi & "1D");
  end Left;

  procedure Right is
  begin
    Ada.Text_Io.Put (Csi & "1C");
  end Right;

  procedure Set_Col (N : Positive) is
  begin
    Ada.Text_Io.Put (Csi & Trans (N) & 'G');
  end Set_Col;

  -- Delete
  procedure Delete is
  begin
    Ada.Text_Io.Put (Csi & "1P");
  end Delete;

  -- Erase whole line
  procedure Erase_Line is
  begin
    Ada.Text_Io.Put (Csi & "2K");
  end Erase_Line;

  -- Erase from beginning of line to cursor
  procedure Erase_Begin_Line is
  begin
    Ada.Text_Io.Put (Csi & "0K");
  end Erase_Begin_Line;

  -- Erase from cursor to end of line
  procedure Erase_End_Line is
  begin
    Ada.Text_Io.Put (Csi & "0K");
  end Erase_End_Line;

  -- Switch reverse
  procedure Set_Reverse (On : in Boolean) is
  begin
    if On then
      Ada.Text_Io.Put (Csi & "7m");
    else
      Ada.Text_Io.Put (Csi & "27m");
    end if;
  end Set_Reverse;

  -- Save/restore cursor pos
  procedure Save is
  begin
    Ada.Text_Io.Put (Csi & 's');
  end Save;

  procedure Restore is
  begin
    Ada.Text_Io.Put (Csi & 'u');
  end Restore;

  procedure Sound (N_Times : in Positive := 1) is
  begin
    for I in 1 .. N_Times loop
      Ada.Text_Io.Put (Ada.Characters.Latin_1.Bel);
      Ada.Text_Io.Flush;
      delay 0.2;
    end loop;
  end Sound;

end Console;

