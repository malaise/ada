with Ada.Characters.Latin_1;
with Basic_Proc;
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
    Basic_Proc.Put_Output (Csi & "2J");
  end Clear;

  -- Move cursor left/right
  procedure Left is
  begin
    Basic_Proc.Put_Output (Csi & "1D");
  end Left;

  procedure Right is
  begin
    Basic_Proc.Put_Output (Csi & "1C");
  end Right;

  procedure Set_Col (N : Positive) is
  begin
    Basic_Proc.Put_Output (Csi & Trans (N) & 'G');
  end Set_Col;

  -- Delete
  procedure Delete is
  begin
    Basic_Proc.Put_Output (Csi & "1P");
  end Delete;

  -- Erase whole line
  procedure Erase_Line is
  begin
    Basic_Proc.Put_Output (Csi & "2K");
  end Erase_Line;

  -- Erase from beginning of line to cursor
  procedure Erase_Begin_Line is
  begin
    Basic_Proc.Put_Output (Csi & "0K");
  end Erase_Begin_Line;

  -- Erase from cursor to end of line
  procedure Erase_End_Line is
  begin
    Basic_Proc.Put_Output (Csi & "0K");
  end Erase_End_Line;

  -- Switch reverse
  procedure Set_Reverse (On : in Boolean) is
  begin
    Basic_Proc.Put_Output (Csi & (if On then "7m" else "27m"));
  end Set_Reverse;

  -- Save/restore cursor pos
  procedure Save is
  begin
    Basic_Proc.Put_Output (Csi & 's');
  end Save;

  procedure Restore is
  begin
    Basic_Proc.Put_Output (Csi & 'u');
  end Restore;

  procedure Sound (N_Times : in Positive := 1) is
  begin
    for I in 1 .. N_Times loop
      Basic_Proc.Put_Output (Ada.Characters.Latin_1.Bel);
      Basic_Proc.Flush_Output;
      delay 0.2;
    end loop;
  end Sound;

end Console;

