with System, Interfaces.C_Streams;
with Ada.Command_Line, Ada.Characters.Latin_1;
package body Basic_Proc is

  -- Put line on stdout
  procedure Put_Output (Str : in String) is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
    Str4C : constant String := Str & Ada.Characters.Latin_1.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (Str4C'Address,
                 Interfaces.C_Streams.Stdout);
  end Put_Output;

  procedure Put_Output (Char : in Character) is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
    Str4C : constant String := Char & Ada.Characters.Latin_1.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (Str4C'Address,
                 Interfaces.C_Streams.Stdout);
  end Put_Output;

  procedure New_Line_Output is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
    Str4C : constant String := Ada.Characters.Latin_1.Lf
                             & Ada.Characters.Latin_1.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (Str4C'Address,
                 Interfaces.C_Streams.Stdout);
  end New_Line_Output;

  procedure Put_Line_Output (Str : in String) is
  begin
    Put_Output (Str);
    New_Line_Output;
  end Put_Line_Output;

  procedure Flush_Output is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
  begin
    I := Interfaces.C_Streams.Fflush (Interfaces.C_Streams.Stdout);
  end Flush_Output;

  -- Put line on stderr
  procedure Put_Error (Str : in String) is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
    Str4C : constant String := Str & Ada.Characters.Latin_1.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (Str4C'Address,
                 Interfaces.C_Streams.Stderr);
  end Put_Error;

  procedure Put_Error (Char : in Character) is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
    Str4C : constant String := Char & Ada.Characters.Latin_1.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (Str4C'Address,
                 Interfaces.C_Streams.Stderr);
  end Put_Error;

  procedure New_Line_Error is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
    Str4C : constant String := Ada.Characters.Latin_1.Lf
                             & Ada.Characters.Latin_1.Nul;
  begin
    I := Interfaces.C_Streams.Fputs (Str4C'Address,
                 Interfaces.C_Streams.Stderr);
  end New_Line_Error;

  procedure Put_Line_Error (Str : in String) is
  begin
    Put_Error (Str);
    New_Line_Error;
  end Put_Line_Error;

  procedure Flush_Error is
    I : Interfaces.C_Streams.Int;
    pragma Unreferenced (I);
  begin
    I := Interfaces.C_Streams.Fflush (Interfaces.C_Streams.Stderr);
  end Flush_Error;

  -- Get line from stdin
  procedure Get_Input (Item : out String;
                       Last : out Natural) is
   Chrs : Interfaces.C_Streams.Chars;
   Str : String (1 .. Item'Length);
   use type System.Address;
  begin
    Chrs := Interfaces.C_Streams.Fgets (Str'Address,
                    Str'Length,
                    Interfaces.C_Streams.Stdin);
    if Chrs = System.Null_Address then
      raise End_Error;
    end if;
    for I in Str'Range loop
      if Str(I) = Ada.Characters.Latin_1.Lf
      or else Str(I) = Ada.Characters.Latin_1.Cr
      or else Str(I) = Ada.Characters.Latin_1.Nul then
        -- Copy up to Cr/Lf/Nul excluded
        Item(Item'First .. Item'First + I - Str'First) := Str(Str'First .. I);
        Last := Item'First + I - 2;
        return;
      end if;
    end loop;
    -- Should not occur because fgets always appends a Nul
    Last := 0;
  end Get_Input;

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural) is
  begin
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status(Code));
  end Set_Exit_Code;

  -- Set ok or error exit code
  procedure Set_Ok_Exit_Code is
  begin
    Set_Exit_Code(Exit_Code_Ok);
  end Set_Ok_Exit_Code;

  procedure Set_Error_Exit_Code is
  begin
    Set_Exit_Code(Exit_Code_Error);
  end Set_Error_Exit_Code;

end Basic_Proc;

