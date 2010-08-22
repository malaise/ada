with Interfaces.C_Streams;
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
  begin
    I := Interfaces.C_Streams.Fflush (Interfaces.C_Streams.Stderr);
  end Flush_Error;

  -- Set exit code
  procedure Set_Exit_Code (Code : in Natural) is
  begin
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status(Code));
  end Set_Exit_Code;

  -- Set ok or error exit code
  procedure Set_Ok_Exit_Code is
  begin
    Set_Exit_Code(0);
  end Set_Ok_Exit_Code;

  procedure Set_Error_Exit_Code is
  begin
    Set_Exit_Code(1);
  end Set_Error_Exit_Code;

end Basic_Proc;

