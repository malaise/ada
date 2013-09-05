with Ada.Finalization;
with As.U, Dynamic_List, Text_Line, Smart_Alias;
package Output_Flows is

  type Output_Flow is tagged private;

  -- Get a flow
  -- Predefined flows are (case sensitive)
  -- Text_Line flow on stdout / stderr
  Stdout_Name : constant String := "Stdout";
  Stderr_Name : constant String := "Stderr";
  -- Async_Stdin flow on stdout / stderr
  Async_Stdout_Name : constant String := "Async_Stdout";
  Async_Stderr_Name : constant String := "Async_Stderr";
  -- Other strings lead to a file (fd), possibly raising
  --  Name_Error or Io_Error
  function Get (Name : String) return Output_Flow;
  procedure Set (Flow : out Output_Flow; Name : in String);

  -- Release access to a flow, which becomes unset
  -- No effect if it is already unset
  procedure Release (Flow : in out Output_Flow);

  -- Is a flow set
  function Is_Set (Flow : Output_Flow) return Boolean;

  -- The following operations raise Status_Error if the Flow is not set

  -- Get the name of a flow
  function Get_Name (Flow : Output_Flow) return String;

  -- Output a string and a line feed on a flow
  -- May raise Io_Error
  procedure Put_Line (Flow  : in Output_Flow;
                      Str   : in String;
                      Flush : in Boolean := False);

  -- Flush a flow
  -- May raise Io_Error
  procedure Flush (Flow  : in Output_Flow);

  -- Exceptions
  Status_Error : exception;
  Name_Error : exception;
  Io_Error : exception;

private

  type File_Access is access Text_Line.File_Type;

  type Flow_Kinds is (File, Async_Stdout, Async_Stderr);
  type Cell (Kind : Flow_Kinds := File) is record
    Name : As.U.Asu_Us;
    case Kind is
      when File =>
        File : File_Access;
      when Async_Stdout | Async_Stderr =>
        null;
    end case;
  end record;
  type Cell_Access is access all Cell;

  package Flow_List_Mng is new  Dynamic_List (Cell);
  package Flows_Mng renames Flow_List_Mng.Dyn_List;
  Flows : Flows_Mng.List_Type;

  procedure Finalize (Dest : access Cell);
  package Flow_Aliases is new Smart_Alias (Cell, Finalize);

  type Output_Flow is new Ada.Finalization.Controlled with record
    Handle : Flow_Aliases.Handle;
  end record;
  overriding procedure Finalize (Flow : in out Output_Flow);

end Output_Flows;

