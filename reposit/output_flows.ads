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

  -- Set a new flow on an already open File
  -- May raise Already_Error if a flow with the same name
  --  is already set
  procedure Set (Flow : out Output_Flow; Name : in String;
                 File_Acc : access Text_Line.File_Type);

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
  Already_Error : exception;

private

  type File_Access is access all Text_Line.File_Type;

  type Flow_Kinds is (File, Async_Stdout, Async_Stderr);
  type Cell_Type (Kind : Flow_Kinds := File) is record
    Name : As.U.Asu_Us;
    case Kind is
      when File =>
        Close : Boolean := True;
        File : File_Access;
      when Async_Stdout | Async_Stderr =>
        null;
    end case;
  end record;
  type Cell_Access is access all Cell_Type;


  procedure Released (Cell : access Cell_Type; Nb_Access : in Natural);
  package Flow_Aliases is new Smart_Alias (Cell_Type, Released);

  type Output_Flow is tagged record
    Handle : Flow_Aliases.Handle;
  end record;

  package Flow_List_Mng is new  Dynamic_List (Flow_Aliases.Handle);
  package Flows_Mng renames Flow_List_Mng.Dyn_List;
  Flows : Flows_Mng.List_Type;

end Output_Flows;

