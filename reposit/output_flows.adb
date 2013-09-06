with Ada.Unchecked_Deallocation;
with Async_Stdin, Sys_Calls, Trace;
package body Output_Flows is

  package Logger is new Trace.Basic_Logger ("Output_Flows");

  procedure Free_File is new Ada.Unchecked_Deallocation
      (Text_Line.File_Type, File_Access);
  procedure Free_Cell is new Ada.Unchecked_Deallocation
      (Cell_Type, Cell_Access);

  -- Find a flow
  function Match (Current : Flow_Aliases.Handle;
                  Criteria : As.U.Asu_Us) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Get_Access.Name = Criteria;
  end Match;
  function Search is new Flows_Mng.Search_Criteria (As.U.Asu_Us, Match);
  function Search (Name : String) return Boolean is
  begin
    return Search (Flows, As.U.Tus (Name), From => Flows_Mng.Absolute);
  end Search;

  -- A cell is released, when there is one reference it is the list
  procedure Released (Cell : access Cell_Type;
                      Nb_Access : in Natural) is
    Cell_Acc : Cell_Access := Cell_Access (Cell);
    Dummy : Boolean;
  begin
    Logger.Log (Trace.Debug, "Released of " & Cell_Acc.Name.Image
                    & " to" & Nb_Access'Img);
    if Nb_Access = 1 then
      -- The last reference is the list
      if not Search (Cell_Acc.Name.Image) then
        Logger.Log (Trace.Error,
                    "Flow " & Cell_Acc.Name.Image & " not found in list");
        return;
      end if;
      -- Release handle and Delete it
      Logger.Log (Trace.Debug, "Deleting " & Cell_Acc.Name.Image);
      Flows.Deallocate (Moved => Dummy);
      Logger.Log (Trace.Debug, "Deleted");
    elsif Nb_Access = 0 then
      Logger.Log (Trace.Debug, "Finalizing " & Cell_Acc.Name.Image);
      -- No more reference
      if Cell_Acc.Kind = File then
        -- Close and free file
        Cell_Acc.File.Close_All;
        Free_File (Cell_Acc.File);
      end if;
      -- Free Cell
      Free_Cell (Cell_Acc);
      Logger.Log (Trace.Debug, "Finalized");
    end if;
  end Released;

  -- Get a flow
  function Get (Name : String) return Output_Flow is
  begin
    return Result : Output_Flow do
      Set (Result, Name);
    end return;
  end Get;

  procedure Set (Flow : out Output_Flow; Name : in String) is
    File_Acc : File_Access;
    New_Cell : Cell_Access;
    New_Handle : Flow_Aliases.Handle;
  begin
    if not Search (Name) then
      Logger.Log (Trace.Debug, "Creating " & Name);
      -- This flow does not exist in list, create it
      -- Init Cell
      if Name = Stdout_Name then
        File_Acc := new Text_Line.File_Type;
        File_Acc.Open (Text_Line.Out_File, Sys_Calls.Stdout);
        New_Cell := new Cell_Type'(File, As.U.Tus (Name), File_Acc);
      elsif Name = Stderr_Name then
        File_Acc := new Text_Line.File_Type;
        File_Acc.Open (Text_Line.Out_File, Sys_Calls.Stderr);
        New_Cell:= new Cell_Type'(File, As.U.Tus (Name), File_Acc);
      elsif Name = Async_Stdout_Name then
        New_Cell := new Cell_Type'(Async_Stdout, As.U.Tus (Name));
      elsif Name = Async_Stderr_Name then
        New_Cell := new Cell_Type'(Async_Stderr, As.U.Tus (Name));
      else
        File_Acc := new Text_Line.File_Type;
        File_Acc.Create_All (Name);
        New_Cell := new Cell_Type'(File, As.U.Tus (Name), File_Acc);
      end if;
      -- Set handle and insert in list
      New_Handle.Init (New_Cell);
      Flows.Insert (New_Handle);
    end if;

    -- Now current cell is the correct one
    Flows.Read (Flow.Handle, Flows_Mng.Current);
    Logger.Log (Trace.Debug, "Setting " & Flow.Handle.Get_Access.Name.Image);
  exception
    when Text_Line.Name_Error =>
      Free_File (File_Acc);
      raise Name_Error;
    when Text_Line.Io_Error =>
      Free_File (File_Acc);
      raise Io_Error;
  end Set;

  -- Release access to a flow, which becomes unset
  procedure Release (Flow : in out Output_Flow) is
  begin
    Logger.Log (Trace.Debug, "Releasing " &
        (if Flow.Handle.Is_Set then Flow.Handle.Get_Access.Name.Image
         else " not set") );
    Flow.Handle.Release;
  end Release;

  -- Is a flow set
  function Is_Set (Flow : Output_Flow) return Boolean is
  begin
    return Flow.Handle.Is_Set;
  end Is_Set;

  -- Get the name of a flow
  function Get_Name (Flow : Output_Flow) return String is
  begin
    return Flow.Handle.Get_Access.Name.Image;
  exception
    when Constraint_Error =>
      raise Status_Error;
  end Get_Name;


  -- Output a string and a line feed on a flow
  -- May raise Io_Error
  procedure Put_Line (Flow  : in Output_Flow;
                      Str   : in String;
                      Flush : in Boolean := False) is
    Cell_Acc : Cell_Access;
  begin
    Cell_Acc := Flow.Handle.Get_Access;
    case Cell_Acc.Kind is
      when File =>
        Cell_Acc.File.Put_Line (Str);
      when Async_Stdout =>
        Async_Stdin.Put_Line_Out (Str);
      when Async_Stderr =>
        Async_Stdin.Put_Line_Err (Str);
    end case;
    if Flush then
      Output_Flows.Flush (Flow);
    end if;
  exception
    when Constraint_Error =>
      raise Status_Error;
  end Put_Line;

  -- Flush a flow
  -- May raise Io_Error
  procedure Flush (Flow  : in Output_Flow) is
    Cell_Acc : Cell_Access;
  begin
    Cell_Acc := Flow.Handle.Get_Access;
    case Cell_Acc.Kind is
      when File =>
        Cell_Acc.File.Flush;
      when Async_Stdout =>
        Async_Stdin.Flush_Out;
      when Async_Stderr =>
        Async_Stdin.Flush_Out;
    end case;
  exception
    when Constraint_Error =>
      raise Status_Error;
  end Flush;

end Output_Flows;

