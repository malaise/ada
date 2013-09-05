with Ada.Unchecked_Deallocation;
with Async_Stdin, Sys_Calls;
package body Output_Flows is

  procedure Free_File is new Ada.Unchecked_Deallocation
      (Text_Line.File_Type, File_Access);

  -- Find a flow
  function Match (Current, Criteria : Cell) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end Match;
  function Search is new Flows_Mng.Search (Match);
  function Search (Name : String) return Boolean is
    Criteria : Cell;
  begin
    Criteria.Name := As.U.Tus (Name);
    return Search (Flows, Criteria, From => Flows_Mng.Absolute);
  end Search;

  -- Finalize a handle, when last reference is destroyed
  procedure Finalize (Dest : access Cell) is
    Dummy : Boolean;
  begin
    if not Flows.Search_Access (Dest) then
      -- This shall not occur
      raise Program_Error;
    end if;
    if Dest.Kind = File then
      -- Close and free file
      Dest.File.Close_All;
      Free_File (Dest.File);
      -- Delete cell
      Flows.Delete (Moved => Dummy);
    end if;
  end Finalize;

  -- Get a flow
  function Get (Name : String) return Output_Flow is
  begin
    return Result : Output_Flow do
      Set (Result, Name);
    end return;
  end Get;

  procedure Set (Flow : out Output_Flow; Name : in String) is
    File_Acc : File_Access;
    New_Cell : Cell;
  begin
    if not Search (Name) then
      -- This flow does not exist in list, create it
      -- Init Cell
      if Name = Stdout_Name then
        File_Acc := new Text_Line.File_Type;
        File_Acc.Open (Text_Line.Out_File, Sys_Calls.Stdout);
        New_Cell := (File, As.U.Tus (Name), File_Acc);
      elsif Name = Stderr_Name then
        File_Acc := new Text_Line.File_Type;
        File_Acc.Open (Text_Line.Out_File, Sys_Calls.Stderr);
        New_Cell := (File, As.U.Tus (Name), File_Acc);
      elsif Name = Async_Stdout_Name then
        New_Cell := (Async_Stdout, As.U.Tus (Name));
      elsif Name = Async_Stderr_Name then
        New_Cell := (Async_Stderr, As.U.Tus (Name));
      else
        File_Acc := new Text_Line.File_Type;
        File_Acc.Create_All (Name);
        New_Cell := (File, As.U.Tus (Name), File_Acc);
      end if;
      -- Insert cell
      Flows.Insert (New_Cell);
    end if;

    -- Now current cell is the correct one
    Flow.Handle.Init (Flows.Access_Current);
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
    return Flow.Handle.Get_Access.all.Name.Image;
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

