-- Basic_Proc output on stdout or stderr, protected by a Mutex
with Basic_Proc, Mutex_Manager;
package body Protected_Put is

  -- Mutex on Stdout and on Stderr
  Out_Mutex, Err_Mutex : Mutex_Manager.Mutex (Mutex_Manager.Simple, False);

  procedure Put_Line_Output (Str : in String) is
  begin
    Out_Mutex.Get;
    Basic_Proc.Put_Line_Output (Str);
    Out_Mutex.Release;
  exception
    when others =>
      Out_Mutex.Release;
      raise;
  end Put_Line_Output;

  procedure New_Line_Output is
  begin
    Out_Mutex.Get;
    Basic_Proc.New_Line_Output;
    Out_Mutex.Release;
  exception
    when others =>
      Out_Mutex.Release;
      raise;
  end New_Line_Output;

  procedure Flush_Output is
  begin
    Out_Mutex.Get;
    Basic_Proc.Flush_Output;
    Out_Mutex.Release;
  exception
    when others =>
      Out_Mutex.Release;
      raise;
  end Flush_Output;

  procedure Put_Line_Error (Str : in String) is
  begin
    Err_Mutex.Get;
    Basic_Proc.Put_Line_Error (Str);
    Err_Mutex.Release;
  exception
    when others =>
      Err_Mutex.Release;
      raise;
  end Put_Line_Error;

  procedure New_Line_Error is
  begin
    Err_Mutex.Get;
    Basic_Proc.New_Line_Error;
    Err_Mutex.Release;
  exception
    when others =>
      Err_Mutex.Release;
      raise;
  end New_Line_Error;

  procedure Flush_Error is
  begin
    Err_Mutex.Get;
    Basic_Proc.Flush_Error;
    Err_Mutex.Release;
  exception
    when others =>
      Err_Mutex.Release;
      raise;
  end Flush_Error;

end Protected_Put;

