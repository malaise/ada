-- Bufferize input flow (strings) until separator is found
-- Report the received string (text between separators)
with Str_Util;
package body Input_Buffer is

  -- Initialise the buffer Buf with the sentence delimiter and the notifier
  procedure Set (Buf : in out Buffer;
                 Notifier : in Sentence_Notifier;
                 Delimiter : in String := Line_Feed;
                 Size : in Buffer_Size := Infinite_Size) is
  begin
    if Notifier = null or else Delimiter = "" then
      raise Constraint_Error;
    end if;
    Buf.Notif := Notifier;
    Buf.Delim := As.U.Tus (Delimiter);
    Buf.Size := Size;
    Buf.Text.Set_Null;
    Buf.Susp := False;
  end Set;

  -- INTERNAL: Raise Status_Error if Buf is not set
  procedure Check_Set (Buf : in Buffer) is
  begin
    if not Is_Set (Buf) then
      raise Status_Error;
    end if;
  end Check_Set;

  -- INTERNAL: Invoque the notifier for each delimited string,
  --  if Buf is not suspended
  procedure Flush (Buf : in out Buffer) is
    Ind : Natural;
    Head : As.U.Asu_Us;
  begin
    if Buf.Susp then
      return;
    end if;
    -- Several notifs
    loop
      Ind := Str_Util.Locate (Buf.Text.Image, Buf.Delim.Image);
      -- Truncate at Size if it is set
      if Buf.Size /= Infinite_Size
      and then ( (Ind = 0 and then Buf.Text.Length > Buf.Size)
                or else Ind > Buf.Size) then
        Ind := Buf.Size;
      else
        -- Move to end of sentence (end of delim)
        Ind := Ind + Buf.Delim.Length - 1;
      end if;
      -- No delim and within size
      exit when Ind = 0;
      Head := Buf.Text.Uslice (1, Ind);
      -- Del head
      Buf.Text.Delete (1, Ind);
      -- Finally notify (after the delete so that exception in Notifier
      --  don't lead to infinite loop if ignored by push/resume.)
      begin
        Buf.Notif (Head.Image);
      exception
        when others =>
          null;
      end;
    end loop;
  end Flush;

  -- Push text in buffer Buf
  -- Can lead the notifier to be invoqued once or several times
  -- May raise Buffer_Full  if Buf is Suspended and Text or Char whould
  --  make it larger than Size (if no Infinite)
  procedure Push (Buf : in out Buffer; Text : in String) is
  begin
    Check_Set (Buf);
    if Buf.Susp
    and then Buf.Size /= Infinite_Size
    and then Buf.Text.Length + Text'Length > Buf.Size then
      -- Buf would overflow
      raise Buffer_Full;
    end if;
    Buf.Text.Append (Text);
    Flush (Buf);
  end Push;
  procedure Push (Buf : in out Buffer; Char : in Character) is
  begin
    Push (Buf, Char & "");
  end Push;

  -- Get the tail of the buffer Buf (text not ending with Delimiter)
  function Tail (Buf : in out Buffer) return String is
  begin
    Check_Set (Buf);
    return Buf.Text.Image;
  end Tail;

  -- Suspend the buffer Buf
  procedure Suspend (Buf : in out Buffer) is
  begin
    Check_Set (Buf);
    Buf.Susp := True;
  end Suspend;

  -- Resume the buffer Buf
  procedure Resume (Buf : in out Buffer) is
  begin
    Check_Set (Buf);
    Buf.Susp := False;
    Flush (Buf);
  end Resume;

  -- Is the buffer Buf suspended?
  function Is_Suspended (Buf : Buffer) return Boolean is
  begin
    Check_Set (Buf);
    return Buf.Susp;
  end Is_Suspended;

  -- Is the buffer Buf set?
  function Is_Set (Buf : Buffer) return Boolean is
    (not Buf.Delim.Is_Null);

  -- Reset the buffer, which becomes not set
  procedure Reset (Buf : in out Buffer) is
  begin
    Check_Set (Buf);
    Buf := Init;
  end Reset;

  -- Clean the current tail of the buffer
  procedure Clean (Buf : in out Buffer) is
  begin
    Check_Set (Buf);
    Buf.Text.Set_Null;
  end Clean;

end Input_Buffer;

