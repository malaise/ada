-- Bufferize input flow (strings) until separator is found
-- Report the received string (text between separators)
-- This is the same as Text_Line input except that the input flow is
--  explicitly pushed by the application
with As.U, Text_Line;
package Input_Buffer is

  -------------------------
  -- GENERAL DEFINITIONS --
  -------------------------
  -- Default separator
  Line_Feed : constant String := Text_Line.Line_Feed_Str;

  -- Callback invoqued when a sentence has been identified in input flow
  --  (text ending by separator)
  type Sentence_Notifier is access procedure (Sentence : in String);

  -- A buffer
  type Buffer is tagged private;

  ----------------------
  -- BASIC OPERATIONS --
  ----------------------
  -- Initialise the buffer Buf with the sentence delimiter and the notifier
  -- Previous content of Buf is overwritten
  -- May raise Constraint_Error if Delimiter is empty
  procedure Set (Buf : in out Buffer;
                 Notifier : in Sentence_Notifier;
                 Delimiter : in String := Line_Feed);

  -- Push text in buffer Buf
  -- Can lead the notifier to be invoqued once or several times
  -- May raise Status_Error if Buf is not set
  procedure Push (Buf : in out Buffer; Text : in String);
  procedure Push (Buf : in out Buffer; Char : in Character);

  ----------------------
  -- OTHER OPERATIONS --
  ----------------------
  -- Read the tail of the buffer Buf (text not ending with Delimiter)
  -- Beware that there may be Delimiters in the tail if the buffer is
  --  suspended
  -- May raise Status_Error if Buf is not set
  function Tail (Buf : in out Buffer) return String;

  -- Suspend the buffer Buf
  -- It will store the text pushed but not notify
  -- No effect if already suspended
  -- May raise Status_Error if Buf is not set
  procedure Suspend (Buf : in out Buffer);

  -- Resume the buffer Buf
  -- Can lead the notifier to be invoqued once or several times
  -- No effect if not suspended
  -- May raise Status_Error if Buf is not set
  procedure Resume (Buf : in out Buffer);

  -- Is the buffer Buf suspended?
  -- May raise Status_Error if Buf is not set
  function Is_Suspended (Buf : Buffer) return Boolean;

  ---------------
  -- UTILITIES --
  ---------------
  -- Is the buffer Buf set?
  function Is_Set (Buf : Buffer) return Boolean;

  -- Reset the buffer, which becomes not set
  -- May raise Status_Error if Buf is not set
  procedure Reset (Buf : in out Buffer);

  -- Clean the current tail of the buffer
  -- May raise Status_Error if Buf is not set
  procedure Clean (Buf : in out Buffer);

  Status_Error : exception;
private

  type Buffer is tagged record
    Notif : Sentence_Notifier := null;
    Delim : As.U.Asu_Us;
    Text : As.U.Asu_Us;
    Susp : Boolean := False;
  end record;

  Init : constant Buffer := (others => <>);

end Input_Buffer;

