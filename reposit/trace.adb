with TEXT_IO;
with NORMAL;
package body TRACE is

  FILE            : TEXT_IO.FILE_TYPE;
  TRACE_FILE_NAME : constant STRING := "_trace_";
  COUNT           : POSITIVE        := POSITIVE'FIRST;
  ACTIVATED       : BOOLEAN := TRUE;
  CREATED         : BOOLEAN := FALSE;

  procedure ACTIVATE (ON : in BOOLEAN := TRUE) is
  begin
    ACTIVATED := ON;
  end ACTIVATE;

  procedure CREATE is
  begin
    TEXT_IO.CREATE (FILE => FILE, MODE => TEXT_IO.OUT_FILE,
     NAME => TRACE_FILE_NAME);
    CREATED := TRUE;
  end CREATE;

  procedure PUT (MESSAGE : in STRING := "") is
  begin
    if not ACTIVATED then
      return;
    end if;
    if not CREATED then
      CREATE;
    end if;
    TEXT_IO.PUT_LINE (
     FILE => FILE,
     ITEM => NORMAL(COUNT, 5) & " ->" & MESSAGE & "<");
    if COUNT /= POSITIVE'LAST then
      COUNT := POSITIVE'SUCC(COUNT);
    else
      COUNT := POSITIVE'FIRST;
    end if;
  end PUT;

end TRACE;
