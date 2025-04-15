-- Compute and put the number of Ada/Java statements of file
--  the percent number of comments and the numbers of lines
package One_File_Statements is

  -- The metrics of a file
  type Metrics is record
    Statements : Natural := 0;
    Comments : Natural := 0;
    Lines : Natural := 0;
  end record;
  Skipped : constant Metrics := (others => <>);
  Empty : constant Metrics := (Lines => 1, others => <>);

  -- Computing operation
  ----------------------
  -- Compute the metrics of a file
  --  if Java then use java comments ("//" or /*.. */ instead of Ada "--")
  --    and handle java suote character ("\"")
  -- May raise File_Error on error accessing file
  -- May raise Process_Error on error processing file
  File_Error : exception;
  Process_Error : exception;
  function Count_Statements_Of_File (File_Name : String;
                                     Java_Syntax : Boolean) return Metrics;
  -- Putting operations
  ---------------------
  Default_Width : constant := 50;
  -- Put header, for further Put_File with this Width
  procedure Put_Header (Width : in Positive := Default_Width);

  -- Put the metrics of a file
  procedure Put_File (
             File_Name : in String;
             Metric    : in Metrics;
             Width     : in Positive := Default_Width);

  -- Put the total metrics
  --  if Summary, put the summary metrics (after some Put_File with this Width)
  --  else put the unformated total of statements so far
  procedure Put_Total (Metric  : in Metrics;
                       Summary : in Boolean;
                       Width   : in Positive := Default_Width);

end One_File_Statements;

