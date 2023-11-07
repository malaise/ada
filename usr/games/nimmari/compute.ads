with Common;
package Compute is

  -- To be done once, before first Play
  procedure Init;

  -- Do machine play and return result
  procedure Play (Row    : out Common.Row_Range;
                  Remove : out Common.Bar_Status_Array;
                  Result : out Common.Result_List);
end Compute;

