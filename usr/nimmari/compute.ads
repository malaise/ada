with Common;
package Compute is

  procedure Init;

  type Result_List is (Won, Lost, Played_And_Won, Played_And_Lost, Played);
  subtype Played_Result_List is Result_List range Played_And_Won .. Played;

  procedure Play (Game : in Common.Game_List;
                  Result : out Result_List;
                  Row : out Common.Row_Range;
                  Bars : out Common.Full_Bar_Range);
end Compute;
