with Data_Base;
package Client_Mng is

  procedure Start;
  procedure Quit;

  procedure Modified (Item : in Data_Base.Item_Rec);
  function Stable return Boolean;

  procedure New_Status;

end Client_Mng;

