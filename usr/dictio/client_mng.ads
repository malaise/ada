with Data_Base;
package Client_Mng is

  procedure Start;
  procedure Quit;

  procedure Modified (Kind : in Character; Item : in Data_Base.Item_Rec);

end Client_Mng;

