with Table;
procedure Lapeau is
begin
  Table.Init;
  while Table.Wait_Event loop
    null;
  end loop;
end Lapeau;
