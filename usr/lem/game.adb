with Flight, Lem;
procedure Game is
  Status : Flight.Status_Rec;
begin
  loop
    Status := Flight.Get_Status;
  end loop;
end Game;

