with My_Io, Rnd, Text_Io, Schedule; use My_Io; 
procedure Servprio is 

  type Urgence is (Critique, Urgent, Normal, Lent); 
  subtype Client_Range is Positive range 1 .. 10; 

  type Tableau is array(Urgence) of Natural; 
  Totals, Totalc : Tableau := (others => 0); 
  Total_Appel    : Natural := 0; 

  function My_Random is 
    new Rnd.Discr_Random(Urgence); 

  Fich : Text_Io.File_Type; 
  Nom  : constant String := ("servprio.dat"); 

  task Serveur is 
    entry Init; 
    entry Service(Urgence)(No_Client : in Client_Range); 
    entry Fin; 
  end Serveur; 

  task type Client is 
    entry Init(No : in Client_Range); 
    entry Fin(Total : in out Tableau); 
  end Client; 

  Clients : array(Client_Range) of Client; 

  procedure Print(Auteur    : in String; 
                  Degre     : in Urgence; 
                  Client_No : in Client_Range) is 
  begin
    Text_Io.Put(Fich, Auteur); 
    Text_Io.Put(Fich, " de "); 
    Text_Io.Put(Fich, Client_Range'Image(Client_No)); 
    Text_Io.Put(Fich, " priorite "); 
    Text_Io.Put_Line(Fich, Urgence'Image(Degre)); 
  end Print; 


  task body Serveur is 
    I : Urgence := Urgence'First; 

  begin
    accept Init; 

    loop
      Schedule; 

      select
        accept Service(I)(No_Client : in Client_Range) do
          Print("Service", I, No_Client); 
          Put_Line("Service"); 
          Totals(I) := Totals(I) + 1; 
        end Service; 
        I := Urgence'First; 
      or 
        delay 0.0; 
        if I /= Urgence'Last then 
          I := Urgence'Succ(I); 
        else 
          I := Urgence'First; 
        end if; 
      or 
        accept Fin; 
        exit; 
      end select; 
    end loop; 

  exception
    when others => 
      Put_Line("Exception serveur"); 
  end Serveur; 


  task body Client is 
    No       : Client_Range; 
    Prio     : Urgence; 
    Max_Loop : constant Positive := 10; 
    N_Loop   : Positive; 
    Calcul   : Tableau := (others => 0); 
  begin
    accept Init(No : in Client_Range) do
      Client.No := No; 
    end Init; 

    N_Loop := Rnd.Int_Random(1, Max_Loop); 
    for I in 1 .. N_Loop loop
      Schedule; 
      Prio := My_Random; 
      Print("                           Requete", Prio, No); 

      Serveur.Service(Prio)(No); 

      Calcul(Prio) := Calcul(Prio) + 1; 
    end loop; 

    accept Fin(Total : in out Tableau) do
      for I in Urgence loop
        Total(I) := Total(I) + Calcul(I); 
      end loop; 
    end Fin; 

    Text_Io.Put(Fich, "                           Fin client "); 
    Text_Io.Put(Fich, Client_Range'Image(No)); 
    Text_Io.Put(Fich, " , "); 
    Text_Io.Put(Fich, Positive'Image(N_Loop)); 
    Text_Io.Put_Line(Fich, " appels"); 

  exception
    when others => 
      Put_Line("Exception client"); 
      raise; 
  end Client; 

begin
  Text_Io.Create(Fich, Text_Io.Out_File, Nom); 

  for I in Client_Range loop
    Clients(I).Init(I); 
  end loop; 

  Serveur.Init; 

  for I in Client_Range loop
    Clients(I).Fin(Totalc); 
  end loop; 

  Serveur.Fin; 

  Text_Io.Put_Line(Fich, "TOTAUX"); 
  for I in Urgence loop
    Text_Io.Put(Fich, Urgence'Image(I)); 
    Text_Io.Put(Fich, " -> "); 
    Text_Io.Put(Fich, Positive'Image(Totals(I))); 
    Text_Io.Put(Fich, " & "); 
    Text_Io.Put_Line(Fich, Positive'Image(Totalc(I))); 
    Total_Appel := Total_Appel + Totalc(I); 
  end loop; 
  Text_Io.Put_Line(Fich, " TOTAL APPELS : " & Positive'Image(Total_Appel)); 

  Text_Io.Close(Fich); 

exception
  when others => 
    Put_Line("Exception prog"); 
    raise; 
end Servprio; 
