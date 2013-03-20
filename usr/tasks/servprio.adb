with Basic_Proc, Rnd, Schedule, Text_Line;
use Basic_Proc;
procedure Servprio is

  type Urgence is (Critique, Urgent, Normal, Lent);
  subtype Client_Range is Positive range 1 .. 10;

  type Tableau is array(Urgence) of Natural;
  Totals, Totalc : Tableau := (others => 0);
  Total_Appel    : Natural := 0;

  function My_Random is
    new Rnd.Discr_Random(Urgence);

  Fich : Text_Line.File_Type;
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
    Text_Line.Put (Fich, Auteur);
    Text_Line.Put (Fich, " de ");
    Text_Line.Put (Fich, Client_Range'Image(Client_No));
    Text_Line.Put (Fich, " priorite ");
    Text_Line.Put_Line(Fich, Urgence'Image(Degre));
  end Print;


  task body Serveur is
    I : Urgence := Urgence'First;

  begin
    accept Init;

    loop
      Schedule;

      select
        accept Service(I) (No_Client : in Client_Range) do
          Print ("Service", I, No_Client);
          Put_Line_Output ("Service");
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
      Put_Line_Output ("Exception serveur");
  end Serveur;


  task body Client is
    No       : Client_Range;
    Prio     : Urgence;
    Max_Loop : constant Positive := 10;
    N_Loop   : Positive;
    Calcul   : Tableau := (others => 0);
  begin
    accept Init (No : in Client_Range) do
      Client.No := No;
    end Init;

    N_Loop := Rnd.Gen.Int_Random (1, Max_Loop);
    for I in 1 .. N_Loop loop
      Schedule;
      Prio := My_Random (Rnd.Gen.all);
      Print ("                           Requete", Prio, No);

      Serveur.Service(Prio) (No);

      Calcul (Prio) := Calcul (Prio) + 1;
    end loop;

    accept Fin (Total : in out Tableau) do
      for I in Urgence loop
        Total(I) := Total(I) + Calcul(I);
      end loop;
    end Fin;

    Text_Line.Put (Fich, "                           Fin client ");
    Text_Line.Put (Fich, Client_Range'Image(No));
    Text_Line.Put (Fich, " , ");
    Text_Line.Put (Fich, Positive'Image(N_Loop));
    Text_Line.Put_Line (Fich, " appels");

  exception
    when others =>
      Put_Line_Output ("Exception client");
      raise;
  end Client;

begin
  Rnd.Gen.Randomize;
  Text_Line.Create_All (Fich, Nom);

  for I in Client_Range loop
    Clients(I).Init (I);
  end loop;

  Serveur.Init;

  for I in Client_Range loop
    Clients(I).Fin (Totalc);
  end loop;

  Serveur.Fin;

  Text_Line.Put_Line (Fich, "TOTAUX");
  for I in Urgence loop
    Text_Line.Put (Fich, Urgence'Image(I));
    Text_Line.Put (Fich, " -> ");
    Text_Line.Put (Fich, Positive'Image(Totals(I)));
    Text_Line.Put (Fich, " & ");
    Text_Line.Put_Line (Fich, Positive'Image(Totalc(I)));
    Total_Appel := Total_Appel + Totalc(I);
  end loop;
  Text_Line.Put_Line (Fich, " TOTAL APPELS : " & Positive'Image(Total_Appel));

  Text_Line.Close_All (Fich);

exception
  when others =>
    Put_Line_Output ("Exception prog");
    raise;
end Servprio;

