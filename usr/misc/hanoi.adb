with Ada.Text_Io, Ada.Calendar;
with My_Io; use My_Io; 
-- Simulation du jeu des tours de hanoi

procedure Hanoi is 

  type Typ_Support is (A, B, C); 
  Origine, Destination : Typ_Support; 
  package Support_Io is 
    new Ada.Text_Io.Enumeration_Io(Typ_Support); 
  Supports_Identiques : exception; 

  subtype Typ_Numero_De_Disque is Positive; 
  Nombre_De_Disques      : Typ_Numero_De_Disque; 

  Nombre_De_Deplacements : Long_Long_Integer;
  Compteur_Overflow      : exception;

  Reponse                : Character;
  Trace                  : Boolean;

  Temps                  : Ada.Calendar.Time;
  Seconde                : Ada.Calendar.Day_Duration;
  package Duration_Io is
    new Ada.Text_Io.Fixed_Io(Duration);

  function Determine_Intermediaire (
   Premier_Support, Deuxieme_Support : Typ_Support) return Typ_Support is
  begin
    case Premier_Support is
      when A =>
        case Deuxieme_Support is
          when B =>
            return C;
          when C =>
            return B;
          when others =>
            null;
        end case;
      when B =>
        case Deuxieme_Support is
          when C =>
            return A;
          when A =>
            return C;
          when others =>
            null;
        end case;
      when C =>
        case Deuxieme_Support is
          when A =>
            return B;
          when B =>
            return A;
          when others =>
            null;
        end case;
    end case;
    Put_Line("PROBLEME: Deux supports identiques.");
    raise Supports_Identiques;
  end Determine_Intermediaire;

  procedure Deplacer (
   Origine, Intermediaire, Destination : in Typ_Support;
   Numero_De_Disque                    : in Typ_Numero_De_Disque) is
  begin
    begin
      Deplacer(Origine, Destination, Intermediaire, Typ_Numero_De_Disque'Pred(
        Numero_De_Disque));
    exception
      when Constraint_Error =>
        null;
    end; 

    if Trace then 
      Put("Deplacer le disque "); 
      Put(Numero_De_Disque); 
      Put(" de "); 
      Support_Io.Put(Origine); 
      Put(" vers "); 
      Support_Io.Put(Destination); 
      Put_Line("."); 
    end if; 

    begin
      Nombre_De_Deplacements := Long_Long_Integer'Succ(Nombre_De_Deplacements); 
    exception
      when others => 
        raise Compteur_Overflow; 
    end; 

    begin
      Deplacer(Intermediaire, Origine, Destination, Typ_Numero_De_Disque'Pred(
        Numero_De_Disque)); 
    exception
      when Constraint_Error => 
        null; 
    end; 

  end Deplacer; 

begin -- hanoi
  Put_Line("Probleme des tours de HANOI:"); 
  loop
    New_Line; 

    loop
      begin
        Put("Entrez le support de depart (a, b, c) ? "); 
        Support_Io.Get(Origine); 
        New_Line; 
        exit;
      exception
        when others => 
          Skip_Line; 
          Put_Line("ERREUR de saisie, recommencez."); 
      end; 
    end loop; 

    loop
      begin
        Put("Entrez le support d'arrivee (a, b, c) ? "); 
        Support_Io.Get(Destination); 
        if (Destination = Origine) then 
          raise Supports_Identiques; 
        end if; 
        New_Line; 
        exit; 
      exception
        when Supports_Identiques => 
          Skip_Line; 
          Put_Line("ERREUR, les deux supports sont identiques. Recommencez."); 
        when others => 
          Skip_Line; 
          Put_Line("ERREUR de saisie, recommencez."); 
      end; 
    end loop; 

    loop
      begin
        Put("Entrez le nombre de disques (au moins 1) ? "); 
        Get(Nombre_De_Disques); 
        New_Line; 
        exit; 
      exception
        when others => 
          Skip_Line; 
          Put_Line("ERREUR, recommencez."); 
      end; 
    end loop; 

    loop
      begin
        Put("Voulez-vous une trace detaillee (O/N) ? "); 
        Get(Reponse); 
        if (Reponse /= 'o') and then (Reponse /= 'O') and then
           (Reponse /= 'n') and then (Reponse /= 'N') then
          raise Constraint_Error;
        end if; 
        New_Line; 
        Trace := ((Reponse = 'o') or else (Reponse = 'O'));
        exit; 
      exception
        when others => 
          Skip_Line; 
          Put_Line("ERREUR, recommencez."); 
      end; 
    end loop; 

    New_Line; 

    begin
      Nombre_De_Deplacements := 0; 
      Temps := Ada.Calendar.Clock; 
      Deplacer(Origine, Determine_Intermediaire(Origine, Destination), 
        Destination, Nombre_De_Disques); 
      Seconde := Ada.Calendar."-"(Ada.Calendar.Clock, Temps); 

      New_Line; 
      Put("Transfert de "); 
      Put(Nombre_De_Disques); 
      Put(" disques de "); 
      Support_Io.Put(Origine); 
      Put(" vers "); 
      Support_Io.Put(Destination); 
      Put_Line(" ."); 

      Put("Operation effectuee en "); 
      Put(Nombre_De_Deplacements); 
      Put_Line(" deplacements "); 
      Put(" et en "); 
      Duration_Io.Put(Seconde); 
      Put_Line(" secondes."); 

    exception
      when Compteur_Overflow => 
        Put("Depassement de la capacite du compteur. Mettez moins de disques.")
          ; 
    end; 

  end loop; 

end Hanoi; 

