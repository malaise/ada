with Protected_Put, Images, Rnd;
procedure Accountants is

  -- The account: Balance is always >=0
  protected Account is
    procedure Deposit (Amount : in Positive);
    procedure Withdraw (Amount : in Positive; Ok : out Boolean);
    function Read return Natural;
  private
    Balance : Natural := 0;
  end Account;

  protected body Account is
    procedure Deposit (Amount : in Positive) is
    begin
      Balance := Balance + Amount;
    end Deposit;

    procedure Withdraw (Amount : in Positive; Ok : out Boolean) is
    begin
      if Amount > Balance then
        Ok := False;
      else
         Balance := Balance - Amount;
         Ok := True;
      end if;
    end Withdraw;

    function Read return Natural is
    begin
      return Balance;
    end Read;
  end Account;

  -- Actions
  type Action_List is (Deposit, Withdraw, Read);
  function Action_Rnd is new Rnd.Discr_Random (Action_List);

  -- An accountant
  task type Accountant is
    entry Birth (Num : in Positive);
    entry Dye;
  end Accountant;

  task body Accountant is
    Name : Positive;
    Action : Action_List;
    Amount : Natural;
    Live, Ok : Boolean;
  begin
    accept Birth (Num : in Positive) do
      Name := Num;
    end Birth;
    -- That's life
    Live := True;
    loop
      select
        -- Dye
        accept Dye do
          Live := False;
        end Dye;
      or
        -- or wait a bit
        delay Rnd.Gen.Dur_Random (0.1, 1.0);
      end select;
      exit when not Live;
      -- Do something
      -- Choose an action
      Action :=  Action_Rnd (Rnd.Gen.all);
      case Action is
        when Deposit =>
          Amount := 4000;
          Account.Deposit (Amount);
          Protected_Put.Put_Line_Output (
            "Accountant " & Images.Integer_Image (Name) & " "
            & "Deposit of " & Images.Integer_Image (Amount));
        when Withdraw =>
          -- Choose an amount
          Amount := Rnd.Gen.Int_Random (1, 10000);
          Account.Withdraw (Amount, Ok);
          Protected_Put.Put_Line_Output (
            "Accountant " & Images.Integer_Image (Name) & " "
            & "Withdraw of " & Images.Integer_Image (Amount)
            & (if Ok then " accepted" else " rejected"));
        when Read =>
          Amount := Account.Read;
          Protected_Put.Put_Line_Output (
            "Accountant " & Images.Integer_Image (Name) & " "
            & "Balance is " & Images.Integer_Image (Amount));
      end case;


    end loop;
  end Accountant;

  Family : array (1 .. 5) of Accountant;

begin
  Rnd.Gen.Randomize;
  for I in Family'Range loop
    Family(I).Birth(I);
  end loop;
  delay 30.0;
  for I in Family'Range loop
    Family(I).Dye;
  end loop;
  Protected_Put.Put_Line_Output (
    "Heritage is " & Images.Integer_Image (Account.Read));
end Accountants;

