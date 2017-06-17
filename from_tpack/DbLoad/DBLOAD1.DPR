program Dbload1;

uses
  Forms,
  Dbload in 'DBLOAD.PAS' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
