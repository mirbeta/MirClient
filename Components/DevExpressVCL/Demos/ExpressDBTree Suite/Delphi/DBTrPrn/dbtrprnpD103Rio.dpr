program dbtrprnpD103Rio;

uses
  Forms,
  main in 'main.pas' {FMain},
  dbtreeqr in 'dbtreeqr.pas' {QRListForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TQRListForm, QRListForm);
  Application.Run;
end.
