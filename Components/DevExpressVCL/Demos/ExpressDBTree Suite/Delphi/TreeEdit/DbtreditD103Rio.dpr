program dbtredit;

uses
  Forms,
  main in 'main.pas' {FMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
