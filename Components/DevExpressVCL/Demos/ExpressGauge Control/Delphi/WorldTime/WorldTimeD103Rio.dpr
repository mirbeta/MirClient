program WorldTimeD103Rio;

uses
  Forms,
  uWorldTime in 'uWorldTime.pas' {frmWorldTime};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'World Time';
  Application.CreateForm(TfrmWorldTime, frmWorldTime);
  Application.Run;
end.
