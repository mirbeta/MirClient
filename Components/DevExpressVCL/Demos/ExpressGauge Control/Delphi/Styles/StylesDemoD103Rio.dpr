program StylesDemoD103Rio;

uses
  Forms,
  uMain in 'uMain.pas' {frmGaugeStyles};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmGaugeStyles, frmGaugeStyles);
  Application.Run;
end.
