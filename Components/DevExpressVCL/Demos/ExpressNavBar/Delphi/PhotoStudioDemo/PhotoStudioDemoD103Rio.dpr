program PhotoStudioDemoD103Rio;

uses
  Forms,
  PhotoStudioMain in 'PhotoStudioMain.pas' {frmMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
