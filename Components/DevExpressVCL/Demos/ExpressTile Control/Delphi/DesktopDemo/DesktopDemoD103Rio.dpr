program DesktopDemoD103Rio;

uses
  Forms,
  DesktopDemoMain in 'DesktopDemoMain.pas' {DesktopDemoMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TDesktopDemoMainForm, DesktopDemoMainForm);
  Application.Run;
end.
