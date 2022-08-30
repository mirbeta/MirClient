program GroupControlDemoD103Rio;

uses
  Forms,
  GroupControlMain in 'GroupControlMain.pas' {fmGroupControlMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TfmGroupControlMain, fmGroupControlMain);
  Application.Run;
end.
