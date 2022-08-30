program FlexibleLayoutDemoD103Rio;

uses
  Forms,
  DemoDM in '..\common\DemoDM.pas' {dmDemo: TDataModule},
  FlexibleLayoutDemoMain in 'FlexibleLayoutDemoMain.pas' {fmFlexibleLayoutDemooMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmDemo, dmDemo);
  Application.CreateForm(TfmFlexibleLayoutDemoMain, fmFlexibleLayoutDemoMain);
  Application.Run;
end.
