program FeaturesDemoD103Rio;

uses
  dxGDIPlusAPI,
  Forms,
  FeaturesMain in 'FeaturesMain.pas' {fmFeaturesMain},
  FeaturesDialog in 'FeaturesDialog.pas' {fmFeaturesDailog},
  NavBarUtils in '..\Common\NavBarUtils.pas' {dmCommonData: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


var
  ADialog: TForm;
begin
  Application.Initialize;
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TfmFeaturesMain, fmFeaturesMain);
  if not CheckGdiPlus then
  begin
    ADialog := TfmFeaturesDailog.Create(Application);
    ADialog.ShowModal;
    ADialog.Free;
  end;
  Application.Run;
end.
