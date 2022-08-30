program AlphaBlendingDemoD103Rio;

uses
  dxGDIPlusAPI,
  Forms,
  AlphaBlendingMain in 'AlphaBlendingMain.pas' {fmAlphaBlendingMain},
  AlphaBlendingDialog in 'AlphaBlendingDialog.pas' {fmAlphaBlendingDailog};

  {$R *.res}
  {$R WindowsXP.res}


var
  ADialog: TForm;
begin
  Application.Initialize;
  Application.CreateForm(TfmAlphaBlendingMain, fmAlphaBlendingMain);
  if not CheckGdiPlus then
  begin
    ADialog := TfmAlphaBlendingDailog.Create(Application);
    ADialog.ShowModal;
    ADialog.Free;
  end;
  Application.Run;
end.
