program ViewBandedFixedDemoD103Rio;

uses
  Forms,
  ViewBandedFixedMain in 'ViewBandedFixedMain.pas' {ViewBandedFixedDemoMainForm},
  ViewBandedFixedMainData in 'ViewBandedFixedMainData.pas' {ViewBandedFixedDemoDMMain: TDataModule},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid ViewBandedFixed Demo';
  Application.CreateForm(TViewBandedFixedDemoMainForm, ViewBandedFixedDemoMainForm);
  Application.CreateForm(TViewBandedFixedDemoDMMain, ViewBandedFixedDemoDMMain);
  Application.Run;
end.
