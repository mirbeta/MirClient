program LayoutViewCarouselModeDemoD103Rio;

uses
  Forms,
  LayoutViewCarouselModeDemoMain in 'LayoutViewCarouselModeDemoMain.pas' {frmMain},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid LayoutView Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
