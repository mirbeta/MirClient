program ImageSliderDemoD103Rio;

uses
  Forms,
  ImageSliderDemoMain in 'ImageSliderDemoMain.pas' {frmImageSlider},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}

{$IF defined(VER220) or defined(VER210) or defined(VER185) or defined(VER150)}
  {$R WindowsXP.res}
{$IFEND}

begin
  Application.Initialize;
  Application.Title := 'ExpressEditors ImageSlider Demo';
  Application.CreateForm(TfrmImageSlider, frmImageSlider);
  Application.Run;
end.
