program BarCodeDemoD103Rio;

uses
  Forms,
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  BarCodeDemoMain in 'BarCodeDemoMain.pas' {dxBarCodeDemoForm};

  {$R *.res}
  {$R WindowsXP.res}

{$IF defined(VER220) or defined(VER210) or defined(VER185) or defined(VER150)}
  {$R WindowsXP.res}
{$IFEND}

begin
  Application.Initialize;
  Application.CreateForm(TdxBarCodeDemoForm, dxBarCodeDemoForm);
  Application.Run;
end.
