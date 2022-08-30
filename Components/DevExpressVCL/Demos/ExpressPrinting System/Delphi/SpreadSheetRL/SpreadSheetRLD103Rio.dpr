program SpreadSheetRLD103Rio;

uses
  Forms,
  SpreadSheetRLMain in 'SpreadSheetRLMain.pas' {SpreadSheetRLForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'SpreadSheetRLD103Rio';
  Application.CreateForm(TSpreadSheetRLForm, SpreadSheetRLForm);
  Application.Run;
end.
