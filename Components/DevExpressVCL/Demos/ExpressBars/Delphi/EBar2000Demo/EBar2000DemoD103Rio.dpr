program EBar2000DemoD103Rio;

uses
  Forms,
  EBar2000DemoMain in 'EBar2000DemoMain.pas' {EBar2000DemoMainForm},
  EBarsDemoRating in '..\Common\EBarsDemoRating.pas' {EBarsDemoRatingForm},
  EBarsUtils in '..\Common\EBarsUtils.pas' {dmCommonData: TDataModule},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressBar Demo (Enchanced Style)';
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TEBar2000DemoMainForm, EBar2000DemoMainForm);
  Application.Run;
end.
