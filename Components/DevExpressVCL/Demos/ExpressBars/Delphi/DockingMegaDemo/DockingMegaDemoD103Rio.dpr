program DockingMegaDemoD103Rio;

uses
  Forms,
  DockingMegaDemoMain in 'DockingMegaDemoMain.pas' {DockingMegaDemoMainForm},
  EBarsDemoRating in '..\Common\EBarsDemoRating.pas' {EBarsDemoRatingForm},
  EBarsUtils in '..\Common\EBarsUtils.pas' {dmCommonData: TDataModule},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressBars DockingMega Demo';
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TDockingMegaDemoMainForm, DockingMegaDemoMainForm);
  Application.CreateForm(TEBarsDemoRatingForm, EBarsDemoRatingForm);
  Application.Run;
end.
