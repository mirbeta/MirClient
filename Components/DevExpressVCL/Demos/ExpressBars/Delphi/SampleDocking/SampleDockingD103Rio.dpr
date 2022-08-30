program SampleDockingD103Rio;

uses
  Forms,
  SampleDockingMain in 'SampleDockingMain.pas' {SampleDockingMainForm},
  EBarsDemoRating in '..\Common\EBarsDemoRating.pas' {EBarsDemoRatingForm},
  SampleDockingRichText in 'SampleDockingRichText.pas' {SampleDockingRichTextFrame: TForm},
  SampleDockingRadioGroup in 'SampleDockingRadioGroup.pas' {SampleDockingRadioGroupFrame: TForm},
  SampleDockingTreeView in 'SampleDockingTreeView.pas' {SampleDockingTreeViewFrame: TForm},
  SampleDockingListBox in 'SampleDockingListBox.pas' {SampleDockingListBoxFrame: TForm},
  EBarsUtils in '..\Common\EBarsUtils.pas' {dmCommonData: TDataModule},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressBars SampleDockingD103Rio Demo';
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TSampleDockingMainForm, SampleDockingMainForm);
  Application.Run;
end.
