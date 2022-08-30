program RibbonNotepadDemoD103Rio;

uses
  Forms,
  RibbonNotepadMainForm in 'RibbonNotepadMainForm.pas' {frmRibbonNotepadMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  NotepadChildForm in '..\Common\NotepadChildForm.pas' {frmNotepadChild},
  NotepadMainForm in '..\Common\NotepadMainForm.pas' {frmNotepadMain},
  RibbonNotepadDemoGallerySetup in 'RibbonNotepadDemoGallerySetup.pas',
  EBarsUtils in '..\Common\EBarsUtils.pas' {dmCommonData: TDataModule},
  EBarsDemoRating in '..\Common\EBarsDemoRating.pas' {EBarsDemoRatingForm},
  RibbonNotepadDemoOptions in 'RibbonNotepadDemoOptions.pas' {RibbonDemoOptionsForm},
  RibbonNotepadChildForm in 'RibbonNotepadChildForm.pas' {frmRibbonNotepadChild};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TfrmRibbonNotepadMain, frmRibbonNotepadMain);
  Application.CreateForm(TColorDialogSetupForm, ColorDialogSetupForm);
  Application.Run;
end.
