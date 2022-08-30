program NotesDemoD103Rio;

uses
  Forms,
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  EBarsUtils in '..\Common\EBarsUtils.pas' {dmCommonData: TDataModule},
  EBarsDemoRating in '..\Common\EBarsDemoRating.pas' {EBarsDemoRatingForm},
  NotepadChildForm in '..\Common\NotepadChildForm.pas' {frmNotepadChild},
  NotepadMainForm in '..\Common\NotepadMainForm.pas' {frmNotepadMain},
  NotesMainForm in 'NotesMainForm.pas' {fmNotesMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TfmNotesMainForm, fmNotesMainForm);
  Application.Run;
end.
