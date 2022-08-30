program RichEditMasterDetailMailMergeDemoD103Rio;

uses
  Forms,
  RibbonRichEditDemoOptions in '..\Common\RibbonRichEditDemoOptions.pas' {RibbonRichEditDemoOptions},
  DBUriStreamProvider in '..\Common\DBUriStreamProvider.pas' {DBUriStreamProvider},
  RichEditControlBase in '..\Common\RichEditControlBase.pas' {frmRichEditControlBase},
  RichEditMasterDetailMailMerge in 'RichEditMasterDetailMailMerge.pas' {frmRichEditMasterDetailMailMerge};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Title := 'Rich Edit Master-Detail Mail Merge Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRichEditMasterDetailMailMerge, frmRichEditMasterDetailMailMerge);
  Application.Run;
end.
