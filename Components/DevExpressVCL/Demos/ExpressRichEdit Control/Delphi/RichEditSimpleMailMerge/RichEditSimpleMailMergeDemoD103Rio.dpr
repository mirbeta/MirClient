program RichEditSimpleMailMergeDemoD103Rio;

uses
  Forms,
  RibbonRichEditDemoOptions in '..\Common\RibbonRichEditDemoOptions.pas' {RibbonRichEditDemoOptions},
  DBUriStreamProvider in '..\Common\DBUriStreamProvider.pas' {DBUriStreamProvider},
  RichEditControlBase in '..\Common\RichEditControlBase.pas' {frmRichEditControlBase},
  RibbonRichEditDemoGallerySetup in '..\RibbonRichEditMainForm\RibbonRichEditDemoGallerySetup.pas' {RibbonRichEditDemoGallerySetup},
  ColorPicker in '..\RibbonRichEditMainForm\ColorPicker.pas' {ColorPicker},
  RibbonRichEditMainForm in '..\RibbonRichEditMainForm\RibbonRichEditMainForm.pas' {RibbonRichEditMainForm},
  RichEditSimpleMailMerge in 'RichEditSimpleMailMerge.pas' {frmRichEditSimpleMailMerge};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Title := 'Rich Edit Simple Mail Merge Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRichEditSimpleMailMerge, frmRichEditSimpleMailMerge);
  Application.Run;
end.
