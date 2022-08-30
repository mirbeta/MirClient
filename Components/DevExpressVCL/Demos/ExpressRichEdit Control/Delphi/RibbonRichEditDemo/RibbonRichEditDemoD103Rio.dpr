program RibbonRichEditDemoD103Rio;

uses
  Forms,
  RibbonRichEditDemoOptions in '..\Common\RibbonRichEditDemoOptions.pas' {TRibbonDemoOptionsForm},
  RichEditControlBase in '..\Common\RichEditControlBase.pas' {frmRichEditControlBase},
  RibbonRichEditDemoGallerySetup in '..\RibbonRichEditMainForm\RibbonRichEditDemoGallerySetup.pas' {TColorDialogSetupForm},
  ColorPicker in '..\RibbonRichEditMainForm\ColorPicker.pas' {ColorPicker},
  RibbonRichEditMainForm in '..\RibbonRichEditMainForm\RibbonRichEditMainForm.pas' {RibbonRichEditMainForm},
  RibbonRichEditForm in 'RibbonRichEditForm.pas' {frmRibbonRichEditForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  EBarsUtils in '..\Common\EBarsUtils.pas' {dmCommonData: TDataModule},
  EBarsDemoRating in '..\Common\EBarsDemoRating.pas' {EBarsDemoRatingForm};

  {$R *.res}
  {$R WindowsXP.res}

{$IF defined(VER220) or defined(VER210) or defined(VER185) or defined(VER150)}
  {$R WindowsXP.res}
{$IFEND}

begin
  Application.Title := 'Rich Edit Demo';
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.Initialize;
  Application.CreateForm(TfrmRibbonRichEditForm, frmRibbonRichEditForm);
  Application.CreateForm(TColorDialogSetupForm, ColorDialogSetupForm);
  Application.Run;
end.
