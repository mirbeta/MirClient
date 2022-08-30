{=======================================================================================================================
  ButtonsFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit ButtonsFrame;

interface

uses
  Forms,
  ImgList,
  Controls,
  Menus,
  RzButton,
  StdCtrls,
  Mask,
  RzEdit,
  RzLabel,
  RzSpnEdt,
  RzPrgres,
  Buttons,
  RzRadChk,
  RzBckgnd,
  RzPanel,
  Classes,
  ExtCtrls,
  RzDlgBtn,
  RzCommon,
  RzBorder;

type
  TFmeButtons = class(TFrame)
    BtnSet: TRzDialogButtons;
    grpTRzButton: TRzGroupBox;
    grpRadChk: TRzGroupBox;
    RzCheckBox1: TRzCheckBox;
    RzRadioButton1: TRzRadioButton;
    RzRadioButton2: TRzRadioButton;
    RzCheckBox2: TRzCheckBox;
    RzButton1: TRzButton;
    RzButton2: TRzButton;
    RzButton4: TRzButton;
    MnuAccount: TPopupMenu;
    MnuNew: TMenuItem;
    MnuEdit: TMenuItem;
    MnuDelete: TMenuItem;
    N1: TMenuItem;
    MnuMakeInactive: TMenuItem;
    MnuShowAllAccounts: TMenuItem;
    N2: TMenuItem;
    MnuUse: TMenuItem;
    MnuFind: TMenuItem;
    N3: TMenuItem;
    MnuPrintList: TMenuItem;
    MnuResortList: TMenuItem;
    grpTRzMenuButton: TRzGroupBox;
    RzMenuButton1: TRzMenuButton;
    grpRepeating: TRzGroupBox;
    BtnIncrease: TRzRapidFireButton;
    SpbRepeat: TRzSpinButtons;
    SpnRepeat: TRzSpinner;
    grpDialogButtons: TRzGroupBox;
    chkShowDivider: TRzCheckBox;
    chkAlignRight: TRzCheckBox;
    chkShowHelp: TRzCheckBox;
    edtOK: TRzEdit;
    RzLabel1: TRzLabel;
    edtCancel: TRzEdit;
    RzLabel2: TRzLabel;
    PbrRepeat: TRzProgressBar;
    chkShowGlyphs: TRzCheckBox;
    RzBitBtn1: TRzBitBtn;
    ImageList1: TImageList;
    grpShapeButtons: TRzGroupBox;
    RzShapeButton1: TRzShapeButton;
    RzShapeButton2: TRzShapeButton;
    RzShapeButton3: TRzShapeButton;
    RzShapeButton4: TRzShapeButton;
    RzShapeButton5: TRzShapeButton;
    RzSeparator1: TRzSeparator;
    SpbRepeat2: TRzSpinButtons;
    chkHotTrack: TRzCheckBox;
    RzMenuController1: TRzMenuController;
    pnlHeader: TRzPanel;
    RzToolbar1: TRzToolbar;
    RzToolButton1: TRzToolButton;
    RzSpacer1: TRzSpacer;
    BtnOpen: TRzToolButton;
    BtnSave: TRzToolButton;
    RzSpacer2: TRzSpacer;
    BtnCut: TRzToolButton;
    BtnCopy: TRzToolButton;
    BtnPaste: TRzToolButton;
    BtnUndo: TRzToolButton;
    BtnRedo: TRzToolButton;
    RzSpacer3: TRzSpacer;
    BtnFind: TRzToolButton;
    BtnReplace: TRzToolButton;
    BtnFindNext: TRzToolButton;
    imlPopup: TImageList;
    MnuPopup: TPopupMenu;
    MnuMailMessage: TMenuItem;
    MenuItem1: TMenuItem;
    MnuAppointment: TMenuItem;
    MnuContact: TMenuItem;
    MnuTask: TMenuItem;
    MnuNote: TMenuItem;
    imlCheckGlyphs: TImageList;
    imlRadioGlyphs: TImageList;
    chkThemeAware: TRzCheckBox;
    procedure chkShowDividerClick(Sender: TObject);
    procedure chkAlignRightClick(Sender: TObject);
    procedure chkShowHelpClick(Sender: TObject);
    procedure edtOKChange(Sender: TObject);
    procedure edtCancelChange(Sender: TObject);
    procedure SpnRepeatChange(Sender: TObject);
    procedure BtnIncreaseClick(Sender: TObject);
    procedure SpbRepeatUpRightClick(Sender: TObject);
    procedure SpbRepeatDownLeftClick(Sender: TObject);
    procedure chkShowGlyphsClick(Sender: TObject);
    procedure chkHotTrackClick(Sender: TObject);
    procedure RzToolButton1Click(Sender: TObject);
    procedure chkThemeAwareClick(Sender: TObject);
  private
  public
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}

uses
  Graphics,
  ComCtrls,
  Dialogs,
  MainForm;


procedure TFmeButtons.Init;
begin
//  ParentBackground := False;
end;


procedure TFmeButtons.UpdateVisualStyle( VS: TRzVisualStyle;
                                         GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
  RzToolbar1.VisualStyle := VS;
  RzToolbar1.GradientColorStyle := GCS;
  RzMenuController1.GradientColorStyle := GCS;
end;


procedure TFmeButtons.RzToolButton1Click(Sender: TObject);
begin
  ShowMessage( 'Create a new message' );
end;

procedure TFmeButtons.chkShowDividerClick(Sender: TObject);
begin
  BtnSet.ShowDivider := ChkShowDivider.Checked;
end;

procedure TFmeButtons.chkAlignRightClick(Sender: TObject);
begin
  if ChkAlignRight.Checked then
  begin
    ClientWidth := 647;
    ClientHeight := 337;
    BtnSet.Align := alRight
  end
  else
  begin
    ClientWidth := 564;
    ClientHeight := 371;
    BtnSet.Align := alBottom;
  end;
end;

procedure TFmeButtons.chkShowHelpClick(Sender: TObject);
begin
  BtnSet.ShowHelpButton := ChkShowHelp.Checked;
end;

procedure TFmeButtons.chkShowGlyphsClick(Sender: TObject);
begin
  BtnSet.ShowGlyphs := ChkShowGlyphs.Checked;
end;

procedure TFmeButtons.edtOKChange(Sender: TObject);
begin
  BtnSet.CaptionOK := EdtOK.Text;
end;

procedure TFmeButtons.edtCancelChange(Sender: TObject);
begin
  BtnSet.CaptionCancel := EdtCancel.Text;
end;

procedure TFmeButtons.SpnRepeatChange(Sender: TObject);
begin
  PbrRepeat.Percent := SpnRepeat.Value;
end;

procedure TFmeButtons.BtnIncreaseClick(Sender: TObject);
begin
  PbrRepeat.Percent := PbrRepeat.Percent + 1;
end;

procedure TFmeButtons.SpbRepeatUpRightClick(Sender: TObject);
begin
  PbrRepeat.Percent := PbrRepeat.Percent + 1;
end;

procedure TFmeButtons.SpbRepeatDownLeftClick(Sender: TObject);
begin
  PbrRepeat.Percent := PbrRepeat.Percent - 1;
end;

procedure TFmeButtons.chkHotTrackClick(Sender: TObject);
begin
  RzButton1.HotTrack := ChkHotTrack.Checked;
  RzButton2.HotTrack := ChkHotTrack.Checked;
  RzButton4.HotTrack := ChkHotTrack.Checked;
  RzBitBtn1.HotTrack := ChkHotTrack.Checked;
  RzMenuButton1.HotTrack := ChkHotTrack.Checked;
  RzCheckBox1.HotTrack := chkHotTrack.Checked;
  RzCheckBox2.HotTrack := chkHotTrack.Checked;
  RzRadioButton1.HotTrack := chkHotTrack.Checked;
  RzRadioButton2.HotTrack := chkHotTrack.Checked;
  BtnSet.HotTrack := ChkHotTrack.Checked;
  ChkShowDivider.HotTrack := ChkHotTrack.Checked;
  ChkAlignRight.HotTrack := ChkHotTrack.Checked;
  ChkShowHelp.HotTrack := ChkHotTrack.Checked;
  ChkShowGlyphs.HotTrack := ChkHotTrack.Checked;
  ChkHotTrack.HotTrack := ChkHotTrack.Checked;
end;

procedure TFmeButtons.chkThemeAwareClick(Sender: TObject);
begin
  RzButton1.ThemeAware := chkThemeAware.Checked;
  RzButton2.ThemeAware := chkThemeAware.Checked;
  RzButton4.ThemeAware := chkThemeAware.Checked;
  RzBitBtn1.ThemeAware := chkThemeAware.Checked;
  RzMenuButton1.ThemeAware := chkThemeAware.Checked;
end;

end.



