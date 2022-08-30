unit AlertWindowDemoOptions;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  Menus, cxContainer, cxEdit, cxCheckBox, cxSpinEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, StdCtrls, cxGroupBox, cxButtons, dxAlertWindow,
  cxLabel;

type
  TFormOptions = class(TForm)
    cbPosition: TcxComboBox;
    seWidth: TcxSpinEdit;
    seHeight: TcxSpinEdit;
    cbAutoWidth: TcxCheckBox;
    cbAutoHeight: TcxCheckBox;
    btOk: TcxButton;
    btCancel: TcxButton;
    lbPosition: TcxLabel;
    lbWidth: TcxLabel;
    lbHeight: TcxLabel;
    gbOptionsManager: TcxGroupBox;
    gbOptionsSize: TcxGroupBox;
    lbMaxAlertShownAtOnce: TcxLabel;
    seMaxInDisplay: TcxSpinEdit;
    gbOptionsBehavior: TcxGroupBox;
    lbTime: TcxLabel;
    seTime: TcxSpinEdit;
    cbCloseOnRightClick: TcxCheckBox;
    cbHotTrack: TcxCheckBox;
    gbOptionsAnimation: TcxGroupBox;
    lbShowingDirection: TcxLabel;
    cbShowingDirection: TcxComboBox;
    lbShowingAnimation: TcxLabel;
    cbShowingAnimation: TcxComboBox;
    lbHidingAnimation: TcxLabel;
    cbHidingAnimation: TcxComboBox;
    cbHidingDirection: TcxComboBox;
    lbHidingDirection: TcxLabel;
    btnApply: TcxButton;
    cbAutoSizeAdjustment: TcxCheckBox;
    cbCollapseEmptySlots: TcxCheckBox;
    procedure btOkClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  public
    procedure LoadWindowOptions(AManager: TdxAlertWindowManager);
    procedure SaveWindowOptions(AManager: TdxAlertWindowManager);
  end;

var
  FormOptions: TFormOptions;

implementation

uses AlertWindowDemoMain;

{$R *.dfm}

{ TFormOptions }

procedure TFormOptions.LoadWindowOptions(AManager: TdxAlertWindowManager);
begin
  cbPosition.ItemIndex := Integer(AManager.WindowPosition);
  cbShowingAnimation.ItemIndex := Integer(AManager.OptionsAnimate.ShowingAnimation);
  cbShowingDirection.ItemIndex := Integer(AManager.OptionsAnimate.ShowingAnimationDirection);
  cbHidingAnimation.ItemIndex := Integer(AManager.OptionsAnimate.HidingAnimation);
  cbHidingDirection.ItemIndex := Integer(AManager.OptionsAnimate.HidingAnimationDirection);
  seWidth.Value := AManager.OptionsSize.Width;
  seHeight.Value := AManager.OptionsSize.Height;
  cbAutoWidth.Checked := AManager.OptionsSize.AutoWidth;
  cbAutoHeight.Checked := AManager.OptionsSize.AutoHeight;
  cbAutoSizeAdjustment.Checked := AManager.OptionsSize.AutoSizeAdjustment;
  seTime.Value := AManager.OptionsBehavior.DisplayTime;
  cbCloseOnRightClick.Checked := AManager.OptionsBehavior.CloseOnRightClick;
  cbHotTrack.Checked := AManager.OptionsAnimate.HotTrack;
  cbCollapseEmptySlots.Checked := AManager.OptionsAnimate.CollapseEmptySlots;
  seMaxInDisplay.Value := AManager.WindowMaxCount;
end;

procedure TFormOptions.SaveWindowOptions(AManager: TdxAlertWindowManager);
begin
  AManager.WindowPosition := TdxAlertWindowPosition(cbPosition.ItemIndex);
  AManager.OptionsSize.Width := seWidth.Value;
  AManager.OptionsSize.Height := seHeight.Value;
  AManager.OptionsSize.AutoWidth := cbAutoWidth.Checked;
  AManager.OptionsSize.AutoHeight := cbAutoHeight.Checked;
  AManager.OptionsSize.AutoSizeAdjustment := cbAutoSizeAdjustment.Checked;
  AManager.OptionsBehavior.DisplayTime := seTime.Value;
  AManager.OptionsBehavior.CloseOnRightClick := cbCloseOnRightClick.Checked;
  AManager.OptionsAnimate.HotTrack := cbHotTrack.Checked;
  AManager.OptionsAnimate.CollapseEmptySlots := cbCollapseEmptySlots.Checked;
  AManager.WindowMaxCount := seMaxInDisplay.Value;
  AManager.OptionsAnimate.ShowingAnimation :=
    TdxAlertWindowAnimation(cbShowingAnimation.ItemIndex);
  AManager.OptionsAnimate.HidingAnimation :=
    TdxAlertWindowAnimation(cbHidingAnimation.ItemIndex);
  AManager.OptionsAnimate.ShowingAnimationDirection :=
    TdxAlertWindowMovingDirection(cbShowingDirection.ItemIndex);
  AManager.OptionsAnimate.HidingAnimationDirection :=
    TdxAlertWindowMovingDirection(cbHidingDirection.ItemIndex);
end;

procedure TFormOptions.btOkClick(Sender: TObject);
begin
  SaveWindowOptions(AlertWindowDemoForm.dxAlertWindowManager1);
  Hide;
end;

procedure TFormOptions.btnApplyClick(Sender: TObject);
begin
  SaveWindowOptions(AlertWindowDemoForm.dxAlertWindowManager1);
end;

procedure TFormOptions.btCancelClick(Sender: TObject);
begin
  Hide;
end;

end.
