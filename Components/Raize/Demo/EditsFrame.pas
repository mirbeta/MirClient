{=======================================================================================================================
  EditsFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit EditsFrame;

interface

uses
  Forms,
  RzCommon,
  RzEdit,
  Controls,
  StdCtrls,
  RzCmboBx,
  RzBtnEdt,
  RzSpnEdt,
  RzButton,
  RzRadChk,
  Mask,
  RzLabel,
  Classes,
  ExtCtrls,
  RzPanel,
  RzBorder;

type
  TFmeEdits = class(TFrame)
    GrpEdits: TRzGroupBox;
    GrpSpinEdits: TRzGroupBox;
    GrpButtonEdits: TRzGroupBox;
    bedPreview: TRzButtonEdit;
    spnPreview: TRzSpinEdit;
    RzEdit1: TRzEdit;
    RzMaskEdit1: TRzMaskEdit;
    chkSpnFlat: TRzCheckBox;
    chkHorzBtns: TRzCheckBox;
    chkDirection: TRzCheckBox;
    SpnButtonWidth: TRzSpinner;
    RzLabel1: TRzLabel;
    chkBtnFlat: TRzCheckBox;
    chkButtonVisible: TRzCheckBox;
    chkAltBtnVisible: TRzCheckBox;
    CbxButtonKind: TRzComboBox;
    CbxAltBtnKind: TRzComboBox;
    RzLabel2: TRzLabel;
    RzLabel3: TRzLabel;
    GrpDateTimeColor: TRzGroupBox;
    RzLabel5: TRzLabel;
    RzLabel6: TRzLabel;
    Label1: TLabel;
    RzDateTimeEdit1: TRzDateTimeEdit;
    RzDateTimeEdit2: TRzDateTimeEdit;
    RzFrameController1: TRzFrameController;
    pnlHeader: TRzPanel;
    RzGroupBox1: TRzGroupBox;
    RzGroupBox2: TRzGroupBox;
    RzNumericEdit1: TRzNumericEdit;
    chkCalculatorVisible: TRzCheckBox;
    RzColorEdit1: TRzColorEdit;
    RzLabel4: TRzLabel;
    RzLabel7: TRzLabel;
    chkNoColorArea: TRzCheckBox;
    chkDefaultColor: TRzCheckBox;
    chkCustomColorArea: TRzCheckBox;
    chkSystemColors: TRzCheckBox;
    chkColorHints: TRzCheckBox;
    optDisplayFormat3: TRzRadioButton;
    optDisplayFormat1: TRzRadioButton;
    optDisplayFormat2: TRzRadioButton;
    RzLabel8: TRzLabel;
    chkIntegersOnly: TRzCheckBox;
    procedure SpnButtonWidthChange(Sender: TObject);
    procedure chkSpnFlatClick(Sender: TObject);
    procedure chkHorzBtnsClick(Sender: TObject);
    procedure chkDirectionClick(Sender: TObject);
    procedure chkBtnFlatClick(Sender: TObject);
    procedure chkButtonVisibleClick(Sender: TObject);
    procedure chkAltBtnVisibleClick(Sender: TObject);
    procedure CbxButtonKindChange(Sender: TObject);
    procedure CbxAltBtnKindChange(Sender: TObject);
    procedure DisplayFormatClick(Sender: TObject);
    procedure chkCalculatorVisibleClick(Sender: TObject);
    procedure chkIntegersOnlyClick(Sender: TObject);
    procedure chkDefaultColorClick(Sender: TObject);
    procedure chkSystemColorsClick(Sender: TObject);
    procedure chkNoColorAreaClick(Sender: TObject);
    procedure chkCustomColorAreaClick(Sender: TObject);
    procedure chkColorHintsClick(Sender: TObject);
  private
  public
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}

uses
  SysUtils;

procedure TFmeEdits.Init;
begin
//  ParentBackground := False;

  RzDateTimeEdit1.Date := Date;
  RzDateTimeEdit2.Time := Time;
end;


procedure TFmeEdits.UpdateVisualStyle( VS: TRzVisualStyle;
                                       GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
end;


procedure TFmeEdits.SpnButtonWidthChange(Sender: TObject);
begin
  SpnPreview.ButtonWidth := SpnButtonWidth.Value;
end;


procedure TFmeEdits.chkSpnFlatClick(Sender: TObject);
begin
  SpnPreview.FlatButtons := ChkSpnFlat.Checked;
end;


procedure TFmeEdits.chkHorzBtnsClick(Sender: TObject);
begin
  if ChkHorzBtns.Checked then
    SpnPreview.Orientation := orHorizontal
  else
    SpnPreview.Orientation := orVertical;
end;


procedure TFmeEdits.chkDirectionClick(Sender: TObject);
begin
  if ChkDirection.Checked then
    SpnPreview.Direction := sdLeftRight
  else
    SpnPreview.Direction := sdUpDown;
end;


procedure TFmeEdits.chkBtnFlatClick(Sender: TObject);
begin
  BedPreview.FlatButtons := ChkBtnFlat.Checked;
end;


procedure TFmeEdits.chkButtonVisibleClick(Sender: TObject);
begin
  BedPreview.ButtonVisible := ChkButtonVisible.Checked;
end;


procedure TFmeEdits.chkAltBtnVisibleClick(Sender: TObject);
begin
  BedPreview.AltBtnVisible := ChkAltBtnVisible.Checked;
end;


procedure TFmeEdits.CbxButtonKindChange(Sender: TObject);
begin
  BedPreview.ButtonKind := TButtonKind( CbxButtonKind.ItemIndex + 1 );
end;


procedure TFmeEdits.CbxAltBtnKindChange(Sender: TObject);
begin
  BedPreview.AltBtnKind := TButtonKind( CbxAltBtnKind.ItemIndex + 1 );
end;


procedure TFmeEdits.DisplayFormatClick(Sender: TObject);
begin
  RzNumericEdit1.DisplayFormat := TRzRadioButton( Sender ).Caption;
end;


procedure TFmeEdits.chkCalculatorVisibleClick(Sender: TObject);
begin
  RzNumericEdit1.CalculatorVisible := chkCalculatorVisible.Checked;
end;


procedure TFmeEdits.chkIntegersOnlyClick(Sender: TObject);
begin
  RzNumericEdit1.IntegersOnly := chkIntegersOnly.Checked;
end;


procedure TFmeEdits.chkDefaultColorClick(Sender: TObject);
begin
  RzColorEdit1.ShowDefaultColor := chkDefaultColor.Checked;
end;


procedure TFmeEdits.chkSystemColorsClick(Sender: TObject);
begin
  RzColorEdit1.ShowSystemColors := chkSystemColors.Checked;
end;


procedure TFmeEdits.chkNoColorAreaClick(Sender: TObject);
begin
  RzColorEdit1.ShowNoColor := chkNoColorArea.Checked;
end;


procedure TFmeEdits.chkCustomColorAreaClick(Sender: TObject);
begin
  RzColorEdit1.ShowCustomColor := chkCustomColorArea.Checked;
end;


procedure TFmeEdits.chkColorHintsClick(Sender: TObject);
begin
  RzColorEdit1.ShowColorHints := chkColorHints.Checked;
end;

end.
