unit BarCodeDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  BaseForm, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxDropDownEdit, cxColorComboBox, cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, cxGroupBox, dxBevel,
  cxPC, Menus, StdCtrls, dxToggleSwitch, cxCheckBox, ExtCtrls, dxBarCode, cxMemo;

type
  TdxBarCodeDemoForm = class(TfmBaseForm)
    tcMain: TcxTabControl;
    bvlSeparator: TdxBevel;
    gbSettings: TcxGroupBox;
    Panel1: TPanel;
    BarCode: TdxBarCode;
    pcSettings: TcxPageControl;
    tsBaseProperties: TcxTabSheet;
    tsCustomProperties: TcxTabSheet;
    gbFontSize: TcxGroupBox;
    lbFontSize: TcxLabel;
    seFontSize: TcxSpinEdit;
    gbRotationAngle: TcxGroupBox;
    lbRotationAngle: TcxLabel;
    cbRotationAngle: TcxComboBox;
    gbModuleWidth: TcxGroupBox;
    lbModuleWidth: TcxLabel;
    seModuleWidth: TcxSpinEdit;
    gbFitMode: TcxGroupBox;
    lbModuleHeight: TcxLabel;
    cbFitMode: TcxComboBox;
    gbShowText: TcxGroupBox;
    lbShowText: TcxLabel;
    tsShowText: TdxToggleSwitch;
    gbWideNarrowRatio: TcxGroupBox;
    lbWideNarrowRatio: TcxLabel;
    seWideNarrowRatio: TcxSpinEdit;
    gbCalculateCheckSum: TcxGroupBox;
    lbCalculateCheckSum: TcxLabel;
    tsCalculateCheckSum: TdxToggleSwitch;
    gbCharacterSet: TcxGroupBox;
    lbCharacterSet: TcxLabel;
    cbCharacterSet: TcxComboBox;
    gbCompactionMode: TcxGroupBox;
    lbCompactionMode: TcxLabel;
    cbCompactionMode: TcxComboBox;
    gbErrorCorrectionLevel: TcxGroupBox;
    lbErrorCorrectionLevel: TcxLabel;
    cbErrorCorrectionLevel: TcxComboBox;
    gbSizeVersion: TcxGroupBox;
    lbSizeVersion: TcxLabel;
    cbSizeVersion: TcxComboBox;
    lbCodeText: TcxLabel;
    memText: TcxMemo;
    procedure FormCreate(Sender: TObject);
    procedure tcMainChange(Sender: TObject);
    procedure tsShowTextPropertiesChange(Sender: TObject);
    procedure cbFitModePropertiesChange(Sender: TObject);
    procedure seWideNarrowRatioPropertiesChange(Sender: TObject);
    procedure cbCharacterSetPropertiesChange(Sender: TObject);
    procedure tsCalculateCheckSumPropertiesChange(Sender: TObject);
    procedure cbCompactionModePropertiesChange(Sender: TObject);
    procedure cbErrorCorrectionLevelPropertiesChange(Sender: TObject);
    procedure cbSizeVersionPropertiesChange(Sender: TObject);
    procedure memTextPropertiesChange(Sender: TObject);
    procedure seFontSizePropertiesChange(Sender: TObject);
    procedure seModuleWidthPropertiesChange(Sender: TObject);
    procedure cbRotationAnglePropertiesChange(Sender: TObject);
  private
    FLoading: Boolean;

    procedure SetWideNarrowRatio;
    procedure SetCalculateCheckSum;
  end;

var
  dxBarCodeDemoForm: TdxBarCodeDemoForm;

implementation

{$R *.dfm}

uses
  Math,
  dxBarCodeUtils;

const
  FIndexToSymbologyClassName: array[0..12] of string = ('TdxBarCode11Symbology', 'TdxBarCode39Symbology',
    'TdxBarCode39ExtendedSymbology', 'TdxBarCode93Symbology', 'TdxBarCode93ExtendedSymbology', 'TdxBarCode128Symbology',
    'TdxBarCodeEAN8Symbology', 'TdxBarCodeEAN13Symbology', 'TdxBarCodeInterleaved2Of5Symbology',
    'TdxBarCodeMSISymbology', 'TdxBarCodeUPCASymbology', 'TdxBarCodeUPCESymbology', 'TdxBarCodeQRCodeSymbology');


procedure TdxBarCodeDemoForm.FormCreate(Sender: TObject);
begin
  tcMain.TabIndex := 0;
  memText.Text := '0123456789000';
  tcMainChange(Sender);
end;

procedure TdxBarCodeDemoForm.memTextPropertiesChange(Sender: TObject);
begin
  if not FLoading then
    BarCode.Text := memText.Text;
end;

procedure TdxBarCodeDemoForm.seFontSizePropertiesChange(Sender: TObject);
begin
  if not FLoading and (seFontSize.Value > 0) then
    BarCode.Style.Font.Size := seFontSize.Value;
end;

procedure TdxBarCodeDemoForm.seModuleWidthPropertiesChange(Sender: TObject);
begin
  if not FLoading and (seModuleWidth.Value > 0) then
    BarCode.Properties.ModuleWidth := seModuleWidth.Value;
end;

procedure TdxBarCodeDemoForm.cbCharacterSetPropertiesChange(Sender: TObject);
begin
  if not FLoading and (BarCode.Properties.Symbology is TdxBarCode128Symbology) then
      TdxBarCode128Symbology(BarCode.Properties.Symbology).CharacterSet := TdxBarCode128CharacterSet(cbCharacterSet.ItemIndex);
end;

procedure TdxBarCodeDemoForm.cbCompactionModePropertiesChange(Sender: TObject);
begin
  if not FLoading and (BarCode.Properties.Symbology is TdxBarCodeQRCodeSymbology) then
      TdxBarCodeQRCodeSymbology(BarCode.Properties.Symbology).CompactionMode := TdxQRCodeCompactionMode(cbCompactionMode.ItemIndex);
end;

procedure TdxBarCodeDemoForm.cbErrorCorrectionLevelPropertiesChange(Sender: TObject);
begin
  if not FLoading and (BarCode.Properties.Symbology is TdxBarCodeQRCodeSymbology) then
      TdxBarCodeQRCodeSymbology(BarCode.Properties.Symbology).ErrorCorrectionLevel := TdxQRCodeErrorCorrectionLevel(cbErrorCorrectionLevel.ItemIndex);
end;

procedure TdxBarCodeDemoForm.cbFitModePropertiesChange(Sender: TObject);
begin
  if not FLoading then
    BarCode.Properties.FitMode := TdxBarCodeFitMode(cbFitMode.ItemIndex);
end;

procedure TdxBarCodeDemoForm.cbRotationAnglePropertiesChange(Sender: TObject);
begin
  if not FLoading then
    BarCode.Properties.RotationAngle := TcxRotationAngle(cbRotationAngle.ItemIndex);
end;

procedure TdxBarCodeDemoForm.cbSizeVersionPropertiesChange(Sender: TObject);
begin
  if not FLoading and (BarCode.Properties.Symbology is TdxBarCodeQRCodeSymbology) then
      TdxBarCodeQRCodeSymbology(BarCode.Properties.Symbology).Version := cbSizeVersion.ItemIndex;
end;

procedure TdxBarCodeDemoForm.SetCalculateCheckSum;
begin
  if BarCode.Properties.Symbology is TdxBarCodeInterleaved2Of5Symbology then
    TdxBarCodeInterleaved2Of5Symbology(BarCode.Properties.Symbology).Checksum := tsCalculateCheckSum.Checked
  else
    if BarCode.Properties.Symbology is TdxBarCode39Symbology then
      TdxBarCode39Symbology(BarCode.Properties.Symbology).Checksum := tsCalculateCheckSum.Checked
    else
      if BarCode.Properties.Symbology is TdxBarCode39ExtendedSymbology then
        TdxBarCode39ExtendedSymbology(BarCode.Properties.Symbology).Checksum := tsCalculateCheckSum.Checked;
end;

procedure TdxBarCodeDemoForm.SetWideNarrowRatio;
begin
  if BarCode.Properties.Symbology is TdxBarCodeInterleaved2Of5Symbology then
    TdxBarCodeInterleaved2Of5Symbology(BarCode.Properties.Symbology).WideNarrowRatio := seWideNarrowRatio.Value
  else
    if BarCode.Properties.Symbology is TdxBarCode39Symbology then
      TdxBarCode39Symbology(BarCode.Properties.Symbology).WideNarrowRatio := seWideNarrowRatio.Value
    else
      if BarCode.Properties.Symbology is TdxBarCode39ExtendedSymbology then
        TdxBarCode39ExtendedSymbology(BarCode.Properties.Symbology).WideNarrowRatio := seWideNarrowRatio.Value;
end;

procedure TdxBarCodeDemoForm.seWideNarrowRatioPropertiesChange(Sender: TObject);
begin
  if not FLoading and (seWideNarrowRatio.Value > 0) then
    SetWideNarrowRatio;
end;

procedure TdxBarCodeDemoForm.tsCalculateCheckSumPropertiesChange(Sender: TObject);
begin
  if not FLoading then
    SetCalculateCheckSum;
end;

procedure TdxBarCodeDemoForm.tsShowTextPropertiesChange(Sender: TObject);
begin
  if not FLoading then
    BarCode.Properties.ShowText := tsShowText.Checked;
end;

procedure TdxBarCodeDemoForm.tcMainChange(Sender: TObject);
begin
  if tcMain.TabIndex < 0 then
    Exit;

  FLoading := True;
  try
    BarCode.Style.Font.Size := seFontSize.Value;
    BarCode.Properties.BeginUpdate;
    try
      BarCode.Properties.BarCodeSymbologyClassName := FIndexToSymbologyClassName[tcMain.TabIndex];
      pcSettings.Pages[1].Enabled := (tcMain.TabIndex in [1, 2, 5, 8, 12]);
      if not pcSettings.Pages[1].Enabled then
        pcSettings.ActivePageIndex := 0;

      gbFontSize.Visible := not (tcMain.TabIndex in [6, 7, 10, 11]);
      gbWideNarrowRatio.Visible := tcMain.TabIndex in [1, 2, 8];
      gbWideNarrowRatio.Top := 500;
      gbCalculateCheckSum.Visible := gbWideNarrowRatio.Visible;
      gbCalculateCheckSum.Top := 500;
      gbCharacterSet.Visible := tcMain.TabIndex = 5;
      gbCalculateCheckSum.Top := 500;
      gbCompactionMode.Visible := tcMain.TabIndex = 12;
      gbCalculateCheckSum.Top := 500;
      gbErrorCorrectionLevel.Visible := gbCompactionMode.Visible;
      gbCalculateCheckSum.Top := 500;
      gbSizeVersion.Visible := gbCompactionMode.Visible;
      gbCalculateCheckSum.Top := 500;
      BarCode.Properties.RotationAngle := TcxRotationAngle(cbRotationAngle.ItemIndex);
      BarCode.Properties.FitMode := TdxBarCodeFitMode(cbFitMode.ItemIndex);
      if BarCode.Properties.Symbology is TdxBarCode128Symbology then
        TdxBarCode128Symbology(BarCode.Properties.Symbology).CharacterSet := TdxBarCode128CharacterSet(cbCharacterSet.ItemIndex);
      if BarCode.Properties.Symbology is TdxBarCodeQRCodeSymbology then
      begin
        TdxBarCodeQRCodeSymbology(BarCode.Properties.Symbology).CompactionMode := TdxQRCodeCompactionMode(cbCompactionMode.ItemIndex);
        TdxBarCodeQRCodeSymbology(BarCode.Properties.Symbology).ErrorCorrectionLevel := TdxQRCodeErrorCorrectionLevel(cbErrorCorrectionLevel.ItemIndex);
        TdxBarCodeQRCodeSymbology(BarCode.Properties.Symbology).Version := cbSizeVersion.ItemIndex;
      end;
      SetWideNarrowRatio;
      SetCalculateCheckSum;
      case tcMain.TabIndex of
        0: memText.Text := '01234-56789';
        1, 3: memText.Text := 'ABC-1234';
        2, 4, 5: memText.Text := 'Abc-123';
        6, 11: memText.Text := '0123456';
        7, 8, 9: memText.Text := '012345678901';
        10: memText.Text := '01234567890';
        12: memText.Text := 'https://www.devexpress.com/';
      end;
      if tcMain.TabIndex = 12 then
        seModuleWidth.Value := 5
      else
        seModuleWidth.Value := 2;

      tsShowText.Checked := tcMain.TabIndex <> 12;
      BarCode.Text := memText.Text;
      BarCode.Properties.ShowText := tsShowText.Checked;
      BarCode.Properties.ModuleWidth := seModuleWidth.Value;
    finally
      BarCode.Properties.EndUpdate;
    end;
  finally
    FLoading := False;
  end;
end;

end.
