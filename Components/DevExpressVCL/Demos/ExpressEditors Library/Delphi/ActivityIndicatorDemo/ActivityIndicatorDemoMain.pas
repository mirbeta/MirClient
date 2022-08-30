unit ActivityIndicatorDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Forms, ComCtrls, BaseForm, Menus, Classes, Controls, StdCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxPC, cxContainer, cxEdit, cxGroupBox, dxActivityIndicator,
  cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, dxBevel, cxDropDownEdit, cxColorComboBox, cxCheckBox;

type

  { TdxActivityIndicatorDemoForm }

  TdxActivityIndicatorDemoForm = class(TfmBaseForm)
    ActivityIndicator: TdxActivityIndicator;
    bvlSeparator: TdxBevel;
    ccbArcColor: TcxColorComboBox;
    ccbDotColor: TcxColorComboBox;
    gbArcBased: TcxGroupBox;
    gbCommon: TcxGroupBox;
    gbDotBased: TcxGroupBox;
    gbSettings: TcxGroupBox;
    lbAnimationTime: TcxLabel;
    lbArcColor: TcxLabel;
    lbArcThickness: TcxLabel;
    lbDotColor: TcxLabel;
    lbDotCount: TcxLabel;
    lbDotSize: TcxLabel;
    seAnimationTime: TcxSpinEdit;
    seArcThickness: TcxSpinEdit;
    seDotCount: TcxSpinEdit;
    seDotSize: TcxSpinEdit;
    tcMain: TcxTabControl;

    procedure FormCreate(Sender: TObject);
    procedure seAnimationTimePropertiesChange(Sender: TObject);
    procedure seArcPropertiesChange(Sender: TObject);
    procedure seDotPropertiesChange(Sender: TObject);
    procedure tcMainChange(Sender: TObject);
  private
    FLoading: Boolean;
  end;

var
  dxActivityIndicatorDemoForm: TdxActivityIndicatorDemoForm;

implementation

uses
  dxCoreGraphics;

{$R *.dfm}

{ TdxActivityIndicatorDemoForm }

procedure TdxActivityIndicatorDemoForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  tcMain.Properties.Tabs.BeginUpdate;
  try
    tcMain.Properties.Tabs.Clear;
    for I := 0 to GetRegisteredActivityIndicatorProperties.Count - 1 do
      tcMain.Properties.Tabs.Add(GetRegisteredActivityIndicatorProperties.Descriptions[I]);
  finally
    tcMain.Properties.Tabs.EndUpdate;
  end;
  tcMainChange(Sender);
end;

procedure TdxActivityIndicatorDemoForm.seAnimationTimePropertiesChange(Sender: TObject);
begin
  ActivityIndicator.Properties.AnimationTime := seAnimationTime.Value;
end;

procedure TdxActivityIndicatorDemoForm.seArcPropertiesChange(Sender: TObject);
var
  AProperties: TdxActivityIndicatorArcBasedProperties;
begin
  if not FLoading then
    if ActivityIndicator.Properties is TdxActivityIndicatorArcBasedProperties then
    begin
      AProperties := TdxActivityIndicatorArcBasedProperties(ActivityIndicator.Properties);
      AProperties.ArcColor := dxColorToAlphaColor(ccbArcColor.ColorValue);
      AProperties.ArcThickness := seArcThickness.Value;
    end;
end;

procedure TdxActivityIndicatorDemoForm.seDotPropertiesChange(Sender: TObject);
var
  AProperties: TdxActivityIndicatorDotBasedProperties;
begin
  if not FLoading then
    if ActivityIndicator.Properties is TdxActivityIndicatorDotBasedProperties then
    begin
      AProperties := TdxActivityIndicatorDotBasedProperties(ActivityIndicator.Properties);
      AProperties.DotColor := dxColorToAlphaColor(ccbDotColor.ColorValue);
      AProperties.DotCount := seDotCount.Value;
      AProperties.DotSize := seDotSize.Value;
    end;
end;

procedure TdxActivityIndicatorDemoForm.tcMainChange(Sender: TObject);
var
  AArcBasedProperties: TdxActivityIndicatorArcBasedProperties;
  ADotBasedProperties: TdxActivityIndicatorDotBasedProperties;
begin
  if tcMain.TabIndex < 0 then
    Exit;

  FLoading := True;
  try
    ActivityIndicator.PropertiesClassName := GetRegisteredActivityIndicatorProperties.Items[tcMain.TabIndex].ClassName;
    seAnimationTime.Value := ActivityIndicator.Properties.AnimationTime;

    gbArcBased.Visible := ActivityIndicator.Properties is TdxActivityIndicatorArcBasedProperties;
    if gbArcBased.Visible then
    begin
      AArcBasedProperties := TdxActivityIndicatorArcBasedProperties(ActivityIndicator.Properties);
      seArcThickness.Value := AArcBasedProperties.ArcThickness;
      ccbArcColor.ColorValue := dxAlphaColorToColor(AArcBasedProperties.ArcColor);
    end;

    gbDotBased.Visible := ActivityIndicator.Properties is TdxActivityIndicatorDotBasedProperties;
    if gbDotBased.Visible then
    begin
      ADotBasedProperties := TdxActivityIndicatorDotBasedProperties(ActivityIndicator.Properties);
      ccbDotColor.ColorValue := dxAlphaColorToColor(ADotBasedProperties.DotColor);
      seDotCount.Value := ADotBasedProperties.DotCount;
      seDotSize.Value := ADotBasedProperties.DotSize;
    end;
  finally
    FLoading := False;
  end;
end;

end.
