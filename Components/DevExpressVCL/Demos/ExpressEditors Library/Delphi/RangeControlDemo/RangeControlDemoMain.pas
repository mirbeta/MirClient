unit RangeControlDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, BaseForm, Menus, StdCtrls, Types,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxRangeControl,
  cxContainer, cxEdit, dxBevel, cxGroupBox, Generics.Collections, Generics.Defaults,
  cxCheckBox, dxTypeHelpers, cxRadioGroup, cxTrackBar, cxLabel, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxCheckComboBox, dxCore;

type
  TChartData = record
    X: Variant;
    Y: Integer;
  end;

  TdxRangeControlDemoForm = class(TfmBaseForm)
    cxGroupBox1: TcxGroupBox;
    rcNumericClient: TdxRangeControl;
    cxGroupBox2: TcxGroupBox;
    rcDateTimeClient: TdxRangeControl;
    cxGroupBox3: TcxGroupBox;
    rcDateTimeHeaderClient: TdxRangeControl;
    cxGroupBox4: TcxGroupBox;
    cxGroupBox6: TcxGroupBox;
    cxGroupBox7: TcxGroupBox;
    rgDateTimeClientContentType: TcxRadioGroup;
    rgNumberClientContentType: TcxRadioGroup;
    chbAutoFormatScaleCaptions: TcxCheckBox;
    tbNumericClient: TcxTrackBar;
    cxLabel1: TcxLabel;
    tbDateTimeClient: TcxTrackBar;
    cxLabel2: TcxLabel;
    cxGroupBox5: TcxGroupBox;
    Options1: TMenuItem;
    ShowRuler1: TMenuItem;
    ShowZoomscrollbar1: TMenuItem;
    Animation1: TMenuItem;
    cxCheckComboBox1: TcxCheckComboBox;
    cxLabel3: TcxLabel;
    procedure Animation1Click(Sender: TObject);
    procedure rcNumericClientDrawContent(ASender: TdxCustomRangeControl; ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rcDateTimeClientDrawContent(ASender: TdxCustomRangeControl; ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean);
    procedure rcDateTimeHeaderClientDrawContent(ASender: TdxCustomRangeControl; ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ShowRuler1Click(Sender: TObject);
    procedure ShowZoomscrollbar1Click(Sender: TObject);
    procedure tbDateTimeClientPropertiesChange(Sender: TObject);
    procedure rgNumberClientContentTypePropertiesEditValueChanged(
      Sender: TObject);
    procedure tbNumericClientPropertiesChange(Sender: TObject);
    procedure rgDateTimeClientContentTypePropertiesEditValueChanged(
      Sender: TObject);
    procedure chbAutoFormatScaleCaptionsPropertiesEditValueChanged(
      Sender: TObject);
    procedure cxCheckComboBox1PropertiesEditValueChanged(Sender: TObject);
  private
    FNumericClientData: TList<TChartData>;
    FDateTimeClientData2: TList<TChartData>;
    FDateTimeClientData1: TList<TChartData>;
    FDateTimeHeaderClientData1: TDictionary<TDateTime, Integer>;
    FDateTimeHeaderClientData2: TDictionary<TDateTime, Integer>;
  public
    procedure AddPolygonPoint(var APolygon: TPoints; X, Y: Integer);
    procedure InitializeChartData;
    function IsDateTimeClientContentLineMode: Boolean;
    function IsNumberClientContentLineMode: Boolean;
  end;

var
  dxRangeControlDemoForm: TdxRangeControlDemoForm;

implementation

uses
  Math, DateUtils, cxGeometry, dxGdiPlusClasses, dxCoreGraphics, DemoUtils;

{$R *.dfm}

procedure TdxRangeControlDemoForm.AddPolygonPoint(var APolygon: TPoints; X, Y: Integer);
begin
  SetLength(APolygon, Length(APolygon) + 1);
  APolygon[High(APolygon)].X := X;
  APolygon[High(APolygon)].Y := Y;
end;

procedure TdxRangeControlDemoForm.Animation1Click(Sender: TObject);
var
  AIsChecked: Boolean;
begin
  AIsChecked := dxDemoIsMenuChecked(Sender);
  rcNumericClient.Animation := AIsChecked;
  rcDateTimeClient.Animation := AIsChecked;
  rcDateTimeHeaderClient.Animation := AIsChecked;
end;

procedure TdxRangeControlDemoForm.chbAutoFormatScaleCaptionsPropertiesEditValueChanged(
  Sender: TObject);
begin
  (rcDateTimeHeaderClient.ClientProperties as TdxRangeControlDateTimeHeaderClientProperties).AutoFormatScaleCaptions := chbAutoFormatScaleCaptions.Checked;
end;

procedure TdxRangeControlDemoForm.cxCheckComboBox1PropertiesEditValueChanged(
  Sender: TObject);
var
  AProperties: TdxRangeControlDateTimeHeaderClientProperties;
  AScales: TdxRangeControlDateTimeScales;
  I: Integer;
begin
  AProperties := (rcDateTimeHeaderClient.ClientProperties as TdxRangeControlDateTimeHeaderClientProperties);
  AScales := AProperties.Scales;
  for I := 0 to cxCheckComboBox1.Properties.Items.Count - 1 do
    AScales.GetScale(TdxRangeControlDateTimeScaleUnit(I + Ord(rcduDay))).Visible := cxCheckComboBox1.States[I] = cbsChecked;
end;

procedure TdxRangeControlDemoForm.rcNumericClientDrawContent(ASender: TdxCustomRangeControl; ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean);

  procedure DrawData(ADataSource: TList<TChartData>; const R: TRect; AColor: TdxAlphaColor);
  var
    AData: TChartData;
    APos, AValue: Integer;
    APolygon: TPoints;
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, AViewInfo.Bounds);
    dxGPPaintCanvas.SmoothingMode := smAntiAlias;
    try
      for AData in ADataSource do
        if InRange(AData.X, ASender.VisibleRangeMinValue - 1, ASender.VisibleRangeMaxValue + 1) then
        begin
          APos := ASender.GetPositionFromValue(AData.X);
          AValue := R.Bottom - AData.Y * R.Height div 100;
          if IsNumberClientContentLineMode then
            AddPolygonPoint(APolygon, APos, AValue)
          else
            dxGPPaintCanvas.Line(APos, R.Bottom, APos, AValue, AColor, 3);
        end;
      if IsNumberClientContentLineMode then
        dxGPPaintCanvas.Polyline(APolygon, AColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;

var
  R: TRect;
begin
  R := AViewInfo.Content.Bounds;
  DrawData(FNumericClientData, R, $FFEE8C4B);
end;

procedure TdxRangeControlDemoForm.rgDateTimeClientContentTypePropertiesEditValueChanged(
  Sender: TObject);
begin
  rcDateTimeClient.ContentChanged;
end;

procedure TdxRangeControlDemoForm.rgNumberClientContentTypePropertiesEditValueChanged(
  Sender: TObject);
begin
  rcNumericClient.ContentChanged;
end;

procedure TdxRangeControlDemoForm.tbDateTimeClientPropertiesChange(Sender: TObject);
begin
  (rcDateTimeClient.ClientProperties as TdxRangeControlDateTimeClientProperties).ScaleInterval := tbDateTimeClient.Position;
end;

procedure TdxRangeControlDemoForm.tbNumericClientPropertiesChange(
  Sender: TObject);
begin
  (rcNumericClient.ClientProperties as TdxRangeControlNumericClientProperties).ScaleInterval := tbNumericClient.Position;
end;

procedure TdxRangeControlDemoForm.FormCreate(Sender: TObject);
begin
  inherited;
  FNumericClientData := TList<TChartData>.Create;
  FDateTimeClientData2 := TList<TChartData>.Create;
  FDateTimeClientData1 := TList<TChartData>.Create;
  FDateTimeHeaderClientData1 := TDictionary<TDateTime, Integer>.Create;
  FDateTimeHeaderClientData2 := TDictionary<TDateTime, Integer>.Create;
  rcNumericClient.SelectedRangeMaxValue := 10;
  rcDateTimeClient.ClientProperties.MinValue := Date - 5;
  rcDateTimeClient.ClientProperties.MaxValue := Date + 5;
  rcDateTimeClient.SelectedRangeMinValue := Date - 1;
  rcDateTimeClient.SelectedRangeMaxValue := Date + 1;
  rcDateTimeHeaderClient.ClientProperties.MinValue := Date - 5;
  rcDateTimeHeaderClient.ClientProperties.MaxValue := Date + 5;
  rcDateTimeHeaderClient.SelectedRangeMinValue := Date;
  rcDateTimeHeaderClient.SelectedRangeMaxValue := Date + 2;
  InitializeChartData;
  Width := 850;
  Height := 650;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TdxRangeControlDemoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDateTimeHeaderClientData2);
  FreeAndNil(FDateTimeHeaderClientData1);
  FreeAndNil(FDateTimeClientData1);
  FreeAndNil(FDateTimeClientData2);
  FreeAndNil(FNumericClientData);
  inherited;
end;

procedure TdxRangeControlDemoForm.FormResize(Sender: TObject);
const
  ControlCount = 3;
  Interval = 10;
var
  R: TRect;
  AHeight: Integer;
begin
  R := cxGroupBox5.ClientBounds;
  AHeight := (R.Height - 4 * 10) div 3;
  cxGroupBox1.Top := R.Top + 10;
  cxGroupBox1.Height := AHeight;
  cxGroupBox2.Top := cxGroupBox1.Top + AHeight + 10;
  cxGroupBox2.Height := AHeight;
  cxGroupBox3.Top := cxGroupBox2.Top + AHeight + 10;
  cxGroupBox3.Height := AHeight;
end;

procedure TdxRangeControlDemoForm.rcDateTimeClientDrawContent(ASender: TdxCustomRangeControl;
  ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean);

  procedure DrawData(ADataSource: TList<TChartData>; const R: TRect; AColor, ABrushColor: TdxAlphaColor);
  var
    AData: TChartData;
    APos, AValue: Integer;
    APolygon: TPoints;
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, AViewInfo.Bounds);
    dxGPPaintCanvas.SmoothingMode := smAntiAlias;
    try
      if not IsDateTimeClientContentLineMode then
        AddPolygonPoint(APolygon, R.Left, R.Bottom);
      for AData in ADataSource do
        if InRange(AData.X, ASender.VisibleRangeMinValue - 1, ASender.VisibleRangeMaxValue + 1) then
        begin
          APos := ASender.GetPositionFromValue(AData.X);
          AValue := R.Bottom - AData.Y * R.Height div 100;
          AddPolygonPoint(APolygon, APos, AValue);
        end;
      if IsDateTimeClientContentLineMode then
        dxGPPaintCanvas.Polyline(APolygon, AColor)
      else
      begin
        AddPolygonPoint(APolygon, R.Right, R.Bottom);
        dxGPPaintCanvas.Polygon(APolygon, AColor, ABrushColor);
      end;
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;

var
  R: TRect;
begin
  R := AViewInfo.Content.Bounds;
  DrawData(FDateTimeClientData2, R, $FFEE8C4B, $70EE8C4B);
  DrawData(FDateTimeClientData1, R, $FF6AA4D9, $906AA4D9);
end;

procedure TdxRangeControlDemoForm.rcDateTimeHeaderClientDrawContent(
  ASender: TdxCustomRangeControl; ACanvas: TcxCanvas; AViewInfo: TdxRangeControlCustomClientViewInfo; var AHandled: Boolean);
var
  AContentElements: TList<TdxRangeControlDateTimeHeaderClientContentElementViewInfo>;
  AElement: TdxRangeControlDateTimeHeaderClientContentElementViewInfo;
  I: Integer;
  AValue: Integer;
  ARect: TRect;
begin
  AContentElements := (AViewInfo.Content as TdxRangeControlDateTimeHeaderClientContentViewInfo).Elements;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, AViewInfo.Bounds);
  dxGPPaintCanvas.SmoothingMode := smAntiAlias;
  try
    for I := 0 to AContentElements.Count - 1 do
    begin
      AElement := AContentElements[I];
      dxGPPaintCanvas.SaveClipRegion;
      try
        dxGPPaintCanvas.SetClipRect(AElement.Bounds, gmIntersect);
        if FDateTimeHeaderClientData1.TryGetValue(AElement.MinDate, AValue) then
        begin
          ARect := AElement.Bounds;
          ARect.Right := cxRectCenter(ARect).X - 2;
          ARect.Left := ARect.Right - 15;
          Inc(ARect.Bottom);
          ARect.Top := ARect.Bottom - AValue * ARect.Height div 100;
          dxGPPaintCanvas.Rectangle(ARect, $FF6AA4D9, $646AA4D9);
        end;
        if FDateTimeHeaderClientData2.TryGetValue(AElement.MinDate, AValue) then
        begin
          ARect := AElement.Bounds;
          ARect.Left := cxRectCenter(ARect).X + 2;
          ARect.Right := ARect.Left + 15;
          Inc(ARect.Bottom);
          ARect.Top := ARect.Bottom - AValue * ARect.Height div 100;
          dxGPPaintCanvas.Rectangle(ARect, $FFEE8C4B, $64EE8C4B);
        end;
      finally
        dxGPPaintCanvas.RestoreClipRegion;
      end;
    end;
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxRangeControlDemoForm.InitializeChartData;
var
  ADateTime: TDateTime;
  AData: TChartData;
  I: Integer;
begin
  Randomize;
  ADateTime := rcDateTimeClient.ClientProperties.MinValue;
  while ADateTime <= rcDateTimeClient.ClientProperties.MaxValue do
  begin
    AData.X := ADateTime;
    AData.Y := RandomRange(2, 50);
    FDateTimeClientData1.Add(AData);
    AData.Y := RandomRange(52, 100);
    FDateTimeClientData2.Add(AData);
    ADateTime := IncHour(ADateTime, 3);
  end;
  for I := rcNumericClient.ClientProperties.MinValue to rcNumericClient.ClientProperties.MaxValue do
  begin
    AData.X := I;
    AData.Y := RandomRange(2, 100);
    FNumericClientData.Add(AData);
  end;
  ADateTime := rcDateTimeHeaderClient.ClientProperties.MinValue;
  while ADateTime <= rcDateTimeHeaderClient.ClientProperties.MaxValue do
  begin
    FDateTimeHeaderClientData1.Add(ADateTime, RandomRange(10, 90));
    FDateTimeHeaderClientData2.Add(ADateTime, RandomRange(10, 90));
    ADateTime := IncDay(ADateTime);
  end;
end;

function TdxRangeControlDemoForm.IsDateTimeClientContentLineMode: Boolean;
begin
  Result := rgDateTimeClientContentType.ItemIndex = 0;
end;

function TdxRangeControlDemoForm.IsNumberClientContentLineMode: Boolean;
begin
  Result := rgNumberClientContentType.ItemIndex = 0;
end;

procedure TdxRangeControlDemoForm.ShowRuler1Click(Sender: TObject);
var
  AIsChecked: Boolean;
begin
  AIsChecked := dxDemoIsMenuChecked(Sender);
  rcNumericClient.ShowRuler := AIsChecked;
  rcDateTimeClient.ShowRuler := AIsChecked;
  rcDateTimeHeaderClient.ShowRuler := AIsChecked;
end;

procedure TdxRangeControlDemoForm.ShowZoomscrollbar1Click(Sender: TObject);
var
  AIsChecked: Boolean;
begin
  AIsChecked := dxDemoIsMenuChecked(Sender);
  rcNumericClient.ShowZoomScrollBar := AIsChecked;
  rcDateTimeClient.ShowZoomScrollBar := AIsChecked;
  rcDateTimeHeaderClient.ShowZoomScrollBar := AIsChecked;
end;

end.
