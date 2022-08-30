unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  ExtCtrls, StdCtrls, dxGaugeDigitalScale, cxClasses, dxGaugeCustomScale,
  dxGaugeQuantitativeScale, dxGaugeCircularScale, dxGaugeControl, ComCtrls, dxGaugeLinearScale, dxGaugeDBScale,
  cxContainer, cxEdit, cxLabel, cxTrackBar, cxGroupBox, cxRadioGroup, DB, DBClient, dxBarBuiltInMenu, cxPC;

type
  TfrmGaugeStyles = class(TForm)
    Timer1: TTimer;
    cxLabel2: TcxLabel;
    cxLookAndFeelController1: TcxLookAndFeelController;
    cxTrackBar1: TcxTrackBar;
    tcStyles: TcxTabControl;
    dxGaugeControl1: TdxGaugeControl;
    dxGaugeControl1CircularScale1: TdxGaugeCircularScale;
    dxGaugeControl1DigitalScale1: TdxGaugeDigitalScale;
    dxGaugeControl1LinearScale1: TdxGaugeLinearScale;
    dxGaugeControl1CircularHalfScale1: TdxGaugeCircularHalfScale;
    dxGaugeControl1CircularQuarterLeftScale1: TdxGaugeCircularQuarterLeftScale;
    dxGaugeControl1CircularQuarterRightScale1: TdxGaugeCircularQuarterRightScale;
    dxGaugeControl1ContainerScale1: TdxGaugeContainerScale;
    dxGaugeControl1CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cxTrackBar1PropertiesChange(Sender: TObject);
    procedure tcStylesChange(Sender: TObject);
  protected
    procedure InitTrackBar;
    procedure PopulateStyleList;
    procedure UpdateGauges;
    procedure UpdateTime;
  end;

var
  frmGaugeStyles: TfrmGaugeStyles;

implementation

uses
  dxCore, dxCoreGraphics, cxDateUtils;

type
  TdxGaugeQuantitativeScaleAccess = class(TdxGaugeQuantitativeScale);

{$R *.dfm}

function GetStyleNameByCaption(const ACaption: string): string;
begin
  Result := StringReplace(ACaption, ' ', '', [rfReplaceAll]);
end;

function GetStyleCaption(const AStyleName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AStyleName) do
  begin
    if (UpperCase(AStyleName[I]) = AStyleName[I]) and (I <> 1) and (AStyleName[1] <> 'i') and (AStyleName[2] <> ' ') then
      Result := Result + ' ';
     Result := Result + AStyleName[I];
  end;
end;

procedure TfrmGaugeStyles.UpdateGauges;
var
  I: Integer;
begin
  dxGaugeControl1.BeginUpdate;
  for I := 0 to dxGaugeControl1.Scales.Count - 1 do
  begin
    dxGaugeControl1.Scales[I].StyleName := GetStyleNameByCaption(tcStyles.Tabs[tcStyles.TabIndex].Caption);
    if dxGaugeControl1.Scales[I].ScaleType = stDigitalScale then
      TdxGaugeDigitalScale(dxGaugeControl1.Scales[I]).OptionsView.SegmentColorOff := dxMakeAlphaColor(clNone)
    else
      TdxGaugeQuantitativeScaleAccess(dxGaugeControl1.Scales[I]).Value := cxTrackBar1.Position;
  end;
  dxGaugeControl1.EndUpdate;
end;

procedure TfrmGaugeStyles.UpdateTime;
begin
  dxGaugeControl1DigitalScale1.Value := 'Gauges ' + cxTimeToStr(Now, 'hh:mm:ss');
end;

procedure TfrmGaugeStyles.InitTrackBar;
begin
  cxTrackBar1.Properties.Max := Round(dxGaugeControl1CircularScale1.OptionsView.MaxValue);
  cxTrackBar1.Properties.Min := Round(dxGaugeControl1CircularScale1.OptionsView.MinValue);
end;

procedure TfrmGaugeStyles.PopulateStyleList;
var
  I: Integer;
  AStyleNames: TStringList;
begin
  AStyleNames := TStringList.Create;
  try
    dxGaugeGetPredefinedStyleNames(AStyleNames);
    AStyleNames.Sorted := True;
    for I := 0 to AStyleNames.Count - 1 do
      tcStyles.Tabs.Add(GetStyleCaption(AStyleNames.Strings[I]));
    if AStyleNames.Count > 0 then
      tcStyles.TabIndex := 0;
  finally
    AStyleNames.Free;
  end;
end;

procedure TfrmGaugeStyles.tcStylesChange(Sender: TObject);
begin
  UpdateGauges;
end;

procedure TfrmGaugeStyles.cxTrackBar1PropertiesChange(Sender: TObject);
begin
  UpdateGauges;
end;

procedure TfrmGaugeStyles.FormCreate(Sender: TObject);
begin
  inherited;
  InitTrackBar;
  PopulateStyleList;
  UpdateGauges;
  UpdateTime;
  Timer1.Enabled := True;
end;

procedure TfrmGaugeStyles.Timer1Timer(Sender: TObject);
begin
  UpdateTime;
end;

end.
