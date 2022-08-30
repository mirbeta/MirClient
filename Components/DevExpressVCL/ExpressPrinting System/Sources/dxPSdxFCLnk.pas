{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPSdxFCLnk;

interface

{$I cxVer.inc}

uses
  Classes, Windows, Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, dxCore, dxFlChrt, dxPSGraphicLnk, dxPSCore,
  ImgList, cxPC, cxControls, cxContainer, cxEdit, cxLabel, cxGraphics, cxTextEdit, cxMaskEdit, cxDropDownEdit, Menus,
  cxColorComboBox, cxCheckBox, cxLookAndFeels, cxLookAndFeelPainters, dxPSReportLinkDesignWindow, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutContainer, cxClasses, cxButtons, dxLayoutControl, dxLayoutcxEditAdapters;

type
  TdxFlowChartReportLink = class(TCustomdxGraphicReportLink)
  private
    FUseMetafile: Boolean;
    function GetFlowChart: TdxCustomFlowChart;
  protected
    function GetGraphic: TGraphic; override;
    function GetGraphicClass: TGraphicClass; override;
    procedure InitializeGraphicItem(AnItem: TdxReportCellGraphic); override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property FlowChart: TdxCustomFlowChart read GetFlowChart;
  published
    property BorderColor;
    property Color;
    property DrawBorder;
    property Transparent;
    property TransparentColor;
    property UseMetafile: Boolean read FUseMetafile write FUseMetafile default True;
  end;

  TdxFCReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    ccbxColor: TcxColorComboBox;
    ccbxLineColor: TcxColorComboBox;
    chbxDrawBorder: TcxCheckBox;
    chbxTransparent: TcxCheckBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    ilFlowChart: TImageList;
    lblColor: TdxLayoutItem;
    lblGridLinesColor: TdxLayoutItem;
    lblPreview: TdxLayoutItem;
    pcMain: TdxLayoutGroup;
    pnlPreview: TPanel;
    stDrawBorder: TcxLabel;
    stTransparent: TcxLabel;
    tshOptions: TdxLayoutGroup;

    procedure ccbxColorChange(Sender: TObject);
    procedure chbxDrawBorderClick(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure lblColorClick(Sender: TObject);
    procedure stDrawBorderClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
  private
    procedure CreateControls;
    function GetReportLink: TdxFlowChartReportLink;
    procedure pbxPreviewPaint(Sender: TObject);
  protected
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadStrings; override;
    procedure PaintPreview(ACanvas: TCanvas; R: TRect); override;
    procedure UpdatePreview; override;
    procedure UpdateControlsState; override;
  public
    FPreviewBox: TCustomControl;

    constructor Create(AOwner: TComponent); override;
    property ReportLink: TdxFlowChartReportLink read GetReportLink;
  end;

  PdxFlowChartGetAsGraphicData = ^TdxFlowChartGetAsGraphicData;
  TdxFlowChartGetAsGraphicData = record
    FlowChart: TdxCustomFlowChart;
    GraphicClass: TGraphicClass;
    Transparent: Boolean;
    TransparentColor: TColor;
    Enhanced: Boolean;
  end;

function dxGetFlowChartAsGraphic(const AData: PdxFlowChartGetAsGraphicData): TGraphic;

implementation

{$R *.DFM}

uses
  SysUtils, Types, dxPSUtl, dxExtCtrls, dxPSRes, dxPSGlbl, cxGeometry;

type
  TdxFlowChartAccess = class(TdxCustomFlowChart);

function dxGetFlowChartAsGraphic(const AData: PdxFlowChartGetAsGraphicData): TGraphic;

  procedure SaveSelection(AFlowChart: TdxCustomFlowChart; ASelections: TList);
  var
    I: Integer;
  begin
    for I := 0 to AFlowChart.SelectedObjectCount - 1 do
      ASelections.Add(AFlowChart.SelectedObjects[I]);
    for I := 0 to AFlowChart.SelectedConnectionCount - 1 do
      ASelections.Add(AFlowChart.SelectedConnections[I]);
  end;

  procedure SetSelected(ASelections: TList; ASelected: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to ASelections.Count - 1 do
      TdxFCItem(ASelections[I]).Selected := ASelected;
  end;

var
  ASavedAntialiasing: Boolean;
  ASavedBorderStyle: TBorderStyle;
  ASavedColor: TColor;
  Canvas: TCanvas;
  DC: HDC;
  Graphic: TGraphic;
  IsBitmap: Boolean;
  IsMetafile: Boolean;
  SaveTopEdge, SaveLeftEdge: Integer;
  Selections: TList;
begin
  Result := nil;
  if (AData = nil) or (AData^.FlowChart = nil) or (AData^.GraphicClass = nil) then Exit;
  IsBitmap := AData^.GraphicClass.InheritsFrom(TBitmap);
  IsMetafile := AData^.GraphicClass.InheritsFrom(TMetafile);
  try
    if IsMetafile then
      Graphic := TMetafile.Create
    else
      Graphic := TBitmap.Create;
    try
      Graphic.Width := TdxFlowChartAccess(AData^.FlowChart).ChartWidth;
      Graphic.Height := TdxFlowChartAccess(AData^.FlowChart).ChartHeight;
      if IsMetafile then
      begin
        TMetafile(Graphic).Enhanced := AData^.Enhanced;
        Canvas := TMetafileCanvas.Create(TMetafile(Graphic), 0)
      end
      else
        Canvas := TBitmap(Graphic).Canvas;

      try
        if not IsMetafile and AData^.Transparent then
        begin
          Canvas.Brush.Color := ColorToRGB(AData^.TransparentColor);
          Canvas.FillRect(Rect(0, 0, Graphic.Width, Graphic.Height));
        end;
        DC := Canvas.Handle;
        with TdxFlowChartAccess(AData^.FlowChart) do
        begin
          BeginUpdate;
          try
            ASavedColor := Color;
            ASavedBorderStyle := BorderStyle;
            ASavedAntialiasing := Antialiasing;
            ControlState := ControlState + [csPaintCopy];
            SaveTopEdge := TopEdge;
            SaveLeftEdge := LeftEdge;
            Selections := TList.Create;
            try
              SaveSelection(AData^.FlowChart, Selections);
              SetSelected(Selections, False);
              Antialiasing := Antialiasing and not IsMetafile;
              BorderStyle := bsNone;
              Color := clNone;
              TopEdge := 0;
              LeftEdge := 0;
              PaintWindow(DC);
            finally
              TopEdge := SaveTopEdge;
              LeftEdge := SaveLeftEdge;
              SetSelected(Selections, True);
              Selections.Free;
              Antialiasing := ASavedAntialiasing;
              BorderStyle := ASavedBorderStyle;
              Color := ASavedColor;
            end;
            ControlState := ControlState - [csPaintCopy];
          finally
            CancelUpdate;
          end;
        end;

        if IsBitmap and AData^.Transparent then
          TBitmap(Graphic).TransparentColor := AData^.TransparentColor;

        if not IsMetafile and not IsBitmap then
        begin
          Result := dxPSUtl.CreateGraphic(AData^.GraphicClass);
          Result.Assign(Graphic);
        end
        else
          Result := Graphic;
      finally
        if IsMetafile then Canvas.Free;
      end;
    finally
      if not IsMetafile and not IsBitmap then Graphic.Free;
    end;
  except
    if Result <> nil then Result.Free;
    raise;
  end;
end;

{ TdxFlowChartReportLink }

constructor TdxFlowChartReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FUseMetafile := True;
end;

procedure TdxFlowChartReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxFlowChartReportLink then
    UseMetafile := TdxFlowChartReportLink(Source).UseMetafile;
end;

function TdxFlowChartReportLink.GetGraphic: TGraphic;
var
  Data: TdxFlowChartGetAsGraphicData;
begin
  Result := nil;
  if (FlowChart = nil) or (FlowChart.ObjectCount = 0) then
    Exit;

  FillChar(Data, SizeOf(TdxFlowChartGetAsGraphicData), 0);
  Data.FlowChart := FlowChart;
  Data.GraphicClass := GetGraphicClass;
  Data.Transparent := Transparent;
  Data.TransparentColor := TransparentColor;
  Data.Enhanced := True;
  Result := dxGetFlowChartAsGraphic(@Data);
end;

function TdxFlowChartReportLink.GetGraphicClass: TGraphicClass;
const
  GraphicClasses: array[Boolean] of TGraphicClass = (TBitmap, TMetafile);
begin
  Result := GraphicClasses[UseMetafile];
end;

procedure TdxFlowChartReportLink.InitializeGraphicItem(
  AnItem: TdxReportCellGraphic);
begin
  inherited InitializeGraphicItem(AnItem);
  AnItem.ImageTransparent := UseMetafile;
  AnItem.Transparent := Transparent;
  AnItem.Color := Color;
end;

procedure TdxFlowChartReportLink.InternalRestoreDefaults;
begin
  inherited;
  UseMetafile := True;
end;

procedure TdxFlowChartReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  if FlowChart <> nil then
    TransparentColor := dxPSUtl.Control_GetColor(FlowChart);
end;

function TdxFlowChartReportLink.GetFlowChart: TdxCustomFlowChart;
begin
  Result := TdxCustomFlowChart(Component);
end;

{ TdxFCReportLinkDesignWindow }

constructor TdxFCReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhcFlowChartReportLinkDesigner;
  inherited;
  CreateControls;
end;

procedure TdxFCReportLinkDesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  FPreviewBox.Parent := pnlPreview;
  FPreviewBox.Align := alClient;
  TdxPSPaintPanel(FPreviewBox).EdgeInner := esNone;
  TdxPSPaintPanel(FPreviewBox).EdgeOuter := esNone;
  TdxPSPaintPanel(FPreviewBox).OnPaint := pbxPreviewPaint;
end;

function TdxFCReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxFCReportLinkDesignWindow.LoadStrings;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  stDrawBorder.Caption := ' ' + cxGetResourceString(@sdxBorderLines) + ' ';
  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);
end;

procedure TdxFCReportLinkDesignWindow.DoInitialize;
begin
  inherited;
  chbxDrawBorder.Checked := ReportLink.DrawBorder;
  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ReportLink.Color;
  ccbxLineColor.ColorValue := ReportLink.BorderColor;
end;

procedure TdxFCReportLinkDesignWindow.UpdateControlsState;
begin
  inherited;
  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := not chbxTransparent.Checked;
  ccbxLineColor.Enabled := chbxDrawBorder.Checked;
  lblGridLinesColor.Enabled := chbxDrawBorder.Checked;
end;

procedure TdxFCReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TdxFCReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);

  procedure DrawText(ACanvas: TCanvas; const R: TRect; const S: string);
  const
    uFormat: UINT = DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  begin
    ACanvas.Brush.Style := bsClear;
    Windows.DrawText(ACanvas.Handle, PChar(S), Length(S), PRect(@R)^, uFormat);
  end;

var
  R2, R3: TRect;
  W, H: Integer;
begin
  inherited;
  OffsetRect(R, -R.Left, -R.Top);
  InflateRect(R, -ScaleFactor.Apply(4), -ScaleFactor.Apply(4));

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Color := clWindowText;
  ACanvas.Font.Height := ScaleFactor.Apply(ReportLink.Font.Height, ReportLink.ScaleFactor);
  { Border }
  if ReportLink.DrawBorder then
  begin
    InflateRect(R, 1, 1);
    ACanvas.Brush.Color := ReportLink.BorderColor;
    ACanvas.FrameRect(R);
    InflateRect(R, -1, -1);
  end;

  { Interior }
  if not ReportLink.Transparent then
  begin
    ACanvas.Brush.Color := ReportLink.Color;
    ACanvas.FillRect(R);
  end;

  { Charts }
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  { Plan }
  R2 := Bounds(R.Left + ScaleFactor.Apply(2), ScaleFactor.Apply(4), R.Right, ScaleFactor.Apply(12));
  ACanvas.Font.Style := [fsBold];
  DrawText(ACanvas, R2, cxGetResourceString(@sdxPlan));
  R2 := Rect(R.Left + ScaleFactor.Apply(30), R2.Bottom + ScaleFactor.Apply(2), R.Right - ScaleFactor.Apply(30),
    R2.Bottom + ScaleFactor.Apply(3));
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWindowText;
  ACanvas.FillRect(R2);

  ACanvas.Brush.Style := bsClear;
  { Swimming-pool }
  R2 := Bounds(R.Left + ScaleFactor.Apply(2), ScaleFactor.Apply(27), R.Left + W div 2 - ScaleFactor.Apply(22), H div 4);
  ACanvas.RoundRect(R2.Left, R2.Top, R2.Right, R2.Bottom, ScaleFactor.Apply(10), ScaleFactor.Apply(10));
  DrawText(ACanvas, R2, cxGetResourceString(@sdxSwimmingPool));

  { Administration }
  OffsetRect(R2, R2.Right - R2.Left + ScaleFactor.Apply(30), 0);
  ACanvas.Rectangle(R2.Left, R2.Top, R2.Right, R2.Bottom);
  DrawText(ACanvas, R2, cxGetResourceString(@sdxAdministration));

  { Park }
  OffsetRect(R2, 0, R2.Bottom - R2.Top + ScaleFactor.Apply(30));
  Inc(R2.Bottom, R2.Bottom - R2.Top);
  ACanvas.Ellipse(R2.Left, R2.Top, R2.Right, R2.Bottom);
  cxPaintCanvas.BeginPaint(ACanvas);
  try
    R3 := ScaleFactor.Apply(Rect(0, 0, ilFlowChart.Width, ilFlowChart.Height));
    cxDrawImage(cxPaintCanvas, cxRectSetOrigin(R3, Point(cxRectCenter(R2).X - R3.Right div 2,
      R2.Top + ScaleFactor.Apply(10))), nil, ilFlowChart, 0, Enabled, nil, ScaleFactor);
    cxDrawImage(cxPaintCanvas, cxRectSetOrigin(R3, Point(cxRectCenter(R2).X - 2 * R3.Right,
      R2.Top + R3.Bottom + ScaleFactor.Apply(20))), nil, ilFlowChart, 0, Enabled, nil, ScaleFactor);
    cxDrawImage(cxPaintCanvas, cxRectSetOrigin(R3, Point(cxRectCenter(R2).X + R3.Right,
      R2.Top + R3.Bottom + ScaleFactor.Apply(20))), nil, ilFlowChart, 0, Enabled, nil, ScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
  DrawText(ACanvas, R2, cxGetResourceString(@sdxPark));

  { Car-parking }
  OffsetRect(R2, -R2.Right + R2.Left - ScaleFactor.Apply(30), 0);
  ACanvas.Rectangle(R2.Left, R2.Top, R2.Right, R2.Bottom);
  DrawText(ACanvas, R2, cxGetResourceString(@sdxCarParking));

  { Crosses }
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clBtnShadow;
  R2 := Rect(R.Left + W div 2 - ScaleFactor.Apply(10), ScaleFactor.Apply(27), R.Left + W div 2 + ScaleFactor.Apply(8),
    R.Bottom - ScaleFactor.Apply(2));
  ACanvas.FillRect(R2);
  R2 := Rect(R.Left + ScaleFactor.Apply(2), R.Top + ScaleFactor.Apply(28) + H div 4, R.Right - ScaleFactor.Apply(2),
    R.Top + ScaleFactor.Apply(27) + H div 4 + ScaleFactor.Apply(20));
  ACanvas.FillRect(R2);

  ACanvas.Font.Style := [];
end;

function TdxFCReportLinkDesignWindow.GetReportLink: TdxFlowChartReportLink;
begin
  Result := inherited ReportLink as TdxFlowChartReportLink;
end;

procedure TdxFCReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
var
  Color: TColor;
begin
  if LockControlsUpdate then Exit;
  Color := TcxColorComboBox(Sender).ColorValue;
  case TcxColorComboBox(Sender).Tag of
    0: ReportLink.Color := Color;
    1: ReportLink.BorderColor := Color;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxFCReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

procedure TdxFCReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.Transparent := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxFCReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxFCReportLinkDesignWindow.chbxDrawBorderClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.DrawBorder := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxFCReportLinkDesignWindow.stDrawBorderClick(Sender: TObject);
begin
  if chbxDrawBorder.CanFocus then
    ActiveControl := chbxDrawBorder;
  chbxDrawBorder.Checked := not chbxDrawBorder.Checked;
end;

procedure TdxFCReportLinkDesignWindow.lblColorClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

initialization
  dxPSRegisterReportLink(TdxFlowChartReportLink, TdxCustomFlowChart, TdxFCReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxFlowChartReportLink, TdxCustomFlowChart, TdxFCReportLinkDesignWindow);

end.

