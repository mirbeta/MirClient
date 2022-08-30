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

unit dxPSdxOCLnk;

interface

{$I cxVer.inc}

uses
  Classes, Windows, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, dxCore, dxOrgChr, dxPSCore, dxPSGraphicLnk,
  cxLabel, cxControls, cxContainer, cxEdit, cxCheckBox, cxGraphics, cxTextEdit, cxMaskEdit, cxDropDownEdit, Menus, Types,
  cxColorComboBox, cxPC, cxLookAndFeels, cxLookAndFeelPainters, dxPSReportLinkDesignWindow, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutContainer, cxClasses, cxButtons, dxLayoutControl, dxLayoutcxEditAdapters;

type
  TCustomdxOrgChartReportLink = class(TCustomdxGraphicReportLink)
  private
    FFullExpand: Boolean;
    FUseMetafile: Boolean;
    function GetOrgChart: TdxOrgChart;
    procedure SetFullExpand(Value: Boolean);
  protected
    function GetGraphic: TGraphic; override;
    function GetGraphicClass: TGraphicClass; override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;
    property OrgChart: TdxOrgChart read GetOrgChart;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property Color;
    property FullExpand: Boolean read FFullExpand write SetFullExpand default False;
    property Transparent;
    property UseMetafile: Boolean read FUseMetafile write FUseMetafile default True;
  end;

  TdxOrgChartReportLink = class(TCustomdxOrgChartReportLink)
  public
    property OrgChart;
  published
    property BorderColor;
    property Color;
    property DrawBorder;
    property FullExpand;
    property Transparent;
    property TransparentColor;
    property UseMetafile;
  end;

  TdxOCReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    lblPreview: TdxLayoutItem;
    pnlPreview: TPanel;
    ocPreview: TdxOrgChart;
    dxLayoutGroup1: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;
    pcMain: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    chbxFullExpand: TcxCheckBox;
    dxLayoutItem2: TdxLayoutItem;
    stDrawBorder: TcxLabel;
    dxLayoutItem3: TdxLayoutItem;
    chbxDrawBorder: TcxCheckBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    lblGridLinesColor: TdxLayoutItem;
    ccbxGridLineColor: TcxColorComboBox;
    dxLayoutItem4: TdxLayoutItem;
    chbxTransparent: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    stTransparent: TcxLabel;
    lblColor: TdxLayoutItem;
    ccbxColor: TcxColorComboBox;
    liTransparent: TdxLayoutGroup;
    procedure ccbxColorChange(Sender: TObject);
    procedure chbxDrawBorderClick(Sender: TObject);
    procedure chbxFullExpandClick(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure lblColorClick(Sender: TObject);
    procedure stDrawBorderClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
  private
    procedure CreateControls;
    function GetReportLink: TCustomdxOrgChartReportLink;
    procedure pbxPreviewPaint(Sender: TObject);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    FPreviewBox: TCustomControl;
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadStrings; override;
    procedure PaintPreview(ACanvas: TCanvas; R: TRect); override;
    procedure UpdatePreview; override;
    procedure UpdateControlsState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ReportLink: TCustomdxOrgChartReportLink read GetReportLink;
  end;

  PdxOrgChartGetAsGraphicData = ^TdxOrgChartGetAsGraphicData;
  TdxOrgChartGetAsGraphicData = record
    OrgChart: TdxCustomOrgChart;
    GraphicClass: TGraphicClass;
    FullExpand: Boolean;
    Transparent: Boolean;
    TransparentColor: TColor;
    Enhanced: Boolean;
  end;

function dxGetOrgChartAsGraphic(const AData: TdxOrgChartGetAsGraphicData): TGraphic;
implementation

{$R *.DFM}

uses
  SysUtils, Messages, dxPSRes, dxPSUtl, dxPSGlbl, dxExtCtrls, cxGeometry;

type
  TdxOrgChartAccess = class(TdxCustomOrgChart);

function dxGetOrgChartAsGraphic(const AData: TdxOrgChartGetAsGraphicData): TGraphic;

  function CreateGraphic(out ACanvas: TCanvas; out AIsBitmap, AIsMetaFile: Boolean): TGraphic;
  begin
    AIsBitmap := AData.GraphicClass.InheritsFrom(TBitmap);
    AIsMetafile := AData.GraphicClass.InheritsFrom(TMetafile);
    if AIsMetafile then
      Result := TMetafile.Create
    else
      Result := TBitmap.Create;

    Result.Width := AData.OrgChart.FullWidth;
    Result.Height := AData.OrgChart.FullHeight;
    if Result is TBitmap then
      ACanvas := TBitmap(Result).Canvas
    else
    begin
      TMetafile(Result).Enhanced := AData.Enhanced;
      ACanvas := TMetafileCanvas.Create(TMetaFile(Result), 0);
    end;
  end;

  procedure FullExpandOrgChart(AOrgChart: TdxCustomOrgChart);
  var
    ASavedOptions: TdxOcOptions;
  begin
    with TdxOrgChartAccess(AOrgChart) do
    begin
      ASavedOptions := Options;
      try
        Options := Options - [ocAnimate];
        AOrgChart.FullExpand;
      finally
        Options := ASavedOptions;
      end;
    end;
  end;

  procedure DrawOrgChart(ACanvas: TCanvas; AIsMetaFile: Boolean);
  var
    ASavedAntialiazing: Boolean;
    ASavedBorderStyle: TBorderStyle;
    ASavedColor: TColor;
    ASavedLeftEdge, ASavedTopEdge: Integer;
    ASavedSelected: TdxOcNode;
  begin
    with TdxOrgChartAccess(AData.OrgChart) do
    begin
      ASavedAntialiazing := Antialiasing;
      ASavedBorderStyle := BorderStyle;
      ASavedSelected := Selected;
      ASavedLeftEdge := LeftEdge;
      ASavedTopEdge := TopEdge;
      ASavedColor := Color;
      try
        LeftEdge := 0;
        TopEdge := 0;
        Selected := nil;
        Color := clNone;
        BorderStyle := bsNone;
        Antialiasing := Antialiasing and not AIsMetaFile;
        PaintContentTo(ACanvas, LeftEdge, TopEdge);
      finally
        Color := ASavedColor;
        BorderStyle := ASavedBorderStyle;
        Antialiasing := ASavedAntialiazing;
        LeftEdge := ASavedLeftEdge;
        TopEdge := ASavedTopEdge;
        Selected := ASavedSelected;
      end;
    end;
  end;

var
  ACanvas: TCanvas;
  AGraphic: TGraphic;
  AIsBitmap, AIsMetaFile: Boolean;
begin
  Result := nil;
  if Assigned(AData.OrgChart) and Assigned(AData.GraphicClass) then
  try
    if AData.FullExpand then
      FullExpandOrgChart(AData.OrgChart);

    AGraphic := CreateGraphic(ACanvas, AIsBitmap, AIsMetaFile);
    try
      if not AIsMetafile and AData.Transparent then
      begin
        ACanvas.Brush.Color := AData.TransparentColor;
        ACanvas.FillRect(Rect(0, 0, AGraphic.Width, AGraphic.Height));
      end;
      DrawOrgChart(ACanvas, AIsMetaFile);
      if AIsBitmap and AData.Transparent then
        TBitmap(AGraphic).TransparentColor := AData.TransparentColor;
    finally
      if AIsMetafile then
        FreeAndNil(ACanvas);
    end;

    if AIsMetafile or AIsBitmap then
      Result := AGraphic
    else
    begin
      Result := dxPSUtl.CreateGraphic(AData.GraphicClass);
      Result.Assign(AGraphic);
      FreeAndNil(AGraphic);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TCustomdxOrgChartReportLink }

constructor TCustomdxOrgChartReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FFullExpand := False;
  FUseMetafile := True;
end;

procedure TCustomdxOrgChartReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCustomdxOrgChartReportLink then
    with TCustomdxOrgChartReportLink(Source) do
    begin
      Self.FullExpand := FullExpand;
      Self.UseMetafile := UseMetafile;
    end;
end;

function TCustomdxOrgChartReportLink.GetGraphic: TGraphic;
var
  Data: TdxOrgChartGetAsGraphicData;
begin
  Result := nil;
  if (OrgChart = nil) or (OrgChart.Count = 0) then Exit;
  FillChar(Data, SizeOf(TdxOrgChartGetAsGraphicData), 0);
  Data.OrgChart := OrgChart;
  Data.GraphicClass := GetGraphicClass;
  Data.FullExpand := FullExpand;
  Data.Transparent := Transparent;
  Data.TransparentColor := TransparentColor;
  Data.Enhanced := True;
  Result := dxGetOrgChartAsGraphic(Data);
end;

function TCustomdxOrgChartReportLink.GetGraphicClass: TGraphicClass;
const
  GraphicClasses: array[Boolean] of TGraphicClass = (TBitmap, TMetafile);
begin
  Result := GraphicClasses[UseMetafile];
end;

procedure TCustomdxOrgChartReportLink.InternalRestoreDefaults;
begin
  inherited;
  FullExpand := False;
  UseMetafile := True;
end;

procedure TCustomdxOrgChartReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  if OrgChart <> nil then
    TransparentColor := OrgChart.Color;
end;

procedure TCustomdxOrgChartReportLink.SetFullExpand(Value: Boolean);
begin
  if FFullExpand <> Value then
  begin
    FFullExpand := Value;
    LinkModified(True);
  end;
end;

function TCustomdxOrgChartReportLink.GetOrgChart: TdxOrgChart;
begin
  Result := TdxOrgChart(Component);
end;

{ TdxOCReportLinkDesignWindow }

constructor TdxOCReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhcOrgChartReportLinkDesigner;
  inherited;
  CreateControls;
end;

destructor TdxOCReportLinkDesignWindow.Destroy;
begin
  inherited;
end;

procedure TdxOCReportLinkDesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  FPreviewBox.Parent := pnlPreview;
  FPreviewBox.Align := alClient;
  TdxPSPaintPanel(FPreviewBox).EdgeInner := esNone;
  TdxPSPaintPanel(FPreviewBox).EdgeOuter := esNone;
  TdxPSPaintPanel(FPreviewBox).OnPaint := pbxPreviewPaint;
end;

function TdxOCReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxOCReportLinkDesignWindow.LoadStrings;
var
  Item: TdxOcNode;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);

  Item := ocPreview.Items[0];
  Item.Text := cxGetResourceString(@sdxCorporateHeadquarters);
  Item[0].Text := cxGetResourceString(@sdxSalesAndMarketing);
  Item[0].Items[0].Text := cxGetResourceString(@sdxFieldOfficeCanada);
  Item[1].Text := cxGetResourceString(@sdxEngineering);

  stDrawBorder.Caption := ' ' + cxGetResourceString(@sdxBorderLines) + ' ';
  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));
end;

procedure TdxOCReportLinkDesignWindow.DoInitialize;
begin
  inherited;
  lblColor.Visible := not ReportLink.UseMetafile;
  liTransparent.Visible := not ReportLink.UseMetafile;

  chbxFullExpand.Checked := ReportLink.FullExpand;
  chbxDrawBorder.Checked := ReportLink.DrawBorder;
  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ReportLink.Color;
  ccbxGridLineColor.ColorValue := ReportLink.BorderColor;
end;

procedure TdxOCReportLinkDesignWindow.UpdateControlsState;
begin
  inherited;
  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := not chbxTransparent.Checked;
  ccbxGridLineColor.Enabled := chbxDrawBorder.Checked;
  lblGridLinesColor.Enabled := chbxDrawBorder.Checked;
end;

procedure TdxOCReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TdxOCReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
var
  OffsetX, OffsetY: Integer;
  AOCBmp: TBitmap;
begin
  inherited;
  OffsetRect(R, -R.Left, -R.Top);
  InflateRect(R, -ScaleFactor.Apply(4), -ScaleFactor.Apply(4));

  ACanvas.Brush.Style := bsSolid;
  { border }
  if ReportLink.DrawBorder then
  begin
    InflateRect(R, 1, 1);
    ACanvas.Brush.Color := ReportLink.BorderColor;
    ACanvas.FrameRect(R);
    InflateRect(R, -1, -1);
  end;
  { interior }
  if not ReportLink.Transparent then
  begin
    ACanvas.Brush.Color := ReportLink.Color;
    ACanvas.FillRect(R);
  end;
  { charts }
  AOCBmp := TBitmap.Create;
  try
    ocPreview.FullExpand;
    AOCBmp.Width := ocPreview.FullWidth + 1;
    AOCBmp.Height := ocPreview.FullHeight + 1;
    Control_PaintWindow(ocPreview, AOCBmp.Canvas.Handle);
    OffsetX := R.Left + (R.Right - R.Left - AOCBmp.Width) div 2;
    OffsetY := R.Top + (R.Bottom - R.Top - AOCBmp.Height) div 2;
    ACanvas.Brush.Style := bsClear;
    ACanvas.BrushCopy(Bounds(OffsetX, OffsetY, AOCBmp.Width, AOCBmp.Height),
      AOCBmp, Rect(0, 0, AOCBmp.Width, AOCBmp.Height),
      AOCBmp.Canvas.Pixels[0, AOCBmp.Height - 1]);
  finally
    FreeAndNil(AOCBmp);
  end;
end;

function TdxOCReportLinkDesignWindow.GetReportLink: TCustomdxOrgChartReportLink;
begin
  Result := inherited ReportLink as TCustomdxOrgChartReportLink;
end;

procedure TdxOCReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
var
  AColor: TColor;
begin
  if LockControlsUpdate then Exit;
  AColor := TcxColorComboBox(Sender).ColorValue;
  case TTagToInt(TcxColorComboBox(Sender).Tag) of
    0: ReportLink.Color := AColor;
    1: ReportLink.BorderColor := AColor;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxOCReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

procedure TdxOCReportLinkDesignWindow.lblColorClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

procedure TdxOCReportLinkDesignWindow.chbxFullExpandClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.FullExpand := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxOCReportLinkDesignWindow.chbxDrawBorderClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.DrawBorder := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxOCReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.Transparent := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxOCReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxOCReportLinkDesignWindow.stDrawBorderClick(Sender: TObject);
begin
  if chbxDrawBorder.CanFocus then
    ActiveControl := chbxDrawBorder;
  chbxDrawBorder.Checked := not chbxDrawBorder.Checked;
end;

procedure TdxOCReportLinkDesignWindow.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(Message.CharCode, pcMain.Items[I].Caption) then
    begin
      Message.Result := 1;
      pcMain.ItemIndex := I;
      Exit;
    end;
end;

initialization
  dxPSRegisterReportLink(TdxOrgChartReportLink, TdxOrgChart, TdxOCReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxOrgChartReportLink, TdxOrgChart, TdxOCReportLinkDesignWindow);

end.


