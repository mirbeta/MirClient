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

unit dxPSStdGrLnk;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids, StdCtrls, ExtCtrls, ComCtrls,
  CheckLst, dxCore, dxPSRes, dxPSCore, dxPSGrLnks, dxPSReportRenderCanvas, dxPSGlbl, dxPSBaseGridLnk, cxDrawTextUtils,
  cxControls, cxContainer, cxEdit, cxLabel, cxCheckBox, Menus, cxLookAndFeelPainters, cxPC, cxButtons, cxTextEdit,
  cxGraphics, cxMaskEdit, cxDropDownEdit, cxColorComboBox, cxLookAndFeels, cxImageComboBox, dxPSReportLinkDesignWindow,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutcxEditAdapters,
  cxImage;

type
  TStddxGridReportLink = class(TCustomdxGridReportLink)
  private
    FSaveFont: TFont;
    FCustomDrawFontChanged: Boolean;
    procedure CustomDrawFontChanged(Sender: TObject);
  protected
    class function IsDrawGridLink: Boolean; virtual;
    class function IsStringGridLink: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TdxCustomDrawTextCellEvent = procedure(Sender: TBasedxReportLink;
    ACol, ARow: Integer; ACanvas: TCanvas; ABoundsRect, AClientRect: TRect;
    var AText: string; AFont: TFont; var AColor: TColor;
    var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
    var ADone: Boolean) of object;

  TdxStringGridReportLink = class(TStddxGridReportLink)
  private
    FTextAlignX: TcxTextAlignX;
    FTextAlignY: TcxTextAlignY;
    FOnCustomDrawCell: TdxCustomDrawTextCellEvent;
    function GetStringGrid: TStringGrid;
    procedure SetTextAlignX(Value: TcxTextAlignX);
    procedure SetTextAlignY(Value: TcxTextAlignY);
  protected
    procedure InternalRestoreDefaults; override;
    class function IsStringGridLink: Boolean; override;

    function GetCellText(ACol, ARow: Integer): string; override;
    function GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX; override;
    function GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; override;
    procedure SetDrawMode(Value: TdxGridDrawMode); override;

    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure DoCustomDrawCell(ACol, ARow: Integer; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var AText: string; AFont: TFont;
      var AColor: TColor; var ATextAlignX: TcxTextAlignX;
      var ATextAlignY: TcxTextAlignY; var ADone: Boolean); virtual;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    property StringGrid: TStringGrid read GetStringGrid;
  published
    property AutoWidth;
    property Color;
    property DrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property FixedColor;
    property FixedFont;
    property FixedTransparent;
    property Font;
    property GridLineColor;
    property HeadersOnEveryPage;
    property IncludeFixed;
    property Multiline;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property Options;
    property RowAutoHeight;
    property ScaleFonts;
    property Soft3D;
    property SupportedCustomDraw;
    property TextAlignX: TcxTextAlignX read FTextAlignX write SetTextAlignX default taLeft;
    property TextAlignY: TcxTextAlignY read FTextAlignY write SetTextAlignY default taCenterY;
    property Transparent;
    property UseCustomPageBreaks;
    property UseHorzDelimiters;
    property UseVertDelimiters;

    property OnCustomDrawCell: TdxCustomDrawTextCellEvent read FOnCustomDrawCell write FOnCustomDrawCell;
    property OnGetCustomPageBreaks;
    property OnInitializeItem;
  end;

  TdxCustomDrawCellEvent = procedure(Sender: TBasedxReportLink;
    ACol, ARow: Integer; ACanvas: TCanvas; ABoundsRect, AClientRect: TRect) of object;

  TdxCustomDrawCellExEvent = procedure(Sender: TBasedxReportLink;
    ACol, ARow: Integer; ACanvas: TCanvas; AFont: TFont; ABoundsRect, AClientRect: TRect;
    var ADone: Boolean) of object;

  TdxDrawGridReportLink = class(TStddxGridReportLink)
  private
    FDefaultDrawing: Boolean;
    FOnCustomDrawCell: TdxCustomDrawCellEvent;
    FOnCustomDrawCellEx: TdxCustomDrawCellExEvent;
    function GetDrawGrid: TDrawGrid;
    procedure SetDefaultDrawing(Value: Boolean);
  protected
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;

    function GetCellText(ACol, ARow: Integer): string; override;
    function GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass; override;
    procedure SetDrawMode(Value: TdxGridDrawMode); override;

    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure DoCustomDrawCell(ACol, ARow: Integer; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect); virtual;
    procedure DoCustomDrawCellEx(ACol, ARow: Integer; ACanvas: TCanvas;
      AFont: TFont; ABoundsRect, AClientRect: TRect); virtual;
    class function IsDrawGridLink: Boolean; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    class function Serializable: Boolean; override;

    property DrawGrid: TDrawGrid read GetDrawGrid;
  published
    property AutoWidth;
    property Color;
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default True;
    property DrawMode;
    property Effects3D;
    property EvenColor;
    property EvenFont;
    property FixedColor;
    property FixedFont;
    property FixedTransparent;
    property Font;
    property GridLineColor;
    property HeadersOnEveryPage;
    property IncludeFixed;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property Options;
    property ScaleFonts;
    property Soft3D;
    property SupportedCustomDraw default True;
    property Transparent;
    property UseCustomPageBreaks;
    property UseHorzDelimiters;
    property UseVertDelimiters;

    property OnCustomDrawCell: TdxCustomDrawCellEvent read FOnCustomDrawCell write FOnCustomDrawCell;
    property OnCustomDrawCellEx: TdxCustomDrawCellExEvent read FOnCustomDrawCellEx write FOnCustomDrawCellEx;
    property OnGetCustomPageBreaks;
    property OnInitializeItem;
  end;

  TdxGridReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    lblPreview: TdxLayoutItem;
    pnlPreview: TPanel;
    pcMain: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    tshOptions: TdxLayoutGroup;
    tshColor: TdxLayoutGroup;
    tshFont: TdxLayoutGroup;
    tshBehaviors: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    lblShow: TcxLabel;
    dxLayoutItem2: TdxLayoutItem;
    lblOnEveryPage: TcxLabel;
    dxLayoutItem3: TdxLayoutItem;
    imgGrid: TcxImage;
    dxLayoutItem4: TdxLayoutItem;
    Image1: TcxImage;
    dxLayoutItem5: TdxLayoutItem;
    chbxShowBorders: TcxCheckBox;
    dxLayoutItem6: TdxLayoutItem;
    chbxShowHorzLines: TcxCheckBox;
    dxLayoutItem7: TdxLayoutItem;
    chbxShowVertLines: TcxCheckBox;
    dxLayoutItem8: TdxLayoutItem;
    chbxShowFixedHorzLines: TcxCheckBox;
    dxLayoutItem9: TdxLayoutItem;
    chbxShowFixedVertLines: TcxCheckBox;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutItem10: TdxLayoutItem;
    chbxFixedRowsOnEveryPage: TcxCheckBox;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    lblDrawMode: TdxLayoutItem;
    cbxDrawMode: TcxImageComboBox;
    dxLayoutItem11: TdxLayoutItem;
    chbxTransparent: TcxCheckBox;
    dxLayoutItem12: TdxLayoutItem;
    stTransparent: TcxLabel;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    lblColor: TdxLayoutItem;
    ccbxColor: TcxColorComboBox;
    lblEvenColor: TdxLayoutItem;
    ccbxEvenColor: TcxColorComboBox;
    dxLayoutItem13: TdxLayoutItem;
    chbxFixedTransparent: TcxCheckBox;
    dxLayoutItem14: TdxLayoutItem;
    stFixedTransparent: TcxLabel;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    lblFixedColor: TdxLayoutItem;
    ccbxFixedColor: TcxColorComboBox;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    lblGridLinesColor: TdxLayoutItem;
    ccbxGridLineColor: TcxColorComboBox;
    dxLayoutItem15: TdxLayoutItem;
    btnFont: TcxButton;
    dxLayoutItem16: TdxLayoutItem;
    edFont: TcxTextEdit;
    dxLayoutItem17: TdxLayoutItem;
    btnEvenFont: TcxButton;
    dxLayoutItem18: TdxLayoutItem;
    edEvenFont: TcxTextEdit;
    dxLayoutItem19: TdxLayoutItem;
    btnFixedFont: TcxButton;
    dxLayoutItem20: TdxLayoutItem;
    edFixedFont: TcxTextEdit;
    dxLayoutItem21: TdxLayoutItem;
    lblSelection: TcxLabel;
    dxLayoutItem22: TdxLayoutItem;
    lblLookAndFeel: TcxLabel;
    dxLayoutItem23: TdxLayoutItem;
    lblMiscellaneous: TcxLabel;
    dxLayoutItem24: TdxLayoutItem;
    Image2: TcxImage;
    dxLayoutItem25: TdxLayoutItem;
    Image3: TcxImage;
    dxLayoutItem26: TdxLayoutItem;
    imgMiscellaneous: TcxImage;
    dxLayoutItem27: TdxLayoutItem;
    chbxOnlySelected: TcxCheckBox;
    dxLayoutItem28: TdxLayoutItem;
    chbxIncludeFixed: TcxCheckBox;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutItem29: TdxLayoutItem;
    chbxUse3DEffects: TcxCheckBox;
    dxLayoutItem30: TdxLayoutItem;
    chbxUseSoft3D: TcxCheckBox;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutItem31: TdxLayoutItem;
    chbxAutoWidth: TcxCheckBox;
    lichbxRowAutoHeight: TdxLayoutItem;
    chbxRowAutoHeight: TcxCheckBox;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup;
    procedure ccbxColorChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
    procedure chbxOnlySelectedClick(Sender: TObject);
    procedure chbxIncludeFixedClick(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure lblColorClick(Sender: TObject);
    procedure chbxRowAutoHeightClick(Sender: TObject);
    procedure chbxFixedRowsOnEveryPageClick(Sender: TObject);
    procedure chbxShowBordersClick(Sender: TObject);
    procedure cbxDrawModeClick(Sender: TObject);
    procedure chbxUse3DEffectsClick(Sender: TObject);
    procedure chbxUseSoft3DClick(Sender: TObject);
    procedure chbxAutoWidthClick(Sender: TObject);
    procedure stFixedTransparentClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
  private
    FPreviewBox: TCustomControl;
    function GetReportLink: TStddxGridReportLink;
    procedure CreateControls;
    procedure CMDialogChar(var message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadGroupsIcons; override;
    procedure LoadStrings; override;
    procedure PaintPreview(ACanvas: TCanvas; R: TRect); override;
    procedure UpdateControlsState; override;
    procedure UpdatePreview; override;
  public
    constructor Create(AOwner: TComponent); override;

    property ReportLink: TStddxGridReportLink read GetReportLink;
  end;

implementation

{$R *.DFM}

uses
  dxExtCtrls, dxPrnDev, dxPSUtl, dxPSImgs;

type

  { TdxReportCustomDrawCellData }

  TdxReportCustomDrawCellData = class(TAbstractdxReportCellData)
  private
    FDefaultDrawing: Boolean;
  protected
    class function Serializable: Boolean; override;
    function CustomDraw(ACanvas: TdxPSReportRenderCustomCanvas): Boolean; override;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;
    //
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing;
  end;

{ TStddxGridReportLink }

constructor TStddxGridReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSaveFont := TFont.Create;
  FSaveFont.OnChange := CustomDrawFontChanged;
end;

destructor TStddxGridReportLink.Destroy;
begin
  FreeAndNil(FSaveFont);
  inherited Destroy;
end;

class function TStddxGridReportLink.IsDrawGridLink: Boolean;
begin
  Result := False;
end;

class function TStddxGridReportLink.IsStringGridLink: Boolean;
begin
  Result := False;
end;

procedure TStddxGridReportLink.CustomDrawFontChanged(Sender: TObject);
begin
  FCustomDrawFontChanged := True;
end;

{ TdxStringGridReportLink }

procedure TdxStringGridReportLink.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxStringGridReportLink then
  begin
    TextAlignX := TdxStringGridReportLink(Source).TextAlignX;
    TextAlignY := TdxStringGridReportLink(Source).TextAlignY;
  end;
end;

class function TdxStringGridReportLink.IsStringGridLink: Boolean;
begin
  Result := True;
end;

procedure TdxStringGridReportLink.InternalRestoreDefaults;
begin
  inherited InternalRestoreDefaults;
  TextAlignX := dxPSCore.dxDefaultTextAlignX; {taLeft}
  TextAlignY := dxPSCore.dxDefaultTextAlignY; {taCenterY}
end;

function TdxStringGridReportLink.GetStringGrid: TStringGrid;
begin
  Result := TStringGrid(Component);
end;

procedure TdxStringGridReportLink.SetTextAlignX(Value: TcxTextAlignX);
begin
  if FTextAlignX <> Value then
  begin
    FTextAlignX := Value;
    LinkModified(True);
  end;
end;

function TdxStringGridReportLink.GetCellText(ACol, ARow: Integer): string;
begin
  Result := StringGrid.Cells[ACol, ARow];
end;

function TdxStringGridReportLink.GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX;
begin
  if DrawMode <> gdmBorrowSource then
    Result := FTextAlignX
  else
    Result := inherited GetCellTextAlignX(ACol, ARow);
end;

function TdxStringGridReportLink.GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  if DrawMode <> gdmBorrowSource then
    Result := FTextAlignY
  else
    Result := inherited GetCellTextAlignY(ACol, ARow);
end;

procedure TdxStringGridReportLink.SetTextAlignY(Value: TcxTextAlignY);
begin
  if FTextAlignY <> Value then
  begin
    FTextAlignY := Value;
    LinkModified(True);
  end;
end;

procedure TdxStringGridReportLink.SetDrawMode(Value: TdxGridDrawMode);
begin
  if Value > gdmChess then
    Value := gdmChess;
  inherited SetDrawMode(Value);
end;

function TdxStringGridReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := inherited IsSupportedCustomDraw(Item) and Assigned(FOnCustomDrawCell);
end;

procedure TdxStringGridReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  AColor: TColor;
  AText: string;
  ACol, ARow: Integer;
  ATextAlignX: TcxTextAlignX;
  ATextAlignY: TcxTextAlignY;
begin
  with TdxReportCellString(AItem) do
  begin
    GetCellColRow(AItem, ACol, ARow);
    AColor := ColorToRGB(Color);
    if Transparent then
      AColor := clNone;
    FSaveFont.Assign(Font);
    FCustomDrawFontChanged := False;
    AText := Text;
    ATextAlignX := TextAlignX;
    ATextAlignY := TextAlignY;
    DoCustomDrawCell(ACol, ARow, ACanvas, ABoundsRect, AClientRect, AText, FSaveFont, AColor, ATextAlignX, ATextAlignY, ADone);
    if not ADone then
    begin
      if FCustomDrawFontChanged then
      begin
        SelectObject(ACanvas.Handle, FSaveFont.Handle);
        SetTextColor(ACanvas.Handle, ColorToRGB(FSaveFont.Color));
        FontIndex := -1;
      end;
      if AColor <> clNone then
      begin
        AItem.Color := AColor;
        AItem.Transparent := False;
      end;
      Text := AText;
      TextAlignX := ATextAlignX;
      TextAlignY := ATextAlignY;
    end;
  end;
end;

procedure TdxStringGridReportLink.DoCustomDrawCell(ACol, ARow: Integer; ACanvas: TCanvas;
  ABoundsRect, AClientRect: TRect; var AText: string; AFont: TFont; var AColor: TColor;
  var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawCell) then
    FOnCustomDrawCell(Self, ACol, ARow, ACanvas, ABoundsRect, AClientRect, AText,
      AFont, AColor, ATextAlignX, ATextAlignY, ADone);
end;

{ TdxReportCustomDrawCellData }

constructor TdxReportCustomDrawCellData.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  DefaultDrawing := True;
end;

procedure TdxReportCustomDrawCellData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportCustomDrawCellData then
    DefaultDrawing := TdxReportCustomDrawCellData(Source).DefaultDrawing;
end;

class function TdxReportCustomDrawCellData.Serializable: Boolean;
begin
  Result := False;
end;

function TdxReportCustomDrawCellData.CustomDraw(ACanvas: TdxPSReportRenderCustomCanvas): Boolean;
begin
  if DefaultDrawing then
  begin
    if IsBackgroundDrawn then
      DrawBackground(ACanvas);
    if IsBordersDrawn then
      DrawBorders(ACanvas);
  end;
  inherited CustomDraw(ACanvas);
  Result := True;
end;

{ TdxDrawGridReportLink }

procedure TdxDrawGridReportLink.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxDrawGridReportLink then
    DefaultDrawing := TdxDrawGridReportLink(Source).DefaultDrawing;
end;

class function TdxDrawGridReportLink.Serializable: Boolean;
begin
  Result := False;
end;

class function TdxDrawGridReportLink.IsDrawGridLink: Boolean;
begin
  Result := True;
end;

procedure TdxDrawGridReportLink.InternalRestoreDefaults;
begin
  inherited InternalRestoreDefaults;
  DefaultDrawing := True;
  SupportedCustomDraw := True;
end;

function TdxDrawGridReportLink.GetCellText(ACol, ARow: Integer): string;
begin
  Result := '';
end;

procedure TdxDrawGridReportLink.InternalRestoreFromOriginal;
begin
  inherited InternalRestoreFromOriginal;
  if Assigned(DrawGrid) then
    DefaultDrawing := DrawGrid.DefaultDrawing;
end;

function TdxDrawGridReportLink.GetDrawGrid: TDrawGrid;
begin
  Result := TDrawGrid(Component);
end;

procedure TdxDrawGridReportLink.SetDefaultDrawing(Value: Boolean);
begin
  if DefaultDrawing <> Value then
  begin
    FDefaultDrawing := Value;
    if SupportedCustomDraw then
      LinkModified(True);
  end;
end;

procedure TdxDrawGridReportLink.SetDrawMode(Value: TdxGridDrawMode);
begin
  if Value > gdmChess then
    Value := gdmChess;
  inherited SetDrawMode(Value);
end;

function TdxDrawGridReportLink.GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass;
begin
  Result := TdxReportCustomDrawCellData;
end;

procedure TdxDrawGridReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  ACol, ARow: Integer;
begin
  with TdxReportCustomDrawCellData(AItem) do
  begin
    GetCellColRow(AItem, ACol, ARow);
    FSaveFont.Assign(Font);
    DoCustomDrawCellEx(ACol, ARow, ACanvas, FSaveFont, ABoundsRect, AClientRect);
  end;
  ADone := True;
end;

procedure TdxDrawGridReportLink.DoCustomDrawCellEx(
  ACol, ARow: Integer; ACanvas: TCanvas; AFont: TFont; ABoundsRect, AClientRect: TRect);
var
  ADone: Boolean;
begin
  ADone := False;
  if Assigned(FOnCustomDrawCellEx) then
    FOnCustomDrawCellEx(Self, ACol, ARow, ACanvas, AFont, ABoundsRect, AClientRect, ADone);
  if not ADone then
    DoCustomDrawCell(ACol, ARow, ACanvas, ABoundsRect, AClientRect);
end;

procedure TdxDrawGridReportLink.DoCustomDrawCell(
  ACol, ARow: Integer; ACanvas: TCanvas; ABoundsRect, AClientRect: TRect);
begin
  if Assigned(FOnCustomDrawCell) then
    FOnCustomDrawCell(Self, ACol, ARow, ACanvas, ABoundsRect, AClientRect);
end;

function TdxDrawGridReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := inherited IsSupportedCustomDraw(Item) and (Item is TdxReportCustomDrawCellData) and
   (Assigned(FOnCustomDrawCellEx) or Assigned(FOnCustomDrawCell));
end;

{ TdxSGrReportLinkDesignWindow }

constructor TdxGridReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxPSGlbl.dxhcStringGridReportLinkDesigner;
  inherited Create(AOwner);
  CreateControls;
end;

function TdxGridReportLinkDesignWindow.GetReportLink: TStddxGridReportLink;
begin
  Result := inherited ReportLink as TStddxGridReportLink;
end;

function TdxGridReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxGridReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgGrid, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(Image1, IDB_DXPSGROUPICON_ONEVERYPAGE);
  dxLoadIconFromResourceEx(Image2, IDB_DXPSGROUPICON_SELECTION);
  dxLoadIconFromResourceEx(Image3, IDB_DXPSGROUPICON_LOOKANDFEEL);
  dxLoadIconFromResourceEx(imgMiscellaneous, IDB_DXPSGROUPICON_MISCELLANEOUS);
end;

procedure TdxGridReportLinkDesignWindow.LoadStrings;
begin
  inherited LoadStrings;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  tshFont.Caption := cxGetResourceString(@sdxFonts);
  tshColor.Caption := cxGetResourceString(@sdxColors);
  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviors);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorderLines);
  chbxShowHorzLines.Caption := cxGetResourceString(@sdxHorzLines);
  chbxShowVertLines.Caption := cxGetResourceString(@sdxVertLines);
  chbxShowFixedHorzLines.Caption := cxGetResourceString(@sdxFixedHorzLines);
  chbxShowFixedVertLines.Caption := cxGetResourceString(@sdxFixedVertLines);

  lblMiscellaneous.Caption := cxGetResourceString(@sdxMiscellaneous);
  chbxRowAutoHeight.Caption := cxGetResourceString(@sdxRowAutoHeight);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  lblDrawMode.Caption := cxGetResourceString(@sdxDrawMode);
  dxPSInitalizeDrawModeCombo(cbxDrawMode, [gdmStrict..gdmChess]);

  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblEvenColor.Caption := cxGetResourceString(@sdxEvenColor);
  stFixedTransparent.Caption := ' ' + cxGetResourceString(@sdxFixedTransparent) + ' ';
  lblFixedColor.Caption := cxGetResourceString(@sdxFixedColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);

  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnEvenFont.Caption := cxGetResourceString(@sdxBtnEvenFont);
  btnFixedFont.Caption := cxGetResourceString(@sdxBtnFixedFont);

  lblOnEveryPage.Caption := cxGetResourceString(@sdxOnEveryPage);
  chbxFixedRowsOnEveryPage.Caption := cxGetResourceString(@sdxFixedRowOnEveryPage);

  lblSelection.Caption := cxGetResourceString(@sdxSelection);
  chbxOnlySelected.Caption := cxGetResourceString(@sdxOnlySelected);
  chbxIncludeFixed.Caption := cxGetResourceString(@sdxIncludeFixed);

  lblLookAndFeel.Caption := cxGetResourceString(@sdxLookAndFeel);
  chbxUse3DEffects.Caption := cxGetResourceString(@sdxUse3DEffects);
  chbxUseSoft3D.Caption := cxGetResourceString(@sdxSoft3D);
end;

procedure TdxGridReportLinkDesignWindow.DoInitialize;
begin
  inherited DoInitialize;
  chbxShowBorders.Checked := gpoBorder in ReportLink.Options;
  chbxShowHorzLines.Checked := gpoHorzLines in ReportLink.Options;
  chbxShowVertLines.Checked := gpoVertLines in ReportLink.Options;
  chbxShowFixedHorzLines.Checked := gpoFixedHorzLines in ReportLink.Options;
  chbxShowFixedVertLines.Checked := gpoFixedVertLines in ReportLink.Options;

  lichbxRowAutoHeight.Visible := ReportLink.IsStringGridLink;
  if lichbxRowAutoHeight.Visible then
    chbxRowAutoHeight.Checked := TdxStringGridReportLink(ReportLink).RowAutoHeight;
  chbxAutoWidth.Checked := ReportLink.AutoWidth;
  dxPSSyncDrawModeComboItemIndex(cbxDrawMode, ReportLink.DrawMode);

  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ColorToRGB(ReportLink.Color);
  ccbxEvenColor.ColorValue := ColorToRGB(ReportLink.EvenColor);
  chbxFixedTransparent.Checked := ReportLink.FixedTransparent;
  ccbxFixedColor.ColorValue := ColorToRGB(ReportLink.FixedColor);
  ccbxGridLineColor.ColorValue := ColorToRGB(ReportLink.GridLineColor);

  FontInfoToText(ReportLink.Font, edFont);
  FontInfoToText(ReportLink.EvenFont, edEvenFont);
  FontInfoToText(ReportLink.FixedFont, edFixedFont);

  chbxFixedRowsOnEveryPage.Checked := ReportLink.HeadersOnEveryPage;
  chbxOnlySelected.Checked := ReportLink.OnlySelected;
  chbxIncludeFixed.Checked := ReportLink.IncludeFixed;

  chbxUse3DEffects.Checked := ReportLink.Effects3D;
  chbxUseSoft3D.Checked := ReportLink.Soft3D;
end;

procedure TdxGridReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
begin
  inherited;
  dxPSDrawGridPreview(ACanvas, R, ReportLink, True, True, ScaleFactor);
end;

procedure TdxGridReportLinkDesignWindow.UpdateControlsState;
begin
  inherited UpdateControlsState;

  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := ccbxColor.Enabled;
  ccbxEvenColor.Enabled := not chbxTransparent.Checked and (ReportLink.DrawMode in [gdmOddEven, gdmChess]);
  lblEvenColor.Enabled := ccbxEvenColor.Enabled;
  ccbxFixedColor.Enabled := not chbxFixedTransparent.Checked;
  lblFixedColor.Enabled := ccbxFixedColor.Enabled;

  btnEvenFont.Enabled := ReportLink.DrawMode in [gdmOddEven, gdmChess];
  if ReportLink.DrawMode in [gdmOddEven, gdmChess] then
  begin
    lblColor.Caption := cxGetResourceString(@sdxOddColor);
    btnFont.Caption := cxGetResourceString(@sdxBtnOddFont);
  end
  else
  begin
    lblColor.Caption := cxGetResourceString(@sdxColor);
    btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  end;
  chbxFixedRowsOnEveryPage.Enabled := not ReportLink.IsAggregated;
  chbxIncludeFixed.Enabled := chbxOnlySelected.Enabled and chbxOnlySelected.Checked;
  chbxUseSoft3D.Enabled := chbxUse3DEffects.Checked;
end;

procedure TdxGridReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

procedure TdxGridReportLinkDesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  FPreviewBox.Parent := pnlPreview;
  FPreviewBox.Align := alClient;
  TdxPSPaintPanel(FPreviewBox).EdgeInner := esNone;
  TdxPSPaintPanel(FPreviewBox).EdgeOuter := esNone;
  TdxPSPaintPanel(FPreviewBox).OnPaint := pbxPreviewPaint;
end;

procedure TdxGridReportLinkDesignWindow.CMDialogChar(var message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(message.CharCode, pcMain.Items[I].Caption) then
    begin
      pcMain.ItemIndex := I;
      message.Result := 1;
      Exit;
    end;
end;

procedure TdxGridReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TdxGridReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
var
  AColor: TColor;
begin
  if not LockControlsUpdate then
  begin
    AColor := TcxColorComboBox(Sender).ColorValue;
    case TTagToInt(TcxColorComboBox(Sender).Tag) of
      0: ReportLink.Color := AColor;
      1: ReportLink.EvenColor := AColor;
      2: ReportLink.FixedColor := AColor;
      3: ReportLink.GridLineColor := AColor;
    end;
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxGridReportLinkDesignWindow.btnFontClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;

  with dxPSGlbl.FontDialog do
  begin
    case TTagToInt(TcxButton(Sender).Tag) of
      0: Font := ReportLink.Font;
      1: Font := ReportLink.EvenFont;
      2: Font := ReportLink.FixedFont;
    end;

    if Execute then
    begin
      case TTagToInt(TcxButton(Sender).Tag) of
        0:
          begin
            ReportLink.Font := Font;
            FontInfoToText(ReportLink.Font, edFont);
          end;
        1:
          begin
            ReportLink.EvenFont := Font;
            FontInfoToText(ReportLink.EvenFont, edEvenFont);
          end;
        2:
          begin
            ReportLink.FixedFont := Font;
            FontInfoToText(ReportLink.FixedFont, edFixedFont);
          end;
      end;
      Modified := True;
      UpdatePreview;
    end;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxRowAutoHeightClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    if ReportLink.IsStringGridLink then
      TdxStringGridReportLink(ReportLink).RowAutoHeight := TcxCheckBox(Sender).Checked;
    Modified := True;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxAutoWidthClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.AutoWidth := TcxCheckBox(Sender).Checked;
    Modified := True;
  end;
end;

procedure TdxGridReportLinkDesignWindow.cbxDrawModeClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.DrawMode := dxPSGetSelectedDrawMode(cbxDrawMode);
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxOnlySelectedClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.OnlySelected := TcxCheckBox(Sender).Checked;
    Modified := True;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxIncludeFixedClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.IncludeFixed := TcxCheckBox(Sender).Checked;
    Modified := True;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxUse3DEffectsClick(
  Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.Effects3D := TcxCheckBox(Sender).Checked;
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxUseSoft3DClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.Soft3D := TcxCheckBox(Sender).Checked;
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxShowBordersClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    with TcxCheckBox(Sender) do
      if Checked then
        ReportLink.Options := ReportLink.Options + [TdxGridPaintOption(TTagToInt(Tag))]
      else
        ReportLink.Options := ReportLink.Options - [TdxGridPaintOption(TTagToInt(Tag))];
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxFixedRowsOnEveryPageClick(
  Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.HeadersOnEveryPage := TcxCheckBox(Sender).Checked;
    Modified := True;
  end;
end;

procedure TdxGridReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    case TTagToInt(TcxCheckBox(Sender).Tag) of
      0: ReportLink.Transparent := TcxCheckBox(Sender).Checked;
      1: ReportLink.FixedTransparent := TcxCheckBox(Sender).Checked;
    end;
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxGridReportLinkDesignWindow.stFixedTransparentClick(Sender: TObject);
begin
  if chbxFixedTransparent.CanFocus then
    ActiveControl := chbxFixedTransparent;
  chbxFixedTransparent.Checked := not chbxFixedTransparent.Checked;
end;

procedure TdxGridReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxGridReportLinkDesignWindow.lblColorClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

initialization
  dxPSRegisterReportLink(TdxStringGridReportLink, TStringGrid, TdxGridReportLinkDesignWindow);
  dxPSRegisterReportLink(TdxDrawGridReportLink, TDrawGrid, TdxGridReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxDrawGridReportLink, TDrawGrid, TdxGridReportLinkDesignWindow);
  dxPSUnregisterReportLink(TdxStringGridReportLink, TStringGrid, TdxGridReportLinkDesignWindow);

end.

