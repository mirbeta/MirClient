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

unit dxPSGrLnks;

interface

{$I cxVer.inc}

uses
  Windows, Forms, Classes, Graphics, StdCtrls, Grids, dxPSCore, dxPSBaseGridLnk,
  dxPSGlbl, cxDrawTextUtils;

type
  TdxGridPaintOption = (gpoBorder, gpoHorzLines, gpoVertLines, gpoFixedHorzLines, gpoFixedVertLines);
  TdxGridPaintOptions = set of TdxGridPaintOption;

  TCustomdxGridReportLink = class(TAbstractdxGridReportLink)
  private
    FOptions: TdxGridPaintOptions;
    function GetCustomGrid: TCustomGrid;
    function GetOptions: TdxGridPaintOptions;
    procedure SetOptions(Value: TdxGridPaintOptions);
  protected
    FSourceFontIndex: Integer;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;

    function GetColCount: Integer; override;
    function GetFixedColCount: Integer; override;
    function GetFixedRowCount: Integer; override;
    function GetRowCount: Integer; override;

    function GetSelectionRect: TRect; override;
    function GetSourceCellColor(ACol, ARow: Integer): TColor; override;
    function GetSourceCellEdgeMode(ACol, ARow: Integer): TdxCellEdgeMode; override;
    function GetSourceCellFont(ACol, ARow: Integer): TFont; override;
    function GetSourceCellFontIndex(ACol, ARow: Integer): Integer; override;
    function GetSourceCellTransparent(ACol, ARow: Integer): Boolean; override;
    function GetSourceColWidth(ACol: Integer): Integer; override;
    function GetSourceRowHeight(ARow: Integer): Integer; override;

    function IsDrawBorder: Boolean; override;
    function IsDrawFixedHorzLines: Boolean; override;
    function IsDrawFixedVertLines: Boolean; override;
    function IsDrawHorzLines: Boolean; override;
    function IsDrawVertLines: Boolean; override;

    procedure PrepareConstruct(AReportCells: TdxReportCells); override;

    property CustomGrid: TCustomGrid read GetCustomGrid;
  public
    procedure Assign(Source: TPersistent); override;

    property AutoWidth;
    property DrawMode;
    property Effects3D;
    property EvenColor;
    property EvenFont;
    property FixedColor;
    property FixedFont;
    property FixedTransparent;
    property GridLineColor;
    property HeadersOnEveryPage;
    property IncludeFixed;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property Options: TdxGridPaintOptions read GetOptions write SetOptions
      default [gpoBorder, gpoHorzLines, gpoVertLines, gpoFixedHorzLines, gpoFixedVertLines];
    property Soft3D;
    property SupportedCustomDraw;
    property Transparent;
  end;

  TdxCustomDrawItemEvent = procedure(Sender: TBasedxReportLink;
    Index: Integer; ACanvas: TCanvas; ABoundsRect, AClientRect: TRect;
    var AText: string; AFont: TFont; var AColor: TColor;
    var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
    var ADone: Boolean) of object;

  TdxCustomListBoxReportLink = class(TAbstractdxGridReportLink)
  private
    FCustomDrawFontChanged: Boolean;
    FIsWidthAssigned: Boolean;
    FSaveFont: TFont;
    FTextAlignX: TcxTextAlignX;
    FTextAlignY: TcxTextAlignY;
    FWidth: Integer;
    FOnCustomDrawItem: TdxCustomDrawItemEvent;
    function GetWidth: Integer;
    function IsWidthStored: Boolean;
    procedure SetTextAlignX(Value: TcxTextAlignX);
    procedure SetTextAlignY(Value: TcxTextAlignY);
    procedure SetWidth(Value: Integer);
    procedure CustomDrawFontChanged(Sender: TObject);
  protected
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure DoCustomDrawItem(Index: Integer; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var AText: string; AFont: TFont;
      var AColor: TColor; var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
      var ADone: Boolean); virtual;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;
    function IsSupportedCustomDraw(AItem: TAbstractdxReportCellData): Boolean; override;

    function GetColCount: Integer; override;
    function GetRowCount: Integer; override;

    function GetCellSides(ACol, ARow: Integer): TdxCellSides; override;
    function GetCellText(ACol, ARow: Integer): string; override;
    function GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX; override;
    function GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; override;
    function GetSourceColWidth(ACol: Integer): Integer; override;
    function GetSourceRowHeight(ARow: Integer): Integer; override;
    function GetSelectedColCount: Integer; override;
    function GetSelectedRowCount: Integer; override;
    function HasSelection: Boolean; override;
    function HasSelectionInRow(ARow: Integer): Boolean; override;
    function IsDrawBorder: Boolean; override;
    function IsDrawHorzLines: Boolean; override;
    function IsSelectedCell(ACol, ARow: Integer): Boolean; override;
    function IsSelectedRow(ARow: Integer): Boolean; override;

    function GetCustomListBox: TCustomListBox; virtual;

    property CustomListBox: TCustomListBox read GetCustomListBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultWidth: Integer; virtual;

    property AutoWidth;
    property DrawMode;
    property EndEllipsis;
    property GridLineColor;
    property OnlySelected;
    property SupportedCustomDraw;
    property TextAlignX: TcxTextAlignX read FTextAlignX write SetTextAlignX default taLeft;
    property TextAlignY: TcxTextAlignY read FTextAlignY write SetTextAlignY default taCenterY;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property OnCustomDrawItem: TdxCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
  end;

const
  dxDefaultGridPaintOptions: TdxGridPaintOptions =
    [gpoBorder, gpoHorzLines, gpoVertLines, gpoFixedHorzLines, gpoFixedVertLines];
  dxDefaultListBoxWidth = 400;

implementation

uses
  dxPSRes, SysUtils, dxPSUtl;

type
  TCustomGridAccess = class(TCustomGrid);
  TCustomListBoxAccess = class(TCustomListBox);

{ CustomGrid Helpers }

function GridGetColCount(ACustomGrid: TCustomGrid): Integer;
begin
  Result := TCustomGridAccess(ACustomGrid).ColCount;
end;

function GridGetColor(ACustomGrid: TCustomGrid): TColor;
begin
  Result := TCustomGridAccess(ACustomGrid).Color;
end;

function GridGetColWidth(ACustomGrid: TCustomGrid; AColIndex: Integer): Integer;
begin
  Result := TCustomGridAccess(ACustomGrid).ColWidths[AColIndex];
end;

function GridGetFixedColor(ACustomGrid: TCustomGrid): TColor;
begin
  Result := TCustomGridAccess(ACustomGrid).FixedColor;
end;

function GridGetFixedCols(ACustomGrid: TCustomGrid): Integer;
begin
  Result := TCustomGridAccess(ACustomGrid).FixedCols;
end;

function GridGetFixedRows(ACustomGrid: TCustomGrid): Integer;
begin
  Result := TCustomGridAccess(ACustomGrid).FixedRows;
end;

function GridGetFont(ACustomGrid: TCustomGrid): TFont;
begin
  Result := TCustomGridAccess(ACustomGrid).Font;
end;

function GridGetOptions(ACustomGrid: TCustomGrid): TGridOptions;
begin
  Result := TCustomGridAccess(ACustomGrid).Options;
end;

function GridGetBorderStyle(ACustomGrid: TCustomGrid): TBorderStyle;
begin
  Result := TCustomGridAccess(ACustomGrid).BorderStyle;
end;

function GridGetRowCount(ACustomGrid: TCustomGrid): Integer;
begin
  Result := TCustomGridAccess(ACustomGrid).RowCount;
end;

function GridGetRowHeight(ACustomGrid: TCustomGrid; ARowIndex: Integer): Integer;
begin
  Result := TCustomGridAccess(ACustomGrid).RowHeights[ARowIndex];
end;

function GridGetSelection(ACustomGrid: TCustomGrid): TRect;
begin
  Result := TRect(TCustomGridAccess(ACustomGrid).Selection);
end;

{ CustomListBox Helpers }

function ListBoxGetItemHeight(ACustomListBox: TCustomListBox): Integer;
begin
  Result := TCustomListBoxAccess(ACustomListBox).ItemHeight;
end;

function ListBoxGetItemText(ACustomListBox: TCustomListBox; AnIndex: Integer): string;
begin
  Result := TCustomListBoxAccess(ACustomListBox).Items[AnIndex];
end;

function ListBoxGetStyle(ACustomListBox: TCustomListBox): TListBoxStyle;
begin
  Result := TCustomListBoxAccess(ACustomListBox).Style;
end;

function ListBoxMeasureItem(ACustomListBox: TCustomListBox; AnIndex: Integer): Integer;
begin
  TCustomListBoxAccess(ACustomListBox).MeasureItem(AnIndex, Result);
end;


{ TCustomdxGridReportLink }

procedure TCustomdxGridReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCustomdxGridReportLink then
    Options := TCustomdxGridReportLink(Source).Options;
end;

procedure TCustomdxGridReportLink.InternalRestoreDefaults;
begin
  inherited;
  Options := dxDefaultGridPaintOptions; {[Low(TdxDrawGridPaintOption)..High(TdxDrawGridPaintOption)]}
end;

procedure TCustomdxGridReportLink.InternalRestoreFromOriginal;

  procedure XorOption(var AOptions: TdxGridPaintOptions; AElement: TdxGridPaintOption;
     Value: Boolean);
  begin
    if Value then
      AOptions := AOptions + [AElement]
    else
      AOptions := AOptions - [AElement];
  end;

var
  Opt: TdxGridPaintOptions;
begin
  inherited;
  FixedColor := GridGetFixedColor(CustomGrid);
  Opt := Options;
  XorOption(Opt, gpoFixedVertLines, goFixedVertLine in GridGetOptions(CustomGrid));
  XorOption(Opt, gpoFixedHorzLines, goFixedHorzLine in GridGetOptions(CustomGrid));
  XorOption(Opt, gpoVertLines, goVertLine in GridGetOptions(CustomGrid));
  XorOption(Opt, gpoHorzLines, goHorzLine in GridGetOptions(CustomGrid));
  XorOption(Opt, gpoBorder, GridGetBorderStyle(CustomGrid) <> bsNone);
  Options := Opt;
end;

function TCustomdxGridReportLink.IsDrawBorder: Boolean;
begin
  Result := gpoBorder in Options;
end;

function TCustomdxGridReportLink.IsDrawHorzLines: Boolean;
begin
  Result := gpoHorzLines in Options;
end;

function TCustomdxGridReportLink.IsDrawVertLines: Boolean;
begin
  Result := gpoVertLines in Options;
end;

function TCustomdxGridReportLink.IsDrawFixedHorzLines: Boolean;
begin
  Result := gpoFixedHorzLines in Options;
end;

function TCustomdxGridReportLink.IsDrawFixedVertLines: Boolean;
begin
  Result := gpoFixedVertLines in Options;
end;

function TCustomdxGridReportLink.GetColCount: Integer;
begin
  Result := GridGetColCount(CustomGrid);
end;

function TCustomdxGridReportLink.GetSourceColWidth(ACol: Integer): Integer;
begin
  Result := GridGetColWidth(CustomGrid, ACol);
end;

function TCustomdxGridReportLink.GetSourceRowHeight(ARow: Integer): Integer;
begin
  Result := GridGetRowHeight(CustomGrid, ARow);
end;

function TCustomdxGridReportLink.GetFixedColCount: Integer;
begin
  Result := GridGetFixedCols(CustomGrid);
end;

function TCustomdxGridReportLink.GetFixedRowCount: Integer;
begin
  Result := GridGetFixedRows(CustomGrid);
end;

function TCustomdxGridReportLink.GetRowCount: Integer;
begin
  Result := GridGetRowCount(CustomGrid);
end;

function TCustomdxGridReportLink.GetSelectionRect: TRect;
begin
  Result := GridGetSelection(CustomGrid);
end;

function TCustomdxGridReportLink.GetSourceCellColor(ACol, ARow: Integer): TColor;
begin
  if IsFixedCell(ACol, ARow) then
    Result := GridGetFixedColor(CustomGrid)
  else
    Result := GridGetColor(CustomGrid);
end;

function TCustomdxGridReportLink.GetSourceCellEdgeMode(ACol, ARow: Integer): TdxCellEdgeMode;
const
  EdgeModes: array[Boolean] of TdxCellEdgeMode = (cemPattern, cem3DEffects);
begin
  Result := EdgeModes[IsFixedCell(ACol, ARow)];
end;

function TCustomdxGridReportLink.GetSourceCellFont(ACol, ARow: Integer): TFont;
begin
  Result := GridGetFont(CustomGrid);
end;

function TCustomdxGridReportLink.GetSourceCellFontIndex(ACol, ARow: Integer): Integer;
begin
  Result := FSourceFontIndex;
end;

function TCustomdxGridReportLink.GetSourceCellTransparent(ACol, ARow: Integer): Boolean;
begin
  Result := not IsFixedCell(ACol, ARow);
end;

procedure TCustomdxGridReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  inherited;
  FSourceFontIndex := AddFontToPool(GridGetFont(CustomGrid));
end;

function TCustomdxGridReportLink.GetCustomGrid: TCustomGrid;
begin
  Result := TCustomGrid(Component);
end;

function TCustomdxGridReportLink.GetOptions: TdxGridPaintOptions;
begin
  Result := FOptions;
end;

procedure TCustomdxGridReportLink.SetOptions(Value: TdxGridPaintOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    LinkModified(True);
  end;
end;

{ TdxCustomListBoxReportLink }

constructor TdxCustomListBoxReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FTextAlignX := taLeft;
  FTextAlignY := taCenterY;
  FSaveFont := TFont.Create;
  FSaveFont.OnChange := CustomDrawFontChanged;
end;

destructor TdxCustomListBoxReportLink.Destroy;
begin
  FreeAndNil(FSaveFont);
  inherited;
end;

procedure TdxCustomListBoxReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCustomListBoxReportLink then
    with TdxCustomListBoxReportLink(Source) do
    begin
      Self.FIsWidthAssigned := FIsWidthAssigned;
      Self.AutoWidth := AutoWidth;
      Self.TextAlignX := TextAlignX;
      Self.TextAlignY := TextAlignY;
      Self.Width := Width;
    end;
  inherited;
end;

function TdxCustomListBoxReportLink.DefaultWidth: Integer;
begin
  Result := dxDefaultListBoxWidth;
end;

function TdxCustomListBoxReportLink.GetCustomListBox: TCustomListBox;
begin
  Result := TCustomListBox(Component);
end;

function TdxCustomListBoxReportLink.IsSelectedCell(ACol, ARow: Integer): Boolean;
begin
  Result := IsSelectedRow(ARow);
end;

function TdxCustomListBoxReportLink.IsSelectedRow(ARow: Integer): Boolean;
begin
  Result := HasSelection and CustomListBox.Selected[ARow];
end;

procedure TdxCustomListBoxReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  AColor: TColor;
  AText: string;
  ATextAlignX: TcxTextAlignX;
  ATextAlignY: TcxTextAlignY;
begin
  with TdxReportCellString(AItem) do
  begin
    AColor := ColorToRGB(Color);
    if Transparent then AColor := clNone;
    FSaveFont.Assign(Font);
    FCustomDrawFontChanged := False;
    AText := Text;
    ATextAlignX := TextAlignX;
    ATextAlignY := TextAlignY;
    DoCustomDrawItem(AItem.Parent.Index, ACanvas, ABoundsRect, AClientRect, AText,
      FSaveFont, AColor, ATextAlignX, ATextAlignY, ADone);
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
        Color := AColor;
        AItem.Transparent := False;
      end;
      Text := AText;
      TextAlignX := ATextAlignX;
      TextAlignY := ATextAlignY;
    end;
  end;
end;

procedure TdxCustomListBoxReportLink.DoCustomDrawItem(Index: Integer;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var AText: string;
  AFont: TFont; var AColor: TColor; var ATextAlignX: TcxTextAlignX;
  var ATextAlignY: TcxTextAlignY; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawItem) then
    FOnCustomDrawItem(Self, Index, ACanvas, ABoundsRect, AClientRect, AText,
      AFont, AColor, ATextAlignX, ATextAlignY, ADone);
end;

procedure TdxCustomListBoxReportLink.InternalRestoreDefaults;
begin
  inherited;
  TextAlignX := dxPSCore.dxDefaultTextAlignX; {taLeft}
  TextAlignY := dxPSCore.dxDefaultTextAlignY; {taCenterY}
  FIsWidthAssigned := False;
end;

procedure TdxCustomListBoxReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  Width := CustomListBox.Width;
end;

function TdxCustomListBoxReportLink.IsSupportedCustomDraw(AItem: TAbstractdxReportCellData): Boolean;
begin
  Result := inherited IsSupportedCustomDraw(AItem) and Assigned(FOnCustomDrawItem);
end;

function TdxCustomListBoxReportLink.GetColCount: Integer;
begin
  Result := 1;
end;

function TdxCustomListBoxReportLink.GetRowCount: Integer;
begin
  Result := CustomListBox.Items.Count;
end;

function TdxCustomListBoxReportLink.GetCellText(ACol, ARow: Integer): string;
begin
  Result := ListBoxGetItemText(CustomListBox, ARow);
end;

function TdxCustomListBoxReportLink.GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX;
begin
  Result := FTextAlignX;
end;

function TdxCustomListBoxReportLink.GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  Result := FTextAlignY;
end;

function TdxCustomListBoxReportLink.GetCellSides(ACol, ARow: Integer): TdxCellSides;

  function IsFirstItem(AItemIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    if not OnlySelected or (CustomListBox.SelCount = 0) then
      Result := AItemIndex = 0
    else
    begin
      for I := 0 to CustomListBox.Items.Count - 1 do
        if CustomListBox.Selected[I] then
        begin
          Result := AItemIndex = I;
          Exit;
        end;
      Result := False;
    end;
  end;

  function IsLastItem(AItemIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    if not OnlySelected or (CustomListBox.SelCount = 0) then
      Result := AItemIndex = CustomListBox.Items.Count - 1
    else
    begin
      for I := CustomListBox.Items.Count - 1 downto 0 do
        if CustomListBox.Selected[I] then
        begin
          Result := AItemIndex = I;
          Exit;
        end;
      Result := False;
    end;
  end;

begin
  Result := csAll;
  if not IsDrawBorder then
  begin
    Result := Result - [csLeft, csRight];
    if IsFirstItem(ARow) then Exclude(Result, csTop);
    if IsLastItem(ARow) then Exclude(Result, csBottom);
  end;
  if not IsDrawHorzLines then
  begin
    if not IsFirstItem(ARow) then Exclude(Result, csTop);
    if not IsLastItem(ARow) then Exclude(Result, csBottom);
  end;
end;

function TdxCustomListBoxReportLink.GetSelectedColCount: Integer;
begin
  Result := 1;
end;

function TdxCustomListBoxReportLink.GetSelectedRowCount: Integer;
begin
  Result := CustomListBox.SelCount;
end;

function TdxCustomListBoxReportLink.GetSourceColWidth(ACol: Integer): Integer;
begin
  if IsAggregated then
    Result := CustomListBox.Width
  else
    Result := Width;
end;

function TdxCustomListBoxReportLink.GetSourceRowHeight(ARow: Integer): Integer;
begin
  Result := 4 + ListBoxGetItemHeight(CustomListBox);
  if ListBoxGetStyle(CustomListBox) = lbOwnerDrawVariable then
    Result := ListBoxMeasureItem(CustomListBox, ARow);
  if Result < 2 then Result := 2;
end;

function TdxCustomListBoxReportLink.HasSelection: Boolean;
begin
  Result := inherited HasSelection and (CustomListBox.SelCount > 0);
end;

function TdxCustomListBoxReportLink.HasSelectionInRow(ARow: Integer): Boolean;
begin
  Result := IsSelectedRow(ARow);
end;

function TdxCustomListBoxReportLink.IsDrawBorder: Boolean;
begin
  Result := True;
end;

function TdxCustomListBoxReportLink.IsDrawHorzLines: Boolean;
begin
  Result := True;
end;

function TdxCustomListBoxReportLink.GetWidth: Integer;
begin
  if FIsWidthAssigned then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TdxCustomListBoxReportLink.IsWidthStored: Boolean;
begin
  Result := FIsWidthAssigned and (Width <> DefaultWidth);
end;

procedure TdxCustomListBoxReportLink.SetTextAlignX(Value: TcxTextAlignX);
begin
  if FTextAlignX <> Value then
  begin
    FTextAlignX := Value;
    LinkModified(True);
  end;
end;

procedure TdxCustomListBoxReportLink.SetTextAlignY(Value: TcxTextAlignY);
begin
  if FTextAlignY <> Value then
  begin
    FTextAlignY := Value;
    LinkModified(True);
  end;
end;

procedure TdxCustomListBoxReportLink.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FIsWidthAssigned := True;
    FWidth := Value;
    if not AutoWidth then LinkModified(True);
  end;
end;

procedure TdxCustomListBoxReportLink.CustomDrawFontChanged(Sender: TObject);
begin
  FCustomDrawFontChanged := True;
end;

end.
