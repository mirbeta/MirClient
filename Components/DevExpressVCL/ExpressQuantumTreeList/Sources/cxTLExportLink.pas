{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumTreeList                                   }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMTREELIST AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxTLExportLink;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Graphics, cxGeometry, Variants, Types, SysUtils, Classes, Contnrs, dxCore, dxCoreClasses, cxControls, cxClasses, cxGraphics,
  cxInplaceContainer, cxTL, cxTLStrs, cxExport, cxMaskEdit, cxImage, cxImageCombobox, RTLConsts;

procedure cxExportTLToHTML(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
procedure cxExportTLToXML(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);

procedure cxExportTLToCSV(const AFileName: string; ATreeList: TcxCustomTreeList;
  AExpand: Boolean = True; ASaveAll: Boolean = True; const ASeparator: Char = ',';
  const AFileExt: string = 'csv'; AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure cxExportTLToText(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean;
  ASaveAll: Boolean = True; const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
procedure cxExportTLToText(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: string = ''; const ABeginString: string = ''; const AEndString: string = '';
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;

procedure cxExportTLToExcel(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
procedure cxExportTLToXLSX(const AFileName: string; ATreeList: TcxCustomTreeList;
  AExpand: Boolean = True; ASaveAll: Boolean = True; AUseNativeFormat: Boolean = True;
  const AFileExt: string = 'xlsx'; AHandler: TObject = nil);

procedure cxExportTLToFile(AFileName: string; ATreeList: TcxCustomTreeList; AExportType: Integer;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparators: array of string; const AFileExt: string;
  AHandler: TObject = nil; AEncoding: TEncoding = nil);

implementation

uses
  Math, cxEdit, cxDataUtils, cxCalendar, cxCurrencyEdit, cxSpinEdit, cxCalc, cxTimeEdit, cxTextEdit, cxDBTL,
  dxGDIPlusClasses, cxExportProviders;

type
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);
  TcxCustomViewInfoItemAccess = class(TcxCustomViewInfoItem);
  TcxTreeListAccess = class(TcxCustomTreeList);
  TcxTreeListBandAccess = class(TcxTreeListBand);
  TcxTreeListEditCellViewInfoAccess = class(TcxTreeListEditCellViewInfo);
  TcxImagePropertiesAccess = class(TcxImageProperties);
  TcxTreeListExportHelper = class;

  { TcxTreeListExportViewInfo }

  TcxTreeListExportViewInfo = class(TcxTreeListViewInfo)
  private
    FHelper: TcxTreeListExportHelper;
    FNodes: TList;
  protected
    procedure AfterCalculate; override;
    procedure BeforeCalculate; override;
    procedure CalculateBackgroundParts; override;
    procedure CalculateNodesOrigin; override;
    procedure CalculateNodesViewData; override;
    function GetContentBounds: TRect; override;
    function GetIsIndicatorVisible: Boolean; override;
    function GetIsPrinting: Boolean; override;
    procedure UpdateScrollBars; override;
  public
    constructor CreateEx(AHelper: TcxTreeListExportHelper); virtual;

    property Nodes: TList read FNodes;
  end;

  TcxItemVisualInfo = class
  public
    Alignment: TAlignment;
    BorderColor: TColor;
    Borders: TcxBorders;
    FontSize: Integer;
    ViewParams: TcxViewParams;
  end;

    { TcxTreeListExportHelper }

  TcxTreeListExportHelper = class
  private
    FColumns: TcxExportScale;
    FDefaultRowHeight: Integer;
    FDefaultStyle: TcxCacheCellStyle;
    FDefaultStyleIndex: Integer;
    FExpand: Boolean;
    FHandler: TObject;
    FIsNativeFormat: Boolean;
    FNodes: TList;
    FProgressHelper: TcxProgressCalculationHelper;
    FProvider: IcxExportProvider2;
    FRows: TcxExportScale;
    FSaveAll: Boolean;
    FTreeList: TcxTreeListAccess;
    FViewInfo: TcxTreeListExportViewInfo;
    FVisualInfo: TcxItemVisualInfo;
    FVisualBackgroundItems: TdxFastObjectList;
    FVisualItemsList: TdxFastObjectList;
    function GetCells: TcxCustomControlCells;
    function GetIncludeParentNodes: Boolean;
    function GetVisualItem(AIndex: Integer): TcxExportDataItem;
    function GetVisualItemCount: Integer;
    procedure ProgressHandler(Sender: TObject; Percents: Integer);
  protected
    FCell: TdxSpreadSheetCellAccess;
    function AddVisualItem(AItemClass: TcxExportDataItemClass; const ABounds: TRect; AIsBackground: Boolean): TcxExportCustomItem;
    function AddVisualItemEx(const AItemBounds: TRect; AVisualInfo: TcxItemVisualInfo; AIsBackground: Boolean = False): TcxExportCustomItem; overload;
    function AddVisualItemEx(const AItemBounds: TRect; AStyle: Integer; AIsBackground: Boolean = False): TcxExportCustomItem; overload;
    function CanHasBorders(ACell: TcxCustomViewInfoItem): Boolean;
    procedure CreateExportCache;
    procedure CreateExportCells;
    procedure ExportCells;
    procedure ExtractNodesForExport;
    procedure DrawIndentImage(AItem: TcxExportDataItem; ACell: TcxTreeListIndentCellViewInfo);
    procedure FillArea(const ABounds: TRect; AStyleIndex: Integer;
      ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
    procedure FillRealArea(const ABounds: TRect; AStyleIndex: Integer;
      ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
    function GetCellVisualInfo(ACell: TcxCustomViewInfoItem): TcxItemVisualInfo;
    function GetDisplayFormat(AProperties: TcxCustomEditProperties): string;
    function GetDisplayFormatType(AProperties: TcxCustomEditProperties): TcxValueDisplayFormatType;
    function HasAllBorders(ACell: TcxCustomViewInfoItem): Boolean;
    function IsNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
    function IsBackgroundCell(ACell: TcxCustomViewInfoItem): Boolean;
    function IsGraphicProperties(AProperties: TcxCustomEditProperties): Boolean;
    function IsEmpty: Boolean;
    procedure PrepareExportCells;
    procedure ProcessExportCells;
    procedure RealBoundsToLogicalBounds(const ABounds: TRect; out ALogicalBounds: TRect);
    procedure RealBoundsToLogicalBoundsEx(const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
    procedure RegisterStyles;

    procedure SetRealCellStyleAndValue(const ASearchArea: TRect; AInfo: TcxExportCustomItem);

    procedure SetRealCellStyleAndValueEx(const ARealBounds, ASearchArea: TRect; AStyleIndex: Integer; AGraphic: TGraphic;
      const AValue: Variant; const AValueDisplayFormat: string = ''; const AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto; AFitMode: TcxImageFitMode = ifmStretch);
    function SkipExportCell(ACell: TcxCustomViewInfoItem): Boolean;

    procedure ViewParamsToExportStyle(const AViewParams: TcxViewParams; var AExportStyle: TcxCacheCellStyle;
      const AAlignment: TAlignment = taLeftJustify; ABorders: TcxBorders = []; ABorderColor: TColor = clDefault;
      AFontSize: Integer = 0);

    property Cells: TcxCustomControlCells read GetCells;
    property DefaultRowHeight: Integer read FDefaultRowHeight write FDefaultRowHeight;
    property DefaultStyle: TcxCacheCellStyle read FDefaultStyle write FDefaultStyle;
    property DefaultStyleIndex: Integer read FDefaultStyleIndex write FDefaultStyleIndex;
  public
    constructor Create(ATreeList: TcxCustomTreeList; AExportType: Integer;
      const AFileName: string; AHandler: TObject = nil); virtual;
    destructor Destroy; override;
    procedure DoExport;
    property Columns: TcxExportScale read FColumns;
    property Expand: Boolean read FExpand write FExpand;
    property IncludeParentNodes: Boolean read GetIncludeParentNodes;
    property IsNativeFormat: Boolean read FIsNativeFormat write FIsNativeFormat;
    property Provider: IcxExportProvider2 read FProvider;
    property Rows: TcxExportScale read FRows;
    property SaveAll: Boolean read FSaveAll write FSaveAll;
    property TreeList: TcxTreeListAccess read FTreeList;
    property VisualItemCount: Integer read GetVisualItemCount;
    property VisualItems[Index: Integer]: TcxExportDataItem read GetVisualItem;
  end;

const
  cxDefaultRowHeight   = 19;
  cxInvalidIndex       = -1;
  cxIndentFontName     = 'Tahoma';
  vsBackground         = 0;

  AlignToCxAlign: array[TAlignment] of TcxAlignText = (catLeft, catRight, catCenter);
  BorderWidths: array[Boolean] of Integer = (0, 1);

{ TcxTreeListExportViewInfo }

constructor TcxTreeListExportViewInfo.CreateEx(
  AHelper: TcxTreeListExportHelper);
begin
  FHelper := AHelper;
  FNodes := FHelper.FNodes;
  inherited Create(FHelper.TreeList);
end;

procedure TcxTreeListExportViewInfo.AfterCalculate;
begin
end;

procedure TcxTreeListExportViewInfo.BeforeCalculate;
var
  R: TRect;
  I: Integer;
begin
  inherited BeforeCalculate;
  R := cxRect(0, 0, 0, 200);
  for I := 0 to Bands.BottomItemCount - 1 do
    Inc(R.Right, Bands.BottomItems[I].DisplayWidth);
  if Bands.VisibleLeftFixedCount > 0 then
    Inc(R.Right, OptionsView.FixedSeparatorWidth);
  if Bands.VisibleRightFixedCount > 0 then
    Inc(R.Right, OptionsView.FixedSeparatorWidth);
  Bounds := R;
  GridLines := [];
end;

procedure TcxTreeListExportViewInfo.CalculateBackgroundParts;
begin
end;

procedure TcxTreeListExportViewInfo.CalculateNodesOrigin;
begin
end;

procedure TcxTreeListExportViewInfo.CalculateNodesViewData;
var
  AOffset, I, J, ASelectionIndex: Integer;
  ANode: TcxTreeListNode;
  ACells: TcxCustomControlCells;
  ACell: TcxCustomViewInfoItemAccess;
  AViewData: TcxTreeListNodeViewData;
begin
  ContentHeight := MaxInt;
  ACells := Cells;
  AOffset := HeadersHeight + BandsHeight;
  FHelper.PrepareExportCells;
  for I := 0 to Nodes.Count - 1 do
  try
    ANode := TcxTreeListNode(Nodes[I]);
    ASelectionIndex := TreeList.SelectionList.Remove(ANode);
    AViewData := AddNodeViewData(ANode, False);
    for J := 0 to Cells.Count - 1 do
    begin
      ACell := TcxCustomViewInfoItemAccess(Cells[J]);
      ACell.CheckVisibleInfo;
      OffsetRect(ACell.DisplayRect, 0, AOffset);
      OffsetRect(ACell.VisibleBounds, 0, AOffset);
    end;
    Inc(AOffset, AViewData.Height);
    if ASelectionIndex <> -1 then
      TreeList.SelectionList.Insert(ASelectionIndex, ANode);
    FHelper.PrepareExportCells;
  finally
    Cells.Clear;
    Cells := ACells;
  end;
  ContentHeight := AOffset;
  Bounds := cxRectSetHeight(Bounds, AOffset + FooterHeight);
  ContentHeight := HeadersHeight + BandsHeight;
end;

function TcxTreeListExportViewInfo.GetContentBounds: TRect;
begin
  Result := inherited GetContentBounds;
end;

function TcxTreeListExportViewInfo.GetIsIndicatorVisible: Boolean;
begin
  Result := False;
end;

function TcxTreeListExportViewInfo.GetIsPrinting: Boolean;
begin
  Result := True;
end;

procedure TcxTreeListExportViewInfo.UpdateScrollBars;
begin
end;

{ TcxExportVisualItem }

{ TcxTreeListExportHelper }

constructor TcxTreeListExportHelper.Create(ATreeList: TcxCustomTreeList;
  AExportType: Integer; const AFileName: string; AHandler: TObject = nil);

  function DefaultStyle: TcxCacheCellStyle;
  begin
    ViewParamsToExportStyle(TreeList.Styles.GetBackgroundParams, Result);
  end;

begin
  FHandler := AHandler;
  FTreeList := TcxTreeListAccess(ATreeList);
  FProgressHelper := TcxProgressCalculationHelper.Create(3, ATreeList, ProgressHandler);
  TcxExport.Provider(AExportType, AFileName).GetInterface(IcxExportProvider, FProvider);
  FProvider.SetDefaultStyle(DefaultStyle);
  FVisualBackgroundItems := TdxFastObjectList.Create;
  FVisualItemsList := TdxFastObjectList.Create;
  FColumns := TcxExportScale.Create;
  FRows := TcxExportScale.Create;
  FDefaultRowHeight := cxDefaultRowHeight;
  FVisualInfo := TcxItemVisualInfo.Create;
end;

destructor TcxTreeListExportHelper.Destroy;
begin
  try
    FreeAndNil(FVisualInfo);
    FProvider := nil;
    FreeAndNil(FProgressHelper);
    FreeAndNil(FVisualItemsList);
    FreeAndNil(FColumns);
    FreeAndNil(FRows);
  finally
    inherited Destroy;
  end;
end;

procedure TcxTreeListExportHelper.DoExport;
begin
  CreateExportCache;
  Provider.Commit(FProgressHelper, FHandler);
end;

function TcxTreeListExportHelper.AddVisualItem(AItemClass: TcxExportDataItemClass;
  const ABounds: TRect; AIsBackground: Boolean): TcxExportCustomItem;
begin
  Result := AItemClass.Create();
  Result.Style := -1;
  Result.Bounds := ABounds;
  Columns.AddPairs(ABounds.Left, ABounds.Right);
  Rows.AddPairs(ABounds.Top, ABounds.Bottom);
  if AIsBackground then
    FVisualBackgroundItems.Add(Result)
  else
    if FVisualItemsList.Count > 54 then
    FVisualItemsList.Add(Result)
    else
    FVisualItemsList.Add(Result);
end;

function TcxTreeListExportHelper.AddVisualItemEx(const AItemBounds: TRect;
  AVisualInfo: TcxItemVisualInfo; AIsBackground: Boolean = False): TcxExportCustomItem;
var
  ASide: TcxBorder;
  AStyle: TcxCacheCellStyle;
begin
  ViewParamsToExportStyle(AVisualInfo.ViewParams, AStyle, AVisualInfo.Alignment,
    [], clDefault, AVisualInfo.FontSize);
  if (AVisualInfo.BorderColor <> clDefault) and not AIsBackground then
  begin
    for ASide := bLeft to bBottom do
      if ASide in AVisualInfo.Borders then
      begin
        AStyle.Borders[Integer(ASide)].IsDefault := False;
        AStyle.Borders[Integer(ASide)].Color := ColorToRgb(AVisualInfo.BorderColor);
        AStyle.Borders[Integer(ASide)].Width := 1;
      end;
  end;
  Result := AddVisualItemEx(AItemBounds, Provider.RegisterStyle(AStyle), AIsBackground);
  Result.Borders := AVisualInfo.Borders;
end;

function TcxTreeListExportHelper.AddVisualItemEx(const AItemBounds: TRect;
  AStyle: Integer; AIsBackground: Boolean = False): TcxExportCustomItem;
const
  AClass: array[Boolean] of TcxExportDataItemClass = (TcxExportDataItem, TcxExportCustomItem);
begin
  Result := AddVisualItem(AClass[AIsBackground], AItemBounds, AIsBackground);
  Result.Style := AStyle;
end;

function TcxTreeListExportHelper.CanHasBorders(ACell: TcxCustomViewInfoItem): Boolean;
begin
  Result := (ACell is TcxTreeListBandCellViewInfo) or (ACell is TcxTreeListCustomHeaderCellViewInfo) or
   (ACell is TcxTreeListEditCellViewInfo) or (ACell is TcxTreeListFooterCellViewInfo);
end;

procedure TcxTreeListExportHelper.CreateExportCache;
begin
  ExtractNodesForExport;
  RegisterStyles;
  CreateExportCells;
end;

procedure TcxTreeListExportHelper.CreateExportCells;
begin
  TreeList.BeginUpdate;
  try
    FViewInfo := TcxTreeListExportViewInfo.CreateEx(Self);
    try
      FProgressHelper.BeginStage(FNodes.Count + 2);
      try
        FVisualItemsList.Capacity := FNodes.Count * (TreeList.VisibleColumnCount + 1) * 2;
        FViewInfo.Calculate;
        PrepareExportCells;
      finally
        FProgressHelper.EndStage;
      end;
    finally
      FreeAndNil(FViewInfo);
    end;
    ProcessExportCells;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListExportHelper.ExportCells;
var
  I: Integer;
  R: TRect;
  AItem: TcxExportCustomItem;
begin
  for I := 0 to Columns.VisibleCount - 1 do
    Provider.SetColumnWidth(I, Columns.Delta[I]);
  //
  for I := 0 to Rows.VisibleCount - 1 do
  begin
    if Rows.Delta[I] <> DefaultRowHeight then
      Provider.SetRowHeight(I, Rows.Delta[I]);
  end;
  //
  FillArea(Rect(0, 0, Columns.Count - 1, Rows.Count - 1),  DefaultStyleIndex);
  R := Rect(0, 0, Columns.Count - 1, Rows.Count - 1);

  FProgressHelper.BeginStage(VisualItemCount);
  try
    for I := 0 to FVisualBackgroundItems.Count - 1 do
    begin
      AItem := FVisualBackgroundItems.List[I];
      FillRealArea(AItem.Bounds, AItem.Style, clBtnShadow, AItem.Borders);
    end;
    FreeAndNil(FVisualBackgroundItems);
    for I := 0 to VisualItemCount - 1 do
    begin
      SetRealCellStyleAndValue(R, VisualItems[I]);
      FreeAndNil(FVisualItemsList.List[I]);
      FProgressHelper.NextTask;
    end;
  finally
    FProgressHelper.EndStage;
  end;
end;

procedure TcxTreeListExportHelper.ExtractNodesForExport;
var
  ACount, I: Integer;
begin
  if SaveAll then
  begin
    repeat
      ACount := TreeList.AbsoluteVisibleCount;
      TreeList.BeginUpdate;
      try
        FNodes := TreeList.AbsoluteVisibleItemsList;
        for I := FNodes.Count - 1 downto 0 do
          if Expand and TcxTreeListNode(FNodes[I]).HasChildren then
            TcxTreeListNode(FNodes[I]).Expanded := True;
      finally
        TreeList.EndUpdate;
      end;
    until ACount = TreeList.AbsoluteVisibleCount;
  end
  else
    FNodes := TreeList.SelectionList;
end;

function TcxTreeListExportHelper.GetCellVisualInfo(ACell: TcxCustomViewInfoItem): TcxItemVisualInfo;
const
  ABorders: array[TcxTreeListGridLines] of TcxBorders = ([], [bTop, bBottom], [bLeft, bRight], cxBordersAll);
begin
  Result := FVisualInfo;
  Result.FontSize := 0;
  Result.ViewParams := ACell.ViewParams;
  Result.Borders := [];
  Result.BorderColor := clDefault;
  if ACell is TcxTreeListFooterCellViewInfo then
    Result.Alignment  := TcxTreeListFooterCellViewInfo(ACell).AlignHorz
  else
    if ACell is TcxTreeListIndentCellViewInfo then
    begin
      Result.Alignment := taCenter;
      Result.FontSize := 12;
    end
    else
      if ACell is TcxTreeListHeaderCellViewInfo then
        Result.Alignment := TcxTreeListHeaderCellViewInfo(ACell).AlignHorz
      else
        if ACell is TcxTreeListEditCellViewInfo then
          Result.Alignment := TcxTreeListEditCellViewInfoAccess(ACell).Column.PropertiesValue.Alignment.Horz
        else
          Result.Alignment := taLeftJustify;

  if CanHasBorders(ACell) then
  begin
    if HasAllBorders(ACell) then
      Result.Borders := cxBordersAll
    else
      if (ACell is TcxTreeListBandCellViewInfo) and (TcxTreeListBandCellViewInfo(ACell).Part in [tlbpGroupFooter, tlbpFooter]) then
      begin
        Result.Borders := cxBordersAll;
        if TcxTreeListBandCellViewInfo(ACell).Band.VisibleIndex > 0 then
          Exclude(Result.Borders, bLeft);
        if TcxTreeListBandCellViewInfo(ACell).Band.VisibleIndex < (TreeList.Bands.BottomItemCount - 1) then
          Exclude(Result.Borders, bRight);
      end
      else
        Result.Borders := ABorders[TreeList.OptionsView.GridLines];

    if Result.Borders <> [] then
      Result.BorderColor := FViewInfo.GridLineColor;
  end;
end;

function TcxTreeListExportHelper.GetDisplayFormat(AProperties: TcxCustomEditProperties): string;
begin
  if AProperties is TcxCustomTextEditProperties then
    Result := TcxCustomTextEditPropertiesAccess(AProperties).DisplayFormat
  else
    Result := '';

  if (Result = '') and (AProperties.Owner is TcxDBTreeListColumn) then
    Result := cxGetFieldDisplayFormat(TcxDBTreeListColumn(AProperties.Owner).DataBinding.Field);
end;

function TcxTreeListExportHelper.GetDisplayFormatType(AProperties: TcxCustomEditProperties): TcxValueDisplayFormatType;
begin
  if AProperties.Owner is TcxDBTreeListColumn then
    Result := cxGetDisplayFormatType(AProperties, TcxDBTreeListColumn(AProperties.Owner).DataBinding.Field)
  else
    Result := cxGetDisplayFormatType(AProperties)
end;

function TcxTreeListExportHelper.HasAllBorders(ACell: TcxCustomViewInfoItem): Boolean;
begin
  Result := (ACell is TcxTreeListFooterCellViewInfo) or (ACell is TcxTreeListCustomHeaderCellViewInfo);
end;

function TcxTreeListExportHelper.IsNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := IsNativeFormat and
   ((AProperties is TcxDateEditProperties) or
    (AProperties is TcxMaskEditProperties) or
    (AProperties is TcxCurrencyEditProperties) or
    (AProperties is TcxSpinEditProperties) or
    (AProperties is TcxCalcEditProperties) or
    (AProperties is TcxTimeEditProperties));
  Result := Result or
   (Provider.SupportGraphic and IsGraphicProperties(AProperties)) or
   (IsRTFSupported(AProperties) and Provider.SupportRTF);
end;

procedure TcxTreeListExportHelper.FillArea(const ABounds: TRect; AStyleIndex: Integer;
  ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
var
  AStyle: TcxCacheCellStyle;
  I, J, AActualStyleIndex: Integer;

  procedure SetBorderStyle(ASide: TcxBorder);
  begin
    if not (ASide in ABorders) then Exit;
    AStyle.Borders[Integer(ASide)].IsDefault := False;
    AStyle.Borders[Integer(ASide)].Width := 1;
    AStyle.Borders[Integer(ASide)].Color := ColorToRgb(ABorderColor);
  end;

begin
  for I := ABounds.Top to ABounds.Bottom - 1 do
    for J := ABounds.Left to ABounds.Right - 1 do
    begin
      AActualStyleIndex := AStyleIndex;
      if (ABorderColor <> clDefault) and (ABorders <> []) then
      begin
        AStyle := Provider.GetStyle(AStyleIndex);
        if J = ABounds.Left then
          SetBorderStyle(bLeft);
        if I = ABounds.Top then
          SetBorderStyle(bTop);
        if J = ABounds.Right - 1 then
          SetBorderStyle(bRight);
        if I = ABounds.Bottom - 1 then
          SetBorderStyle(bBottom);
        AActualStyleIndex := Provider.RegisterStyle(AStyle);
      end;
      Provider.SetCellStyle(J, I, AActualStyleIndex);
    end;
end;

procedure TcxTreeListExportHelper.FillRealArea(const ABounds: TRect; AStyleIndex: Integer;
  ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
var
  ALogicalBounds: TRect;
begin
  RealBoundsToLogicalBounds(ABounds, ALogicalBounds);
  FillArea(ALogicalBounds, AStyleIndex, ABorderColor, ABorders);
end;

function TcxTreeListExportHelper.IsBackgroundCell(
  ACell: TcxCustomViewInfoItem): Boolean;
begin
  Result := ACell is TcxTreeListBandCellViewInfo;
end;

function TcxTreeListExportHelper.IsGraphicProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := (AProperties is TcxCustomImageProperties) or (AProperties is TcxCustomImageComboBoxProperties);
end;

function TcxTreeListExportHelper.IsEmpty: Boolean;
begin
  Result := (Columns.VisibleCount = 0) or (Rows.VisibleCount = 0);
end;

procedure TcxTreeListExportHelper.DrawIndentImage(AItem: TcxExportDataItem; ACell: TcxTreeListIndentCellViewInfo);
var
  ABitmap: TcxBitmap;
begin
  if ACell.Images <> nil then
  begin
    ABitmap := TcxBitmap.CreateSize(ACell.Images.Width, ACell.Images.Height, pf24bit);
    try
      ABitmap.cxCanvas.FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height), ACell.ViewParams);
      ACell.Images.Draw(ABitmap.Canvas, 0, 0, ACell.ImageIndex);
      ACell.Images.Draw(ABitmap.Canvas, 0, 0, ACell.OverlayIndex);
      Provider.AddGraphic(AItem, ABitmap, ifmNormal);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TcxTreeListExportHelper.PrepareExportCells;
const
  AExpandChar: array[Boolean] of Char = ('+', '-');
var
  I: Integer;
  AFormat: string;
  ACell: TcxCustomViewInfoItem;
  AProperties: TcxCustomEditProperties;
  AEditCell: TcxTreeListEditCellViewInfoAccess;
  AExportVisualItem: TcxExportDataItem;
begin                                                                                                                            ;
  for I := 0 to Cells.Count - 1 do
  begin
    ACell := Cells[I];
    ACell.CheckVisibleInfo;
    if SkipExportCell(ACell) then
      Continue;

    AExportVisualItem := TcxExportDataItem(AddVisualItemEx(ACell.BoundsRect, GetCellVisualInfo(ACell), IsBackgroundCell(ACell)));
    if ACell is TcxTreeListEditCellViewInfo then
    begin
      AEditCell := TcxTreeListEditCellViewInfoAccess(ACell);

      if IsNativeFormatProperties(AEditCell.Properties) then
        Provider.AddValue(AExportVisualItem, AEditCell.Node.Values[AEditCell.Column.ItemIndex], AEditCell.Properties, GetDisplayFormat(AEditCell.Properties))
      else
        Provider.AddText(AExportVisualItem, AEditCell.Node.Texts[AEditCell.Column.ItemIndex])
    end
    else

    if ACell is TcxTreeListIndentCellViewInfo then
    begin
      if TcxTreeListIndentCellViewInfo(ACell).Button then
        Provider.AddText(AExportVisualItem, AExpandChar[TcxTreeListIndentCellViewInfo(ACell).Node.Expanded])
      else
        if TcxTreeListIndentCellViewInfo(ACell).HasImage and Provider.SupportGraphic then
        begin
          if TcxTreeListIndentCellViewInfo(ACell).OverlayIndex <> -1 then
            DrawIndentImage(AExportVisualItem, TcxTreeListIndentCellViewInfo(ACell))
          else
            Provider.AddImageListItem(AExportVisualItem, TcxTreeListIndentCellViewInfo(ACell).Images,
              TcxTreeListIndentCellViewInfo(ACell).ImageIndex, ACell.ViewParams.Color);
        end;
    end
    else

    if ACell is TcxTreeListHeaderCellViewInfo then
      Provider.AddText(AExportVisualItem, TcxTreeListHeaderCellViewInfo(ACell).Text)
    else

    if ACell is TcxTreeListFooterCellViewInfo then
    begin
      AProperties := TcxTreeListFooterCellViewInfo(ACell).Column.PropertiesValue;
      if IsNativeFormatProperties(AProperties) and not IsGraphicProperties(AProperties) then
      begin
        AFormat := TcxTreeListFooterCellViewInfo(ACell).DisplayFormat;
        if AFormat = '' then
          AFormat := GetDisplayFormat(AProperties);
        Provider.AddValue(AExportVisualItem, TcxTreeListFooterCellViewInfo(ACell).Value, AProperties, AFormat);
      end
      else
        Provider.AddText(AExportVisualItem, TcxTreeListFooterCellViewInfo(ACell).Text);
    end;
  end;

  FProgressHelper.NextTask;
end;

procedure TcxTreeListExportHelper.ProcessExportCells;
begin
  if IsEmpty then
    FProgressHelper.SkipStage
  else
  begin
    Columns.Arrange;
    Rows.Arrange;
    ExportCells;
  end;
end;

procedure TcxTreeListExportHelper.RealBoundsToLogicalBounds(
  const ABounds: TRect; out ALogicalBounds: TRect);
begin
  with ALogicalBounds do
  begin
    Columns.GetPosition(ABounds.Left, ABounds.Right, Left, Right);
    Rows.GetPosition(ABounds.Top, ABounds.Bottom, Top, Bottom);
  end;
end;

procedure TcxTreeListExportHelper.RealBoundsToLogicalBoundsEx(
  const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
begin
  with ALogicalBounds do
  begin
    Columns.GetPositionEx(ABounds.Left, ABounds.Right,
      ASearchArea.Left, ASearchArea.Right, Left, Right);
    Rows.GetPositionEx(ABounds.Top, ABounds.Bottom,
      ASearchArea.Top, ASearchArea.Bottom, Top, Bottom);
  end;
end;

procedure TcxTreeListExportHelper.RegisterStyles;
var
  AViewParams: TcxViewParams;
begin
  TreeList.Styles.GetViewParams(vsBackground, nil, TreeList.Styles.BandBackground, AViewParams);
  ViewParamsToExportStyle(AViewParams, FDefaultStyle);
  Provider.SetDefaultStyle(DefaultStyle);
  DefaultStyleIndex := Provider.RegisterStyle(DefaultStyle);
end;

procedure TcxTreeListExportHelper.SetRealCellStyleAndValue(const ASearchArea: TRect; AInfo: TcxExportCustomItem);
begin
  if cxRectIsEmpty(AInfo.Bounds) then
    Exit;
  RealBoundsToLogicalBoundsEx(AInfo.Bounds, ASearchArea, AInfo.Bounds);
  Provider.SetData(TcxExportDataItem(AInfo));
end;

procedure TcxTreeListExportHelper.SetRealCellStyleAndValueEx(const ARealBounds, ASearchArea: TRect;
  AStyleIndex: Integer; AGraphic: TGraphic; const AValue: Variant; const AValueDisplayFormat: string = '';
  const AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto; AFitMode: TcxImageFitMode = ifmStretch);
var
  R: TRect;
begin
  RealBoundsToLogicalBoundsEx(ARealBounds, ASearchArea, R);
  if Provider.SupportGraphic and (AGraphic <> nil) and not AGraphic.Empty then
    Provider.SetCellGraphic(R, AStyleIndex, AGraphic, AFitMode)
  else
  begin
    Provider.SetCellValue(R.Left, R.Top, AValue, AValueDisplayFormat, AValueDisplayFormatType);
    Provider.SetCellStyle(R, AStyleIndex);
  end;
end;

function TcxTreeListExportHelper.SkipExportCell(ACell: TcxCustomViewInfoItem): Boolean;
begin
  Result := not ACell.Visible or not (
    (ACell is TcxTreeListCustomCellViewInfo) or (ACell is TcxTreeListEditCellViewInfo)) or
    ((ACell is TcxTreeListFooterCellViewInfo) and TcxTreeListFooterCellViewInfo(ACell).Hidden);
end;

procedure TcxTreeListExportHelper.ViewParamsToExportStyle(const AViewParams: TcxViewParams;
 var AExportStyle: TcxCacheCellStyle; const AAlignment: TAlignment = taLeftJustify;
 ABorders: TcxBorders = []; ABorderColor: TColor = clDefault; AFontSize: Integer = 0);
var
  I: Integer;
begin
  AExportStyle := DefaultCellStyle;
  with AExportStyle do
  begin
    if ABorders = [] then
      ABorderColor := clDefault;
    BrushBkColor := ColorToRGB(AViewParams.Color);
    FontColor := ColorToRGB(AViewParams.TextColor);
    FontName := AViewParams.Font.Name;
    FontStyle := AViewParams.Font.Style;
    if AFontSize = 0 then
      FontSize := AViewParams.Font.Size
    else
      FontSize := AFontSize;
    FontCharset := Integer(AViewParams.Font.Charset);
    AlignText := AlignToCxAlign[AAlignment];
    for I := 0 to 3 do
    begin
      Borders[I].IsDefault := (ABorderColor = clDefault) or not (TcxBorder(I) in ABorders);
      Borders[I].Width := Byte(not Borders[I].IsDefault);
      if not Borders[I].IsDefault then
        Borders[I].Color := ColorToRgb(ABorderColor);
    end;
  end;
end;

function TcxTreeListExportHelper.GetCells: TcxCustomControlCells;
begin
  Result := FViewInfo.Cells;
end;

function TcxTreeListExportHelper.GetIncludeParentNodes: Boolean;
begin
  Result := True;
end;

function TcxTreeListExportHelper.GetVisualItem(AIndex: Integer): TcxExportDataItem;
begin
  Result := FVisualItemsList[AIndex] as TcxExportDataItem;
end;

function TcxTreeListExportHelper.GetVisualItemCount: Integer;
begin
  Result := FVisualItemsList.Count;
end;

procedure TcxTreeListExportHelper.ProgressHandler(Sender: TObject; Percents: Integer);
var
  AIntf: IcxExportProgress;
begin
  if Supports(FHandler, IcxExportProgress, AIntf) then
    AIntf.OnProgress(Sender, Percents);
end;

procedure cxExportTLToHTML(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
begin
  cxExportTLToFile(AFileName, ATreeList, cxExportToHtml, AExpand, ASaveAll, False, [], AFileExt, AHandler);
end;

procedure cxExportTLToXML(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);
begin
  cxExportTLToFile(AFileName, ATreeList, cxExportToXML, AExpand, ASaveAll, False, [], AFileExt, AHandler);
end;

procedure cxExportTLToExcel(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
begin
  cxExportTLToFile(AFileName, ATreeList, cxExportToExcel, AExpand, ASaveAll, AUseNativeFormat, [], AFileExt, AHandler);
end;

procedure cxExportTLToCSV(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportTLToFile(AFileName, ATreeList, cxExportToCSV, AExpand,
    ASaveAll, False, [ASeparator], AFileExt, AHandler, AEncoding);
end;

procedure cxExportTLToText(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean;
  ASaveAll: Boolean = True; const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportTLToText(AFileName, ATreeList, AExpand, ASaveAll, '', '', '', AFileExt, AHandler, AEncoding);
end;

procedure cxExportTLToText(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: string = ''; const ABeginString: string = ''; const AEndString: string = '';
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
begin
  cxExportTLToFile(AFileName, ATreeList, cxExportToText, AExpand, ASaveAll, False,
    [ASeparator, ABeginString, AEndString], AFileExt, AHandler, AEncoding);
end;

function Supports(const Instance: IUnknown; const IID: TGUID; out Intf): Boolean; overload;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

procedure cxExportTLToFile(AFileName: string; ATreeList: TcxCustomTreeList; AExportType: Integer;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparators: array of string; const AFileExt: string;
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
var
  AEncodingIntf: IcxExportProviderEncoding;
  AIntf: IcxExportWithSeparators;
  I: Integer;
begin
  if AFileExt <> '' then
    AFileName := ChangeFileExt(AFileName, '.' + AFileExt);
  if not ATreeList.Visible then
    cxTreeListError(scxExportNotVisibleControl);
  with TcxTreeListExportHelper.Create(ATreeList, AExportType, AFileName, AHandler) do
  try
    Expand := AExpand;
    IsNativeFormat := AUseNativeFormat;
    SaveAll := ASaveAll;
    if cxTLExportLink.Supports(Provider, IcxExportProviderEncoding, AEncodingIntf) then
      AEncodingIntf.SetEncoding(AEncoding);
    if cxTLExportLink.Supports(Provider, IcxExportWithSeparators, AIntf) and (Length(ASeparators) > 0) then
    begin
      for I := Low(ASeparators) to High(ASeparators) do
        AIntf.AddSeparator(ASeparators[I]);
    end;
    DoExport;
  finally
    Free;
  end;
end;

procedure cxExportTLToXLSX(const AFileName: string; ATreeList: TcxCustomTreeList; AExpand: Boolean = True;
  ASaveAll: Boolean = True; AUseNativeFormat: Boolean = True; const AFileExt: string = 'xlsx';
  AHandler: TObject = nil);
begin
  cxExportTLToFile(AFileName, ATreeList, cxExportToXlsx, AExpand, ASaveAll, AUseNativeFormat, [], AFileExt, AHandler);
end;


end.


// 12:00
