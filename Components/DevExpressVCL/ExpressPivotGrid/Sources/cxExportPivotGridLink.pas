{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxExportPivotGridLink;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Graphics, SysUtils, Variants, Types, DB, Math, cxCustomPivotGrid;


procedure cxExportPivotGridToHTML(const AFileName: string; APivotGrid: TcxCustomPivotGrid;
  AExpand: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
procedure cxExportPivotGridToXML(const AFileName: string; APivotGrid: TcxCustomPivotGrid;
  AExpand: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);

procedure cxExportPivotGridToExcel(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
procedure cxExportPivotGridToXLSX(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);

procedure cxExportPivotGridToCSV(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  const ASeparator: Char = ','; const AFileExt: string = 'csv'; AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure cxExportPivotGridToText(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
procedure cxExportPivotGridToText(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  const ASeparator: string = ''; const ABeginString: string = ''; const AEndString: string = '';
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;

procedure cxExportPivotGridToFile(AFileName: string; APivotGrid: TcxCustomPivotGrid; AExportType: Integer;
  AExpand, AUseNativeFormat: Boolean; const ASeparators: array of string; const AFileExt: string;
  AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure cxExportPivotGridDataToExcel(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AHandler: TObject = nil;
  AAutoBestFit: Boolean = True; AHighlightTotals: Boolean = True; ACellBorders: Boolean = True; AFieldHeaders: Boolean = False);

implementation
uses
  cxPivotGridStrs, dxCore, dxCoreClasses, cxClasses, dxSpreadSheetCore, dxHashUtils, dxSpreadSheetClasses, cxFormats,
  dxSpreadSheetGraphics, dxSpreadSheetTypes, cxExportProviders, cxGeometry, cxEdit, cxCalendar, cxCurrencyEdit,
  cxSpinEdit, cxCalc, cxTimeEdit, cxMaskEdit, cxGraphics, cxStyles, cxDataUtils, cxDataStorage, cxExport, cxTextEdit,
  cxVariants, dxSpreadSheetNumberFormat, dxSpreadSheetCoreStyles, dxSpreadSheetStyles;

type
  TcxPivotGridExportHelper = class;
  TcxCustomPivotGridAccess = class(TcxCustomPivotGrid);
  TcxPivotGridFieldAccess = class(TcxPivotGridField);
  TcxPivotGridRowAccess = class(TcxPivotGridRowItem);
  TcxPivotGridHeaderCellAccess = class(TcxPivotGridHeaderCellViewInfo);
  TcxPropertiesAccess = class(TcxCustomTextEditProperties);

  { TcxPivotGridExportCell }

  TcxPivotGridExportCell = class(TcxDoublyLinkedObject)
  private
    FBounds: TRect;
    FDisplayFormat: string;
    FDisplayFormatType: TcxValueDisplayFormatType;
    FValue: Variant;
    FStyleIndex: Integer;
    FNativeFormat: Boolean;
  public
    procedure Initialize(const ABounds: TRect; AStyleIndex: Integer);

    property Bounds: TRect read FBounds;
    property DisplayFormat: string read FDisplayFormat;
    property DisplayFormatType: TcxValueDisplayFormatType read FDisplayFormatType;
    property NativeFormat: Boolean read FNativeFormat;
    property StyleIndex: Integer read FStyleIndex;
    property Value: Variant read FValue;
  end;

  { TcxPivotGridExportCellList }

  TcxPivotGridExportCellList = class(TcxDoublyLinkedObjectList)
  protected
    function GetLinkedObjectClass: TcxDoublyLinkedObjectClass; override;
  end;

  { TcxPivotGridMapInfo }

  TcxPivotGridMapsInfo = class
  private
    FCells: TcxPivotGridExportCellList;
    FController: TcxPivotGridExportController;
    FExportArea: TdxExportArea;
    FGridLineColor: TColor;
    FGridLines: TcxPivotGridLines;
    FOwner: TcxPivotGridExportHelper;
    FViewInfo: TcxPivotGridViewInfo;

    function CheckNativeValue(AProperties: TcxCustomEditProperties; APivotField: TcxPivotGridField; const AValue: Variant): Variant;
    function GetProvider: IcxExportProvider;
    function IsCurrencyField(APivotField: TcxPivotGridField): Boolean;
    function IsCurrencyProperties(AProperties: TcxCustomEditProperties): Boolean;
    function IsNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
  protected
    procedure AddPivotGridCell(ACell: TcxPivotGridCustomCellViewInfo);
    procedure AddPivotGridCells(ACells: TcxPivotGridCells);
    procedure ConvertCellToCacheCellStyle(ACell: TcxPivotGridCustomCellViewInfo; var AStyle: TcxCacheCellStyle);
    procedure ConvertDataCell(ACell: TcxPivotGridDataCellViewInfo; var AStyle: TcxCacheCellStyle);
    procedure ConvertHeaderCell(ACell: TcxPivotGridHeaderCellViewInfo; var AStyle: TcxCacheCellStyle);
    procedure InitializeExportCell(AExportCell: TcxPivotGridExportCell; ASource: TcxPivotGridCustomCellViewInfo);

    procedure ProcessColsAndRows;
    procedure ProcessDataCells;

    procedure WriteCell(AExportCell: TcxPivotGridExportCell);
    procedure WriteCells;
    procedure WriteColumnWidths;
    procedure WriteRowHeights;

    property Controller: TcxPivotGridExportController read FController;
    property ExportArea: TdxExportArea read FExportArea;
    property GridLineColor: TColor read FGridLineColor;
    property GridLines: TcxPivotGridLines read FGridLines;
    property Owner: TcxPivotGridExportHelper read FOwner;
    property Provider: IcxExportProvider read GetProvider;
    property ViewInfo: TcxPivotGridViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TcxPivotGridExportHelper;
      AController: TcxPivotGridExportController); virtual;
    destructor Destroy; override;
  end;

  TcxPivotGridMapsInfoClass = class of TcxPivotGridMapsInfo;

  { TcxPivotGridExportViewInfo }

  TcxPivotGridExportViewInfo = class(TcxPivotGridViewInfo)
  private
    FMapsInfo: TcxPivotGridMapsInfo;
    function GetHelper: TcxPivotGridExportHelper;
  protected
    procedure AfterRowCellsCalculated(ARow: TcxPivotGridViewDataItem); override;
    procedure CalculateCells; override;
  public
    constructor Create(AMapsInfo: TcxPivotGridMapsInfo); reintroduce; virtual;

    property Helper: TcxPivotGridExportHelper read GetHelper;
    property MapsInfo: TcxPivotGridMapsInfo read FMapsInfo;
  end;

  { TcxPivotGridExportHelper }

  TcxPivotGridExportHelper = class
  private
    FExpand: Boolean;
    FHandler: TObject;
    FIsNativeFormat: Boolean;
    FPivotGrid: TcxCustomPivotGrid;
    FProgressHelper: TcxProgressCalculationHelper;
    FProvider: IcxExportProvider;

    procedure ProgressHandler(Sender: TObject; Percents: Integer);
  protected
    MapsInfo: TcxPivotGridMapsInfo;

    function GetMapsInfoClass: TcxPivotGridMapsInfoClass; virtual;
  public
    constructor Create(APivotGrid: TcxCustomPivotGrid; AExportType: Integer;
      const AFileName: string; AHandler: TObject); virtual;
    destructor Destroy; override;
    procedure ExportPivotGrid;

    property Expand: Boolean read FExpand write FExpand;
    property IsNativeFormat: Boolean read FIsNativeFormat;
    property PivotGrid: TcxCustomPivotGrid read FPivotGrid;
    property ProgressHelper: TcxProgressCalculationHelper read FProgressHelper;
    property Provider: IcxExportProvider read FProvider;
  end;

  { TdxPivotGridExportGroupItem }

  TdxPivotGridExportGroupItem = class
  strict private
    function GetIsGrandTotal: Boolean;
    function GetIsTotal: Boolean;
    function GetTotalDisplayText: string;
    function GetValue: Variant;
  protected
    DataField: TcxPivotGridField;
    Group: TcxPivotGridGroupItem;
    Level: Integer;
    Next: TdxPivotGridExportGroupItem;
    Size: Integer;
    TotalIndex: Integer;
    GroupStart, GroupFinish: Integer;
    TableItem: TdxSpreadSheetTableItem;
  public
    constructor Create(AGroup: TcxPivotGridGroupItem; ADataField: TcxPivotGridField; ALevel: Integer);
    destructor Destroy; override;
    function Add(AGroup: TcxPivotGridGroupItem; ADataField: TcxPivotGridField; ALevel: Integer): TdxPivotGridExportGroupItem;

    property IsTotal: Boolean read GetIsTotal;
    property IsGrandTotal: Boolean read GetIsGrandTotal;
    property Value: Variant read GetValue;
  end;

  { TdxPivotGridExportContentItem }

  TdxPivotGridExportContentItem = class
  public
    Field: TcxPivotGridField;
    Group: TcxPivotGridGroupItem;
    DataField: TcxPivotGridField;
    Next: TdxPivotGridExportContentItem;
    Style: TdxSpreadSheetCellStyle;
    TableItem: TdxSpreadSheetTableItem;
    TotalIndex: Integer;
    constructor Create(APattern: TdxPivotGridExportContentItem);
    destructor Destroy; override;
    function IsTotal: Boolean;
  end;

  { TcxPivotGridDataExportHelper }

  TcxPivotGridDataExportHelper = class(TdxCustomDataExport)
  strict private
    FAutoBestFit: Boolean;
    FContentItem: TdxPivotGridExportContentItem;
    FCellBorders: Boolean;
    FFieldHeaders: Boolean;
    FHighlightTotals: Boolean;
    FWordWrap: Boolean;
    function GetPivotGrid: TcxCustomPivotGrid;
  protected
    Groups: TList;
    ColumnCount: Integer;
    ColumnMaxLevel: Integer;
    Columns: TdxPivotGridExportGroupItem;
    ColumnsContent: TdxPivotGridExportContentItem;
    ContentStyles: array[Boolean] of TdxSpreadSheetCellStyleHandle;
    DataField: TcxPivotGridField;
    RowCount: Integer;
    RowMaxLevel: Integer;
    Rows: TdxPivotGridExportGroupItem;
    RowsContent: TdxPivotGridExportContentItem;
    ViewData: TcxPivotGridViewData;
    procedure AfterExport; override;
    procedure BeforeExport; override;
    procedure ExportFieldHeaders;
    procedure DoExport; override;
    function GetStageCount: Integer; override;
    //
    procedure AddContentItem(AGroup: TdxPivotGridExportGroupItem; var AContentItems: TdxPivotGridExportContentItem);
    function AddFieldHeader(ARow, AColumn: Integer; AField: TcxPivotGridField): TdxSpreadSheetCell;
    function AddGroupValue(ARow, AColumn: Integer;
      AGroup: TdxPivotGridExportGroupItem; ATableItems: TdxSpreadSheetTableItems;
      var AContentItems: TdxPivotGridExportContentItem; var AIndex: Integer): TdxSpreadSheetCell;
    procedure ApplyBestFit(AStartItem: TdxSpreadSheetTableItem);
    procedure CalculateGroupsFinish;
    function GetDataField(ARow, AColumn: TdxPivotGridExportContentItem): TcxPivotGridField;
    function GetDataFieldIndex(AArea: TcxPivotGridDataFieldArea): Integer;
    function GetColumnWidthForColumn(AColumn: TdxPivotGridExportGroupItem): Integer;
    function GetColumnWidthForRow(ARow: TdxPivotGridExportGroupItem): Integer;
    function GetSummaryType(AItem: TdxPivotGridExportContentItem;
      ADefaultType: TcxPivotGridSummaryType; var ADisplayFormat: string): TcxPivotGridSummaryType;
    function IsItemExpanded(AGroup: TdxPivotGridExportGroupItem): Boolean;
    function IsLastGroup(AGroup: TdxPivotGridExportGroupItem): Boolean;
    function IsNewGroup(AGroup: TdxPivotGridExportGroupItem): Boolean;
    function IsTotal(AGroup: TdxPivotGridExportGroupItem): Boolean;
    function NeedNewLineForNextItem(AItem: TdxPivotGridExportGroupItem): Boolean;
    procedure PrepareColumns;
    procedure PrepareRows;
    procedure SetContentCellStyle(ACell: TdxSpreadSheetCell; ADataField: TcxPivotGridField; AIsTotal: Boolean;
      const ADisplayFormat: string = '');
    procedure WriteFieldHeaders(ARow, AColumn: Integer; AFields: TcxPivotGridFields; AArea: TcxPivotGridDataFieldArea);
    procedure WriteGroupColumns;
    procedure WriteGroupRows;
    procedure WriteGroupsPost(ARoot: TdxPivotGridExportGroupItem; AItems: TdxSpreadSheetTableItems);
    procedure WriteRow(ARow: TdxPivotGridExportContentItem);
    procedure WriteRows;
  public
    constructor Create(AControl: TObject; const AFileName: string; AHandler: TObject); override;
    destructor Destroy; override;

    property AutoBestFit: Boolean read FAutoBestFit write FAutoBestFit;
    property CellBorders: Boolean read FCellBorders write FCellBorders;
    property HighlightTotals: Boolean read FHighlightTotals write FHighlightTotals;
    property FieldHeaders: Boolean read FFieldHeaders write FFieldHeaders;
    property PivotGrid: TcxCustomPivotGrid read GetPivotGrid;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  end;

const
  cxDefaultExportFontName = 'Tahoma';
  cxDefaultExportStyle: TcxCacheCellStyle = (
    AlignText: catCenter;
    BrushStyle: cbsSolid;
    FontCharset: 0;
    FontColor: 0;
    FontSize: 12;
    FontStyle: [];
  );

  AlignToCxAlign: array[TAlignment] of TcxAlignText = (catLeft, catRight, catCenter);
  AlignVertToCxAlignVert: array[TcxEditVertAlignment] of TcxAlignTextVert = (atvTop, atvBottom, atvCenter);


  cxUsedBorder: TcxCellBorders = (IsDefault: False; Width: 1);
  cxEmptyBorder: TcxCellBorders = (IsDefault: True; Width: 0);

function cxSetBorder(var ABorder: TcxCellBorders; NeedSet: Boolean; Color: Integer): Boolean;
begin
  Result := NeedSet;
  if NeedSet then
  begin
    ABorder := cxUsedBorder;
    ABorder.Color := Color;
  end
  else
    ABorder := cxEmptyBorder;
end;

procedure cxCheckBorders(var AStyle: TcxCacheCellStyle;
  const ABorders: TcxBorders; AColor: TColor; AGridLines: TcxPivotGridLines);
begin
  cxSetBorder(AStyle.Borders[0],
    (bLeft in ABorders) and (AGridLines in [pglVert, pglBoth]), AColor);
  cxSetBorder(AStyle.Borders[1],
    (bTop in ABorders) and (AGridLines in [pglHorz, pglBoth]), AColor);
  cxSetBorder(AStyle.Borders[2],
    (bRight in ABorders) and (AGridLines in [pglVert, pglBoth]), AColor);
  cxSetBorder(AStyle.Borders[3],
    (bBottom in ABorders) and (AGridLines in [pglHorz, pglBoth]), AColor);
end;

procedure cxViewParamsToCacheStyle(
  AViewParams: TcxViewParams; var ACacheStyle: TcxCacheCellStyle);
begin
  ACacheStyle := DefaultCellStyle;
  with ACacheStyle do
  begin
    FontName := AViewParams.Font.Name;
    FontStyle := AViewParams.Font.Style;
    FontColor := ColorToRgb(AViewParams.TextColor);
    FontSize := AViewParams.Font.Size;
    FontCharset := AViewParams.Font.Charset;
    BrushStyle := cbsSolid;
    BrushBkColor := ColorToRgb(AViewParams.Color);
    BrushFgColor := BrushBkColor;
  end;
end;

{ TcxPivotGridExportCell }

procedure TcxPivotGridExportCell.Initialize(const ABounds: TRect; AStyleIndex: Integer);
begin
  FStyleIndex := AStyleIndex;
  FBounds := ABounds;
end;

{ TcxPivotGridExportCellList }

function TcxPivotGridExportCellList.GetLinkedObjectClass: TcxDoublyLinkedObjectClass;
begin
  Result := TcxPivotGridExportCell;
end;

{ TcxPivotGridMapsInfo }

function CompareInt(A, B: Pointer): Integer;
begin
  Result := Integer(A) - Integer(B);
end;

constructor TcxPivotGridMapsInfo.Create(AOwner: TcxPivotGridExportHelper; AController: TcxPivotGridExportController);
begin
  FOwner := AOwner;
  FController := AController;
  with FController.OptionsView do
  begin
    //todo: FGridLineColor := GridLineColor;
    FGridLines := GridLines;
  end;
  FExportArea := TdxExportArea.Create;
  FCells := TcxPivotGridExportCellList.Create;
  FViewInfo := TcxPivotGridExportViewInfo.Create(Self);
  FController.CalculateViewInfo;
end;

destructor TcxPivotGridMapsInfo.Destroy;
begin
  FreeAndNil(FExportArea);
  FreeAndNil(FCells);
  TcxCustomPivotGridAccess(Controller.PivotGrid).RecreateViewInfo;
  inherited Destroy;
end;

procedure TcxPivotGridMapsInfo.AddPivotGridCell(ACell: TcxPivotGridCustomCellViewInfo);
var
  AStyle: TcxCacheCellStyle;
  AExportCell: TcxPivotGridExportCell;
begin
  cxViewParamsToCacheStyle(ACell.ViewParams, AStyle);
  cxCheckBorders(AStyle, cxBordersAll, GridLineColor, GridLines);
  ConvertCellToCacheCellStyle(ACell, AStyle);
  AExportCell := TcxPivotGridExportCell(FCells.Add);
  AExportCell.Initialize(ExportArea.BoundsToPosition(ACell.Bounds), Provider.RegisterStyle(AStyle));
  InitializeExportCell(AExportCell, ACell);
end;

procedure TcxPivotGridMapsInfo.AddPivotGridCells(ACells: TcxPivotGridCells);
var
  I: Integer;
begin
  ACells.BeforePaint;
  for I := 0 to ACells.Count - 1 do
    AddPivotGridCell(ACells[I]);
  Owner.ProgressHelper.NextTask;
end;

procedure TcxPivotGridMapsInfo.ProcessColsAndRows;

  procedure AddCellBounds(ACells: TcxPivotGridCells);
  var
    I: Integer;
  begin
    for I := 0 to ACells.Count - 1 do
      ExportArea.Add(ACells[I].Bounds);
    Owner.ProgressHelper.NextTask;
  end;

begin
  Owner.ProgressHelper.BeginStage(8);
  try
    AddCellBounds(ViewInfo.ColumnHeaders);
    AddCellBounds(ViewInfo.FieldHeaders);
    AddCellBounds(ViewInfo.RowHeaders);
    //
    AddPivotGridCells(ViewInfo.ColumnHeaders);
    AddPivotGridCells(ViewInfo.FieldHeaders);
    WriteCells;
    AddPivotGridCells(ViewInfo.RowHeaders);
    WriteCells;
    //
    if ExportArea.AxisX.Count * ExportArea.AxisY.Count <> 0 then
    begin
      Provider.SetRange(ExportArea.AxisX.Count - 1, ExportArea.AxisY.Count - 1, False);
      WriteColumnWidths;
      WriteRowHeights;
    end;
  finally
    Owner.ProgressHelper.EndStage;
  end;
end;

procedure TcxPivotGridMapsInfo.ProcessDataCells;
begin
  AddPivotGridCells(ViewInfo.DataCells);
  WriteCells;
  ViewInfo.DataCells.Clear;
end;

procedure TcxPivotGridMapsInfo.ConvertCellToCacheCellStyle(
  ACell: TcxPivotGridCustomCellViewInfo; var AStyle: TcxCacheCellStyle);
begin
  if ACell is TcxPivotGridDataCellViewInfo then
    ConvertDataCell(TcxPivotGridDataCellViewInfo(ACell), AStyle)
  else if ACell is TcxPivotGridHeaderCellViewInfo then
    ConvertHeaderCell(TcxPivotGridHeaderCellViewInfo(ACell), AStyle)
end;

procedure TcxPivotGridMapsInfo.ConvertDataCell(ACell: TcxPivotGridDataCellViewInfo; var AStyle: TcxCacheCellStyle);
begin
  if ACell.Properties <> nil then
    AStyle.AlignText := AlignToCxAlign[ACell.Properties.Alignment.Horz]
  else
    AStyle.AlignText := AlignToCxAlign[ACell.Align];
end;

procedure TcxPivotGridMapsInfo.ConvertHeaderCell(ACell: TcxPivotGridHeaderCellViewInfo; var AStyle: TcxCacheCellStyle);
begin
  AStyle.AlignText := AlignToCxAlign[ACell.AlignHorz];
  AStyle.AlignTextVert := TcxAlignTextVert(Byte(ACell.AlignVert) + 1);
  if ACell.Properties <> nil then
  begin
    AStyle.AlignText := AlignToCxAlign[ACell.Properties.Alignment.Horz];
    AStyle.AlignTextVert := AlignVertToCxAlignVert[ACell.Properties.Alignment.Vert];
  end;
end;

procedure TcxPivotGridMapsInfo.InitializeExportCell(AExportCell: TcxPivotGridExportCell; ASource: TcxPivotGridCustomCellViewInfo);
var
  ADisplayTextAssigned: Boolean;
  AProperties: TcxCustomEditProperties;
  ADataCell: TcxPivotGridDataCellViewInfo;
begin
  AExportCell.FNativeFormat := False;
  if ASource is TcxPivotGridDataCellViewInfo then
  begin
    ADataCell := TcxPivotGridDataCellViewInfo(ASource);
    if ADataCell.DataField <> nil then
    begin
      AExportCell.FDisplayFormat := '';
      AExportCell.FDisplayFormatType := vdftAuto;
      ADisplayTextAssigned := Assigned(ADataCell.DataField.OnGetDisplayText);
      AExportCell.FNativeFormat := Owner.IsNativeFormat and not ADisplayTextAssigned;
      if AExportCell.NativeFormat then
      begin
        AProperties := ADataCell.Properties;
        if IsNativeFormatProperties(AProperties) then
        begin
          AExportCell.FValue := CheckNativeValue(AProperties, ADataCell.DataField, ADataCell.Value);
          AExportCell.FDisplayFormat := cxGetDisplayFormat(AProperties);
          AExportCell.FDisplayFormatType := cxGetDisplayFormatType(AProperties);
        end
        else
          AExportCell.FValue := ADataCell.Value;
      end
      else
        AExportCell.FValue := ADataCell.DisplayText;
    end;
  end
  else
    if ASource is TcxPivotGridHeaderCellViewInfo then
    begin
      AProperties := TcxPivotGridHeaderCellViewInfo(ASource).Properties;
      if IsNativeFormatProperties(AProperties) then
      begin
        AExportCell.FNativeFormat := Owner.IsNativeFormat and (TcxPivotGridHeaderCellAccess(ASource).FDataField <> nil) and
           not Assigned(TcxPivotGridHeaderCellAccess(ASource).FDataField.OnGetGroupValueDisplayText);
        AExportCell.FValue := CheckNativeValue(AProperties,
          TcxPivotGridHeaderCellAccess(ASource).FDataField, TcxPivotGridHeaderCellViewInfo(ASource).Value);
        AExportCell.FDisplayFormat := cxGetDisplayFormat(AProperties);
        AExportCell.FDisplayFormatType := cxGetDisplayFormatType(AProperties);
      end
      else
        AExportCell.FValue := TcxPivotGridHeaderCellViewInfo(ASource).DisplayText;
    end;
end;

procedure TcxPivotGridMapsInfo.WriteCell(AExportCell: TcxPivotGridExportCell);
begin
  if AExportCell.NativeFormat then
    Provider.SetCellValue(AExportCell.Bounds.Left, AExportCell.Bounds.Top,
      AExportCell.Value, AExportCell.DisplayFormat, AExportCell.DisplayFormatType)
  else
    Provider.SetCellValueAsString(AExportCell.Bounds.Left, AExportCell.Bounds.Top, AExportCell.Value);

  Provider.SetCellStyle(AExportCell.Bounds, AExportCell.StyleIndex);
end;

procedure TcxPivotGridMapsInfo.WriteCells;
var
  ACell: TcxPivotGridExportCell;
begin
  try
    ACell := TcxPivotGridExportCell(FCells.First);
    while ACell <> nil do
    begin
      WriteCell(ACell);
      ACell := TcxPivotGridExportCell(ACell.Next);
    end;
  finally
    Owner.ProgressHelper.NextTask;
    FCells.Clear;
  end;
end;

procedure TcxPivotGridMapsInfo.WriteColumnWidths;
var
  I: Integer;
begin
  for I := 0 to ExportArea.AxisX.Count - 2 do
    Provider.SetColumnWidth(I, ExportArea.AxisX.Size[I]);
end;

procedure TcxPivotGridMapsInfo.WriteRowHeights;
var
  I: Integer;
begin
  for I := 0 to ExportArea.AxisY.Count - 2 do
    Provider.SetRowHeight(I, ExportArea.AxisY.Size[I]);
end;

function TcxPivotGridMapsInfo.CheckNativeValue(AProperties: TcxCustomEditProperties;
  APivotField: TcxPivotGridField; const AValue: Variant): Variant;
begin
  if VarIsSoftEmpty(AValue) then
  begin
    if TcxPropertiesAccess(AProperties).UseNullString and (TcxPropertiesAccess(AProperties).Nullstring <> '') then
      Result := TcxPropertiesAccess(AProperties).Nullstring
    else
      Result := AValue
  end
  else
  begin
    try
      if (IsCurrencyField(APivotField) and IsCurrencyProperties(AProperties)) or (AProperties is TcxCurrencyEditProperties) then
        VarCast(Result, AValue, varCurrency)
      else
        if (VarType(AValue) = varCurrency) and not IsCurrencyField(APivotField) and not (AProperties is TcxCurrencyEditProperties) then
          VarCast(Result, AValue, varDouble)
        else
          Result := AValue;
    except
      on EVariantError do
        Result := AValue
      else
        raise;
    end;
  end;
end;

function TcxPivotGridMapsInfo.GetProvider: IcxExportProvider;
begin
  Result := FOwner.Provider;
end;

function TcxPivotGridMapsInfo.IsCurrencyField(APivotField: TcxPivotGridField): Boolean;
var
  APivotFieldAccess: TcxPivotGridFieldAccess;
begin
  APivotFieldAccess := TcxPivotGridFieldAccess(APivotField);
  Result := APivotFieldAccess.IsCurrency(APivotField.DataBinding.ValueTypeClass);
end;

function TcxPivotGridMapsInfo.IsCurrencyProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := (AProperties is TcxMaskEditProperties) or (AProperties is TcxCalcEditProperties) or
    (AProperties is TcxCurrencyEditProperties);
end;

function TcxPivotGridMapsInfo.IsNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := (AProperties is TcxDateEditProperties) or (AProperties is TcxSpinEditProperties)
     or (AProperties is TcxTimeEditProperties) or IsCurrencyProperties(AProperties);
end;

{ TcxPivotGridExportHelper }

constructor TcxPivotGridExportHelper.Create(APivotGrid: TcxCustomPivotGrid;
  AExportType: Integer; const AFileName: string; AHandler: TObject);

  function DefaultStyle: TcxCacheCellStyle;
  var
    I: Integer;
  begin
    cxViewParamsToCacheStyle(PivotGrid.Styles.GetBackgroundParams, Result);
    for I := 0 to 3 do
      Result.Borders[I] := cxEmptyBorder;
    Result.BrushBkColor := clDefault;
    Result.BrushFgColor := clDefault;
  end;

begin
  FHandler := AHandler;
  FPivotGrid := APivotGrid;
  FProgressHelper := TcxProgressCalculationHelper.Create(3, APivotGrid, ProgressHandler);
  TcxExport.Provider(AExportType, AFileName).GetInterface(IcxExportProvider, FProvider);
  FProvider.SetDefaultStyle(DefaultStyle);
end;

destructor TcxPivotGridExportHelper.Destroy;
begin
  try
    FProvider := nil;
  finally
    FreeAndNil(FProgressHelper);
    inherited Destroy;
  end;
end;

procedure TcxPivotGridExportHelper.ExportPivotGrid;
var
  AController: TcxPivotGridExportController;
begin
  AController := TcxPivotGridExportController.Create(PivotGrid);
  try
    AController.ExpandRows := Expand;
    AController.ExpandColumns := Expand;
    MapsInfo := GetMapsInfoClass.Create(Self, AController);
    try
      Provider.Commit(ProgressHelper, FHandler);
    finally
      FreeAndNil(MapsInfo);
    end;
  finally
    AController.Free;
  end;
end;

function TcxPivotGridExportHelper.GetMapsInfoClass: TcxPivotGridMapsInfoClass;
begin
  Result := TcxPivotGridMapsInfo;
end;

procedure TcxPivotGridExportHelper.ProgressHandler(Sender: TObject; Percents: Integer);
var
  AIntf: IcxExportProgress;
begin
  if Supports(FHandler, IcxExportProgress, AIntf) then
    AIntf.OnProgress(Sender, Percents);
end;

// external procedures definition

function cxSupports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

procedure cxExportPivotGridToFile(AFileName: string; APivotGrid: TcxCustomPivotGrid; AExportType: Integer;
  AExpand, AUseNativeFormat: Boolean; const ASeparators: array of string; const AFileExt: string;
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
var
  AEncodingIntf: IcxExportProviderEncoding;
  AIntf: IcxExportWithSeparators;
  I: Integer;
begin
  if AFileExt <> '' then
    AFileName := ChangeFileExt(AFileName, '.' + AFileExt);
  with TcxPivotGridExportHelper.Create(APivotGrid, AExportType, AFileName, AHandler) do
  try
    Expand := AExpand;
    FIsNativeFormat :=  AUseNativeFormat;
    if cxSupports(Provider, IcxExportProviderEncoding, AEncodingIntf) then
      AEncodingIntf.SetEncoding(AEncoding);
    if cxSupports(Provider, IcxExportWithSeparators, AIntf) and (Length(ASeparators) > 0) then
    begin
      for I := Low(ASeparators) to High(ASeparators) do
        AIntf.AddSeparator(ASeparators[I]);
    end;
    ExportPivotGrid;
  finally
    Free;
  end;
end;

procedure cxExportPivotGridToHTML(const AFileName: string; APivotGrid: TcxCustomPivotGrid;
  AExpand: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
begin
  cxExportPivotGridToFile(AFileName, APivotGrid, cxExportToHtml, AExpand, False, [], AFileExt, AHandler);
end;

procedure cxExportPivotGridToXML(const AFileName: string; APivotGrid: TcxCustomPivotGrid;
  AExpand: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);
begin
  cxExportPivotGridToFile(AFileName, APivotGrid, cxExportToXML, AExpand, False, [], AFileExt, AHandler);
end;

procedure cxExportPivotGridToExcel(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
begin
  cxExportPivotGridToFile(AFileName, APivotGrid, cxExportToExcel, AExpand, AUseNativeFormat, [], AFileExt, AHandler);
end;

procedure cxExportPivotGridToXLSX(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);
begin
  cxExportPivotGridToFile(AFileName, APivotGrid, cxExportToXlsx, AExpand, AUseNativeFormat, [], AFileExt, AHandler);
end;

procedure cxExportPivotGridToCSV(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  const ASeparator: Char = ','; const AFileExt: string = 'csv'; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportPivotGridToFile(AFileName, APivotGrid, cxExportToCSV,
    AExpand, False, [ASeparator], AFileExt, AHandler, AEncoding);
end;

procedure cxExportPivotGridToText(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
begin
  cxExportPivotGridToText(AFileName, APivotGrid, AExpand, '', '', '', AFileExt, AHandler, AEncoding);
end;

procedure cxExportPivotGridToText(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AExpand: Boolean = True;
  const ASeparator: string = ''; const ABeginString: string = ''; const AEndString: string = '';
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
begin
  cxExportPivotGridToFile(AFileName, APivotGrid, cxExportToText, AExpand, False,
    [ASeparator, ABeginString, AEndString], AFileExt, AHandler, AEncoding);
end;

//
procedure ExportPivotGridDataToExcel(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AHandler: TObject = nil;
  AAutoBestFit: Boolean =  True; AHighlightTotals: Boolean = True; ACellBorders: Boolean = True; AFieldHeaders: Boolean = False);
var
  AHelper: TcxPivotGridDataExportHelper;
begin
  AHelper := TcxPivotGridDataExportHelper.Create(APivotGrid, AFileName, AHandler);
  try
    AHelper.AutoBestFit := AAutoBestFit;
    AHelper.HighlightTotals := AHighlightTotals;
    AHelper.CellBorders := ACellBorders;
    AHelper.FieldHeaders := AFieldHeaders;
    AHelper.Export;
  finally
    AHelper.Free;
  end;
end;

{ TcxPivotGridExportViewInfo }

procedure TcxPivotGridExportViewInfo.AfterRowCellsCalculated(ARow: TcxPivotGridViewDataItem);
begin
  MapsInfo.ProcessDataCells;
  ARow.ClearCache;
end;

procedure TcxPivotGridExportViewInfo.CalculateCells;
begin
  MapsInfo.ProcessColsAndRows;
  Helper.ProgressHelper.BeginStage(FRowItems.Count * 2);
  try
    inherited CalculateCells;
  finally
    Helper.ProgressHelper.EndStage;
  end;
end;

constructor TcxPivotGridExportViewInfo.Create(AMapsInfo: TcxPivotGridMapsInfo);
begin
  FMapsInfo := AMapsInfo;
  inherited Create(MapsInfo.Controller.PivotGrid);
  FIsPrinting := True;
  TcxCustomPivotGridAccess(PivotGrid).ReplaceViewInfo(Self);
end;

function TcxPivotGridExportViewInfo.GetHelper: TcxPivotGridExportHelper;
begin
  Result := MapsInfo.Owner;
end;


type
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TcxPivotGridDataFieldAccess = class(TcxPivotGridOptionsDataField);

  { TcxPivotGridDataExportGroupsProducer }

  TcxPivotGridDataExportGroupsProducer = class
  private
    function GetOptionsView: TcxPivotGridOptionsView;
  protected
    Count: Integer;
    Current: TdxPivotGridExportGroupItem;
    DataFields: TcxPivotGridFields;
    DataFieldsIndex: Integer;
    FarTotalsLocation: Boolean;
    Fields: TcxPivotGridFields;
    GrandTotals: Boolean;
    GroupItem: TcxPivotGridGroupItem;
    Root: TdxPivotGridExportGroupItem;
    Totals: Boolean;
    TotalsForSingleValues: Boolean;
    function Add(AData: TcxPivotGridGroupItem; ALevel: Integer): TdxPivotGridExportGroupItem; overload;
    function Add(AData: TcxPivotGridGroupItem; AField: TcxPivotGridField; ALevel: Integer): TdxPivotGridExportGroupItem; overload;
    function IsTotalNeeded(AItem: TcxPivotGridGroupItem): Boolean; inline;
    function LastLevel: Integer;
    procedure PrepareDataFields(AParent: TcxPivotGridGroupItem; ALevel: Integer);
    procedure PrepareLevelItems(AParent: TcxPivotGridGroupItem; ALevel: Integer);
    procedure PrepareTotals(AParent: TcxPivotGridGroupItem; ALevel: Integer); inline;

    property OptionsView: TcxPivotGridOptionsView read GetOptionsView;
  public
    constructor Create(AParent: TcxPivotGridGroupItem);
    function MakeItems: TdxPivotGridExportGroupItem;

  end;

{ TcxPivotGridDataExportGroupsProducer }

constructor TcxPivotGridDataExportGroupsProducer.Create(AParent: TcxPivotGridGroupItem);
begin
  DataFieldsIndex := -1;
  GroupItem := AParent;
end;

function TcxPivotGridDataExportGroupsProducer.MakeItems: TdxPivotGridExportGroupItem;
begin
  if (GroupItem.ItemCount = 1) and not OptionsView.GrandTotalsForSingleValues then
    PrepareLevelItems(GroupItem.Items[0], 0)
  else
    PrepareLevelItems(GroupItem, 0);
  Result := Root;
end;

function TcxPivotGridDataExportGroupsProducer.Add(AData: TcxPivotGridGroupItem; ALevel: Integer): TdxPivotGridExportGroupItem;
begin
  Result := Add(AData, nil, ALevel);
end;

function TcxPivotGridDataExportGroupsProducer.Add(
  AData: TcxPivotGridGroupItem; AField: TcxPivotGridField; ALevel: Integer): TdxPivotGridExportGroupItem;
begin
  if Current = nil then
    Current := TdxPivotGridExportGroupItem.Create(AData, AField, ALevel)
  else
    Current := Current.Add(AData, AField, ALevel);
  if Root = nil then
    Root := Current;
  Result := Current;
  Inc(Count);
end;

procedure TcxPivotGridDataExportGroupsProducer.PrepareDataFields(AParent: TcxPivotGridGroupItem; ALevel: Integer);
var
  AIndex, AChildIndex: Integer;
begin
  if (ALevel = 0) and (AParent.Parent <> nil) then
    AParent := AParent.Parent;
  for AIndex := 0 to DataFields.Count - 1 do
  begin
    Add(nil, DataFields[AIndex], ALevel);
    for AChildIndex := 0 to AParent.ItemCount - 1 do
      PrepareLevelItems(AParent.Items[AChildIndex], ALevel + 1)
  end;
end;

procedure TcxPivotGridDataExportGroupsProducer.PrepareLevelItems(AParent: TcxPivotGridGroupItem; ALevel: Integer);
var
  AIndex: Integer;
  ANeedTotal: Boolean;
begin
  if (ALevel = DataFieldsIndex) and (DataFields.Count > 1) then
  begin
    if (ALevel = 0) and (AParent.Parent = nil) and (AParent.ItemCount = 0)  then
    begin
      Add(AParent, ALevel);
      Inc(ALevel)
    end;
    PrepareDataFields(AParent, ALevel)
  end
  else
  begin
    ANeedTotal := IsTotalNeeded(AParent);
    if not FarTotalsLocation and ANeedTotal then
      PrepareTotals(AParent, ALevel);
    //
    if (AParent.Parent = nil) and (AParent.ItemCount > 0)  then
      Dec(ALevel)
    else
      Add(AParent, ALevel);
    if ALevel + 1 = DataFieldsIndex then
      PrepareDataFields(AParent, ALevel + 1)
    else
    begin
      if AParent.ItemCount > 0 then
        for AIndex := 0 to AParent.ItemCount - 1 do
          PrepareLevelItems(AParent.Items[AIndex], ALevel + 1);
    end;
    //
    if FarTotalsLocation and ANeedTotal then
      PrepareTotals(AParent, Max(0, ALevel));
  end;
end;

procedure TcxPivotGridDataExportGroupsProducer.PrepareTotals(AParent: TcxPivotGridGroupItem; ALevel: Integer);
var
  ATotalIndex, ADataIndex: Integer;
begin
  for ATotalIndex := 0 to AParent.TotalsCount - 1 do
  begin
    Add(AParent, ALevel).TotalIndex := ATotalIndex;
    if (DataFieldsIndex >= ALevel) and (DataFields.Count > 1) then
      for ADataIndex := 0 to DataFields.Count - 1 do
        Add(AParent, DataFields[ADataIndex], LastLevel).TotalIndex := ATotalIndex;
  end;
end;

function TcxPivotGridDataExportGroupsProducer.IsTotalNeeded(AItem: TcxPivotGridGroupItem): Boolean;
begin
  Result := Totals or ((AItem.Parent = nil) and GrandTotals);
  if not Result then
    Exit;
  if AItem.ItemCount = 1 then
    Result := TotalsForSingleValues
  else
    Result := AItem.ItemCount > 0;
end;

function TcxPivotGridDataExportGroupsProducer.LastLevel: Integer;
begin
  Result := Fields.Count - 1;
  if (DataFieldsIndex <> -1) and (DataFields.Count > 0) then
    Inc(Result);
end;

function TcxPivotGridDataExportGroupsProducer.GetOptionsView: TcxPivotGridOptionsView;
begin
  Result := GroupItem.PivotGrid.OptionsView;
end;

{ TdxPivotGridExportGroupItem }

constructor TdxPivotGridExportGroupItem.Create(AGroup: TcxPivotGridGroupItem; ADataField: TcxPivotGridField; ALevel: Integer);
begin
  Group := AGroup;
  GroupStart := -1;
  GroupFinish := -1;
  DataField := ADataField;
  Level := ALevel;
  TotalIndex := -1;

end;

function TdxPivotGridExportGroupItem.Add(AGroup: TcxPivotGridGroupItem; ADataField: TcxPivotGridField; ALevel: Integer): TdxPivotGridExportGroupItem;
begin
  Result := TdxPivotGridExportGroupItem.Create(AGroup, ADataField, ALevel);
  Next := Result;
end;

destructor TdxPivotGridExportGroupItem.Destroy;
begin
  Next.Free;
  inherited Destroy;
end;

function TdxPivotGridExportGroupItem.GetIsGrandTotal: Boolean;
begin
  Result := Group.Parent = nil;
end;

function TdxPivotGridExportGroupItem.GetIsTotal: Boolean;
begin
  Result := TotalIndex >= 0;
end;

function TdxPivotGridExportGroupItem.GetTotalDisplayText: string;
begin
  if Group.Field.CustomTotals.Count = 0 then
    Result := cxGetResourceString(@scxGroupTotal)
   else
     Result := cxGetResourceString(TotalDescriptions[Group.Field.CustomTotals[TotalIndex].SummaryType]);
   Result := Format(Result, [Group.Value])
end;

function TdxPivotGridExportGroupItem.GetValue: Variant;
begin
  if DataField <> nil then
    Result := DataField.Caption
  else
    if IsTotal and not IsGrandTotal then
      Result := GetTotalDisplayText
    else
      Result := Group.DisplayText;
end;

{ TdxPivotGridExportContentItem }

constructor TdxPivotGridExportContentItem.Create(APattern: TdxPivotGridExportContentItem);
begin
  TotalIndex := -1;
  if APattern <> nil then
  begin
    Field := APattern.Field;
    Group := APattern.Group;
    DataField := APattern.DataField;
    Style := APattern.Style;
    TotalIndex := APattern.TotalIndex;
  end;
end;

destructor TdxPivotGridExportContentItem.Destroy;
begin
  FreeAndNil(Next);
  inherited Destroy;
end;

function TdxPivotGridExportContentItem.IsTotal: Boolean;
begin
  Result := (TotalIndex >= 0) or ((Group <> nil) and (Group.Parent = nil));
end;

{ TcxPivotGridDataExportHelper }

constructor TcxPivotGridDataExportHelper.Create(AControl: TObject; const AFileName: string; AHandler: TObject);
begin
  inherited Create(AControl, AFileName, AHandler);
  FAutoBestFit := True;
  FWordWrap := True;
  FHighlightTotals := True;
end;

destructor TcxPivotGridDataExportHelper.Destroy;
begin
  FreeAndNil(Groups);
  FreeAndNil(Columns);
  FreeAndNil(Rows);
  FreeAndNil(ColumnsContent);
  FreeAndNil(RowsContent);
  inherited Destroy;
end;

procedure TcxPivotGridDataExportHelper.BeforeExport;
begin
  Groups := TList.Create;
  PivotGrid.BeginUpdate;
  ViewData := PivotGrid.ViewData;
  PrepareRows;
  PrepareColumns;
end;

procedure TcxPivotGridDataExportHelper.AfterExport;
begin
  PivotGrid.EndUpdate;
end;

procedure TcxPivotGridDataExportHelper.DoExport;
begin
   View.FreezePanes(ColumnMaxLevel, RowMaxLevel);
   ExecuteTask(WriteGroupRows);
   ExecuteTask(WriteGroupColumns);
   WriteRows;
end;

procedure TcxPivotGridDataExportHelper.ExportFieldHeaders;
begin
end;

function TcxPivotGridDataExportHelper.GetStageCount: Integer;
begin
  Result := RowCount + 2;  // 2 - for column and row group values
end;

procedure TcxPivotGridDataExportHelper.AddContentItem(AGroup: TdxPivotGridExportGroupItem;
  var AContentItems: TdxPivotGridExportContentItem);
begin
  if AContentItems = nil then
     FContentItem := nil;
  if FContentItem = nil then
  begin
    FContentItem :=  TdxPivotGridExportContentItem.Create(nil);
    AContentItems := FContentItem;
  end
  else
    if FContentItem.TableItem <> AGroup.TableItem then
    begin
      FContentItem.Next := TdxPivotGridExportContentItem.Create(FContentItem);
      FContentItem := FContentItem.Next;
    end;
  //
  if AGroup.Group <> nil then
  begin
    FContentItem.Group := AGroup.Group;
    FContentItem.Field := AGroup.Group.Field;
    FContentItem.TotalIndex := AGroup.TotalIndex;
  end;

  if AGroup.DataField <> nil then
    FContentItem.DataField := AGroup.DataField;
  if AGroup.TotalIndex >= 0 then
  begin
    FContentItem.Field := AGroup.Group.Field;
    FContentItem.TotalIndex := AGroup.TotalIndex;
  end;
  FContentItem.TableItem := AGroup.TableItem;
end;

function TcxPivotGridDataExportHelper.AddFieldHeader(ARow, AColumn: Integer; AField: TcxPivotGridField): TdxSpreadSheetCell;
var
  AText: string;
begin
  if AField <> nil then
    AText := AField.Caption
  else
    AText := cxGetResourceString(@scxDataField);
  Result := AddCellValue(ARow, AColumn, AText);
  SetContentCellStyle(Result, nil, True);
end;

function TcxPivotGridDataExportHelper.AddGroupValue(ARow, AColumn: Integer;
  AGroup: TdxPivotGridExportGroupItem; ATableItems: TdxSpreadSheetTableItems;
  var AContentItems: TdxPivotGridExportContentItem; var AIndex: Integer): TdxSpreadSheetCell;
var
  ANewGroup: Boolean;
  AField: TcxPivotGridField;
begin
  Result := View.CreateCell(ARow, AColumn);
  Result.AsVariant := AGroup.Value;
  AField := nil;
  if AGroup.Group <> nil then
    AField := AGroup.Group.Field;

  if (AField <> nil) and (AField.Properties <> nil) then
    Result.AsVariant := AField.Properties.GetDisplayText(AGroup.Value);

  SetContentCellStyle(Result, AField, (AGroup.TotalIndex >= 0) and HighlightTotals);
  AGroup.TableItem := ATableItems[AIndex];
  ANewGroup := IsNewGroup(AGroup) and not IsTotal(AGroup);
  if ANewGroup then
  begin
    Result.Style.Font.Style := [fsBold];
    Groups.Add(AGroup);
    AGroup.GroupStart := AIndex + 1;
  end
  else
    if not IsTotal(AGroup) then
      Result.Style.WordWrap := WordWrap;
  if not ANewGroup or IsTotal(AGroup) or (AGroup.DataField <> nil) then
    AddContentItem(AGroup, AContentItems);
  if ANewGroup or NeedNewLineForNextItem(AGroup) then
    Inc(AIndex);
end;

procedure TcxPivotGridDataExportHelper.ApplyBestFit(AStartItem: TdxSpreadSheetTableItem);
begin
  while AStartItem <> nil do
  begin
    AStartItem.ApplyBestFit;
    AStartItem := AStartItem.Next;
  end;
end;

procedure TcxPivotGridDataExportHelper.CalculateGroupsFinish;

  procedure FindFinish(AGroup: TdxPivotGridExportGroupItem);
  var
    AItem: TdxPivotGridExportGroupItem;
  begin
    AItem := AGroup;
    while AItem <> nil do
    begin
      AGroup.GroupFinish := AItem.TableItem.Index;
      if (AItem.Level < AGroup.Level) or
        ((AItem.Group = AGroup.Group) and IsTotal(AItem)) or
        ((AItem.Group <> AGroup.Group) and (AItem.Level = AGroup.Level)) then
        Break;
      AItem := AItem.Next;
    end;
    Dec(AGroup.GroupFinish);
  end;

var
  I: Integer;
begin
  for I := 0 to Groups.Count - 1 do
    FindFinish(TdxPivotGridExportGroupItem(Groups[I]));
end;

function TcxPivotGridDataExportHelper.GetColumnWidthForColumn(AColumn: TdxPivotGridExportGroupItem): Integer;
begin
  Result := PivotGrid.OptionsView.ColumnGrandTotalWidth;
  if AColumn.DataField <> nil then
    Result := AColumn.DataField.Width
  else
     if (AColumn.Group <> nil) and (AColumn.Group.Field <> nil) then
       Result := AColumn.Group.Field.Width;
  if Result = 0 then
    Result := cxPivotGridDefaultFieldWidth;
end;

function TcxPivotGridDataExportHelper.GetColumnWidthForRow(ARow: TdxPivotGridExportGroupItem): Integer;
begin
  Result := PivotGrid.OptionsView.RowGrandTotalWidth;
  if ARow.DataField <> nil then
    Result := PivotGrid.OptionsDataField.Width
  else
     if (ARow.Group <> nil) and (ARow.Group.Field <> nil) then
       Result := ARow.Group.Field.Width;
  if Result = 0 then
    Result := cxPivotGridDefaultFieldWidth;
end;

function TcxPivotGridDataExportHelper.GetDataField(ARow, AColumn: TdxPivotGridExportContentItem): TcxPivotGridField;
begin
  Result := DataField;
  if ARow.DataField <> nil then
    Result := ARow.DataField;
  if AColumn.DataField <> nil then
    Result := AColumn.DataField;
end;

function TcxPivotGridDataExportHelper.GetDataFieldIndex(AArea: TcxPivotGridDataFieldArea): Integer;
begin
  Result := TcxPivotGridDataFieldAccess(PivotGrid.OptionsDataField).GetActualAreaIndex(False);
  if ((AArea = dfaRow) and (PivotGrid.OptionsDataField.Area <> dfaRow)) or
    ((AArea = dfaColumn) and (PivotGrid.OptionsDataField.Area = dfaRow)) then
    Result := -1;
end;

function TcxPivotGridDataExportHelper.GetSummaryType(AItem: TdxPivotGridExportContentItem;
  ADefaultType: TcxPivotGridSummaryType; var ADisplayFormat: string): TcxPivotGridSummaryType;
begin
  Result := ADefaultType;
  if (AItem.TotalIndex >= 0) and (AItem.Field <> nil) and (AItem.Field.CustomTotals.Count > 0) then
  begin
    Result := AItem.Field.CustomTotals[AItem.TotalIndex].SummaryType;
    ADisplayFormat := AItem.Field.CustomTotals[AItem.TotalIndex].DisplayFormat;
  end;
end;

function TcxPivotGridDataExportHelper.IsItemExpanded(AGroup: TdxPivotGridExportGroupItem): Boolean;
begin
  Result := (AGroup.Group = nil) or AGroup.Group.Expanded;
end;

function TcxPivotGridDataExportHelper.IsLastGroup(AGroup: TdxPivotGridExportGroupItem): Boolean;
begin
  Result := AGroup.Next = nil;
  if Result then
    Exit;
  Result := (AGroup.Next.Level < AGroup.Level) or IsTotal(AGroup);
end;

function TcxPivotGridDataExportHelper.IsNewGroup(AGroup: TdxPivotGridExportGroupItem): Boolean;
begin
  Result := (AGroup.DataField = nil) and (AGroup.Group <> nil) and (AGroup.Group.ItemCount > 0);
end;

function TcxPivotGridDataExportHelper.IsTotal(AGroup: TdxPivotGridExportGroupItem): Boolean;
begin
  Result := AGroup.TotalIndex >= 0;
end;

function TcxPivotGridDataExportHelper.NeedNewLineForNextItem(AItem: TdxPivotGridExportGroupItem): Boolean;
begin
  Result := (AItem.Next <> nil) and (AItem.Level >= AItem.Next.Level);
end;

procedure TcxPivotGridDataExportHelper.PrepareColumns;
var
  AProducer: TcxPivotGridDataExportGroupsProducer;
begin
  AProducer := TcxPivotGridDataExportGroupsProducer.Create(ViewData.DataBuilder.Columns);
  try
    AProducer.DataFieldsIndex := GetDataFieldIndex(dfaColumn);
    AProducer.Fields := ViewData.DataBuilder.ColumnFields;
    AProducer.DataFields := ViewData.DataBuilder.DataFields;
    AProducer.GrandTotals := PivotGrid.OptionsView.ColumnGrandTotals;
    AProducer.FarTotalsLocation := PivotGrid.OptionsView.ColumnTotalsLocation = ctlFar;
    AProducer.Totals := PivotGrid.OptionsView.ColumnTotals;
    AProducer.TotalsForSingleValues := PivotGrid.OptionsView.TotalsForSingleValues;
    Columns := AProducer.MakeItems;
    ColumnCount := AProducer.Count;
    ColumnMaxLevel := AProducer.LastLevel;
    if (AProducer.Fields.Count = 0) and (AProducer.DataFields.Count > 1) then
      Inc(ColumnMaxLevel);
  finally
    AProducer.Free;
  end;
end;

procedure TcxPivotGridDataExportHelper.PrepareRows;
var
  AProducer: TcxPivotGridDataExportGroupsProducer;
begin
  AProducer := TcxPivotGridDataExportGroupsProducer.Create(ViewData.DataBuilder.Rows);
  try
    AProducer.DataFieldsIndex := GetDataFieldIndex(dfaRow);
    AProducer.Fields := ViewData.DataBuilder.RowFields;
    AProducer.DataFields := ViewData.DataBuilder.DataFields;

    AProducer.FarTotalsLocation := PivotGrid.OptionsView.RowTotalsLocation <> rtlNear;
    AProducer.Totals := PivotGrid.OptionsView.RowTotals;
    AProducer.TotalsForSingleValues := PivotGrid.OptionsView.TotalsForSingleValues;
    AProducer.GrandTotals := PivotGrid.OptionsView.RowGrandTotals;
    Rows := AProducer.MakeItems;
    RowCount := AProducer.Count;
    RowMaxLevel := AProducer.LastLevel;
  finally
    AProducer.Free;
  end;
end;

procedure TcxPivotGridDataExportHelper.SetContentCellStyle(ACell: TdxSpreadSheetCell;
  ADataField: TcxPivotGridField; AIsTotal: Boolean; const ADisplayFormat: string = '');
var
  AFormat: string;
  ABrush: TdxSpreadSheetBrushHandle;
  ABorders: TdxSpreadSheetBordersHandle;
  AStyle: TdxSpreadSheetCellStyleHandle;
begin
  if ContentStyles[AIsTotal] = nil then
  begin
    AStyle := ACell.StyleHandle.Clone;
    if CellBorders then
    begin
      ABorders := AStyle.Borders.Clone;
      FillChar(ABorders.BorderStyle, SizeOf(ABorders.BorderStyle), sscbsThin);
      AStyle.Borders := SpreadSheet.CellStyles.Borders.AddBorders(ABorders);
    end;
    if AIsTotal then
    begin
      ABrush := AStyle.Brush.Clone;
      if ABrush.BackgroundColor = clDefault then
        ABrush.BackgroundColor := clWindow;
      ABrush.BackgroundColor := dxGetDarkerColor(ColorToRgb(ABrush.BackgroundColor), 85);
      ABrush.Style := sscfsSolid;
      AStyle.Brush := SpreadSheet.CellStyles.Brushes.AddBrush(ABrush);
    end;
    dxChangeHandle(TdxHashTableItem(ContentStyles[AIsTotal]), SpreadSheet.CellStyles.AddStyle(AStyle));
  end;
  AStyle := ContentStyles[AIsTotal];
  //
  AFormat := ADisplayFormat;
  if (AFormat = '') and (ADataField <> nil) then
    AFormat := PrepareDisplayFormat(ACell, ADataField.DisplayFormat);
  if AFormat = '' then
    AFormat := ACell.Style.DataFormat.FormatCode;
  if AFormat <> '' then
  begin
    AStyle := AStyle.Clone;
    AStyle.DataFormat := SpreadSheet.CellStyles.Formats.AddFormat(AFormat);
    AStyle := SpreadSheet.CellStyles.AddStyle(AStyle);
  end;
  //
  ACell.StyleHandle := AStyle;
end;

procedure TcxPivotGridDataExportHelper.WriteFieldHeaders(
  ARow, AColumn: Integer; AFields: TcxPivotGridFields; AArea: TcxPivotGridDataFieldArea);
var
  ADataFieldIndex, AIndex, AFieldIndex: Integer;
begin
  AIndex := 0;
  AFieldIndex := 0;
  ADataFieldIndex := TcxPivotGridDataFieldAccess(PivotGrid.OptionsDataField).GetActualAreaIndex;
  if (ViewData.DataBuilder.DataFields.Count <= 1) or (PivotGrid.OptionsDataField.Area <> AArea) then
    ADataFieldIndex := -1;
  while AIndex < AFields.Count + Byte(ADataFieldIndex > 0)  do
  begin
    if AIndex = ADataFieldIndex then
      AddFieldHeader(ARow, AColumn + AIndex, nil).Style.Font.Color := clRed
    else
    begin
      AddFieldHeader(ARow, AColumn + AIndex, AFields[AFieldIndex]);
      Inc(AFieldIndex);
    end;
    Inc(AIndex);
  end;
end;

procedure TcxPivotGridDataExportHelper.WriteGroupColumns;
var
  AColumnIndex, AIndex: Integer;
  AColumn: TdxPivotGridExportGroupItem;
begin
  Groups.Clear;
  AColumn := Columns;
  AIndex := RowMaxLevel + 1;
  if FieldHeaders then
    WriteFieldHeaders(0, AIndex, ViewData.DataBuilder.ColumnFields, dfaColumn);
  while AColumn <> nil do
  begin
    AColumnIndex := AIndex;
    AddGroupValue(AColumn.Level + Byte(FieldHeaders), AIndex, AColumn, View.Columns, ColumnsContent, AIndex);
    if AColumn.Level = ColumnMaxLevel then
      View.Columns[AColumnIndex].Size := GetColumnWidthForColumn(AColumn);
    AColumn := AColumn.Next;
  end;
  WriteGroupsPost(Columns, View.Columns);
  FreeAndNil(Columns);
end;

procedure TcxPivotGridDataExportHelper.WriteGroupRows;
var
  AIndex: Integer;
  ARow: TdxPivotGridExportGroupItem;
begin
  Groups.Clear;
  ARow := Rows;
  AIndex := ColumnMaxLevel + 1;

  if FieldHeaders then
  begin
    WriteFieldHeaders(AIndex, 0, ViewData.DataBuilder.RowFields, dfaRow);
    Inc(AIndex);
  end;

  while ARow <> nil do
  begin
    AddGroupValue(AIndex, ARow.Level, ARow, View.Rows, RowsContent, AIndex);
    View.Columns[ARow.Level].Size := GetColumnWidthForRow(ARow);
    ARow := ARow.Next;
  end;
  WriteGroupsPost(Rows, View.Rows);
  FreeAndNil(Rows);
end;

procedure TcxPivotGridDataExportHelper.WriteGroupsPost(
  ARoot: TdxPivotGridExportGroupItem; AItems: TdxSpreadSheetTableItems);
var
  AIndex: Integer;
  AGroup: TdxPivotGridExportGroupItem;
begin
  CalculateGroupsFinish;
  for AIndex := 0 to Groups.Count - 1 do
  begin
    AGroup := TdxPivotGridExportGroupItem(Groups[AIndex]);
    AddGroupBy(AItems, AGroup.GroupStart, AGroup.GroupFinish, IsItemExpanded(AGroup));
  end;
end;

procedure TcxPivotGridDataExportHelper.WriteRow(ARow: TdxPivotGridExportContentItem);
var
  AValue: Variant;
  ADisplayFormat: string;
  AField: TcxPivotGridField;
  ADestCell: TdxSpreadSheetCell;
  ACrossCell: TcxPivotGridCrossCell;
  AColumn: TdxPivotGridExportContentItem;
  ASummaryType: TcxPivotGridSummaryType;
begin
  AColumn := ColumnsContent;
  while AColumn <> nil do
  begin
    ADestCell := ARow.TableItem.CreateCell(AColumn.TableItem.Index);
    AValue := Null;
    AField := GetDataField(ARow, AColumn);
    ADisplayFormat := '';
    if (AColumn.Group <> nil) and (ARow.Group <> nil) and (AField <> nil) then
    begin
      ASummaryType := GetSummaryType(ARow, AField.SummaryType, ADisplayFormat);
      ASummaryType := GetSummaryType(AColumn, ASummaryType, ADisplayFormat);
      ACrossCell := ARow.Group.GetCellByCrossItem(AColumn.Group);
      if ACrossCell.Records.Count > 0 then
      begin
        if AField.SummaryVariation <> svNone then
          AValue := ACrossCell.SummaryCells[AField.SummaryIndex].SummaryVariation
        else
          AValue := ACrossCell.GetSummaryByField(AField, ASummaryType);
        try
          if not (VarIsNull(AValue) or VarIsEmpty(AValue) or ((AValue = Unassigned) and (AValue <> 0))) then
          begin
            if Pos('%', ADisplayFormat) > 0 then
              AValue := AValue / 100;
            ADestCell.AsVariant := AValue;
          end;
        except
          on EVariantError do;
        end;
      end;
    end;
    SetContentCellStyle(ADestCell, AField, (ARow.IsTotal or AColumn.IsTotal) and HighlightTotals, ADisplayFormat);
    AColumn := AColumn.Next;
  end;
  TcxPivotGridRowAccess(ARow.Group).ClearCache;
end;

procedure TcxPivotGridDataExportHelper.WriteRows;
var
  ARow: TdxPivotGridExportContentItem;
begin
  if ViewData.DataBuilder.DataFields.Count = 1 then
    DataField := ViewData.DataBuilder.DataFields[0];
  ARow := RowsContent;
  while ARow <> nil do
  begin
    ProgressHelper.BeginStage(1);
    try
      WriteRow(ARow);
    finally
      ProgressHelper.EndStage();
    end;
    ARow := ARow.Next;
  end;
  if AutoBestFit then
  begin
    ApplyBestFit(View.Columns.First);
    ApplyBestFit(View.Rows.First);
  end;
end;

function TcxPivotGridDataExportHelper.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := TcxCustomPivotGrid(Control);
end;

//
procedure cxExportPivotGridDataToExcel(const AFileName: string; APivotGrid: TcxCustomPivotGrid; AHandler: TObject = nil;
  AAutoBestFit: Boolean =  True; AHighlightTotals: Boolean = True; ACellBorders: Boolean = True; AFieldHeaders: Boolean = False);
var
  AHelper: TcxPivotGridDataExportHelper;
begin
  AHelper := TcxPivotGridDataExportHelper.Create(APivotGrid, AFileName, AHandler);
  try
    AHelper.AutoBestFit := AAutoBestFit;
    AHelper.HighlightTotals := AHighlightTotals;
    AHelper.CellBorders := ACellBorders;
    AHelper.FieldHeaders := AFieldHeaders;
    AHelper.Export;
  finally
    AHelper.Free;
  end;
end;


initialization
  cxDefaultExportStyle.FontName := cxDefaultExportFontName;
end.



