{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressExport                                            }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEXPORT AND ALL                 }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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

unit cxExportProviders;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, Variants, Generics.Defaults, Generics.Collections, ImgList,
{$IFNDEF NONDB}
  FMTBcd, SqlTimSt,
{$ENDIF}
  cxClasses, dxCore, cxExport, cxGraphics, dxSpreadSheet, dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetTypes,
  dxSpreadSheetGraphics, dxSpreadSheetUtils, dxSpreadSheetFormatHTML, dxSpreadSheetFormatXLS, dxSpreadSheetFormatTXT,
  dxSpreadSheetFormatXML, dxSpreadSheetFormatCSV, cxFormats, dxHashUtils, cxGeometry, dxSpreadSheetCoreStyles,
  dxSpreadSheetConditionalFormatting, dxCoreClasses, dxSpreadSheetContainers, cxEdit, cxImage, cxImageCombobox;

type
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);

  { TdxSpreadSheetBasedExporter }

  TdxSpreadSheetBasedExporter = class(TdxSpreadSheet)
  protected
    FGenerateLockCount: Integer;

    function DoCreateSheet(var ASheet: TdxSpreadSheetCustomView; const ACaption: string = '';
      AViewClass: TdxSpreadSheetCustomViewClass = nil): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginGenerate;
    procedure EndGenerate;
  end;

  { TdxSpreadSheetBasedExporterCell }

  TdxSpreadSheetBasedExporterCell = class(TdxSpreadSheetCell)
  protected
    procedure Changed(AChanges: TdxSpreadSheetChanges); override;
    procedure CheckDefaultCellFormat; override;
  end;

  { TdxSpreadSheetBasedExporterTableRow }

  TdxSpreadSheetBasedExporterTableRow = class(TdxSpreadSheetTableRow)
  protected
    function CreateRowCells: TdxSpreadSheetTableRowCells; override;
    procedure DoCellCreated(ACell: TdxSpreadSheetCell); override;
  end;

  { TdxSpreadSheetBasedExporterTableRowCells }

  TdxSpreadSheetBasedExporterTableRowCells = class(TdxSpreadSheetTableRowCells)
  protected
    function GetItemClass: TdxDynamicListItemClass; override;
  end;

  { TdxSpreadSheetBasedExporterTableRows }

  TdxSpreadSheetBasedExporterTableRows = class(TdxSpreadSheetTableRows)
  protected
    function GetItemClass: TdxDynamicListItemClass; override;
  end;

  { TdxSpreadSheetBasedExporterTableViewInfo }

  TdxSpreadSheetBasedExporterTableViewInfo = class(TdxSpreadSheetTableViewInfo)
  protected
    procedure CalculateCells; override;
  end;

  { TdxSpreadSheetBasedExporterTableView }

  TdxSpreadSheetBasedExporterTableView = class(TdxSpreadSheetTableView)
  protected
    function CreateRows: TdxSpreadSheetTableRows; override;
    function CreateViewInfo: TdxSpreadSheetCustomViewViewInfo; override;
    procedure Pack; override;
  end;

   TcxExportSharedImagesInfo = TDictionary<Integer, TObject>;

  { TcxExportProvider }

  TcxExportProvider = class(TcxCustomExportProvider,
    IcxExportProvider,
    IcxExportProvider2,
    IcxNameExportProvider)
  strict private
    FProgressHelper: TcxCustomProgressCalculationHelper;
    FProxyCell: TdxSpreadSheetCellAccess;
    FSpreadSheet: TdxSpreadSheetBasedExporter;
    FStyles: TList<TcxCacheCellStyle>;
    FTableView: TdxSpreadSheetTableView;

    function CreateStyle(const AStyle: TcxCacheCellStyle): TdxSpreadSheetCellStyleHandle;
    function GetBorderStyleByWidth(AWidth: Integer): TdxSpreadSheetCellBorderStyle;
    function GetStyleIndex(AHandle: TdxSpreadSheetCellStyleHandle): Integer;
    procedure ProgressHandler(Sender: TObject; Percent: Integer);
    procedure SetupPictureContainer(AContainer: TdxSpreadSheetPictureContainer; const AArea: TRect; AFitMode: TcxImageFitMode; const ASize: TSize);
  protected
    FDefaultAlignVert: TdxSpreadSheetDataAlignVert;
    FSharedImagesMap: TObjectDictionary<TObject, TcxExportSharedImagesInfo>;

    procedure Clear; override;

    // IcxExportProvider
    procedure AddSharedImageHandle(AImageList: TObject; AImageIndex: Integer; AHandle: TObject);
    function AddPictureContainer(const AArea: TRect; AFitMode: TcxImageFitMode; const ASize: TSize): TdxSpreadSheetPictureContainer; inline;
    procedure AddConditionalFormattingRule(ARule: TdxSpreadSheetCustomConditionalFormattingRule; AAreas: TdxRectList);
    procedure AddOutlineGroup(AStart, AFinish: Integer);
    procedure ApplyBestFit;
    procedure Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject); virtual;
    procedure FreezePanes(ACol, ARow: Integer);
    function GetFormatCode(ACol, ARow: Integer): string;
    function GetStyle(AStyleIndex: Integer): TcxCacheCellStyle;
    function RegisterStyle(const AStyle: TcxCacheCellStyle): Integer;
    function SetCellGraphic(const AArea: TRect; AGraphic: TGraphic; AFitMode: TcxImageFitMode = ifmStretch): TObject; overload;
    function SetCellGraphic(const AArea: TRect; AStyleIndex: Integer; AGraphic: TGraphic; AFitMode: TcxImageFitMode = ifmStretch): TObject; overload;
    procedure SetCellGraphicAsSharedHandle(const AArea: TRect; AStyleIndex: Integer; AHandle: TObject; AFitMode: TcxImageFitMode = ifmStretch);
    procedure SetCellPNGImage(const ACol, ARow: Integer; APNGImage: TGraphic; AFitMode: TcxImageFitMode = ifmStretch);
    procedure SetCellStyle(const AArea: TRect; AStyleIndex: Integer); overload;
    procedure SetCellStyle(const ACol, ARow, AStyleIndex: Integer); overload;
    procedure SetCellUnion(const ACol, ARow: Integer; H, W: Integer);
    procedure SetCellValue(const ACol, ARow: Integer; const AValue: Variant;
      const AValueDisplayFormat: string = ''; AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto);
    procedure SetCellValueAsFormula(const ACol, ARow: Integer; const AValue: string; ADisplayText: string = '';
      AFormatCode: string = ''; AListSeparator: Char = ',');
    procedure SetCellValueAsString(const ACol, ARow: Integer; const AText: string);
    procedure SetColumnWidth(const ACol, AWidth: Integer);
    procedure SetDefaultStyle(const AStyle: TcxCacheCellStyle);
    procedure SetRange(const AColCount, ARowCount: Integer; IsVisible: Boolean = True);
    procedure SetRowHeight(const ARow, AHeight: Integer);
    function SupportGraphic: Boolean; virtual;
    function SupportRTF: Boolean; virtual;
    // IcxExportProvider2
    function AddContainer(AItem: TcxExportDataItem; AFitMode: TcxImageFitMode): TdxSpreadSheetPictureContainer; inline;
    procedure AddGraphic(AItem: TcxExportDataItem; AGraphic: TGraphic; AFitMode: TcxImageFitMode);
    procedure AddImageListItem(AItem: TcxExportDataItem; AImages: TCustomImageList; AImageIndex: Integer; ABkColor: TColor);
    procedure AddText(AItem: TcxExportDataItem; const AText: string);
    procedure AddValue(AItem: TcxExportDataItem; const AValue: Variant; AProperties: TcxCustomEditProperties;
      const AValueDisplayFormat: string = ''; AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto);
    procedure SetData(AItem: TcxExportDataItem);
    //
    function TryGetSharedImageHandle(AImageList: TObject; AImageIndex: Integer; var AHandle: TObject): Boolean; inline;

    // IcxNameExportProvider
    procedure SetName(const AName: string);
    procedure SetRangeName(const AName: string; const ARange: TRect);
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; virtual; abstract;
    //
    property SpreadSheet: TdxSpreadSheetBasedExporter read FSpreadSheet;
    property TableView: TdxSpreadSheetTableView read FTableView;
  end;

 { TcxExportToCSVProvider }

  TcxExportToCSVProvider = class(TcxExportProvider,
    IcxExportWithSeparators,
    IcxExportProviderEncoding)
  strict private
    FEncoding: TEncoding;
    FSeparator: Char;
    FSeparatorIndex: Integer;
  protected
    procedure Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject); override;
    // IcxExportWithSeparators
    procedure AddSeparator(const ASeparator: string);
    // IcxExportProviderEncoding
    procedure SetEncoding(AEncoding: TEncoding);
    function SupportRTF: Boolean; override;
  public
    class function ExportName: string; override;
    class function ExportType: Integer; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; override;
  end;

  { TcxExportToHTMLProvider }

  TcxExportToHTMLProvider = class(TcxExportProvider)
  protected
    procedure Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject); override;
  public
    constructor Create(const AFileName: string); override;
    class function ExportName: string; override;
    class function ExportType: Integer; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; override;
  end;

  { TcxExportToTXTProvider }

  TcxExportToTXTProvider = class(TcxExportProvider,
    IcxExportProviderEncoding,
    IcxExportWithSeparators)
  strict private
    FFormatSettings: TdxSpreadSheetTXTFormatSettings;
    FSeparatorIndex: Integer;

    // IcxExportWithSeparators
    procedure AddSeparator(const ASeparator: string);
    // IcxExportProviderEncoding
    procedure SetEncoding(AEncoding: TEncoding);
  protected
    procedure Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject); override;
    function SupportGraphic: Boolean; override;
    function SupportRTF: Boolean; override;
  public
    procedure AfterConstruction; override;
    class function ExportName: string; override;
    class function ExportType: Integer; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; override;
  end;

  { TcxExportToXLSProvider }

  TcxExportToXLSProvider = class(TcxExportProvider)
  public
    class function ExportName: string; override;
    class function ExportType: Integer; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; override;
  end;

  { TcxExportToXLSXProvider }

  TcxExportToXLSXProvider = class(TcxExportProvider)
  public
    class function ExportName: string; override;
    class function ExportType: Integer; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; override;
  end;

  { TcxExportToXMLProvider }

  TcxExportToXMLProvider = class(TcxExportProvider)
  public
    constructor Create(const AFileName: string); override;
    class function ExportName: string; override;
    class function ExportType: Integer; override;
    function GetFormat: TdxSpreadSheetCustomFormatClass; override;
  end;

  { TcxPivotGridDataExportHelper }

  TdxCustomDataExport = class
  strict private
    FControl: TObject;
    FFileFormat: TdxSpreadSheetCustomFormatClass;
    FFileName: string;
    FHandler: TObject;
    FProgressHelper: TcxProgressCalculationHelper;
    FProgressValue: Integer;
    FSpreadSheet: TdxCustomSpreadSheet;
    FView: TdxSpreadSheetTableView;

    procedure ProgressHandler(ASender: TObject; AProgressValue: Integer);
  protected
    function AddCellValue(ARow, AColumn: Integer; const AValue: Variant): TdxSpreadSheetCell;
    procedure AddFrame(const AArea: TRect); overload; inline;
    procedure AddFrame(ARow, AColumn: Integer); overload; inline;
    procedure AddGroupBy(AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer; AExpanded: Boolean);
    procedure AddGroupByColumns(AStartIndex, AFinishIndex: Integer; AExpanded: Boolean = True);
    procedure AddGroupByRows(AStartIndex, AFinishIndex: Integer; AExpanded: Boolean = True);
    function PrepareDisplayFormat(ACell: TdxSpreadSheetCell; const ADisplayFormat: string): string; inline;
    procedure RemoveInnerBorders(const AArea: TRect); inline;
    procedure SetBorders(const AArea: TRect; AInnerStyle, AOuterStyle: TdxSpreadSheetCellBorderStyle);
    procedure SetDisplayFormat(ACell: TdxSpreadSheetCell; const ADisplayFormat: string); overload;
    procedure SetDisplayFormat(ARow, AColumn: Integer; const ADisplayFormat: string); overload;
    //
    procedure AfterExport; virtual;
    procedure BeforeExport; virtual;
    function CreateSpreadSheet: TdxCustomSpreadSheet; virtual;
    procedure DoExport; virtual;
    procedure ExecuteTask(AProc: TThreadMethod);
    function GetStageCount: Integer; virtual;
    //
    property Control: TObject read FControl;
    property FileFormat: TdxSpreadSheetCustomFormatClass read FFileFormat;
    property ProgressValue: Integer read FProgressValue write FProgressValue;
  public
    constructor Create(AControl: TObject; const AFileName: string; AHandler: TObject); virtual;
    procedure Export;

    property FileName: string read FFileName;
    property Handler: TObject read FHandler;
    property ProgressHelper: TcxProgressCalculationHelper read FProgressHelper;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
    property View: TdxSpreadSheetTableView read FView;
  end;

implementation

uses
  Math, StrUtils, DateUtils, cxExportStrs, dxSpreadSheetFormatXLSX, dxGDIPlusClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetNumberFormat,
  dxSpreadSheetStyles, dxDPIAwareUtils;

type
  TdxSpreadSheetAccess = class(TdxSpreadSheet);
  TdxSpreadSheetPictureAccess = class(TdxSpreadSheetPicture);
  TdxSpreadSheetTableItemAccess = class(TdxSpreadSheetTableItem);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

  { TdxSpreadSheetCellStyleHelper }

  TdxSpreadSheetCellStyleHelper = class
  public
    class procedure SetDataFormat(ACell: TdxSpreadSheetCell; const AFormatCode: string); overload;
    class procedure SetDataFormat(ACell: TdxSpreadSheetCell; const AFormatCodeID: Integer); overload;
  end;

{ TdxSpreadSheetBasedExporter }

procedure TdxSpreadSheetBasedExporter.BeginGenerate;
begin
  Inc(FGenerateLockCount);
  BeginUpdate;
end;

procedure TdxSpreadSheetBasedExporter.EndGenerate;
begin
  AddChanges([sscData, sscLayout, sscStyle]);
  EndUpdate;
  Dec(FGenerateLockCount);
end;

constructor TdxSpreadSheetBasedExporter.Create(AOwner: TComponent);
begin
  inherited;
  Include(FState, sssExporting);
end;

function TdxSpreadSheetBasedExporter.DoCreateSheet(var ASheet: TdxSpreadSheetCustomView;
  const ACaption: string; AViewClass: TdxSpreadSheetCustomViewClass): Boolean;
begin
  if AViewClass = nil then
    AViewClass := TdxSpreadSheetBasedExporterTableView;
  Result := inherited DoCreateSheet(ASheet, ACaption, AViewClass);
end;

{ TdxSpreadSheetBasedExporterCell }

procedure TdxSpreadSheetBasedExporterCell.Changed(AChanges: TdxSpreadSheetChanges);
begin
  // do nothing
end;

procedure TdxSpreadSheetBasedExporterCell.CheckDefaultCellFormat;
begin
  if FDataType = cdtCurrency then
    TdxSpreadSheetCellStyleHelper.SetDataFormat(Self, 7);
end;

{ TdxSpreadSheetBasedExporterTableRow }

function TdxSpreadSheetBasedExporterTableRow.CreateRowCells: TdxSpreadSheetTableRowCells;
begin
  Result := TdxSpreadSheetBasedExporterTableRowCells.Create(Self);
end;

procedure TdxSpreadSheetBasedExporterTableRow.DoCellCreated(ACell: TdxSpreadSheetCell);
begin
  Changed;
end;

{ TdxSpreadSheetBasedExporterTableRowCells }

function TdxSpreadSheetBasedExporterTableRowCells.GetItemClass: TdxDynamicListItemClass;
begin
  Result := TdxSpreadSheetBasedExporterCell;
end;

{ TdxSpreadSheetBasedExporterTableRows }

function TdxSpreadSheetBasedExporterTableRows.GetItemClass: TdxDynamicListItemClass;
begin
  Result := TdxSpreadSheetBasedExporterTableRow;
end;

{ TdxSpreadSheetBasedExporterTableView }

function TdxSpreadSheetBasedExporterTableView.CreateRows: TdxSpreadSheetTableRows;
begin
  Result := TdxSpreadSheetBasedExporterTableRows.Create(Self);
end;

function TdxSpreadSheetBasedExporterTableView.CreateViewInfo: TdxSpreadSheetCustomViewViewInfo;
begin
  Result := TdxSpreadSheetBasedExporterTableViewInfo.Create(Self);
end;

procedure TdxSpreadSheetBasedExporterTableView.Pack;
begin
  if TdxSpreadSheetBasedExporter(SpreadSheet).FGenerateLockCount = 0 then
    inherited Pack;
end;

{ TdxSpreadSheetBasedExporterTableViewInfo }

procedure TdxSpreadSheetBasedExporterTableViewInfo.CalculateCells;
begin
  // do nothing
end;

{ TcxExportProvider }

constructor TcxExportProvider.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FDefaultAlignVert := ssavTop;
  FStyles := TList<TcxCacheCellStyle>.Create;
  FSpreadSheet := TdxSpreadSheetBasedExporter.Create(nil);
  FSpreadSheet.OnProgress := ProgressHandler;
  FSpreadSheet.BeginGenerate;
  FTableView := FSpreadSheet.ActiveSheetAsTable;
  FProxyCell := TdxSpreadSheetCellAccess(FTableView.CreateCell(0, 0));
  FSharedImagesMap := TObjectDictionary<TObject, TcxExportSharedImagesInfo>.Create([doOwnsValues], 0);
end;

destructor TcxExportProvider.Destroy;
begin
  FreeAndNil(FSharedImagesMap);
  FreeAndNil(FSpreadSheet);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

procedure TcxExportProvider.ApplyBestFit;

  procedure Prepare(AItems: TdxSpreadSheetTableItems; AInitialSize: Integer);
  var
    AItem: TdxSpreadSheetTableItemAccess;
  begin
    AItem := TdxSpreadSheetTableItemAccess(AItems.First);
    while AItem <> nil do
    begin
      if AItem.Visible then
      begin
        AItem.FSize := AInitialSize;
        AItem.FDefaultSize := True;
        AItem.FIsCustomSize := False;
      end;
      AItem := TdxSpreadSheetTableItemAccess(AItem.Next);
    end;
  end;

  procedure Unprepare(AItems: TdxSpreadSheetTableItems);
  var
    AItem: TdxSpreadSheetTableItemAccess;
  begin
    AItem := TdxSpreadSheetTableItemAccess(AItems.First);
    while AItem <> nil do
    begin
      if AItem.Visible then
      begin
        if AItem.FSize = 0 then
          AItem.FSize := AItem.Owner.DefaultSize;
        AItem.FIsCustomSize := False;
        AItem.FDefaultSize := False;
      end;
      AItem := TdxSpreadSheetTableItemAccess(AItem.Next);
    end;
  end;

var
  ACanvas: TcxCanvas;
  ACell: TdxSpreadSheetBasedExporterCell;
  AColumn: TdxSpreadSheetTableItemAccess;
  AHeight: Integer;
  APrevDPI: Integer;
  ARow: TdxSpreadSheetTableItemAccess;
  AWidth: Integer;
begin
  ACanvas := cxScreenCanvas;
  Prepare(TableView.Columns, 0);
  Prepare(TableView.Rows, TableView.Rows.DefaultSize);
  APrevDPI := dxSpreadSheetPrepareCanvas(ACanvas, TdxSpreadSheetAccess(TableView.SpreadSheet).Font, dxDefaultDPI);
  try
    ARow := TdxSpreadSheetTableItemAccess(TableView.Rows.First);
    while ARow <> nil do
    begin
      if ARow.Visible then
      begin
        ACell := TdxSpreadSheetBasedExporterCell(TdxSpreadSheetTableRowAccess(ARow).RowCells.First);
        while ACell <> nil do
        begin
          dxSpreadSheetTextService.MeasureSize(ACanvas, ACell, @AWidth, @AHeight);
          AColumn := TdxSpreadSheetTableItemAccess(ACell.Column);
          AColumn.FSize := Max(AColumn.FSize, AWidth);
          ARow.FSize := Max(ARow.FSize, AHeight);
          ACell.ReleaseDisplayValue;
          ACell := TdxSpreadSheetBasedExporterCell(ACell.FNext);
        end;
      end;
      ARow := TdxSpreadSheetTableItemAccess(ARow.Next);
    end;
  finally
    dxSpreadSheetUnprepareCanvas(ACanvas, APrevDPI);
    Unprepare(TableView.Columns);
    Unprepare(TableView.Rows);
  end;
end;

procedure TcxExportProvider.Clear;
var
  I: Integer;
begin
  inherited Clear;
  FSharedImagesMap.Clear;
  FTableView.BeginUpdate;
  try
    FTableView.Containers.Clear;
    FTableView.DeleteAllCells;
    for I := 0 to FStyles.Count - 1 do
      TdxSpreadSheetCellStyleHandle(FStyles.Items[I].Reference).Release;
    FStyles.Clear;
  finally
    FTableView.EndUpdate;
  end;
end;

function TcxExportProvider.AddPictureContainer(
  const AArea: TRect; AFitMode: TcxImageFitMode; const ASize: TSize): TdxSpreadSheetPictureContainer;
begin
  TableView.Containers.Add(TdxSpreadSheetPictureContainer, Result);
  SetupPictureContainer(Result, AArea, AFitMode, ASize);
end;

procedure TcxExportProvider.AddSharedImageHandle(AImageList: TObject; AImageIndex: Integer; AHandle: TObject);
var
  ASharedInfo: TcxExportSharedImagesInfo;
begin
  if not FSharedImagesMap.TryGetValue(AImageList, ASharedInfo) then
  begin
    ASharedInfo := TcxExportSharedImagesInfo.Create();
    FSharedImagesMap.Add(AImageList, ASharedInfo);
  end;
  ASharedInfo.AddOrSetValue(AImageIndex, AHandle);
end;

procedure TcxExportProvider.AddConditionalFormattingRule(ARule: TdxSpreadSheetCustomConditionalFormattingRule; AAreas: TdxRectList);
var
  ATargetRule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  TableView.ConditionalFormatting.Add(TdxSpreadSheetConditionalFormattingCustomRuleClass(ARule.ClassType), ATargetRule);
  ATargetRule.Assign(ARule);
  ATargetRule.Areas.Assign(AAreas);
end;

procedure TcxExportProvider.AddOutlineGroup(AStart, AFinish: Integer);
begin
  TableView.Rows.Groups.ExpandButtonPosition := gebpGroupStart;
  TableView.Rows.Groups.Add(AStart, AFinish);
end;

function TcxExportProvider.AddContainer(AItem: TcxExportDataItem; AFitMode: TcxImageFitMode): TdxSpreadSheetPictureContainer;
begin
  AItem.DataType := $FF - Byte(AFitMode);
  TableView.Containers.Add(TdxSpreadSheetPictureContainer, Result);
  PObject(@AItem.Value)^ := Result;
end;

procedure TcxExportProvider.AddGraphic(AItem: TcxExportDataItem; AGraphic: TGraphic; AFitMode: TcxImageFitMode);
var
  AContainer: TdxSpreadSheetPictureContainer;
  ASmartImage: TdxSmartImage;
begin
  if not SupportGraphic then
  begin
    AddText(AItem, 'GRAPHIC');
    Exit;
  end;
  AContainer := AddContainer(AItem, AFitMode);
  if AGraphic is TdxSmartImage then
    AContainer.Picture.Image := TdxSmartImage(AGraphic)
  else
  begin
    ASmartImage := TdxSmartImage.Create;
    try
      ASmartImage.Assign(AGraphic);
      AContainer.Picture.Image := ASmartImage;
    finally
      ASmartImage.Free;
    end;
  end;
end;

procedure TcxExportProvider.AddImageListItem(AItem: TcxExportDataItem;
  AImages: TCustomImageList; AImageIndex: Integer; ABkColor: TColor);
var
  R: TRect;
  AHandle: TdxPNGImage;
  ABitmap: TcxBitmap;
  AContainer: TdxSpreadSheetPictureContainer;
begin
  if not SupportGraphic then
  begin
    AddText(AItem, 'GRAPHIC');
    Exit;
  end;
  AContainer := AddContainer(AItem, ifmNormal);
  if not TryGetSharedImageHandle(AImages, AImageIndex, TObject(AHandle)) then
  begin
    ABitmap := TcxBitmap.CreateSize(AImages.Width, AImages.Height, pf24bit);
    try
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ABkColor);
      AImages.Draw(ABitmap.Canvas, 0, 0, AImageIndex);
      R := cxRectBounds(0, 0, ABitmap.Width, ABitmap.Height);
      AHandle := GraphicToPNGImage(R, R, ABitmap);
      try
        AContainer.Picture.Image := AHandle;
      finally
        AHandle.Free;
      end;
      AddSharedImageHandle(AImages, AImageIndex, TdxSpreadSheetPictureAccess(AContainer.Picture).ImageHandle);
    finally
      ABitmap.Free;
    end;
  end
  else
    TdxSpreadSheetPictureAccess(AContainer.Picture).ImageHandle := TdxSpreadSheetSharedImageHandle(AHandle);
end;

procedure TcxExportProvider.AddText(AItem: TcxExportDataItem; const AText: string);
begin
  AItem.DataType := 0;
  if AText = '' then
    Exit;
  FProxyCell.AsString := AText;
  PObject(@AItem.Value)^ := PObject(@FProxyCell.FData)^;
  AItem.DataType := Byte(cdtString);
  FProxyCell.FDataType := cdtBlank;
end;

procedure TcxExportProvider.AddValue(
  AItem: TcxExportDataItem; const AValue: Variant; AProperties: TcxCustomEditProperties;
  const AValueDisplayFormat: string = ''; AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto);
var
  R: TRect;
  AText: string;
  APicture: TPicture;
  AImageIndex: TcxImageIndex;
  AContainer: TdxSpreadSheetPictureContainer;
begin
  AItem.DataType := 0;
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Exit;
  if AProperties is TcxImageComboBoxProperties then
  begin
    TcxImageComboBoxProperties(AProperties).GetImageComboBoxDisplayValue(AValue, AText, AImageIndex);
    if TcxImageComboBoxProperties(AProperties).ShowDescriptions or (TcxImageComboBoxProperties(AProperties).Images = nil) or
      (AImageIndex < 0) or (AImageIndex >= TcxImageComboBoxProperties(AProperties).Images.Count) then
      AddText(AItem, AText)
    else
      AddImageListItem(AItem, TcxImageComboBoxProperties(AProperties).Images, AImageIndex, GetStyle(AItem.Style).BrushBkColor);
    Exit;
  end
  else
    if AProperties is TcxImageProperties then
    begin
      R := cxRectSetOrigin(AItem.Bounds, cxNullPoint);
      APicture := TPicture.Create;
      try
        LoadPicture(APicture, TdxSmartImage, AValue);
        if (APicture.Graphic <> nil) and not cxRectIsEmpty(R) then
        begin
          AContainer := AddContainer(AItem, TcxImageProperties(AProperties).FitMode);
          AContainer.Picture.Image := TdxSmartImage(APicture.Graphic);
        end;
      finally
        APicture.Free;
      end;
      Exit;
    end;

  SetCellValue(0, 0, AValue, AValueDisplayFormat, AValueDisplayFormatType);
  AItem.DataType := Byte(FProxyCell.FDataType);
  PFloat(@AItem.Value)^ := PFloat(@FProxyCell.FData)^;
  if FProxyCell.Style.Handle.DataFormat <> SpreadSheet.DefaultCellStyle.Handle.DataFormat then
  begin
    AItem.DataFormat := FProxyCell.Style.Handle.DataFormat;
    FProxyCell.Style.Handle.DataFormat.AddRef;
  end;
  FProxyCell.StyleHandle := SpreadSheet.DefaultCellStyle.Handle;
  FProxyCell.FDataType := cdtBlank;
end;

procedure TcxExportProvider.Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
var
  AFileStream: TFileStream;
  AIntf: IcxExportBeforeSave;
begin
  if Supports(AHandler, IcxExportBeforeSave, AIntf) then
    AIntf.OnBeforeSave(FSpreadSheet);
  FSpreadSheet.EndGenerate;

  FProgressHelper := AProgressHelper;
  FProgressHelper.BeginStage(100);
  try
    AFileStream := TFileStream.Create(FileName, fmCreate);
    try
      FSpreadSheet.SaveToStream(AFileStream, GetFormat);
    finally
      AFileStream.Free;
    end;
  finally
    FProgressHelper.EndStage;
    FProgressHelper := nil;
  end;
end;

procedure TcxExportProvider.FreezePanes(ACol, ARow: Integer);
begin
  TableView.FreezePanes(ARow, ACol);
end;

function TcxExportProvider.GetFormatCode(ACol, ARow: Integer): string;
begin
  Result := TableView.Cells[ARow, ACol].StyleHandle.DataFormat.FormatCode;
end;

function TcxExportProvider.GetStyle(AStyleIndex: Integer): TcxCacheCellStyle;
begin
  Result := FStyles.Items[AStyleIndex];
end;

function TcxExportProvider.RegisterStyle(const AStyle: TcxCacheCellStyle): Integer;
var
  ACacheStyle: TcxCacheCellStyle;
  ACellStyle: TdxSpreadSheetCellStyleHandle;
begin
  ACellStyle := CreateStyle(AStyle);
  Result := GetStyleIndex(ACellStyle);
  if Result < 0 then
  begin
    ACellStyle.AddRef;
    ACacheStyle := AStyle;
    ACacheStyle.Reference := ACellStyle;
    Result := FStyles.Add(ACacheStyle);
  end;
end;

function TcxExportProvider.SetCellGraphic(const AArea: TRect; AGraphic: TGraphic; AFitMode: TcxImageFitMode): TObject;
var
  AContainer: TdxSpreadSheetPictureContainer;
  ASmartImage: TdxSmartImage;
begin
  try
    AContainer := AddPictureContainer(AArea, AFitMode, cxSize(AGraphic.Width, AGraphic.Height));

    if AGraphic is TdxSmartImage then
      AContainer.Picture.Image := TdxSmartImage(AGraphic)
    else
    begin
      ASmartImage := TdxSmartImage.Create;
      try
        ASmartImage.Assign(AGraphic);
        AContainer.Picture.Image := ASmartImage;
      finally
        ASmartImage.Free;
      end;
    end;
    Result := TdxSpreadSheetPictureAccess(AContainer.Picture).ImageHandle;
  finally
    AGraphic.Free;
  end;
end;

function TcxExportProvider.SetCellGraphic(const AArea: TRect; AStyleIndex: Integer;
  AGraphic: TGraphic; AFitMode: TcxImageFitMode): TObject;
begin
  SetCellStyle(AArea, AStyleIndex);
  Result := SetCellGraphic(AArea, AGraphic, AFitMode);
end;

procedure TcxExportProvider.SetCellGraphicAsSharedHandle(const AArea: TRect; AStyleIndex: Integer;
  AHandle: TObject; AFitMode: TcxImageFitMode);
var
  AContainer: TdxSpreadSheetPictureContainer;
begin
  SetCellStyle(AArea, AStyleIndex);
  AContainer := AddPictureContainer(AArea, AFitMode,  TdxSpreadSheetSharedImageHandle(AHandle).Image.Size);
  TdxSpreadSheetPictureAccess(AContainer.Picture).ImageHandle := TdxSpreadSheetSharedImageHandle(AHandle);
end;

procedure TcxExportProvider.SetCellPNGImage(const ACol, ARow: Integer; APNGImage: TGraphic; AFitMode: TcxImageFitMode);
begin
  SetCellGraphic(cxRectBounds(ACol, ARow, 1, 1), APNGImage, AFitMode);
end;

procedure TcxExportProvider.SetCellStyle(const AArea: TRect; AStyleIndex: Integer);
var
  C, R: Integer;
begin
  SetCellUnion(AArea.Left, AArea.Top, cxRectHeight(AArea), cxRectWidth(AArea));
  for C := AArea.Left to AArea.Right - 1 do
  begin
    for R := AArea.Top to AArea.Bottom - 1 do
      SetCellStyle(C, R, AStyleIndex);
  end;
end;

procedure TcxExportProvider.SetCellStyle(const ACol, ARow, AStyleIndex: Integer);
var
  ACell: TdxSpreadSheetCell;
  AOriginalFormatCode: string;
  AStyle: TcxCacheCellStyle;
begin
{$IF DEFINED(DELPHIXE3) AND NOT DEFINED(DEBUG)}
  AStyle := FStyles.List[AStyleIndex];
{$ELSE}
  AStyle := FStyles.Items[AStyleIndex];
{$IFEND}
  ACell := TableView.CreateCell(ARow, ACol);
  AOriginalFormatCode := ACell.StyleHandle.DataFormat.FormatCode;
  ACell.StyleHandle := TdxSpreadSheetCellStyleHandle(AStyle.Reference);
  TdxSpreadSheetCellStyleHelper.SetDataFormat(ACell, AOriginalFormatCode);
end;

procedure TcxExportProvider.SetCellUnion(const ACol, ARow: Integer; H, W: Integer);
begin
  if (H > 1) or (W > 1) then
    TableView.MergedCells.Add(cxRectBounds(ACol, ARow, Max(0, W - 1), Max(0, H - 1)));
end;

procedure TcxExportProvider.SetCellValue(const ACol, ARow: Integer; const AValue: Variant;
  const AValueDisplayFormat: string = ''; AValueDisplayFormatType: TcxValueDisplayFormatType = vdftAuto);
var
  ACell: TdxSpreadSheetCell;
  ACellFormatCode: string;
  ACurr: Currency;
begin
  if AValueDisplayFormatType = vdftRTF then
    if dxSpreadSheetTextService.ForceSetAsRTF(TableView.CreateCell(ARow, ACol), dxVariantToString(AValue)) then
      Exit;

  if VarIsStr(AValue) then
  begin
    SetCellValueAsString(ACol, ARow, AValue);
    Exit;
  end;

  ACell := TableView.CreateCell(ARow, ACol);
{$IFNDEF NONDB}
  if TVarData(AValue).VType = VarSQLTimeStamp then
    ACell.AsDateTime := AValue
  else

  if TVarData(AValue).VType = VarFMTBcd then
  begin
    if BcdToCurr(VarToBcd(AValue), ACurr) then
      ACell.AsCurrency := ACurr
    else
      ACell.AsFloat := BcdToDouble(VarToBcd(AValue));
  end
  else
{$ENDIF}
    ACell.AsVariant := AValue;

  if AValueDisplayFormatType = vdftAuto then
  begin
    if ACell.DataType = cdtDateTime then
      AValueDisplayFormatType := vdftDateTime
    else
      AValueDisplayFormatType := vdftNumeric;
  end;

  if AValueDisplayFormat <> '' then
  begin
    if AValueDisplayFormatType = vdftDateTime then
      ACellFormatCode := TdxSpreadSheetDisplayFormatConverter.ConvertDateTimeDisplayFormat(AValueDisplayFormat, ACell.AsDateTime)
    else
      ACellFormatCode := TdxSpreadSheetDisplayFormatConverter.ConvertFloatValueDisplayFormat(AValueDisplayFormat);
  end
  else
    if AValueDisplayFormatType = vdftDateTime then
      ACellFormatCode := TdxSpreadSheetDateTimeFormatHelper.GetDefaultFormat(ACell.AsDateTime)
    else
      ACellFormatCode := '';

  TdxSpreadSheetCellStyleHelper.SetDataFormat(ACell, ACellFormatCode);
end;

procedure TcxExportProvider.SetCellValueAsFormula(const ACol, ARow: Integer;
  const AValue: string; ADisplayText: string = ''; AFormatCode: string = ''; AListSeparator: Char = ',');
var
  ACell: TdxSpreadSheetCell;
  AFormula: string;
begin
  AFormula := StringReplace(AValue, AListSeparator,
    SpreadSheet.FormulaController.FormatSettings.Data.ListSeparator, [rfReplaceAll, rfIgnoreCase]);

  ACell := TableView.CreateCell(ARow, ACol);
  try
    ACell.SetText(AFormula, True);
    TdxSpreadSheetCellStyleHelper.SetDataFormat(ACell, AFormatCode);
  except
    on EdxSpreadSheetFormulaIsTooLongError do
      ACell.SetText(ADisplayText);
  end;
end;

procedure TcxExportProvider.SetCellValueAsString(const ACol, ARow: Integer; const AText: string);
begin
  TableView.CreateCell(ARow, ACol).AsString := AText;
end;

procedure TcxExportProvider.SetColumnWidth(const ACol, AWidth: Integer);
begin
  TableView.Columns.CreateItem(ACol).Size := AWidth;
end;

procedure TcxExportProvider.SetDefaultStyle(const AStyle: TcxCacheCellStyle);
begin
  SpreadSheet.DefaultCellStyle.Handle := CreateStyle(AStyle);
end;

procedure TcxExportProvider.SetData(AItem: TcxExportDataItem);
var
  ACell: TdxSpreadSheetCellAccess;
  AContainer: TdxSpreadSheetPictureContainer;
  AHandle: TdxSpreadSheetCellStyleHandle;
begin
  if cxRectIsEmpty(AItem.Bounds) then
   Exit;

  ACell := TdxSpreadSheetCellAccess(SpreadSheet.ActiveSheetAsTable.CreateCell(AItem.Bounds.Top, AItem.Bounds.Left));
  if AItem.DataType <> 0 then
  begin
    if AItem.DataType > Byte(High(TdxSpreadSheetCellDataType)) then
    begin
      AContainer := TdxSpreadSheetPictureContainer(PObject(@AItem.Value)^);
      SetupPictureContainer(AContainer, AItem.Bounds, TcxImageFitMode($FF - AItem.DataType),
        TdxSpreadSheetPictureAccess(AContainer.Picture).ImageHandle.Image.Size);
    end
    else
    begin
      ACell.FDataType := TdxSpreadSheetCellDataType(AItem.DataType);
      PFloat(@ACell.FData)^ := PFloat(@AItem.Value)^;
    end;
  end
  else
    ACell.Clear;
  if AItem.DataFormat <> nil then
  begin
    AHandle := ACell.StyleHandle.Clone;
    AHandle.DataFormat := TdxSpreadSheetFormatHandle(AItem.DataFormat);
    ACell.StyleHandle := SpreadSheet.CellStyles.AddStyle(AHandle);
  end;
  SetCellStyle(AItem.Bounds, AItem.Style);
end;

procedure TcxExportProvider.SetRange(const AColCount, ARowCount: Integer; IsVisible: Boolean);
begin
  // do nothing
end;

procedure TcxExportProvider.SetRowHeight(const ARow, AHeight: Integer);
begin
  TableView.Rows.CreateItem(ARow).Size := AHeight;
end;

function TcxExportProvider.SupportGraphic: Boolean;
begin
  Result := True;
end;

function TcxExportProvider.SupportRTF: Boolean;
begin
  Result := True;
end;

function TcxExportProvider.TryGetSharedImageHandle(
  AImageList: TObject; AImageIndex: Integer; var AHandle: TObject): Boolean;
var
  ASharedInfo: TcxExportSharedImagesInfo;
begin
  Result := FSharedImagesMap.TryGetValue(AImageList, ASharedInfo);
  if Result then
    Result := ASharedInfo.TryGetValue(AImageIndex, AHandle);
end;

procedure TcxExportProvider.SetName(const AName: string);
begin
  TableView.Caption := AName;
end;

procedure TcxExportProvider.SetRangeName(const AName: string; const ARange: TRect);
begin
  SpreadSheet.DefinedNames.Add(AName, dxReferenceToString(ARange, False), TableView);
end;

function TcxExportProvider.CreateStyle(const AStyle: TcxCacheCellStyle): TdxSpreadSheetCellStyleHandle;
const
  AlignTextMap: array [TcxAlignText] of TdxSpreadSheetDataAlignHorz = (ssahLeft, ssahCenter, ssahRight);
  AlignTextVertMap: array [TcxAlignTextVert] of TdxSpreadSheetDataAlignVert = (ssavTop, ssavTop, ssavCenter, ssavBottom);

var
  ABordersHandle: TdxSpreadSheetBordersHandle;
  ABorderSide: TcxBorder;
  ABrushHandle: TdxSpreadSheetBrushHandle;
  AFontHandle: TdxSpreadSheetFontHandle;
begin
  ABordersHandle := SpreadSheet.CellStyles.Borders.CreateBorders;
  for ABorderSide := Low(ABorderSide) to High(ABorderSide) do
  begin
    if AStyle.Borders[Ord(ABorderSide)].IsDefault then
      ABordersHandle.BorderStyle[ABorderSide] := sscbsDefault
    else
    begin
      ABordersHandle.BorderColor[ABorderSide] := AStyle.Borders[Ord(ABorderSide)].Color;
      ABordersHandle.BorderStyle[ABorderSide] := GetBorderStyleByWidth(AStyle.Borders[Ord(ABorderSide)].Width);
    end;
  end;
  ABordersHandle := SpreadSheet.CellStyles.Borders.AddBorders(ABordersHandle);

  ABrushHandle := SpreadSheet.CellStyles.Brushes.CreateBrush;
  if cxColorIsValid(AStyle.BrushBkColor) then
    ABrushHandle.BackgroundColor := AStyle.BrushBkColor;
  if cxColorIsValid(AStyle.BrushFgColor) then
    ABrushHandle.ForegroundColor := AStyle.BrushFgColor;
  ABrushHandle.Style := sscfsSolid;
  ABrushHandle := SpreadSheet.CellStyles.Brushes.AddBrush(ABrushHandle);

  AFontHandle := SpreadSheet.CellStyles.Fonts.CreateFont;
  AFontHandle.Charset := AStyle.FontCharset;
  AFontHandle.Color := AStyle.FontColor;
  AFontHandle.Name := AStyle.FontName;
  AFontHandle.Size := AStyle.FontSize;
  AFontHandle.Style := AStyle.FontStyle;
  AFontHandle := SpreadSheet.CellStyles.Fonts.AddFont(AFontHandle);

  Result := SpreadSheet.CellStyles.CreateStyle(AFontHandle, nil, ABrushHandle, ABordersHandle);
  Result.AlignHorz := AlignTextMap[AStyle.AlignText];

  if AStyle.AlignTextVert = atvDefault then
    Result.AlignVert := FDefaultAlignVert
  else
    Result.AlignVert := AlignTextVertMap[AStyle.AlignTextVert];

  if AStyle.SingleLine then
    Result.States := Result.States - [csWordWrap]
  else
    Result.States := Result.States + [csWordWrap];

  Result := SpreadSheet.CellStyles.AddStyle(Result);
end;

function TcxExportProvider.GetBorderStyleByWidth(AWidth: Integer): TdxSpreadSheetCellBorderStyle;
begin
  case AWidth of
    0: Result := sscbsNone;
    2: Result := sscbsMedium;
    3: Result := sscbsThick;
  else
    Result :=  sscbsThin;
  end;
end;

function TcxExportProvider.GetStyleIndex(AHandle: TdxSpreadSheetCellStyleHandle): Integer;
var
  I: Integer;
begin
  for I := 0 to FStyles.Count - 1 do
  begin
    if FStyles[I].Reference = AHandle then
      Exit(I);
  end;
  Result := -1;
end;

procedure TcxExportProvider.ProgressHandler(Sender: TObject; Percent: Integer);
begin
  FProgressHelper.SetTaskNumber(Percent);
end;

procedure TcxExportProvider.SetupPictureContainer(AContainer: TdxSpreadSheetPictureContainer;
  const AArea: TRect; AFitMode: TcxImageFitMode; const ASize: TSize);
var
  R, ABounds: TRect;
begin
  AContainer.AnchorPoint1.Cell := TableView.CreateCell(AArea.Top, AArea.Left);
  AContainer.AnchorPoint1.FixedToCell := True;

  if AFitMode = ifmNormal then
  begin
    ABounds := Rect(TableView.Columns.GetPosition(AArea.Left), TableView.Rows.GetPosition(AArea.Top),
      TableView.Columns.GetPosition(AArea.Right), TableView.Rows.GetPosition(AArea.Bottom));
    R := cxRectCenter(ABounds, ASize.cx, ASize.cy);
    AContainer.AnchorPoint1.Offset := cxPointOffset(cxPointOffset(ABounds.TopLeft, R.TopLeft, False), 1, 1);
    AContainer.AnchorPoint2.Offset := cxPoint(ASize);
    AContainer.AnchorType := catOneCell;
  end
  else
  begin
    AContainer.AnchorPoint2.Cell := TableView.CreateCell(AArea.Bottom, AArea.Right);
    AContainer.AnchorPoint2.FixedToCell := True;
    AContainer.AnchorType := catTwoCell;
  end;
end;

{ TcxExportToCSVProvider }

class function TcxExportToCSVProvider.ExportName: string;
begin
  Result := cxGetResourceString(@scxExportToCSV);
end;

class function TcxExportToCSVProvider.ExportType: Integer;
begin
  Result := cxExportToCSV;
end;

function TcxExportToCSVProvider.GetFormat: TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetCSVFormat;
end;

procedure TcxExportToCSVProvider.Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
var
  APrevEncoding: TEncoding;
  APrevSeparator: Char;
begin
  APrevEncoding := dxSpreadSheetCSVFormatSettings.Encoding;
  APrevSeparator := dxSpreadSheetCSVFormatSettings.ValueSeparator;
  try
    if FSeparator <> #0 then
      dxSpreadSheetCSVFormatSettings.ValueSeparator := FSeparator;
    if FEncoding <> nil then
      dxSpreadSheetCSVFormatSettings.Encoding := FEncoding;
    inherited Commit(AProgressHelper, AHandler);
  finally
    dxSpreadSheetCSVFormatSettings.ValueSeparator := APrevSeparator;
    dxSpreadSheetCSVFormatSettings.Encoding := APrevEncoding;
  end;
end;

procedure TcxExportToCSVProvider.AddSeparator(const ASeparator: string);
begin
  if (FSeparatorIndex = 0) and (ASeparator <> '') then
    FSeparator := ASeparator[1];
  Inc(FSeparatorIndex);
end;

procedure TcxExportToCSVProvider.SetEncoding(AEncoding: TEncoding);
begin
  FEncoding := AEncoding;
end;

function TcxExportToCSVProvider.SupportRTF: Boolean;
begin
  Result := False;
end;

{ TcxExportToHTMLProvider }

constructor TcxExportToHTMLProvider.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FDefaultAlignVert := ssavCenter;
end;

class function TcxExportToHTMLProvider.ExportName: string;
begin
  Result := cxGetResourceString(@scxExportToHTML);
end;

class function TcxExportToHTMLProvider.ExportType: Integer;
begin
  Result := cxExportToHTML;
end;

function TcxExportToHTMLProvider.GetFormat: TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetHTMLFormat;
end;

procedure TcxExportToHTMLProvider.Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
var
  APrevCellAutoHeight: Boolean;
begin
  APrevCellAutoHeight := TdxSpreadSheetHTMLFormat.CellAutoHeight;
  try
    TdxSpreadSheetHTMLFormat.CellAutoHeight := True;
    inherited Commit(AProgressHelper, AHandler);
  finally
    TdxSpreadSheetHTMLFormat.CellAutoHeight := APrevCellAutoHeight;
  end;
end;

{ TcxExportToTXTProvider }

procedure TcxExportToTXTProvider.AfterConstruction;
begin
  inherited AfterConstruction;
  FFormatSettings := dxSpreadSheetTXTFormatSettings;
end;

class function TcxExportToTXTProvider.ExportName: string;
begin
  Result := cxGetResourceString(@scxExportToText);
end;

class function TcxExportToTXTProvider.ExportType: Integer;
begin
  Result := cxExportToText;
end;

function TcxExportToTXTProvider.GetFormat: TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetTXTFormat;
end;

procedure TcxExportToTXTProvider.Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
var
  APrevFormatSettings: TdxSpreadSheetTXTFormatSettings;
begin
  APrevFormatSettings := dxSpreadSheetTXTFormatSettings;
  try
    dxSpreadSheetTXTFormatSettings := FFormatSettings;
    inherited Commit(AProgressHelper, AHandler);
  finally
    dxSpreadSheetTXTFormatSettings := APrevFormatSettings;
  end;
end;

function TcxExportToTXTProvider.SupportGraphic: Boolean;
begin
  Result := False;
end;

function TcxExportToTXTProvider.SupportRTF: Boolean;
begin
  Result := False;
end;

procedure TcxExportToTXTProvider.AddSeparator(const ASeparator: string);
begin
  case FSeparatorIndex of
    0: FFormatSettings.Separator := ASeparator;
    1: FFormatSettings.BeginString := ASeparator;
    2: FFormatSettings.EndString := ASeparator;
  end;
  Inc(FSeparatorIndex);
end;

procedure TcxExportToTXTProvider.SetEncoding(AEncoding: TEncoding);
begin
  FFormatSettings.Encoding := AEncoding;
end;

{ TcxExportToXLSProvider }

class function TcxExportToXLSProvider.ExportName: string;
begin
  Result := cxGetResourceString(@scxExportToExcel);
end;

class function TcxExportToXLSProvider.ExportType: Integer;
begin
  Result := cxExportToExcel;
end;

function TcxExportToXLSProvider.GetFormat: TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetXLSFormat;
end;

{ TcxExportToXLSXProvider }

class function TcxExportToXLSXProvider.ExportName: string;
begin
  Result := cxGetResourceString(@scxExportToXLSX);
end;

class function TcxExportToXLSXProvider.ExportType: Integer;
begin
  Result := cxExportToXLSX;
end;

function TcxExportToXLSXProvider.GetFormat: TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetXLSXFormat;
end;

{ TcxExportToXMLProvider }

constructor TcxExportToXMLProvider.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FDefaultAlignVert := ssavCenter;
end;

class function TcxExportToXMLProvider.ExportName: string;
begin
  Result := cxGetResourceString(@scxExportToXML);
end;

class function TcxExportToXMLProvider.ExportType: Integer;
begin
  Result := cxExportToXml;
end;

function TcxExportToXMLProvider.GetFormat: TdxSpreadSheetCustomFormatClass;
begin
  Result := TdxSpreadSheetXMLFormat;
end;


{ TdxCustomDataExport }

constructor TdxCustomDataExport.Create(AControl: TObject; const AFileName: string; AHandler: TObject);
begin
  FFileFormat := TdxSpreadSheetXLSXFormat;
  FFileName := AFileName;
  FControl := AControl;
  if SameText(ExtractFileExt(AFileName), '.xls') then
    FFileFormat := TdxSpreadSheetXLSFormat
  else
    FFileName := ChangeFileExt(AFileName, '.xlsx');
  FHandler := AHandler;
  FProgressValue := -1;
end;

procedure TdxCustomDataExport.Export;
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    BeforeExport;
    try
      FProgressHelper := TcxProgressCalculationHelper.Create(GetStageCount + 1, Control, ProgressHandler);
      try
        FSpreadSheet := CreateSpreadSheet;
        try
          FView := TdxSpreadSheetTableView(FSpreadSheet.ActiveSheetAsTable);
          FView.BeginUpdate;
          try
            FView.Caption := cxGetResourceString(@scxDefaultSheetCaption);
            FView.Columns.Groups.ExpandButtonPosition := gebpGroupStart;
            FView.Rows.Groups.ExpandButtonPosition := gebpGroupStart;
            DoExport;
          finally
            FView.EndUpdate;
          end;
          SpreadSheet.SaveToStream(AStream, FileFormat);
        finally
          FreeAndNil(FSpreadSheet);
          FreeAndNil(FProgressHelper);
        end;
      finally
        FreeAndNil(FProgressHelper);
      end;
    finally
      AfterExport;
    end;
  finally
    AStream.Free;
  end;
end;

function TdxCustomDataExport.AddCellValue(ARow, AColumn: Integer; const AValue: Variant): TdxSpreadSheetCell;
begin
  Result := View.CreateCell(ARow, AColumn);
  Result.AsVariant := AValue;
end;

procedure TdxCustomDataExport.AddFrame(const AArea: TRect);
begin
  SetBorders(AArea, sscbsDefault, sscbsThin);
end;

procedure TdxCustomDataExport.AddFrame(ARow, AColumn: Integer);
begin
  AddFrame(Rect(AColumn, ARow, AColumn, ARow))
end;

procedure TdxCustomDataExport.AddGroupBy(AItems: TdxSpreadSheetTableItems;
  AStartIndex, AFinishIndex: Integer; AExpanded: Boolean);
var
  AGroup: TdxSpreadSheetTableItemGroup;
begin
  if (AStartIndex < 0) or (AFinishIndex < AStartIndex) then
    Exit;
  AItems.Groups.Add(AStartIndex, AFinishIndex);
  AGroup := AItems.Groups.Find(AStartIndex);
  if AGroup <> nil then
    AGroup.Expanded := AExpanded;
end;

procedure TdxCustomDataExport.AddGroupByColumns(AStartIndex, AFinishIndex: Integer; AExpanded: Boolean = True);
begin
  AddGroupBy(View.Columns, AStartIndex, AFinishIndex, AExpanded);
end;

procedure TdxCustomDataExport.AddGroupByRows(AStartIndex, AFinishIndex: Integer; AExpanded: Boolean = True);
begin
  AddGroupBy(View.Rows, AStartIndex, AFinishIndex, AExpanded);
end;

function TdxCustomDataExport.PrepareDisplayFormat(ACell: TdxSpreadSheetCell; const ADisplayFormat: string): string;
var
  AValue: TDateTime;
begin
  Result := ADisplayFormat;
  if ACell.DataType = cdtDateTime then
  begin
    AValue := ACell.AsDateTime;
    if (ADisplayFormat = '') then
    begin
      if Trunc(AValue) = 0 then
        Result := dxFormatSettings.ShortTimeFormat
      else
        if Frac(AValue) = 0 then
          Result := dxFormatSettings.ShortDateFormat
        else
          Result := 'C';
    end;
    Result := TdxSpreadSheetDisplayFormatConverter.ConvertDateTimeDisplayFormat(Result, ACell.AsDateTime)
  end
  else
    Result := TdxSpreadSheetDisplayFormatConverter.ConvertFloatValueDisplayFormat(Result);
end;

procedure TdxCustomDataExport.RemoveInnerBorders(const AArea: TRect);
begin
  SetBorders(AArea, sscbsNone, sscbsDefault);
end;

procedure TdxCustomDataExport.SetBorders(const AArea: TRect; AInnerStyle, AOuterStyle: TdxSpreadSheetCellBorderStyle);
var
  AColumn, ARow: Integer;
  AStyleChanged: Boolean;
  ACell: TdxSpreadSheetCell;
  ABordersHandle: TdxSpreadSheetBordersHandle;
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
  AStyles: array[TcxBorder] of TdxSpreadSheetCellBorderStyle;

  procedure CheckBorderStyle(ACondition: Boolean; ABorder: TcxBorder; AStyle: TdxSpreadSheetCellBorderStyle);
  begin
    if not ACondition or (AStyle = sscbsDefault) or (AStyles[ABorder] = AStyle) then Exit;
    AStyleChanged := True;
    AStyles[ABorder] := AStyle;
  end;

begin
  for ARow := AArea.Top to AArea.Bottom do
    for AColumn := AArea.Left to AArea.Right do
    begin
      AStyleChanged := False;
      ACell := View.Cells[ARow, AColumn];
      if ACell <> nil then
        Move(ACell.StyleHandle.Borders.BorderStyle, AStyles, SizeOf(AStyles))
      else
        FillChar(AStyles, SizeOf(AStyles), 0);
      //
      CheckBorderStyle(ARow = AArea.Top, bTop, AOuterStyle);
      CheckBorderStyle(ARow = AArea.Bottom, bBottom, AOuterStyle);
      CheckBorderStyle(AColumn = AArea.Left, bLeft, AOuterStyle);
      CheckBorderStyle(AColumn = AArea.Right, bRight, AOuterStyle);
      //
      CheckBorderStyle(ARow <> AArea.Top, bTop, AInnerStyle);
      CheckBorderStyle(ARow <> AArea.Bottom, bBottom, AInnerStyle);
      CheckBorderStyle(AColumn <> AArea.Left, bLeft, AInnerStyle);
      CheckBorderStyle(AColumn <> AArea.Right, bRight, AInnerStyle);
      //
      if not AStyleChanged then
        Continue;
      if ACell = nil then
        ACell := View.CreateCell(ARow, AColumn);
      //
      AStyleHandle := ACell.StyleHandle.Clone;
      ABordersHandle := AStyleHandle.Borders.Clone;
      Move(AStyles, ABordersHandle.BorderStyle, SizeOf(AStyles));
      AStyleHandle.Borders := SpreadSheet.CellStyles.Borders.AddBorders(ABordersHandle);
      ACell.StyleHandle := SpreadSheet.CellStyles.AddStyle(AStyleHandle);
    end;
end;

procedure TdxCustomDataExport.SetDisplayFormat(ACell: TdxSpreadSheetCell; const ADisplayFormat: string);
var
  AFormat: string;
begin
  AFormat := PrepareDisplayFormat(ACell, ADisplayFormat);
  if AFormat <> '' then
    ACell.Style.DataFormat.FormatCode := AFormat;
end;

procedure TdxCustomDataExport.SetDisplayFormat(ARow, AColumn: Integer; const ADisplayFormat: string);
begin
  SetDisplayFormat(View.CreateCell(ARow, AColumn), ADisplayFormat);
end;

procedure TdxCustomDataExport.AfterExport;
begin
end;

procedure TdxCustomDataExport.BeforeExport;
begin
end;

function TdxCustomDataExport.CreateSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := TdxCustomSpreadSheet.Create(nil);
end;

procedure TdxCustomDataExport.DoExport;
begin
end;

procedure TdxCustomDataExport.ExecuteTask(AProc: TThreadMethod);
begin
  ProgressHelper.BeginStage(1);
  try
    AProc;
  finally
    ProgressHelper.EndStage();
  end;
end;

function TdxCustomDataExport.GetStageCount: Integer;
begin
  Result := 0;
end;

procedure TdxCustomDataExport.ProgressHandler(ASender: TObject; AProgressValue: Integer);
var
  AIntf: IcxExportProgress;
begin
  if AProgressValue <> FProgressValue then
  begin
    FProgressValue := AProgressValue;
    if Supports(FHandler, IcxExportProgress, AIntf) then
      AIntf.OnProgress(ASender, FProgressValue);
  end;
end;

{ TdxSpreadSheetCellStyleHelper }

class procedure TdxSpreadSheetCellStyleHelper.SetDataFormat(ACell: TdxSpreadSheetCell; const AFormatCode: string);
var
  AHandle: TdxSpreadSheetCellStyleHandle;
  AStyles: TdxSpreadSheetCellStyles;
begin
  if (AFormatCode <> '') and (ACell.StyleHandle.DataFormat.FormatCode <> AFormatCode) then
  begin

    AStyles := ACell.SpreadSheet.CellStyles;
    AHandle := ACell.StyleHandle.Clone;
    AHandle.DataFormat := AStyles.Formats.AddFormat(AFormatCode, AStyles.Formats.PredefinedFormats.GetIDByFormatCode(AFormatCode));
    ACell.StyleHandle := AStyles.AddStyle(AHandle);
  end;
end;

class procedure TdxSpreadSheetCellStyleHelper.SetDataFormat(ACell: TdxSpreadSheetCell; const AFormatCodeID: Integer);
begin
  SetDataFormat(ACell, ACell.SpreadSheet.CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(AFormatCodeID).FormatCode);
end;

initialization
  TcxExport.RegisterProviderClass(TcxExportToCSVProvider);
  TcxExport.RegisterProviderClass(TcxExportToHTMLProvider);
  TcxExport.RegisterProviderClass(TcxExportToTXTProvider);
  TcxExport.RegisterProviderClass(TcxExportToXLSProvider);
  TcxExport.RegisterProviderClass(TcxExportToXLSXProvider);
  TcxExport.RegisterProviderClass(TcxExportToXMLProvider);
end.
