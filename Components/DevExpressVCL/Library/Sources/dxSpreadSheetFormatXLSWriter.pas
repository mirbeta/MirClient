{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetFormatXLSWriter;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  RTLConsts, Windows, Generics.Defaults, Generics.Collections, Classes, Types, SysUtils, Math, Variants, Graphics,
  dxCore, dxOLEDocument, cxClasses, cxVariants, dxCoreClasses, dxGDIPlusClasses, dxHashUtils, dxCoreGraphics,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetFormatXLSTypes, dxSpreadSheetUtils,
  dxSpreadSheetGraphics, cxGraphics, dxSpreadSheetFormulas, dxSpreadSheetFunctions, dxSpreadSheetFormatUtils,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetHyperlinks,
  dxSpreadSheetFormatXLSProtection, dxSpreadSheetProtection, dxSpreadSheetCoreStyles, dxSpreadSheetStrs,
  dxSpreadSheetStyles;

type
  TdxSpreadSheetXLSWriter = class;

  TdxXLSWriter = class;
  TdxXLSRecordWriter = class;

  { TdxXLSRecordWriter }

  TdxXLSRecordWriter = class(TMemoryStream)
  strict private
    FBands: TObjectList<TdxXLSRecordBand>;
    FCancel: Boolean;
    FRecordID: Word;
    FRecordSize: Word;

    function GetCurrentBandSize: Integer;
  protected
    procedure AddBand;
    procedure WriteRecordBlock(const ABuffer; ACount: Integer; ADataIsString: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginWrite(ARecordID: Word);
    procedure Cancel;
    function CheckBounds(ASizeNeeded: Word): Boolean;
    procedure EndWrite(ADestStream: TStream);

    property Bands: TObjectList<TdxXLSRecordBand> read FBands;
    property CurrentBandSize: Integer read GetCurrentBandSize;
    property RecordID: Word read FRecordID write FRecordID;
    property RecordSize: Word read FRecordSize write FRecordSize;
  end;

  { TdxXLSWriter }

  TdxXLSWriter = class(TcxWriter)
  strict private
    FOwner: TdxSpreadSheetXLSWriter;

    function GetByte(AOffset: Integer): Byte; inline;
    function GetBytes: PByteArray;
    function GetInteger(AOffset: Integer): Integer; inline;
    function GetPosition: Integer; inline;
    function GetStream: TdxXLSRecordWriter; inline;
    function GetWord(AOffset: Integer): Word; inline;
    procedure SetByte(AOffset: Integer; AValue: Byte); inline;
    procedure SetInteger(AOffset: Integer; AValue: Integer); inline;
    procedure SetPosition(AValue: Integer);
    procedure SetWord(AOffset: Integer; AValue: Word); inline;
  protected
    function GetFormattedStringData(AValue: TdxSpreadSheetFormattedSharedString): TBytes;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSWriter);

    procedure SetSize(ASize: Integer); inline;
    procedure XLS_WriteRef(const AArea: TRect);
    procedure XLS_WriteSharedString(AValue: TdxSpreadSheetSharedString);
    procedure XLS_WriteSimpleRef(const AArea: TRect);
    procedure XLS_WriteSimpleString(const AValue: string; ASize: Integer; AOffset: Integer = -1); overload;
    procedure WriteBlock(const ABuffer: array of Byte); overload;
    procedure WriteBlock(const ABuffer; ACount: Integer); overload;
    procedure WriteStream(ASource: TMemoryStream);

    property Bytes[AOffset: Integer]: Byte read GetByte write SetByte;
    property Integers[AOffset: Integer]: Integer read GetInteger write SetInteger;
    property Owner: TdxSpreadSheetXLSWriter read FOwner;
    property Position: Integer read GetPosition write SetPosition;
    property Stream: TdxXLSRecordWriter read GetStream;
    property Words[AOffset: Integer]: Word read GetWord write SetWord;
  end;

  TdxXLSRecordWriterInfo = class
  public
    ID: Word;
    Method: TdxXLSMethod;
  end;

  { TdxSpreadSheetXLSWriterProgressHelper }

  TdxSpreadSheetXLSWriterProgressHelper = class(TdxSpreadSheetCustomFilerProgressHelper);

  { TdxSpreadSheetXLSWriter }

  TdxSpreadSheetXLSWriter = class(TdxSpreadSheetCustomWriter)
  strict private
    FAllStyles: TList<TdxSpreadSheetCellStyleHandle>;
    FBoundSheets: TDictionary<TdxSpreadSheetTableView, Integer>;
    FColors: TDictionary<TColor, Integer>;
    FColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper;
    FCommonWriters: TObjectList<TdxXLSRecordWriterInfo>;
    FContainers: TDictionary<TdxSpreadSheetContainer, Integer>;
    FCRC32: TdxMsoCRC32Compute;
    FCurrentObject: Integer;
    FCurrentSheet: TdxSpreadSheetTableView;
    FCurrentSheetDimensions: TRect;
    FDefaultFont: TdxSpreadSheetFontHandle;
    FDocument: TdxOLEDocument;
    FDrawings: TDictionary<TObject, Integer>;
    FEncryptor: TdxXLSAbstractEncryptor;
    FExtSSTBackets: TList<TdxXLSWriterSSTBacketInfo>;
    FFontID: Integer;
    FFonts: TDictionary<TdxSpreadSheetFontHandle, Integer>;
    FFormatIndex: Integer;
    FFormats: TDictionary<TdxSpreadSheetFormatHandle, Integer>;
    FPictures: TList<TdxSpreadSheetContainer>;
    FPicturesCount: Integer;
    FRecordWriter: TdxXLSRecordWriter;
    FShapes: TList<TdxSpreadSheetContainer>;
    FShapesCount: Integer;
    FSST: TDictionary<TdxSpreadSheetSharedString, Integer>;
    FStyleID: Integer;
    FStyles: TDictionary<TdxSpreadSheetCellStyleHandle, Integer>;
    FTotalObjectsCount: Integer;
    FViewWriters: TObjectList<TdxXLSRecordWriterInfo>;
    FWorkbookData: TMemoryStream;
    FWriter: TdxXLSWriter;

    procedure AddSSTBucket;
    function GetProgressValue: Integer;
    procedure SetCurrentObject(AValue: Integer);
    procedure SetProgressValue(AValue: Integer);
    procedure WriteBOFInfo(AType: Word);
    procedure WriteDefaultBoolean(AValue: TdxDefaultBoolean);
    function WriteFormulaResult(AFormula: TdxSpreadSheetFormula; var AValue: Variant): Boolean;
  protected
    ExtXFCount: Integer;
    Palette: TColors;

    procedure AddObject;
    procedure CalculateObjectsCount; virtual;
    function CreateEncryptor: TdxXLSAbstractEncryptor; virtual;
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    function CreateRecordWriter(ARecordID: Integer; AWriter: TdxXLSMethod): TdxXLSRecordWriterInfo;
    procedure DoWriteRecord(ARecordInfo: TdxXLSRecordWriterInfo);
    procedure EncryptWorkbook;
    function GetColorIndex(AColor: TColor; ADefValue, ADefIndex: Integer; ANoneIndex: Integer = -1): Integer;
    function GetContainerID(AContainer: TdxSpreadSheetContainer): Integer;
    function HasPicture(AContainer: TdxSpreadSheetContainer): Boolean;
    procedure InitializePalette;
    procedure PrepareData; virtual;
    procedure RegisterCommonWriter(ARecordID: Integer; AWriter: TdxXLSMethod);
    procedure RegisterViewWriter(ARecordID: Integer; AWriter: TdxXLSMethod);
    procedure RegisterWriters; virtual;
    procedure WriteTableViewSheet(ASheet: TdxSpreadSheetTableView);
    procedure WriteViews;
    // common part
    procedure WriteBoundSheet;
    procedure WriteBoundSheets;
    procedure WriteCommonBOF;
    procedure WriteCommonEOF;
    procedure WriteDataFormat(AFormat: TdxSpreadSheetFormatHandle);
    procedure WriteDataFormatTable;
    procedure WriteDate1904;
    procedure WriteExternSheets;

    procedure WriteFilePassword;
    procedure WriteFont(AFont: TdxSpreadSheetFontHandle);
    procedure WriteFontTable;
    procedure WriteMSODrawingGroups;
    procedure WriteName(AName: TdxSpreadSheetDefinedName; AIsSpecialName: Boolean = False);
    procedure WriteNames;
    procedure WritePalette;
    procedure WritePassword;
    procedure WritePrecision;
    procedure WritePrintArea(const AName, AReference: string; AScope: TdxSpreadSheetTableView);
    procedure WritePrintAreas(AScope: TdxSpreadSheetTableView);
    procedure WriteProtect;
    procedure WriteRecalcID;
    procedure WriteRefreshAll;
    procedure WriteSharedStringExtTable;
    procedure WriteSharedStringTable;
    procedure WriteStyle(AStyle: TdxSpreadSheetCellStyleHandle; ACellStyle: Boolean = True; ADataFormat: Integer = -1);
    procedure WriteStyleExtProperties(AID: Word; AStyle: TdxSpreadSheetCellStyleHandle);
    procedure WriteStyles;
    procedure WriteStylesExtProperties;
    procedure WriteStyleSheets;
    procedure WriteSupBook;
    procedure WriteTabID;
    procedure WriteWindow1;

    // table view part
    procedure WriteBOF;
    procedure WriteCalcCount;
    procedure WriteCalcMode;
    procedure WriteCell(ARowIndex: Integer; ACell: TdxSpreadSheetCell);
    procedure WriteCellArrayFormula(ACell: TdxSpreadSheetCell);
    procedure WriteCellFormula(ACell: TdxSpreadSheetCell);
    procedure WriteCells;
    procedure WriteColumnFormatInformation(AStartColumn, AFinishColumn: TdxSpreadSheetTableColumn);
    procedure WriteColumns;
    procedure WriteComments;
    procedure WriteConditionalFormatting;
    procedure WriteConditionalFormattingRule(ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
    procedure WriteConditionalFormattingRuleStyle(var AOptions: Cardinal; AStyle: TdxSpreadSheetCellStyle);
    procedure WriteConditionalFormattingRuleStyleBorders(var AOptions: Cardinal; ADefaultBorders, ABorders: TdxSpreadSheetBordersHandle);
    procedure WriteConditionalFormattingRuleStyleBrush(var AOptions: Cardinal; ADefaultBrush, ABrush: TdxSpreadSheetBrushHandle);
    procedure WriteConditionalFormattingRuleStyleFont(ADefaultFont, AFont: TdxSpreadSheetFontHandle);
    procedure WriteDefaultColumnWidth;
    procedure WriteDefaultRowHeight;
    procedure WriteDimension;
    procedure WriteEOF;
    procedure WriteFooter;
    procedure WriteGuts;
    procedure WriteHeader;
    procedure WriteHyperlink(AHyperlink: TdxSpreadSheetHyperlink);
    procedure WriteHyperlinks;
    procedure WriteHyperlinkTooltip(AHyperlink: TdxSpreadSheetHyperlink);
    procedure WriteIndex;
    procedure WriteIteration;
    procedure WritePanes;
    procedure WriteMergedCells;
    procedure WriteMSODrawingObjectInfo(AContainer: TdxSpreadSheetContainer);
    procedure WriteMSODrawing(AContainer: TdxSpreadSheetContainer; var AIndex, ASize: Integer);
    procedure WriteMSODrawings;
    procedure WriteMSOTextObject(ATextBox: TdxSpreadSheetCustomTextBox);
    procedure WritePageBreaksHorizontal;
    procedure WritePageBreaksVertical;
    procedure WritePrintBottomMargin;
    procedure WritePrintCenterH;
    procedure WritePrintCenterV;
    procedure WritePrintFooter;
    procedure WritePrintGridLines;
    procedure WritePrintHeader;
    procedure WritePrintHeaders;
    procedure WritePrintLeftMargin;
    procedure WritePrintRightMargin;
    procedure WritePrintSetup;
    procedure WritePrintTopMargin;
    procedure WritePrintWorkspaceInfo;
    procedure WriteRefMode;
    procedure WriteRowFormatInformation(ARow: TdxSpreadSheetTableRow);
    procedure WriteRows;
    procedure WriteSelection;
    procedure WriteSharedFeatures;
    procedure WriteWindow2;
    procedure WriteZoomFactor;
    //
    property AllStyles: TList<TdxSpreadSheetCellStyleHandle> read FAllStyles;
    property CRC32: TdxMsoCRC32Compute read FCRC32;
    property CurrentObject: Integer read FCurrentObject write SetCurrentObject;
    property CurrentSheetDimensions: TRect read FCurrentSheetDimensions write FCurrentSheetDimensions;
    property Encryptor: TdxXLSAbstractEncryptor read FEncryptor;
    property ExtSSTBackets: TList<TdxXLSWriterSSTBacketInfo> read FExtSSTBackets;
    property ProgressValue: Integer read GetProgressValue write SetProgressValue;
    property TotalObjectsCount: Integer read FTotalObjectsCount write FTotalObjectsCount;
 public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    procedure FlushRecord;
    procedure WriteData; override;

    property BoundSheets: TDictionary<TdxSpreadSheetTableView, Integer> read FBoundSheets;
    property Colors: TDictionary<TColor, Integer> read FColors;
    property ColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper read FColumnWidthHelper;
    property CommonWriters: TObjectList<TdxXLSRecordWriterInfo> read FCommonWriters;
    property Containers: TDictionary<TdxSpreadSheetContainer, Integer> read FContainers;
    property CurrentSheet: TdxSpreadSheetTableView read FCurrentSheet;
    property Document: TdxOLEDocument read FDocument;
    property Drawings: TDictionary<TObject, Integer> read FDrawings;
    property Fonts: TDictionary<TdxSpreadSheetFontHandle, Integer> read FFonts;
    property Formats: TDictionary<TdxSpreadSheetFormatHandle, Integer> read FFormats;
    property Pictures: TList<TdxSpreadSheetContainer> read FPictures;
    property PicturesCount: Integer read FPicturesCount;
    property RecordWriter: TdxXLSRecordWriter read FRecordWriter;
    property Shapes: TList<TdxSpreadSheetContainer> read FShapes;
    property ShapesCount: Integer read FShapesCount;
    property SST: TDictionary<TdxSpreadSheetSharedString, Integer> read FSST;
    property Styles: TDictionary<TdxSpreadSheetCellStyleHandle, Integer> read FStyles;
    property ViewWriters: TObjectList<TdxXLSRecordWriterInfo> read FViewWriters;
    property WorkbookData: TMemoryStream read FWorkbookData;
    property Writer: TdxXLSWriter read FWriter;
  end;

implementation

uses
  cxGeometry, dxSpreadSheetFormatXLSFormulas, dxSpreadSheetFormatXLSDrawing, dxSpreadSheetCoreHelpers,
  dxSpreadSheetFormatXLS, dxSpreadSheetCoreFormulas, dxSpreadSheetConditionalFormatting;

type
  TdxHashTableAccess = class(TdxHashTable);
  TdxSpreadSheetCustomFormulaControllerAccess = class(TdxSpreadSheetCustomFormulaController);
  TdxDynamicListItemAccess = class(TdxDynamicListItem);
  TdxSpreadSheetItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetCellStylesAccess = class(TdxSpreadSheetCellStyles);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetConditionalFormattingExpressionAccess = class(TdxSpreadSheetConditionalFormattingExpression);
  TdxSpreadSheetConditionalFormattingRuleExpressionAccess = class(TdxSpreadSheetConditionalFormattingRuleExpression);
  TdxSpreadSheetCustomTextBoxContainerAccess = class(TdxSpreadSheetCustomTextBoxContainer);

const
  dxMaxXFStyles = 4050;

function dxFindNearestColor(AColor: TColor; const APalette: TColors): Integer;
var
  I: Integer;
  ACandidate: TColor;
  AMinLength, ALength: Double;
begin
  Result := 0;
  AMinLength := MaxDouble;
  for I := 0 to Length(APalette) - 1 do
  begin
    ACandidate := APalette[I];
    ALength := Sqr(TRGBQuad(AColor).rgbRed - TRGBQuad(ACandidate).rgbRed) +
      Sqr(TRGBQuad(AColor).rgbGreen - TRGBQuad(ACandidate).rgbGreen) +
      Sqr(TRGBQuad(AColor).rgbBlue - TRGBQuad(ACandidate).rgbBlue);
    if AMinLength > ALength then
    begin
      Result := I;
      AMinLength := ALength;
    end;
  end;
end;

{ TdxSpreadSheetXLSWriter }

constructor TdxSpreadSheetXLSWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
var
  ARoot: TdxOLEDocumentDirectoryEntry;
begin
  inherited Create(AOwner, AStream);
  FDocument := TdxOLEDocument.Create(AStream, dmWriting);
  ARoot := FDocument.CreateDirEntry(TdxOLEDocument.RootDirName, ET_ROOT);
  FAllStyles := TList<TdxSpreadSheetCellStyleHandle>.Create();
  FAllStyles.Capacity := dxMaxXFStyles;
  FWorkbookData := FDocument.CreateStream(ARoot, 'Workbook') as TMemoryStream;
  FDocument.CreateDirEntry('', ET_EMPTY, ARoot);
  FDocument.CreateDirEntry('', ET_EMPTY, ARoot);
  FExtSSTBackets := TList<TdxXLSWriterSSTBacketInfo>.Create;
  FBoundSheets := TDictionary<TdxSpreadSheetTableView, Integer>.Create;
  FFonts:= TDictionary<TdxSpreadSheetFontHandle, Integer>.Create(SpreadSheet.CellStyles.Fonts.Count + 1);
  FFormats := TDictionary<TdxSpreadSheetFormatHandle, Integer>.Create;

  FEncryptor := CreateEncryptor;
  FColors := TDictionary<TColor, Integer>.Create(56);
  FColumnWidthHelper := TdxSpreadSheetExcelColumnWidthHelper.Create;
  FCommonWriters := TObjectList<TdxXLSRecordWriterInfo>.Create;
  FContainers := TDictionary<TdxSpreadSheetContainer, Integer>.Create;
  FCRC32 := TdxMsoCRC32Compute.Create;
  FViewWriters := TObjectList<TdxXLSRecordWriterInfo>.Create;
  FRecordWriter := TdxXLSRecordWriter.Create;
  FDrawings := TDictionary<TObject, Integer>.Create;
  FPictures := TList<TdxSpreadSheetContainer>.Create;
  FShapes := TList<TdxSpreadSheetContainer>.Create;
  FSST := TDictionary<TdxSpreadSheetSharedString, Integer>.Create;
  FStyles := TDictionary<TdxSpreadSheetCellStyleHandle, Integer>.Create;
  FWriter := TdxXLSWriter.Create(Self);
  RegisterWriters;
end;

destructor TdxSpreadSheetXLSWriter.Destroy;
begin
  FreeAndNil(FAllStyles);
  FreeAndNil(FBoundSheets);
  FreeAndNil(FFonts);
  FreeAndNil(FExtSSTBackets);
  FreeAndNil(FCommonWriters);
  FreeAndNil(FColors);
  FreeAndNil(FColumnWidthHelper);
  FreeAndNil(FCRC32);
  FreeAndNil(FFormats);
  FreeAndNil(FDocument);
  FreeAndNil(FRecordWriter);
  FreeAndNil(FSST);
  FreeAndNil(FStyles);
  FreeAndNil(FWriter);
  FreeAndNil(FViewWriters);
  FreeAndNil(FDrawings);
  FreeAndNil(FPictures);
  FreeAndNil(FShapes);
  FreeAndNil(FContainers);
  FreeAndNil(FEncryptor);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSWriter.FlushRecord;
var
  ARecordID: Integer;
begin
  ARecordID := FRecordWriter.RecordID;
  try
    FRecordWriter.EndWrite(WorkbookData);
  finally
    FRecordWriter.BeginWrite(ARecordID);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteData;
var
  AIndex: Integer;
begin
  DoProgress(0);
  try
    PrepareData;
    for AIndex := 0 to CommonWriters.Count - 1 do
      DoWriteRecord(CommonWriters[AIndex]);
    if Encryptor <> nil then
      EncryptWorkbook;
    FDocument.Commit;
  finally
    CurrentObject := TotalObjectsCount;
  end;
end;

function TdxSpreadSheetXLSWriter.CreateEncryptor: TdxXLSAbstractEncryptor;
begin
  if SpreadSheet.Password <> '' then
    Result := TdxXLSStandardEncryptor.Create(SpreadSheet.Password)
  else
    if SpreadSheet.OptionsProtection.Protected then
      Result := TdxXLSStandardEncryptor.Create(dxSpreadSheetDefaultPassword)
    else
      Result := nil;
end;

function TdxSpreadSheetXLSWriter.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetXLSWriterProgressHelper.Create(Self, 0);
end;

procedure TdxSpreadSheetXLSWriter.AddObject;
begin
  CurrentObject := CurrentObject + 1;
end;

procedure TdxSpreadSheetXLSWriter.CalculateObjectsCount;

  function GetRowCellCount(ACells: TdxSpreadSheetTableRowCells): Integer;
  var
    AItem: TdxDynamicListItem;
  begin
    Result := 0;
    AItem := ACells.First;
    while AItem <> nil do
    begin
      Inc(Result);
      AItem := TdxDynamicListItemAccess(AItem).FNext;
    end;
  end;

var
  ASheet: Integer;
  ATableView: TdxSpreadSheetTableView;
  ARow: TdxSpreadSheetTableRow;
begin
  FTotalObjectsCount := 1;
  Inc(FTotalObjectsCount, TdxSpreadSheetAccess(SpreadSheet).StringTable.UniqueCount);
  TdxHashTableAccess(TdxSpreadSheetAccess(SpreadSheet).CellStyles).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      Inc(FTotalObjectsCount);
    end);

  for ASheet := 0 to SpreadSheet.SheetCount - 1 do
  begin
    if not (SpreadSheet.Sheets[ASheet] is TdxSpreadSheetTableView) then
      Continue;
    ATableView := TdxSpreadSheetTableView(SpreadSheet.Sheets[ASheet]);
    ARow := TdxSpreadSheetTableRow(ATableView.Rows.First);
    while ARow <> nil do
    begin
      Inc(FTotalObjectsCount, GetRowCellCount(TdxSpreadSheetRowAccess(ARow).RowCells) + 1);
      ARow := TdxSpreadSheetTableRow(ARow.Next);
      if (ARow <> nil) and (ARow.Index > dxMaxRow) then
        Break;
    end;
    Inc(FTotalObjectsCount, ATableView.Containers.Count);
    Inc(FTotalObjectsCount);
  end;
end;

function TdxSpreadSheetXLSWriter.CreateRecordWriter(ARecordID: Integer; AWriter: TdxXLSMethod): TdxXLSRecordWriterInfo;
begin
  Result := TdxXLSRecordWriterInfo.Create;
  Result.ID := ARecordID;
  Result.Method := AWriter;
end;

procedure TdxSpreadSheetXLSWriter.DoWriteRecord(ARecordInfo: TdxXLSRecordWriterInfo);
begin
  if ARecordInfo.ID <> 0 then
  begin
    RecordWriter.BeginWrite(ARecordInfo.ID);
    try
      ARecordInfo.Method;
    finally
      RecordWriter.EndWrite(WorkbookData);
    end;
  end
  else
    ARecordInfo.Method;
end;

procedure TdxSpreadSheetXLSWriter.EncryptWorkbook;
var
  AOffset: Integer;
  ID, ASize: Word;
begin
  WorkbookData.Position := 0;
  while WorkbookData.Position + 4 <= WorkbookData.Size do
  begin
    ID := ReadWordFunc(WorkbookData);
    ASize := ReadWordFunc(WorkbookData);
    if (ID <> brcBOF) and (ID <> brcFILEPASS) then
    begin
      if ID = brcBOUNDSHEET then
        AOffset := SizeOf(Integer)
      else
        AOffset := 0;

      Encryptor.Encrypt(
        PByte(WorkbookData.Memory) + WorkbookData.Position + AOffset,
        WorkbookData.Position + AOffset, ASize - AOffset);
    end;
    WorkbookData.Seek(ASize, soCurrent);
  end;
end;

function TdxSpreadSheetXLSWriter.GetColorIndex(AColor: TColor;
  ADefValue, ADefIndex: Integer; ANoneIndex: Integer = -1): Integer;
var
  AValue: Integer;
begin
  Result := ADefIndex;
  if ANoneIndex < 0 then
    ANoneIndex := ADefIndex;

  if (AColor = clDefault) or (ColorToRgb(AColor) = ADefValue) then
    Exit
  else
    if AColor = clNone then
    begin
      Result := ANoneIndex;
      Exit;
    end;

  AColor := ColorToRgb(AColor);
  if Colors.TryGetValue(AColor, AValue) then
    Result := AValue
  else
  begin
    Result := dxFindNearestColor(AColor, Palette);
    Colors.Add(AColor, Result);
  end;
  Result := (Result + 8) and $7F;
end;

function TdxSpreadSheetXLSWriter.GetContainerID(AContainer: TdxSpreadSheetContainer): Integer;
begin
  if not Containers.TryGetValue(AContainer, Result) then
    Result := -1;
end;

function TdxSpreadSheetXLSWriter.HasPicture(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  Result :=
    (AContainer is TdxSpreadSheetPictureContainer) and not TdxSpreadSheetPictureContainer(AContainer).Picture.Empty or
    (AContainer is TdxSpreadSheetShapeContainer) and not TdxSpreadSheetShapeContainer(AContainer).Shape.Brush.Texture.Empty;
end;

procedure TdxSpreadSheetXLSWriter.InitializePalette;
var
  I: Integer;
begin
  Palette := TdxSpreadSheetCellStylesAccess(SpreadSheet.CellStyles).Palette;
  if Length(Palette) = 0 then
  begin
    SetLength(Palette, Length(dxExcelStandardColors));
    Move(dxExcelStandardColors[0], Palette[0], Length(dxExcelStandardColors) * SizeOf(TColor));
  end;
  for I := Length(Palette) - 1 downto 0 do
    Colors.AddOrSetValue(Palette[I], I);
end;

procedure TdxSpreadSheetXLSWriter.PrepareData;
var
  AContainer: TdxSpreadSheetContainer;
  AContainerID: Integer;
  AContainerIndex: Integer;
  ASheet: TdxSpreadSheetTableView;
  ASheetIndex: Integer;

  procedure AddShape(AContainer: TdxSpreadSheetContainer);
  begin
    Shapes.Add(AContainer);
    FDrawings.Add(AContainer, FDrawings.Count + 1);
    Inc(AContainerID);
  end;

  function CanProcessContainerAsShape(AContainer: TdxSpreadSheetContainer): Boolean;
  begin
    Result :=
      (AContainer.ClassType = TdxSpreadSheetCommentContainer) or
      (AContainer.ClassType = TdxSpreadSheetShapeContainer) or
      (AContainer.ClassType = TdxSpreadSheetTextBoxContainer);
  end;

begin
  FPicturesCount := 0;
  FShapesCount := 0;
  AContainerID := 0;
  CalculateObjectsCount;

  TdxHashTableAccess(TdxSpreadSheetAccess(SpreadSheet).SharedImages).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      FDrawings.Add(TdxSpreadSheetSharedImageHandle(AItem), FDrawings.Count + 1);
      Inc(FPicturesCount, TdxSpreadSheetSharedImageHandle(AItem).RefCount);
    end);

  for ASheetIndex := 0 to SpreadSheet.SheetCount - 1 do
  begin
    ASheet := SpreadSheet.Sheets[ASheetIndex] as TdxSpreadSheetTableView;
    for AContainerIndex := 0 to ASheet.Containers.Count - 1 do
    begin
      AContainer := ASheet.Containers[AContainerIndex];
      if AContainer.Visible or (AContainer.ClassType = TdxSpreadSheetCommentContainer) then
      begin
        if HasPicture(AContainer) then
        begin
          Pictures.Add(AContainer as TdxSpreadSheetShapeContainer);
          if CanProcessContainerAsShape(AContainer) then
            AddShape(AContainer)
          else
            Inc(AContainerID);

          Inc(AContainerID);
        end
        else
          if CanProcessContainerAsShape(AContainer) then
            AddShape(AContainer)
          else
            Continue;

        Containers.Add(AContainer, AContainerID);
      end;
    end;
  end;
  Inc(FTotalObjectsCount, Pictures.Count);
end;

procedure TdxSpreadSheetXLSWriter.RegisterCommonWriter(ARecordID: Integer; AWriter: TdxXLSMethod);
begin
  CommonWriters.Add(CreateRecordWriter(ARecordID, AWriter));
end;

procedure TdxSpreadSheetXLSWriter.RegisterViewWriter(ARecordID: Integer; AWriter: TdxXLSMethod);
begin
  ViewWriters.Add(CreateRecordWriter(ARecordID, AWriter));
end;

procedure TdxSpreadSheetXLSWriter.RegisterWriters;
begin
  InitializePalette;

  RegisterCommonWriter(brcBOF, WriteCommonBOF);
  if Encryptor <> nil then
    RegisterCommonWriter(brcFILEPASS, WriteFilePassword);
  RegisterCommonWriter(brcTabID, WriteTabID);
  RegisterCommonWriter(brcProtect, WriteProtect);
  RegisterCommonWriter(brcPASSWORD, WritePassword);
  RegisterCommonWriter(brc1904, WriteDate1904);
  RegisterCommonWriter(brcWindow1, WriteWindow1);

  RegisterCommonWriter(0, WriteFontTable);
  RegisterCommonWriter(0, WriteDataFormatTable);
  RegisterCommonWriter(0, WriteStyles);
  RegisterCommonWriter(0, WriteStyleSheets);
  RegisterCommonWriter(brcPalette, WritePalette);
  RegisterCommonWriter(0, WriteBoundSheets);
  RegisterCommonWriter(brcSUPBOOK, WriteSupBook);
  RegisterCommonWriter(brcExternSheet, WriteExternSheets);
  RegisterCommonWriter(0, WriteNames);
  RegisterCommonWriter(brcRecalcID, WriteRecalcID);
  RegisterCommonWriter(brcSST, WriteSharedStringTable);
  RegisterCommonWriter(brcEXTSST, WriteSharedStringExtTable);
  RegisterCommonWriter(brcMSODRAWINGGROUP, WriteMSODrawingGroups);
  RegisterCommonWriter(brcEOF, WriteEOF);
  RegisterCommonWriter(0, WriteViews);

  // Table view records
  RegisterViewWriter(brcBOF, WriteBOF);
  RegisterViewWriter(brcIndex, WriteIndex);
  RegisterViewWriter(brcProtect, WriteProtect);
  RegisterViewWriter(brcPASSWORD, WritePassword);
  RegisterViewWriter(brcCalcMode, WriteCalcMode);
  RegisterViewWriter(brcCalcCount, WriteCalcCount);
  RegisterViewWriter(brcRefMode, WriteRefMode);
  RegisterViewWriter(brcIteration, WriteIteration);
  RegisterViewWriter(brcSTANDARDWIDTH, WriteDefaultColumnWidth);
  RegisterViewWriter(brcDefaultRowHeight, WriteDefaultRowHeight);
  RegisterViewWriter(brcSetup, WritePrintSetup);
  RegisterViewWriter(brcHORIZONTALPAGEBREAKS, WritePageBreaksHorizontal);
  RegisterViewWriter(brcVERTICALPAGEBREAKS, WritePageBreaksVertical);
  RegisterViewWriter(brcHeader, WritePrintHeader);
  RegisterViewWriter(brcPrintHeaders, WritePrintHeaders);
  RegisterViewWriter(brcPrintGridLines, WritePrintGridLines);
  RegisterViewWriter(brcGuts, WriteGuts);
  RegisterViewWriter(brcFooter, WritePrintFooter);
  RegisterViewWriter(brcLeftMargin, WritePrintLeftMargin);
  RegisterViewWriter(brcTopMargin, WritePrintTopMargin);
  RegisterViewWriter(brcRightMargin, WritePrintRightMargin);
  RegisterViewWriter(brcBottomMargin, WritePrintBottomMargin);
  RegisterViewWriter(brcHCenter, WritePrintCenterH);
  RegisterViewWriter(brcVCenter, WritePrintCenterV);
  RegisterViewWriter(brcWSBOOL, WritePrintWorkspaceInfo);
  RegisterViewWriter(brcDimensions, WriteDimension);
  RegisterViewWriter(brcWINDOW2, WriteWindow2);
  RegisterViewWriter(brcScl, WriteZoomFactor);

  RegisterViewWriter(0, WriteColumns);
  RegisterViewWriter(0, WriteRows);
  RegisterViewWriter(0, WriteCells);

  RegisterViewWriter(0, WriteMSODrawings);
  RegisterViewWriter(0, WriteComments);

  RegisterViewWriter(brcPane, WritePanes);
  RegisterViewWriter(brcSelection, WriteSelection);
  RegisterViewWriter(0, WriteHyperlinks);
  RegisterViewWriter(brcMergeCells, WriteMergedCells);
  RegisterViewWriter(0, WriteConditionalFormatting);
  RegisterViewWriter(brcFEATHEADR, WriteSharedFeatures);
  RegisterViewWriter(brcEOF, WriteEOF);
end;

procedure TdxSpreadSheetXLSWriter.WriteTableViewSheet(ASheet: TdxSpreadSheetTableView);
var
  AIndex: Integer;
begin
  FCurrentSheet := ASheet;
  FCurrentSheetDimensions := ASheet.Dimensions;
  PInteger(@PByteArray(WorkbookData.Memory)^[BoundSheets[ASheet] + 4])^ := WorkbookData.Size;
  for AIndex := 0 to ViewWriters.Count - 1 do
    DoWriteRecord(ViewWriters[AIndex]);
  //
  AddObject;
end;

procedure TdxSpreadSheetXLSWriter.WriteViews;
var
  ASheetIndex: Integer;
begin
  for ASheetIndex := 0 to SpreadSheet.SheetCount - 1 do
    if SpreadSheet.Sheets[ASheetIndex] is TdxSpreadSheetTableView then
       WriteTableViewSheet(SpreadSheet.Sheets[ASheetIndex] as TdxSpreadSheetTableView);
end;

// common part

procedure TdxSpreadSheetXLSWriter.WriteBoundSheet;
begin
  BoundSheets.Add(CurrentSheet, WorkbookData.Position);
  Writer.WriteInteger(0);
  Writer.WriteByte(Ord(not CurrentSheet.Visible));
  Writer.WriteByte(0);
  Writer.XLS_WriteSimpleString(CurrentSheet.Caption, 1)
end;

procedure TdxSpreadSheetXLSWriter.WriteBoundSheets;
var
  I: Integer;
begin
  for I := 0 to SpreadSheet.SheetCount - 1 do
  begin
    FCurrentSheet := TdxSpreadSheetTableView(SpreadSheet.Sheets[I]);
    RecordWriter.BeginWrite(brcBOUNDSHEET);
    try
      WriteBoundSheet;
    finally
      RecordWriter.EndWrite(WorkbookData);
    end;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteCommonEOF;
begin
  // EOF record is empty
end;

procedure TdxSpreadSheetXLSWriter.WriteDataFormat(AFormat: TdxSpreadSheetFormatHandle);
begin
  RecordWriter.BeginWrite(brcFORMAT);
  try
    Writer.WriteWord(FFormatIndex);
    Writer.XLS_WriteSimpleString(AFormat.FormatCode, 2);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteDataFormatTable;
const
  CustomNumberFormatBase = 164;
begin
  FFormatIndex := CustomNumberFormatBase;
  TdxHashTableAccess(SpreadSheet.CellStyles.Formats).ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      AFormat: TdxSpreadSheetFormatHandle;
    begin
      AFormat := TdxSpreadSheetFormatHandle(AItem);
      if SpreadSheet.CellStyles.Formats.IsCustom(AFormat) then
      begin
        Formats.Add(AFormat, FFormatIndex);
        WriteDataFormat(AFormat);
        Inc(FFormatIndex);
      end
      else
        Formats.Add(AFormat, AFormat.FormatCodeID);
    end);
end;

procedure TdxSpreadSheetXLSWriter.WriteComments;
var
  AAuthor: AnsiString;
  AAuthorLength: Integer;
  AContainer: TdxSpreadSheetContainer;
  AContainerID: Integer;
  I: Integer;
begin
  for I := 0 to CurrentSheet.Containers.Count - 1 do
  begin
    AContainer := CurrentSheet.Containers[I];
    if (AContainer is TdxSpreadSheetCommentContainer) and (TdxSpreadSheetCommentContainer(AContainer).Cell <> nil) then
    begin
      AContainerID := GetContainerID(AContainer);
      if AContainerID > 0 then
      begin
        RecordWriter.BeginWrite(brcNOTE);
        try
          Writer.WriteWord(TdxSpreadSheetCommentContainer(AContainer).Cell.RowIndex);
          Writer.WriteWord(TdxSpreadSheetCommentContainer(AContainer).Cell.ColumnIndex);
          Writer.WriteWord(IfThen(TdxSpreadSheetCommentContainer(AContainer).Visible, $2));
          Writer.WriteWord(AContainerID);

          AAuthor := dxStringToAnsiString(TdxSpreadSheetCommentContainer(AContainer).Author);
          AAuthorLength := Length(AAuthor);
          Writer.WriteWord(AAuthorLength);
          if AAuthorLength > 0 then
          begin
            if (WorkbookData.Position + Writer.Position) mod 2 <> 0 then
              Writer.WriteByte(0);
            Writer.Stream.WriteBuffer(AAuthor[1], AAuthorLength);
            Writer.WriteByte(0);
          end;
        finally
          RecordWriter.EndWrite(WorkbookData);
        end;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteCommonBOF;
begin
  WriteBofInfo($05); // Workbook globals
end;

procedure TdxSpreadSheetXLSWriter.WriteDate1904;
begin
  Writer.WriteWord(Word(SpreadSheet.OptionsView.DateTimeSystem = dts1904));
end;

procedure TdxSpreadSheetXLSWriter.WriteExternSheets;
var
  I: Integer;
begin
  if SpreadSheet.SheetCount = 0 then
  begin
    Writer.SetSize(8);
    Writer.Words[0] := 1;
  end
  else
  begin
    Writer.WriteWord(SpreadSheet.SheetCount);
    for I := 0 to SpreadSheet.SheetCount - 1 do
    begin
      Writer.WriteWord(0);
      Writer.WriteWord(I);
      Writer.WriteWord(I);
    end;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteFilePassword;
begin
  Writer.WriteWord(1); // Standard Encryption
  Writer.WriteWord(1); // RC4
  Encryptor.Save(Writer);
end;

procedure TdxSpreadSheetXLSWriter.WriteFont(AFont: TdxSpreadSheetFontHandle);
const
  ABold: array[Boolean] of Word = ($190, $2BC);
  AItalic: array[Boolean] of Word = (0, $02);
  AStrikeOut: array[Boolean] of Word = (0, 9);
begin
  Fonts.AddOrSetValue(AFont, FFontID);
  RecordWriter.BeginWrite(brcFont);
  try
    Writer.SetSize(16 + Length(AFont.Name) * 2);
    Writer.Words[0] := AFont.Size * 20;
    Writer.Words[2] := AItalic[fsItalic in AFont.Style] + AStrikeOut[fsStrikeOut in AFont.Style];
    Writer.Words[4] := GetColorIndex(AFont.Color, 0, $7FFF, $7FFF);
    Writer.Words[6] := ABold[fsBold in AFont.Style];
    Writer.Words[8] := Byte(AFont.Script);
    Writer.Words[10] := Byte(fsUnderline in AFont.Style);

    Writer.Bytes[12] := IfThen(AFont.Charset = DEFAULT_CHARSET, 0, Integer(AFont.Charset));
    Writer.XLS_WriteSimpleString(AFont.Name, 1, 14);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
  Inc(FFontID);
end;

procedure TdxSpreadSheetXLSWriter.WriteFontTable;
var
  I: Integer;
  AFont: TFont;
begin
  AFont := TdxSpreadSheetAccess(SpreadSheet).Font;
  FDefaultFont := SpreadSheet.CellStyles.Fonts.CreateFont;
  FDefaultFont.Name := AFont.Name;
  FDefaultFont.Style := AFont.Style;
  FDefaultFont.Size := AFont.Size;
  FDefaultFont.Pitch := AFont.Pitch;
  FDefaultFont := SpreadSheet.CellStyles.Fonts.AddFont(FDefaultFont);
  for I := 0 to 4 do
    WriteFont(FDefaultFont);
  //
  ColumnWidthHelper.Font := FDefaultFont.GraphicObject;
  TdxHashTableAccess(SpreadSheet.CellStyles.Fonts).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      WriteFont(TdxSpreadSheetFontHandle(AItem));
    end);
end;

procedure TdxSpreadSheetXLSWriter.WriteMSODrawingGroups;
begin
  with TdxSpreadSheetMSODrawingWriter.Create(Self) do
  try
    WriteDrawingGroups;
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteName(AName: TdxSpreadSheetDefinedName; AIsSpecialName: Boolean = False);
var
  AFormulaWriter: TdxXLSFormulaWriter;
begin
  RecordWriter.BeginWrite(brcName);
  try
    Writer.WriteWord(IfThen(AIsSpecialName, $20));
    Writer.WriteByte(0);
    Writer.WriteByte(Length(AName.Caption));
    //
    AFormulaWriter := TdxXLSFormulaWriter.Create(Self);
    try
      //
      if AName.Scope <> nil then
      begin
        Writer.WriteWord(AName.Scope.Index + 1);
        Writer.WriteWord(AName.Scope.Index + 1);
      end
      else
        Writer.WriteInteger(0);
      //
      Writer.WriteByte(0);
      Writer.WriteByte(0);
      Writer.WriteByte(0);
      Writer.WriteByte(0);
      //
      Writer.WriteByte(1);
      Writer.WriteBlock(AName.Caption[1], Length(AName.Caption) * 2);
      //
      AFormulaWriter.WriteNameFormula(AName);
    finally
      AFormulaWriter.Free;
    end;
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteNames;
var
  I: Integer;
begin
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    WriteName(SpreadSheet.DefinedNames[I]);
  for I := 0 to SpreadSheet.SheetCount - 1 do
  begin
    if SpreadSheet.Sheets[I] is TdxSpreadSheetTableView then
      WritePrintAreas(TdxSpreadSheetTableView(SpreadSheet.Sheets[I]));
  end;
end;

procedure TdxSpreadSheetXLSWriter.WritePalette;
begin
  Writer.WriteWord(Length(Palette));
  Writer.Stream.WriteBuffer(Palette[0], Length(Palette) * SizeOf(TColor));
end;

procedure TdxSpreadSheetXLSWriter.WritePrecision;
begin
  Writer.WriteByte(0);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintArea(const AName, AReference: string; AScope: TdxSpreadSheetTableView);
var
  ADefinedName: TdxSpreadSheetDefinedName;
begin
  ADefinedName := TdxSpreadSheetDefinedName.Create(SpreadSheet.DefinedNames);
  try
    ADefinedName.Caption := AName;
    ADefinedName.Scope := AScope;
    ADefinedName.Reference := AReference;
    WriteName(ADefinedName, True);
  finally
    ADefinedName.Free;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WritePrintAreas(AScope: TdxSpreadSheetTableView);
begin
  if AScope.OptionsPrint.Source.Area.Assigned then
    WritePrintArea(#6, dxReferenceToString(AScope.OptionsPrint.Source.Area.Rect, False, [croSheetName], AScope.Caption), AScope);
  if AScope.OptionsPrint.Source.RowsToRepeat.Assigned or AScope.OptionsPrint.Source.ColumnsToRepeat.Assigned then
    WritePrintArea(#7, TdxSpreadSheetPrintAreasHelper.BuildPrintTitlesReference(AScope), AScope);
end;

procedure TdxSpreadSheetXLSWriter.WriteProtect;
var
  AProtected: Boolean;
begin
  if CurrentSheet <> nil then
    AProtected := CurrentSheet.OptionsProtection.Protected
  else
    AProtected := SpreadSheet.OptionsProtection.Protected;

  Writer.WriteWord(Ord(AProtected));
end;

procedure TdxSpreadSheetXLSWriter.WriteRecalcID;
const
  Data: array[0..7] of Byte = ($C1, $01, $00, $00, $54, $8D, $01, $00);
begin
  Writer.WriteBlock(Data);
end;

procedure TdxSpreadSheetXLSWriter.WriteRefreshAll;
begin
  Writer.WriteWord(1);
end;

procedure TdxSpreadSheetXLSWriter.WriteSharedStringExtTable;
var
  I: Integer;
  AInfo: TdxXLSWriterSSTBacketInfo;
begin
  if TdxSpreadSheetAccess(SpreadSheet).StringTable.Count = 0 then
  begin
    RecordWriter.Cancel;
    Exit;
  end;
  Writer.WriteWord(dxXLSCountStringsInSSTBucked);
  for I := 0 to FExtSSTBackets.Count - 1 do
  begin
    AInfo := FExtSSTBackets[I];
    Writer.WriteBlock(AInfo, SizeOf(AInfo));
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteSharedStringTable;
var
  AStrings: TdxSpreadSheetSharedStringTable;
begin
  AStrings := TdxSpreadSheetAccess(SpreadSheet).StringTable;
  if AStrings.Count = 0 then
  begin
    RecordWriter.Cancel;
    Exit;
  end;
  Writer.WriteInteger(Max(AStrings.Count, AStrings.UniqueCount));
  Writer.WriteInteger(AStrings.UniqueCount);
  TdxHashTableAccess(AStrings).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      SST.Add(TdxSpreadSheetSharedString(AItem), SST.Count);
      if SST.Count mod dxXLSCountStringsInSSTBucked = 0 then
        AddSSTBucket;
      Writer.XLS_WriteSharedString(TdxSpreadSheetSharedString(AItem));
      AddObject;
    end);
end;

procedure TdxSpreadSheetXLSWriter.WriteStyle(AStyle: TdxSpreadSheetCellStyleHandle; ACellStyle: Boolean = True;
  ADataFormat: Integer = -1);
const
  AAssignedState = $0400 or $0800 or $1000 or $2000 or $4000 or $8000;
  ADefaultValues: array[0..19] of Byte =
    ($05, $00, $00, $00, $F5, $FF, $20, $00, $00, $F8, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $20);

  function GetLineStyle(ASide: TcxBorder): Integer;
  begin
    Result := BorderLineStyleMap[AStyle.Borders.BorderStyle[ASide]];
  end;

  function GetLineColor(ASide: TcxBorder): Integer;
  begin
    Result := GetColorIndex(AStyle.Borders.BorderColor[ASide], $40, $40);
  end;

var
  ABackgroundColor: TColor;
  AForegroundColor: TColor;
begin
  RecordWriter.BeginWrite(brcXF);
  try
    Writer.SetSize(20);
    if Styles.Count > 0 then
    begin
      Writer.Words[0] := Fonts[AStyle.Font];
      if Writer.Words[0] > 4 then
        Writer.Words[0] := Writer.Words[0] + 1;
    end;

    if not ACellStyle then
      Writer.Words[4] := $FFF0 or $4
    else
      Writer.Words[2] := Formats.Items[AStyle.DataFormat];

    Writer.Words[4] := Writer.Words[4] or Byte(csLocked in AStyle.States) or Byte(csHidden in AStyle.States) shl 1;
    //
    Writer.Words[6] := Byte(AStyle.AlignHorz) or (Byte(csWordWrap in AStyle.States) shl 3) or (Byte(AStyle.AlignVert) shl 4);
    //
    Writer.Words[8] := (AStyle.AlignHorzIndent and $F) or (Byte(csShrinkToFit in AStyle.States) shl 4) or AAssignedState;
    //
    Writer.Words[10] := GetLineStyle(bLeft) or GetLineStyle(bRight) shl 4 or GetLineStyle(bTop) shl 8 or GetLineStyle(bBottom) shl 12;

    Writer.Words[12] := GetLineColor(bLeft) or GetLineColor(bRight) shl 7;

    Writer.Integers[14] := GetLineColor(bTop) or GetLineColor(bBottom) shl 7;
    if (AStyle.Brush.BackgroundColor <> clDefault) or (AStyle.Brush.ForegroundColor <> clDefault) then
    begin
      Writer.Integers[14] :=  Writer.Integers[14] or FillStyleMap[AStyle.Brush.Style] shl 26 or $02000000;
      // 02000000h = 1 when a subsequent XFEXT
      if (AStyle.Brush.Style = sscfsSolid) then
      begin
        AForegroundColor := AStyle.Brush.BackgroundColor;
        ABackgroundColor := AStyle.Brush.ForegroundColor;
      end
      else
      begin
        ABackgroundColor := AStyle.Brush.BackgroundColor;
        AForegroundColor := AStyle.Brush.ForegroundColor;
      end;
      Writer.Words[18] :=
        GetColorIndex(AForegroundColor, clDefault, $41, $41) or
        GetColorIndex(ABackgroundColor, clWindow, $40, $40) shl 7;
    end
    else
       Writer.Words[18] := $41 or $40 shl 7;
    //
    if ADataFormat <> -1 then  // Predefined XLS styles
    begin
      Move(ADefaultValues[0], Writer.Stream.Memory^, SizeOF(ADefaultValues));
      Writer.Words[2] := ADataFormat;
    end;
    //
    if FStyleID <= dxMaxXFStyles then
      CRC32.Add(RecordWriter.Memory, RecordWriter.Size);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
  AllStyles.Add(AStyle);
  if AStyle <> SpreadSheet.CellStyles.DefaultStyle then
    Styles.AddOrSetValue(AStyle, FStyleID)
  else
    Styles.AddOrSetValue(AStyle, 15);
  Inc(FStyleID);
end;

procedure TdxSpreadSheetXLSWriter.WriteStyles;
var
  I, ADataFormat: Integer;
begin

  for I := 0 to DefaultStylesRecordCount - 1 do
  begin
    case I of
      $12:
        ADataFormat := $09;
      $13:
        ADataFormat := $2C;
      $14:
        ADataFormat := $2B;
    else
      ADataFormat := -1;
    end;
    WriteStyle(SpreadSheet.CellStyles.DefaultStyle, I = 15, ADataFormat);
  end;

  TdxHashTableAccess(SpreadSheet.CellStyles).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      WriteStyle(TdxSpreadSheetCellStyleHandle(AItem));
      AddObject;
    end);

  // write XF CRC for using extended style properties
  RecordWriter.BeginWrite(brcXFCRC);
  try
    Writer.WriteWord($087C);
    Writer.WriteWord(0);
    Writer.WriteDateTime(0);
    Writer.WriteWord(0);
    Writer.WriteWord(Min(dxMaxXFStyles, AllStyles.Count));
    Writer.WriteCardinal(CRC32.crcValue);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
  //
  WriteStylesExtProperties;
end;

procedure TdxSpreadSheetXLSWriter.WriteStyleExtProperties(AID: Word; AStyle: TdxSpreadSheetCellStyleHandle);
var
  APropCount: Integer;

  procedure CheckColorPropValueValue(AColor: TColor; APropID: TdxXFExtPropertyID);
  var
    AIndex: Integer;
    AColorData: TdxXFExtColorProp;
  begin
    AIndex := GetColorIndex(AColor, -1, -1, -1);
    if (AColor = clDefault) or (AColor = clNone) or
      (AIndex = -1) or (Palette[AIndex] = ColorToRgb(AColor)) then Exit;
    Inc(APropCount);
    FillChar(AColorData, 0, SizeOf(AColorData));
    AColorData.xclrType := 2;
    AColorData.nTintShade := 0;

    with TRgbQuad(ColorToRgb(AColor)) do
      AColorData.xclrValue := dxColorToAlphaColor(Rgb(rgbRed, rgbGreen, rgbBlue), 255);

    Writer.WriteWord(Byte(APropID));
    Writer.WriteWord(SizeOf(AColorData) + 4);
    Writer.WriteBlock(AColorData, SizeOf(AColorData));
  end;

  procedure CheckIndentPropValueValue(AValue: Word);
  begin
    if AValue < 15 then Exit;
    Writer.WriteWord(Byte(xfextIndent));
    Writer.WriteWord(SizeOf(Word) + 4);
    Writer.WriteWord(AStyle.AlignHorzIndent);
    Inc(APropCount);
  end;

begin
  RecordWriter.BeginWrite(brcXFEXT);
  try
    APropCount := 0;
    Writer.WriteWord($087D);
    Writer.WriteWord(0);
    Writer.WriteDateTime(0);
    Writer.WriteWord(0);
    Writer.WriteWord(Min(dxMaxXFStyles, AID));
    Writer.WriteWord(0);
    Writer.WriteWord(0);
    //
    //
    if AStyle.Brush.Style = sscfsSolid then
      CheckColorPropValueValue(AStyle.Brush.BackgroundColor, xfextForeColor)
    else
    begin
      CheckColorPropValueValue(AStyle.Brush.ForegroundColor, xfextForeColor);
      CheckColorPropValueValue(AStyle.Brush.BackgroundColor, xfextBackColor)
    end;
    //
    CheckColorPropValueValue(AStyle.Font.Color, xfextTextColor);
    //
    CheckColorPropValueValue(AStyle.Borders.BorderColor[bTop], xfextBorderColorTop);
    CheckColorPropValueValue(AStyle.Borders.BorderColor[bLeft], xfextBorderColorLeft);
    CheckColorPropValueValue(AStyle.Borders.BorderColor[bRight], xfextBorderColorRight);
    CheckColorPropValueValue(AStyle.Borders.BorderColor[bBottom], xfextBorderColorBottom);
    //
    CheckIndentPropValueValue(AStyle.AlignHorzIndent);
    Writer.Words[18] := APropCount;
    if APropCount = 0 then
      RecordWriter.Cancel
    else
      Inc(ExtXFCount)
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteStylesExtProperties;
var
  I: Integer;
begin
  for I := 0 to AllStyles.Count - 1 do
    if ExtXFCount <= dxMaxXFStyles then
      WriteStyleExtProperties(I, AllStyles[I]);
end;

procedure TdxSpreadSheetXLSWriter.WriteStyleSheets;
const
  PredefinedStyles: array[0..5, 0..3] of Byte = (
   ($14, $80, $03, $FF),
   ($14, $80, $06, $FF),
   ($13, $80, $04, $FF),
   ($13, $80, $07, $FF),
   ($00, $80, $00, $FF),
   ($12, $80, $05, $FF)
  );
var
  I: Integer;
begin
  for I := 0 to High(PredefinedStyles) do
  begin
    RecordWriter.BeginWrite(brcSTYLE);
    try
      RecordWriter.WriteBuffer(PredefinedStyles[I], SizeOf(PredefinedStyles[I]));
    finally
      RecordWriter.EndWrite(WorkbookData);
    end;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteSupBook;
begin
  Writer.WriteWord(SpreadSheet.SheetCount);
  Writer.WriteWord($0401);
end;

procedure TdxSpreadSheetXLSWriter.WriteTabID;
var
  I: Integer;
begin
  for I := 0 to SpreadSheet.SheetCount - 1 do
    Writer.WriteWord(I + 1);
end;

procedure TdxSpreadSheetXLSWriter.WriteWindow1;
begin
  Writer.SetSize(18);
  Writer.Words[4] := 800 * 20;
  Writer.Words[6] := 600 * 20;
  Writer.Words[8] :=  Byte(SpreadSheet.OptionsView.HorizontalScrollBar) shl 3 or
    Byte(SpreadSheet.OptionsView.VerticalScrollBar) shl 4 or
    Byte(SpreadSheet.PageControl.Visible) shl 5;
  Writer.Words[10] := SpreadSheet.ActiveSheetIndex;
  Writer.Words[12] := SpreadSheet.PageControl.FirstVisiblePageIndex;
  Writer.Words[14] := $01;
  Writer.Words[16] := $0258;
end;

// TableView Part
procedure TdxSpreadSheetXLSWriter.WriteBOF;
begin
  WriteBOFInfo($10);
end;

procedure TdxSpreadSheetXLSWriter.WriteCalcCount;
begin
  if SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount = 0 then
    Writer.WriteWord(100)
  else
    Writer.WriteWord(SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount);
end;

procedure TdxSpreadSheetXLSWriter.WriteCalcMode;
begin
  Writer.WriteWord(Byte(SpreadSheet.OptionsBehavior.AutomaticCalculation));
end;

procedure TdxSpreadSheetXLSWriter.WriteCell(ARowIndex: Integer; ACell: TdxSpreadSheetCell);
const
  RecordType: array[TdxSpreadSheetCellDataType] of Word = (
    brcBlank, brcBoolErr, brcBoolErr, brcNumber, brcNumber, brcNumber, brcNumber, brcLabelSST, brcFORMULA
  );
var
  AController: TdxSpreadSheetCustomFormulaControllerAccess;
begin
  if (ARowIndex > dxMaxRow) or (ACell.ColumnIndex > dxMaxColumn) then
    Exit;

  AController := TdxSpreadSheetCustomFormulaControllerAccess(SpreadSheet.FormulaController);
  if AController.IsPartOfArrayFormula(CurrentSheet, ARowIndex, ACell.ColumnIndex) = afpSlaveCell then
    Exit;

  RecordWriter.BeginWrite(RecordType[ACell.DataType]);
  try
    Writer.WriteWord(ARowIndex);
    Writer.WriteWord(ACell.Index);
    Writer.WriteWord(Styles[ACell.StyleHandle]);
    case ACell.DataType of
       cdtInteger, cdtCurrency, cdtFloat, cdtDateTime:
         Writer.WriteDateTime(ACell.AsFloat);
       cdtString:
         Writer.WriteInteger(SST[ACell.AsSharedString]);
       cdtFormula:
         WriteCellFormula(ACell);
       cdtBoolean, cdtError:
         begin
           if ACell.DataType = cdtBoolean then
             Writer.WriteBoolean(ACell.AsBoolean)
           else
             Writer.WriteByte(ErrorCodeToErrorIndex[ACell.AsError]);
           Writer.WriteBoolean(ACell.DataType = cdtError);
         end;
    end;
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteCellArrayFormula(ACell: TdxSpreadSheetCell);
var
  AFormula: TdxSpreadSheetFormula;
begin
  RecordWriter.EndWrite(WorkbookData);
  RecordWriter.BeginWrite(brcARRAY);
  AFormula := ACell.AsFormula;
  Writer.WriteWord(ACell.RowIndex);
  Writer.WriteWord(ACell.RowIndex + AFormula.ArrayFormulaSize.cy - 1);
  Writer.WriteByte(ACell.ColumnIndex);
  Writer.WriteByte(ACell.ColumnIndex + AFormula.ArrayFormulaSize.cx - 1);
  Writer.WriteWord(3);     // Always calculate (options)
  Writer.WriteInteger(0);  // Reserved
  with TdxXLSFormulaWriter.Create(Self) do
  try
    WriteFormula(ACell);
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteCellFormula(ACell: TdxSpreadSheetCell);
var
  AValue: Variant;
begin
  AValue := Null;
  if ACell.AsFormula.IsArrayFormula or not WriteFormulaResult(ACell.AsFormula, AValue) then
    Writer.WriteDateTime(0);
  Writer.WriteWord(3);     // Always calculate (options)
  Writer.WriteInteger(0);  // Reserved
  with TdxXLSFormulaWriter.Create(Self) do
  try
    WriteFormula(ACell);
  finally
    Free;
  end;
  if ACell.AsFormula.IsArrayFormula then
    WriteCellArrayFormula(ACell);
  if VarIsStr(AValue) then
  begin
    RecordWriter.EndWrite(WorkbookData);
    RecordWriter.BeginWrite(brcSTRING);
    Writer.XLS_WriteSimpleString(AValue, 2);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteCells;
var
  ACell: TdxSpreadSheetCellAccess;
  ARow: TdxSpreadSheetRowAccess;
begin
  ARow := TdxSpreadSheetRowAccess(CurrentSheet.Rows.First);
  while ARow <> nil do
  begin
    if ARow.Index > dxMaxRow then
      Break;
    ACell := TdxSpreadSheetCellAccess(ARow.RowCells.First);
    while ACell <> nil do
    begin
      WriteCell(ARow.Index, ACell);
      ACell := TdxSpreadSheetCellAccess(ACell.FNext);
      AddObject;
    end;
    ARow := TdxSpreadSheetRowAccess(ARow.Next);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteColumnFormatInformation(
  AStartColumn, AFinishColumn: TdxSpreadSheetTableColumn);
var
  AGroup: TdxSpreadSheetTableItemGroup;
  AOptions: Word;
begin
  if AStartColumn.Index > dxMaxColumn then
    Exit;
  RecordWriter.BeginWrite(brcCOLINFO);
  try
    Writer.WriteWord(CheckColumn(AStartColumn.Index));
    Writer.WriteWord(CheckColumn(AFinishColumn.Index));
    Writer.WriteWord(MulDiv(TdxSpreadSheetItemsAccess(CurrentSheet.Columns).GetRealItemSize(AStartColumn.Index),
      256, ColumnWidthHelper.MaxDigitWidth));
    Writer.WriteWord(Styles[AStartColumn.Style.Handle]);
    AOptions := Byte(not AStartColumn.Visible);
    AGroup := CurrentSheet.Columns.Groups.Find(AStartColumn.Index);
    if AGroup <> nil then
      AOptions := (((AGroup.Level + 1) and $7) or (Byte(not AGroup.Expanded) shl 4)) shl 8 or AOptions;
    Writer.WriteWord(AOptions);
    Writer.WriteWord(0);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteColumns;

  function IsColumnsEqual(AColumn1, AColumn2: TdxSpreadSheetTableColumn): Boolean;
  begin
    Result := (AColumn2 <> nil) and (AColumn1.Size = AColumn2.Size) and (AColumn1.Visible = AColumn2.Visible) and
      (AColumn1.Style.Handle = AColumn2.Style.Handle) and (AColumn1.DefaultSize = AColumn2.DefaultSize) and
      (CurrentSheet.Columns.Groups.Find(AColumn1.Index) = CurrentSheet.Columns.Groups.Find(AColumn2.Index));
  end;

var
  AFirstColumn, ALastColumn: TdxSpreadSheetTableColumn;
begin
  AFirstColumn := TdxSpreadSheetTableColumn(CurrentSheet.Columns.First);
  while AFirstColumn <> nil do
  begin
    ALastColumn := AFirstColumn;
    while IsColumnsEqual(AFirstColumn, TdxSpreadSheetTableColumn(ALastColumn.Next)) do
      ALastColumn := TdxSpreadSheetTableColumn(ALastColumn.Next);
    WriteColumnFormatInformation(AFirstColumn, ALastColumn);
    AFirstColumn := TdxSpreadSheetTableColumn(ALastColumn.Next);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteConditionalFormatting;
var
  AAreaIndex: Integer;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  ARuleIndex: Integer;
begin
  for ARuleIndex := 0 to CurrentSheet.ConditionalFormatting.RuleCount - 1 do
  begin
    ARule := CurrentSheet.ConditionalFormatting.Rules[ARuleIndex];

    if (ARule is TdxSpreadSheetConditionalFormattingRuleExpression) and (ARule.Areas.Count > 0) then
    begin
      RecordWriter.BeginWrite(brcCONDFMT);
      try
        Writer.WriteWord(1);
        Writer.WriteWord(0);
        Writer.XLS_WriteRef(ARule.Area);
        Writer.WriteWord(ARule.Areas.Count);
        for AAreaIndex := 0 to ARule.Areas.Count - 1 do
          Writer.XLS_WriteRef(ARule.Areas[AAreaIndex]);
      finally
        RecordWriter.EndWrite(WorkbookData);
      end;
      WriteConditionalFormattingRule(TdxSpreadSheetConditionalFormattingRuleExpression(ARule));
    end;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteConditionalFormattingRule(ARule: TdxSpreadSheetConditionalFormattingRuleExpression);

  function WriteFormula(AFormula: TdxSpreadSheetCustomFormula): Integer;
  var
    ASavedPosition: Int64;
    AWriter: TdxXLSFormulaWriter;
  begin
    Result := 0;
    if AFormula <> nil then
    begin
      ASavedPosition := Writer.Stream.Position;
      AWriter := TdxXLSFormulaWriter.Create(Self, False);
      try
        AWriter.WriteFormula(AFormula, False, 0, 0);
      finally
        AWriter.Free;
      end;
      Result := Writer.Stream.Position - ASavedPosition;
    end;
  end;

const
  OperatorMap: array[TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator] of Integer = (1, 3, 5, 7, 6, 8, 2, 4);
var
  AOptions: Cardinal;
  ASavedPosition: Int64;
  ASize1: Integer;
  ASize2: Integer;
  AStartPosition: Int64;
begin
  RecordWriter.BeginWrite(brcCF);
  try
    AOptions := 0;

    if ARule is TdxSpreadSheetConditionalFormattingRuleCellIs then
    begin
      Writer.WriteByte(1);
      Writer.WriteByte(OperatorMap[TdxSpreadSheetConditionalFormattingRuleCellIs(ARule).ComparisonOperator]);
    end
    else
    begin
      Writer.WriteByte(2);
      Writer.WriteByte(0);
    end;

    AStartPosition := Writer.Stream.Position;
    Writer.WriteWord(0);
    Writer.WriteWord(0);
    Writer.WriteCardinal(0);
    Writer.WriteWord(0);

    WriteConditionalFormattingRuleStyle(AOptions, ARule.Style);
    ASize1 := WriteFormula(TdxSpreadSheetConditionalFormattingRuleExpressionAccess(ARule).Formulas[0]);
    ASize2 := WriteFormula(TdxSpreadSheetConditionalFormattingRuleExpressionAccess(ARule).Formulas[1]);

    ASavedPosition := Writer.Stream.Position;
    try
      Writer.Stream.Position := AStartPosition;
      Writer.WriteWord(ASize1);
      Writer.WriteWord(ASize2);
      Writer.WriteCardinal(AOptions);
    finally
      Writer.Stream.Position := ASavedPosition;
    end;
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteConditionalFormattingRuleStyle(var AOptions: Cardinal; AStyle: TdxSpreadSheetCellStyle);
var
  ADefaultStyle: TdxSpreadSheetCellStyle;
begin
  ADefaultStyle := SpreadSheet.DefaultCellStyle;
  if not AStyle.Handle.Font.IsEqual(ADefaultStyle.Handle.Font) then
  begin
    AOptions := AOptions or CFRULE_OPTION_FONT;
    WriteConditionalFormattingRuleStyleFont(ADefaultStyle.Handle.Font, AStyle.Handle.Font);
  end;

  if not AStyle.Handle.Borders.IsEqual(ADefaultStyle.Handle.Borders) then
  begin
    AOptions := AOptions or CFRULE_OPTION_BORDER;
    WriteConditionalFormattingRuleStyleBorders(AOptions, ADefaultStyle.Handle.Borders, AStyle.Handle.Borders);
  end;

  if not AStyle.Handle.Brush.IsEqual(ADefaultStyle.Handle.Brush) then
  begin
    AOptions := AOptions or CFRULE_OPTION_PATTERN;
    WriteConditionalFormattingRuleStyleBrush(AOptions, ADefaultStyle.Handle.Brush, AStyle.Handle.Brush);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteConditionalFormattingRuleStyleBorders(
  var AOptions: Cardinal; ADefaultBorders, ABorders: TdxSpreadSheetBordersHandle);

  function CheckNeedWriteBorder(ASide: TcxBorder): Boolean;
  begin
    Result :=
      (ADefaultBorders.BorderColor[ASide] <> ABorders.BorderColor[ASide]) or
      (ADefaultBorders.BorderStyle[ASide] <> ABorders.BorderStyle[ASide]);
  end;

  function GetLineColor(ASide: TcxBorder): Integer;
  begin
    Result := GetColorIndex(ABorders.BorderColor[ASide], $40, $40);
  end;

const
  Offsets: array[TcxBorder] of Byte = (0, 8, 4, 12);
  OptionFlags: array[TcxBorder] of Cardinal = (
    CFRULE_OPTION_BORDER_LEFT, CFRULE_OPTION_BORDER_TOP,
    CFRULE_OPTION_BORDER_RIGHT, CFRULE_OPTION_BORDER_BOTTOM
  );
var
  AFlags: Word;
  ASide: TcxBorder;
begin
  AOptions := AOptions or CFRULE_OPTION_BORDER_BLTR or CFRULE_OPTION_BORDER_TLBR;

  AFlags := 0;
  for ASide := Low(ASide) to High(ASide) do
  begin
    if CheckNeedWriteBorder(ASide) then
      AFlags := AFlags or (BorderLineStyleMap[ABorders.BorderStyle[ASide]] shl Offsets[ASide])
    else
      AOptions := AOptions or OptionFlags[ASide];
  end;
  Writer.WriteWord(AFlags);

  AFlags := 0;
  if CheckNeedWriteBorder(bLeft) then
    AFlags := AFlags or GetLineColor(bLeft);
  if CheckNeedWriteBorder(bRight) then
    AFlags := AFlags or GetLineColor(bRight) shl 7;
  Writer.WriteWord(AFlags);

  AFlags := 0;
  if CheckNeedWriteBorder(bTop) then
    AFlags := AFlags or GetLineColor(bTop);
  if CheckNeedWriteBorder(bBottom) then
    AFlags := AFlags or GetLineColor(bBottom) shl 7;
  Writer.WriteInteger(AFlags);
end;

procedure TdxSpreadSheetXLSWriter.WriteConditionalFormattingRuleStyleBrush(
  var AOptions: Cardinal; ADefaultBrush, ABrush: TdxSpreadSheetBrushHandle);
var
  AFlags: Word;
begin
  // Style
  AFlags := 0;
  if ABrush.Style <> ADefaultBrush.Style then
    AFlags := FillStyleMap[ABrush.Style] shl 10
  else
    AOptions := AOptions or CFRULE_OPTION_PATTERN_STYLE;
  Writer.WriteWord(AFlags);

  // Colors
  AFlags := 0;
  if ABrush.BackgroundColor <> ADefaultBrush.BackgroundColor then
    AFlags := AFlags or GetColorIndex(ABrush.BackgroundColor, 0, $41, $41) shl 7
  else
    AOptions := AOptions or CFRULE_OPTION_PATTERN_BGCOLOR;

  if ABrush.ForegroundColor <> ADefaultBrush.ForegroundColor then
    AFlags := AFlags or (GetColorIndex(ABrush.ForegroundColor, clWindow, $40, $40))
  else
    AOptions := AOptions or CFRULE_OPTION_PATTERN_FGCOLOR;

  Writer.WriteWord(AFlags);
end;

procedure TdxSpreadSheetXLSWriter.WriteConditionalFormattingRuleStyleFont(ADefaultFont, AFont: TdxSpreadSheetFontHandle);
var
  AFontName: array[0..63] of Byte;
  AFontOptions: Cardinal;
begin
  ZeroMemory(@AFontName[0], SizeOf(AFontName));
  if AFont.Name <> ADefaultFont.Name then
  begin
    AFontName[0] := Min(Length(AFont.Name), (Length(AFontName) - 2) div SizeOf(WideChar));
    AFontName[1] := 1;
    Move(AFont.Name[1], AFontName[2], AFontName[0] * SizeOf(WideChar));
  end;
  Writer.WriteBlock(AFontName);

  if AFont.Size <> ADefaultFont.Size then
    Writer.WriteInteger(AFont.Size * 20)
  else
    Writer.WriteInteger(-1);

  Writer.WriteInteger(IfThen(fsItalic in AFont.Style, $2) or IfThen(fsStrikeOut in AFont.Style, $8));
  Writer.WriteWord(IfThen(fsBold in AFont.Style, $2BC, $190));
  Writer.WriteWord(0);
  Writer.WriteByte(Ord(fsUnderline in AFont.Style));

  Writer.WriteByte(0);
  Writer.WriteByte(0);
  Writer.WriteByte(0);

  if AFont.Color <> ADefaultFont.Color then
    Writer.WriteInteger(GetColorIndex(AFont.Color, 0, 0, -1))
  else
    Writer.WriteInteger(-1);

  AFontOptions := $FFFFFFFF;
  if [fsStrikeOut] * AFont.Style <> [] then
    AFontOptions := AFontOptions and not CFRULE_FONT_FLAG_OUTLINE;
  if [fsItalic, fsBold] * AFont.Style <> [] then
    AFontOptions := AFontOptions and not (CFRULE_FONT_FLAG_STYLE or $1);

  Writer.WriteCardinal(0);
  Writer.WriteCardinal(AFontOptions);
  Writer.WriteCardinal(1);
  Writer.WriteCardinal(Ord(not (fsUnderline in AFont.Style)));
  Writer.WriteCardinal(0);
  Writer.WriteCardinal(0);
  Writer.WriteInt64(0);
  Writer.WriteWord(1);
end;

procedure TdxSpreadSheetXLSWriter.WriteDefaultColumnWidth;
begin
  Writer.WriteWord(MulDiv(CurrentSheet.Options.DefaultColumnWidth, 256, ColumnWidthHelper.MaxDigitWidth));
end;

procedure TdxSpreadSheetXLSWriter.WriteDefaultRowHeight;
begin
  Writer.WriteWord(1);
  Writer.WriteWord(Round(TdxValueUnitsHelper.PixelsToPoints(CurrentSheet.Options.DefaultRowHeight) * 20));
end;

procedure TdxSpreadSheetXLSWriter.WriteDimension;
begin
  Writer.WriteInteger(CheckRow(CurrentSheetDimensions.Top));
  Writer.WriteInteger(CheckRow(CurrentSheetDimensions.Bottom + 1));
  Writer.WriteWord(CheckColumn(CurrentSheetDimensions.Left));
  Writer.WriteWord(CheckColumn(CurrentSheetDimensions.Right + 1));
  Writer.WriteWord(0); // reserved
end;

procedure TdxSpreadSheetXLSWriter.WriteEOF;
begin
  // EOF record is empty
end;

procedure TdxSpreadSheetXLSWriter.WriteFooter;
begin
{  CurrentSheet.OptionsPrint.HeaderFooter.CommonHeader.
  TdxSpreadSheetHeaderFooterHelper.Parse(
    CurrentSheet.OptionsPrint.HeaderFooter.CommonFooter,
    Reader.XLS_ReadString(Reader.ReadByte, Reader.ReadWord));}
end;

procedure TdxSpreadSheetXLSWriter.WriteGuts;

  function GetMaxLevelValue(AGroups: TdxSpreadSheetTableItemGroups): Integer;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 0 to AGroups.Count - 1 do
      Result := Max(Result, TdxSpreadSheetTableItemGroupAccess(AGroups.Items[I]).MaxNestingLevel + 1);
  end;

  function GetHeaderSize(ALevel: Word): Word;
  begin
    Result := 0;
    if ALevel = 0 then
      Exit;
    Result := (ALevel - 1) * 14 + 10;
  end;

var
  AMaxColumnsLevel, AMaxRowsLevel: Word;
begin
  AMaxRowsLevel := GetMaxLevelValue(CurrentSheet.Rows.Groups);
  AMaxColumnsLevel := GetMaxLevelValue(CurrentSheet.Columns.Groups);
  Writer.WriteWord(GetHeaderSize(AMaxRowsLevel));
  Writer.WriteWord(GetHeaderSize(AMaxColumnsLevel));
  Writer.WriteWord(AMaxRowsLevel);
  Writer.WriteWord(AMaxColumnsLevel);
end;

procedure TdxSpreadSheetXLSWriter.WriteHeader;
begin
{  CurrentSheet.OptionsPrint.HeaderFooter.CommonHeader.
  TdxSpreadSheetHeaderFooterHelper.Parse(
    CurrentSheet.OptionsPrint.HeaderFooter.CommonFooter,
    Reader.XLS_ReadString(Reader.ReadByte, Reader.ReadWord));}
end;

procedure TdxSpreadSheetXLSWriter.WriteHyperlink(AHyperlink: TdxSpreadSheetHyperlink);
var
  AHelper: TdxBIFFHyperlinkHelper;
begin
  RecordWriter.BeginWrite(brcHLink);
  try
    Writer.XLS_WriteRef(AHyperlink.Area);
    AHelper := TdxBIFFHyperlinkHelper.Create(AHyperlink);
    try
      AHelper.SaveToStream(RecordWriter);
    finally
      AHelper.Free;
    end;
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteHyperlinks;
var
  ALink: TdxSpreadSheetHyperlink;
begin
  ALink := CurrentSheet.Hyperlinks.First;
  while ALink <> nil do
  begin
    if ALink.IsAreaCorrect then
    begin
      WriteHyperlink(ALink);
      WriteHyperlinkTooltip(ALink);
    end;
    ALink := ALink.Next;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteHyperlinkTooltip(AHyperlink: TdxSpreadSheetHyperlink);
var
  S: string;
begin
  if not (havScreenTip in AHyperlink.AssignedValues) then
    Exit;
  RecordWriter.BeginWrite(brcHLINKTOOLTIP);
  try
    Writer.WriteWord(brcHLINKTOOLTIP);
    Writer.XLS_WriteRef(AHyperlink.Area);
    S := AHyperlink.ScreenTip;
    Writer.WriteBlock(S[1], (Length(S) + 1) * 2)
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteIndex;
begin
  Writer.SetSize(16);
  Writer.Integers[4] := CheckRow(CurrentSheetDimensions.Top);
  Writer.Integers[8] := CheckRow(CurrentSheetDimensions.Bottom + 1);
end;

procedure TdxSpreadSheetXLSWriter.WriteIteration;
begin
  Writer.WriteWord(Byte(SpreadSheet.OptionsBehavior.IterativeCalculation));
end;

procedure TdxSpreadSheetXLSWriter.WritePanes;
const
  FrozenPaneFlags: array[Boolean, Boolean] of Word = ((0, 2), (1, 0));
begin
  if (CurrentSheet.FrozenColumn < 0) and (CurrentSheet.FrozenRow < 0) then
  begin
    RecordWriter.Cancel;
    Exit;
  end;
  Writer.WriteWord(CurrentSheet.FrozenColumn + 1);
  Writer.WriteWord(CurrentSheet.FrozenRow + 1);
  Writer.WriteWord(CurrentSheet.FrozenRow + 1);
  Writer.WriteWord(CurrentSheet.FrozenColumn + 1);
  Writer.WriteWord(FrozenPaneFlags[CurrentSheet.FrozenColumn >= 0, CurrentSheet.FrozenRow >= 0]);
end;

procedure TdxSpreadSheetXLSWriter.WritePassword;
var
  AProtection: IdxSpreadSheetProtectionInfo;
begin
  if CurrentSheet <> nil then
    AProtection := CurrentSheet.OptionsProtection.ProtectionInfo
  else
    AProtection := SpreadSheet.OptionsProtection.ProtectionInfo;

  if (AProtection is TdxSpreadSheetCustomProtectionInfo) and not (AProtection is TdxSpreadSheetStandardProtectionInfo) then
    AProtection := TdxSpreadSheetStandardProtectionInfo.Create(TdxSpreadSheetCustomProtectionInfo(AProtection).Password);
  if (AProtection is TdxSpreadSheetStandardProtectionInfo) then
    Writer.WriteWord(TdxSpreadSheetStandardProtectionInfo(AProtection).KeyWord)
  else
    RecordWriter.Cancel;
end;

procedure TdxSpreadSheetXLSWriter.WriteMergedCells;

  function IsAreaValid(const R: TRect): Boolean;
  begin
    Result := ((R.Left >= 0) and (R.Top >= 0)) and
      ((R.Left <> R.Right) or (R.Top <> R.Bottom)) and
      ((R.Left <= R.Right) or (R.Top <= R.Bottom)) and ((R.Left <= dxMaxColumn) and (R.Top < dxMaxRow));
  end;

var
  C, I: Integer;
  ACell: TdxSpreadSheetMergedCell;
const
  AMaxMergedInfoCount = 1020;
begin
  C := 0;
  I := 0;
  // check invalid or single cell references
  ACell := CurrentSheet.MergedCells.First;
  while ACell <> nil do
  begin
    if IsAreaValid(ACell.Area) then
      Inc(C);
    ACell := ACell.Next;
  end;

  if C > 0 then
  begin
    Writer.WriteWord(Min(C - I, AMaxMergedInfoCount));
    ACell := CurrentSheet.MergedCells.First;
    while ACell <> nil do
    begin
      if (I > 0) and ((I mod AMaxMergedInfoCount) = 0) then
      begin
        RecordWriter.EndWrite(WorkbookData);
        RecordWriter.BeginWrite(brcMergeCells);
        Writer.WriteWord(Min(C - I, AMaxMergedInfoCount));
      end;
      if IsAreaValid(ACell.Area) then
      begin
        Writer.XLS_WriteRef(ACell.Area);
        Inc(I);
      end;
      ACell := ACell.Next;
    end;
  end
  else
    RecordWriter.Cancel;
end;

procedure TdxSpreadSheetXLSWriter.WriteMSODrawingObjectInfo(AContainer: TdxSpreadSheetContainer);

  function GetTypeID: Word;
  const
    ShapeID: array[TdxSpreadSheetShapeType] of Byte = (2, 2, 3);
  begin
    if AContainer.ClassType = TdxSpreadSheetTextBoxContainer then
      Result := 6
    else

    if AContainer.ClassType = TdxSpreadSheetPictureContainer then
      Result := 8
    else

    if AContainer.ClassType = TdxSpreadSheetCommentContainer then
      Result := $19
    else
      Result := ShapeID[(AContainer as TdxSpreadSheetShapeContainer).Shape.ShapeType];
  end;

const
  fLocked   = $0001;
  fPrint    = $0010;
  fAutoFill = $2000;
  fAutoLine = $4000;
begin
  Writer.WriteWord(ftCmo);
  Writer.WriteWord(18);

  Writer.WriteWord(GetTypeID);
  Writer.WriteWord(GetContainerID(AContainer));
  Writer.WriteWord(fLocked or fPrint or fAutoFill or fAutoLine);
  Writer.WriteInteger(0);
  Writer.WriteInteger(0);
  Writer.WriteInteger(0);

  if GetTypeID = 8 then
  begin
    Writer.WriteWord(7);
    Writer.WriteWord(2);
    Writer.WriteWord($FFFF);
    //
    Writer.WriteWord(8);
    Writer.WriteWord(2);
    Writer.WriteWord(1);
  end;

  Writer.WriteWord(0);
  Writer.WriteWord(0);
end;

procedure TdxSpreadSheetXLSWriter.WriteMSODrawing(AContainer: TdxSpreadSheetContainer; var AIndex, ASize: Integer);
var
  AMSODrawingWriter: TdxSpreadSheetMSODrawingWriter;
begin
  AddObject;
  if GetContainerID(AContainer) <= 0 then
    Exit;

  RecordWriter.BeginWrite(brcMSODRAWING);
  try
    AMSODrawingWriter := TdxSpreadSheetMSODrawingWriter.Create(Self);
    try
      AMSODrawingWriter.WriteContainer(AContainer, AIndex = 0, ASize);
      Inc(AIndex);
    finally
      AMSODrawingWriter.Free;
    end;
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;

  RecordWriter.BeginWrite(brcOBJ);
  try
    WriteMSODrawingObjectInfo(AContainer);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;

  if AContainer is TdxSpreadSheetCustomTextBoxContainer then
    WriteMSOTextObject(TdxSpreadSheetCustomTextBoxContainerAccess(AContainer).TextBox);
end;

procedure TdxSpreadSheetXLSWriter.WriteMSODrawings;
var
  I, AIndex, ASize, APosition: Integer;
begin
  ASize := 0;
  AIndex := 0;
  APosition := WorkbookData.Position;
  for I := 0 to CurrentSheet.Containers.Count - 1 do
    WriteMSODrawing(CurrentSheet.Containers[I], AIndex, ASize);

  if ASize > 0 then
  begin
    PInteger(@PByteArray(WorkbookData.Memory)^[APosition + 8])^ := ASize - 8;
    PInteger(@PByteArray(WorkbookData.Memory)^[APosition + 32])^ := ASize - 32;
  end
  else
    RecordWriter.Cancel;
end;

procedure TdxSpreadSheetXLSWriter.WriteMSOTextObject(ATextBox: TdxSpreadSheetCustomTextBox);

  function EncodeAlignFlags: Word;
  const
    AlignHorzMap: array[TAlignment] of Byte = (1, 3, 2);
    AlignVertMap: array[TVerticalAlignment] of Byte = (1, 3, 2);
  begin
    Result := (AlignHorzMap[ATextBox.AlignHorz] shl 1) or (AlignVertMap[ATextBox.AlignVert] shl 4);
  end;

var
  ATextLength: Word;
begin
  ATextLength := Length(ATextBox.TextAsString);

  RecordWriter.BeginWrite(brcMSODRAWING);
  try
    Writer.WriteCardinal($F00D0000);
    Writer.WriteInteger(0);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;

  RecordWriter.BeginWrite(brcTXO);
  try
    Writer.WriteWord(EncodeAlignFlags);
    Writer.WriteWord(0);
    Writer.WriteWord(0);
    Writer.WriteInteger(0);
    Writer.WriteWord(ATextLength);
    Writer.WriteWord(8 * IfThen(ATextLength > 0, 2));
    Writer.WriteInteger(0);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;

  if ATextLength > 0 then
  begin
    RecordWriter.BeginWrite(brcCONTINUE);
    try
      Writer.WriteBoolean(SizeOf(Char) > 1);
      Writer.WriteBlock(ATextBox.TextAsString[1], ATextLength * SizeOf(Char));
    finally
      RecordWriter.EndWrite(WorkbookData);
    end;

    RecordWriter.BeginWrite(brcCONTINUE);
    try
      Writer.WriteWord(0);
      Writer.WriteWord(Fonts[ATextBox.Font.Handle] + 1);
      Writer.WriteInteger(0);

      Writer.WriteWord(ATextLength);
      Writer.WriteWord(0);
      Writer.WriteInteger(0);
    finally
      RecordWriter.EndWrite(WorkbookData);
    end;
  end;
end;

procedure TdxSpreadSheetXLSWriter.WritePageBreaksHorizontal;
var
  I: Integer;
  ABreaks: TList<Cardinal>;
begin
  ABreaks := CurrentSheet.OptionsPrint.Pagination.RowPageBreaks;
  if ABreaks.Count = 0 then
    RecordWriter.Cancel
  else
    Writer.WriteWord(ABreaks.Count);

  for I := 0 to ABreaks.Count - 1 do
  begin
    Writer.WriteWord(ABreaks[I]);
    Writer.WriteWord(0);
    Writer.WriteWord(CheckColumn(CurrentSheetDimensions.Right + 1));
  end;
end;

procedure TdxSpreadSheetXLSWriter.WritePageBreaksVertical;
var
  I: Integer;
  ABreaks: TList<Cardinal>;
begin
  ABreaks := CurrentSheet.OptionsPrint.Pagination.ColumnPageBreaks;
  if ABreaks.Count = 0 then
    RecordWriter.Cancel
  else
    Writer.WriteWord(ABreaks.Count);

  for I := 0 to ABreaks.Count - 1 do
  begin
    Writer.WriteWord(ABreaks[I]);
    Writer.WriteWord(0);
    Writer.WriteWord(MAXWORD);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WritePrintBottomMargin;
begin
  Writer.WriteDateTime(CurrentSheet.OptionsPrint.Page.Margins.Bottom);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintCenterH;
begin
  WriteDefaultBoolean(CurrentSheet.OptionsPrint.Printing.HorizontalCentered);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintCenterV;
begin
  WriteDefaultBoolean(CurrentSheet.OptionsPrint.Printing.VerticalCentered);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintFooter;
var
  AText: string;
begin
  AText := TdxSpreadSheetHeaderFooterHelper.Build(CurrentSheet.OptionsPrint.HeaderFooter.CommonFooter);
  if AText = '' then
    RecordWriter.Cancel
  else
    Writer.XLS_WriteSimpleString(AText, 2);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintGridLines;
begin
  WriteDefaultBoolean(CurrentSheet.OptionsPrint.Source.GridLines);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintHeader;
var
  AText: string;
begin
  AText := TdxSpreadSheetHeaderFooterHelper.Build(CurrentSheet.OptionsPrint.HeaderFooter.CommonHeader);
  if AText = '' then
    RecordWriter.Cancel
  else
    Writer.XLS_WriteSimpleString(AText, 2);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintHeaders;
begin
  WriteDefaultBoolean(CurrentSheet.OptionsPrint.Source.Headers);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintLeftMargin;
begin
  Writer.WriteDateTime(CurrentSheet.OptionsPrint.Page.Margins.Left);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintRightMargin;
begin
  Writer.WriteDateTime(CurrentSheet.OptionsPrint.Page.Margins.Right);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintSetup;
var
  AOptions: Word;
begin
  AOptions := $80;
  if CurrentSheet.OptionsPrint.Printing.PageOrder = opppOverThenDown then
    AOptions := AOptions or 1;

  if CurrentSheet.OptionsPrint.Page.Orientation = oppoPortrait then
    AOptions := AOptions or 2;

  if CurrentSheet.OptionsPrint.Page.Paper.Assigned then
    Writer.WriteWord(CurrentSheet.OptionsPrint.Page.Paper.SizeID)
  else
    Writer.WriteWord(0);

  Writer.WriteWord(CurrentSheet.OptionsPrint.Page.Scale);
  Writer.WriteWord(CurrentSheet.OptionsPrint.Page.FirstPageNumber);
  Writer.WriteWord(CurrentSheet.OptionsPrint.Page.FitToWidth);
  Writer.WriteWord(CurrentSheet.OptionsPrint.Page.FitToHeight);
  Writer.WriteWord(AOptions);
  //
  Writer.WriteWord(0);
  Writer.WriteWord(0);
  //
  Writer.WriteDateTime(CurrentSheet.OptionsPrint.Page.Margins.Header);
  Writer.WriteDateTime( CurrentSheet.OptionsPrint.Page.Margins.Footer);

  Writer.WriteWord(CurrentSheet.OptionsPrint.Printing.Copies);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintTopMargin;
begin
  Writer.WriteDateTime(CurrentSheet.OptionsPrint.Page.Margins.Top);
end;

procedure TdxSpreadSheetXLSWriter.WritePrintWorkspaceInfo;
var
  AOptions: Word;
begin
  AOptions := 0;
  if CurrentSheet.OptionsPrint.Page.ScaleMode <> oppsmAdjustToScale then
    AOptions := AOptions or $100;
  if CurrentSheet.Rows.Groups.ExpandButtonPosition = gebpGroupFinish then
    AOptions := AOptions or $40;
  if CurrentSheet.Columns.Groups.ExpandButtonPosition = gebpGroupFinish then
    AOptions := AOptions or $80;
  Writer.WriteWord(AOptions);
end;

procedure TdxSpreadSheetXLSWriter.WriteRefMode;
begin
  Writer.WriteWord(Byte(not SpreadSheet.OptionsView.R1C1Reference));
end;

procedure TdxSpreadSheetXLSWriter.WriteRowFormatInformation(ARow: TdxSpreadSheetTableRow);
var
  AGroup: TdxSpreadSheetTableItemGroup;
  AOptions: Word;
  ASize: Integer;
begin
  AOptions := 320 or
    Byte(TdxSpreadSheetRowAccess(ARow).IsCustomSize) shl 6 or
    Byte(ARow.Style.Handle <> SpreadSheet.DefaultCellStyle.Handle) shl 7;

  RecordWriter.BeginWrite(brcROW);
  try
    Writer.WriteWord(ARow.Index);
    Writer.WriteWord(CheckColumn(CurrentSheetDimensions.Left));
    Writer.WriteWord(CheckColumn(CurrentSheetDimensions.Right));

    ASize := TdxSpreadSheetItemsAccess(CurrentSheet.Rows).GetRealItemSize(ARow.Index);
    if (ASize < 1) or not ARow.Visible then
      AOptions := AOptions or $80 or $20;
//    if (ASize <= 2) or not ARow.Visible then
//      AOptions := AOptions or $80 or $20;
//    if (ASize <= 2) then
//      ASize := Max(2, ARow.Owner.DefaultSize);

    AGroup := CurrentSheet.Rows.Groups.Find(ARow.Index);
    if AGroup <> nil then
      AOptions := AOptions or ((AGroup.Level + 1) and $7) or Byte(not AGroup.Expanded) shl 4;
    Writer.WriteWord(Round(TdxValueUnitsHelper.PixelsToPoints(ASize) * 20));
    Writer.WriteInteger(0);
    Writer.WriteWord(AOptions);
    Writer.WriteWord(Styles[ARow.Style.Handle]);
  finally
    RecordWriter.EndWrite(WorkbookData);
  end;
  AddObject;
end;

procedure TdxSpreadSheetXLSWriter.WriteRows;
var
  ARow: TdxSpreadSheetTableRow;
begin
  ARow := TdxSpreadSheetTableRow(CurrentSheet.Rows.First);
  while ARow <> nil do
  begin
    if ARow.Index > dxMaxRow then
      Break;
    WriteRowFormatInformation(ARow);
    ARow := TdxSpreadSheetTableRow(ARow.Next);
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteSelection;
var
  AIndex: Integer;
begin
  Writer.WriteByte(3);
  Writer.WriteWord(CheckRow(CurrentSheet.Selection.FocusedRow));
  Writer.WriteWord(CheckColumn(CurrentSheet.Selection.FocusedColumn));
  if CurrentSheet.Selection.Count > 0 then
  begin
    Writer.WriteWord(CurrentSheet.Selection.Count);
    Writer.WriteWord(CurrentSheet.Selection.Count);
    for AIndex := 0 to CurrentSheet.Selection.Count - 1 do
      Writer.XLS_WriteSimpleRef(CurrentSheet.Selection.Items[AIndex].Rect);
  end
  else
  begin
    Writer.WriteWord(1);
    Writer.WriteWord(1);
    Writer.XLS_WriteSimpleRef(cxRectBounds(CurrentSheet.Selection.FocusedColumn, CurrentSheet.Selection.FocusedRow, 0, 0));
  end;
end;

procedure TdxSpreadSheetXLSWriter.WriteSharedFeatures;
begin
  Writer.WriteWord(brcFEATHEADR);
  Writer.WriteWord(0);
  Writer.WriteInt64(0);
  Writer.WriteWord(2); // Type: Enhanced Protection
  Writer.WriteByte(1);
  Writer.WriteInteger(-1);
  Writer.WriteInteger(
    IfThen(CurrentSheet.OptionsProtection.AllowDeleteColumns, iprotDeleteColumns) or
    IfThen(CurrentSheet.OptionsProtection.AllowDeleteRows, iprotDeleteRows) or
    IfThen(CurrentSheet.OptionsProtection.AllowResizeColumns, iprotFormatColumns) or
    IfThen(CurrentSheet.OptionsProtection.AllowEditContainers, iprotObject) or
    IfThen(CurrentSheet.OptionsProtection.AllowEditHyperlinks, iprotInsertHyperlinks) or
    IfThen(CurrentSheet.OptionsProtection.AllowResizeRows, iprotFormatRows) or
    IfThen(CurrentSheet.OptionsProtection.AllowFormatCells, iprotFormatCells) or
    IfThen(CurrentSheet.OptionsProtection.AllowInsertColumns, iprotInsertColumns) or
    IfThen(CurrentSheet.OptionsProtection.AllowInsertRows, iprotInsertRows) or
    IfThen(CurrentSheet.OptionsProtection.AllowSelectLockedCells, iprotSelLockedCells) or
    IfThen(CurrentSheet.OptionsProtection.AllowSelectUnlockedCells, iprotSelUnlockedCells) or
    IfThen(CurrentSheet.OptionsProtection.AllowSort, iprotSort)
  );
end;

procedure TdxSpreadSheetXLSWriter.WriteWindow2;
begin
   Writer.SetSize(18);
   Writer.Words[0] := Byte(CurrentSheet.Options.ActualShowFormulas) or
     Byte(CurrentSheet.Options.ActualGridLines) shl 1 or
     Byte(CurrentSheet.Options.ActualHeaders) shl 2 or
     Byte((CurrentSheet.FrozenColumn >= 0) or
     (CurrentSheet.FrozenRow >= 0)) shl 3 or
     Byte(CurrentSheet.Options.ActualZeroValues) shl 4 or
     Byte(SpreadSheet.OptionsView.GridLineColor = clDefault) shl 5 or
     Byte(CurrentSheet.Columns.Groups.Count + CurrentSheet.Rows.Groups.Count > 0) shl 7 or
     Byte(CurrentSheet.Active) shl 9;
   Writer.Words[2] := CurrentSheet.TopRow;
   Writer.Words[4] := CurrentSheet.LeftColumn;
   Writer.Integers[6] := GetColorIndex(SpreadSheet.OptionsView.GridLineColor, 0, 0, 0);
end;

procedure TdxSpreadSheetXLSWriter.WriteZoomFactor;
begin
  Writer.WriteWord(CurrentSheet.Options.ZoomFactor);
  Writer.WriteWord(100);
end;

procedure TdxSpreadSheetXLSWriter.AddSSTBucket;
var
  AInfo: TdxXLSWriterSSTBacketInfo;
begin
  AInfo.StreamPosition := 0;
  AInfo.OffsetInSST := 0;
  AInfo.Reserved := 0;
  FExtSSTBackets.Add(AInfo);
end;

function TdxSpreadSheetXLSWriter.GetProgressValue: Integer;
begin
  if CurrentObject * TotalObjectsCount = 0 then
    Result := 0
  else
    Result := MulDiv(CurrentObject, 100, TotalObjectsCount);
end;

procedure TdxSpreadSheetXLSWriter.SetCurrentObject(AValue: Integer);
var
  AProgress: Integer;
begin
  AProgress := ProgressValue;
  FCurrentObject := AValue;
  if AProgress <> ProgressValue then
    DoProgress(ProgressValue);
end;

procedure TdxSpreadSheetXLSWriter.SetProgressValue(AValue: Integer);
begin
  CurrentObject := MulDiv(TotalObjectsCount, AValue, 100);
end;

procedure TdxSpreadSheetXLSWriter.WriteBOFInfo(AType: Word);
begin
  Writer.SetSize(16);
  Writer.Words[0] := $600;
  Writer.Words[2] := AType;
  Writer.Words[4] := $3267;
  Writer.Words[6] := $80C9;
  Writer.Words[8] := 1;
  Writer.Integers[12] := $606;
end;

procedure TdxSpreadSheetXLSWriter.WriteDefaultBoolean(AValue: TdxDefaultBoolean);
begin
  if AValue = bDefault then
    RecordWriter.Cancel
  else
    Writer.WriteWord(Byte(AValue = bTrue));
end;

function TdxSpreadSheetXLSWriter.WriteFormulaResult(
  AFormula: TdxSpreadSheetFormula; var AValue: Variant): Boolean;
var
  ACode: TdxSpreadSheetFormulaErrorCode;
  AData: array[0..7] of Byte;
begin
  AValue := Null;
  FillChar(AData, 0, SizeOf(AData));
  AData[7] := $FF;
  AData[6] := $FF;
  Result := (AFormula <> nil) and (AFormula.ResultValue <> nil);
  if not Result then
    Exit;
  ACode := AFormula.ResultValue.ErrorCode;
  AValue := AFormula.ResultValue.Value;
  if ACode <> ecNone then
  begin
    AData[0] := 1;
    AData[2] := ErrorCodeToErrorIndex[ACode];
  end
  else
    if VarType(AValue) = varBoolean then
    begin
      AData[0] := 2;
      AData[2] := Byte(Boolean(AValue));
    end
    else
      if VarIsNumeric(AValue) then
        Writer.WriteDateTime(AValue)
      else
        if not VarIsStr(AValue) then
          Result := False;
  if Result and ((ACode <> ecNone) or (VarType(AValue) = varBoolean) or VarIsStr(AValue)) then
    Writer.WriteBlock(AData);
end;

{ TdxXLSRecordWriter }

constructor TdxXLSRecordWriter.Create;
begin
  FBands := TObjectList<TdxXLSRecordBand>.Create;
end;

destructor TdxXLSRecordWriter.Destroy;
begin
  FreeAndNil(FBands);
  inherited Destroy;
end;

procedure TdxXLSRecordWriter.BeginWrite(ARecordID: Word);
begin
  FRecordID := ARecordID;
  FRecordSize := 0;
  Clear;
  Capacity := 8192;
  FCancel := False;
  FBands.Clear;
end;

procedure TdxXLSRecordWriter.Cancel;
begin
  FCancel := True;
end;

function TdxXLSRecordWriter.CheckBounds(ASizeNeeded: Word): Boolean;
begin
  Result := CurrentBandSize + ASizeNeeded < dxXLSMaxRecordSize;
  if not Result then
    AddBand;
end;

procedure TdxXLSRecordWriter.EndWrite(ADestStream: TStream);
var
  I: Integer;
begin
  if FCancel then
    Exit;

  FRecordSize := Size;
  if Bands.Count > 0 then
  begin
    Bands.Last.Size := CurrentBandSize;
    FRecordSize := Bands[0].Size;
  end;

  for I := 1 to Bands.Count - 1 do
    PWord(@PByteArray(Memory)^[Bands[I].Offset + 2])^ := Bands[I].Size;

  ADestStream.WriteBuffer(FRecordID, SizeOf(Word));
  ADestStream.WriteBuffer(FRecordSize, SizeOf(Word));
  if Size > 0 then
    ADestStream.WriteBuffer(Memory^, Size);
end;

procedure TdxXLSRecordWriter.AddBand;
var
  ABand: TdxXLSRecordBand;
begin
  ABand := TdxXLSRecordBand.Create;
  if Bands.Count > 0 then
    Bands.Last.Size := CurrentBandSize;
  Bands.Add(ABand);
  if Bands.Count > 1 then
  begin
    ABand.Offset := Size;
    WriteWordProc(Self, brcContinue);
    WriteWordProc(Self, 0);
  end
  else
    AddBand;
end;

procedure TdxXLSRecordWriter.WriteRecordBlock(const ABuffer; ACount: Integer; ADataIsString: Boolean);
var
  AOffset, ABlockSize: Integer;
begin
  AOffset := 0;
  while ACount > 0 do
  begin
    ABlockSize := Min(dxXLSMaxRecordSize - Byte(ADataIsString and (AOffset <> 0)) - CurrentBandSize, ACount);
    if ADataIsString then
    begin
      ABlockSize := ABlockSize div 2 * 2;
      if ABlockSize = 0 then
      begin
        AddBand;
        Continue;
      end;
      if AOffset > 0 then
        WriteBooleanProc(Self, True);
    end;
    WriteBuffer(PByteArray(@ABuffer)^[AOffset], ABlockSize);
    Inc(AOffset, ABlockSize);
    Dec(ACount, ABlockSize);
    if ACount > 0 then
      AddBand;
  end;
end;

function TdxXLSRecordWriter.GetCurrentBandSize: Integer;
begin
  Result := Size;
  if Bands.Count > 1 then
  begin
    Dec(Result, Bands.Last.Offset);
    Dec(Result, SizeOf(Word) * 2);
  end;
end;

{ TdxXLSWriter }

constructor TdxXLSWriter.Create(AOwner: TdxSpreadSheetXLSWriter);
begin
  inherited Create(AOwner.RecordWriter);
  FOwner := AOwner;
end;

procedure TdxXLSWriter.SetSize(ASize: Integer);
var
  APosition: Integer;
begin
  APosition := Stream.Position;
  Stream.Size := ASize;
  FillChar(PByteArray(Stream.Memory)^[APosition], ASize - APosition, 0);
end;

procedure TdxXLSWriter.XLS_WriteRef(const AArea: TRect);
var
  AData: array[0..7] of Byte;
begin
  PWord(@AData[0])^ := CheckRow(AArea.Top);
  PWord(@AData[2])^ := CheckRow(AArea.Bottom);
  PWord(@AData[4])^ := CheckColumn(AArea.Left);
  PWord(@AData[6])^ := CheckColumn(AArea.Right);
  WriteBlock(AData);
end;

procedure TdxXLSWriter.XLS_WriteSharedString(AValue: TdxSpreadSheetSharedString);
var
  ALength: Word;
  AData: TBytes;
begin
  Stream.CheckBounds(5);
  ALength := Length(AValue.Value);
  WriteWord(ALength);
  if (ALength > 0) and (AValue is TdxSpreadSheetFormattedSharedString) then
  begin
    WriteByte(1 or $08);
    WriteWord(TdxSpreadSheetFormattedSharedString(AValue).Runs.Count);
  end
  else
    WriteByte(1);
  if ALength > 0 then
    Stream.WriteRecordBlock(AValue.Value[1], ALength * 2, True);

  if AValue is TdxSpreadSheetFormattedSharedString then
  begin
    AData := GetFormattedStringData(TdxSpreadSheetFormattedSharedString(AValue));
    Stream.CheckBounds(Length(AData));
    Stream.WriteRecordBlock(AData[0], Length(AData), False);
  end;
end;

procedure TdxXLSWriter.XLS_WriteSimpleRef(const AArea: TRect);
var
  AData: array[0..5] of Byte;
begin
  PWord(@AData[0])^ := AArea.Top;
  PWord(@AData[2])^ := AArea.Bottom;
  AData[4] := AArea.Left;
  AData[5] := AArea.Right;
  WriteBlock(AData)
end;

procedure TdxXLSWriter.XLS_WriteSimpleString(const AValue: string; ASize: Integer; AOffset: Integer = -1);
var
  L: Byte;
begin
  if AOffset >= 0 then
    Position := AOffset;
  L := Min(MAXWORD, Length(AValue));
  if ASize = 2 then
    WriteWord(L)
  else
  begin
    L := Min(MAXBYTE, L);
    WriteByte(L)
  end;
  WriteBoolean(True);
  Stream.WriteBuffer(AValue[1], L * 2);
end;

procedure TdxXLSWriter.WriteBlock(const ABuffer: array of Byte);
begin
  WriteBlock(ABuffer, Length(ABuffer));
end;

procedure TdxXLSWriter.WriteBlock(const ABuffer; ACount: Integer);
begin
  Stream.WriteRecordBlock(ABuffer, ACount, False);
end;

procedure TdxXLSWriter.WriteStream(ASource: TMemoryStream);
begin
  WriteBlock(ASource.Memory^, ASource.Size);
end;


function TdxXLSWriter.GetFormattedStringData(AValue: TdxSpreadSheetFormattedSharedString): TBytes;
var
  I: Integer;
begin
  SetLength(Result, AValue.Runs.Count * 4);
  for I := 0 to AValue.Runs.Count - 1 do
  begin
    PWord(@Result[I * 4])^ := AValue.Runs[I].StartIndex - 1;
    PWord(@Result[I * 4 + 2])^ := Owner.Fonts[AValue.Runs[I].FontHandle];
    if PWord(@Result[I * 4 + 2])^ >= 4 then
      Inc(PWord(@Result[I * 4 + 2])^);
  end;
end;

function TdxXLSWriter.GetByte(AOffset: Integer): Byte;
begin
  Result := PByte(@GetBytes[AOffset])^
end;

function TdxXLSWriter.GetBytes: PByteArray;
begin
  Result := TMemoryStream(Stream).Memory;
end;

function TdxXLSWriter.GetInteger(AOffset: Integer): Integer;
begin
  Result := PInteger(@GetBytes[AOffset])^
end;

function TdxXLSWriter.GetPosition: Integer;
begin
  Result := Stream.Position;
end;

function TdxXLSWriter.GetStream: TdxXLSRecordWriter;
begin
  Result := TdxXLSRecordWriter(inherited Stream);
end;

function TdxXLSWriter.GetWord(AOffset: Integer): Word;
begin
  Result := PWord(@GetBytes[AOffset])^
end;

procedure TdxXLSWriter.SetByte(AOffset: Integer; AValue: Byte);
begin
  PByte(@GetBytes[AOffset])^ := AValue;
end;

procedure TdxXLSWriter.SetInteger(AOffset: Integer; AValue: Integer);
begin
  PInteger(@GetBytes[AOffset])^ := AValue;
end;

procedure TdxXLSWriter.SetPosition(AValue: Integer);
begin
  if AValue >= Stream.Size then
    SetSize(AValue + 1);
  Stream.Position := AValue;
end;

procedure TdxXLSWriter.SetWord(AOffset: Integer; AValue: Word);
begin
  PWord(@GetBytes[AOffset])^ := AValue;
end;

end.
