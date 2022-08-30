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

unit dxSpreadSheetFormatXLSReader;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  RTLConsts, Windows, Generics.Defaults, Generics.Collections, Classes, Types, SysUtils, Math, Graphics, Variants,
  dxCore, dxOLEDocument, cxClasses, cxVariants, cxGraphics, dxSpreadSheetCore, dxCoreClasses, dxSpreadSheetTypes,
  dxSpreadSheetClasses, dxSpreadSheetFormatXLSTypes, dxSpreadSheetUtils, dxSpreadSheetGraphics, dxSpreadSheetFormulas,
  dxSpreadSheetFunctions, dxGDIPlusClasses, dxSpreadSheetFormatUtils, dxSpreadSheetPrinting, dxHashUtils, dxCoreGraphics,
  dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetHyperlinks, dxSpreadSheetFormatXLSProtection,
  dxSpreadSheetProtection, dxSpreadSheetCoreStyles, dxSpreadSheetStyles;

type
  TdxSpreadSheetXLSReader = class;

  { TdxXLSRecordReader }

  TdxXLSRecordReader = class(TMemoryStream)
  strict private
    FCurrentPart: Integer;
    FOwner: TdxSpreadSheetXLSReader;
    FParts: TList<Integer>;
    FRecordID: Word;
    FSize: Integer;
    FStartPosition: Integer;

    function GetEncryptor: TdxXLSAbstractEncryptor; inline;
    function GetSource: TStream; inline;
  protected
    procedure AddPart(ASize: Word);

    property CurrentPart: Integer read FCurrentPart write FCurrentPart;
    property Encryptor: TdxXLSAbstractEncryptor read GetEncryptor;
    property Owner: TdxSpreadSheetXLSReader read FOwner;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSReader);
    destructor Destroy; override;
    procedure Initialize;
    procedure PrepareRecord;
    procedure SkipRecord;

    property Parts: TList<Integer> read FParts;
    property RecordID: Word read FRecordID;
    property Size: Integer read FSize;
    property Source: TStream read GetSource;
    property StartPosition: Integer read FStartPosition;
  end;

  { TdxXLSReader }

  TdxXLSReader = class(TcxReader)
  strict private
    FXLSRecord: TdxXLSRecordReader;

    function GetByteByIndex(AIndex: Integer): Byte; inline;
    function GetIntegerByIndex(AIndex: Integer): Integer; inline;
    function GetWordByIndex(AIndex: Integer): Word; inline;
  protected
    procedure CheckRange(const ASize: Integer; var AFirstSize, ASecondSize, APosition: Integer); inline;
  public
    constructor Create(ARecord: TdxXLSRecordReader);
    function XLS_ReadREF: TRect; inline;
    function XLS_ReadRKValue(AOffset: Integer): Double;
    function XLS_ReadSimpleREF: TRect;
    function XLS_ReadSharedString: TdxSpreadSheetSharedString; overload; inline;
    function XLS_ReadSharedString(AOptions, ALength, ARunCount: Integer): TdxSpreadSheetSharedString; overload;
    function XLS_ReadSimpleString(AOffset: Integer): string; inline;
    function XLS_ReadString(const AOptions, ALength: Integer): string; overload; inline;

    property Bytes[Index: Integer]: Byte read GetByteByIndex;
    property Integers[Index: Integer]: Integer read GetIntegerByIndex;
    property Words[Index: Integer]: Word read GetWordByIndex;
    property XLSRecord: TdxXLSRecordReader read FXLSRecord;
  end;

  { TdxSpreadSheetXLSConditionalFormattingRule }

  TdxSpreadSheetXLSConditionalFormattingRule = class
  public type
  {$REGION 'Types'}
    TAreaInfo = record
      Area: TRect;
      Rule: TdxSpreadSheetConditionalFormattingRuleExpression;
    end;
  {$ENDREGION}
  strict private
    FAreas: TList<TAreaInfo>;
    FID: Integer;
  public
    constructor Create(ID: Integer);
    destructor Destroy; override;
    procedure AddArea(const R: TRect);
    procedure ApplyRuleToAllAreas(ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
    function IsAssigned: Boolean;
    //
    property Areas: TList<TAreaInfo> read FAreas;
    property ID: Integer read FID;
  end;

  { TdxSpreadSheetXLSConditionalFormattingRules}

  TdxSpreadSheetXLSConditionalFormattingRules = class(TObjectList<TdxSpreadSheetXLSConditionalFormattingRule>)
  public
    function Add(ID: Integer): TdxSpreadSheetXLSConditionalFormattingRule;
    function Get(ID: Integer; out ARule: TdxSpreadSheetXLSConditionalFormattingRule): Boolean;
  end;

  { TdxSpreadSheetXLSReader }

  TdxSpreadSheetXLSReader = class(TdxSpreadSheetCustomReader)
  strict private
    FActiveTabIndex: Integer;
    FCodePage: Word;
    FColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper;
    FCommonDataReady: Boolean;
    FConditionalFormattingRules: TdxSpreadSheetXLSConditionalFormattingRules;
    FContainers: TDictionary<Word, TdxSpreadSheetContainer>;
    FCurrentSheet: TdxSpreadSheetTableView;
    FData: TStream;
    FDocument: TdxOLEDocument;
    FEncryptor: TdxXLSAbstractEncryptor;
    FExternalNames: TStringList;
    FFirstTabIndex: Integer;
    FFonts: TcxObjectList;
    FHasFrozenPanes: Boolean;
    FHyperlinks: TStringList;
    FImages: TObjectList<TdxSmartImage>;
    FNames: TStringList;
    FNamesDefinition: TObjectList<TdxXLSNameDefinition>;
    FNumberFormats: TDictionary<Word, TdxSpreadSheetFormatHandle>;
    FPassword: string;
    FProgressValue: Integer;
    FReader: TdxXLSReader;
    FReaders: TList<TdxXLSMethod>;
    FRecordReader: TdxXLSRecordReader;
    FSheetIndexes: array of Integer;
    FSheetOffset: TList<Integer>;
    FStrings: TcxObjectList;
    FStyles: TcxObjectList;
    FTextures: TList<TdxSmartImage>;

    procedure ReadBlankCell;
    procedure ReadBlankCells;
    procedure ReadBOF;
    procedure ReadBoolErrorCell;
    procedure ReadBoundSheet;
    procedure ReadCalcCount;
    procedure ReadCalcMode;
    procedure ReadCodePage;
    procedure ReadColumnFormatInformation;
    procedure ReadComment;
    procedure ReadSpecialName;

    function CreateConditionalFormattingRule(AOptions: Word): TdxSpreadSheetConditionalFormattingRuleExpression;
    procedure ReadConditionalFormatting;
    procedure ReadConditionalFormattingExtension;
    procedure ReadConditionalFormattingRule;
    function ReadConditionalFormattingRuleExpression(ARefSize, AAnchorColumn, AAnchorRow: Integer): string;
    procedure ReadConditionalFormattingRuleStyle(AStyle: TdxSpreadSheetCellStyle); overload;
    procedure ReadConditionalFormattingRuleStyle(AStyle: TdxXLSCellStyle; out AFontHandle: TdxSpreadSheetFontHandle); overload;
    function ReadConditionalFormattingRuleStyleFont: TdxSpreadSheetFontHandle;

    procedure ReadDataFormat;
    procedure ReadDateSystem;
    procedure ReadDefaultColumnWidth;
    procedure ReadDefColumnWidth;
    procedure ReadDefaultRowHeight;
    procedure ReadEOF;
    procedure ReadExternName;
    procedure ReadExternSheet;
    procedure ReadFilePassword;
    procedure ReadFont;
    procedure ReadFormulaArray;
    procedure ReadFormulas(ATop, ALeft, ABottom, ARight, AOffset: Integer);
    procedure ReadFormulaCell;
    procedure ReadHyperlink;
    procedure ReadHyperlinkTooltip;
    procedure ReadIteration;
    procedure ReadLabelCell;
    procedure ReadLabelSSTCell;
    procedure ReadMergedCells;
    procedure ReadMulRKCells;
    procedure ReadMSODrawing;
    procedure ReadMSODrawingGroup;
    procedure ReadMSODrawingObject;
    procedure ReadMSOTextObject;
    procedure ReadName;
    procedure ReadNumberCell;
    procedure ReadPanes;
    procedure ReadPalette;
    procedure ReadPageBreaksHorizontal;
    procedure ReadPageBreaksVertical;
    procedure ReadPassword;
    procedure ReadPrintCenterH;
    procedure ReadPrintCenterV;
    procedure ReadPrintGridLines;
    procedure ReadPrintHeader;
    procedure ReadPrintHeaders;
    procedure ReadPrintFooter;
    procedure ReadPrintLeftMargin;
    procedure ReadPrintTopMargin;
    procedure ReadPrintRightMargin;
    procedure ReadPrintBottomMargin;
    procedure ReadPrintSetup;
    procedure ReadPrintWorkspaceInfo;
    procedure ReadProtection;
    procedure ReadRowFormatInformation;
    procedure ReadRKCell;
    procedure ReadSelection;
    procedure ReadSharedFeatures;
    procedure ReadSharedFormula;
    procedure ReadSharedStringTable;
    procedure ReadTheme;
    procedure ReadWindow1Information;
    procedure ReadWindow2Information;
    procedure ReadXFRecord;
    procedure ReadXFExtension;
    procedure ReadXFExtensionNoFMT(AStyle: TdxXLSCellStyle);
    procedure ReadZoomFactor;

    procedure SetProgressValue(AValue: Integer);
  protected
    Palette: TColors;
    ThemeHelper: TdxSpreadSheetXLSReaderThemeHelper;

    function CreateCell: TdxSpreadSheetCell; overload;
    function CreateCell(ARow, AColumn, AStyleIndex: Integer): TdxSpreadSheetCell; overload;
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    function GetColorDef(AIndex, ADefaultIndex: Integer; ADefaultColor: TColor): TColor;
    function GetFillStyle(AXLSFillStyle: Byte; var AFillStyle: TdxSpreadSheetCellFillStyle): Boolean;
    function GetFont(AIndex: Integer): TdxSpreadSheetFontHandle; inline;
    function GetFontEx(AFontHandle: TdxSpreadSheetFontHandle; AColor: TColor): TdxSpreadSheetFontHandle;
    function GetFormat(AIndex: Integer): TdxSpreadSheetFormatHandle;
    function GetStyle(AIndex: Integer): TdxXLSCellStyle; inline;
    function IsEOF: Boolean; inline;
    function TextBoxContainerNeeded: TdxSpreadSheetTextBoxContainer;
    //
    procedure PrepareCommonData;
    procedure PrepareEncryptorKey;
    procedure PrepareFonts;
    procedure PrepareFormattedSharedString(AString: TdxSpreadSheetFormattedSharedString);
    procedure PrepareSharedStrings;
    procedure PrepareStyle(AXLSStyle: TdxXLSCellStyle; AFontHandle: TdxSpreadSheetFontHandle = nil);
    procedure PrepareStyles;
    function TryDefineName(const AName: string; var ADefinedName: TdxSpreadSheetDefinedName): Boolean;
    //
    procedure RegisterReader(ARecordID: Word; AReader: TdxXLSMethod);
    procedure RegisterReaders; virtual;
    procedure ResolveHyperlinks;
    procedure UpdateProgress;
    procedure ValidateContainers;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    function GetColor(AIndex: Integer): TColor; inline;
    function GetExternalName(AIndex: Integer; var AInfo: TdxSpreadSheetFunctionInfo): string; inline;
    function GetName(AIndex: Integer; var AInfo: TdxSpreadSheetFunctionInfo): string; inline;
    function GetSheet(AIndex: Integer): TdxSpreadSheetTableView;
    procedure ReadData; override;

    property ActiveTabIndex: Integer read FActiveTabIndex write FActiveTabIndex;
    property CodePage: Word read FCodePage;
    property ColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper read FColumnWidthHelper;
    property CommonDataReady: Boolean read FCommonDataReady write FCommonDataReady;
    property ConditionalFormattingRules: TdxSpreadSheetXLSConditionalFormattingRules read FConditionalFormattingRules;
    property Containers: TDictionary<Word, TdxSpreadSheetContainer> read FContainers;
    property CurrentSheet: TdxSpreadSheetTableView read FCurrentSheet;
    property Data: TStream read FData;
    property Document: TdxOLEDocument read FDocument;
    property Encryptor: TdxXLSAbstractEncryptor read FEncryptor;
    property ExternalNames: TStringList read FExternalNames;
    property FirstTabIndex: Integer read FFirstTabIndex write FFirstTabIndex;
    property Fonts: TcxObjectList read FFonts;
    property HasFrozenPanes: Boolean read FHasFrozenPanes write FHasFrozenPanes;
    property Hyperlinks: TStringList read FHyperlinks;
    property Images: TObjectList<TdxSmartImage> read FImages;
    property Names: TStringList read FNames;
    property NamesDefinition: TObjectList<TdxXLSNameDefinition> read FNamesDefinition;
    property NumberFormats: TDictionary<Word, TdxSpreadSheetFormatHandle> read FNumberFormats;
    property ProgressValue: Integer read FProgressValue write SetProgressValue;
    property Reader: TdxXLSReader read FReader;
    property Readers: TList<TdxXLSMethod> read FReaders;
    property RecordReader: TdxXLSRecordReader read FRecordReader;
    property SheetOffset: TList<Integer> read FSheetOffset;
    property Strings: TcxObjectList read FStrings;
    property Styles: TcxObjectList read FStyles;
    property Textures: TList<TdxSmartImage> read FTextures;
  end;

implementation

uses
  cxGeometry, dxSpreadSheetFormatXLSFormulas, dxSpreadSheetFormatXLSDrawing, dxSpreadSheetCoreHelpers,
  dxSpreadSheetFormatXLS, dxOLECryptoContainer, dxOLECryptoContainerStrs, dxProtectionUtils, dxSpreadSheetStrs,
  dxSpreadSheetCoreFormulas, dxSpreadSheetCoreStrs;

type
  TdxHashTableAccess = class(TdxHashTable);
  TdxDynamicListItemAccess = class(TdxDynamicListItem);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetCellStylesAccess = class(TdxSpreadSheetCellStyles);
  TdxSpreadSheetConditionalFormattingExpressionAccess = class(TdxSpreadSheetConditionalFormattingExpression);
  TdxSpreadSheetConditionalFormattingRuleExpressionAccess = class(TdxSpreadSheetConditionalFormattingRuleExpression);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetCustomTextBoxContainerAccess = class(TdxSpreadSheetCustomTextBoxContainer);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);
  TdxSpreadSheetItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

function AreAnchorsAssigned(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  case AContainer.AnchorType of
    catOneCell:
      Result := (AContainer.AnchorPoint1.Cell <> nil);
    catTwoCell:
      Result := (AContainer.AnchorPoint1.Cell <> nil) and (AContainer.AnchorPoint2.Cell <> nil);
  else // catAbsolute:
    Result := not (cxPointIsNull(AContainer.AnchorPoint1.Offset) and cxPointIsNull(AContainer.AnchorPoint2.Offset));
  end;
end;

{ TdxSpreadSheetXLSConditionalFormattingRule }

constructor TdxSpreadSheetXLSConditionalFormattingRule.Create(ID: Integer);
begin
  inherited Create;
  FID := ID;
  FAreas := TList<TAreaInfo>.Create;
end;

destructor TdxSpreadSheetXLSConditionalFormattingRule.Destroy;
begin
  FreeAndNil(FAreas);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSConditionalFormattingRule.AddArea(const R: TRect);
var
  AArea: TAreaInfo;
begin
  AArea.Area := R;
  AArea.Rule := nil;
  Areas.Add(AArea);
end;

procedure TdxSpreadSheetXLSConditionalFormattingRule.ApplyRuleToAllAreas(ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
var
  AAreaInfo: TAreaInfo;
  I: Integer;
begin
  AAreaInfo := FAreas[0];
  AAreaInfo.Rule := ARule;
  FAreas[0] := AAreaInfo;

  for I := 1 to Areas.Count - 1 do
  begin
    AAreaInfo := Areas[I];
    AAreaInfo.Rule := TdxSpreadSheetConditionalFormattingRuleExpression(ARule.Clone(AAreaInfo.Area));
    Areas[I] := AAreaInfo;
  end;
end;

function TdxSpreadSheetXLSConditionalFormattingRule.IsAssigned: Boolean;
begin
  Result := (Areas.Count > 0) and (Areas.First.Rule <> nil);
end;

{ TdxSpreadSheetXLSConditionalFormattingRules }

function TdxSpreadSheetXLSConditionalFormattingRules.Add(ID: Integer): TdxSpreadSheetXLSConditionalFormattingRule;
begin
  Result := TdxSpreadSheetXLSConditionalFormattingRule.Create(ID);
  inherited Add(Result);
end;

function TdxSpreadSheetXLSConditionalFormattingRules.Get(
  ID: Integer; out ARule: TdxSpreadSheetXLSConditionalFormattingRule): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].ID = ID;
    if Result then
    begin
      ARule := Items[I];
      Break;
    end;
  end;
end;

{ TdxSpreadSheetXLSReader }

constructor TdxSpreadSheetXLSReader.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FColumnWidthHelper := TdxSpreadSheetExcelColumnWidthHelper.Create;
  FCodePage := 1252;
  FFonts := TcxObjectList.Create;
  FStyles := TcxObjectList.Create;
  FSheetOffset := TList<Integer>.Create;
  FStrings := TcxObjectList.Create;
  FNames := TStringList.Create;
  FNamesDefinition := TObjectList<TdxXLSNameDefinition>.Create;
  FNumberFormats := TDictionary<Word, TdxSpreadSheetFormatHandle>.Create(100);
  FExternalNames := TStringList.Create;
  FProgressValue := -1;
  FDocument := TdxOLEDocument.Create(AStream, dmReading);
  FData := FDocument.StreamByName('Workbook');
  Check(FData <> nil, sdxInvalidStreamFormat);
  FData.Position := 0;
  FRecordReader := TdxXLSRecordReader.Create(Self);
  FReaders := TList<TdxXLSMethod>.Create;
  FReader := TdxXLSReader.Create(FRecordReader);
  FHyperlinks := TStringList.Create;
  FImages := TObjectList<TdxSmartImage>.Create;
  FContainers := TDictionary<Word, TdxSpreadSheetContainer>.Create;
  ThemeHelper := TdxSpreadSheetXLSReaderThemeHelper.Create;
  FConditionalFormattingRules := TdxSpreadSheetXLSConditionalFormattingRules.Create;
  SetLength(Palette, Length(dxExcelStandardColors));
  FTextures := TList<TdxSmartImage>.Create;
  Move(dxExcelStandardColors[0], Palette[0], Length(dxExcelStandardColors) * SizeOf(TColor));
  RegisterReaders;
  ProgressValue := 0;
end;

destructor TdxSpreadSheetXLSReader.Destroy;
begin
  if CommonDataReady then
    Strings.Count := 0;
  FreeAndNil(FEncryptor);
  FreeAndNil(FImages);
  FreeAndNil(FExternalNames);
  FreeAndNil(FNames);
  FreeAndNil(FNamesDefinition);
  FreeAndNil(FConditionalFormattingRules);
  FreeAndNil(FColumnWidthHelper);
  FreeAndNil(FStrings);
  FreeAndNil(FStyles);
  FreeAndNil(FFonts);
  FreeAndNil(FSheetOffset);
  FreeAndNil(FContainers);
  FreeAndNil(FDocument);
  FreeAndNil(FRecordReader);
  FreeAndNil(FReader);
  FreeAndNil(FReaders);
  FreeAndNil(FNumberFormats);
  FreeAndNil(ThemeHelper);
  FreeAndNil(FTextures);
  FreeAndNil(FHyperlinks);
  inherited Destroy;
end;

function TdxSpreadSheetXLSReader.GetColor(AIndex: Integer): TColor;
begin
  Dec(AIndex, 8);
  if (AIndex < 0) or (AIndex > High(Palette)) then
    Result := clDefault
  else
    Result := Palette[AIndex];
end;

function TdxSpreadSheetXLSReader.GetExternalName(AIndex: Integer; var AInfo: TdxSpreadSheetFunctionInfo): string;
begin
  Result := '';
  AInfo := nil;
  if (AIndex >= 0) and (AIndex < ExternalNames.Count) then
  begin
    Result := FExternalNames[AIndex];
    AInfo := FExternalNames.Objects[AIndex] as TdxSpreadSheetFunctionInfo;
  end;
end;

function TdxSpreadSheetXLSReader.GetName(AIndex: Integer; var AInfo: TdxSpreadSheetFunctionInfo): string;
begin
  Result := '';
  AInfo := nil;
  if InRange(AIndex, 0, Names.Count) then
  begin
    Result := FNames[AIndex];
    if FNames.Objects[AIndex] is TdxSpreadSheetFunctionInfo then
      AInfo := TdxSpreadSheetFunctionInfo(FNames.Objects[AIndex]);
  end;
end;

function TdxSpreadSheetXLSReader.GetSheet(AIndex: Integer): TdxSpreadSheetTableView;
var
  APageIndex: Integer;
begin
  TdxSpreadSheetInvalidObject.AssignTo(Result);
  if AIndex < Length(FSheetIndexes) then
    APageIndex := FSheetIndexes[AIndex]
  else
    APageIndex := AIndex;

  if APageIndex < SpreadSheet.SheetCount then
    Result := TdxSpreadSheetTableView(SpreadSheet.Sheets[APageIndex])
end;

procedure TdxSpreadSheetXLSReader.ReadData;
var
  AReader: TdxXLSMethod;
  ASkipRecord: Boolean;
begin
  ASkipRecord := False;
  while not IsEOF do
  begin
    AReader := nil;
    RecordReader.PrepareRecord;
    ASkipRecord := ASkipRecord and (RecordReader.RecordID <> brcBOF);
    if RecordReader.RecordID < Readers.Count then
      AReader := Readers[RecordReader.RecordID];

    if Assigned(AReader) and not ASkipRecord then
    begin
      RecordReader.Initialize;
      ASkipRecord := RecordReader.RecordID = brcEOF;
      AReader;
    end
    else
      RecordReader.SkipRecord;

    UpdateProgress;
  end;
  ResolveHyperlinks;
  SpreadSheet.ActiveSheetIndex := ActiveTabIndex;
  SpreadSheet.Password := FPassword;
end;

function TdxSpreadSheetXLSReader.CreateCell: TdxSpreadSheetCell;
begin
  Result := CreateCell(Reader.Words[0], Reader.Words[1], Reader.Words[2]);
end;

function TdxSpreadSheetXLSReader.CreateCell(ARow, AColumn, AStyleIndex: Integer): TdxSpreadSheetCell;
begin
  Include(TdxSpreadSheetAccess(SpreadSheet).FState, sssReadingCells);
  try
    Result := CurrentSheet.CreateCell(ARow, AColumn);
    Result.Style.Handle := GetStyle(AStyleIndex).Handle;
  finally
    Exclude(TdxSpreadSheetAccess(SpreadSheet).FState, sssReadingCells);
  end;
end;

function TdxSpreadSheetXLSReader.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 0);
end;

function TdxSpreadSheetXLSReader.GetColorDef(AIndex, ADefaultIndex: Integer; ADefaultColor: TColor): TColor;
begin
  if AIndex = ADefaultIndex then
    Result := ADefaultColor
  else
    Result := GetColor(AIndex);
end;

function TdxSpreadSheetXLSReader.GetFillStyle(AXLSFillStyle: Byte; var AFillStyle: TdxSpreadSheetCellFillStyle): Boolean;
var
  I: Byte;
begin
  Result := False;
  AFillStyle := sscfsSolid;
  for I := 0 to High(FillStyles) do
    if FillStyles[I] = AXLSFillStyle then
    begin
      AFillStyle := TdxSpreadSheetCellFillStyle(I);
      Result := True;
      Break;
    end;
end;

function TdxSpreadSheetXLSReader.GetFont(AIndex: Integer): TdxSpreadSheetFontHandle;
begin
  if AIndex >= Fonts.Count then
    AIndex := 0;
  Result := TdxXLSFont(Fonts.List[AIndex]).Handle;
end;

function TdxSpreadSheetXLSReader.GetFontEx(AFontHandle: TdxSpreadSheetFontHandle;
  AColor: TColor): TdxSpreadSheetFontHandle;
begin
  Result := AFontHandle;
  if Result.Color <> AColor then
  begin
    Result := AFontHandle.Clone;
    Result.Color := AColor;
    Result := CellStyles.Fonts.AddFont(Result);
  end;
end;

function TdxSpreadSheetXLSReader.GetFormat(AIndex: Integer): TdxSpreadSheetFormatHandle;
begin
  Result := nil;
  if not NumberFormats.TryGetValue(AIndex, Result) then
  begin
    Result := CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(AIndex);
    if Result = nil then
      Result := CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(0);
  end;
end;

function TdxSpreadSheetXLSReader.GetStyle(AIndex: Integer): TdxXLSCellStyle;
begin
  if AIndex > Styles.Count then
    AIndex := 0;
  Result := TdxXLSCellStyle(Styles.List[AIndex]);
end;

function TdxSpreadSheetXLSReader.IsEOF: Boolean;
begin
  Result := Data.Position > (Data.Size - 4);
end;

function TdxSpreadSheetXLSReader.TextBoxContainerNeeded: TdxSpreadSheetTextBoxContainer;
var
  ASource: TdxSpreadSheetContainer;
begin
  ASource := CurrentSheet.Containers.Last;
  if ASource is TdxSpreadSheetTextBoxContainer then
    Result := TdxSpreadSheetTextBoxContainer(ASource)
  else
  begin
    CurrentSheet.Containers.Add(TdxSpreadSheetTextBoxContainer, Result);
    Result.Assign(ASource);
    Result.TextBox.WordWrap := True;
    // todo: check in dictionary
    ASource.Free;
  end;
end;

procedure TdxSpreadSheetXLSReader.PrepareCommonData;
var
  I: Integer;
begin
  PrepareFonts;
  PrepareStyles;
  PrepareSharedStrings;
  for I := 0 to NamesDefinition.Count - 1 do
    NamesDefinition[I].Restore(Self);
  CommonDataReady := True;
end;

procedure TdxSpreadSheetXLSReader.PrepareEncryptorKey;
var
  APassword: string;
begin
  if Encryptor.CheckPassword(dxSpreadSheetDefaultPassword) then
    FPassword := dxSpreadSheetDefaultPassword
  else
  begin
    APassword := SpreadSheet.Password;
    if (APassword = '') or not Encryptor.CheckPassword(APassword) then
    begin
      if not TdxSpreadSheetAccess(SpreadSheet).DoGetPassword(APassword) then
        Abort;
    end;
    if Encryptor.CheckPassword(APassword) then
      FPassword := APassword
    else
      raise EdxOLECryptoContainerError.Create(cxGetResourceString(@sdxOleCryptoContainerInvalidPassword));
  end;
end;

procedure TdxSpreadSheetXLSReader.PrepareFonts;
var
  AFont: TdxXLSFont;
  AFontHandle: TdxSpreadSheetFontHandle;
  I: Integer;
begin
  for I := 0 to Fonts.Count - 1 do
  begin
    AFont := TdxXLSFont(Fonts[I]);
    AFont.Color := GetColor(AFont.Color);
    AFontHandle := CellStyles.Fonts.CreateFont;
    AFont.AssignTo(AFontHandle);
    dxChangeHandle(TdxHashTableItem(AFont.Handle), AddFont(AFontHandle));
  end;
  ColumnWidthHelper.Font := TdxXLSFont(Fonts[0]).Handle.GraphicObject;
end;

procedure TdxSpreadSheetXLSReader.PrepareFormattedSharedString(AString: TdxSpreadSheetFormattedSharedString);
var
  I: Integer;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
begin
  for I := 0 to AString.Runs.Count - 1 do
  begin
    ARun := AString.Runs[I];
    if ARun.Tag >= 4 then
      ARun.Tag := ARun.Tag - 1;
    ARun.FontHandle := GetFont(ARun.Tag);
  end;
end;

procedure TdxSpreadSheetXLSReader.PrepareSharedStrings;
var
  I: Integer;
  ASharedString: TdxSpreadSheetSharedString;
begin
  for I := 0 to Strings.Count - 1 do
  begin
    ASharedString := TdxSpreadSheetSharedString(Strings[I]);
    if ASharedString.ClassType = TdxSpreadSheetFormattedSharedString then
      PrepareFormattedSharedString(TdxSpreadSheetFormattedSharedString(ASharedString));
    TList(Strings).List[I] := StringTable.Add(ASharedString);
    TdxSpreadSheetSharedString(Strings[I]).AddRef;
  end;
end;

procedure TdxSpreadSheetXLSReader.PrepareStyle(AXLSStyle: TdxXLSCellStyle; AFontHandle: TdxSpreadSheetFontHandle = nil);
const
  BorderStyleMask: array[TcxBorder] of Integer = ($000F, $0F00, $00F0, $F000);
  BorderStyleOffset: array[TcxBorder] of Integer = (0, 8, 4, 12);
  XLSBorderStyleToSpreadSheetStyle: array[0..15] of TdxSpreadSheetCellBorderStyle = (
    sscbsDefault,	sscbsThin, sscbsMedium, sscbsDashed, sscbsDotted, sscbsThick,	sscbsDouble, sscbsHair,
    sscbsMediumDashed, sscbsDashDot, sscbsMediumDashDot, sscbsDashDotDot, sscbsMediumDashDotDot,
    sscbsSlantedDashDot, sscbsDefault, sscbsDefault
  );

  procedure PrepareBorders(ABorders: TdxSpreadSheetBordersHandle);
  var
    ABorder: TcxBorder;
  begin
    if bLeft in AXLSStyle.Borders then
      ABorders.BorderColor[bLeft] := GetColor(AXLSStyle.LeftRightBorders and $007F);
    if bRight in AXLSStyle.Borders then
      ABorders.BorderColor[bRight] := GetColor((AXLSStyle.LeftRightBorders and $3F80) shr 7);
    if bTop in AXLSStyle.Borders then
      ABorders.BorderColor[bTop] := GetColor(AXLSStyle.TopBottomBordersAndFill and $007F);
    if bBottom in AXLSStyle.Borders then
      ABorders.BorderColor[bBottom] := GetColor((AXLSStyle.TopBottomBordersAndFill and $3F80) shr 7);
    for ABorder := Low(ABorder) to High(ABorder) do
      if ABorder in AXLSStyle.Borders then
      begin
        ABorders.BorderStyle[ABorder] := XLSBorderStyleToSpreadSheetStyle[
          (AXLSStyle.BordersStyle and BorderStyleMask[ABorder]) shr BorderStyleOffset[ABorder]];
      end;
  end;

var
  AFillStyle: TdxSpreadSheetCellFillStyle;
  AStyle: TdxSpreadSheetCellStyleHandle;
  ABorders: TdxSpreadSheetBordersHandle;
  ABrush: TdxSpreadSheetBrushHandle;
begin
  if AXLSStyle.Font >= 4 then
    Dec(AXLSStyle.Font);
  if AFontHandle = nil then
    AFontHandle := GetFont(AXLSStyle.Font);

  if AXLSStyle.ExtendedData <> nil then
  begin
    AXLSStyle.ExtendedData.CheckColors(ThemeHelper);
    if xfextTextColor in AXLSStyle.ExtendedData.AssignedValues then
      AFontHandle := GetFontEx(AFontHandle, AXLSStyle.ExtendedData.DataValue[xfextTextColor]);
  end;

  AStyle := CellStyles.CreateStyle(AFontHandle, GetFormat(AXLSStyle.Format));
  ABorders := CellStyles.Borders.CreateBorders;
  ABrush := CellStyles.Brushes.CreateBrush;

  AStyle.States := [];
  if AXLSStyle.Flags and $01 = $01 then
    AStyle.States := AStyle.States + [csLocked];
  if AXLSStyle.Flags and $02 = $02 then
    AStyle.States := AStyle.States + [csHidden];
  if AXLSStyle.Indents and $0010 = $0010 then
    AStyle.States := AStyle.States + [csShrinkToFit];
  if AXLSStyle.Alignments and $0008 = $0008 then
    AStyle.States := AStyle.States + [csWordWrap];

  AStyle.AlignVert := TdxSpreadSheetDataAlignVert(AXLSStyle.Alignments shr 4 and $7);
  AStyle.AlignHorzIndent := ColumnWidthHelper.SpacesNumberToPixels(AXLSStyle.Indents and $F);
  if AXLSStyle.Alignments and $7 >= 6 then
    AStyle.AlignHorz := ssahCenter
  else
    AStyle.AlignHorz := TdxSpreadSheetDataAlignHorz(AXLSStyle.Alignments and $7);

  PrepareBorders(ABorders);

  if GetFillStyle(AXLSStyle.TopBottomBordersAndFill shr 26 and $FF, AFillStyle) then
  begin
    ABrush.Style := AFillStyle;
    if AXLSStyle.FillColors shr 7 and $7F > $40 then
      ABrush.BackgroundColor := clBlack
    else
      ABrush.BackgroundColor := GetColorDef(AXLSStyle.FillColors shr 7 and $7F, $40, clDefault);

    if ABrush.ForegroundColor = $40 then
      ABrush.ForegroundColor := clWindow
    else
      ABrush.ForegroundColor := GetColorDef(AXLSStyle.FillColors and $7F, $41, clDefault);
  end;

  if AXLSStyle.ExtendedData <> nil then
    AXLSStyle.ExtendedData.CorrectStyle(ABrush, ABorders, AStyle);
  if (ABrush.Style = sscfsSolid) and (ABrush.ForegroundColor <> clDefault) then
    ExchangeLongWords(Longword(ABrush.BackgroundColor), Longword(ABrush.ForegroundColor));

  AStyle.Brush := ABrush;
  AStyle.Borders := ABorders;
  AXLSStyle.Handle := CellStyles.AddStyle(AStyle);
  AXLSStyle.Handle.AddRef;
end;

procedure TdxSpreadSheetXLSReader.PrepareStyles;
var
  AStyle: TdxXLSCellStyle;
  I: Integer;
begin
  for I := 0 to Styles.Count - 1 do
  begin
    AStyle := GetStyle(I);
    PrepareStyle(AStyle);
    if I = 0 then
      ColumnWidthHelper.Font := AStyle.Handle.Font.GraphicObject
    else
      if I = 15 then
        CellStyles.DefaultStyle.Assign(AStyle.Handle);
  end;
end;

function TdxSpreadSheetXLSReader.TryDefineName(const AName: string; var ADefinedName: TdxSpreadSheetDefinedName): Boolean;
begin
  Result := True;
  ADefinedName := nil;
  try
    ADefinedName := SpreadSheet.DefinedNames.Add(AName, '', nil);
  except
    on EdxSpreadSheetError do
      Result := False;
    else
      raise;
  end;
end;

procedure TdxSpreadSheetXLSReader.RegisterReader(ARecordID: Word; AReader: TdxXLSMethod);
begin
  FReaders.Count := Max(FReaders.Count, ARecordID + 1);
  FReaders[ARecordID] := AReader;
end;

procedure TdxSpreadSheetXLSReader.RegisterReaders;
begin
  RegisterReader(brcBOF, ReadBOF);
  RegisterReader(brcFORMAT, ReadDataFormat);
  RegisterReader(brc1904, ReadDateSystem);

  RegisterReader(brcBoundSheet, ReadBoundSheet);
  RegisterReader(brcFont, ReadFont);
  RegisterReader(brcCodePage, ReadCodePage);
  RegisterReader(brcColInfo, ReadColumnFormatInformation);
  RegisterReader(brcROW, ReadRowFormatInformation);
  RegisterReader(brcSTANDARDWIDTH, ReadDefaultColumnWidth);
  RegisterReader(brcDEFCOLWIDTH, ReadDefColumnWidth);
  RegisterReader(brcDefaultRowHeight, ReadDefaultRowHeight);
  RegisterReader(brcSST, ReadSharedStringTable);
  RegisterReader(brcBlank, ReadBlankCell);
  RegisterReader(brcBoolErr, ReadBoolErrorCell);
  RegisterReader(brcMergeCells, ReadMergedCells);
  RegisterReader(brcLabelSST, ReadLabelSSTCell);
  RegisterReader(brcLabel, ReadLabelCell);
  RegisterReader(brcNumber, ReadNumberCell);
  RegisterReader(brcMulBlank, ReadBlankCells);
  RegisterReader(brcMulRk, ReadMulRKCells);
  RegisterReader(brcRK, ReadRKCell);
  RegisterReader(brcRString, ReadLabelCell);
  RegisterReader(brcArray, ReadFormulaArray);
  RegisterReader(brcSHRFMLA, ReadSharedFormula);
  RegisterReader(brcFORMULA, ReadFormulaCell);
  RegisterReader(brcName, ReadName);
  RegisterReader(brcEXTERNNAME, ReadExternName);
  RegisterReader(brcExternSheet, ReadExternSheet);
  RegisterReader(brcXF, ReadXFRecord);
  RegisterReader(brcXFEXT, ReadXFExtension);
  RegisterReader(brcTheme, ReadTheme);

  RegisterReader(brcPALETTE, ReadPalette);
  RegisterReader(brcEOF, ReadEOF);
  RegisterReader(brcScl, ReadZoomFactor);
  RegisterReader(brcSelection, ReadSelection);

  RegisterReader(brcPANE, ReadPanes);
  RegisterReader(brcWindow1, ReadWindow1Information);
  RegisterReader(brcWindow2, ReadWindow2Information);

  RegisterReader(brcCONDFMT, ReadConditionalFormatting);
  RegisterReader(brcCF, ReadConditionalFormattingRule);
  RegisterReader(brcCFEX, ReadConditionalFormattingExtension);

  RegisterReader(brcCALCCOUNT, ReadCalcCount);
  RegisterReader(brcCALCMODE, ReadCalcMode);
  RegisterReader(brcITERATION, ReadIteration);
  //
  RegisterReader(brcMSODRAWING, ReadMSODrawing);
  RegisterReader(brcMSODRAWINGGROUP, ReadMSODrawingGroup);
  RegisterReader(brcOBJ, ReadMSODrawingObject);
  RegisterReader(brcTXO, ReadMSOTextObject);
  //
  RegisterReader(brcSetup, ReadPrintSetup);
  RegisterReader(brcHeader, ReadPrintHeader);
  RegisterReader(brcPrintHeaders, ReadPrintHeaders);
  RegisterReader(brcPrintGridLines, ReadPrintGridLines);
  RegisterReader(brcFooter, ReadPrintFooter);
  RegisterReader(brcLeftMargin, ReadPrintLeftMargin);
  RegisterReader(brcTopMargin, ReadPrintTopMargin);
  RegisterReader(brcRightMargin, ReadPrintRightMargin);
  RegisterReader(brcBottomMargin, ReadPrintBottomMargin);
  RegisterReader(brcHCenter, ReadPrintCenterH);
  RegisterReader(brcVCenter, ReadPrintCenterV);
  RegisterReader(brcWSBool, ReadPrintWorkspaceInfo);
  RegisterReader(brcHORIZONTALPAGEBREAKS, ReadPageBreaksHorizontal);
  RegisterReader(brcVERTICALPAGEBREAKS, ReadPageBreaksVertical);
  //
  RegisterReader(brcNOTE, ReadComment);
  RegisterReader(brcHLINK, ReadHyperlink);
  RegisterReader(brcHLINKTOOLTIP, ReadHyperlinkTooltip);
  //
  RegisterReader(brcFEATHEADR, ReadSharedFeatures);
  RegisterReader(brcFILEPASS, ReadFilePassword);
  RegisterReader(brcPASSWORD, ReadPassword);
  RegisterReader(brcProtect, ReadProtection);
end;

procedure TdxSpreadSheetXLSReader.ResolveHyperlinks;
var
  I: Integer;
begin
  for I := 0 to Hyperlinks.Count - 1 do
    TdxSpreadSheetHyperlink(Hyperlinks.Objects[I]).Value := Hyperlinks[I];
end;

procedure TdxSpreadSheetXLSReader.UpdateProgress;
begin
  ProgressValue := MulDiv(Data.Position, 100, Data.Size);
end;

procedure TdxSpreadSheetXLSReader.ValidateContainers;
var
  I: Integer;
begin
  if CurrentSheet <> nil then
    for I := CurrentSheet.Containers.Count - 1 downto 0 do
    begin
      if not AreAnchorsAssigned(CurrentSheet.Containers[I]) then
        CurrentSheet.Containers.Delete(I);
    end;
end;

procedure TdxSpreadSheetXLSReader.ReadBlankCell;
begin
  CreateCell.Clear;
end;

procedure TdxSpreadSheetXLSReader.ReadBlankCells;
var
  AIndex, ARow, AStartColumn: Integer;
begin
  ARow := Reader.Words[0];
  AStartColumn := Reader.Words[1];
  for AIndex := 0 to RecordReader.Size div 2 - 4 do
    CreateCell(ARow, AIndex + AStartColumn, Reader.Words[2 + AIndex]).Clear;
end;

procedure TdxSpreadSheetXLSReader.ReadBOF;
var
  AIndex: Integer;
begin
  Check(Reader.ReadWord = $600, sdxInvalidStreamVersion);
  FCurrentSheet := nil;
  if Reader.Words[1] = $10 then
  begin
    AIndex := SheetOffset.IndexOf(RecordReader.StartPosition);
    if AIndex >= 0 then
      FCurrentSheet := SpreadSheet.Sheets[AIndex] as TdxSpreadSheetTableView;
  end;
  if (CurrentSheet = nil) and (SpreadSheet.SheetCount > 0) then
  begin
    FCurrentSheet := TdxSpreadSheetTableView(SpreadSheet.AddSheet);
    FCurrentSheet.Visible := False;
  end;
  if not CommonDataReady and (FCurrentSheet <> nil) then
    PrepareCommonData;
  Containers.Clear;
end;

procedure TdxSpreadSheetXLSReader.ReadBoolErrorCell;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := CreateCell;
  if Reader.Bytes[7] = 0 then
    ACell.AsBoolean := Boolean(Reader.Bytes[6])
  else
    ACell.AsError := XLSErrorToErrorCode(Reader.Bytes[6]);
end;

procedure TdxSpreadSheetXLSReader.ReadBoundSheet;
var
  AView: TdxSpreadSheetTableView;
begin
  if InRange((Reader.Words[2] shr 8) and 3, 0, 1) then
  begin
    SheetOffset.Add(Reader.Integers[0]);
    AView := AddTableView(Reader.XLS_ReadSimpleString(6));
    AView.Visible := not (Reader.Bytes[4] in [1, 2]);
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadSpecialName;

  function ReadDefinedName(const ACode: Char; out AName: TdxSpreadSheetDefinedName): Boolean;
  begin
    Result := TryDefineName(ACode, AName);
    if Result then
    begin
      RecordReader.Position := 16;
      with TdxXLSFormulaReader.Create(Self) do
      try
        ReadName(AName, Reader.Words[2], @PByteArray(RecordReader.Memory)^[RecordReader.Position]);
      finally
        Free;
      end;
    end;
  end;

var
  ACode: Char;
  ADefinedName: TdxSpreadSheetDefinedName;
begin
  ACode := Char(Reader.Bytes[15]);
  case ACode of
    #6, #7:
      if ReadDefinedName(ACode, ADefinedName) then
      begin
        if ACode = #6 then
          TdxSpreadSheetPrintAreasHelper.ImportPrintArea(ADefinedName)
        else
          TdxSpreadSheetPrintAreasHelper.ImportPrintTitles(ADefinedName);
      end;
  end;
  Names.Add(ACode);
end;

procedure TdxSpreadSheetXLSReader.ReadCalcCount;
begin
  SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount := Reader.ReadWord;
end;

procedure TdxSpreadSheetXLSReader.ReadCalcMode;
begin
  SpreadSheet.OptionsBehavior.AutomaticCalculation := Reader.ReadWord = 1;
end;

procedure TdxSpreadSheetXLSReader.ReadCodePage;
begin
  FCodePage := Reader.ReadWord;
end;

procedure TdxSpreadSheetXLSReader.ReadColumnFormatInformation;
var
  AColumn: TdxSpreadSheetTableColumn;
  AGroup: TdxSpreadSheetTableItemGroup;
  AIndex, AOptions, AOutlineLevel: Word;
begin
  AOptions := Reader.Words[4];
  for AIndex := Reader.Words[0] to Reader.Words[1] do
  begin
    AColumn := CurrentSheet.Columns.CreateItem(AIndex);
    AColumn.Size := MulDiv(Reader.Words[2], ColumnWidthHelper.MaxDigitWidth, 256);
    AColumn.Style.Handle := GetStyle(Reader.Words[3]).Handle;
    AColumn.Visible := AOptions and $01 = 0;
  end;
  AOutlineLevel := (AOptions shr 8) and $7;
  if AOutlineLevel > 0 then
    TdxSpreadSheetOutlineHelper.IncreaseOutlineLevelTo(CurrentSheet.Columns, Reader.Words[0], Reader.Words[1], AOutlineLevel - 1);
  if (AOptions shr 8) and $10 = $10 then
  begin
    AGroup := CurrentSheet.Columns.Groups.Find(Reader.Words[0] - 1);
    if AGroup <> nil then
      TdxSpreadSheetTableItemGroupAccess(AGroup).FCollapsedByUser := True;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadComment;
var
  AAuthorLength: Word;
  AColumn: Word;
  ACommentContainer: TdxSpreadSheetCommentContainer;
  AContainer: TdxSpreadSheetContainer;
  AContainerID: Integer;
  AOptions: Word;
  ARow: Word;
begin
  ARow := Reader.ReadWord;
  AColumn := Reader.ReadWord;
  AOptions := Reader.ReadWord;
  AContainerID := Reader.ReadWord;
  if Containers.TryGetValue(AContainerID, AContainer) then
  begin
    if AContainer is TdxSpreadSheetTextBoxContainer then
    try
      CurrentSheet.Containers.AddCommentContainer(CurrentSheet.CreateCell(ARow, AColumn), ACommentContainer);

      ACommentContainer.Assign(AContainer);
      if ACommentContainer.Shape.Brush.Color = TdxAlphaColors.Default then
        ACommentContainer.Shape.Brush.Color := dxColorToAlphaColor(ColorToRgb(clInfoBk), 255);
      if ACommentContainer.Shape.Pen.Brush.Color = TdxAlphaColors.Default then
        ACommentContainer.Shape.Pen.Brush.Color := TdxAlphaColors.Black;
      if not AreAnchorsAssigned(ACommentContainer) then
      begin
        TdxSpreadSheetContainerAccess(ACommentContainer).AnchorType := catAbsolute;
        TdxSpreadSheetContainerAccess(ACommentContainer).Calculator.UpdateAnchors(
          TdxSpreadSheetCommentContainerHelper.GetDefaultPosition(ACommentContainer.Cell));
      end;
      ACommentContainer.Visible := AOptions and $2 <> 0;

      AAuthorLength := Reader.ReadWord;
      if (RecordReader.StartPosition + RecordReader.Position) mod 2 <> 0 then
        Reader.ReadByte;
      ACommentContainer.Author := Reader.XLS_ReadString(0, AAuthorLength);
    finally
      Containers.AddOrSetValue(AContainerID, ACommentContainer);
      AContainer.Free;
    end;
  end;
end;

function TdxSpreadSheetXLSReader.CreateConditionalFormattingRule(
  AOptions: Word): TdxSpreadSheetConditionalFormattingRuleExpression;
const
  ComparisonOperatorMap: array[1..8] of TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator = (
    cicoBetween, cicoNotBetween, cicoEqual, cicoNotEqual, cicoGreaterThan,
    cicoLessThan, cicoGreaterThanOrEqual, cicoLessThanOrEqual
  );
begin
  Result := nil;
  case LoByte(AOptions) of
    1:
      if InRange(HiByte(AOptions), Low(ComparisonOperatorMap), High(ComparisonOperatorMap)) then
      begin
        Result := TdxSpreadSheetConditionalFormattingRuleCellIs.Create(CurrentSheet.ConditionalFormatting);
        TdxSpreadSheetConditionalFormattingRuleCellIs(Result).ComparisonOperator := ComparisonOperatorMap[HiByte(AOptions)];
      end;
    2:
      Result := TdxSpreadSheetConditionalFormattingRuleExpression.Create(CurrentSheet.ConditionalFormatting);
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadConditionalFormatting;
var
  ACount: Integer;
  ARule: TdxSpreadSheetXLSConditionalFormattingRule;
begin
  ARule := ConditionalFormattingRules.Add((Reader.Words[1] and $FFFE) shr 1);
  if Reader.Words[0] > 0 then
  begin
    RecordReader.Position := 12;
    ACount := Reader.ReadWord;
    ARule.Areas.Capacity := ACount;
    while ACount > 0 do
    begin
      ARule.AddArea(Reader.XLS_ReadREF);
      Dec(ACount);
    end;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadConditionalFormattingExtension;
var
  AFontHandle: TdxSpreadSheetFontHandle;
  AOptions: Byte;
  ARuleInfo: TdxSpreadSheetXLSConditionalFormattingRule;
  AStructPosition: Int64;
  AStructSize: Integer;
  AStyle: TdxXLSCellStyle;
  I: Integer;
begin
  if (Reader.Words[0] = brcCFEX) and (Reader.Integers[3] = 0) then
  begin
    RecordReader.Position := 16;
    if ConditionalFormattingRules.Get(Reader.ReadWord, ARuleInfo) and ARuleInfo.IsAssigned then
    begin
      Reader.ReadInteger;
      Reader.ReadWord;

      AOptions := Reader.ReadByte;
      if AOptions and $1 = 0 then
      begin
        for I := 0 to ARuleInfo.Areas.Count - 1 do
          ARuleInfo.Areas[I].Rule.Free;
        ARuleInfo.Areas.Clear;
      end
      else
        for I := 0 to ARuleInfo.Areas.Count - 1 do
          ARuleInfo.Areas[I].Rule.StopIfTrue := AOptions and $2 <> 0;

      if Reader.ReadByte <> 1 then
        Exit;
      AStructSize := Reader.ReadInteger;
      if AStructSize = 0 then
        Exit;

      AStyle := TdxXLSCellStyle.Create;
      try
        AStructPosition := RecordReader.Position;
        ReadConditionalFormattingRuleStyle(AStyle, AFontHandle);
        if RecordReader.Position - AStructPosition + 8 < AStructSize then
        begin
          RecordReader.Position := RecordReader.Position + 6;
          ReadXFExtensionNoFMT(AStyle);
        end;
        PrepareStyle(AStyle, AFontHandle);
        for I := 0 to ARuleInfo.Areas.Count - 1 do
          ARuleInfo.Areas[I].Rule.Style.Handle := AStyle.Handle;
      finally
        AStyle.Free;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadConditionalFormattingRule;
var
  ARule: TdxSpreadSheetConditionalFormattingRuleExpressionAccess;
  ARuleInfo: TdxSpreadSheetXLSConditionalFormattingRule;
  AValue1Size: Word;
  AValue2Size: Word;
  I: Integer;
begin
  ARuleInfo := ConditionalFormattingRules.Last;
  if ARuleInfo.Areas.Count > 0 then
  begin
    ARule := TdxSpreadSheetConditionalFormattingRuleExpressionAccess(CreateConditionalFormattingRule(Reader.ReadWord));
    if ARule <> nil then
    begin
      for I := 0 to ARuleInfo.Areas.Count - 1 do
        ARule.Areas.Add(ARuleInfo.Areas[I].Area);

      AValue1Size := Reader.ReadWord;
      AValue2Size := Reader.ReadWord;

      ReadConditionalFormattingRuleStyle(ARule.Style);

      ARule.Expression := ReadConditionalFormattingRuleExpression(AValue1Size, ARule.Area.Left, ARule.Area.Top);
      ARule.Expression2 := ReadConditionalFormattingRuleExpression(AValue2Size, ARule.Area.Left, ARule.Area.Top);

      ARuleInfo.ApplyRuleToAllAreas(ARule);
    end;
  end;
end;

function TdxSpreadSheetXLSReader.ReadConditionalFormattingRuleExpression(ARefSize, AAnchorColumn, AAnchorRow: Integer): string;

  function ReadFormula(AFormula: TdxSpreadSheetCustomFormula): Boolean;
  begin
    with TdxXLSFormulaReader.Create(Self) do
    try
      Result := ReadFormula(AFormula, ARefSize);
    finally
      Free;
    end;
  end;

var
  AFormula: TdxSpreadSheetCustomFormula;
begin
  Result := '';
  if ARefSize > 0 then
  begin
    AFormula := TdxSpreadSheetConditionalFormattingExpression.Create(CurrentSheet.ConditionalFormatting);
    try
      if ReadFormula(AFormula) then
      begin
        TdxSpreadSheetConditionalFormattingExpressionAccess(AFormula).FAnchorRow := AAnchorRow;
        TdxSpreadSheetConditionalFormattingExpressionAccess(AFormula).FAnchorColumn := AAnchorColumn;
        Result := AFormula.AsText;
      end;
      RecordReader.Position := RecordReader.Position + ARefSize;
    finally
      AFormula.Free;
    end;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadConditionalFormattingRuleStyle(
  AStyle: TdxXLSCellStyle; out AFontHandle: TdxSpreadSheetFontHandle);
const
  BorderFlagsMap: array[TcxBorder] of Integer = (
    CFRULE_OPTION_BORDER_LEFT, CFRULE_OPTION_BORDER_TOP,
    CFRULE_OPTION_BORDER_RIGHT, CFRULE_OPTION_BORDER_BOTTOM
  );
var
  ABorder: TcxBorder;
  AOptions: Integer;
  AValue: Integer;
begin
  AOptions := Reader.ReadInteger;
  Reader.ReadWord; // skip unused

  if AOptions and CFRULE_OPTION_FONT = CFRULE_OPTION_FONT then
    AFontHandle := ReadConditionalFormattingRuleStyleFont
  else
    AFontHandle := nil;

  if AOptions and CFRULE_OPTION_ALIGN = CFRULE_OPTION_ALIGN then
    Reader.ReadInt64;

  if AOptions and CFRULE_OPTION_BORDER = CFRULE_OPTION_BORDER then
  begin
    AStyle.Borders := [];
    for ABorder := Low(ABorder) to High(ABorder) do
    begin
      if AOptions and BorderFlagsMap[ABorder] = 0 then
        Include(AStyle.Borders, ABorder);
    end;
    AStyle.BordersStyle := Reader.ReadWord;
    AStyle.LeftRightBorders := Reader.ReadWord;
    AStyle.TopBottomBordersAndFill := Reader.ReadInteger and $3FFFFFF;
  end;

  if AOptions and CFRULE_OPTION_PATTERN = CFRULE_OPTION_PATTERN then
  begin
    AValue := (Reader.ReadWord and $FC00) shl 16;
    if AOptions and CFRULE_OPTION_PATTERN_STYLE <> 0 then
      AValue := $4000000;
    AStyle.TopBottomBordersAndFill := AStyle.TopBottomBordersAndFill or AValue;
    AStyle.FillColors := Reader.ReadWord;
  end;

  if AOptions and CFRULE_OPTION_PROTECTION = CFRULE_OPTION_PROTECTION then
    AStyle.Flags := Reader.ReadWord;
end;

procedure TdxSpreadSheetXLSReader.ReadConditionalFormattingRuleStyle(AStyle: TdxSpreadSheetCellStyle);
var
  AFontHandle: TdxSpreadSheetFontHandle;
  AXLSStyle: TdxXLSCellStyle;
begin
  AXLSStyle := TdxXLSCellStyle.Create;
  try
    ReadConditionalFormattingRuleStyle(AXLSStyle, AFontHandle);
    PrepareStyle(AXLSStyle, AFontHandle);
    AStyle.Handle := AXLSStyle.Handle;
  finally
    AXLSStyle.Free;
  end;
end;

function TdxSpreadSheetXLSReader.ReadConditionalFormattingRuleStyleFont: TdxSpreadSheetFontHandle;
var
  AOptions: Integer;
  AOptionsEx: Integer;
  APosition: Int64;
  AValue: Integer;
begin
  Result := CreateTempFontHandle;

  APosition := RecordReader.Position;
  try
    RecordReader.Position := APosition + 88;
    AOptions := Reader.ReadInteger;
    Reader.ReadInteger;
    AOptionsEx := Reader.ReadInteger;
  finally
    RecordReader.Position := APosition;
  end;

  APosition := RecordReader.Position;
  try
    AValue := Reader.ReadByte;
    if AValue > 0 then
      Result.Name := Reader.XLS_ReadString(Reader.ReadByte, AValue);
  finally
    RecordReader.Position := APosition + 64;
  end;

  AValue := Reader.ReadInteger;
  if AValue > 0 then
    Result.Size := Round(AValue / 20);

  AValue := Reader.ReadInteger;
  if (AValue and $2 <> 0) and (AOptions and CFRULE_FONT_FLAG_STYLE = 0) then
    Include(Result.Style, fsItalic);
  if (AValue and $8 <> 0) and (AOptions and CFRULE_FONT_FLAG_OUTLINE = 0) then
    Include(Result.Style, fsStrikeOut);
  if (Reader.ReadWord > $190) and (AOptions and CFRULE_FONT_FLAG_STYLE = 0) then
    Include(Result.Style, fsBold);

  Reader.ReadWord; // Escapement type

  if (Reader.ReadByte <> 0) and (AOptionsEx = 0) then
    Include(Result.Style, fsUnderline);

  Reader.ReadByte;
  Reader.ReadWord;

  AValue := Reader.ReadInteger;
  if AValue >= 0 then
    Result.Color := GetColor(AValue);

  RecordReader.Position := RecordReader.Position + 34;

  Result := AddFont(Result);
end;

procedure TdxSpreadSheetXLSReader.ReadDataFormat;
begin
  RecordReader.Position := 5;
  NumberFormats.Add(Reader.Words[0], AddNumberFormat(Reader.XLS_ReadString(Reader.Bytes[4], Reader.Bytes[2]), Reader.Words[0]));
end;

procedure TdxSpreadSheetXLSReader.ReadDateSystem;
begin
  SpreadSheet.OptionsView.DateTimeSystem := TdxSpreadSheetDateTimeSystem(Boolean(Reader.ReadWord));
end;



procedure TdxSpreadSheetXLSReader.ReadDefaultColumnWidth;
begin
  CurrentSheet.Options.DefaultColumnWidth := MulDiv(Reader.Words[0], ColumnWidthHelper.MaxDigitWidth, 256);
end;

procedure TdxSpreadSheetXLSReader.ReadDefColumnWidth;
begin
  CurrentSheet.Options.DefaultColumnWidth := Reader.Words[0] * (ColumnWidthHelper.MaxDigitWidth + 1);
end;

procedure TdxSpreadSheetXLSReader.ReadDefaultRowHeight;
begin
  CurrentSheet.Options.DefaultRowHeight := TdxValueUnitsHelper.PointsToPixels(Reader.Words[1] / 20);
end;

procedure TdxSpreadSheetXLSReader.ReadEOF;
begin
  ValidateContainers;
  Containers.Clear;
  FCurrentSheet := nil;
end;

procedure TdxSpreadSheetXLSReader.ReadExternSheet;
var
  I: Integer;
begin
  SetLength(FSheetIndexes, Reader.ReadWord);
  for I := 0 to Length(FSheetIndexes) - 1 do
    FSheetIndexes[I] := Reader.Words[I * 3 + 2];
end;

procedure TdxSpreadSheetXLSReader.ReadExternName;
var
  ADefinedName: TdxSpreadSheetDefinedName;
  AInfo: TdxSpreadSheetFunctionInfo;
  AName, ANameNoPrefix: string;
  AOptions, ASize: Word;
begin
  AOptions := Reader.ReadWord;
  RecordReader.Position := 7;
  AName := Reader.XLS_ReadString(Reader.ReadByte, Reader.Bytes[6]);
  ASize := Reader.ReadWord;
  if ASize < 5 then
  begin
    AInfo := dxSpreadSheetFunctionsRepository.GetInfoByName(AName);
    if (AInfo = nil) and (Pos(dxSpreadSheetFeatureFunctionPrefix, AName) = 1) then
    begin
      ANameNoPrefix := Copy(AName, Length(dxSpreadSheetFeatureFunctionPrefix) + 1, MaxInt);
      AInfo := dxSpreadSheetFunctionsRepository.GetInfoByName(ANameNoPrefix);
      if AInfo <> nil then
        AName := ANameNoPrefix;
    end;
    if AInfo = nil then
      AInfo := dxSpreadSheetFunctionsRepository.AddUnknown(AName);
    ExternalNames.AddObject(AName, AInfo);
  end
  else
    if (AOptions = 0) or (AOptions = 2) then
    begin
      ExternalNames.Add(AName);
      if TryDefineName(AName, ADefinedName) then
        with TdxXLSFormulaReader.Create(Self) do
        try
          ReadName(ADefinedName, ASize, @PByteArray(RecordReader.Memory)^[RecordReader.Position]);
        finally
          Free;
        end;
    end
    else
      ExternalNames.Add(AName);
end;

procedure TdxSpreadSheetXLSReader.ReadFilePassword;
begin
  Check(Reader.ReadWord = 1, sdxUnsupportedEncryption);
  FEncryptor := TdxXLSEncryptors.CreateEncryptor(Reader.ReadWord);
  FEncryptor.Load(Reader);
  PrepareEncryptorKey;
end;

procedure TdxSpreadSheetXLSReader.ReadFont;
var
  AFont: TdxXLSFont;
  AFlag: Integer;
begin
  AFont := TdxXLSFont.Create;
  AFont.Size := Round(Reader.Words[0] / 20);
  AFlag := Reader.Words[1];
  if (AFlag and $2) <> 0 then
    Include(AFont.Style, fsItalic);
  if (AFlag and $8) <> 0 then
    Include(AFont.Style, fsStrikeOut);
  AFlag := Reader.Words[3];
  if AFlag <> $190 then
    Include(AFont.Style, fsBold);
  AFont.Script := Reader.Words[4];
  if Reader.Bytes[10] <> 0 then
    Include(AFont.Style, fsUnderline);
  AFont.Charset := TFontCharset(Reader.Bytes[12]);
  if Integer(AFont.Charset) = 0 then
    AFont.Charset := DEFAULT_CHARSET;
  AFont.Name := Reader.XLS_ReadSimpleString(14);
  AFont.Color := Reader.Words[2];
  Fonts.Add(AFont);
end;

procedure TdxSpreadSheetXLSReader.ReadFormulaArray;
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := CurrentSheet.CreateCell(Reader.Words[0], Reader.Bytes[4]);
  RecordReader.Position := 12;
  with TdxXLSFormulaReader.Create(Self) do
  try
    ReadFormula(ACell);
  finally
    Free;
  end;
  TdxSpreadSheetTableViewAccess(CurrentSheet).AddArrayFormula(ACell.AsFormula,
    Rect(Reader.Bytes[4], Reader.Words[0], Reader.Bytes[5], Reader.Words[1]));
end;

procedure TdxSpreadSheetXLSReader.ReadFormulas(ATop, ALeft, ABottom, ARight, AOffset: Integer);
var
  ARow, AColumn: Integer;
  ACell: TdxSpreadSheetCell;
begin
  for ARow := ATop to ABottom do
  begin
    for AColumn := ALeft to ARight do
    begin
      ACell := CurrentSheet.CreateCell(ARow, AColumn);
      RecordReader.Position := AOffset;
      with TdxXLSFormulaReader.Create(Self) do
      try
        ReadFormula(ACell);
      finally
        Free;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadFormulaCell;
var
  ACell: TdxSpreadSheetCell;
begin
  with TdxXLSFormulaReader.Create(Self) do
  try
    ACell := CreateCell;
    RecordReader.Position := 20;
    ReadFormula(ACell);
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadHyperlink;
var
  AHelper: TdxBIFFHyperlinkHelper;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  AHyperlink := CurrentSheet.Hyperlinks.Add(Reader.XLS_ReadREF);
  AHelper := TdxBIFFHyperlinkHelper.Create(AHyperlink);
  try
    AHelper.LoadFromStream(RecordReader);
    Hyperlinks.AddObject(AHelper.Value, AHyperlink);
  finally
    AHelper.Free;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadHyperlinkTooltip;
var
  L: Integer;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
 if CurrentSheet.Hyperlinks.Last = nil then
   Exit;
  Reader.ReadWord;
  AHyperlink := CurrentSheet.Hyperlinks.Last;
  if not cxRectIsEqual(AHyperlink.Area, Reader.XLS_ReadREF) then
    Exit;
  L := (Reader.Stream.Size - Reader.Stream.Position) div 2;
  if L > 0 then
    AHyperlink.ScreenTip := Reader.XLS_ReadString(1, L - 1);
end;

procedure TdxSpreadSheetXLSReader.ReadLabelCell;
begin
  RecordReader.Position := 6;
  CreateCell.AsSharedString := StringTable.Add(Reader.XLS_ReadSharedString);
end;

procedure TdxSpreadSheetXLSReader.ReadIteration;
begin
  SpreadSheet.OptionsBehavior.IterativeCalculation := Reader.ReadWord = 1;
end;

procedure TdxSpreadSheetXLSReader.ReadLabelSSTCell;
begin
  Reader.Stream.Position := 6;
  CreateCell.AsSharedString := TdxSpreadSheetSharedString(Strings[Reader.ReadInteger]);
end;

procedure TdxSpreadSheetXLSReader.ReadMergedCells;
var
  R: TRect;
  AIndex: Integer;
begin
  for AIndex := 0 to Reader.ReadWord - 1 do
  begin
    R := Reader.XLS_ReadREF;
    if (R.Left <> R.Right) or (R.Top <> R.Bottom) then
      CurrentSheet.MergedCells.Add(R);
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadMulRKCells;
var
  AIndex, ARow, AStartColumn: Integer;
begin
  ARow := Reader.Words[0];
  AStartColumn := Reader.Words[1];
  for AIndex := 0 to (RecordReader.Size - 6) div 6 - 1 do
    CreateCell(ARow, AIndex + AStartColumn, Reader.Words[2 + AIndex * 3]).AsFloat := Reader.XLS_ReadRKValue((AIndex + 1) * 6);
end;

procedure TdxSpreadSheetXLSReader.ReadMSODrawing;
begin
  if CurrentSheet = nil then
    Exit;
  with TdxSpreadSheetMSODrawingReader.Create(Self) do
  try
    Read;
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadMSODrawingGroup;
begin
  with TdxSpreadSheetMSODrawingReader.Create(Self) do
  try
    Read;
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadMSODrawingObject;
var
  I, AType: Integer;
  AContainer: TdxSpreadSheetContainer;
  AKeys: TList<Word>;
  APair: TPair<Word, TdxSpreadSheetContainer>;

const
  oGroup        = $00;
  oLine         = $01;
  oRectangle    = $02;
  oOval         = $03;
  oArc          = $04;
  oChart        = $05;
  oText         = $06;
  oButton       = $07;
  oPicture      = $08;
  oPolygon      = $09;
  oCheckBox     = $0B;
  oOptionButton = $0C;
  oEditBox      = $0D;
  oLabel        = $0E;
  oDialogBox    = $0F;
  oSpinner      = $10;
  oScrollbar    = $11;
  oListbox      = $12;
  oGroupBox     = $13;
  oComboBox     = $14;
  oComment      = $19;
  oDrawing      = $1E;
  //
  AllowedObjects: set of Byte = [oGroup..oText, oPicture, oPolygon, oComment, oDrawing];
begin
  if Reader.ReadWord = ftCmo then
  begin
    Reader.ReadWord;
    AType := Reader.ReadWord;
    if CurrentSheet.Containers.Count > 0 then
    begin
      AContainer := CurrentSheet.Containers.Last;
      if Containers.ContainsValue(AContainer) then
      begin
        AKeys := TList<Word>.Create;
        try
          for APair in Containers do
            if APair.Value = AContainer then
              AKeys.Add(APair.Key);
          for I := 0 to AKeys.Count - 1 do
            Containers.Remove(AKeys[I]);
        finally
          AKeys.Free;
        end;
      end;
      if AType in AllowedObjects then //todo: only allowed containers can be used
        Containers.Add(Reader.ReadWord, AContainer)
      else
        AContainer.Free;
    end;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadMSOTextObject;

  function ReadFormattedString(ATextLength, ARunCount: Integer): TdxSpreadSheetFormattedSharedString;
  var
    ACharIndex, AFontIndex: Word;
  begin
    Result := TdxSpreadSheetFormattedSharedString.CreateObject(Reader.XLS_ReadString(1, ATextLength));
    while ARunCount > 0 do
    begin
      ACharIndex := Reader.ReadWord + 1;
      AFontIndex := Reader.ReadWord;
      Reader.ReadInteger;
      Result.Runs.Add(ACharIndex, nil, AFontIndex);
      Dec(ARunCount);
    end;
    PrepareFormattedSharedString(Result);
  end;

var
  AAlignFlags: Word;
  AContainer: TdxSpreadSheetTextBoxContainer;
  ARunCount: Word;
  ATextLength: Word;
begin
  if (CurrentSheet = nil) or (CurrentSheet.Containers.Count = 0) then
    Exit;

  AContainer := TextBoxContainerNeeded;
  AContainer.BeginUpdate;
  try
    AAlignFlags := Reader.ReadWord;
    Reader.ReadWord;
    Reader.ReadWord;
    Reader.ReadInteger;
    ATextLength := Reader.ReadWord;
    ARunCount := Reader.ReadWord div 8;
    Reader.ReadInteger;

    case (AAlignFlags and $E) shr 1 of
      2: AContainer.TextBox.AlignHorz := taCenter;
      3: AContainer.TextBox.AlignHorz := taRightJustify;
    else
      AContainer.TextBox.AlignHorz := taLeftJustify;
    end;

    case (AAlignFlags and $70) shr 4 of
      2: AContainer.TextBox.AlignVert := taVerticalCenter;
      3: AContainer.TextBox.AlignVert := taAlignBottom;
    else
      AContainer.TextBox.AlignVert := taAlignTop;
    end;

    if ATextLength > 0 then
    begin
      if ARunCount = 0 then
        AContainer.TextBox.Text := TdxSpreadSheetSharedString.CreateObject(Reader.XLS_ReadString(1, ATextLength))
      else
      begin
        AContainer.TextBox.Text := ReadFormattedString(ATextLength, ARunCount);
        AContainer.TextBox.Font.Handle := TdxSpreadSheetFormattedSharedString(AContainer.TextBox.Text).Runs[0].FontHandle;
      end;
   end;
  finally
    AContainer.EndUpdate;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadName;
var
  ADefinedName: TdxSpreadSheetDefinedName;
  AInfo: TdxSpreadSheetFunctionInfo;
  AName, ANameNoPrefix: string;
  AOptions, ASize: Word;
begin
  ADefinedName := nil;
  AOptions := Reader.ReadWord;
  if AOptions and $20 = $20 then
  begin
    ReadSpecialName;
    Exit;
  end;

  ASize := Reader.Words[2];
  RecordReader.Position := 14;
  AName := Reader.XLS_ReadString(Reader.ReadByte, Reader.Bytes[3]);
  if ASize < 5 then
  begin
    AInfo := dxSpreadSheetFunctionsRepository.GetInfoByName(AName);
    if (AInfo = nil) and (Pos(dxSpreadSheetFeatureFunctionPrefix, AName) = 1) then
    begin
      ANameNoPrefix := Copy(AName, Length(dxSpreadSheetFeatureFunctionPrefix) + 1, MaxInt);
      AInfo := dxSpreadSheetFunctionsRepository.GetInfoByName(ANameNoPrefix);
      if AInfo <> nil then
        AName := ANameNoPrefix;
    end;
    if AInfo = nil then
    begin
      if TryDefineName(AName, ADefinedName) then
      begin
        Names.AddObject(AName, ADefinedName);
        Exit;
      end;
      AInfo := dxSpreadSheetFunctionsRepository.AddUnknown(AName);
    end;
    Names.AddObject(AName, AInfo);
  end
  else
  begin
    if ((AOptions and $FF) in [0, 2, $10]) and TryDefineName(AName, ADefinedName) then
      NamesDefinition.Add(TdxXLSNameDefinition.Create(ADefinedName, ASize, @PByteArray(RecordReader.Memory)^[RecordReader.Position]));
    Names.AddObject(AName, ADefinedName);
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadNumberCell;
begin
  RecordReader.Position := 6;
  CreateCell().AsFloat := Reader.ReadDateTime;
end;

procedure TdxSpreadSheetXLSReader.ReadPanes;

  function ReadFrozenItem(AFirstVisibleItemIndex: Integer): Integer;
  begin
    Result := Reader.ReadWord - 1;
    if (Result >= 0) and (AFirstVisibleItemIndex > 0) then
      Inc(Result, AFirstVisibleItemIndex);
  end;

begin
  if HasFrozenPanes then
  begin
    CurrentSheet.FrozenColumn := ReadFrozenItem(CurrentSheet.LeftColumn);
    CurrentSheet.FrozenRow := ReadFrozenItem(CurrentSheet.TopRow);
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadPassword;
var
  AProtectionInfo: TdxSpreadSheetStandardProtectionInfo;
begin
  AProtectionInfo := TdxSpreadSheetStandardProtectionInfo.Create;
  AProtectionInfo.KeyWord := Reader.ReadWord;
  if CurrentSheet <> nil then
    CurrentSheet.OptionsProtection.ProtectionInfo := AProtectionInfo
  else
    SpreadSheet.OptionsProtection.ProtectionInfo := AProtectionInfo;
end;

procedure TdxSpreadSheetXLSReader.ReadPalette;
begin
  SetLength(Palette, Reader.ReadWord);
  Reader.Stream.ReadBuffer(Palette[0], Length(Palette) * SizeOf(TColor));
  TdxSpreadSheetCellStylesAccess(CellStyles).Palette := Palette;
end;

procedure TdxSpreadSheetXLSReader.ReadPageBreaksHorizontal;
var
  ACount, I: Integer;
  ABreaks: TList<Cardinal>;
begin
  ACount := Reader.ReadWord;
  ABreaks := CurrentSheet.OptionsPrint.Pagination.RowPageBreaks;
  for I := 0 to ACount - 1 do
  begin
    ABreaks.Add(Reader.ReadWord);
    Reader.ReadInteger;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadPageBreaksVertical;
var
  ACount, I: Integer;
  ABreaks: TList<Cardinal>;
begin
  ACount := Reader.ReadWord;
  ABreaks := CurrentSheet.OptionsPrint.Pagination.ColumnPageBreaks;
  for I := 0 to ACount - 1 do
  begin
    ABreaks.Add(Reader.ReadWord);
    Reader.ReadInteger; // 0, 65535
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadPrintCenterH;
begin
  CurrentSheet.OptionsPrint.Printing.HorizontalCentered := TdxDefaultBoolean(Reader.ReadWord = 1);
end;

procedure TdxSpreadSheetXLSReader.ReadPrintCenterV;
begin
  CurrentSheet.OptionsPrint.Printing.VerticalCentered := TdxDefaultBoolean(Reader.ReadWord = 1);
end;

procedure TdxSpreadSheetXLSReader.ReadPrintGridLines;
begin
  CurrentSheet.OptionsPrint.Source.GridLines := TdxDefaultBoolean(Reader.ReadWord = 1);
end;

procedure TdxSpreadSheetXLSReader.ReadPrintHeader;
begin
  if RecordReader.Size > 3 then
    TdxSpreadSheetHeaderFooterHelper.Parse(
      CurrentSheet.OptionsPrint.HeaderFooter.CommonHeader,
      Reader.XLS_ReadString(Reader.ReadByte, Reader.ReadWord));
end;

procedure TdxSpreadSheetXLSReader.ReadPrintHeaders;
begin
  CurrentSheet.OptionsPrint.Source.Headers := TdxDefaultBoolean(Reader.ReadWord = 1);
end;

procedure TdxSpreadSheetXLSReader.ReadPrintFooter;
begin
  if RecordReader.Size >= 3 then
    TdxSpreadSheetHeaderFooterHelper.Parse(
      CurrentSheet.OptionsPrint.HeaderFooter.CommonFooter,
      Reader.XLS_ReadString(Reader.ReadByte, Reader.ReadWord));
end;

procedure TdxSpreadSheetXLSReader.ReadPrintLeftMargin;
begin
  CurrentSheet.OptionsPrint.Page.Margins.Left := Reader.ReadDateTime;
end;

procedure TdxSpreadSheetXLSReader.ReadPrintTopMargin;
begin
  CurrentSheet.OptionsPrint.Page.Margins.Top := Reader.ReadDateTime;
end;

procedure TdxSpreadSheetXLSReader.ReadPrintRightMargin;
begin
  CurrentSheet.OptionsPrint.Page.Margins.Right := Reader.ReadDateTime;
end;

procedure TdxSpreadSheetXLSReader.ReadPrintBottomMargin;
begin
  CurrentSheet.OptionsPrint.Page.Margins.Bottom := Reader.ReadDateTime;
end;

procedure TdxSpreadSheetXLSReader.ReadPrintSetup;
var
  AOptions: Word;
const
  AOrientation: array[Boolean] of TdxSpreadSheetTableViewOptionsPrintPageOrientation = (oppoLandscape, oppoPortrait);
  APageOrder: array[Boolean] of TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder = (opppDownThenOver, opppOverThenDown);
begin
  AOptions := Reader.Words[5];
  CurrentSheet.OptionsPrint.Page.Paper.SizeID := Reader.Words[0];
  CurrentSheet.OptionsPrint.Page.Orientation := AOrientation[AOptions and $2 = $2];
  CurrentSheet.OptionsPrint.Page.Scale := Reader.Words[1];
  //
  CurrentSheet.OptionsPrint.Page.FirstPageNumber := Reader.Words[2];
  CurrentSheet.OptionsPrint.Page.FitToWidth := Reader.Words[3];
  CurrentSheet.OptionsPrint.Page.FitToHeight := Reader.Words[4];
  CurrentSheet.OptionsPrint.Printing.Copies := Reader.Words[16];
  CurrentSheet.OptionsPrint.Printing.BlackAndWhite := TdxDefaultBoolean(AOptions and $8 = $8);
  CurrentSheet.OptionsPrint.Printing.Draft := TdxDefaultBoolean(AOptions and $10 = $10);
  CurrentSheet.OptionsPrint.Printing.PageOrder := APageOrder[AOptions and $1 = 1];
  Reader.Stream.Position := 16;
  CurrentSheet.OptionsPrint.Page.Margins.Header := Reader.ReadDateTime;
  CurrentSheet.OptionsPrint.Page.Margins.Footer := Reader.ReadDateTime;
end;

procedure TdxSpreadSheetXLSReader.ReadPrintWorkspaceInfo;
const
  AScaleModes: array[Boolean] of TdxSpreadSheetTableViewOptionsPrintPageScaleMode = (oppsmAdjustToScale, oppsmFitToPage);
begin
  CurrentSheet.OptionsPrint.Page.ScaleMode := AScaleModes[Reader.Words[0] and $100 = $100];
  CurrentSheet.Rows.Groups.ExpandButtonPosition := TdxSpreadSheetTableItemGroupExpandButtonPosition(Reader.Words[0] and $40 = $40);
  CurrentSheet.Columns.Groups.ExpandButtonPosition := TdxSpreadSheetTableItemGroupExpandButtonPosition(Reader.Words[0] and $80 = $80);
end;

procedure TdxSpreadSheetXLSReader.ReadProtection;
begin
  if CurrentSheet <> nil then
    CurrentSheet.OptionsProtection.Protected := Reader.ReadWord = 1
  else
  begin
    SpreadSheet.OptionsProtection.Protected := Reader.ReadWord = 1;
    SpreadSheet.OptionsProtection.AllowChangeStructure := not SpreadSheet.OptionsProtection.Protected;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadRowFormatInformation;
var
  AGroup: TdxSpreadSheetTableItemGroup;
  AOptions, AOutlineLevel: Word;
  ARow: TdxSpreadSheetTableRow;
  ASize: Integer;
begin
  AOptions := Reader.Words[6];
  ARow := CurrentSheet.Rows.CreateItem(Reader.Words[0]);
  ARow.Visible := (AOptions and $20 = 0);
  AOutlineLevel := AOptions and $7;
  if AOutlineLevel > 0 then
    TdxSpreadSheetOutlineHelper.IncreaseOutlineLevelTo(CurrentSheet.Rows, ARow.Index, AOutlineLevel - 1);
  if AOptions and $10 = $10 then
  begin
    AGroup := CurrentSheet.Rows.Groups.Find(Reader.Words[0] - 1);
    if AGroup <> nil then
      TdxSpreadSheetTableItemGroupAccess(AGroup).FCollapsedByUser := True;
  end;
  ASize := TdxValueUnitsHelper.PointsToPixels(Reader.Words[3] / 20);
  if AOptions and $80 = $80 then
  begin
    ARow.Style.Handle := GetStyle((Reader.Words[7] and $FFF)).Handle;
    if ASize > 32768 then
      Exit;
  end;
  ARow.Size := ASize;
  TdxSpreadSheetRowAccess(ARow).IsCustomSize := AOptions and $40 = $40;
  ARow.Visible := (AOptions and $20 = 0);
end;

procedure TdxSpreadSheetXLSReader.ReadRKCell;
begin
  CreateCell.AsFloat := Reader.XLS_ReadRKValue(6);
end;

procedure TdxSpreadSheetXLSReader.ReadSelection;
var
  AIndex, ACount: Integer;
  AFocusedRow, AFocusedColumn: Word;
begin
  CurrentSheet.Selection.Clear;
  Reader.ReadByte;
  AFocusedRow := Reader.ReadWord;
  AFocusedColumn := Reader.ReadWord;
  Reader.ReadWord;
  ACount := Reader.ReadWord;
  for AIndex := 0 to ACount - 1 do
    CurrentSheet.Selection.Add(Reader.XLS_ReadSimpleREF, [ssShift]);
  CurrentSheet.Selection.SetFocused(AFocusedRow, AFocusedColumn, [ssShift]);
end;

procedure TdxSpreadSheetXLSReader.ReadSharedFeatures;
var
  AFlags: Integer;
  ADataSize: Integer;
begin
  Check(Reader.ReadWord = brcFEATHEADR, sdxErrorInternal);
  Reader.ReadWord;
  Reader.ReadInt64;
  if Reader.ReadWord = 2 then // Type: Enhanced Protection
  begin
    Check(Reader.ReadByte = 1, sdxErrorInternal);
    ADataSize := Reader.ReadInteger;
    if (ADataSize < 0) or (ADataSize = 4) then
    begin
      AFlags := Reader.ReadInteger;
      CurrentSheet.OptionsProtection.AllowDeleteColumns := iprotDeleteColumns and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowDeleteRows := iprotDeleteRows and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowResizeColumns := iprotFormatColumns and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowEditContainers := iprotObject and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowEditHyperlinks := iprotInsertHyperlinks and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowResizeRows := iprotFormatRows and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowFormatCells := iprotFormatCells and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowInsertColumns := iprotInsertColumns and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowInsertRows := iprotInsertRows and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowSelectLockedCells := iprotSelLockedCells and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowSelectUnlockedCells := iprotSelUnlockedCells and AFlags <> 0;
      CurrentSheet.OptionsProtection.AllowSort := iprotSort and AFlags <> 0;
    end;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadSharedFormula;
begin
  ReadFormulas(Reader.Words[0], Reader.Bytes[4], Reader.Words[1], Reader.Bytes[5], 8);
end;

procedure TdxSpreadSheetXLSReader.ReadSharedStringTable;
var
  AIndex: Integer;
begin
  Strings.Count := Reader.Integers[1];
  RecordReader.Position := 8;
  for AIndex := 0 to Strings.Count - 1 do
    TList(Strings).Items[AIndex] := Reader.XLS_ReadSharedString;
end;

procedure TdxSpreadSheetXLSReader.ReadTheme;
begin
  RecordReader.Position := 16;
  ThemeHelper.LoadFromStream(RecordReader);
end;

procedure TdxSpreadSheetXLSReader.ReadWindow1Information;
var
  AOptions: Word;
begin
  AOptions := Reader.Words[4];
  ActiveTabIndex := Reader.Words[5];
  FirstTabIndex := Reader.Words[6];
  SpreadSheet.OptionsView.HorizontalScrollBar := AOptions and $8 = $8;
  SpreadSheet.OptionsView.VerticalScrollBar := AOptions and $10 = $10;
  SpreadSheet.PageControl.Visible := AOptions and $20 = $20;
end;

procedure TdxSpreadSheetXLSReader.ReadWindow2Information;
var
  AOptions: Word;
  AColor: Integer;
begin
  AOptions := Reader.ReadWord;
  CurrentSheet.TopRow := Reader.ReadWord;
  CurrentSheet.LeftColumn := Reader.ReadWord;
  AColor := Reader.ReadInteger;
  CurrentSheet.Options.ShowFormulas := TdxDefaultBoolean(AOptions and $1 = $1);
  CurrentSheet.Options.GridLines := TdxDefaultBoolean(AOptions and $2 = $2);
  CurrentSheet.Options.Headers := TdxDefaultBoolean(AOptions and $4 = $4);
  CurrentSheet.Options.ZeroValues := TdxDefaultBoolean(AOptions and $10 = $10);
  HasFrozenPanes := AOptions and $8 = $8;
  if (AOptions and $20 = 0) and (AColor <> 0) and (AColor <> $40) and InRange(AColor, 0, Length(Palette) - 1) then
    SpreadSheet.OptionsView.GridLineColor := Palette[AColor];
end;

procedure TdxSpreadSheetXLSReader.ReadXFRecord;
var
  AStyle: TdxXLSCellStyle;
begin
  AStyle := TdxXLSCellStyle.Create;
  AStyle.Font := Reader.ReadWord;
  AStyle.Format := Reader.ReadWord;
  AStyle.Flags := Reader.ReadWord;
  AStyle.Alignments := Reader.ReadWord;
  AStyle.Indents := Reader.ReadWord;
  AStyle.BordersStyle := Reader.ReadWord;
  AStyle.LeftRightBorders := Reader.ReadWord;
  AStyle.TopBottomBordersAndFill := Reader.ReadInteger;
  AStyle.FillColors := Reader.ReadWord;
  FStyles.Add(AStyle);
end;

procedure TdxSpreadSheetXLSReader.ReadXFExtension;
var
  AStyle: TdxXLSCellStyle;
begin
  if Reader.Words[7] < Styles.Count then
  begin
    AStyle := TdxXLSCellStyle(Styles.List[Reader.Words[7]]);
    RecordReader.Position := 18;
    ReadXFExtensionNoFMT(AStyle);
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadXFExtensionNoFMT(AStyle: TdxXLSCellStyle);
var
  APropType: TdxXFExtPropertyID;
  I, APropSize, APosition, AColor: Integer;

  function ReadColor(var AColor: Integer): Boolean;
  var
    APropData: TdxXFExtColorProp;
  begin
    AColor := 0;
    Result := True;
    RecordReader.ReadBuffer(APropData, SizeOf(APropData));

    case APropData.xclrType of
      1:
       AColor := GetColor(APropData.xclrValue);
      2:
       with TRGBQuad(APropData.xclrValue) do
         AColor := RGB(rgbBlue, rgbGreen, rgbRed);
      3:
      begin
        AStyle.ExtendedData.SetThemeColorValue(APropType, APropData.xclrValue, APropData.nTintShade);
        AColor := APropData.xclrValue;
      end;
    else
      Result := False;
    end;
  end;

var
  ACount: Integer;
begin
  if AStyle.ExtendedData = nil then
    AStyle.ExtendedData := TdxXFExtPropertyData.Create;

  ACount := Reader.ReadWord;
  for I := 0 to ACount - 1 do
  begin
    APosition := RecordReader.Position;
    APropType := TdxXFExtPropertyID(Reader.ReadWord);
    APropSize := Reader.ReadWord;
    case APropType of
      xfextRGBForeColor, xfextRGBBackColor, xfextForeColor, xfextBackColor, xfextTextColor,
      xfextBorderColorTop, xfextBorderColorBottom, xfextBorderColorLeft, xfextBorderColorRight:
        if ReadColor(AColor) then
          AStyle.ExtendedData.SetValue(APropType, AColor);

      xfextIndent:
        AStyle.ExtendedData.SetValue(APropType, Reader.ReadWord);
    end;
    RecordReader.Position := APosition + APropSize;
  end;
end;

procedure TdxSpreadSheetXLSReader.ReadZoomFactor;
var
  ANum, ADenom: Word;
begin
  ANum := Reader.ReadWord;
  ADenom := Reader.ReadWord;
  CurrentSheet.Options.ZoomFactor := MulDiv(100, ANum, ADenom);
end;

procedure TdxSpreadSheetXLSReader.SetProgressValue(AValue: Integer);
begin
  if FProgressValue <> AValue then
  begin
    FProgressValue := AValue;
    DoProgress(AValue);
  end;
end;

{ TdxXLSRecordReader }

constructor TdxXLSRecordReader.Create(AOwner: TdxSpreadSheetXLSReader);
begin
  inherited Create;
  FOwner := AOwner;
  FParts := TList<Integer>.Create;
end;

destructor TdxXLSRecordReader.Destroy;
begin
  FreeAndNil(FParts);
  inherited Destroy;
end;

procedure TdxXLSRecordReader.AddPart(ASize: Word);
var
  ABuffer: PByte;
begin
  if Parts.Count = 0 then
    FSize := 0;
  Inc(FSize, ASize);
  Parts.Add(FSize);
  inherited Size := FSize;

  ABuffer := @PByteArray(Memory)^[FSize - ASize];
  Source.ReadBuffer(ABuffer^, ASize);
  if (Encryptor <> nil) and (RecordID <> brcBOF) then
  begin
    if RecordID = brcBoundSheet then
    begin
      Inc(ABuffer, SizeOf(Integer));
      Dec(ASize, SizeOf(Integer));
    end;
    Encryptor.Decrypt(ABuffer, Source.Position - ASize, ASize);
  end;
end;

procedure TdxXLSRecordReader.Initialize;
var
  AType: Word;
begin
  Clear;
  Parts.Clear;
  CurrentPart := 0;
  AddPart(FSize);
  while not Owner.IsEOF do
  begin
    AType := ReadWordFunc(Source);
    if (AType = brcContinue) or ((RecordID = brcMSODRAWINGGROUP) and (AType = RecordID)) then
      AddPart(ReadWordFunc(Source))
    else
    begin
      Source.Position := Source.Position - 2;
      Break;
    end;
  end;
end;

procedure TdxXLSRecordReader.PrepareRecord;
begin
  FStartPosition := Source.Position;
  FRecordID := ReadWordFunc(Source);
  FSize := ReadWordFunc(Source);
end;

procedure TdxXLSRecordReader.SkipRecord;
begin
  Source.Position := Source.Position + Size;
  while not Owner.IsEOF do
  begin
    if ReadWordFunc(Source) = brcCONTINUE then
    begin
      FSize := ReadWordFunc(Source);
      Source.Position := Source.Position + FSize;
    end
    else
    begin
      Source.Position := Source.Position - 2;
      Break;
    end;
  end;
end;

function TdxXLSRecordReader.GetEncryptor: TdxXLSAbstractEncryptor;
begin
  Result := Owner.Encryptor;
end;

function TdxXLSRecordReader.GetSource: TStream;
begin
  Result := Owner.Data;
end;

{ TdxXLSReader }

constructor TdxXLSReader.Create(ARecord: TdxXLSRecordReader);
begin
  inherited Create(ARecord);
  FXLSRecord := ARecord;
end;

function TdxXLSReader.XLS_ReadREF: TRect;
begin
  Result.Top := ReadWord;
  Result.Bottom := ReadWord;
  Result.Left := ReadWord;
  Result.Right := ReadWord;
end;

function TdxXLSReader.XLS_ReadRKValue(AOffset: Integer): Double;
var
  AValue: Integer;
begin
  Result := 0;
  FXLSRecord.Position := AOffset;
  AValue := ReadInteger;
  PIntegerArray(@Result)^[1] := Integer(AValue and $FFFFFFFC);
  case AValue and 3 of
    1:
      Result := Result / 100;
    2:
      Result := Integer(AValue and $FFFFFFFC) / 4;
    3:
      Result := Integer(AValue and $FFFFFFFC) / 400;
  end;
end;

function TdxXLSReader.XLS_ReadSimpleREF: TRect;
begin
  Result.Top := ReadWord;
  Result.Bottom := ReadWord;
  Result.Left := ReadByte;
  Result.Right := ReadByte;
end;

function TdxXLSReader.XLS_ReadSimpleString(AOffset: Integer): string;
var
  AOptions, ALength: Byte;
begin
  Stream.Position := AOffset;
  ALength := ReadByte;
  AOptions := ReadByte;
  Result := XLS_ReadString(AOptions, ALength);
end;

function TdxXLSReader.XLS_ReadSharedString: TdxSpreadSheetSharedString;
var
  AExtRstLen: Integer;
  ALength, AFormatRunsCount: Word;
  AOptions: Byte;
begin
  ALength := ReadWord;
  AOptions := ReadByte;
  if ALength = 0 then
    Exit(TdxSpreadSheetSharedString.CreateObject(''));

  if (AOptions and $08) <> 0 then
    AFormatRunsCount := ReadWord
  else
    AFormatRunsCount := 0;

  if AOptions and $04 <> 0 then
    AExtRstLen := ReadInteger
  else
    AExtRstLen := 0;

  Result := XLS_ReadSharedString(AOptions, ALength, AFormatRunsCount);
  Stream.Position := Stream.Position + AExtRstLen;
end;

function TdxXLSReader.XLS_ReadSharedString(AOptions, ALength, ARunCount: Integer): TdxSpreadSheetSharedString;
var
  ACharIndex, AFontIndex: Word;
begin
  if ALength = 0 then
    Result := TdxSpreadSheetSharedString.CreateObject('')
  else
    if ARunCount = 0 then
      Result := TdxSpreadSheetSharedString.CreateObject(XLS_ReadString(AOptions, ALength))
    else
    begin
      Result := TdxSpreadSheetFormattedSharedString.CreateObject(XLS_ReadString(AOptions, ALength));
      while ARunCount > 0 do
      begin
        ACharIndex := ReadWord + 1;
        AFontIndex := ReadWord;
        TdxSpreadSheetFormattedSharedString(Result).Runs.Add(ACharIndex, nil, AFontIndex);
        Dec(ARunCount);
      end;
    end;
end;

function TdxXLSReader.XLS_ReadString(const AOptions, ALength: Integer): string;
var
  ACharSize: Byte;
  AAnsiString: AnsiString;
  AFirstSize, ASecondSize, AOffset: Integer;
  L: Integer;
begin
  ACharSize := Byte(AOptions and $01 <> 0) + 1;
  CheckRange(ALength * ACharSize, AFirstSize, ASecondSize, AOffset);
  L := AFirstSize div ACharSize;
  if ACharSize = 2 then
  begin
    SetLength(Result, L);
    Stream.ReadBuffer(Result[1], L * SizeOf(WideChar));
  end
  else
  begin
    SetLength(AAnsiString, L);
    Stream.ReadBuffer(AAnsiString[1], L);
    Result := dxAnsiStringToString(AAnsiString, XLSRecord.Owner.CodePage);
  end;
  if ASecondSize > 0 then
  begin
    Stream.Position := AOffset;
    Result := Result + XLS_ReadString(ReadByte, ASecondSize div ACharSize);
  end;
end;

procedure TdxXLSReader.CheckRange(const ASize: Integer; var AFirstSize, ASecondSize, APosition: Integer);
var
  I, AStartPos, AFinishPos: Integer;
begin
  AFirstSize := ASize;
  ASecondSize := 0;
  APosition := 0;
  if XLSRecord.Parts.Count = 1 then Exit;
  AStartPos := XLSRecord.Position;
  AFinishPos := AStartPos + ASize;
  for I := XLSRecord.CurrentPart to XLSRecord.Parts.Count - 1 do
  begin
    APosition := XLSRecord.Parts[I];
    if AFinishPos < APosition then
      Break
    else
      if AStartPos <= APosition then
      begin
        XLSRecord.CurrentPart := I;
        if AStartPos = APosition then
          AFirstSize := APosition - AStartPos
        else
          AFirstSize := APosition - AStartPos;
        ASecondSize := ASize - AFirstSize;
        Break;
      end
  end;
end;

function TdxXLSReader.GetByteByIndex(AIndex: Integer): Byte;
begin
  Result := PByteArray(FXLSRecord.Memory)^[AIndex];
end;

function TdxXLSReader.GetIntegerByIndex(AIndex: Integer): Integer;
begin
  Result := PIntegerArray(FXLSRecord.Memory)^[AIndex];
end;

function TdxXLSReader.GetWordByIndex(AIndex: Integer): Word;
begin
  Result := PWordArray(FXLSRecord.Memory)^[AIndex];
end;

end.
