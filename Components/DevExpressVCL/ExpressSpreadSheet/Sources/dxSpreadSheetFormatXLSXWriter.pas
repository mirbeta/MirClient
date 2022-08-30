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

unit dxSpreadSheetFormatXLSXWriter;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics,
  dxCore, dxCoreClasses, cxClasses, cxGeometry, dxCustomTree, dxXMLDoc, dxZIPUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetUtils, dxSpreadSheetGraphics, Generics.Defaults, Generics.Collections, dxGDIPlusClasses, dxCoreGraphics,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetHyperlinks,
  dxSpreadSheetFormatXLSXTags, dxSpreadSheetProtection, dxSpreadSheetCoreStyles, dxXMLWriter;

type

  { TdxSpreadSheetXLSXWriterResourceList }

  TdxSpreadSheetXLSXWriterResourceList = class
  strict private const
    DefaultCapacity = 10240;
  strict private
    FData: TdxFastList;
    FIndex: TDictionary<Pointer, Integer>;

    function GetCount: Integer; inline;
    function GetList: PdxPointerList; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AObject: TObject): Integer;// inline;
    function IndexOf(AObject: TObject): Integer;// inline;
    //
    property Count: Integer read GetCount;
    property List: PdxPointerList read GetList;
  end;

  { TdxSpreadSheetXLSXWriterRels }

  TdxSpreadSheetXLSXWriterRels = class(TdxSpreadSheetXMLDocument)
  strict private
    function GetEmpty: Boolean;
  protected
    procedure ConvertToRelativePaths(const ARootPath: TdxXMLString);
    function DoAddRelationship(const AType, ATarget: TdxXMLString): TdxXMLNode;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure AddExternalRelationship(const AType, ATarget: TdxXMLString);
    procedure AddRelationship(const AType, ATarget: TdxXMLString);
    function GetRelationshipId(const ATarget: TdxXMLString): TdxXMLString;
    //
    property Empty: Boolean read GetEmpty;
  end;

  { TdxSpreadSheetXLSXCustomWriter }

  TdxSpreadSheetXLSXCustomWriter = class(TdxSpreadSheetCustomPackedWriter)
  strict private
    FTargetStream: TStream;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
  end;

  { TdxSpreadSheetXLSXWriter }

  TdxSpreadSheetXLSXWriter = class(TdxSpreadSheetXLSXCustomWriter)
  strict private
    FBorders: TdxSpreadSheetXLSXWriterResourceList;
    FCellStyleDefault: TdxSpreadSheetCellStyleHandle;
    FCellStyles: TdxSpreadSheetXLSXWriterResourceList;
    FColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper;
    FConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList;
    FContentType: TdxSpreadSheetXMLDocument;
    FCustomFormats: TdxSpreadSheetXLSXWriterResourceList;
    FFills: TdxSpreadSheetXLSXWriterResourceList;
    FFonts: TdxSpreadSheetXLSXWriterResourceList;
    FImages: TDictionary<TdxGPImage, AnsiString>;
    FSharedStrings: TdxSpreadSheetXLSXWriterResourceList;
  protected
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    function GetContentTypeID: AnsiString; virtual;

    procedure AddDefaultResources;
    procedure InitializeContentTypes(ANode: TdxXMLNode);

    procedure RegisterFile(const AFileName, AContentType: AnsiString;
      const ARelationship: AnsiString = ''; ARels: TdxSpreadSheetXLSXWriterRels = nil); virtual;
    procedure WriteRels(const AFileName: AnsiString; ADocument: TdxSpreadSheetXLSXWriterRels);
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    procedure WriteData; override;

    property Borders: TdxSpreadSheetXLSXWriterResourceList read FBorders;
    property CellStyleDefault: TdxSpreadSheetCellStyleHandle read FCellStyleDefault;
    property CellStyles: TdxSpreadSheetXLSXWriterResourceList read FCellStyles;
    property ConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList read FConditionalFormattingStyles;
    property CustomFormats: TdxSpreadSheetXLSXWriterResourceList read FCustomFormats;
    property Fills: TdxSpreadSheetXLSXWriterResourceList read FFills;
    property Fonts: TdxSpreadSheetXLSXWriterResourceList read FFonts;
    property Images: TDictionary<TdxGPImage, AnsiString> read FImages;
    property SharedStrings: TdxSpreadSheetXLSXWriterResourceList read FSharedStrings;
    //
    property ColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper read FColumnWidthHelper;
  end;

  { TdxSpreadSheetXLSXWriterCustomBuilder }

  TdxSpreadSheetXLSXWriterCustomBuilder = class(TdxSpreadSheetCustomPackedWriterBuilder)
  strict private
    FOwnerRels: TdxSpreadSheetXLSXWriterRels;

    function GetOwner: TdxSpreadSheetXLSXWriter;
  protected
    procedure RegisterFile(const AFileName, AContentType: AnsiString;
      const ARelationship: AnsiString = ''; ARels: TdxSpreadSheetXLSXWriterRels = nil); inline;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels);
    function WriteImage(AImage: TdxGPImage): AnsiString;
    //
    property Owner: TdxSpreadSheetXLSXWriter read GetOwner;
    property OwnerRels: TdxSpreadSheetXLSXWriterRels read FOwnerRels;
  end;

  { TdxSpreadSheetXLSXWriterCustomFileBuilder }

  TdxSpreadSheetXLSXWriterCustomFileBuilder = class(TdxSpreadSheetXLSXWriterCustomBuilder)
  strict private
    FTargetFileName: AnsiString;

    function GetTargetFileNameRels: AnsiString; inline;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXWriter;
      AOwnerRels: TdxSpreadSheetXLSXWriterRels; const ATargetFileName: AnsiString);
    //
    property TargetFileName: AnsiString read FTargetFileName;
    property TargetFileNameRels: AnsiString read GetTargetFileNameRels;
  end;

  { TdxSpreadSheetXLSXWriterCustomXMLBuilder }

  TdxSpreadSheetXLSXWriterCustomXMLBuilder = class(TdxSpreadSheetXLSXWriterCustomFileBuilder)
  strict private const
    BufferCapacity = 1048576;
  protected
    class function GetContentRelationship: AnsiString; virtual; abstract;
    class function GetContentType: AnsiString; virtual; abstract;
  public
    procedure Execute; override; final;
    procedure ExecuteCore(AWriter: TdxXmlWriter); virtual; abstract;
  end;

  { TdxSpreadSheetXLSXWriterExternalLinkBuilder }

  TdxSpreadSheetXLSXWriterExternalLinkBuilder = class(TdxSpreadSheetXLSXWriterCustomFileBuilder)
  strict private
    FLink: TdxSpreadSheetExternalLink;
  protected
    function EncodePath(const APath: string): TdxXMLString; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
      ALink: TdxSpreadSheetExternalLink; const ATargetFileName: AnsiString);
    procedure Execute; override;
    //
    property Link: TdxSpreadSheetExternalLink read FLink;
  end;

  { TdxSpreadSheetXLSXWriterFormattedStringBuilder }

  TdxSpreadSheetXLSXWriterFormattedStringBuilder = class(TdxSpreadSheetXLSXWriterCustomBuilder)
  strict private
    FString: TdxSpreadSheetFormattedSharedString;
    FWriter: TdxXmlWriter;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
      AWriter: TdxXmlWriter; AString: TdxSpreadSheetFormattedSharedString);
    procedure Execute; override;
  end;

  { TdxSpreadSheetXLSXWriterSharedStringsBuilder }

  TdxSpreadSheetXLSXWriterSharedStringsBuilder = class(TdxSpreadSheetXLSXWriterCustomXMLBuilder)
  strict private
    function GetSharedStrings: TdxSpreadSheetXLSXWriterResourceList; inline;
  protected
    class function GetContentRelationship: AnsiString; override;
    class function GetContentType: AnsiString; override;
  public
    procedure ExecuteCore(AWriter: TdxXmlWriter); override;
    //
    property SharedStrings: TdxSpreadSheetXLSXWriterResourceList read GetSharedStrings;
  end;

  { TdxSpreadSheetXLSXWriterStylesBuilder }

  TdxXLSXStyleForEachProc = reference to procedure (AWriter: TdxXmlWriter; AUserData: Pointer);

  TdxSpreadSheetXLSXWriterStylesBuilder = class(TdxSpreadSheetXLSXWriterCustomXMLBuilder)
  strict private
    FFormats: TdxSpreadSheetXLSXWriterResourceList;

    function GetBorders: TdxSpreadSheetXLSXWriterResourceList; inline;
    function GetCellStyleDefault: TdxSpreadSheetCellStyleHandle; inline;
    function GetCellStyles: TdxSpreadSheetXLSXWriterResourceList; inline;
    function GetConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList; inline;
    function GetCustomFormats: TdxSpreadSheetXLSXWriterResourceList; inline;
    function GetFills: TdxSpreadSheetXLSXWriterResourceList; inline;
    function GetFonts: TdxSpreadSheetXLSXWriterResourceList; inline;
  protected
    class function GetContentRelationship: AnsiString; override;
    class function GetContentType: AnsiString; override;
    procedure PrepareResources;

    procedure ProcessStylesGroup(AWriter: TdxXmlWriter; const ANodeName: string;
      AStyles: TdxSpreadSheetXLSXWriterResourceList; AProc: TdxXLSXStyleForEachProc);

    procedure ProcessStylesGroupBorders(AWriter: TdxXmlWriter; AData: Pointer);
    procedure ProcessStylesGroupCellStyle(AWriter: TdxXmlWriter; AData: Pointer);
    procedure ProcessStylesGroupConditionalFormatting(AWriter: TdxXmlWriter; AData: Pointer);
    procedure ProcessStylesGroupFill(AWriter: TdxXmlWriter; AData: Pointer);
    procedure ProcessStylesGroupFont(AWriter: TdxXmlWriter; AData: Pointer);
    procedure ProcessStylesGroupNumberFormat(AWriter: TdxXmlWriter; AData: Pointer);
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    class procedure WriteColor(AWriter: TdxXmlWriter; const ANodeName: string; AColor: TColor);
    class procedure WriteFont(AWriter: TdxXmlWriter; const ANodeName, AFontNameNodeName: string; AFont: TdxSpreadSheetFontHandle);

    procedure ExecuteCore(AWriter: TdxXmlWriter); override;
    function GetFormatID(AFormat: TdxSpreadSheetFormatHandle): Integer;

    property Borders: TdxSpreadSheetXLSXWriterResourceList read GetBorders;
    property CellStyleDefault: TdxSpreadSheetCellStyleHandle read GetCellStyleDefault;
    property CellStyles: TdxSpreadSheetXLSXWriterResourceList read GetCellStyles;
    property ConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList read GetConditionalFormattingStyles;
    property CustomFormats: TdxSpreadSheetXLSXWriterResourceList read GetCustomFormats;
    property Fills: TdxSpreadSheetXLSXWriterResourceList read GetFills;
    property Fonts: TdxSpreadSheetXLSXWriterResourceList read GetFonts;
    property Formats: TdxSpreadSheetXLSXWriterResourceList read FFormats;
  end;

  { TdxSpreadSheetXLSXWriterWorkbookBuilder }

  TdxSpreadSheetXLSXWriterWorkbookBuilder = class(TdxSpreadSheetXLSXWriterCustomFileBuilder)
  strict private
    FHasPrintableAreas: Boolean;
  public
    procedure AfterConstruction; override;
    procedure Execute; override;
    function HasPrintableAreas(AView: TdxSpreadSheetTableView): Boolean; virtual;
    procedure WriteCalcProperties(AWorkbook: TdxXMLNode); virtual;
    procedure WriteDefinedName(ANode: TdxXMLNode; ADefinedName: TdxSpreadSheetDefinedName); overload;
    procedure WriteDefinedName(ANode: TdxXMLNode; const ACaption, AReference: string; AView: TdxSpreadSheetCustomView); overload; virtual;
    procedure WriteDefinedNames(ANode: TdxXMLNode); virtual;
    procedure WriteExternalLink(const AFileName: AnsiString; ALink: TdxSpreadSheetExternalLink; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteExternalLinks(ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WritePrintableAreas(ANode: TdxXMLNode; AView: TdxSpreadSheetTableView); virtual;
    procedure WriteProperties(AWorkbook: TdxXMLNode); virtual;
    procedure WriteProtection(ANode: TdxXMLNode); virtual;
    procedure WriteSharedStrings(const AFileName: AnsiString; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteSheet(ANode: TdxXMLNode; AView: TdxSpreadSheetCustomView; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteSheets(ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteSheetView(const AFileName: AnsiString; AView: TdxSpreadSheetCustomView; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteStyles(const AFileName: AnsiString; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder }

  TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder = class(TdxSpreadSheetXLSXWriterCustomXMLBuilder)
  strict private
    FComments: TList<TdxSpreadSheetCommentContainer>;
    FContainers: TList<TdxSpreadSheetContainer>;
    FView: TdxSpreadSheetTableView;

    procedure ExtendDimensionsByGroups(AGroups: TdxSpreadSheetTableItemGroups; var AStartIndex, AFinishIndex: Integer);
  protected
    class function GetContentRelationship: AnsiString; override;
    class function GetContentType: AnsiString; override;
  protected
    procedure InitializeContainers;

    function ConvertRowHeight(const AValue: Integer): Double; virtual;
    function EncodeFloat(const AValue: Double): string;
    function GetActivePane(out AValue: string): Boolean;
    function ValidateDimensions(const R: TRect): TRect;

    procedure WriteBreaks(AWriter: TdxXMLWriter; const ANodeName: string; AList: TList<Cardinal>; AMaxIndex: Integer); virtual;
    procedure WriteCell(AWriter: TdxXmlWriter; ACell: TdxSpreadSheetCell; ARowIndex, AColumnIndex: Integer); virtual;
    procedure WriteColumns(AWriter: TdxXmlWriter; ADimension: TRect); virtual;
    procedure WriteComments(AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteConditionalFormatting(AWriter: TdxXmlWriter); virtual;
    procedure WriteContainers(AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteContent(AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteExtensions(AWriter: TdxXmlWriter); virtual;
    procedure WriteFixedPaneProperties(AWriter: TdxXmlWriter); virtual;
    procedure WriteHeaderFooter(AWriter: TdxXmlWriter; AHeaderFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooter); virtual;
    procedure WriteHeaderFooterText(AWriter: TdxXmlWriter; const ANodeName: string; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure WriteHyperlink(AWriter: TdxXmlWriter; AHyperlink: TdxSpreadSheetHyperLink; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteHyperlinks(AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteMergedCells(AWriter: TdxXmlWriter; AMergedCells: TdxSpreadSheetMergedCellList); virtual;
    procedure WritePageMargins(AWriter: TdxXmlWriter; AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins); virtual;
    procedure WritePageSetup(AWriter: TdxXmlWriter); virtual;
    procedure WritePrintOptions(AWriter: TdxXmlWriter); virtual;
    procedure WriteProperties(AWriter: TdxXmlWriter); virtual;
    procedure WriteRows(AWriter: TdxXmlWriter; ADimension: TRect); virtual;
    procedure WriteSelection(AWriter: TdxXmlWriter; ASelection: TdxSpreadSheetTableViewSelection); virtual;
    procedure WriteViewProperties(AWriter: TdxXmlWriter); virtual;
    procedure WriteViewProtection(AWriter: TdxXmlWriter); virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
      const ATargetFileName: AnsiString; AView: TdxSpreadSheetTableView);
    destructor Destroy; override;
    procedure ExecuteCore(AWriter: TdxXmlWriter); override;
    //
    property Comments: TList<TdxSpreadSheetCommentContainer> read FComments;
    property Containers: TList<TdxSpreadSheetContainer> read FContainers;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetTableViewSubFileBuilder }

  TdxSpreadSheetXLSXWriterWorksheetTableViewSubFileBuilder = class(TdxSpreadSheetXLSXWriterCustomFileBuilder)
  strict private
    FOwnerWriter: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder;
  public
    constructor Create(const ATargetFileName: AnsiString; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
      AOwnerWriter: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder);
    //
    property OwnerWriter: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder read FOwnerWriter;
  end;

  { TdxSpreadSheetXLTXWriter }

  TdxSpreadSheetXLTXWriter = class(TdxSpreadSheetXLSXWriter)
  protected
    function GetContentTypeID: AnsiString; override;
  end;

implementation

uses
  AnsiStrings, Math, TypInfo, StrUtils, dxColorPicker, cxGraphics, dxHashUtils, dxTypeHelpers,
  dxSpreadSheetFormulas, dxSpreadSheetFormatXLSX, dxSpreadSheetFormatUtils, dxSpreadSheetFormatXLSXWriterComments,
  dxSpreadSheetFormatXLSXWriterConditionalFormatting, dxSpreadSheetFormatXLSXWriterDrawing, dxOLECryptoContainer,
  dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreStrs, ZLib, dxStringHelper;

const
  CustomNumberFormatBase = 164;
  MaxZoomFactor = 400;
  MinZoomFactor = 10;

const
  sMsgWrongDataType = 'wrong data type';

type
  TdxDynamicListItemAccess = class(TdxDynamicListItem);
  TdxSpreadSheetTableColumnAccess = class(TdxSpreadSheetTableColumn);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

  { TdxXMLWriterHelper }

  TdxXMLWriterHelper = class helper for TdxXmlWriter
  public
    procedure WriteElementStringEx(const AName, AValue: string); overload;
  end;

{ TdxXMLWriterHelper }

procedure TdxXMLWriterHelper.WriteElementStringEx(const AName, AValue: string);
begin
  WriteStartElement(AName);
  if AValue <> '' then
  begin
    if TdxXMLHelper.IsPreserveSpacesNeeded(AValue) then
      WriteAttributeString('xml', 'space', '', 'preserve');
    WriteString(AValue);
  end;
  WriteEndElement;
end;

{ TdxSpreadSheetXLSXWriterResourceList }

constructor TdxSpreadSheetXLSXWriterResourceList.Create;
begin
  inherited Create;
  FData := TdxFastList.Create(DefaultCapacity);
  FIndex := TDictionary<Pointer, Integer>.Create(DefaultCapacity);
end;

destructor TdxSpreadSheetXLSXWriterResourceList.Destroy;
begin
  FreeAndNil(FIndex);
  FreeAndNil(FData);
  inherited;
end;

function TdxSpreadSheetXLSXWriterResourceList.Add(AObject: TObject): Integer;
begin
  if not FIndex.TryGetValue(AObject, Result) then
  begin
    Result := FData.Add(AObject);
    FIndex.Add(AObject, Result);
  end;
end;

function TdxSpreadSheetXLSXWriterResourceList.IndexOf(AObject: TObject): Integer;
begin
  Result := FIndex.Items[AObject];
end;

function TdxSpreadSheetXLSXWriterResourceList.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TdxSpreadSheetXLSXWriterResourceList.GetList: PdxPointerList;
begin
  Result := FData.List;
end;

{ TdxSpreadSheetXLSXWriterRels }

constructor TdxSpreadSheetXLSXWriterRels.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  Root.AddChild(sdxXLSXNodeRelationships).Attributes.Add(sdxXLSXAttrXMLNS, sdxXLSXRelsNameSpace);
end;

procedure TdxSpreadSheetXLSXWriterRels.AddExternalRelationship(const AType, ATarget: TdxXMLString);
begin
  DoAddRelationship(AType, ATarget).Attributes.Add(sdxXLSXAttrTargetMode, sdxXLSXValueTargetModeExternal);
end;

procedure TdxSpreadSheetXLSXWriterRels.AddRelationship(const AType, ATarget: TdxXMLString);
begin
  DoAddRelationship(AType, ATarget);
end;

function TdxSpreadSheetXLSXWriterRels.GetRelationshipId(const ATarget: TdxXMLString): TdxXMLString;
var
  ANode: TdxXMLNode;
begin
  Result := '';
  ANode := Root.First.First;
  while ANode <> nil do
  begin
    if ANode.Attributes.GetValue(sdxXLSXAttrTarget) = ATarget then
    begin
      Result := ANode.Attributes.GetValue(sdxXLSXAttrId);
      Break;
    end;
    ANode := ANode.Next;
  end;
end;

procedure TdxSpreadSheetXLSXWriterRels.ConvertToRelativePaths(const ARootPath: TdxXMLString);
var
  ANode: TdxXMLNode;
begin
  ANode := Root.First.First;
  while ANode <> nil do
  begin
    if ANode.Attributes.GetValue(sdxXLSXAttrTargetMode) <> sdxXLSXValueTargetModeExternal then
    begin
      ANode.Attributes.SetValue(sdxXLSXAttrTarget, TdxZIPPathHelper.RelativePath(ARootPath,
        TdxZIPPathHelper.ExcludeRootPathDelimiter(ANode.Attributes.GetValue(sdxXLSXAttrTarget))));
    end;
    ANode := ANode.Next;
  end;
end;

function TdxSpreadSheetXLSXWriterRels.DoAddRelationship(const AType, ATarget: TdxXMLString): TdxXMLNode;
begin
  Result := Root.First.AddChild(sdxXLSXNodeRelationship);
  Result.Attributes.Add(sdxXLSXAttrId, 'rId' + dxStringToAnsiString(IntToStr(Root.First.Count)));
  Result.Attributes.Add(sdxXLSXAttrType, AType);
  Result.Attributes.Add(sdxXLSXAttrTarget, ATarget);
end;

function TdxSpreadSheetXLSXWriterRels.GetEmpty: Boolean;
begin
  Result := Root.First.Count = 0;
end;

{ TdxSpreadSheetXLSXCustomWriter }

constructor TdxSpreadSheetXLSXCustomWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  if AOwner.Password <> '' then
  begin
    FTargetStream := AStream;
    AStream := TMemoryStream.Create;
  end;
  inherited Create(AOwner, AStream);
end;

destructor TdxSpreadSheetXLSXCustomWriter.Destroy;
begin
  inherited Destroy;
  if FTargetStream <> nil then
  try
    TdxOLECryptoContainer.Encrypt(Stream, FTargetStream, SpreadSheet.Password, TdxOLECryptoContainerEncryptorStandard);
  finally
    Stream.Free;
  end;
end;

{ TdxSpreadSheetXLSXWriter }

constructor TdxSpreadSheetXLSXWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FImages := TDictionary<TdxGPImage, AnsiString>.Create;
  FContentType := TdxSpreadSheetXMLDocument.Create(nil);
  InitializeContentTypes(FContentType.AddChild('Types'));

  FConditionalFormattingStyles := TdxSpreadSheetXLSXWriterResourceList.Create;
  FCustomFormats := TdxSpreadSheetXLSXWriterResourceList.Create;
  FCellStyles := TdxSpreadSheetXLSXWriterResourceList.Create;
  FFonts := TdxSpreadSheetXLSXWriterResourceList.Create;
  FSharedStrings := TdxSpreadSheetXLSXWriterResourceList.Create;
  FFills := TdxSpreadSheetXLSXWriterResourceList.Create;
  FBorders := TdxSpreadSheetXLSXWriterResourceList.Create;

  FCellStyleDefault := SpreadSheet.CellStyles.AddStyle(SpreadSheet.CellStyles.CreateStyle);
  FCellStyleDefault.AddRef;

  AddDefaultResources;

  FColumnWidthHelper := TdxSpreadSheetExcelColumnWidthHelper.Create;
  FCellStyleDefault.Font.AssignToFont(FColumnWidthHelper.Font);
end;

destructor TdxSpreadSheetXLSXWriter.Destroy;
begin
  FCellStyleDefault.Release;
  FCellStyleDefault := nil;

  FreeAndNil(FImages);
  FreeAndNil(FFonts);
  FreeAndNil(FBorders);
  FreeAndNil(FFills);
  FreeAndNil(FCellStyles);
  FreeAndNil(FContentType);
  FreeAndNil(FSharedStrings);
  FreeAndNil(FColumnWidthHelper);
  FreeAndNil(FConditionalFormattingStyles);
  FreeAndNil(FCustomFormats);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXWriter.WriteData;
var
  ARels: TdxSpreadSheetXLSXWriterRels;
begin
  ARels := TdxSpreadSheetXLSXWriterRels.Create(nil);
  try
    ExecuteSubTask(TdxSpreadSheetXLSXWriterWorkbookBuilder.Create(Self, ARels, sdxXLSXWorkbookFileName));
    RegisterFile(TdxSpreadSheetXLSXUtils.GetRelsFileNameForFile(''), sdxXLSXRelsContentType);
    WriteRels(TdxSpreadSheetXLSXUtils.GetRelsFileNameForFile(''), ARels);
    WriteXML(sdxXLSXContentTypeFileName, FContentType);
  finally
    ARels.Free;
  end;
end;

function TdxSpreadSheetXLSXWriter.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, SpreadSheet.SheetCount + 2);
end;

function TdxSpreadSheetXLSXWriter.GetContentTypeID: AnsiString;
begin
  Result := sdxXLSXWorkbookContentType;
end;

procedure TdxSpreadSheetXLSXWriter.AddDefaultResources;
var
  ABrush: TdxSpreadSheetBrushHandle;
begin
  Fills.Add(SpreadSheet.CellStyles.Brushes.AddBrush(SpreadSheet.CellStyles.Brushes.CreateBrush));

  ABrush := SpreadSheet.CellStyles.Brushes.CreateBrush;
  ABrush.Style := sscfsGray12;
  Fills.Add(SpreadSheet.CellStyles.Brushes.AddBrush(ABrush));

  CellStyles.Add(FCellStyleDefault);
  CellStyles.Add(SpreadSheet.DefaultCellStyle.Handle);
end;

procedure TdxSpreadSheetXLSXWriter.InitializeContentTypes(ANode: TdxXMLNode);

  procedure AddDefaults(ANode: TdxXMLNode; const AExtension, AContentType: AnsiString);
  begin
    ANode := ANode.AddChild(sdxXLSXNodeDefault);
    ANode.Attributes.Add(sdxXLSXAttrExtension, AExtension);
    ANode.Attributes.Add(sdxXLSXAttrContentType, AContentType);
  end;

begin
  ANode.Attributes.Add(sdxXLSXAttrXMLNS, sdxXLSXContentTypeNameSpace);
  AddDefaults(ANode, sdxXLSXMimeTypePNGExt, sdxXLSXMimeTypePNG);
  AddDefaults(ANode, sdxXLSXMimeTypeJPGExt, sdxXLSXMimeTypeJPG);
  AddDefaults(ANode, sdxXLSXMimeTypeRELSExt, sdxXLSXMimeTypeRELS);
  AddDefaults(ANode, sdxXLSXMimeTypeXMLExt, sdxXLSXMimeTypeXML);
  AddDefaults(ANode, sdxXLSXMimeTypeVMLExt, sdxXLSXMimeTypeVML);
end;

procedure TdxSpreadSheetXLSXWriter.RegisterFile(const AFileName, AContentType: AnsiString;
  const ARelationship: AnsiString = ''; ARels: TdxSpreadSheetXLSXWriterRels = nil);
var
  ANode: TdxXMLNode;
begin
  if ARels <> nil then
    ARels.AddRelationship(ARelationship, dxUnixPathDelim + AFileName);

  if AContentType <> '' then
  begin
    ANode := FContentType.Root.First.AddChild(sdxXLSXNodeOverride);
    ANode.Attributes.Add(sdxXLSXAttrPartName, dxUnixPathDelim + AFileName);
    ANode.Attributes.Add(sdxXLSXAttrContentType, AContentType);
  end;
end;

procedure TdxSpreadSheetXLSXWriter.WriteRels(const AFileName: AnsiString; ADocument: TdxSpreadSheetXLSXWriterRels);
var
  APath: AnsiString;
begin
  APath := TdxZIPPathHelper.ExtractFilePath(AFileName);
  APath := TdxZIPPathHelper.ExcludeTrailingPathDelimiter(APath);
  APath := TdxZIPPathHelper.ExtractFilePath(APath);
  ADocument.ConvertToRelativePaths(APath);
  WriteXML(AFileName, ADocument);
end;

{ TdxSpreadSheetXLSXWriterCustomBuilder }

constructor TdxSpreadSheetXLSXWriterCustomBuilder.Create(
  AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels);
begin
  inherited Create(AOwner);
  FOwnerRels := AOwnerRels;
end;

function TdxSpreadSheetXLSXWriterCustomBuilder.WriteImage(AImage: TdxGPImage): AnsiString;
const
  CodecMap: array[Boolean] of TdxImageDataFormat = (dxImagePng, dxImageJpeg);
  ExtMap: array[Boolean] of string = (sdxXLSXMimeTypePNGExt, sdxXLSXMimeTypeJPGExt);
var
  AFileName: AnsiString;
  AMemStream: TMemoryStream;
  ASaveAsJPEG: Boolean;
begin
  if not Owner.Images.TryGetValue(AImage, AFileName) then
  begin
    AMemStream := TMemoryStream.Create;
    try
      ASaveAsJPEG := AImage.ImageDataFormat = dxImageJpeg;
      AImage.SaveToStreamByCodec(AMemStream, CodecMap[ASaveAsJPEG]);

      AFileName := Format(sdxXLSXFileTemplateImage, [Owner.Images.Count + 1, ExtMap[ASaveAsJPEG]]);
      WriteFile(AFileName, AMemStream);
      Owner.Images.Add(AImage, AFileName);
    finally
      AMemStream.Free;
    end;
  end;
  OwnerRels.AddRelationship(sdxXLSXImageRelationship, dxUnixPathDelim + AFileName);
  Result := OwnerRels.GetRelationshipId(dxUnixPathDelim + AFileName);
end;

procedure TdxSpreadSheetXLSXWriterCustomBuilder.RegisterFile(const AFileName, AContentType: AnsiString;
  const ARelationship: AnsiString = ''; ARels: TdxSpreadSheetXLSXWriterRels = nil);
begin
  Owner.RegisterFile(AFileName, AContentType, ARelationship, ARels);
end;

function TdxSpreadSheetXLSXWriterCustomBuilder.GetOwner: TdxSpreadSheetXLSXWriter;
begin
  Result := TdxSpreadSheetXLSXWriter(inherited Owner);
end;

{ TdxSpreadSheetXLSXWriterCustomFileBuilder }

constructor TdxSpreadSheetXLSXWriterCustomFileBuilder.Create(AOwner: TdxSpreadSheetXLSXWriter;
  AOwnerRels: TdxSpreadSheetXLSXWriterRels; const ATargetFileName: AnsiString);
begin
  inherited Create(AOwner, AOwnerRels);
  FTargetFileName := ATargetFileName;
end;

function TdxSpreadSheetXLSXWriterCustomFileBuilder.GetTargetFileNameRels: AnsiString;
begin
  Result := TdxSpreadSheetXLSXUtils.GetRelsFileNameForFile(TargetFileName);
end;

{ TdxSpreadSheetXLSXWriterCustomXMLBuilder }

procedure TdxSpreadSheetXLSXWriterCustomXMLBuilder.Execute;
var
  AStream: TStream;
  AWriter: TdxXmlWriter;
  AWriterSettings: TdxXmlWriterSettings;
begin
  AStream := TdxCompressedStream.Create(clDefault, BufferCapacity);
  try
    AWriterSettings := TdxXmlWriterSettings.Create;
    AWriterSettings.CheckCharacters := False;
    AWriterSettings.EncodeInvalidXmlCharAsUCS2 := True;

    AWriter := TdxXmlWriter.Create(AStream, AWriterSettings);
    try
      ExecuteCore(AWriter);
      AWriter.Flush;
    finally
      AWriter.Free;
    end;
    RegisterFile(TargetFileName, GetContentType, GetContentRelationship, OwnerRels);
  finally
    WriteFile(TargetFileName, AStream, True);
  end;
end;

{ TdxSpreadSheetXLSXWriterExternalLinkBuilder }

constructor TdxSpreadSheetXLSXWriterExternalLinkBuilder.Create(AOwner: TdxSpreadSheetXLSXWriter;
  AOwnerRels: TdxSpreadSheetXLSXWriterRels; ALink: TdxSpreadSheetExternalLink; const ATargetFileName: AnsiString);
begin
  inherited Create(AOwner, AOwnerRels, ATargetFileName);
  FLink := ALink;
end;

procedure TdxSpreadSheetXLSXWriterExternalLinkBuilder.Execute;
var
  ADoc: TdxXMLDocument;
  ADocRels: TdxSpreadSheetXLSXWriterRels;
  ANode: TdxXMLNode;
  ATarget: TdxXMLString;
begin
  ADoc := TdxXMLDocument.Create(nil);
  try
    ATarget := EncodePath(Link.Target);

    ANode := ADoc.Root.AddChild(sdxXLSXNodeExternalLink);
    ANode.Attributes.SetValue(sdxXLSXAttrXMLNS, sdxXLSXWorkbookNameSpace);

    ADocRels := TdxSpreadSheetXLSXWriterRels.Create(nil);
    try
      ADocRels.AddExternalRelationship(sdxXLSXExternalLinkPathRelationship, ATarget);

      ANode := ANode.AddChild(sdxXLSXNodeExternalBook);
      ANode.Attributes.SetValue(sdxXLSXAttrXMLNSR, sdxXLSXCommonRelationshipPath);
      ANode.Attributes.SetValue(sdxXLSXAttrRId, ADocRels.GetRelationshipId(ATarget));

      Owner.WriteRels(TargetFileNameRels, ADocRels);
    finally
      ADocRels.Free;
    end;

    RegisterFile(TargetFileName, sdxXLSXExternalLinkContentType, sdxXLSXExternalLinkRelationship, OwnerRels);
    WriteXML(TargetFileName, ADoc);
  finally
    ADoc.Free;
  end;
end;

function TdxSpreadSheetXLSXWriterExternalLinkBuilder.EncodePath(const APath: string): TdxXMLString;
begin
  Result := dxStringToXMLString(APath);
end;

{ TdxSpreadSheetXLSXWriterFormattedStringBuilder }

constructor TdxSpreadSheetXLSXWriterFormattedStringBuilder.Create(
  AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
  AWriter: TdxXmlWriter; AString: TdxSpreadSheetFormattedSharedString);
begin
  inherited Create(AOwner, AOwnerRels);
  FWriter := AWriter;
  FString := AString;
end;

procedure TdxSpreadSheetXLSXWriterFormattedStringBuilder.Execute;

  function GetRunLength(AStartIndex, ARunIndex: Integer): Integer;
  begin
    if ARunIndex + 1 < FString.Runs.Count then
      Result := FString.Runs[ARunIndex + 1].StartIndex - AStartIndex
    else
      Result := Length(FString.Value) - AStartIndex + 1;
  end;

var
  ALength: Integer;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
  I: Integer;
begin
  ALength := GetRunLength(1, -1);
  if ALength > 0 then
  begin
    FWriter.WriteStartElement(sdxXLSXNodeRichTextRun);
    FWriter.WriteElementStringEx(sdxXLSXNodeText, Copy(FString.Value, 1, ALength));
    FWriter.WriteEndElement;
  end;

  for I := 0 to FString.Runs.Count - 1 do
  begin
    ARun := FString.Runs[I];
    ALength := GetRunLength(ARun.StartIndex, I);

    FWriter.WriteStartElement(sdxXLSXNodeRichTextRun);
    TdxSpreadSheetXLSXWriterStylesBuilder.WriteFont(FWriter, sdxXLSXNodeRichTextRunParagraph, sdxXLSXNodeFontName, ARun.FontHandle);
    FWriter.WriteElementStringEx(sdxXLSXNodeText, Copy(FString.Value, ARun.StartIndex, ALength));
    FWriter.WriteEndElement;
  end;
end;

{ TdxSpreadSheetXLSXWriterSharedStringsBuilder }

procedure TdxSpreadSheetXLSXWriterSharedStringsBuilder.ExecuteCore(AWriter: TdxXmlWriter);
var
  AString: TdxSpreadSheetSharedString;
  I: Integer;
begin
  AWriter.WriteStartElement(sdxXLSXNodeSST, sdxXLSXWorkbookNameSpace);
  AWriter.WriteAttributeInteger(sdxXLSXAttrCount, SharedStrings.Count);
  AWriter.WriteAttributeInteger(sdxXLSXAttrUniqueCount, SharedStrings.Count);

  Owner.ProgressHelper.BeginStage(SharedStrings.Count);
  try
    for I := 0 to SharedStrings.Count - 1 do
    begin
      AString := TdxSpreadSheetSharedString(SharedStrings.List[I]);
      AWriter.WriteStartElement(sdxXLSXAttrSharedIndex);
      if (AString.ClassType <> TdxSpreadSheetSharedString) and (AString is TdxSpreadSheetFormattedSharedString) then
        ExecuteSubTask(TdxSpreadSheetXLSXWriterFormattedStringBuilder.Create(Owner, OwnerRels, AWriter, TdxSpreadSheetFormattedSharedString(AString)))
      else
        AWriter.WriteElementStringEx(sdxXLSXNodeText, AString.Value);
      AWriter.WriteEndElement;
      Owner.ProgressHelper.NextTask;
    end;
  finally
    Owner.ProgressHelper.EndStage;
  end;
  AWriter.WriteEndElement;
end;

class function TdxSpreadSheetXLSXWriterSharedStringsBuilder.GetContentRelationship: AnsiString;
begin
  Result := sdxXLSXSharedStringRelationship;
end;

class function TdxSpreadSheetXLSXWriterSharedStringsBuilder.GetContentType: AnsiString;
begin
  Result := sdxXLSXSharedStringsContentType;
end;

function TdxSpreadSheetXLSXWriterSharedStringsBuilder.GetSharedStrings: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.SharedStrings;
end;

{ TdxSpreadSheetXLSXWriterStylesBuilder }

procedure TdxSpreadSheetXLSXWriterStylesBuilder.AfterConstruction;
begin
  inherited;
  FFormats := TdxSpreadSheetXLSXWriterResourceList.Create;
end;

destructor TdxSpreadSheetXLSXWriterStylesBuilder.Destroy;
begin
  FreeAndNil(FFormats);
  inherited;
end;

class procedure TdxSpreadSheetXLSXWriterStylesBuilder.WriteColor(
  AWriter: TdxXmlWriter; const ANodeName: string; AColor: TColor);
begin
  if cxColorIsValid(AColor) then
  begin
    AWriter.WriteStartElement(ANodeName);
    AWriter.WriteAttributeString(sdxXLSXAttrRGB, TdxColorHelper.AlphaColorToHexCode(dxColorToAlphaColor(AColor), False));
    AWriter.WriteEndElement;
  end;
end;

class procedure TdxSpreadSheetXLSXWriterStylesBuilder.WriteFont(
  AWriter: TdxXmlWriter; const ANodeName, AFontNameNodeName: string; AFont: TdxSpreadSheetFontHandle);
const
  ScriptMap: array[TdxSpreadSheetFontScript] of AnsiString = (
    '', sdxXLSXValueSuperscript, sdxXLSXValueSubscript
  );
var
  AStyle: TFontStyle;
begin
  AWriter.WriteStartElement(ANodeName);
  try
    WriteColor(AWriter, sdxXLSXNodeColor, AFont.Color);

    if AFont.Script <> fsNone then
    begin
      AWriter.WriteStartElement(sdxXLSXNodeVertAlign);
      AWriter.WriteAttributeString(sdxXLSXAttrVal, ScriptMap[AFont.Script]);
      AWriter.WriteEndElement;
    end;

    AWriter.WriteStartElement(sdxXLSXNodeSZ);
    AWriter.WriteAttributeInteger(sdxXLSXAttrVal, AFont.Size);
    AWriter.WriteEndElement;

    AWriter.WriteStartElement(AFontNameNodeName);
    AWriter.WriteAttributeString(sdxXLSXAttrVal, AFont.Name);
    AWriter.WriteEndElement;

    AWriter.WriteStartElement(sdxXLSXNodeCharset);
    AWriter.WriteAttributeInteger(sdxXLSXAttrVal, AFont.Charset);
    AWriter.WriteEndElement;

    for AStyle := Low(AStyle) to High(AStyle) do
    begin
      if AStyle in AFont.Style then
        AWriter.WriteElementString(dxAnsiStringToString(dxXLSXFontStyles[AStyle]), '');
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ExecuteCore(AWriter: TdxXmlWriter);
begin
  PrepareResources;

  AWriter.WriteStartElement(sdxXLSXNodeStyleSheet, sdxXLSXWorkbookNameSpace);
  try
    Owner.ProgressHelper.BeginStage(CellStyles.Count + Borders.Count + Formats.Count + Fonts.Count + ConditionalFormattingStyles.Count);
    try
      ProcessStylesGroup(AWriter, sdxXLSXNodeStyleNumberFormats, Formats, ProcessStylesGroupNumberFormat);
      ProcessStylesGroup(AWriter, sdxXLSXNodeStyleFonts, Fonts, ProcessStylesGroupFont);
      ProcessStylesGroup(AWriter, sdxXLSXNodeStyleFills, Fills, ProcessStylesGroupFill);
      ProcessStylesGroup(AWriter, sdxXLSXNodeStyleBorders, Borders, ProcessStylesGroupBorders);

      AWriter.WriteStartElement(sdxXLSXNodeStyleCellStyleXfs);
      AWriter.WriteAttributeInteger(sdxXLSXAttrCount, 1);
      AWriter.WriteStartElement(sdxXLSXNodeStyleCellXf);
      AWriter.WriteAttributeInteger(sdxXLSXAttrBorderId, 0);
      AWriter.WriteAttributeInteger(sdxXLSXAttrFillId, 0);
      AWriter.WriteAttributeInteger(sdxXLSXAttrFontId, 0);
      AWriter.WriteAttributeInteger(sdxXLSXAttrNumFmtId, 0);
      AWriter.WriteEndElement;
      AWriter.WriteEndElement;

      ProcessStylesGroup(AWriter, sdxXLSXNodeStyleCellXfs, CellStyles, ProcessStylesGroupCellStyle);
      ProcessStylesGroup(AWriter, sdxXLSXNodeDXFS, ConditionalFormattingStyles, ProcessStylesGroupConditionalFormatting);
    finally
      Owner.ProgressHelper.EndStage;
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetFormatID(AFormat: TdxSpreadSheetFormatHandle): Integer;
begin
  if SpreadSheet.CellStyles.Formats.IsCustom(AFormat) then
  begin
    Result := CustomFormats.Add(AFormat);
    Inc(Result, CustomNumberFormatBase);
  end
  else
    Result := AFormat.FormatCodeID;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroup(
  AWriter: TdxXmlWriter; const ANodeName: string; AStyles: TdxSpreadSheetXLSXWriterResourceList; AProc: TdxXLSXStyleForEachProc);
var
  I: Integer;
begin
  AWriter.WriteStartElement(ANodeName);
  try
    AWriter.WriteAttributeInteger(sdxXLSXAttrCount, AStyles.Count);
    for I := 0 to AStyles.Count - 1 do
    begin
      AProc(AWriter, AStyles.List[I]);
      Owner.ProgressHelper.NextTask;
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroupBorders(AWriter: TdxXmlWriter; AData: Pointer);
const
  BordersOrder: array [0..3] of TcxBorder = (bLeft, bRight, bTop, bBottom);
var
  ABorder: TcxBorder;
  ABordersHandle: TdxSpreadSheetBordersHandle;
  ABorderStyle: AnsiString;
  I: Integer;
begin
  ABordersHandle := TdxSpreadSheetBordersHandle(AData);
  AWriter.WriteStartElement(sdxXLSXNodeStyleBorder);
  try
    for I := 0 to Length(BordersOrder) - 1 do
    begin
      ABorder := BordersOrder[I];
      ABorderStyle := TdxSpreadSheetXLSXHelper.BorderStyleToString(ABordersHandle.BorderStyle[ABorder]);

      AWriter.WriteStartElement(dxAnsiStringToString(dxXLSXBorderNames[ABorder]));
      if ABorderStyle <> '' then
        AWriter.WriteAttributeString(sdxXLSXAttrStyle, ABorderStyle);
      WriteColor(AWriter, sdxXLSXNodeColor, ABordersHandle.BorderColor[ABorder]);
      AWriter.WriteEndElement;
    end;
    AWriter.WriteElementString(sdxXLSXNodeDiagonal, '');
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroupCellStyle(AWriter: TdxXmlWriter; AData: Pointer);
var
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
begin
  AWriter.WriteStartElement(sdxXLSXNodeStyleCellXf);
  try
    AStyleHandle := TdxSpreadSheetCellStyleHandle(AData);

    AWriter.WriteAttributeBoolean(sdxXLSXAttrApplyAlignment, True);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrApplyBorder, True);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrApplyFill, True);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrApplyNumberFormat, True);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrApplyFont, True);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrApplyProtection, True);

    AWriter.WriteAttributeInteger(sdxXLSXAttrBorderId, Borders.IndexOf(AStyleHandle.Borders));
    AWriter.WriteAttributeInteger(sdxXLSXAttrFillId, Fills.IndexOf(AStyleHandle.Brush));
    AWriter.WriteAttributeInteger(sdxXLSXAttrFontId, Fonts.IndexOf(AStyleHandle.Font));
    AWriter.WriteAttributeInteger(sdxXLSXAttrNumFmtId, GetFormatID(AStyleHandle.DataFormat));
    AWriter.WriteAttributeInteger(sdxXLSXAttrXFId, 0);

    AWriter.WriteStartElement(sdxXLSXNodeAlignment);
    try
      AWriter.WriteAttributeString(sdxXLSXAttrHorizontal, TdxSpreadSheetXLSXHelper.AlignHorzToString(AStyleHandle.AlignHorz));
      AWriter.WriteAttributeString(sdxXLSXAttrVertical, TdxSpreadSheetXLSXHelper.AlignVertToString(AStyleHandle.AlignVert));
      if AStyleHandle.AlignHorzIndent <> 0 then
        AWriter.WriteAttributeInteger(sdxXLSXAttrIndent, Owner.ColumnWidthHelper.PixelsToSpacesNumber(AStyleHandle.AlignHorzIndent));
      AWriter.WriteAttributeInteger(sdxXLSXAttrTextRotation, AStyleHandle.Rotation);
      AWriter.WriteAttributeBoolean(sdxXLSXAttrShrinkToFit, csShrinkToFit in AStyleHandle.States);
      AWriter.WriteAttributeBoolean(sdxXLSXAttrWrapText, csWordWrap in AStyleHandle.States);
    finally
      AWriter.WriteEndElement;
    end;

    AWriter.WriteStartElement(sdxXLSXNodeProtection);
    try
      AWriter.WriteAttributeBoolean(sdxXLSXAttrHidden, csHidden in AStyleHandle.States);
      AWriter.WriteAttributeBoolean(sdxXLSXAttrLocked, csLocked in AStyleHandle.States);
    finally
      AWriter.WriteEndElement;
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroupConditionalFormatting(AWriter: TdxXmlWriter; AData: Pointer);
var
  AStyle: TdxSpreadSheetCellStyleHandle;
begin
  AStyle := TdxSpreadSheetCellStyleHandle(AData);

  AWriter.WriteStartElement(sdxXLSXNodeDXF);
  try
    if AStyle.Font <> CellStyleDefault.Font then
      ProcessStylesGroupFont(AWriter, AStyle.Font);
    if AStyle.DataFormat <> CellStyleDefault.DataFormat then
      ProcessStylesGroupNumberFormat(AWriter, AStyle.DataFormat);
    if AStyle.Brush <> CellStyleDefault.Brush then
      ProcessStylesGroupFill(AWriter, AStyle.Brush);
    if AStyle.Borders <> CellStyleDefault.Borders then
      ProcessStylesGroupBorders(AWriter, AStyle.Borders);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroupFill(AWriter: TdxXmlWriter; AData: Pointer);
var
  ABrushHandle: TdxSpreadSheetBrushHandle;
begin
  ABrushHandle := TdxSpreadSheetBrushHandle(AData);

  AWriter.WriteStartElement(sdxXLSXNodeStyleFill);
  try
    AWriter.WriteStartElement(sdxXLSXNodeCellStylePatternFill);
    AWriter.WriteAttributeString(sdxXLSXAttrPatternType, TdxSpreadSheetXLSXHelper.FillStyleToString(ABrushHandle));

    if ABrushHandle.Style = sscfsSolid then
    begin
      WriteColor(AWriter, sdxXLSXNodeForegroundColor, ABrushHandle.BackgroundColor);
      WriteColor(AWriter, sdxXLSXNodeBackgroundColor, ABrushHandle.BackgroundColor);
    end
    else
    begin
      WriteColor(AWriter, sdxXLSXNodeForegroundColor, ABrushHandle.ForegroundColor);
      WriteColor(AWriter, sdxXLSXNodeBackgroundColor, ABrushHandle.BackgroundColor);
    end;

    AWriter.WriteEndElement;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroupFont(AWriter: TdxXmlWriter; AData: Pointer);
begin
  WriteFont(AWriter, sdxXLSXNodeStyleFont, sdxXLSXNodeName, TdxSpreadSheetFontHandle(AData));
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.ProcessStylesGroupNumberFormat(AWriter: TdxXmlWriter; AData: Pointer);
var
  AFormatID: Integer;
begin
  AFormatID := GetFormatID(TdxSpreadSheetFormatHandle(AData));
  if AFormatID >= CustomNumberFormatBase then
  begin
    AWriter.WriteStartElement(sdxXLSXNodeStyleNumberFormat);
    try
      AWriter.WriteAttributeString(sdxXLSXAttrFormatCode, TdxSpreadSheetFormatHandle(AData).FormatCode);
      AWriter.WriteAttributeInteger(sdxXLSXAttrNumFmtId, AFormatID);
    finally
      AWriter.WriteEndElement;
    end;
  end;
end;

class function TdxSpreadSheetXLSXWriterStylesBuilder.GetContentRelationship: AnsiString;
begin
  Result := sdxXLSXStyleRelationship;
end;

class function TdxSpreadSheetXLSXWriterStylesBuilder.GetContentType: AnsiString;
begin
  Result := sdxXLSXStylesContentType;
end;

procedure TdxSpreadSheetXLSXWriterStylesBuilder.PrepareResources;
var
  AStyle: TdxSpreadSheetCellStyleHandle;
  I: Integer;
begin
  for I := 0 to CellStyles.Count - 1 do
  begin
    AStyle := TdxSpreadSheetCellStyleHandle(CellStyles.List[I]);
    Borders.Add(AStyle.Borders);
    Fills.Add(AStyle.Brush);
    Fonts.Add(AStyle.Font);
    if GetFormatID(AStyle.DataFormat) >= CustomNumberFormatBase then
      Formats.Add(AStyle.DataFormat);
  end;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetBorders: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.Borders;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetCellStyleDefault: TdxSpreadSheetCellStyleHandle;
begin
  Result := Owner.CellStyleDefault;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetCellStyles: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.CellStyles;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.ConditionalFormattingStyles;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetCustomFormats: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.CustomFormats;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetFills: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.Fills;
end;

function TdxSpreadSheetXLSXWriterStylesBuilder.GetFonts: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.Fonts;
end;

{ TdxSpreadSheetXLSXWriterWorkbookBuilder }

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.AfterConstruction;
begin
  inherited AfterConstruction;
  FHasPrintableAreas := False;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.Execute;
var
  ANode: TdxXMLNode;
  AWorkbook: TdxXMLDocument;
  AWorkbookRels: TdxSpreadSheetXLSXWriterRels;
begin
  AWorkbook := TdxSpreadSheetXMLDocument.Create(nil);
  try
    AWorkbookRels := TdxSpreadSheetXLSXWriterRels.Create(nil);
    try
      ANode := AWorkbook.Root.AddChild(sdxXLSXNodeWorkbook);
      ANode.Attributes.Add(sdxXLSXAttrXMLNS, sdxXLSXWorkbookNameSpace);
      ANode.Attributes.Add(sdxXLSXAttrXMLNSR, sdxXLSXCommonRelationshipPath);

      WriteProperties(ANode);
      WriteSheets(ANode.AddChild(sdxXLSXNodeSheets), AWorkbookRels);
      if SpreadSheet.ExternalLinks.Count > 0 then
        WriteExternalLinks(ANode.AddChild(sdxXLSXNodeExternalReferences), AWorkbookRels);
      if (SpreadSheet.DefinedNames.Count > 0) or FHasPrintableAreas then
        WriteDefinedNames(ANode.AddChild(sdxXLSXNodeDefinedNames));
      WriteCalcProperties(ANode);

      WriteSharedStrings(sdxXLSXSharedStringsFileName, AWorkbookRels);
      WriteStyles(sdxXLSXStylesFileName, AWorkbookRels);

      Owner.WriteRels(TargetFileNameRels, AWorkbookRels);
    finally
      AWorkbookRels.Free;
    end;

    RegisterFile(TargetFileName, Owner.GetContentTypeID, sdxXLSXWorkbookRelationship, OwnerRels);
    WriteXML(TargetFileName, AWorkbook);
  finally
    AWorkbook.Free;
  end;
end;

function TdxSpreadSheetXLSXWriterWorkbookBuilder.HasPrintableAreas(AView: TdxSpreadSheetTableView): Boolean;
begin
  Result :=
    AView.OptionsPrint.Source.Area.Assigned or
    AView.OptionsPrint.Source.RowsToRepeat.Assigned or
    AView.OptionsPrint.Source.ColumnsToRepeat.Assigned;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteCalcProperties(AWorkbook: TdxXMLNode);
const
  Map: array[Boolean] of AnsiString = (sdxXLSXValueA1, sdxXLSXValueR1C1);
var
  ANode: TdxXMLNode;
begin
  ANode := AWorkbook.AddChild(sdxXLSXNodeCalcPr);
  ANode.Attributes.Add(sdxXLSXAttrRefMode, Map[SpreadSheet.OptionsView.R1C1Reference]);
  if SpreadSheet.OptionsBehavior.IterativeCalculation then
  begin
    ANode.Attributes.Add(sdxXLSXAttrIterate, True);
    if SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount <> 100 then
      ANode.Attributes.Add(sdxXLSXAttrIterateCount, SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount);
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteDefinedName(ANode: TdxXMLNode; ADefinedName: TdxSpreadSheetDefinedName);
begin
  WriteDefinedName(ANode, ADefinedName.Caption, ADefinedName.Reference, ADefinedName.Scope);
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteDefinedName(
  ANode: TdxXMLNode; const ACaption, AReference: string; AView: TdxSpreadSheetCustomView);
begin
  ANode.Attributes.SetValueAsString(sdxXLSXAttrName, ACaption);
  if AView <> nil then
    ANode.Attributes.SetValueAsInteger(sdxXLSXAttrLocalSheetId, AView.Index);
  ANode.TextAsString := dxSpreadSheetFormulaExcludeEqualSymbol(AReference);
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteDefinedNames(ANode: TdxXMLNode);
var
  AView: TdxSpreadSheetCustomView;
  I: Integer;
begin
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    WriteDefinedName(ANode.AddChild(sdxXLSXNodeDefinedName), SpreadSheet.DefinedNames[I]);
  for I := 0 to SpreadSheet.SheetCount - 1 do
  begin
    AView := SpreadSheet.Sheets[I];
    if AView is TdxSpreadSheetTableView then
      WritePrintableAreas(ANode, TdxSpreadSheetTableView(AView));
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteExternalLink(
  const AFileName: AnsiString; ALink: TdxSpreadSheetExternalLink; ARels: TdxSpreadSheetXLSXWriterRels);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXWriterExternalLinkBuilder.Create(Owner, ARels, ALink, AFileName));
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteExternalLinks(
  ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels);
var
  I: Integer;
  AFileName: AnsiString;
begin
  for I := 0 to SpreadSheet.ExternalLinks.Count - 1 do
  begin
    AFileName := Format(sdxXLSXFileTemplateExternalLink, [I + 1]);
    WriteExternalLink(AFileName, SpreadSheet.ExternalLinks[I], ARels);
    ANode.AddChild(sdxXLSXNodeExternalReference).Attributes.Add(
      sdxXLSXAttrRId, ARels.GetRelationshipId(dxUnixPathDelim + AFileName));
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WritePrintableAreas(ANode: TdxXMLNode; AView: TdxSpreadSheetTableView);
begin
  if AView.OptionsPrint.Source.Area.Assigned then
  begin
    WriteDefinedName(ANode.AddChild(sdxXLSXNodeDefinedName), sdxXLSXPrintAreaDefinedName,
      dxReferenceToString(AView.OptionsPrint.Source.Area.Rect,
      SpreadSheet.OptionsView.R1C1Reference, [croSheetName], AView.Caption), AView);
  end;

  if AView.OptionsPrint.Source.RowsToRepeat.Assigned or AView.OptionsPrint.Source.ColumnsToRepeat.Assigned then
  begin
    WriteDefinedName(ANode.AddChild(sdxXLSXNodeDefinedName), sdxXLSXPrintTitlesDefinedName,
      TdxSpreadSheetPrintAreasHelper.BuildPrintTitlesReference(AView), AView);
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteProperties(AWorkbook: TdxXMLNode);
var
  ANode: TdxXMLNode;
begin
  ANode := AWorkbook.AddChild(sdxXLSXNodeWorkbookPr);
  ANode.Attributes.Add(sdxXLSXAttrDate1904, SpreadSheet.OptionsView.ActualDateTimeSystem = dts1904);

  if SpreadSheet.OptionsProtection.Protected then
    WriteProtection(AWorkbook.AddChild(sdxXLSXNodeWorkbookProtection));

  ANode := AWorkbook.AddChild(sdxXLSXNodeBookViews).AddChild(sdxXLSXNodeWorkBookView);
  ANode.Attributes.Add(sdxXLSXAttrActiveTab, SpreadSheet.ActiveSheetIndex);
  ANode.Attributes.Add(sdxXLSXAttrShowHorizontalScroll, SpreadSheet.OptionsView.HorizontalScrollBar);
  ANode.Attributes.Add(sdxXLSXAttrShowVerticalScroll, SpreadSheet.OptionsView.VerticalScrollBar);
  ANode.Attributes.Add(sdxXLSXAttrShowSheetTabs, SpreadSheet.PageControl.Visible);
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteProtection(ANode: TdxXMLNode);
var
  AProtection: TdxSpreadSheetStrongProtectionInfo;
begin
  ANode.Attributes.Add(sdxXLSXAttrLockStructure, not SpreadSheet.OptionsProtection.AllowChangeStructure);

  if SpreadSheet.OptionsProtection.ProtectionInfo is TdxSpreadSheetStandardProtectionInfo then
  begin
    ANode.Attributes.Add(sdxXLSXAttrWorkbookPassword,
      TdxSpreadSheetStandardProtectionInfo(SpreadSheet.OptionsProtection.ProtectionInfo).KeyWordAsString);
  end
  else

  if SpreadSheet.OptionsProtection.ProtectionInfo is TdxSpreadSheetStrongProtectionInfo then
  begin
    AProtection := TdxSpreadSheetStrongProtectionInfo(SpreadSheet.OptionsProtection.ProtectionInfo);
    ANode.Attributes.Add(sdxXLSXAttrWorkbookAlgorithmName, dxXLSXHashAlgorithmTypeNames[AProtection.HashAlgorithm]);
    ANode.Attributes.Add(sdxXLSXAttrWorkbookHashValue, AProtection.HashValueAsString);
    ANode.Attributes.Add(sdxXLSXAttrWorkbookSaltValue, AProtection.SaltValueAsString);
    ANode.Attributes.Add(sdxXLSXAttrWorkbookSpinCount, AProtection.SpinCount);
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteSheets(ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels);
var
  I: Integer;
begin
  for I := 0 to SpreadSheet.SheetCount - 1 do
    WriteSheet(ANode.AddChild(sdxXLSXNodeSheet), SpreadSheet.Sheets[I], ARels);
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteSheetView(
  const AFileName: AnsiString; AView: TdxSpreadSheetCustomView; ARels: TdxSpreadSheetXLSXWriterRels);
var
  ATableView: TdxSpreadSheetTableView;
begin
  if AView is TdxSpreadSheetTableView then
  begin
    ATableView := TdxSpreadSheetTableView(AView);
    ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.Create(Owner, ARels, AFileName, ATableView));
    FHasPrintableAreas := FHasPrintableAreas or HasPrintableAreas(ATableView);
  end
  else
    DoError(sdxErrorInternal, ['export of ' + AView.ClassName + ' is not supported'], ssmtError);
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteStyles(
  const AFileName: AnsiString; ARels: TdxSpreadSheetXLSXWriterRels);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXWriterStylesBuilder.Create(Owner, ARels, AFileName));
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteSharedStrings(
  const AFileName: AnsiString; ARels: TdxSpreadSheetXLSXWriterRels);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXWriterSharedStringsBuilder.Create(Owner, ARels, AFileName));
end;

procedure TdxSpreadSheetXLSXWriterWorkbookBuilder.WriteSheet(
  ANode: TdxXMLNode; AView: TdxSpreadSheetCustomView; ARels: TdxSpreadSheetXLSXWriterRels);
var
  AFileName: AnsiString;
begin
  AFileName := Format(sdxXLSXFileTemplateWorksheet, [AView.Index + 1]);
  ANode.Attributes.Add(sdxXLSXAttrName, AView.Caption);
  ANode.Attributes.Add(sdxXLSXAttrSheetId, AView.Index + 1);
  if not AView.Visible then
    ANode.Attributes.Add(sdxXLSXAttrState, sdxXLSXValueHidden);
  WriteSheetView(AFileName, AView, ARels);
  ANode.Attributes.Add(sdxXLSXAttrRId, ARels.GetRelationshipId(dxUnixPathDelim + AFileName));
end;

{ TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.Create(AOwner: TdxSpreadSheetXLSXWriter;
  AOwnerRels: TdxSpreadSheetXLSXWriterRels; const ATargetFileName: AnsiString; AView: TdxSpreadSheetTableView);
begin
  inherited Create(AOwner, AOwnerRels, ATargetFileName);
  FView := AView;
  FContainers := TList<TdxSpreadSheetContainer>.Create;
  FComments := TList<TdxSpreadSheetCommentContainer>.Create;
end;

destructor TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.Destroy;
begin
  FreeAndNil(FContainers);
  FreeAndNil(FComments);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.ExecuteCore(AWriter: TdxXmlWriter);
var
  ARels: TdxSpreadSheetXLSXWriterRels;
begin
  InitializeContainers;

  ARels := TdxSpreadSheetXLSXWriterRels.Create(nil);
  try
    AWriter.WriteStartElement(sdxXLSXNodeWorksheet, sdxXLSXWorkbookNameSpace);
    AWriter.WriteAttributeString(sdxXLSXAttrXMLNS, 'r', '', sdxXLSXCommonRelationshipPath);
    WriteContent(AWriter, ARels);
    AWriter.WriteEndElement;

    Owner.WriteRels(TargetFileNameRels, ARels);
  finally
    ARels.Free;
  end;
end;

class function TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.GetContentRelationship: AnsiString;
begin
  Result := sdxXLSXWorksheetRelationship;
end;

class function TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.GetContentType: AnsiString;
begin
  Result := sdxXLSXWorksheetContentType;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.InitializeContainers;
var
  AContainer: TdxSpreadSheetContainer;
  I: Integer;
begin
  for I := 0 to View.Containers.Count - 1 do
  begin
    AContainer := View.Containers[I];
    if AContainer is TdxSpreadSheetCommentContainer then
      Comments.Add(TdxSpreadSheetCommentContainer(AContainer))
    else
      Containers.Add(AContainer);
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.ConvertRowHeight(const AValue: Integer): Double;
begin
  Result := TdxValueUnitsHelper.PixelsToPoints(Max(AValue, 1));
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.EncodeFloat(const AValue: Double): string;
begin
  Result := FloatToStr(AValue, dxInvariantFormatSettings);
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.GetActivePane(out AValue: string): Boolean;
begin
  Result := (View.FrozenColumn >= 0) or (View.FrozenRow >= 0);
  if Result then
  begin
    if View.FrozenRow < 0 then
      AValue := sdxXLSXValuePaneTopRight
    else
      if View.FrozenColumn >= 0 then
        AValue := sdxXLSXValuePaneBottomRight
      else
        AValue := sdxXLSXValuePaneBottomLeft;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.ValidateDimensions(const R: TRect): TRect;
begin
  Result := R;
  Result.Right := Min(Result.Right, dxXLSXMaxColumnIndex);
  Result.Bottom := Min(Result.Bottom, dxXLSXMaxRowIndex);
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteBreaks(
  AWriter: TdxXMLWriter; const ANodeName: string; AList: TList<Cardinal>; AMaxIndex: Integer);
var
  I: Integer;
begin
  AList.Sort;
  AWriter.WriteStartElement(ANodeName);
  try
    AWriter.WriteAttributeInteger(sdxXLSXAttrBreaksCount, AList.Count);
    AWriter.WriteAttributeInteger(sdxXLSXAttrBreaksManualBreakCount, AList.Count);
    for I := 0 to AList.Count - 1 do
    begin
      AWriter.WriteStartElement(sdxXLSXNodeBreak);
      AWriter.WriteAttributeInteger(sdxXLSXAttrBreakID, AList[I]);
      AWriter.WriteAttributeInteger(sdxXLSXAttrMin, 0);
      AWriter.WriteAttributeInteger(sdxXLSXAttrMax, AMaxIndex);
      AWriter.WriteAttributeBoolean(sdxXLSXAttrBreakManual, True);
      AWriter.WriteEndElement;
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteCell(
  AWriter: TdxXmlWriter; ACell: TdxSpreadSheetCell; ARowIndex, AColumnIndex: Integer);
const
  Map: array[TdxSpreadSheetCellDataType] of TdxSpreadSheetXLSXCellType = (
    sxctUnknown, sxctBoolean, sxctError, sxctFloat, sxctFloat, sxctFloat, sxctFloat, sxctSharedString, sxctFormula
  );
var
  ADataType: TdxSpreadSheetXLSXCellType;
begin
  AWriter.WriteStartElement('c');
  try
    AWriter.WriteAttributeString(sdxXLSXAttrCellColumn, dxReferenceToString(ARowIndex, AColumnIndex));
    AWriter.WriteAttributeInteger(sdxXLSXAttrStyleIndex, Owner.CellStyles.Add(ACell.StyleHandle));

    ADataType := Map[ACell.DataType];
    if ADataType <> sxctUnknown then
    begin
      AWriter.WriteAttributeString(sdxXLSXAttrCellType, dxXLSXCellDataTypeNames[ADataType]);
      case ADataType of
        sxctFloat:
          AWriter.WriteElementString(sdxXLSXNodeCellValue, EncodeFloat(ACell.AsFloat));
        sxctBoolean:
          AWriter.WriteElementString(sdxXLSXNodeCellValue, TdxXMLHelper.EncodeBoolean(ACell.AsBoolean));
        sxctError:
          AWriter.WriteElementString(sdxXLSXNodeCellValue, dxSpreadSheetErrorCodeToString(ACell.AsError));
        sxctSharedString:
          AWriter.WriteElementStringEx(sdxXLSXNodeCellValue, IntToStr(Owner.SharedStrings.Add(ACell.AsSharedString)));
        sxctFormula:
          begin
            AWriter.WriteStartElement(sdxXLSXNodeCellFunction);
            if ACell.AsFormula.IsArrayFormula then
            begin
              AWriter.WriteAttributeString(sdxXLSXAttrCellType, sdxXLSXValueArray);
              AWriter.WriteAttributeString(sdxXLSXAttrRef, dxReferenceToString(
                cxRect(ACell.ColumnIndex, ACell.RowIndex,
                  ACell.ColumnIndex + ACell.AsFormula.ArrayFormulaSize.cx - 1,
                  ACell.RowIndex + ACell.AsFormula.ArrayFormulaSize.cy - 1)
              ));
            end;
            AWriter.WriteValue(Copy(ACell.AsFormula.AsText, 2, MaxInt));
            AWriter.WriteEndElement;
          end;
      else
        DoError(sdxErrorInternal, [sMsgWrongDataType], ssmtError);
      end;
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteColumns(AWriter: TdxXmlWriter; ADimension: TRect);
var
  ANodeStarted: Boolean;

  function CompareColumns(AColumn1, AColumn2: TdxSpreadSheetTableColumnAccess): Boolean;
  begin
    Result := (AColumn1 = nil) and (AColumn2 = nil) or
      (AColumn1 <> nil) and (AColumn2 <> nil) and (AColumn1.Style.Handle = AColumn2.Style.Handle) and
      (AColumn1.CustomSize = AColumn2.CustomSize) and (AColumn1.Visible = AColumn2.Visible);
  end;

  function GetColumnStyle(AColumn: TdxSpreadSheetTableColumnAccess): TdxSpreadSheetCellStyleHandle;
  begin
    if AColumn <> nil then
      Result := AColumn.Style.Handle
    else
      Result := View.CellStyles.DefaultStyle;
  end;

  function GetColumnSize(AColumn: TdxSpreadSheetTableColumnAccess): Integer;
  begin
    if (AColumn = nil) or AColumn.DefaultSize then
      Result := View.Columns.DefaultSize
    else
      Result := AColumn.CustomSize;
  end;

  procedure AddColumnInfo(AColumn: TdxSpreadSheetTableColumnAccess; AColumnIndex: Integer;
    AColumnGroup: TdxSpreadSheetTableItemGroupAccess; ACollapsed: Boolean; AFinishIndex: Integer;
    AForce: Boolean = False);
  begin
    if not AForce and (AColumn = nil) and (AColumnGroup = nil) and not ACollapsed then
      Exit;

    if not ANodeStarted then
    begin
      AWriter.WriteStartElement(sdxXLSXNodeColumns);
      ANodeStarted := True;
    end;

    AWriter.WriteStartElement(sdxXLSXNodeColumn);
    try
      AWriter.WriteAttributeInteger(sdxXLSXAttrMax, AFinishIndex + 1);
      AWriter.WriteAttributeInteger(sdxXLSXAttrMin, AColumnIndex + 1);
      AWriter.WriteAttributeInteger(sdxXLSXAttrStyle, Owner.CellStyles.Add(GetColumnStyle(AColumn)));
      AWriter.WriteAttributeFloat(sdxXLSXAttrWidth, Owner.ColumnWidthHelper.PixelsToWidth(GetColumnSize(AColumn)));
      if AColumnGroup <> nil then
        AWriter.WriteAttributeInteger(sdxXLSXAttrOutlineLevel, AColumnGroup.Level + 1);
      if ACollapsed then
        AWriter.WriteAttributeBoolean(sdxXLSXAttrCollapsed, True);
      if (AColumn <> nil) and not AColumn.DefaultSize then
      begin
        AWriter.WriteAttributeBoolean(sdxXLSXAttrCustomWidth, True);
        if not AColumn.IsCustomSize then
          AWriter.WriteAttributeBoolean(sdxXLSXAttrBestFit, True);
      end;
      if (AColumn <> nil) and not AColumn.Visible then
        AWriter.WriteAttributeBoolean(sdxXLSXAttrHidden, True);
    finally
      AWriter.WriteEndElement;
    end;
  end;

var
  ACollapsed: Boolean;
  AColumn: TdxSpreadSheetTableColumnAccess;
  AColumnGroup: TdxSpreadSheetTableItemGroupAccess;
  AColumnIndex: Integer;
  APrevColumn: TdxSpreadSheetTableColumnAccess;
  APrevColumnGroup: TdxSpreadSheetTableItemGroupAccess;
  APrevColumnIndex: Integer;
begin
  ANodeStarted := False;
  ADimension.Right := Max(ADimension.Right, View.Columns.LastIndex);
  ExtendDimensionsByGroups(View.Columns.Groups, ADimension.Left, ADimension.Right);
  ADimension := ValidateDimensions(ADimension);

  ACollapsed := False;
  APrevColumnIndex := ADimension.Left;
  APrevColumnGroup := TdxSpreadSheetTableItemGroupAccess(View.Columns.Groups.Find(APrevColumnIndex));
  APrevColumn := TdxSpreadSheetTableColumnAccess(View.Columns.Items[APrevColumnIndex]);

  for AColumnIndex := ADimension.Left + 1 to ADimension.Right do
  begin
    AColumn := TdxSpreadSheetTableColumnAccess(View.Columns.Items[AColumnIndex]);
    AColumnGroup := TdxSpreadSheetTableItemGroupAccess(View.Columns.Groups.Find(AColumnIndex));

    if (APrevColumnGroup <> AColumnGroup) or not CompareColumns(AColumn, APrevColumn) then
    begin
      AddColumnInfo(APrevColumn, APrevColumnIndex, APrevColumnGroup, ACollapsed, AColumnIndex - 1);
      ACollapsed := (APrevColumnGroup <> nil) and APrevColumnGroup.IsCollapsedByUser;
      APrevColumnGroup := AColumnGroup;
      APrevColumnIndex := AColumnIndex;
      APrevColumn := AColumn;
    end;
  end;

  AddColumnInfo(APrevColumn, APrevColumnIndex, APrevColumnGroup, ACollapsed, ADimension.Right);

  if Owner.CellStyleDefault <> SpreadSheet.DefaultCellStyle.Handle then
    AddColumnInfo(nil, ADimension.Right, nil, False, dxXLSXMaxColumnIndex, True);
  if ANodeStarted then
    AWriter.WriteEndElement;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteComments(
  AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels);
var
  AFileName: AnsiString;
begin
  AWriter.WriteStartElement(sdxXLSXNodeLegacyDrawing);
  try
    AFileName := Format(sdxXLSXFileTemplateVMLDrawing, [View.Index + 1]);
    ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder.Create(AFileName, ARels, Self));
    ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.Create(Self, ARels, Format(sdxXLSXFileTemplateComments, [View.Index + 1])));
    AWriter.WriteAttributeString('r', 'id', '', ARels.GetRelationshipId(dxUnixPathDelim + AFileName));
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteConditionalFormatting(AWriter: TdxXmlWriter);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.Create(Owner, AWriter, View.ConditionalFormatting));
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteContainers(
  AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels);
var
  AFileName: AnsiString;
begin
  AWriter.WriteStartElement(sdxXLSXNodeDrawing);
  try
    AFileName := Format(sdxXLSXFileTemplateDrawing, [View.Index + 1]);
    ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.Create(AFileName, ARels, Self));
    AWriter.WriteAttributeString('r', 'id', '', ARels.GetRelationshipId(dxUnixPathDelim + AFileName));
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteContent(
  AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels);
var
  ADimension: TRect;
begin
  ADimension := ValidateDimensions(View.Dimensions);
  // sdxXLSXNodeSheetPr
  AWriter.WriteStartElement(sdxXLSXNodeSheetPr);
  AWriter.WriteStartElement(sdxXLSXNodeOutlinePr);
  AWriter.WriteAttributeBoolean(sdxXLSXAttrSummaryBelow, View.Rows.Groups.ExpandButtonPosition = gebpGroupFinish);
  AWriter.WriteAttributeBoolean(sdxXLSXAttrSummaryRight, View.Columns.Groups.ExpandButtonPosition = gebpGroupFinish);
  AWriter.WriteEndElement;
  if View.OptionsPrint.Page.ScaleMode <> oppsmDefault then
  begin
    AWriter.WriteStartElement(sdxXLSXNodePageSetUpPr);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrFitToPage, View.OptionsPrint.Page.ScaleMode = oppsmFitToPage);
    AWriter.WriteEndElement;
  end;
  AWriter.WriteEndElement;

  // sdxXLSXNodeDimension
  AWriter.WriteStartElement(sdxXLSXNodeDimension);
  AWriter.WriteAttributeString(sdxXLSXAttrRef, dxReferenceToString(ADimension));
  AWriter.WriteEndElement;

  WriteViewProperties(AWriter);
  WriteProperties(AWriter);
  WriteColumns(AWriter, ADimension);
  WriteRows(AWriter, ADimension);

  if View.Options.Protected then
    WriteViewProtection(AWriter);
  if View.MergedCells.Count > 0 then
    WriteMergedCells(AWriter, View.MergedCells);

  WriteConditionalFormatting(AWriter);
  WriteHyperlinks(AWriter, ARels);
  WritePrintOptions(AWriter);

  if View.OptionsPrint.Page.Margins.Assigned then
    WritePageMargins(AWriter, View.OptionsPrint.Page.Margins);

  WritePageSetup(AWriter);

  if View.OptionsPrint.HeaderFooter.Assigned then
    WriteHeaderFooter(AWriter, View.OptionsPrint.HeaderFooter);
  if View.OptionsPrint.Pagination.RowPageBreaks.Count > 0 then
    WriteBreaks(AWriter, sdxXLSXNodeRowBreaks, View.OptionsPrint.Pagination.RowPageBreaks, View.Dimensions.Bottom);
  if View.OptionsPrint.Pagination.ColumnPageBreaks.Count > 0 then
    WriteBreaks(AWriter, sdxXLSXNodeColBreaks, View.OptionsPrint.Pagination.ColumnPageBreaks, View.Dimensions.Right);
  if Containers.Count > 0 then
    WriteContainers(AWriter, ARels);
  if Comments.Count > 0 then
    WriteComments(AWriter, ARels);
  WriteExtensions(AWriter);
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteExtensions(AWriter: TdxXmlWriter);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExBuilder.Create(Owner, AWriter, View.ConditionalFormatting));
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteFixedPaneProperties(AWriter: TdxXmlWriter);
var
  AValue: string;
begin
  AWriter.WriteStartElement(sdxXLSXNodePane);
  try
    AWriter.WriteAttributeString(sdxXLSXAttrTopLeftCell, dxReferenceToString(
      TdxSpreadSheetTableViewAccess(View).ViewInfo.FirstScrollableRow,
      TdxSpreadSheetTableViewAccess(View).ViewInfo.FirstScrollableColumn));
    AWriter.WriteAttributeString(sdxXLSXAttrState, sdxXLSXValueFrozen);
    if GetActivePane(AValue) then
      AWriter.WriteAttributeString(sdxXLSXAttrActivePane, AValue);
    if View.FrozenColumn >= 0 then
      AWriter.WriteAttributeInteger(sdxXLSXAttrSplitX, View.FrozenColumn + 1);
    if View.FrozenRow >= 0 then
      AWriter.WriteAttributeInteger(sdxXLSXAttrSplitY, View.FrozenRow + 1);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteHeaderFooter(
  AWriter: TdxXmlWriter; AHeaderFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooter);
begin
  AWriter.WriteStartElement(sdxXLSXNodeHeaderFooter);
  try
    if AHeaderFooter.AlignWithMargins <> bDefault then
      AWriter.WriteAttributeBoolean(sdxXLSXAttrHeaderFooterAlignWithMargins, AHeaderFooter.AlignWithMargins = bTrue);
    if AHeaderFooter.ScaleWithDocument <> bDefault then
      AWriter.WriteAttributeBoolean(sdxXLSXAttrHeaderFooterScaleWithDocument, AHeaderFooter.ScaleWithDocument = bTrue);
    if AHeaderFooter.EvenPagesFooter.Assigned or AHeaderFooter.EvenPagesHeader.Assigned then
      AWriter.WriteAttributeBoolean(sdxXLSXAttrHeaderFooterDifferentOddEven, True);
    if AHeaderFooter.FirstPageFooter.Assigned or AHeaderFooter.FirstPageHeader.Assigned then
      AWriter.WriteAttributeBoolean(sdxXLSXAttrHeaderFooterDifferentFirst, True);

    WriteHeaderFooterText(AWriter, sdxXLSXNodeOddHeader, AHeaderFooter.CommonHeader);
    WriteHeaderFooterText(AWriter, sdxXLSXNodeOddFooter, AHeaderFooter.CommonFooter);

    if AHeaderFooter.EvenPagesFooter.Assigned or AHeaderFooter.EvenPagesHeader.Assigned then
    begin
      WriteHeaderFooterText(AWriter, sdxXLSXNodeEvenHeader, AHeaderFooter.EvenPagesHeader);
      WriteHeaderFooterText(AWriter, sdxXLSXNodeEvenFooter, AHeaderFooter.EvenPagesFooter);
    end;

    if AHeaderFooter.FirstPageFooter.Assigned or AHeaderFooter.FirstPageHeader.Assigned then
    begin
      WriteHeaderFooterText(AWriter, sdxXLSXNodeFirstHeader, AHeaderFooter.FirstPageHeader);
      WriteHeaderFooterText(AWriter, sdxXLSXNodeFirstFooter, AHeaderFooter.FirstPageFooter);
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteHeaderFooterText(
  AWriter: TdxXmlWriter; const ANodeName: string; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  AWriter.WriteElementStringEx(ANodeName, TdxSpreadSheetHeaderFooterHelper.Build(AText));
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteHyperlink(
  AWriter: TdxXmlWriter; AHyperlink: TdxSpreadSheetHyperLink; ARels: TdxSpreadSheetXLSXWriterRels);
var
  ARelsNode: TdxXMLNode;
begin
  AWriter.WriteStartElement(sdxXLSXNodeHyperlink);
  try
    AWriter.WriteAttributeString(sdxXLSXAttrRef, dxReferenceToString(AHyperlink.Area));
    if havDisplayText in AHyperlink.AssignedValues then
      AWriter.WriteAttributeString(sdxXLSXAttrDisplay, AHyperlink.DisplayText);
    if havScreenTip in AHyperlink.AssignedValues then
      AWriter.WriteAttributeString(sdxXLSXAttrTooltip, AHyperlink.ScreenTip);
    if AHyperlink.ValueType = hvtReference then
      AWriter.WriteAttributeString(sdxXLSXAttrLocation, dxSpreadSheetFormulaExcludeEqualSymbol(AHyperlink.Value))
    else
    begin
      ARelsNode := ARels.DoAddRelationship(sdxXLSXHyperlinkRelationship, dxStringToXMLString(AHyperlink.Value));
      ARelsNode.Attributes.Add(sdxXLSXAttrTargetMode, sdxXLSXValueTargetModeExternal);
      AWriter.WriteAttributeString('r', 'id', '', ARelsNode.Attributes.GetValue(sdxXLSXAttrId));
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteHyperlinks(
  AWriter: TdxXmlWriter; ARels: TdxSpreadSheetXLSXWriterRels);
var
  ANodeStarted: Boolean;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  ANodeStarted := False;
  AHyperlink := View.Hyperlinks.First;
  while AHyperlink <> nil do
  begin
    if AHyperlink.IsAreaCorrect then
    begin
      if not ANodeStarted then
      begin
        ANodeStarted := True;
        AWriter.WriteStartElement(sdxXLSXNodeHyperlinks);
      end;
      WriteHyperlink(AWriter, AHyperlink, ARels);
    end;
    AHyperlink := AHyperlink.Next;
  end;
  if ANodeStarted then
    AWriter.WriteEndElement;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteMergedCells(
  AWriter: TdxXmlWriter; AMergedCells: TdxSpreadSheetMergedCellList);
var
  AItem: TdxSpreadSheetMergedCell;
begin
  AWriter.WriteStartElement(sdxXLSXNodeMergeCells);
  try
    AItem := AMergedCells.First;
    while AItem <> nil do
    begin
      AWriter.WriteStartElement(sdxXLSXNodeMergeCell);
      AWriter.WriteAttributeString(sdxXLSXAttrRef, dxReferenceToString(AItem.Area));
      AWriter.WriteEndElement;

      AItem := TdxSpreadSheetMergedCell(AItem.Next);
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WritePageMargins(
  AWriter: TdxXmlWriter; AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins);
begin
  AWriter.WriteStartElement(sdxXLSXNodePageMargins);
  try
    AWriter.WriteAttributeFloat(sdxXLSXAttrPageMarginsLeft, AMargins.Left);
    AWriter.WriteAttributeFloat(sdxXLSXAttrPageMarginsTop, AMargins.Top);
    AWriter.WriteAttributeFloat(sdxXLSXAttrPageMarginsRight, AMargins.Right);
    AWriter.WriteAttributeFloat(sdxXLSXAttrPageMarginsBottom, AMargins.Bottom);
    AWriter.WriteAttributeFloat(sdxXLSXAttrPageMarginsHeader, AMargins.Header);
    AWriter.WriteAttributeFloat(sdxXLSXAttrPageMarginsFooter, AMargins.Footer);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WritePageSetup(AWriter: TdxXmlWriter);
var
  APage: TdxSpreadSheetTableViewOptionsPrintPage;
  APaper: TdxSpreadSheetTableViewOptionsPrintPagePaper;
  APrinting: TdxSpreadSheetTableViewOptionsPrintPrinting;
begin
  AWriter.WriteStartElement(sdxXLSXNodePageSetup);
  try
    APage := View.OptionsPrint.Page;
    AWriter.WriteAttributeString(sdxXLSXAttrPageSetupOrientation, dxXLSXPrintPageOrientation[APage.Orientation]);
    AWriter.WriteAttributeInteger(sdxXLSXAttrPageSetupFitToHeight, APage.FitToHeight);
    AWriter.WriteAttributeInteger(sdxXLSXAttrPageSetupFitToWidth, APage.FitToWidth);

    if APage.Scale <> 100 then
      AWriter.WriteAttributeInteger(sdxXLSXAttrPageSetupScale, APage.Scale);

    if APage.FirstPageNumber > 0 then
    begin
      AWriter.WriteAttributeInteger(sdxXLSXAttrPageSetupFirstPageNumber, APage.FirstPageNumber);
      AWriter.WriteAttributeBoolean(sdxXLSXAttrPageSetupUseFirstPageNumber, True);
    end;

    APaper := View.OptionsPrint.Page.Paper;
    if APaper.Assigned then
    begin
      if APaper.SizeID > 0 then
        AWriter.WriteAttributeInteger(sdxXLSXAttrPageSetupPaperSize, APaper.SizeID)
      else
        if (APaper.CustomSize.X > 0) and (APaper.CustomSize.Y > 0) then
        begin
          AWriter.WriteAttributeString(sdxXLSXAttrPageSetupPaperWidth, FloatToStr(APaper.CustomSize.X, dxInvariantFormatSettings) + 'in');
          AWriter.WriteAttributeString(sdxXLSXAttrPageSetupPaperHeight, FloatToStr(APaper.CustomSize.Y, dxInvariantFormatSettings) + 'in');
        end;
    end;

    APrinting := View.OptionsPrint.Printing;
    if APrinting.BlackAndWhite <> bDefault then
      AWriter.WriteAttributeBoolean(sdxXLSXAttrPageSetupBlackAndWhite, APrinting.BlackAndWhite = bTrue);
    if APrinting.Draft <> bDefault then
      AWriter.WriteAttributeBoolean(sdxXLSXAttrPageSetupDraft, APrinting.Draft = bTrue);
    if APrinting.Copies > 0 then
      AWriter.WriteAttributeInteger(sdxXLSXAttrPageSetupCopies, Min(APrinting.Copies, MAXSHORT));
    if APrinting.PageOrder <> opppDefault then
      AWriter.WriteAttributeString(sdxXLSXAttrPageSetupPageOrder, dxXLSXPrintPageOrder[APrinting.PageOrder]);
    if View.OptionsPrint.Source.ErrorIndication <> pseiDefault then
      AWriter.WriteAttributeString(sdxXLSXAttrPageSetupErrors, dxXLSXPrintErrorIndication[View.OptionsPrint.Source.ErrorIndication]);
    AWriter.WriteAttributeString(sdxXLSXAttrPageSetupCellComments, dxXLSXPrintCellComments[View.OptionsPrint.Source.CellComments]);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WritePrintOptions(AWriter: TdxXmlWriter);

  function ArePrintOptionsAssigned: Boolean;
  begin
    Result :=
      (View.OptionsPrint.Source.Headers <> bDefault) or
      (View.OptionsPrint.Source.GridLines <> bDefault) or
      (View.OptionsPrint.Printing.HorizontalCentered <> bDefault) or
      (View.OptionsPrint.Printing.VerticalCentered <> bDefault);
  end;

begin
  if ArePrintOptionsAssigned then
  begin
    AWriter.WriteStartElement(sdxXLSXNodePrintOptions);
    try
      if View.OptionsPrint.Printing.HorizontalCentered <> bDefault then
        AWriter.WriteAttributeBoolean(sdxXLSXAttrPrintOptionsHorzCenter, View.OptionsPrint.Printing.HorizontalCentered = bTrue);
      if View.OptionsPrint.Printing.VerticalCentered <> bDefault then
        AWriter.WriteAttributeBoolean(sdxXLSXAttrPrintOptionsVertCenter, View.OptionsPrint.Printing.VerticalCentered = bTrue);
      if View.OptionsPrint.Source.Headers <> bDefault then
        AWriter.WriteAttributeBoolean(sdxXLSXAttrPrintOptionsHeadings, View.OptionsPrint.Source.Headers = bTrue);
      if View.OptionsPrint.Source.GridLines <> bDefault then
      begin
        AWriter.WriteAttributeBoolean(sdxXLSXAttrPrintOptionsGridLines, View.OptionsPrint.Source.GridLines = bTrue);
        AWriter.WriteAttributeBoolean(sdxXLSXAttrPrintOptionsGridLinesSet, True);
      end;
    finally
      AWriter.WriteEndElement;
    end;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteProperties(AWriter: TdxXmlWriter);
begin
  AWriter.WriteStartElement(sdxXLSXNodeSheetFormatPr);
  try
    AWriter.WriteAttributeFloat(sdxXLSXAttrDefaultColumnWidth, Owner.ColumnWidthHelper.PixelsToWidth(View.Columns.DefaultSize));
    AWriter.WriteAttributeBoolean(sdxXLSXAttrCustomHeight, True);
    AWriter.WriteAttributeFloat(sdxXLSXAttrDefaultRowHeight, ConvertRowHeight(View.Rows.DefaultSize));
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteRows(AWriter: TdxXmlWriter; ADimension: TRect);
var
  ACell: TdxDynamicListItem;
  AGroup: TdxSpreadSheetTableItemGroupAccess;
  APrevGroupCollapsed: Boolean;
  ARow: TdxSpreadSheetTableRowAccess;
  ARowIndex: Integer;
begin
  APrevGroupCollapsed := False;
  ExtendDimensionsByGroups(View.Rows.Groups, ADimension.Top, ADimension.Bottom);
  ADimension := ValidateDimensions(ADimension);

  Owner.ProgressHelper.BeginStage(cxRectHeight(ADimension) + 1);
  try
    AWriter.WriteStartElement(sdxXLSXNodeSheetData);
    try
      for ARowIndex := ADimension.Top to ADimension.Bottom do
      begin
        ARow := TdxSpreadSheetTableRowAccess(View.Rows.Items[ARowIndex]);
        AGroup := TdxSpreadSheetTableItemGroupAccess(View.Rows.Groups.Find(ARowIndex));
        if (ARow <> nil) or (AGroup <> nil) then
        begin
          AWriter.WriteStartElement(sdxXLSXNodeRow);
          AWriter.WriteAttributeInteger(sdxXLSXAttrRowIndex, ARowIndex + 1);

          if APrevGroupCollapsed then
            AWriter.WriteAttributeBoolean(sdxXLSXAttrCollapsed, True);
          if AGroup <> nil then
            AWriter.WriteAttributeInteger(sdxXLSXAttrOutlineLevel, AGroup.Level + 1);
          APrevGroupCollapsed := (AGroup <> nil) and AGroup.IsCollapsedByUser;

          if ARow <> nil then
          begin
            if not ARow.Visible then
              AWriter.WriteAttributeBoolean(sdxXLSXAttrHidden, True);

            if not ARow.DefaultSize then
            begin
              if ARow.IsCustomSize then
                AWriter.WriteAttributeBoolean(sdxXLSXAttrCustomHeight, True);
              AWriter.WriteAttributeFloat(sdxXLSXAttrRowHeight, ConvertRowHeight(ARow.CustomSize));
            end;

            if ARow.Style.Handle <> Owner.CellStyleDefault then
            begin
              AWriter.WriteAttributeBoolean(sdxXLSXAttrCustomFormat, True);
              AWriter.WriteAttributeInteger(sdxXLSXAttrStyleIndex, Owner.CellStyles.Add(ARow.Style.Handle));
            end;

            ACell := ARow.RowCells.First;
            while ACell <> nil do
            begin
              WriteCell(AWriter, TdxSpreadSheetCell(ACell), ARowIndex, ACell.Index);
              ACell := TdxDynamicListItemAccess(ACell).FNext;
            end;
          end;

          AWriter.WriteEndElement; // sdxXLSXNodeRow
        end;
        Owner.ProgressHelper.NextTask;
      end;
    finally
      AWriter.WriteEndElement;
    end;
  finally
    Owner.ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteSelection(
  AWriter: TdxXmlWriter; ASelection: TdxSpreadSheetTableViewSelection);

  function UniqueSelection: TStringList;
  var
    I: Integer;
  begin
    Result := TStringList.Create;
    Result.Duplicates := dupIgnore;
    Result.Sorted := True;
    for I := 0 to ASelection.Count - 1 do
      Result.Add(dxReferenceToString(ValidateDimensions(ASelection.Items[I].Rect)));
  end;

  function EncodeSelection: string;
  var
    B: TStringBuilder;
    I: Integer;
    S: TStringList;
  begin
    S := UniqueSelection;
    try
      if S.Count = 1 then
        Exit(S[0]);

      B := TdxStringBuilderManager.Get;
      try
        for I := 0 to S.Count - 1 do
        begin
          if I > 0 then
            B.Append(' ');
          B.Append(S[I]);
        end;
        Result := B.ToString;
      finally
        TdxStringBuilderManager.Release(B);
      end;
    finally
      S.Free;
    end;
  end;

var
  AValue: string;
begin
  AWriter.WriteStartElement(sdxXLSXNodeSelection);
  try
    if GetActivePane(AValue) then
      AWriter.WriteAttributeString(sdxXLSXAttrPane, AValue);

    AWriter.WriteAttributeString(sdxXLSXAttrSqRef, EncodeSelection);
    if InRange(ASelection.FocusedRow, 0, dxXLSXMaxRowIndex) and InRange(ASelection.FocusedColumn, 0, dxXLSXMaxColumnIndex) then
      AWriter.WriteAttributeString(sdxXLSXAttrActiveCell, dxReferenceToString(ASelection.FocusedRow, ASelection.FocusedColumn));
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteViewProperties(AWriter: TdxXmlWriter);
var
  AZoomFactor: Integer;
begin
  AWriter.WriteStartElement(sdxXLSXNodeSheetsView);
  AWriter.WriteStartElement(sdxXLSXNodeSheetView);
  AWriter.WriteAttributeInteger(sdxXLSXAttrWorkbookViewId, 0);

  AZoomFactor := Max(Min(View.Options.ZoomFactor, MaxZoomFactor), MinZoomFactor);
  if AZoomFactor <> 100 then
  begin
    AWriter.WriteAttributeInteger(sdxXLSXAttrZoomScale, AZoomFactor);
    AWriter.WriteAttributeInteger(sdxXLSXAttrZoomScaleNormal, AZoomFactor);
  end;

  if View.Active then
    AWriter.WriteAttributeBoolean(sdxXLSXAttrTabSelected, True);

  AWriter.WriteAttributeBoolean(sdxXLSXAttrZeroValues, View.Options.ActualZeroValues);
  AWriter.WriteAttributeBoolean(sdxXLSXAttrShowFormulas, View.Options.ActualShowFormulas);
  AWriter.WriteAttributeBoolean(sdxXLSXAttrGridLines, View.Options.ActualGridLines);
  AWriter.WriteAttributeBoolean(sdxXLSXAttrShowRowColHeaders, View.Options.ActualHeaders);

  if (View.FrozenColumn >= 0) or (View.FrozenRow >= 0) then
    WriteFixedPaneProperties(AWriter);

  if View.Selection.Count > 0 then
    WriteSelection(AWriter, View.Selection);

  AWriter.WriteEndElement; // sdxXLSXNodeSheetView
  AWriter.WriteEndElement; // sdxXLSXNodeSheetsView
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.WriteViewProtection(AWriter: TdxXmlWriter);
var
  AProtection: TdxSpreadSheetStrongProtectionInfo;
begin
  AWriter.WriteStartElement(sdxXLSXNodeSheetProtection);
  try
    AWriter.WriteAttributeBoolean(sdxXLSXAttrSheet, View.OptionsProtection.Protected);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrDeleteColumns, not View.OptionsProtection.AllowDeleteColumns);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrDeleteRows, not View.OptionsProtection.AllowDeleteRows);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrFormatCells, not View.OptionsProtection.AllowFormatCells);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrFormatColumns, not View.OptionsProtection.AllowResizeColumns);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrFormatRows, not View.OptionsProtection.AllowResizeRows);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrInsertColumns, not View.OptionsProtection.AllowInsertColumns);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrInsertRows, not View.OptionsProtection.AllowInsertRows);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrInsertHyperlinks, not View.OptionsProtection.AllowEditHyperlinks);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrObjects, not View.OptionsProtection.AllowEditContainers);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrSelectLockedCells, not View.OptionsProtection.AllowSelectLockedCells);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrSelectUnlockedCell, not View.OptionsProtection.AllowSelectUnlockedCells);
    AWriter.WriteAttributeBoolean(sdxXLSXAttrSort, not View.OptionsProtection.AllowSort);

    if View.OptionsProtection.ProtectionInfo is TdxSpreadSheetStandardProtectionInfo then
      AWriter.WriteAttributeString(sdxXLSXAttrPassword, TdxSpreadSheetStandardProtectionInfo(View.OptionsProtection.ProtectionInfo).KeyWordAsString)
    else

    if View.OptionsProtection.ProtectionInfo is TdxSpreadSheetStrongProtectionInfo then
    begin
      AProtection := TdxSpreadSheetStrongProtectionInfo(View.OptionsProtection.ProtectionInfo);
      AWriter.WriteAttributeString(sdxXLSXAttrAlgorithmName, dxXLSXHashAlgorithmTypeNames[AProtection.HashAlgorithm]);
      AWriter.WriteAttributeString(sdxXLSXAttrHashValue, AProtection.HashValueAsString);
      AWriter.WriteAttributeString(sdxXLSXAttrSaltValue, AProtection.SaltValueAsString);
      AWriter.WriteAttributeInteger(sdxXLSXAttrSpinCount, AProtection.SpinCount);
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder.ExtendDimensionsByGroups(
  AGroups: TdxSpreadSheetTableItemGroups; var AStartIndex, AFinishIndex: Integer);
begin
  if AGroups.Count > 0 then
  begin
    AFinishIndex := Max(AFinishIndex, AGroups[AGroups.Count - 1].FinishIndex);
    AStartIndex := Min(AStartIndex, AGroups[0].StartIndex);
  end;
end;

{ TdxSpreadSheetXLSXWriterWorksheetTableViewSubFileBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetTableViewSubFileBuilder.Create(const ATargetFileName: AnsiString;
  AOwnerRels: TdxSpreadSheetXLSXWriterRels; AOwnerWriter: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder);
begin
  inherited Create(AOwnerWriter.Owner, AOwnerRels, ATargetFileName);
  FOwnerWriter := AOwnerWriter;
end;

{ TdxSpreadSheetXLTXWriter }

function TdxSpreadSheetXLTXWriter.GetContentTypeID: AnsiString;
begin
  Result := sdxXLSXWorkbookTemplateContentType;
end;

end.
