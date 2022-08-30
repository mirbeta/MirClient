{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Import.OpenXML.DestinationBase;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections, RegularExpressions, Math,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.NumberParser,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import.Core,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter;

const
  NullSingleValue: Single = -MaxSingle;

type
  TdxDestinationBasedImporter = class;
  TdxDestinationAndXmlBasedImporter = class;
  TdxDestination = class;
  TdxDestinationStack = class;

  { TdxLockAspectRatioTable }

  TdxLockAspectRatioTable = class(TdxStringsDictionary);

  { TdxDestinationBasedImporter }

  TdxDestinationBasedImporter = class abstract(TdxDocumentModelImporter)
  strict private
    FDestinationStack: TdxDestinationStack;
    FObjectsToDelete: TdxFastObjectList;
  protected
    function GetIgnoreParseErrors: Boolean; virtual;

    property IgnoreParseErrors: Boolean read GetIgnoreParseErrors;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions); override;
    destructor Destroy; override;
    procedure PushDestination(ADestination: TdxDestination);
    function PopDestination: TdxDestination;
    function PeekDestination: TdxDestination;

    procedure AddToGarbageCollector(AObject: TObject);
    procedure BeginCollectGarbage;
    procedure EndCollectGarbage;

    property DestinationStack: TdxDestinationStack read FDestinationStack;
  end;

  { TdxDestinationAndXmlBasedImporter }

  TdxDestinationAndXmlBasedImporter = class abstract(TdxDestinationBasedImporter)
  strict private
    class var
      FXmlCharDecodingRegex: TRegex;
    class constructor Initialize;
  strict private
    FBaseStream: TStream;
  protected
    function GetCreateEmptyDocumentOnLoadError: Boolean; virtual;
    function CreateXmlReaderSettings: TdxXmlReaderSettings; virtual;
    function ReadToRootElement(AReader: TdxXmlReader; const AName: string): Boolean; overload; virtual;
    function ReadToRootElement(AReader: TdxXmlReader; const AName: string; const ANs: string): Boolean; overload; virtual;
    procedure ImportContent(AReader: TdxXmlReader); overload;
    procedure ProcessCurrentDestination(AReader: TdxXmlReader); virtual;
    procedure ImportMainDocument(AReader: TdxXmlReader; ABaseStream: TStream); overload; virtual;
    procedure BeforeImportMainDocument; virtual;
    procedure BeginSetMainDocumentContent; virtual; abstract;
    procedure EndSetMainDocumentContent; virtual; abstract;
    procedure SetMainDocumentEmptyContent; virtual; abstract;
    procedure ImportMainDocument(AReader: TdxXmlReader); overload; virtual;
    procedure AfterImportMainDocument; virtual;
    function CreateMainDocumentDestination: TdxDestination; virtual; abstract;
    function GetWpEnumValueCore<T>(const AValue: string; ATable: TdxNamedOrdinalDictionary<T>; ADefaultValue: T): T; overload;

    property BaseStream: TStream read FBaseStream;
    property CreateEmptyDocumentOnLoadError: Boolean read GetCreateEmptyDocumentOnLoadError;
  public
    function CreateXmlReader(AStream: TStream): TdxXmlReader; virtual;
    function ConvertToBool(const AValue: string): Boolean; virtual; abstract;
    function GetWpSTOnOffValue(AReader: TdxXmlReader; const AAttributeName: string): Boolean; overload;
    function GetWpSTOnOffValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Boolean): Boolean; overload;

    function GetOnOffValue(AReader: TdxXmlReader; const AAttributeName: string): Boolean; overload;
    function GetOnOffValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Boolean): Boolean; overload;
    function GetOnOffValue(const AValue: string; ADefaultValue: Boolean): Boolean; overload;
    function GetWpSTOnOffNullValue(AReader: TdxXmlReader; const AAttributeName: string): TdxNullableValue<Boolean>;

    function GetWpSTIntegerValue(AReader: TdxXmlReader; const AAttributeName: string): Integer; overload;
    function GetWpSTIntegerValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Integer): Integer; overload;
    function GetWpSTIntegerValue(AReader: TdxXmlReader; const AAttributeName: string;
      ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Integer): Integer; overload;

    function GetIntegerValue(AReader: TdxXmlReader; const AAttributeName: string): Integer; overload;
    function GetIntegerValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Integer): Integer; overload;
    function GetIntegerValueInPoints(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Integer): Integer;
    function GetIntegerValue(AReader: TdxXmlReader; const AAttributeName: string;
      ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Integer): Integer; overload;
    function GetIntegerValue(const AValue: string; ANumberStyles: TdxNumberStyles.TStyles;
      ADefaultValue: Integer): Integer; overload;

    function GetFloatValueInPoints(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Single): Single;
    function GetFloatValue(const AValue: string; ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Single): Single;

    function GetWpSTFloatValue(AReader: TdxXmlReader; const AAttributeName: string;
      ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Single; const ANs: string): Single; overload;
    function GetWpSTFloatValue(AReader: TdxXmlReader; const AAttributeName: string;
      ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Single): Single; overload;
    function GetWpSTFloatValue(const AValue: string; ANumberStyles: TdxNumberStyles.TStyles;
      ADefaultValue: Single): Single; overload;

    function GetWpDoubleValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Double): Double; virtual;

    function GetLongValue(AReader: TdxXmlReader; const AAttributeName: string): Int64; overload;
    function GetLongValue(const AValue: string; ADefaultValue: Int64): Int64; overload;

    function GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TDictionary<T, string>; ADefaultValue: T): T; overload;
    function GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxEnumeratedDictionary<T, string>; ADefaultValue: T): T; overload;
    function GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TDictionary<T, string>; ADefaultValue: T; const ANs: string): T; overload;
    function GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxNamedOrdinalDictionary<T>; ADefaultValue: T): T; overload;

    function GetWpEnumOnOffNullValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxEnumeratedDictionary<T, string>): TdxNullableValue<T>;
    function GetWpEnumOnOffNullValueCore<T>(const AValue: string; ATable: TdxEnumeratedDictionary<T, string>): TdxNullableValue<T>;
    function GetWpEnumValueCore<T>(const AValue: string; ATable: TDictionary<T, string>; ADefaultValue: T): T; overload;
    function GetWpEnumValueCore<T>(const AValue: string; ATable: TdxEnumeratedDictionary<T, string>; ADefaultValue: T): T; overload;

    procedure ImportContent(AReader: TdxXmlReader; AInitialDestination: TdxDestination); overload; virtual;

    function ReadAttribute(AReader: TdxXmlReader; const AAttributeName: string): string; overload; virtual; abstract;
    function ReadAttribute(AReader: TdxXmlReader; const AAttributeName: string; const ANs: string): string; overload; virtual; abstract;

    function DecodeXmlChars(const AVal: string): string;
  end;

  { TdxRichEditDestinationAndXmlBasedImporter }

  TdxRichEditDestinationAndXmlBasedImporter = class abstract(TdxDestinationAndXmlBasedImporter)
  strict private
    FCurrentSection: TdxSection;
    FDefaultCharacterStyleProcessed: Boolean;
    FDefaultParagraphStyleProcessed: Boolean;
    FDefaultTableStyleProcessed: Boolean;
    FDefaultTableCellStyleProcessed: Boolean;
    FPieceTableInfos: TdxObjectStack<TdxImportPieceTableInfo>;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTableInfo: TdxImportPieceTableInfo;
    function GetPieceTable: TdxPieceTable;
    function GetPosition: TdxImportInputPosition;
    function GetFieldInfoStack: TdxObjectStack<TdxImportFieldInfo>;
    function GetBookmarks: TdxImportBookmarkInfos;
    function GetRangePermissions: TdxImportRangePermissionInfos;
    function GetOptions: TdxXmlBasedDocumentImporterOptions;
    function GetTableStack: TdxObjectStack<TdxTable>;
  protected
    function GetIgnoreParseErrors: Boolean; override;
    function GetWordProcessingNamespaceConst: string; virtual; abstract;
    procedure BeforeImportMainDocument; override;
    procedure AfterImportMainDocument; override;
    procedure BeginSetMainDocumentContent; override;
    procedure EndSetMainDocumentContent; override;
    procedure SetMainDocumentEmptyContent; override;

    property PieceTableInfos: TdxObjectStack<TdxImportPieceTableInfo> read FPieceTableInfos;
    property PieceTableInfo: TdxImportPieceTableInfo read GetPieceTableInfo;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions); override;
    destructor Destroy; override;

    procedure InsertBookmarks; virtual;
    procedure InsertRangePermissions;
    procedure InsertComments;

    procedure PushCurrentPieceTable(APieceTable: TdxSimplePieceTable); virtual;
    function PopCurrentPieceTable: TdxPieceTable; virtual;
    function GetWordProcessingMLValue(AValue: TdxWordProcessingMLValue): string; virtual; abstract;
    function LookupCharacterStyleIndex(const AStyleId: string): Integer; virtual; abstract;
    procedure ResetPositionCharacterProperties; virtual;

    property Bookmarks: TdxImportBookmarkInfos read GetBookmarks;
    property CurrentSection: TdxSection read FCurrentSection write FCurrentSection;
    property DefaultCharacterStyleProcessed: Boolean read FDefaultCharacterStyleProcessed write FDefaultCharacterStyleProcessed;
    property DefaultParagraphStyleProcessed: Boolean read FDefaultParagraphStyleProcessed write FDefaultParagraphStyleProcessed;
    property DefaultTableCellStyleProcessed: Boolean read FDefaultTableCellStyleProcessed write FDefaultTableCellStyleProcessed;
    property DefaultTableStyleProcessed: Boolean read FDefaultTableStyleProcessed write FDefaultTableStyleProcessed;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property FieldInfoStack: TdxObjectStack<TdxImportFieldInfo> read GetFieldInfoStack;
    property InnerOptions: TdxXmlBasedDocumentImporterOptions read GetOptions;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property Position: TdxImportInputPosition read GetPosition;
    property RangePermissions: TdxImportRangePermissionInfos read GetRangePermissions;
    property TableStack: TdxObjectStack<TdxTable> read GetTableStack;
    property WordProcessingNamespaceConst: string read GetWordProcessingNamespaceConst;
  end;


  { TdxDestination }

  TdxDestination = class abstract
  strict private
    FRefCount: Integer;
    FImporter: TdxRichEditDestinationAndXmlBasedImporter;
  protected
    procedure AddReference;
    procedure RemoveReference;

    function GetImporter: TdxRichEditDestinationAndXmlBasedImporter; virtual;
    function ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination; virtual; abstract;
    function ProcessCore(AReader: TdxXmlReader): Boolean; virtual;
    function Peek: TdxDestination; virtual;
    function ShouldProcessWhitespaces(AReader: TdxXmlReader): Boolean; virtual;

    property Importer: TdxRichEditDestinationAndXmlBasedImporter read GetImporter;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    procedure Release;
    procedure ProcessElementOpen(AReader: TdxXmlReader); virtual; abstract;
    function ProcessText(AReader: TdxXmlReader): Boolean; virtual; abstract;
    procedure ProcessElementClose(AReader: TdxXmlReader); virtual; abstract;
    function Process(AReader: TdxXmlReader): Boolean;
  end;

  { TdxDestinationStack }

  TdxDestinationStack = class(TdxObjectStack<TdxDestination>)
  public
    constructor Create; reintroduce;
    procedure Push(ADestination: TdxDestination);
    function Pop: TdxDestination;
  end;

  { TdxTransparentDestination }

  TdxTransparentDestination = class(TdxDestination)
  strict private
    FDestination: TdxDestination;
  protected
    function ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination; override;
    function Peek: TdxDestination; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

  { TdxStructuredDocumentDestination }

  TdxStructuredDocumentDestination = class(TdxTransparentDestination)
  protected
    function ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination; override;
  end;

  { TdxStructuredDocumentContentDestination }

  TdxStructuredDocumentContentDestination = class(TdxTransparentDestination)
  protected
    function ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination; override;
  end;

  { TdxCustomXmlDestination }

  TdxCustomXmlDestination = class(TdxTransparentDestination);


  TdxElementHandler = function(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;

  { TdxElementHandlerTable }

  TdxElementHandlerTable = class(TdxNamedOrdinalDictionary<TdxElementHandler>)
  strict private
    class var
      FEmpty: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  public
    class property Empty: TdxElementHandlerTable read FEmpty;
  end;

  { TdxElementDestination }

  TdxElementDestination = class abstract(TdxDestination)
  strict private
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; virtual; abstract;
    function ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination; override;
    function OnAlternateContent: TdxDestination;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property ElementHandlerTable: TdxElementHandlerTable read GetElementHandlerTable;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
    function IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean; virtual;
  end;

  { TdxAlternateContentDestination }

  TdxAlternateContentDestination = class(TdxElementDestination)
  strict private
    FParentDestination: TdxElementDestination;
    FHasProcessedChoice: Boolean;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParentDestination: TdxElementDestination);
  end;

  { TdxLeafElementDestination }

  TdxLeafElementDestination = class abstract(TdxElementDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxEmptyDestination }

  TdxEmptyDestination = class(TdxLeafElementDestination);

  { TdxRangePermissionElementDestination }

  TdxRangePermissionElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    class var
      FActualGroupNames: TdxStringsDictionary;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateActualGroupNames: TdxStringsDictionary; static;
  protected
    procedure AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo); virtual; abstract;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    function GetActualGroupName(const AValue: string): string;
  end;

  { TdxRangePermissionStartElementDestination }

  TdxRangePermissionStartElementDestination = class(TdxRangePermissionElementDestination)
  protected
    procedure AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo); override;
  end;

  { TdxRangePermissionEndElementDestination }

  TdxRangePermissionEndElementDestination = class(TdxRangePermissionElementDestination)
  protected
    procedure AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo); override;
  end;

  { TdxBorderDestination }

  TdxBorderDestination = class(TdxLeafElementDestination)
  strict private
    class var
      FBorderStyleTable: TdxEnumeratedDictionary<TdxBorderLineStyle, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateBorderStyleTable: TdxEnumeratedDictionary<TdxBorderLineStyle, string>; static;
  strict protected
    class property BorderStyleTable: TdxEnumeratedDictionary<TdxBorderLineStyle, string> read FBorderStyleTable;
  end;

  { TdxOpenXmlStyleInfo }

  TdxOpenXmlStyleInfo = class
  strict private
    FStyleId: string;
    FParentStyleId: string;
    FStyleIndex: Integer;
    FLinkedStyleId: string;
    FNextStyleId: string;
    FNumberingId: Integer;
    FListLevelIndex: Integer;
  public
    constructor Create;

    property StyleId: string read FStyleId write FStyleId;
    property StyleIndex: Integer read FStyleIndex write FStyleIndex;
    property LinkedStyleId: string read FLinkedStyleId write FLinkedStyleId;
    property NextStyleId: string read FNextStyleId write FNextStyleId;
    property ParentStyleId: string read FParentStyleId write FParentStyleId;

    property NumberingId: Integer read FNumberingId write FNumberingId;
    property ListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
  end;

  { TdxOpenXmlStyleInfoCollection }

  TdxOpenXmlStyleInfoCollection = class(TdxObjectList<TdxOpenXmlStyleInfo>)
  public
    function LookupStyleById(const AId: string): TdxOpenXmlStyleInfo;
  end;

implementation

uses
  Contnrs, RTLConsts,
  dxZIPUtils,
  dxRichEdit.Strs,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper,
  dxUriRecord,
  dxRichEdit.Options.Simple;

{ TdxDestination }

constructor TdxDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create;
  Assert(AImporter <> nil);
  FImporter := AImporter;
end;

procedure TdxDestination.AddReference;
begin
  Inc(FRefCount);
end;

function TdxDestination.GetImporter: TdxRichEditDestinationAndXmlBasedImporter;
begin
  Result := FImporter;
end;

function TdxDestination.Process(AReader: TdxXmlReader): Boolean;
begin
  while True do
  begin
    if ProcessCore(AReader) then
      Exit(True);
    if AReader.ReadState = TdxXmlReadState.EndOfFile then
      Exit(False);
  end;
end;

function TdxDestination.ProcessCore(AReader: TdxXmlReader): Boolean;
var
  ADestination, ANextDestination: TdxDestination;
begin
  if AReader.NodeType = TdxXmlNodeType.EndElement then
  begin
    ADestination := Importer.PopDestination;
    try
      ADestination.ProcessElementClose(AReader);
    finally
      ADestination.Release;
    end;
    Exit(True);
  end;
  if ShouldProcessWhitespaces(AReader) then
    Exit(Importer.PeekDestination.ProcessText(AReader))
  else
    if AReader.NodeType in [TdxXmlNodeType.Text, TdxXmlNodeType.SignificantWhitespace, TdxXmlNodeType.CDATA] then
      Exit(Importer.PeekDestination.ProcessText(AReader));

  ANextDestination := ProcessCurrentElement(AReader);
  if ANextDestination = nil then
  begin
    AReader.Skip;
    Exit(False);
  end;
  if AReader.NodeType = TdxXmlNodeType.Element then
  begin
    if AReader.IsEmptyElement then
    try
      ANextDestination.ProcessElementOpen(AReader);
      ANextDestination.ProcessElementClose(AReader);
    finally
      ANextDestination.Free;
    end
    else
    begin
      Importer.PushDestination(ANextDestination);
      ANextDestination.ProcessElementOpen(AReader);
    end;
  end;
  Result := True;
end;

procedure TdxDestination.Release;
begin
  if (Self <> nil) and (FRefCount <= 0) then
    Destroy;
end;

procedure TdxDestination.RemoveReference;
begin
  Dec(FRefCount);
end;

function TdxDestination.Peek: TdxDestination;
begin
  Result := Self;
end;

function TdxDestination.ShouldProcessWhitespaces(AReader: TdxXmlReader): Boolean;
begin
  Result := False;
end;

{ TdxDestinationStack }

constructor TdxDestinationStack.Create;
begin
  inherited Create(False);
end;

function TdxDestinationStack.Pop: TdxDestination;
begin
  Result := TdxDestination(inherited PopItem);
  Result.RemoveReference;
end;

procedure TdxDestinationStack.Push(ADestination: TdxDestination);
begin
  inherited Push(ADestination);
  ADestination.AddReference;
end;

{ TdxTransparentDestination }

constructor TdxTransparentDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FDestination := AImporter.PeekDestination;
  Assert(FDestination <> nil, 'destination');
end;

procedure TdxTransparentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;

procedure TdxTransparentDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
end;

function TdxTransparentDestination.ProcessText(AReader: TdxXmlReader): Boolean;
begin
  Result := FDestination.ProcessText(AReader);
end;

function TdxTransparentDestination.ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination;
begin
  Result := FDestination.ProcessCurrentElement(AReader);
end;

function TdxTransparentDestination.Peek: TdxDestination;
begin
  Result := FDestination.Peek;
end;

{ TdxStructuredDocumentDestination }

function TdxStructuredDocumentDestination.ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination;
var
  ALocalName: string;
begin
  ALocalName := AReader.LocalName;
  if ALocalName = 'sdtContent' then
    Result := TdxStructuredDocumentContentDestination.Create(Importer)
  else
    Result := inherited ProcessCurrentElement(AReader);
end;

{ TdxStructuredDocumentContentDestination }

function TdxStructuredDocumentContentDestination.ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination;
var
  ALocalName: string;
begin
  ALocalName := AReader.LocalName;
  if ALocalName = 'sdt' then
    Result := TdxStructuredDocumentDestination.Create(Importer)
  else
    Result := inherited ProcessCurrentElement(AReader);
end;



{ TdxElementHandlerTable }

class constructor TdxElementHandlerTable.Initialize;
begin
  FEmpty := TdxElementHandlerTable.Create;
end;

class destructor TdxElementHandlerTable.Finalize;
begin
  FEmpty.Free;
end;

{ TdxElementDestination }

function TdxElementDestination.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(Importer.DocumentModel);
end;

function TdxElementDestination.GetPieceTable: TdxPieceTable;
begin
  Result := Importer.PieceTable;
end;

function TdxElementDestination.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Importer.UnitConverter;
end;

procedure TdxElementDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;

procedure TdxElementDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
end;

function TdxElementDestination.ProcessText(AReader: TdxXmlReader): Boolean;
begin
  Result := True;
end;

function TdxElementDestination.ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination;
var
  ALocalName: string;
  AHandler: TdxElementHandler;
begin
  ALocalName := AReader.LocalName;
  if ALocalName = 'AlternateContent' then
    Exit(OnAlternateContent);

  if ElementHandlerTable.TryGetValue(ALocalName, AHandler) then
    Result := AHandler(Importer, AReader)
  else
    Result := nil;
end;

function TdxElementDestination.OnAlternateContent: TdxDestination;
begin
  Result := TdxAlternateContentDestination.Create(Importer, Self);
end;

function TdxElementDestination.IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean;
begin
  Result := False;
end;

{ TdxAlternateContentDestination }

constructor TdxAlternateContentDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AParentDestination: TdxElementDestination);
begin
  inherited Create(AImporter);
  FParentDestination := AParentDestination;
end;

function TdxAlternateContentDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

function TdxAlternateContentDestination.ProcessCurrentElement(AReader: TdxXmlReader): TdxDestination;
var
  ALocalName, ARequeries, ANamespaceUri: string;
begin
  ALocalName := AReader.LocalName;
  if (ALocalName = 'Choice') and not FHasProcessedChoice then
  begin
    ARequeries := AReader.GetAttribute('Requires');
    ANamespaceUri := AReader.LookupNamespace(ARequeries);
    if FParentDestination.IsChoiceNamespaceSupported(ANamespaceUri) then
    begin
      FHasProcessedChoice := True;
      Exit(FParentDestination);
    end;
  end
  else
    if (ALocalName = 'Fallback') and not FHasProcessedChoice then
      Exit(FParentDestination);

  Result := TdxEmptyDestination.Create(Importer);
end;

{ TdxLeafElementDestination }

function TdxLeafElementDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Empty;
end;

{ TdxRangePermissionElementDestination }

class constructor TdxRangePermissionElementDestination.Initialize;
begin
  FActualGroupNames := CreateActualGroupNames;
end;

class destructor TdxRangePermissionElementDestination.Finalize;
begin
  FreeAndNil(FActualGroupNames);
end;

class function TdxRangePermissionElementDestination.CreateActualGroupNames: TdxStringsDictionary;
var
  AKeys: TArray<string>;
  AKey: string;
begin
  Result := TdxStringsDictionary.Create;
  AKeys := TdxWordProcessingMLBaseExporter.PredefinedGroupNames.Keys.ToArray;
  for AKey in AKeys do
    Result.Add(TdxWordProcessingMLBaseExporter.PredefinedGroupNames[AKey], AKey);
end;

procedure TdxRangePermissionElementDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AId, AValue: string;
  ARangePermission: TdxImportRangePermissionInfo;
begin
  AId := AReader.GetAttribute('id', Importer.WordProcessingNamespaceConst);
  AId := Trim(AId);
  if AId = '' then
    Exit;

  if not Importer.RangePermissions.TryGetValue(AId, ARangePermission) then
  begin
    ARangePermission := TdxImportRangePermissionInfo.Create;
    Importer.RangePermissions.Add(AId, ARangePermission);
  end;
  AValue := AReader.GetAttribute('ed', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
  begin
    AValue := Trim(AValue);
    ARangePermission.PermissionInfo.UserName := AValue;
  end;
  AValue := AReader.GetAttribute('edGrp', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
  begin
    AValue := Trim(AValue);
    ARangePermission.PermissionInfo.Group := GetActualGroupName(AValue);
  end;
  AssignRangePermissionPosition(ARangePermission);
end;

function TdxRangePermissionElementDestination.GetActualGroupName(const AValue: string): string;
begin
  if not FActualGroupNames.TryGetValue(AValue, Result) then
    Result := AValue;
end;

{ TdxRangePermissionStartElementDestination }

procedure TdxRangePermissionStartElementDestination.AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo);
begin
  ARangePermission.Start := Importer.Position.LogPosition;
end;

{ TdxRangePermissionEndElementDestination }

procedure TdxRangePermissionEndElementDestination.AssignRangePermissionPosition(ARangePermission: TdxImportRangePermissionInfo);
begin
  ARangePermission.&End := Importer.Position.LogPosition;
end;

{ TdxBorderDestination }

class constructor TdxBorderDestination.Initialize;
begin
  FBorderStyleTable := CreateBorderStyleTable;
end;

class destructor TdxBorderDestination.Finalize;
begin
  FBorderStyleTable.Free;
end;

class function TdxBorderDestination.CreateBorderStyleTable: TdxEnumeratedDictionary<TdxBorderLineStyle, string>;
begin
  Result := TdxEnumeratedDictionary<TdxBorderLineStyle, string>.Create;
  Result.Add(TdxBorderLineStyle.DashDotStroked, 'dashDotStroked');
  Result.Add(TdxBorderLineStyle.Dashed, 'dashed');
  Result.Add(TdxBorderLineStyle.DashSmallGap, 'dashSmallGap');
  Result.Add(TdxBorderLineStyle.DotDash, 'dotDash');
  Result.Add(TdxBorderLineStyle.DotDotDash, 'dotDotDash');
  Result.Add(TdxBorderLineStyle.Dotted, 'dotted');
  Result.Add(TdxBorderLineStyle.Double, 'double');
  Result.Add(TdxBorderLineStyle.DoubleWave, 'doubleWave');
  Result.Add(TdxBorderLineStyle.Inset, 'inset');
  Result.Add(TdxBorderLineStyle.Disabled, 'disabled');
  Result.Add(TdxBorderLineStyle.None, 'none');
  Result.Add(TdxBorderLineStyle.Nil, 'nil');
  Result.Add(TdxBorderLineStyle.Outset, 'outset');
  Result.Add(TdxBorderLineStyle.Single, 'single');
  Result.Add(TdxBorderLineStyle.Thick, 'thick');
  Result.Add(TdxBorderLineStyle.ThickThinLargeGap, 'thickThinLargeGap');
  Result.Add(TdxBorderLineStyle.ThickThinMediumGap, 'thickThinMediumGap');
  Result.Add(TdxBorderLineStyle.ThickThinSmallGap, 'thickThinSmallGap');
  Result.Add(TdxBorderLineStyle.ThinThickLargeGap, 'thinThickLargeGap');
  Result.Add(TdxBorderLineStyle.ThinThickMediumGap, 'thinThickMediumGap');
  Result.Add(TdxBorderLineStyle.ThinThickSmallGap, 'thinThickSmallGap');
  Result.Add(TdxBorderLineStyle.ThinThickThinLargeGap, 'thinThickThinLargeGap');
  Result.Add(TdxBorderLineStyle.ThinThickThinMediumGap, 'thinThickThinMediumGap');
  Result.Add(TdxBorderLineStyle.ThinThickThinSmallGap, 'thinThickThinSmallGap');
  Result.Add(TdxBorderLineStyle.ThreeDEmboss, 'threeDEmboss');
  Result.Add(TdxBorderLineStyle.ThreeDEngrave, 'threeDEngrave');
  Result.Add(TdxBorderLineStyle.Triple, 'triple');
  Result.Add(TdxBorderLineStyle.Wave, 'wave');

  Result.Add(TdxBorderLineStyle.Apples, 'apples');
  Result.Add(TdxBorderLineStyle.ArchedScallops, 'archedScallops');
  Result.Add(TdxBorderLineStyle.BabyPacifier, 'babyPacifier');
  Result.Add(TdxBorderLineStyle.BabyRattle, 'babyRattle');
  Result.Add(TdxBorderLineStyle.Balloons3Colors, 'balloons3Colors');
  Result.Add(TdxBorderLineStyle.BalloonsHotAir, 'balloonsHotAir');
  Result.Add(TdxBorderLineStyle.BasicBlackDashes, 'basicBlackDashes');
  Result.Add(TdxBorderLineStyle.BasicBlackDots, 'basicBlackDots');
  Result.Add(TdxBorderLineStyle.BasicBlackSquares, 'basicBlackSquares');
  Result.Add(TdxBorderLineStyle.BasicThinLines, 'basicThinLines');
  Result.Add(TdxBorderLineStyle.BasicWhiteDashes, 'basicWhiteDashes');
  Result.Add(TdxBorderLineStyle.BasicWhiteDots, 'basicWhiteDots');
  Result.Add(TdxBorderLineStyle.BasicWhiteSquares, 'basicWhiteSquares');
  Result.Add(TdxBorderLineStyle.BasicWideInline, 'basicWideInline');
  Result.Add(TdxBorderLineStyle.BasicWideMidline, 'basicWideMidline');
  Result.Add(TdxBorderLineStyle.BasicWideOutline, 'basicWideOutline');
  Result.Add(TdxBorderLineStyle.Bats, 'bats');
  Result.Add(TdxBorderLineStyle.Birds, 'birds');
  Result.Add(TdxBorderLineStyle.BirdsFlight, 'birdsFlight');
  Result.Add(TdxBorderLineStyle.Cabins, 'cabins');
  Result.Add(TdxBorderLineStyle.CakeSlice, 'cakeSlice');
  Result.Add(TdxBorderLineStyle.CandyCorn, 'candyCorn');
  Result.Add(TdxBorderLineStyle.CelticKnotwork, 'celticKnotwork');
  Result.Add(TdxBorderLineStyle.CertificateBanner, 'certificateBanner');
  Result.Add(TdxBorderLineStyle.ChainLink, 'chainLink');
  Result.Add(TdxBorderLineStyle.ChampagneBottle, 'champagneBottle');
  Result.Add(TdxBorderLineStyle.CheckedBarBlack, 'checkedBarBlack');
  Result.Add(TdxBorderLineStyle.CheckedBarColor, 'checkedBarColor');
  Result.Add(TdxBorderLineStyle.Checkered, 'checkered');
  Result.Add(TdxBorderLineStyle.ChristmasTree, 'christmasTree');
  Result.Add(TdxBorderLineStyle.CirclesLines, 'circlesLines');
  Result.Add(TdxBorderLineStyle.CirclesRectangles, 'circlesRectangles');
  Result.Add(TdxBorderLineStyle.ClassicalWave, 'classicalWave');
  Result.Add(TdxBorderLineStyle.Clocks, 'clocks');
  Result.Add(TdxBorderLineStyle.Compass, 'compass');
  Result.Add(TdxBorderLineStyle.Confetti, 'confetti');
  Result.Add(TdxBorderLineStyle.ConfettiGrays, 'confettiGrays');
  Result.Add(TdxBorderLineStyle.ConfettiOutline, 'confettiOutline');
  Result.Add(TdxBorderLineStyle.ConfettiStreamers, 'confettiStreamers');
  Result.Add(TdxBorderLineStyle.ConfettiWhite, 'confettiWhite');
  Result.Add(TdxBorderLineStyle.CornerTriangles, 'cornerTriangles');
  Result.Add(TdxBorderLineStyle.CouponCutoutDashes, 'couponCutoutDashes');
  Result.Add(TdxBorderLineStyle.CouponCutoutDots, 'couponCutoutDots');
  Result.Add(TdxBorderLineStyle.CrazyMaze, 'crazyMaze');
  Result.Add(TdxBorderLineStyle.CreaturesButterfly, 'creaturesButterfly');
  Result.Add(TdxBorderLineStyle.CreaturesFish, 'creaturesFish');
  Result.Add(TdxBorderLineStyle.CreaturesInsects, 'creaturesInsects');
  Result.Add(TdxBorderLineStyle.CreaturesLadyBug, 'creaturesLadyBug');
  Result.Add(TdxBorderLineStyle.CrossStitch, 'crossStitch');
  Result.Add(TdxBorderLineStyle.Cup, 'cup');
  Result.Add(TdxBorderLineStyle.DecoArch, 'decoArch');
  Result.Add(TdxBorderLineStyle.DecoArchColor, 'decoArchColor');
  Result.Add(TdxBorderLineStyle.DecoBlocks, 'decoBlocks');
  Result.Add(TdxBorderLineStyle.DiamondsGray, 'diamondsGray');
  Result.Add(TdxBorderLineStyle.DoubleD, 'doubleD');
  Result.Add(TdxBorderLineStyle.DoubleDiamonds, 'doubleDiamonds');
  Result.Add(TdxBorderLineStyle.Earth1, 'earth1');
  Result.Add(TdxBorderLineStyle.Earth2, 'earth2');
  Result.Add(TdxBorderLineStyle.EclipsingSquares1, 'eclipsingSquares1');
  Result.Add(TdxBorderLineStyle.EclipsingSquares2, 'eclipsingSquares2');
  Result.Add(TdxBorderLineStyle.EggsBlack, 'eggsBlack');
  Result.Add(TdxBorderLineStyle.Fans, 'fans');
  Result.Add(TdxBorderLineStyle.Film, 'film');
  Result.Add(TdxBorderLineStyle.Firecrackers, 'firecrackers');
  Result.Add(TdxBorderLineStyle.FlowersBlockPrint, 'flowersBlockPrint');
  Result.Add(TdxBorderLineStyle.FlowersDaisies, 'flowersDaisies');
  Result.Add(TdxBorderLineStyle.FlowersModern1, 'flowersModern1');
  Result.Add(TdxBorderLineStyle.FlowersModern2, 'flowersModern2');
  Result.Add(TdxBorderLineStyle.FlowersPansy, 'flowersPansy');
  Result.Add(TdxBorderLineStyle.FlowersRedRose, 'flowersRedRose');
  Result.Add(TdxBorderLineStyle.FlowersRoses, 'flowersRoses');
  Result.Add(TdxBorderLineStyle.FlowersTeacup, 'flowersTeacup');
  Result.Add(TdxBorderLineStyle.FlowersTiny, 'flowersTiny');
  Result.Add(TdxBorderLineStyle.Gems, 'gems');
  Result.Add(TdxBorderLineStyle.GingerbreadMan, 'gingerbreadMan');
  Result.Add(TdxBorderLineStyle.Gradient, 'gradient');
  Result.Add(TdxBorderLineStyle.Handmade1, 'handmade1');
  Result.Add(TdxBorderLineStyle.Handmade2, 'handmade2');
  Result.Add(TdxBorderLineStyle.HeartBalloon, 'heartBalloon');
  Result.Add(TdxBorderLineStyle.HeartGray, 'heartGray');
  Result.Add(TdxBorderLineStyle.Hearts, 'hearts');
  Result.Add(TdxBorderLineStyle.HeebieJeebies, 'heebieJeebies');
  Result.Add(TdxBorderLineStyle.Holly, 'holly');
  Result.Add(TdxBorderLineStyle.HouseFunky, 'houseFunky');
  Result.Add(TdxBorderLineStyle.Hypnotic, 'hypnotic');
  Result.Add(TdxBorderLineStyle.IceCreamCones, 'iceCreamCones');
  Result.Add(TdxBorderLineStyle.LightBulb, 'lightBulb');
  Result.Add(TdxBorderLineStyle.Lightning1, 'lightning1');
  Result.Add(TdxBorderLineStyle.Lightning2, 'lightning2');
  Result.Add(TdxBorderLineStyle.MapleLeaf, 'mapleLeaf');
  Result.Add(TdxBorderLineStyle.MapleMuffins, 'mapleMuffins');
  Result.Add(TdxBorderLineStyle.MapPins, 'mapPins');
  Result.Add(TdxBorderLineStyle.Marquee, 'marquee');
  Result.Add(TdxBorderLineStyle.MarqueeToothed, 'marqueeToothed');
  Result.Add(TdxBorderLineStyle.Moons, 'moons');
  Result.Add(TdxBorderLineStyle.Mosaic, 'mosaic');
  Result.Add(TdxBorderLineStyle.MusicNotes, 'musicNotes');
  Result.Add(TdxBorderLineStyle.Northwest, 'northwest');
  Result.Add(TdxBorderLineStyle.Ovals, 'ovals');
  Result.Add(TdxBorderLineStyle.Packages, 'packages');
  Result.Add(TdxBorderLineStyle.PalmsBlack, 'palmsBlack');
  Result.Add(TdxBorderLineStyle.PalmsColor, 'palmsColor');
  Result.Add(TdxBorderLineStyle.PaperClips, 'paperClips');
  Result.Add(TdxBorderLineStyle.Papyrus, 'papyrus');
  Result.Add(TdxBorderLineStyle.PartyFavor, 'partyFavor');
  Result.Add(TdxBorderLineStyle.PartyGlass, 'partyGlass');
  Result.Add(TdxBorderLineStyle.Pencils, 'pencils');
  Result.Add(TdxBorderLineStyle.People, 'people');
  Result.Add(TdxBorderLineStyle.PeopleHats, 'peopleHats');
  Result.Add(TdxBorderLineStyle.PeopleWaving, 'peopleWaving');
  Result.Add(TdxBorderLineStyle.Poinsettias, 'poinsettias');
  Result.Add(TdxBorderLineStyle.PostageStamp, 'postageStamp');
  Result.Add(TdxBorderLineStyle.Pumpkin1, 'pumpkin1');
  Result.Add(TdxBorderLineStyle.PushPinNote1, 'pushPinNote1');
  Result.Add(TdxBorderLineStyle.PushPinNote2, 'pushPinNote2');
  Result.Add(TdxBorderLineStyle.Pyramids, 'pyramids');
  Result.Add(TdxBorderLineStyle.PyramidsAbove, 'pyramidsAbove');
  Result.Add(TdxBorderLineStyle.Quadrants, 'quadrants');
  Result.Add(TdxBorderLineStyle.Rings, 'rings');
  Result.Add(TdxBorderLineStyle.Safari, 'safari');
  Result.Add(TdxBorderLineStyle.Sawtooth, 'sawtooth');
  Result.Add(TdxBorderLineStyle.SawtoothGray, 'sawtoothGray');
  Result.Add(TdxBorderLineStyle.ScaredCat, 'scaredCat');
  Result.Add(TdxBorderLineStyle.Seattle, 'seattle');
  Result.Add(TdxBorderLineStyle.ShadowedSquares, 'shadowedSquares');
  Result.Add(TdxBorderLineStyle.SharksTeeth, 'sharksTeeth');
  Result.Add(TdxBorderLineStyle.ShorebirdTracks, 'shorebirdTracks');
  Result.Add(TdxBorderLineStyle.Skyrocket, 'skyrocket');
  Result.Add(TdxBorderLineStyle.SnowflakeFancy, 'snowflakeFancy');
  Result.Add(TdxBorderLineStyle.Snowflakes, 'snowflakes');
  Result.Add(TdxBorderLineStyle.Sombrero, 'sombrero');
  Result.Add(TdxBorderLineStyle.Southwest, 'southwest');
  Result.Add(TdxBorderLineStyle.Stars, 'stars');
  Result.Add(TdxBorderLineStyle.Stars3d, 'stars3d');
  Result.Add(TdxBorderLineStyle.StarsBlack, 'starsBlack');
  Result.Add(TdxBorderLineStyle.StarsShadowed, 'starsShadowed');
  Result.Add(TdxBorderLineStyle.StarsTop, 'starsTop');
  Result.Add(TdxBorderLineStyle.Sun, 'sun');
  Result.Add(TdxBorderLineStyle.Swirligig, 'swirligig');
  Result.Add(TdxBorderLineStyle.TornPaper, 'tornPaper');
  Result.Add(TdxBorderLineStyle.TornPaperBlack, 'tornPaperBlack');
  Result.Add(TdxBorderLineStyle.Trees, 'trees');
  Result.Add(TdxBorderLineStyle.TriangleParty, 'triangleParty');
  Result.Add(TdxBorderLineStyle.Triangles, 'triangles');
  Result.Add(TdxBorderLineStyle.Tribal1, 'tribal1');
  Result.Add(TdxBorderLineStyle.Tribal2, 'tribal2');
  Result.Add(TdxBorderLineStyle.Tribal3, 'tribal3');
  Result.Add(TdxBorderLineStyle.Tribal4, 'tribal4');
  Result.Add(TdxBorderLineStyle.Tribal5, 'tribal5');
  Result.Add(TdxBorderLineStyle.Tribal6, 'tribal6');
  Result.Add(TdxBorderLineStyle.TwistedLines1, 'twistedLines1');
  Result.Add(TdxBorderLineStyle.TwistedLines2, 'twistedLines2');
  Result.Add(TdxBorderLineStyle.Vine, 'vine');
  Result.Add(TdxBorderLineStyle.Waveline, 'waveline');
  Result.Add(TdxBorderLineStyle.WeavingAngles, 'weavingAngles');
  Result.Add(TdxBorderLineStyle.WeavingBraid, 'weavingBraid');
  Result.Add(TdxBorderLineStyle.WeavingRibbon, 'weavingRibbon');
  Result.Add(TdxBorderLineStyle.WeavingStrips, 'weavingStrips');
  Result.Add(TdxBorderLineStyle.WhiteFlowers, 'whiteFlowers');
  Result.Add(TdxBorderLineStyle.Woodwork, 'woodwork');
  Result.Add(TdxBorderLineStyle.XIllusions, 'xIllusions');
  Result.Add(TdxBorderLineStyle.ZanyTriangles, 'zanyTriangles');
  Result.Add(TdxBorderLineStyle.ZigZag, 'zigZag');
  Result.Add(TdxBorderLineStyle.ZigZagStitch, 'zigZagStitch');
end;

{ TdxDestinationBasedImporter }

constructor TdxDestinationBasedImporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FDestinationStack := TdxDestinationStack.Create;
  FObjectsToDelete  := TdxFastObjectList.Create(True, 32);
end;

destructor TdxDestinationBasedImporter.Destroy;
begin
  FObjectsToDelete.Free;
  FDestinationStack.Free;
  inherited Destroy;
end;

procedure TdxDestinationBasedImporter.AddToGarbageCollector(AObject: TObject);
begin
  FObjectsToDelete.Add(AObject);
end;

procedure TdxDestinationBasedImporter.BeginCollectGarbage;
begin
end;

procedure TdxDestinationBasedImporter.EndCollectGarbage;
begin
  FObjectsToDelete.Clear;
end;

function TdxDestinationBasedImporter.GetIgnoreParseErrors: Boolean;
begin
  Result := False;
end;

procedure TdxDestinationBasedImporter.PushDestination(ADestination: TdxDestination);
begin
  DestinationStack.Push(ADestination);
end;

function TdxDestinationBasedImporter.PopDestination: TdxDestination;
begin
  Result := DestinationStack.Pop;
end;

function TdxDestinationBasedImporter.PeekDestination: TdxDestination;
var
  ADestination: TdxDestination;
begin
  ADestination := DestinationStack.Peek;
  if ADestination <> nil then
    Result := ADestination.Peek
  else
    Result := ADestination;
end;

{ TdxDestinationAndXmlBasedImporter }

class constructor TdxDestinationAndXmlBasedImporter.Initialize;
begin
  FXmlCharDecodingRegex := TRegex.Create('_x(?<value>([\da-fA-F]){4})_');
end;

function TdxDestinationAndXmlBasedImporter.GetCreateEmptyDocumentOnLoadError: Boolean;
begin
  Result := True;
end;

function TdxDestinationAndXmlBasedImporter.CreateXmlReader(AStream: TStream): TdxXmlReader;
var
  AXmlReaderSettings: TdxXmlReaderSettings;
begin
  AXmlReaderSettings := CreateXmlReaderSettings;
  try
    Result := AXmlReaderSettings.CreateReader(AStream);
  finally
    AXmlReaderSettings.Free;
  end;
end;

function TdxDestinationAndXmlBasedImporter.CreateXmlReaderSettings: TdxXmlReaderSettings;
begin
  Result := TdxXmlReaderSettings.Create;
  Result.IgnoreComments := True;
  Result.IgnoreWhitespace := True;
end;

function TdxDestinationAndXmlBasedImporter.ReadToRootElement(AReader: TdxXmlReader; const AName: string): Boolean;
begin
  try
    Result := AReader.ReadToFollowing(AName);
  except
    Result := False;
  end;
end;

function TdxDestinationAndXmlBasedImporter.ReadToRootElement(AReader: TdxXmlReader; const AName: string; const ANs: string): Boolean;
begin
  try
    Result := AReader.ReadToFollowing(AName, ANs);
  except
    Result := False;
  end;
end;

procedure TdxDestinationAndXmlBasedImporter.ImportContent(AReader: TdxXmlReader; AInitialDestination: TdxDestination);
var
  ADestinationCount: Integer;
  ADestination: TdxDestination;
begin
  ADestinationCount := DestinationStack.Count;
  PushDestination(AInitialDestination);
  try
    ImportContent(AReader);
  finally
    while DestinationStack.Count > ADestinationCount do
    begin
      ADestination := PopDestination;
      ADestination.Release;
    end;
  end;
  Assert(ADestinationCount = DestinationStack.Count);
end;

procedure TdxDestinationAndXmlBasedImporter.ImportContent(AReader: TdxXmlReader);
begin
  while True do
  begin
    try
      AReader.Read;
    except
    end;
    if AReader.ReadState in [TdxXmlReadState.EndOfFile, TdxXmlReadState.Error] then
      Break;
    if BaseStream <> nil then
      ProgressIndication.SetProgress(Integer(BaseStream.Position));

    ProcessCurrentDestination(AReader);
  end;
end;

procedure TdxDestinationAndXmlBasedImporter.ProcessCurrentDestination(AReader: TdxXmlReader);
begin
  DestinationStack.Peek.Process(AReader);
end;

procedure TdxDestinationAndXmlBasedImporter.ImportMainDocument(AReader: TdxXmlReader; ABaseStream: TStream);
var
  APosition: Integer;
  AMessage: string;
begin
  FBaseStream := ABaseStream;
  AMessage := cxGetResourceString(@sdxRichEditMsg_Loading);
  if ABaseStream <> nil then
  begin
    APosition := Integer(ABaseStream.Position);
    ProgressIndication.&Begin(AMessage, APosition, Integer(ABaseStream.Size - ABaseStream.Position), APosition);
  end
  else
    ProgressIndication.&Begin(AMessage, 0, 1, 0);

  try
    BeginSetMainDocumentContent;
    try
      try
        ImportMainDocument(AReader);
      except
        if CreateEmptyDocumentOnLoadError then
          SetMainDocumentEmptyContent;
        raise;
      end;
    finally
      EndSetMainDocumentContent;
    end;
  finally
    ProgressIndication.&End;
    FBaseStream := nil;
  end;
end;

procedure TdxDestinationAndXmlBasedImporter.BeforeImportMainDocument;
begin
end;

procedure TdxDestinationAndXmlBasedImporter.ImportMainDocument(AReader: TdxXmlReader);
begin
  BeforeImportMainDocument;
  DestinationStack.Clear;
  ImportContent(AReader, CreateMainDocumentDestination);
  AfterImportMainDocument;
end;

procedure TdxDestinationAndXmlBasedImporter.AfterImportMainDocument;
begin
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTOnOffValue(AReader: TdxXmlReader; const AAttributeName: string): Boolean;
begin
  Result := GetWpSTOnOffValue(AReader, AAttributeName, True);
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTOnOffValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Boolean): Boolean;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  Result := GetOnOffValue(AValue, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetOnOffValue(AReader: TdxXmlReader; const AAttributeName: string): Boolean;
begin
  Result := GetOnOffValue(AReader, AAttributeName, True);
end;

function TdxDestinationAndXmlBasedImporter.GetOnOffValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Boolean): Boolean;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  Result := GetOnOffValue(AValue, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetOnOffValue(const AValue: string; ADefaultValue: Boolean): Boolean;
begin
  if AValue <> '' then
    Result := ConvertToBool(AValue)
  else
    Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTOnOffNullValue(AReader: TdxXmlReader; const AAttributeName: string): TdxNullableValue<Boolean>;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue <> '' then
    Result := ConvertToBool(AValue)
  else
    Result.Reset;
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTIntegerValue(AReader: TdxXmlReader; const AAttributeName: string): Integer;
begin
  Result := GetWpSTIntegerValue(AReader, AAttributeName, MinInt);
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTIntegerValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Integer): Integer;
begin
  Result := GetWpSTIntegerValue(AReader, AAttributeName, TdxNumberStyles.Integer, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTIntegerValue(AReader: TdxXmlReader; const AAttributeName: string;
  ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Integer): Integer;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  Result := GetIntegerValue(AValue, ANumberStyles, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetIntegerValue(AReader: TdxXmlReader; const AAttributeName: string): Integer;
begin
  Result := GetIntegerValue(AReader, AAttributeName, MinInt);
end;

function TdxDestinationAndXmlBasedImporter.GetIntegerValue(AReader: TdxXmlReader; const AAttributeName: string;
  ADefaultValue: Integer): Integer;
begin
  Result := GetIntegerValue(AReader, AAttributeName, TdxNumberStyles.Integer, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetIntegerValueInPoints(AReader: TdxXmlReader; const AAttributeName: string;
  ADefaultValue: Integer): Integer;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue <> '' then
    Result := GetIntegerValue(TdxStringHelper.Replace(AValue, 'pt', ''), TdxNumberStyles.Integer, ADefaultValue)
  else
    Result := MinInt;
end;

function TdxDestinationAndXmlBasedImporter.GetIntegerValue(AReader: TdxXmlReader; const AAttributeName: string;
  ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Integer): Integer;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  Result := GetIntegerValue(AValue, ANumberStyles, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetIntegerValue(const AValue: string; ANumberStyles: TdxNumberStyles.TStyles;
  ADefaultValue: Integer): Integer;
var
  AResult: Integer;
  ALongResult: Int64;
  ADoubleResult: Double;
begin
  if AValue <> '' then
  begin
    if TdxNumber.TryParse(AValue, ANumberStyles, AResult) then
      Exit(AResult)
    else
    begin
      if TdxNumber.TryParse(AValue, ANumberStyles, ALongResult) then
        Exit(Integer(ALongResult))
      else
      begin
        if TdxNumber.TryParse(AValue, ANumberStyles + TdxNumberStyles.AllowDecimalPoint, AResult) then
          Exit(AResult);
        if TdxNumber.TryParse(AValue, ANumberStyles + TdxNumberStyles.AllowDecimalPoint, ADoubleResult) then
          Exit(Trunc(ADoubleResult));
        if IgnoreParseErrors then
          Exit(ADefaultValue);
        ThrowInvalidFile;
      end;
    end;
  end;
  Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetFloatValueInPoints(AReader: TdxXmlReader; const AAttributeName: string;
  ADefaultValue: Single): Single;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName, '');
  if AValue <> '' then
    Result := GetFloatValue(TdxStringHelper.Replace(AValue, 'pt', ''), TdxNumberStyles.Float, ADefaultValue)
  else
    Result := NullSingleValue;
end;

function TdxDestinationAndXmlBasedImporter.GetFloatValue(const AValue: string; ANumberStyles: TdxNumberStyles.TStyles;
  ADefaultValue: Single): Single;
begin
  if AValue <> '' then
  begin
    if not TdxNumber.TryParse(AValue, ANumberStyles, Result) then
      Result := MinInt;
  end
  else
    Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTFloatValue(AReader: TdxXmlReader; const AAttributeName: string;
 ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Single; const ANs: string): Single;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName, ANs);
  Result := GetWpSTFloatValue(AValue, ANumberStyles, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTFloatValue(AReader: TdxXmlReader; const AAttributeName: string;
  ANumberStyles: TdxNumberStyles.TStyles; ADefaultValue: Single): Single;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  Result := GetWpSTFloatValue(AValue, ANumberStyles, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpSTFloatValue(const AValue: string; ANumberStyles: TdxNumberStyles.TStyles;
  ADefaultValue: Single): Single;
var
  AResult: Double;
begin
  if AValue <> '' then
  begin
    if TdxNumber.TryParse(AValue, ANumberStyles, AResult) then
      Exit(AResult)
    else
      ThrowInvalidFile;
  end;
  Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetWpDoubleValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: Double): Double;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);

  if AValue <> '' then
  begin
    if TdxNumber.TryParse(AValue, TdxNumberStyles.Float, Result) then
      Exit
    else
      ThrowInvalidFile;
  end;
  Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetLongValue(AReader: TdxXmlReader; const AAttributeName: string): Int64;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  Result := GetLongValue(AValue, MinInt);
end;

function TdxDestinationAndXmlBasedImporter.GetLongValue(const AValue: string; ADefaultValue: Int64): Int64;
begin
  Result := StrToInt64Def(AValue, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TDictionary<T, string>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxEnumeratedDictionary<T, string>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TDictionary<T, string>; ADefaultValue: T; const ANs: string): T;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName, ANs);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumOnOffNullValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TdxEnumeratedDictionary<T, string>): TdxNullableValue<T>;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  if AValue = '' then
  begin
    Result.Reset;
    Exit;
  end;

  Result := GetWpEnumOnOffNullValueCore<T>(AValue, ATable);
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumOnOffNullValueCore<T>(const AValue: string; ATable: TdxEnumeratedDictionary<T, string>): TdxNullableValue<T>;
var
  AKey: T;
  AValueString: string;
begin
  for AKey in ATable.Keys do
  begin
    AValueString := ATable[AKey];
    if AValue = AValueString then
      Exit(AKey);
  end;
  Result.Reset;
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValueCore<T>(const AValue: string; ATable: TDictionary<T, string>; ADefaultValue: T): T;
var
  AKey: T;
  AValueString: string;
begin
  for AKey in ATable.Keys do
  begin
    AValueString := ATable[AKey];
    if AValue = AValueString then
      Exit(AKey);
  end;
  Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValueCore<T>(const AValue: string; ATable: TdxEnumeratedDictionary<T, string>; ADefaultValue: T): T;
var
  AKey: T;
  AValueString: string;
begin
  for AKey in ATable.Keys do
  begin
    AValueString := ATable[AKey];
    if AValue = AValueString then
      Exit(AKey);
  end;
  Result := ADefaultValue;
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxNamedOrdinalDictionary<T>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := ReadAttribute(AReader, AAttributeName);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxDestinationAndXmlBasedImporter.GetWpEnumValueCore<T>(const AValue: string; ATable: TdxNamedOrdinalDictionary<T>; ADefaultValue: T): T;
begin
  if not ATable.TryGetValue(AValue, Result) then
    Result := ADefaultValue
end;

function TdxDestinationAndXmlBasedImporter.DecodeXmlChars(const AVal: string): string;
var
  AUnderscoreIndex, I, AValue: Integer;
  AMatches: TMatchCollection;
  AMatch: TMatch;
  AHexValue: string;
begin
  if AVal = '' then
    Exit(AVal);

  AUnderscoreIndex := Pos('_', AVal);
  if AUnderscoreIndex = 0 then
    Exit(AVal);

  AMatches := FXmlCharDecodingRegex.Matches(AVal);
  if AMatches.Count = 0 then
    Exit(AVal);

  Result := AVal;
  for I := AMatches.Count - 1 downto 0 do
  begin
    AMatch := AMatches[I];
    AHexValue := AMatch.Groups['value'].Value;
    if TdxNumber.TryParse(AHexValue, TdxNumberStyles.HexNumber, AValue) then
    begin
      if (AValue <= $001F) or (AValue >= $FFFF) then
      begin
        Delete(Result, AMatch.Index, AMatch.Length);
        Insert(Char(AValue), Result, AMatch.Index);
      end;
    end;
  end;
end;

{ TdxRichEditDestinationAndXmlBasedImporter }

constructor TdxRichEditDestinationAndXmlBasedImporter.Create(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxImporterOptions);
begin
  Assert(AOptions is TdxXmlBasedDocumentImporterOptions);
  inherited Create(ADocumentModel, AOptions);
  FPieceTableInfos := TdxObjectStack<TdxImportPieceTableInfo>.Create(True);
  PushCurrentPieceTable(TdxPieceTable(ADocumentModel.MainPart));
  ResetPositionCharacterProperties;
end;

destructor TdxRichEditDestinationAndXmlBasedImporter.Destroy;
begin
  FPieceTableInfos.Free;
  inherited Destroy;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetPieceTableInfo: TdxImportPieceTableInfo;
begin
  Result := FPieceTableInfos.Peek;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(PieceTableInfo.PieceTable);
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetPosition: TdxImportInputPosition;
begin
  Result := PieceTableInfo.Position;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetFieldInfoStack: TdxObjectStack<TdxImportFieldInfo>;
begin
  Result := PieceTableInfo.FieldInfoStack;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetBookmarks: TdxImportBookmarkInfos;
begin
  Result := PieceTableInfo.Bookmarks;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetRangePermissions: TdxImportRangePermissionInfos;
begin
  Result := PieceTableInfo.RangePermissions;
end;


function TdxRichEditDestinationAndXmlBasedImporter.GetTableStack: TdxObjectStack<TdxTable>;
begin
  Result := PieceTableInfo.TableStack;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetIgnoreParseErrors: Boolean;
begin
  Result := InnerOptions.IgnoreParseErrors;
end;

function TdxRichEditDestinationAndXmlBasedImporter.GetOptions: TdxXmlBasedDocumentImporterOptions;
begin
  Result := TdxXmlBasedDocumentImporterOptions(inherited Options);
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.ResetPositionCharacterProperties;
begin
  Position.CharacterFormatting.ReplaceInfo(DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem,
    TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseNone));
  Position.CharacterStyleIndex := 0;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.BeforeImportMainDocument;
begin
  inherited BeforeImportMainDocument;
  CurrentSection := DocumentModel.Sections.First;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.AfterImportMainDocument;
begin
  inherited AfterImportMainDocument;
  PieceTable.FixLastParagraph;
  if Position.IsContainsParagraphFrame then
    PieceTable.FixParagraphFramesInTables;
  InsertBookmarks;
  InsertRangePermissions;
  InsertComments;
  PieceTable.FixTables;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.PushCurrentPieceTable(APieceTable: TdxSimplePieceTable);
begin
  FPieceTableInfos.Push(TdxImportPieceTableInfo.Create(APieceTable));
end;

function TdxRichEditDestinationAndXmlBasedImporter.PopCurrentPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FPieceTableInfos.Peek.PieceTable);
  FPieceTableInfos.Pop;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.BeginSetMainDocumentContent;
begin
  DocumentModel.BeginSetContent;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.EndSetMainDocumentContent;
var
  AOptions: TdxFieldUpdateOnLoadOptions;
begin
  AOptions := TdxDocumentImporterOptions(Options).UpdateField.GetNativeOptions;
  try
    DocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, True, AOptions);
  finally
    AOptions.Free;
  end;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.SetMainDocumentEmptyContent;
begin
  DocumentModel.ClearDocument;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.InsertBookmarks;
var
  AId: string;
  ABookmark: TdxImportBookmarkInfo;
begin
  if not DocumentModel.DocumentCapabilities.BookmarksAllowed then
    Exit;
  for AId in Bookmarks.Keys do
  begin
    ABookmark := Bookmarks[AId];

    if ABookmark.Validate(PieceTable) then
      PieceTable.CreateBookmarkCore(ABookmark.Start, ABookmark.&End - ABookmark.Start, ABookmark.Name);
  end;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.InsertRangePermissions;
var
  AId: string;
  ARangePermission: TdxImportRangePermissionInfo;
begin
  for AId in RangePermissions.Keys do
  begin
    ARangePermission := RangePermissions[AId];
    if ARangePermission.Validate(PieceTable) then
      PieceTable.ApplyDocumentPermission(ARangePermission.Start, ARangePermission.&End, ARangePermission.PermissionInfo);
  end;
end;

procedure TdxRichEditDestinationAndXmlBasedImporter.InsertComments;
begin
end;

{ TdxOpenXmlStyleInfo }

constructor TdxOpenXmlStyleInfo.Create;
begin
  FNumberingId := -1;
end;

{ TdxOpenXmlStyleInfoCollection }

function TdxOpenXmlStyleInfoCollection.LookupStyleById(const AId: string): TdxOpenXmlStyleInfo;
var
  I: Integer;
begin
  if AId = '' then
    Exit(nil);

  for I := 0 to Count - 1 do
    if Items[I].StyleId = AId then
      Exit(Items[I]);

  Result := nil;
end;

end.

