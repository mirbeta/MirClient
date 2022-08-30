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

unit dxRichEdit.Import.OpenXML.DestinationStyles;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationParagraph,
  dxRichEdit.Import.OpenXML.DestinationRunProperties;

type

  { TdxStylesDestination }

  TdxStylesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnDocumentDefaults(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxOpenXmlTableFormattingInfo }

  TdxOpenXmlTableFormattingInfo = class(TcxIUnknownObject, IdxCellPropertiesOwner)
  strict private
    FConditionType: TdxConditionalTableStyleFormattingType;
    FCharacterFormatting: TdxCharacterFormattingBase;
    FParagraphFormatting: TdxParagraphFormattingBase;
    FTabs: TdxTabFormattingInfo;
    FTableProperties: TdxTableProperties;
    FTableRowProperties: TdxTableRowProperties;
    FTableCellProperties: TdxTableCellProperties;
  public
    constructor Create(const ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;

    property ConditionType: TdxConditionalTableStyleFormattingType read FConditionType write FConditionType;
    property CharacterFormatting: TdxCharacterFormattingBase read FCharacterFormatting write FCharacterFormatting;
    property ParagraphFormatting: TdxParagraphFormattingBase read FParagraphFormatting write FParagraphFormatting;
    property Tabs: TdxTabFormattingInfo read FTabs write FTabs;
    property TableProperties: TdxTableProperties read FTableProperties write FTableProperties;
    property TableRowProperties: TdxTableRowProperties read FTableRowProperties write FTableRowProperties;
    property TableCellProperties: TdxTableCellProperties read FTableCellProperties write FTableCellProperties;
  end;

  { TdxStyleDestinationBase }

  TdxStyleDestinationBase = class abstract(TdxElementDestination)
  protected
    FStyleInfo: TdxOpenXmlTableFormattingInfo;
    FNumberingId: Integer;
    FListLevelIndex: Integer;
    function GetCharacterFormatting: TdxCharacterFormattingBase;
    function GetParagraphFormatting: TdxParagraphFormattingBase;
    function GetTableProperties: TdxTableProperties;
    function GetTableRowProperties: TdxTableRowProperties;
    function GetTabs: TdxTabFormattingInfo;
    function GetTableCellProperties: TdxTableCellProperties;
    class procedure FillElementHandlerTable(AHandlerTable: TdxElementHandlerTable); static;

    property CharacterFormatting: TdxCharacterFormattingBase read GetCharacterFormatting;
    property ParagraphFormatting: TdxParagraphFormattingBase read GetParagraphFormatting;
    property Tabs: TdxTabFormattingInfo read GetTabs;
    property TableProperties: TdxTableProperties read GetTableProperties;
    property TableRowProperties: TdxTableRowProperties read GetTableRowProperties;
    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxStyleDestinationBase; static;
    class function OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property NumberingId: Integer read FNumberingId write FNumberingId;
    property ListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
  end;

  { TdxStyleDestination }

  TdxStyleDestination = class(TdxStyleDestinationBase)
  public const
    NormalTableStyleName = 'Normal Table';
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FId: string;
    FStyleType: string;
    FParentId: string;
    FLinkedStyleId: string;
    FStyleName: string;
    FSemiHidden: Boolean;
    FHidden: Boolean;
    FIsDefaultStyle: Boolean;
    FQFormat: Boolean;
    FNextStyleId: string;
    FConditionalTableFormattingInfoList: TdxObjectList<TdxOpenXmlTableFormattingInfo>;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    procedure ImportParagraphStyle; virtual;
    procedure ImportCharacterStyle; virtual;
    procedure ImportTableStyle; virtual;
    procedure ImportTableCellStyle; virtual;
    procedure ImportNumberingStyle; virtual;
    function ImportParagraphStyleCore: Integer; virtual;
    function ImportCharacterStyleCore: Integer; virtual;
    function ImportTableStyleCore: Integer; virtual;
    function ImportTableCellStyleCore: Integer; virtual;
    function ImportNumberingStyleCore: Integer; virtual;
    function ImportStyleCore(AStyleIndex: Integer): TdxOpenXmlStyleInfo; virtual;
    function CreateParagraphStyle: TdxParagraphStyle; virtual;
    procedure ApplyParagraphStyleProperties(AStyle: TdxParagraphStyle); virtual;
    function CreateCharacterStyle: TdxCharacterStyle; virtual;
    procedure ApplyCharacterStyleProperties(AStyle: TdxCharacterStyle); virtual;
    function CreateTableStyle: TdxTableStyle; virtual;
    function CreateTableCellStyle: TdxTableCellStyle; virtual;
    function CreateNumberingListStyle: TdxNumberingListStyle; virtual;
    procedure ApplyTableStyleProperties(AStyle: TdxTableStyle); virtual;
    function GetParentTableStyle(AIndex: Integer): TdxTableStyle; virtual;
    procedure ApplyTableCellStyleProperties(AStyle: TdxTableCellStyle); virtual;
    function GetParentTableCellStyle(AIndex: Integer): TdxTableCellStyle; virtual;
    procedure ApplyConditionalTableFormattingInfo(AStyle: TdxTableStyle; AInfo: TdxOpenXmlTableFormattingInfo); virtual;
    procedure AddStyleConditionalTableFormatting(AInfo: TdxOpenXmlTableFormattingInfo); virtual;
  public
    destructor Destroy; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxStyleDestination; static;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnParentStyleId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnHidden(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnQFormat(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSemiHidden(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnName(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLinkedStyleId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNextStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStyleConditionalTableFormatting(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property ParentId: string read FParentId write FParentId;
    property LinkedStyleId: string read FLinkedStyleId write FLinkedStyleId;
    property StyleName: string read FStyleName write FStyleName;
    property SemiHidden: Boolean read FSemiHidden write FSemiHidden;
    property Hidden: Boolean read FHidden write FHidden;
    property IsDefaultStyle: Boolean read FIsDefaultStyle write FIsDefaultStyle;
    property NextStyleId: string read FNextStyleId write FNextStyleId;
    property QFormat: Boolean read FQFormat write FQFormat;
  end;

  { TdxDocumentDefaultsDestination }

  TdxDocumentDefaultsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnDefaultParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDefaultRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxDefaultRunPropertiesDestination }

  TdxDefaultRunPropertiesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    class function OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxInnerDefaultRunPropertiesDestination }

  TdxInnerDefaultRunPropertiesDestination = class(TdxRunPropertiesBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function GetCharacterProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxCharacterProperties; static;
    class function OnFontName(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxDefaultParagraphPropertiesDestination }

  TdxDefaultParagraphPropertiesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxInnerDefaultParagraphPropertiesDestination }

  TdxInnerDefaultParagraphPropertiesDestination = class(TdxParagraphPropertiesBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function GetNumberingId: Integer; override;
    procedure SetNumberingId(const AValue: Integer); override;
    function GetListLevelIndex: Integer; override;
    procedure SetListLevelIndex(const AValue: Integer); override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParagraphProperties: TdxParagraphProperties; ATabs: TdxTabFormattingInfo);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxStyleParagraphPropertiesDestination }

  TdxStyleParagraphPropertiesDestination = class(TdxParagraphPropertiesBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FStyleDestination: TdxStyleDestinationBase;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function GetNumberingId: Integer; override;
    procedure SetNumberingId(const AValue: Integer); override;
    function GetListLevelIndex: Integer; override;
    procedure SetListLevelIndex(const AValue: Integer); override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AStyleDestination: TdxStyleDestinationBase;
      AParagraphFormatting: TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxStyleParagraphPropertiesDestination; static;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxStyleRunPropertiesDestination }

  TdxStyleRunPropertiesDestination = class(TdxRunPropertiesBaseDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxStyleLeafElementDestination }

  TdxStyleLeafElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FStyleDestination: TdxStyleDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AStyleDestination: TdxStyleDestination);

    property StyleDestination: TdxStyleDestination read FStyleDestination;
  end;

  { TdxStyleParentIdDestination }

  TdxStyleParentIdDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxStyleHiddenDestination }

  TdxStyleHiddenDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxStyleSemiHiddenDestination }

  TdxStyleSemiHiddenDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxStyleNameDestination }

  TdxStyleNameDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxStyleQFormatDestination }

  TdxStyleQFormatDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxLinkedStyleIdDestination }

  TdxLinkedStyleIdDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxNextStyleDestination }

  TdxNextStyleDestination = class(TdxStyleLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxStyleConditionalTableFormatting }

  TdxStyleConditionalTableFormatting = class(TdxStyleDestinationBase)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class var
      FCondtionTypesTable: TdxNamedOrdinalDictionary<TdxConditionalTableStyleFormattingType>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    class function CreateCondtionTypesTable: TdxNamedOrdinalDictionary<TdxConditionalTableStyleFormattingType>; static;
  strict private
    FConditionType: TdxConditionalTableStyleFormattingType;
    FStyleDestination: TdxStyleDestination;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AStyleDestination: TdxStyleDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML.DestinationTable,
  dxRichEdit.Export.OpenXML;

type
  { TdxStyleTablePropertiesDestination }

  TdxStyleTablePropertiesDestination = class(TdxTablePropertiesDestinationCore)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxStyleTableRowPropertiesDestination }

  TdxStyleTableRowPropertiesDestination = class(TdxTableRowPropertiesDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxStyleTableCellPropertiesDestination }

  TdxStyleTableCellPropertiesDestination = class(TdxTableCellPropertiesDestinationCore)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;


{ TdxStylesDestination }

class constructor TdxStylesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class function TdxStylesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('docDefaults', OnDocumentDefaults);
  Result.Add('style', OnStyle);
end;

class destructor TdxStylesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

function TdxStylesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxStylesDestination.OnDocumentDefaults(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentDefaultsDestination.Create(AImporter);
end;

class function TdxStylesDestination.OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleDestination.Create(AImporter);
end;

{ TdxOpenXmlTableFormattingInfo }

constructor TdxOpenXmlTableFormattingInfo.Create(const ADocumentModel: TdxDocumentModel);
var
  AMainPieceTable: TdxPieceTable;
begin
  inherited Create;
  AMainPieceTable := ADocumentModel.MainPieceTable;
  FCharacterFormatting := ADocumentModel.CreateEmptyCharacterFormatting;
  FParagraphFormatting := ADocumentModel.CreateEmptyParagraphFormatting;
  FTableProperties := TdxTableProperties.Create(AMainPieceTable);
  FTableRowProperties := TdxTableRowProperties.Create(AMainPieceTable);
  FTableCellProperties := TdxTableCellProperties.Create(AMainPieceTable, Self);
  FTabs := TdxTabFormattingInfo.Create;
end;

destructor TdxOpenXmlTableFormattingInfo.Destroy;
begin
  FreeAndNil(FTableCellProperties);
  FreeAndNil(FCharacterFormatting);
  FreeAndNil(FParagraphFormatting);
  FreeAndNil(FTabs);
  FreeAndNil(FTableProperties);
  FreeAndNil(FTableRowProperties);
  inherited Destroy;
end;

procedure TdxOpenXmlTableFormattingInfo.BeginUpdate;
begin
  FCharacterFormatting.BeginUpdate;
  FParagraphFormatting.BeginUpdate;
  FTableProperties.BeginUpdate;
  FTableRowProperties.BeginUpdate;
  TableCellProperties.BeginUpdate;
end;

function TdxOpenXmlTableFormattingInfo.CreateCellPropertiesChangedHistoryItem(
  AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = FTableCellProperties);
  Result := TdxIndexChangedHistoryItem.Create(AProperties.PieceTable, AProperties);
end;

procedure TdxOpenXmlTableFormattingInfo.EndUpdate;
begin
  FCharacterFormatting.EndUpdate;
  FParagraphFormatting.EndUpdate;
  FTableProperties.EndUpdate;
  FTableRowProperties.EndUpdate;
  TableCellProperties.EndUpdate;
end;

{ TdxStyleDestinationBase }

constructor TdxStyleDestinationBase.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FNumberingId := -1;
  FStyleInfo := TdxOpenXmlTableFormattingInfo.Create(DocumentModel);
end;

destructor TdxStyleDestinationBase.Destroy;
begin
  FStyleInfo.Free;
  inherited Destroy;
end;

class procedure TdxStyleDestinationBase.FillElementHandlerTable(AHandlerTable: TdxElementHandlerTable);
begin
  AHandlerTable.Add('pPr', OnParagraphProperties);
  AHandlerTable.Add('rPr', OnRunProperties);
  AHandlerTable.Add('tblPr', OnTableProperties);
  AHandlerTable.Add('trPr', OnTableRowProperties);
  AHandlerTable.Add('tcPr', OnTableCellProperties);
end;

procedure TdxStyleDestinationBase.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FStyleInfo.BeginUpdate;
end;

procedure TdxStyleDestinationBase.ProcessElementClose(AReader: TdxXmlReader);
begin
  FStyleInfo.EndUpdate;
end;

function TdxStyleDestinationBase.GetCharacterFormatting: TdxCharacterFormattingBase;
begin
  Assert(FStyleInfo <> nil);
  Result := FStyleInfo.CharacterFormatting;
end;

function TdxStyleDestinationBase.GetParagraphFormatting: TdxParagraphFormattingBase;
begin
  Assert(FStyleInfo <> nil);
  Result := FStyleInfo.ParagraphFormatting;
end;

function TdxStyleDestinationBase.GetTableCellProperties: TdxTableCellProperties;
begin
  Assert(FStyleInfo <> nil);
  Result := FStyleInfo.TableCellProperties;
end;

function TdxStyleDestinationBase.GetTableProperties: TdxTableProperties;
begin
  Assert(FStyleInfo <> nil);
  Result := FStyleInfo.TableProperties;
end;

function TdxStyleDestinationBase.GetTableRowProperties: TdxTableRowProperties;
begin
  Assert(FStyleInfo <> nil);
  Result := FStyleInfo.TableRowProperties
end;

function TdxStyleDestinationBase.GetTabs: TdxTabFormattingInfo;
begin
  Assert(FStyleInfo <> nil);
  Result := FStyleInfo.Tabs
end;

class function TdxStyleDestinationBase.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxStyleDestinationBase;
begin
  Result := TdxStyleDestinationBase(AImporter.PeekDestination);
end;

class function TdxStyleDestinationBase.OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  AParagraphFormatting := GetThis(AImporter).ParagraphFormatting;
  AImporter.DocumentModel.ResetParagraphFormatting(AParagraphFormatting);
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateStyleParagraphPropertiesDestination(GetThis(AImporter),
    AParagraphFormatting, GetThis(AImporter).Tabs);
end;

class function TdxStyleDestinationBase.OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := GetThis(AImporter).CharacterFormatting;
  AImporter.DocumentModel.ResetCharacterFormatting(ACharacterFormatting);
  Result := TdxStyleRunPropertiesDestination.Create(AImporter, ACharacterFormatting);
end;

class function TdxStyleDestinationBase.OnTableProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ATableProperties: TdxTableProperties;
begin
  ATableProperties := GetThis(AImporter).TableProperties;
  ATableProperties.ReplaceInfo(AImporter.DocumentModel.Cache.TablePropertiesOptionsCache.DefaultItem, []);
  Result := TdxStyleTablePropertiesDestination.Create(AImporter, ATableProperties);
end;

class function TdxStyleDestinationBase.OnTableRowProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ATableRowProperties: TdxTableRowProperties;
begin
  ATableRowProperties := GetThis(AImporter).TableRowProperties;
  ATableRowProperties.ReplaceInfo(AImporter.DocumentModel.Cache.TableRowPropertiesOptionsCache.DefaultItem, []);
  Result := TdxStyleTableRowPropertiesDestination.Create(AImporter, ATableRowProperties);
end;

class function TdxStyleDestinationBase.OnTableCellProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ATableCellProperties: TdxTableCellProperties;
begin
  ATableCellProperties := GetThis(AImporter).TableCellProperties;
  ATableCellProperties.ReplaceInfo(AImporter.DocumentModel.Cache.TableCellPropertiesOptionsCache.DefaultItem, []);
  Result := TdxStyleTableCellPropertiesDestination.Create(AImporter, ATableCellProperties);
end;

{ TdxStyleDestination }

class constructor TdxStyleDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxStyleDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxStyleDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  FillElementHandlerTable(Result);
  Result.Add('basedOn', OnParentStyleId);
  Result.Add('hidden', OnHidden);
  Result.Add('qFormat', OnQFormat);
  Result.Add('semiHidden', OnSemiHidden);
  Result.Add('name', OnName);
  Result.Add('link', OnLinkedStyleId);
  Result.Add('next', OnNextStyle);
  Result.Add('tblStylePr', OnStyleConditionalTableFormatting);
end;

function TdxStyleDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxStyleDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxStyleDestination;
begin
  Result := TdxStyleDestination(AImporter.PeekDestination);
end;

procedure TdxStyleDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  inherited ProcessElementOpen(AReader);

  FId := AReader.GetAttribute('styleId', Importer.WordProcessingNamespaceConst);
  FStyleType := AReader.GetAttribute('type', Importer.WordProcessingNamespaceConst);
  FIsDefaultStyle := Importer.GetWpSTOnOffValue(AReader, 'default', False);
end;

procedure TdxStyleDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  inherited ProcessElementClose(AReader);

  if FId = '' then
    Exit;

  if FStyleType = '' then
    FStyleType := 'paragraph';

  if FStyleType = 'paragraph' then
    ImportParagraphStyle
  else
    if FStyleType = 'character' then
      ImportCharacterStyle
    else
      if FStyleType = 'table' then
        ImportTableStyle
      else
        if FStyleType = 'tableCell' then
          ImportTableCellStyle
        else
          if FStyleType = 'numbering' then
            ImportNumberingStyle;
end;

class function TdxStyleDestination.OnParentStyleId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleParentIdDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnHidden(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleHiddenDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnQFormat(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleQFormatDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnSemiHidden(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleSemiHiddenDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnName(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleNameDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnLinkedStyleId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxLinkedStyleIdDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnNextStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNextStyleDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxStyleDestination.OnStyleConditionalTableFormatting(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStyleConditionalTableFormatting.Create(AImporter, GetThis(AImporter));
end;

procedure TdxStyleDestination.ImportParagraphStyle;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := ImportParagraphStyleCore;
  TdxWordProcessingMLBaseImporter(Importer).ParagraphStyleInfos.Add(ImportStyleCore(AStyleIndex));
end;

procedure TdxStyleDestination.ImportCharacterStyle;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := ImportCharacterStyleCore;
  TdxWordProcessingMLBaseImporter(Importer).CharacterStyleInfos.Add(ImportStyleCore(AStyleIndex));
end;

procedure TdxStyleDestination.ImportTableStyle;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := ImportTableStyleCore;
  TdxWordProcessingMLBaseImporter(Importer).TableStyleInfos.Add(ImportStyleCore(AStyleIndex));
end;

procedure TdxStyleDestination.ImportTableCellStyle;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := ImportTableCellStyleCore;
  TdxWordProcessingMLBaseImporter(Importer).TableCellStyleInfos.Add(ImportStyleCore(AStyleIndex));
end;

procedure TdxStyleDestination.ImportNumberingStyle;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := ImportNumberingStyleCore;
  TdxWordProcessingMLBaseImporter(Importer).NumberingStyleInfos.Add(ImportStyleCore(AStyleIndex));
end;

function TdxStyleDestination.ImportParagraphStyleCore: Integer;
var
  AStyleIndex: Integer;
  AStyle: TdxParagraphStyle;
begin
  if IsDefaultStyle and not Importer.DefaultParagraphStyleProcessed then
  begin
    ApplyParagraphStyleProperties(TdxParagraphStyle(DocumentModel.ParagraphStyles.DefaultItem));
    Importer.DefaultParagraphStyleProcessed := True;
    Result := 0;
  end
  else
  begin
    AStyleIndex := DocumentModel.ParagraphStyles.GetStyleIndexByName(StyleName);
    if AStyleIndex >= 0 then
    begin
      ApplyParagraphStyleProperties(DocumentModel.ParagraphStyles[AStyleIndex]);
      Result := AStyleIndex;
    end
    else
    begin
      AStyle := CreateParagraphStyle;
      Result := DocumentModel.ParagraphStyles.Add(AStyle);
    end;
  end;
end;

function TdxStyleDestination.ImportCharacterStyleCore: Integer;
var
  AStyleIndex: Integer;
  AStyle: TdxCharacterStyle;
begin
  if IsDefaultStyle and not Importer.DefaultCharacterStyleProcessed then
  begin
    ApplyCharacterStyleProperties(TdxCharacterStyle(DocumentModel.CharacterStyles.DefaultItem));
    Importer.DefaultCharacterStyleProcessed := True;
    Result := 0;
  end
  else
  begin
    AStyleIndex := DocumentModel.CharacterStyles.GetStyleIndexByName(StyleName);
    if AStyleIndex >= 0 then
    begin
      ApplyCharacterStyleProperties(DocumentModel.CharacterStyles[AStyleIndex]);
      Result := AStyleIndex;
    end
    else
    begin
      AStyle := CreateCharacterStyle;
      Result := DocumentModel.CharacterStyles.Add(AStyle);
    end;
  end;
end;

function TdxStyleDestination.ImportTableStyleCore: Integer;
var
  AStyleIndex: Integer;
  AStyle: TdxTableStyle;
begin
  if IsDefaultStyle and not Importer.DefaultTableStyleProcessed then
  begin
    ApplyTableStyleProperties(TdxTableStyle(DocumentModel.TableStyles.DefaultItem));
    Importer.DefaultTableStyleProcessed := True;
    Result := 0;
  end
  else
  begin
    AStyleIndex := DocumentModel.TableStyles.GetStyleIndexByName(FStyleName);
    if AStyleIndex >= 0 then
    begin
      ApplyTableStyleProperties(DocumentModel.TableStyles[AStyleIndex]);
      Result := AStyleIndex;
    end
    else
    begin
      AStyle := CreateTableStyle;
      Result := DocumentModel.TableStyles.Add(AStyle);
    end;
  end;
end;

function TdxStyleDestination.ImportTableCellStyleCore: Integer;
var
  AStyleIndex: Integer;
  AStyle: TdxTableCellStyle;
begin
  if IsDefaultStyle and not Importer.DefaultTableCellStyleProcessed then
  begin
    ApplyTableCellStyleProperties(TdxTableCellStyle(DocumentModel.TableCellStyles.DefaultItem));
    Importer.DefaultTableCellStyleProcessed := True;
    Result := 0;
  end
  else
  begin
    AStyleIndex := DocumentModel.TableCellStyles.GetStyleIndexByName(FStyleName);
    if AStyleIndex >= 0 then
    begin
      ApplyTableCellStyleProperties(DocumentModel.TableCellStyles[AStyleIndex]);
      Result := AStyleIndex;
    end
    else
    begin
      AStyle := CreateTableCellStyle;
      Result := DocumentModel.TableCellStyles.Add(AStyle);
    end;
  end;
end;

function TdxStyleDestination.ImportNumberingStyleCore: Integer;
var
  AStyleIndex: Integer;
  AStyle: TdxNumberingListStyle;
begin
  AStyleIndex := DocumentModel.NumberingListStyles.GetStyleIndexByName(StyleName);
  if AStyleIndex >= 0 then
    Exit(AStyleIndex);
  AStyle := CreateNumberingListStyle;
  Result := DocumentModel.NumberingListStyles.Add(AStyle);
end;

function TdxStyleDestination.ImportStyleCore(AStyleIndex: Integer): TdxOpenXmlStyleInfo;
begin
  Result := TdxOpenXmlStyleInfo.Create;
  Result.StyleId := FId;
  Result.ParentStyleId := FParentId;
  Result.LinkedStyleId := FLinkedStyleId;
  Result.NextStyleId := FNextStyleId;
  Result.StyleIndex := AStyleIndex;
  Result.NumberingId := NumberingId;
  Result.ListLevelIndex := ListLevelIndex;
end;

function TdxStyleDestination.CreateParagraphStyle: TdxParagraphStyle;
begin
  Result := TdxParagraphStyle.Create(DocumentModel);
  ApplyParagraphStyleProperties(Result);
end;

procedure TdxStyleDestination.ApplyParagraphStyleProperties(AStyle: TdxParagraphStyle);
begin
  AStyle.StyleName := FStyleName;
  AStyle.SemihiddenCore := FSemiHidden;
  AStyle.Hidden := FHidden;
  AStyle.Primary := FQFormat;
  AStyle.CharacterProperties.CopyFrom(CharacterFormatting);
  AStyle.ParagraphProperties.CopyFrom(ParagraphFormatting);
  if Importer.Position.ParagraphFrameFormatting.Options.Value <> [] then
  begin
    AStyle.CreateFrameProperties;
    AStyle.FrameProperties.CopyFrom(Importer.Position.ParagraphFrameFormatting);
    Importer.Position.ParagraphFrameFormatting.ReplaceInfo(
      Importer.DocumentModel.Cache.ParagraphFrameFormattingInfoCache.DefaultItem,
      TdxParagraphFrameFormattingOptions.Create);
  end;
  AStyle.Tabs.SetTabs(Tabs);
  if NumberingId = NumberingListIndexNoNumberingList then
    AStyle.SetNumberingListIndex(NumberingListIndexNoNumberingList);
end;

function TdxStyleDestination.CreateCharacterStyle: TdxCharacterStyle;
begin
  Result := TdxCharacterStyle.Create(DocumentModel);
  ApplyCharacterStyleProperties(Result);
end;

procedure TdxStyleDestination.ApplyCharacterStyleProperties(AStyle: TdxCharacterStyle);
begin
  AStyle.StyleName := FStyleName;
  AStyle.SemihiddenCore := FSemiHidden;
  AStyle.Hidden := FHidden;
  AStyle.Primary := FQFormat;
  AStyle.CharacterProperties.CopyFrom(CharacterFormatting);
end;

function TdxStyleDestination.CreateTableStyle: TdxTableStyle;
begin
  Result := TdxTableStyle.Create(DocumentModel);
  ApplyTableStyleProperties(Result);
end;

destructor TdxStyleDestination.Destroy;
begin
  FreeAndNil(FConditionalTableFormattingInfoList);
  inherited Destroy;
end;

function TdxStyleDestination.CreateTableCellStyle: TdxTableCellStyle;
begin
  Result := TdxTableCellStyle.Create(DocumentModel);
  ApplyTableCellStyleProperties(Result);
end;

function TdxStyleDestination.CreateNumberingListStyle: TdxNumberingListStyle;
begin
  Result := TdxNumberingListStyle.Create(DocumentModel, FStyleName);
end;

procedure TdxStyleDestination.ApplyTableStyleProperties(AStyle: TdxTableStyle);
var
  AIndex, ACount, I: Integer;
  AParentStyle: TdxTableStyle;
  AIsNormalTableStyle: Boolean;
begin
  AStyle.StyleName := FStyleName;
  AStyle.SemihiddenCore := FSemiHidden;
  AStyle.Hidden := FHidden;
  AStyle.Primary := FQFormat;
  if ParentId <> '' then
  begin
    AIndex := TdxWordProcessingMLBaseImporter(Importer).LookupTableStyleIndex(ParentId);
    AParentStyle := GetParentTableStyle(AIndex);
    if AParentStyle <> nil then
      AStyle.Parent := AParentStyle;
  end;
  AIsNormalTableStyle := AStyle.LocalizedStyleName = NormalTableStyleName;
  if (AIsNormalTableStyle and (TableProperties.CellMargins.UseLeftMargin or TableProperties.CellMargins.UseRightMargin or
      TableProperties.CellMargins.UseBottomMargin or TableProperties.CellMargins.UseTopMargin)) or not AIsNormalTableStyle then
    AStyle.TableProperties.CopyFrom(TableProperties);
  AStyle.TableRowProperties.CopyFrom(TableRowProperties);
  AStyle.TableCellProperties.CopyFrom(TableCellProperties);
  AStyle.CharacterProperties.CopyFrom(CharacterFormatting);
  AStyle.ParagraphProperties.CopyFrom(ParagraphFormatting);
  AStyle.Tabs.SetTabs(Tabs);
  if FConditionalTableFormattingInfoList <> nil then
  begin
    ACount := FConditionalTableFormattingInfoList.Count;
    for I := 0 to ACount - 1 do
      ApplyConditionalTableFormattingInfo(AStyle, FConditionalTableFormattingInfoList[I]);
  end;
end;

function TdxStyleDestination.GetParentTableStyle(AIndex: Integer): TdxTableStyle;
begin
  if (AIndex >= 0) and (AIndex < DocumentModel.TableStyles.Count) then
    Result := DocumentModel.TableStyles[AIndex]
  else
    Result := nil;
end;

procedure TdxStyleDestination.ApplyTableCellStyleProperties(AStyle: TdxTableCellStyle);
var
  AIndex: Integer;
  AParentStyle: TdxTableCellStyle;
begin
  AStyle.StyleName := FStyleName;
  AStyle.SemihiddenCore := FSemiHidden;
  AStyle.Hidden := FHidden;
  AStyle.Primary := FQFormat;
  if ParentId <> '' then
  begin
    AIndex := TdxWordProcessingMLBaseImporter(Importer).LookupTableCellStyleIndex(ParentId);
    AParentStyle := GetParentTableCellStyle(AIndex);
    if AParentStyle <> nil then
      AStyle.Parent := AParentStyle;
  end;
  AStyle.TableCellProperties.CopyFrom(TableCellProperties);
  AStyle.CharacterProperties.CopyFrom(CharacterFormatting);
  AStyle.ParagraphProperties.CopyFrom(ParagraphFormatting);
  AStyle.Tabs.SetTabs(Tabs);
end;

function TdxStyleDestination.GetParentTableCellStyle(AIndex: Integer): TdxTableCellStyle;
begin
  if (AIndex >= 0) and (AIndex < DocumentModel.TableCellStyles.Count) then
    Result := DocumentModel.TableCellStyles[AIndex]
  else
    Result := nil;
end;

procedure TdxStyleDestination.ApplyConditionalTableFormattingInfo(AStyle: TdxTableStyle;
  AInfo: TdxOpenXmlTableFormattingInfo);
var
  AConditionalStyle: TdxTableConditionalStyle;
begin
  AConditionalStyle := AStyle.ConditionalStyleProperties.GetStyleSafe(AInfo.ConditionType);
  AConditionalStyle.TableCellProperties.CopyFrom(AInfo.TableCellProperties);
  AConditionalStyle.TableRowProperties.CopyFrom(AInfo.TableRowProperties);
  AConditionalStyle.TableProperties.CopyFrom(AInfo.TableProperties);
  AConditionalStyle.CharacterProperties.CopyFrom(AInfo.CharacterFormatting);
  AConditionalStyle.ParagraphProperties.CopyFrom(AInfo.ParagraphFormatting);
  AConditionalStyle.Tabs.CopyFrom(AInfo.Tabs);
end;

procedure TdxStyleDestination.AddStyleConditionalTableFormatting(AInfo: TdxOpenXmlTableFormattingInfo);
begin
  if FConditionalTableFormattingInfoList = nil then
    FConditionalTableFormattingInfoList := TdxObjectList<TdxOpenXmlTableFormattingInfo>.Create;
  FConditionalTableFormattingInfoList.Add(AInfo);
end;

{ TdxDocumentDefaultsDestination }

class constructor TdxDocumentDefaultsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDocumentDefaultsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDocumentDefaultsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pPrDefault', OnDefaultParagraphProperties);
  Result.Add('rPrDefault', OnDefaultRunProperties);
end;

function TdxDocumentDefaultsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDocumentDefaultsDestination.OnDefaultParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDefaultParagraphPropertiesDestination.Create(AImporter);
end;

class function TdxDocumentDefaultsDestination.OnDefaultRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDefaultRunPropertiesDestination.Create(AImporter);
end;

{ TdxDefaultRunPropertiesDestination }

class constructor TdxDefaultRunPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDefaultRunPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDefaultRunPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('rPr', OnRunProperties);
end;

function TdxDefaultRunPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxDefaultRunPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AProperties: TdxCharacterProperties;
begin
  inherited ProcessElementOpen(AReader);
  AProperties := DocumentModel.DefaultCharacterProperties;
  AProperties.BeginUpdate;
  try
    AProperties.FontName := 'Times New Roman';
    AProperties.DoubleFontSize := 20;
  finally
    AProperties.EndUpdate;
  end;
end;

class function TdxDefaultRunPropertiesDestination.OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxInnerDefaultRunPropertiesDestination.Create(AImporter, AImporter.DocumentModel.DefaultCharacterProperties);
end;

{ TdxInnerDefaultRunPropertiesDestination }

class constructor TdxInnerDefaultRunPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxInnerDefaultRunPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxInnerDefaultRunPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxRunPropertiesBaseDestination.CreateElementHandlerTable;
  Result.Remove('rFonts');
  Result.Add('rFonts', OnFontName);
end;

function TdxInnerDefaultRunPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxInnerDefaultRunPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterProperties;
begin
  ACharacterFormatting := TdxCharacterProperties(CharacterProperties);
  ACharacterFormatting.BeginUpdate;
end;

procedure TdxInnerDefaultRunPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterProperties;
begin
  ACharacterFormatting := TdxCharacterProperties(CharacterProperties);
  ACharacterFormatting.EndUpdate;
end;

class function TdxInnerDefaultRunPropertiesDestination.GetCharacterProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxCharacterProperties;
var
  AThisObject: TdxRunPropertiesBaseDestination;
begin
  AThisObject := TdxRunPropertiesBaseDestination(AImporter.PeekDestination);
  Result := AThisObject.CharacterProperties;
end;

class function TdxInnerDefaultRunPropertiesDestination.OnFontName(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDefaultFontNameDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

{ TdxDefaultParagraphPropertiesDestination }

class constructor TdxDefaultParagraphPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDefaultParagraphPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

procedure TdxDefaultParagraphPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AProperties: TdxParagraphProperties;
begin
  inherited ProcessElementOpen(AReader);
  AProperties := DocumentModel.DefaultParagraphProperties;
  AProperties.BeginUpdate;
  try
    AProperties.LineSpacingType := TdxParagraphLineSpacing.Single;
    AProperties.SpacingBefore := 0;
    AProperties.SpacingAfter := 0;
  finally
    AProperties.EndUpdate;
  end;
end;

class function TdxDefaultParagraphPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pPr', OnParagraphProperties);
end;

function TdxDefaultParagraphPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDefaultParagraphPropertiesDestination.OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxInnerDefaultParagraphPropertiesDestination.Create(AImporter, AImporter.DocumentModel.DefaultParagraphProperties, TdxTabFormattingInfo.Create);
end;

{ TdxInnerDefaultParagraphPropertiesDestination }

constructor TdxInnerDefaultParagraphPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParagraphProperties: TdxParagraphProperties; ATabs: TdxTabFormattingInfo);
begin
  inherited Create(AImporter, AParagraphProperties, ATabs);
  ATabs.Clear;
end;

destructor TdxInnerDefaultParagraphPropertiesDestination.Destroy;
begin
  Tabs.Free;
  inherited Destroy;
end;

class constructor TdxInnerDefaultParagraphPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxInnerDefaultParagraphPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxInnerDefaultParagraphPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxParagraphPropertiesBaseDestination.CreateElementHandlerTable;
end;

function TdxInnerDefaultParagraphPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxInnerDefaultParagraphPropertiesDestination.GetNumberingId: Integer;
begin
  Result := -1;
end;

procedure TdxInnerDefaultParagraphPropertiesDestination.SetNumberingId(const AValue: Integer);
begin
end;

function TdxInnerDefaultParagraphPropertiesDestination.GetListLevelIndex: Integer;
begin
  Result := -1;
end;

procedure TdxInnerDefaultParagraphPropertiesDestination.SetListLevelIndex(const AValue: Integer);
begin
end;

procedure TdxInnerDefaultParagraphPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AParagraphProperties: TdxParagraphProperties;
begin
  AParagraphProperties := TdxParagraphProperties(ParagraphFormatting);
  AParagraphProperties.BeginUpdate;
end;

procedure TdxInnerDefaultParagraphPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AParagraphProperties: TdxParagraphProperties;
begin
  AParagraphProperties := TdxParagraphProperties(ParagraphFormatting);
  AParagraphProperties.EndUpdate;
end;

{ TdxStyleParagraphPropertiesDestination }

constructor TdxStyleParagraphPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AStyleDestination: TdxStyleDestinationBase; AParagraphFormatting: TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo);
begin
  inherited Create(AImporter, AParagraphFormatting, ATabs);
  Assert(AStyleDestination <> nil);
  FStyleDestination := AStyleDestination;
end;

class constructor TdxStyleParagraphPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxStyleParagraphPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxStyleParagraphPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxParagraphPropertiesBaseDestination.CreateElementHandlerTable;
  Result.Add('numPr', OnNumbering);
end;

function TdxStyleParagraphPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxStyleParagraphPropertiesDestination.GetNumberingId: Integer;
begin
  Result := FStyleDestination.NumberingId;
end;

procedure TdxStyleParagraphPropertiesDestination.SetNumberingId(const AValue: Integer);
begin
  FStyleDestination.NumberingId := AValue;
end;

function TdxStyleParagraphPropertiesDestination.GetListLevelIndex: Integer;
begin
  Result := FStyleDestination.ListLevelIndex;
end;

procedure TdxStyleParagraphPropertiesDestination.SetListLevelIndex(const AValue: Integer);
begin
  FStyleDestination.ListLevelIndex := AValue;
end;

class function TdxStyleParagraphPropertiesDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxStyleParagraphPropertiesDestination;
begin
  Result := TdxStyleParagraphPropertiesDestination(AImporter.PeekDestination);
end;

procedure TdxStyleParagraphPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  AParagraphFormatting := TdxParagraphFormattingBase(ParagraphFormatting);
  AParagraphFormatting.BeginUpdate;
end;

procedure TdxStyleParagraphPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  AParagraphFormatting := TdxParagraphFormattingBase(ParagraphFormatting);
  AParagraphFormatting.EndUpdate;
end;

class function TdxStyleParagraphPropertiesDestination.OnNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphNumberingReferenceDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxStyleRunPropertiesDestination }

procedure TdxStyleRunPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := TdxCharacterFormattingBase(CharacterProperties);
  ACharacterFormatting.BeginUpdate;
end;

procedure TdxStyleRunPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := TdxCharacterFormattingBase(CharacterProperties);
  ACharacterFormatting.EndUpdate;
end;

{ TdxStyleTablePropertiesDestination }

procedure TdxStyleTablePropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Properties.BeginUpdate;
end;

procedure TdxStyleTablePropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  Properties.EndUpdate;
end;

{ TdxStyleTableRowPropertiesDestination }

procedure TdxStyleTableRowPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Properties.BeginUpdate;
end;

procedure TdxStyleTableRowPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  Properties.EndUpdate;
end;

{ TdxStyleTableCellPropertiesDestination }

procedure TdxStyleTableCellPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Properties.BeginUpdate;
end;

procedure TdxStyleTableCellPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  Properties.EndUpdate;
end;

{ TdxStyleLeafElementDestination }

constructor TdxStyleLeafElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AStyleDestination: TdxStyleDestination);
begin
  inherited Create(AImporter);
  Assert(AStyleDestination <> nil);
  FStyleDestination := AStyleDestination;
end;

{ TdxStyleParentIdDestination }

procedure TdxStyleParentIdDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.ParentId := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
end;

{ TdxStyleHiddenDestination }

procedure TdxStyleHiddenDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.Hidden := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxStyleSemiHiddenDestination }

procedure TdxStyleSemiHiddenDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.SemiHidden := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxStyleNameDestination }

procedure TdxStyleNameDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.StyleName := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
end;

{ TdxStyleQFormatDestination }

procedure TdxStyleQFormatDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.QFormat := Importer.GetWpSTOnOffValue(AReader, 'val', True);
end;

{ TdxLinkedStyleIdDestination }

procedure TdxLinkedStyleIdDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.LinkedStyleId := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
end;

{ TdxNextStyleDestination }

procedure TdxNextStyleDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  StyleDestination.NextStyleId := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
end;

{ TdxStyleConditionalTableFormatting }

constructor TdxStyleConditionalTableFormatting.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AStyleDestination: TdxStyleDestination);
begin
  inherited Create(AImporter);
  FConditionType := TdxConditionalTableStyleFormattingType.WholeTable;
  Assert(AStyleDestination <> nil);
  FStyleDestination := AStyleDestination;
end;

class constructor TdxStyleConditionalTableFormatting.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
  FCondtionTypesTable := CreateCondtionTypesTable;
end;

class destructor TdxStyleConditionalTableFormatting.Finalize;
begin
  FCondtionTypesTable.Free;
  FHandlerTable.Free;
end;

class function TdxStyleConditionalTableFormatting.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  FillElementHandlerTable(Result);
end;

class function TdxStyleConditionalTableFormatting.CreateCondtionTypesTable: TdxNamedOrdinalDictionary<TdxConditionalTableStyleFormattingType>;
begin
  Result := TdxNamedOrdinalDictionary<TdxConditionalTableStyleFormattingType>.Create;
  Result.Add('band1Horz', TdxConditionalTableStyleFormattingType.OddRowBanding);
  Result.Add('band1Vert', TdxConditionalTableStyleFormattingType.OddColumnBanding);
  Result.Add('band2Horz', TdxConditionalTableStyleFormattingType.EvenRowBanding);
  Result.Add('band2Vert', TdxConditionalTableStyleFormattingType.EvenColumnBanding);
  Result.Add('firstCol', TdxConditionalTableStyleFormattingType.FirstColumn);
  Result.Add('firstRow', TdxConditionalTableStyleFormattingType.FirstRow);
  Result.Add('lastCol', TdxConditionalTableStyleFormattingType.LastColumn);
  Result.Add('lastRow', TdxConditionalTableStyleFormattingType.LastRow);
  Result.Add('neCell', TdxConditionalTableStyleFormattingType.TopRightCell);
  Result.Add('nwCell', TdxConditionalTableStyleFormattingType.TopLeftCell);
  Result.Add('seCell', TdxConditionalTableStyleFormattingType.BottomRightCell);
  Result.Add('swCell', TdxConditionalTableStyleFormattingType.BottomLeftCell);
  Result.Add('wholeTable', TdxConditionalTableStyleFormattingType.WholeTable);
end;

function TdxStyleConditionalTableFormatting.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxStyleConditionalTableFormatting.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
begin
  inherited ProcessElementOpen(AReader);
  AValue := AReader.GetAttribute('type', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
    FConditionType := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxConditionalTableStyleFormattingType>(
      AValue, TdxOpenXmlExporter.ConditionalTableStyleFormattingTypesTable,
      TdxConditionalTableStyleFormattingType.WholeTable)
  else
    FConditionType := TdxConditionalTableStyleFormattingType.WholeTable;
end;

procedure TdxStyleConditionalTableFormatting.ProcessElementClose(AReader: TdxXmlReader);
begin
  inherited ProcessElementClose(AReader);
  FStyleInfo.ConditionType := FConditionType;
  FStyleDestination.AddStyleConditionalTableFormatting(FStyleInfo);
  FStyleInfo := nil
end;

end.

