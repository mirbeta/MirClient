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

unit dxRichEdit.Import.OpenXML.DestinationNumbering;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections, RegularExpressions,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationRunProperties,
  dxRichEdit.Import.OpenXML.DestinationParagraph;

type
  TdxMatch = class(TObject);


  { TdxNumberingsDestination }

  TdxNumberingsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnAbstractNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxAbstractNumberingListDestination }

  TdxAbstractNumberingListDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FList: TdxAbstractNumberingList;
    FId: string;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxAbstractNumberingListDestination; static;
    class function OnMultilevelType(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnName(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnUniqueId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumberingStyleLink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStyleLink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTemplate(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property List: TdxAbstractNumberingList read FList;
    property Id: string read FId write FId;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;

    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnLevel(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxOpenXmlListLevelOverride }

  TdxOpenXmlListLevelOverride = class
  strict private
    FLevelIndex: Integer;
    FDocumentModel: TdxDocumentModel;
    FLevel: TdxOverrideListLevel;
    FOverrideStart: Boolean;
    FNewStart: Integer;
    procedure SetNewStart(const AValue: Integer);
  protected
    procedure ApplyStartOverride(ALevel: TdxOverrideListLevel); virtual;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    function GetOverrideListLevelCore(AOriginalLevel: TdxNumberingListReferenceLevel): TdxAbstractListLevel; virtual;
    function GetOverrideListLevel: TdxOverrideListLevel;

    property LevelIndex: Integer read FLevelIndex write FLevelIndex;
    property NewStart: Integer read FNewStart write SetNewStart;
  end;

  { TdxOpenXmlListLevelOverrideCollection }

  TdxOpenXmlListLevelOverrideCollection = class(TdxList<TdxOpenXmlListLevelOverride>);

  { TdxOpenXmlNumberingListInfo }

  TdxOpenXmlNumberingListInfo = class
  strict private
    FId: Integer;
    FAbstractNumberingListId: string;
    FLevelOverrides: TdxOpenXmlListLevelOverrideCollection;
    FListIndex: TdxNumberingListIndex;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Integer read FId write FId;
    property AbstractNumberingListId: string read FAbstractNumberingListId write FAbstractNumberingListId;
    property ListIndex: TdxNumberingListIndex read FListIndex write FListIndex;
    property LevelOverrides: TdxOpenXmlListLevelOverrideCollection read FLevelOverrides;
  end;

  { TdxNumberingListDestination }

  TdxNumberingListDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FListInfo: TdxOpenXmlNumberingListInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxNumberingListDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnAbstractNumberingId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLevelOverride(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxAbstractNumberingLeafElementDestination }

  TdxAbstractNumberingLeafElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FList: TdxAbstractNumberingList;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AList: TdxAbstractNumberingList);

    property List: TdxAbstractNumberingList read FList;
  end;

  { TdxNumberingListNameDestination }

  TdxNumberingListNameDestination = class(TdxAbstractNumberingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxAbstractNumberingListUniqueIdDestination }

  TdxAbstractNumberingListUniqueIdDestination = class(TdxAbstractNumberingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxNumberingListMultiLevelTypeDestination }

  TdxNumberingListMultiLevelTypeDestination = class(TdxAbstractNumberingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxNumberingListNumStyleLinkDestination }

  TdxNumberingListNumStyleLinkDestination = class(TdxAbstractNumberingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxAbstractNumberingListReferenceDestination }

  TdxAbstractNumberingListReferenceDestination = class(TdxLeafElementDestination)
  strict private
    FListInfo: TdxOpenXmlNumberingListInfo;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AListInfo: TdxOpenXmlNumberingListInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelOverrideDestination }

  TdxListLevelOverrideDestination = class(TdxLeafElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FListInfo: TdxOpenXmlNumberingListInfo;
    FLevelOverride: TdxOpenXmlListLevelOverride;
    FOverrideRead: Boolean;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AListInfo: TdxOpenXmlNumberingListInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxListLevelOverrideDestination; static;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnLevelOverride(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLevelStartOverride(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxNumberingLevelBaseDestination }

  TdxNumberingLevelBaseDestination = class abstract(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetLevel: IdxListLevel; virtual; abstract;
    function GetLevelIndex: Integer; virtual; abstract;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxNumberingLevelBaseDestination; static;
    class function OnTextAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRestart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnText(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSuffix(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumberingFormat(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLegalNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLegacy(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property Level: IdxListLevel read GetLevel;
    property LevelIndex: Integer read GetLevelIndex;
  end;

  { TdxNumberingLevelDestination }

  TdxNumberingLevelDestination = class(TdxNumberingLevelBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    FList: TdxAbstractNumberingList;
    FLevelIndex: Integer;
    FLevel: TdxListLevel;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function GetLevel: IdxListLevel; override;
    function GetLevelIndex: Integer; override;
    class function OnParagraphStyleReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AList: TdxAbstractNumberingList);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelOverrideLevelDestination }

  TdxListLevelOverrideLevelDestination = class(TdxNumberingLevelBaseDestination)
  strict private
    FLevelOverride: TdxOpenXmlListLevelOverride;
    FLevel: IdxListLevel;
  protected
    function GetLevel: IdxListLevel; override;
    function GetLevelIndex: Integer; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ALevelOverride: TdxOpenXmlListLevelOverride);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelOverrideStartDestination }

  TdxListLevelOverrideStartDestination = class(TdxLeafElementDestination)
  strict private
    FLevelOverride: TdxOpenXmlListLevelOverride;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ALevelOverride: TdxOpenXmlListLevelOverride);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelElementDestination }

  TdxListLevelElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FLevel: IdxListLevel;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ALevel: IdxListLevel);

    property Level: IdxListLevel read FLevel;
  end;

  { TdxListLevelTextAlignmentDestination }

  TdxListLevelTextAlignmentDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelStartDestination }

  TdxListLevelStartDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelLegacyDestination }

  TdxListLevelLegacyDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelFormatStringDestination }

  TdxListLevelFormatStringDestination = class(TdxListLevelElementDestination)
  public
    const Pattern = '%(?<number>\d)';
  protected
    function ConvertMatch(const AMatch: TMatch): string; virtual;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    function ReplaceStringFormatInBulletNumbering(const AValue: string): string;
    function ReplaceStringFormatInSimpleNumbering: string;
    function ConvertFormatString(const AValue: string): string;
  end;

  { TdxListLevelRestartDestination }

  TdxListLevelRestartDestination = class(TdxListLevelElementDestination)
  strict private
    FLevelIndex: Integer;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ALevel: IdxListLevel; ALevelIndex: Integer);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelSuffixDestination }

  TdxListLevelSuffixDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelNumberingFormatDestination }

  TdxListLevelNumberingFormatDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelLegalNumberingDestination }

  TdxListLevelLegalNumberingDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelParagraphStyleReferenceDestination }

  TdxListLevelParagraphStyleReferenceDestination = class(TdxListLevelElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxListLevelRunPropertiesDestination }

  TdxListLevelRunPropertiesDestination = class(TdxRunPropertiesBaseDestination)
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ALevel: IdxListLevel);
  end;

  { TdxListLevelParagraphPropertiesDestination }

  TdxListLevelParagraphPropertiesDestination = class(TdxParagraphPropertiesBaseDestination)
  strict private
    FLevel: IdxListLevel;
  protected
    function GetNumberingId: Integer; override;
    procedure SetNumberingId(const AValue: Integer); override;
    function GetListLevelIndex: Integer; override;
    procedure SetListLevelIndex(const AValue: Integer); override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ALevel: IdxListLevel);
    destructor Destroy; override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxOpenXmlAbstractNumberingInfo }

  TdxOpenXmlAbstractNumberingInfo = class
  strict private
    FAbstractNumberingListId: string;
    FAbstractNumberingIndex: Integer;
    FType: TdxNumberingType;
  public
    property AbstractNumberingListId: string read FAbstractNumberingListId write FAbstractNumberingListId;
    property AbstractNumberingIndex: Integer read FAbstractNumberingIndex write FAbstractNumberingIndex;
    property ListType: TdxNumberingType read FType write FType;
  end;

  { TdxOpenXmlAbstractNumberingInfoCollection }

  TdxOpenXmlAbstractNumberingInfoCollection = class(TdxObjectList<TdxOpenXmlAbstractNumberingInfo>)
  public
    function FindById(const AId: string): TdxOpenXmlAbstractNumberingInfo;
  end;

  { TdxOpenXmlNumberingListInfoCollection }

  TdxOpenXmlNumberingListInfoCollection = class(TdxObjectList<TdxOpenXmlNumberingListInfo>)
  public
    function FindById(AId: Integer): TdxOpenXmlNumberingListInfo;
  end;

implementation

uses
  Math, Contnrs,
  dxCharacters,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.DocumentModel.Section;

{ TdxNumberingsDestination }

class constructor TdxNumberingsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxNumberingsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxNumberingsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('abstractNum', OnAbstractNumbering);
  Result.Add('num', OnNumbering);
end;

function TdxNumberingsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxNumberingsDestination.OnAbstractNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAbstractNumberingListDestination.Create(AImporter);
end;

class function TdxNumberingsDestination.OnNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNumberingListDestination.Create(AImporter);
end;

{ TdxAbstractNumberingListDestination }

constructor TdxAbstractNumberingListDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FList := TdxAbstractNumberingList.Create(AImporter.DocumentModel);
end;

class constructor TdxAbstractNumberingListDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxAbstractNumberingListDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxAbstractNumberingListDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('lvl', OnLevel);
  Result.Add('multiLevelType', OnMultilevelType);
  Result.Add('name', OnName);
  Result.Add('nsid', OnUniqueId);
  Result.Add('numStyleLink', OnNumberingStyleLink);
  Result.Add('styleLink', OnStyleLink);
  Result.Add('tmpl', OnTemplate);
end;

destructor TdxAbstractNumberingListDestination.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxAbstractNumberingListDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxAbstractNumberingListDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxAbstractNumberingListDestination;
begin
  Result := TdxAbstractNumberingListDestination(AImporter.PeekDestination);
end;

procedure TdxAbstractNumberingListDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FId := AReader.GetAttribute('abstractNumId', Importer.WordProcessingNamespaceConst);
end;

procedure TdxAbstractNumberingListDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ALists: TdxAbstractNumberingListCollection;
  AIndex: Integer;
  AInfo: TdxOpenXmlAbstractNumberingInfo;
begin
  if FId = '' then
    Exit;

  ALists := Importer.DocumentModel.AbstractNumberingLists;
  AIndex := ALists.Count;
  ALists.Add(FList);
  FList := nil;

  AInfo := TdxOpenXmlAbstractNumberingInfo.Create;
  AInfo.AbstractNumberingListId := FId;
  AInfo.AbstractNumberingIndex := AIndex;
  TdxWordProcessingMLBaseImporter(Importer).AbstractListInfos.Add(AInfo);
end;

class function TdxAbstractNumberingListDestination.OnLevel(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNumberingLevelDestination.Create(AImporter, GetThis(AImporter).List);
end;

class function TdxAbstractNumberingListDestination.OnMultilevelType(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNumberingListMultiLevelTypeDestination.Create(AImporter, GetThis(AImporter).List);
end;

class function TdxAbstractNumberingListDestination.OnName(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNumberingListNameDestination.Create(AImporter, GetThis(AImporter).List);
end;

class function TdxAbstractNumberingListDestination.OnUniqueId(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAbstractNumberingListUniqueIdDestination.Create(AImporter, GetThis(AImporter).List);
end;

class function TdxAbstractNumberingListDestination.OnNumberingStyleLink(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNumberingListNumStyleLinkDestination.Create(AImporter, GetThis(AImporter).List);
end;

class function TdxAbstractNumberingListDestination.OnStyleLink(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := nil;
end;

class function TdxAbstractNumberingListDestination.OnTemplate(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := nil;
end;

{ TdxOpenXmlListLevelOverride }

constructor TdxOpenXmlListLevelOverride.Create(ADocumentModel: TdxDocumentModel);
begin
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
end;

procedure TdxOpenXmlListLevelOverride.SetNewStart(const AValue: Integer);
begin
  FNewStart := AValue;
  FOverrideStart := True;
  if FLevel <> nil then
    ApplyStartOverride(FLevel);
end;

function TdxOpenXmlListLevelOverride.GetOverrideListLevel: TdxOverrideListLevel;
begin
  if FLevel = nil then
  begin
    FLevel := TdxOverrideListLevel.Create(FDocumentModel);
    ApplyStartOverride(FLevel);
  end;
  Result := FLevel;
end;

procedure TdxOpenXmlListLevelOverride.ApplyStartOverride(ALevel: TdxOverrideListLevel);
begin
  Assert(ALevel <> nil);
  if FOverrideStart then
  begin
    ALevel.SetOverrideStart(True);
    ALevel.ListLevelProperties.Start := FNewStart;
  end;
end;

function TdxOpenXmlListLevelOverride.GetOverrideListLevelCore(AOriginalLevel: TdxNumberingListReferenceLevel): TdxAbstractListLevel;
begin
  if FLevel <> nil then
    Exit(FLevel)
  else
    if FOverrideStart then
    begin
      AOriginalLevel.SetOverrideStart(True);
      AOriginalLevel.NewStart := FNewStart;
    end;
  Result := AOriginalLevel;
end;

{ TdxOpenXmlNumberingListInfo }

constructor TdxOpenXmlNumberingListInfo.Create;
begin
  FLevelOverrides := TdxOpenXmlListLevelOverrideCollection.Create;
end;

destructor TdxOpenXmlNumberingListInfo.Destroy;
begin
  FLevelOverrides.Free;
  inherited Destroy;
end;

{ TdxNumberingListDestination }

constructor TdxNumberingListDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FListInfo := TdxOpenXmlNumberingListInfo.Create;
end;

destructor TdxNumberingListDestination.Destroy;
begin
  FListInfo.Free;
  inherited Destroy;
end;

class constructor TdxNumberingListDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxNumberingListDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxNumberingListDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('abstractNumId', OnAbstractNumberingId);
  Result.Add('lvlOverride', OnLevelOverride);
end;

function TdxNumberingListDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxNumberingListDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxNumberingListDestination;
begin
  Result := TdxNumberingListDestination(AImporter.PeekDestination);
end;

procedure TdxNumberingListDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FListInfo.Id := Importer.GetWpSTIntegerValue(AReader, 'numId', MinInt);
end;

procedure TdxNumberingListDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if (FListInfo.AbstractNumberingListId = '') or (FListInfo.Id = MinInt) then
    Exit;

  TdxWordProcessingMLBaseImporter(Importer).ListInfos.Add(FListInfo);
  FListInfo := nil;
end;

class function TdxNumberingListDestination.OnAbstractNumberingId(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAbstractNumberingListReferenceDestination.Create(AImporter, GetThis(AImporter).FListInfo);
end;

class function TdxNumberingListDestination.OnLevelOverride(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelOverrideDestination.Create(AImporter, GetThis(AImporter).FListInfo);
end;

{ TdxAbstractNumberingLeafElementDestination }

constructor TdxAbstractNumberingLeafElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AList: TdxAbstractNumberingList);
begin
  inherited Create(AImporter);
  Assert(AList <> nil);
  FList := AList;
end;

{ TdxNumberingListNameDestination }

procedure TdxNumberingListNameDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;

{ TdxAbstractNumberingListUniqueIdDestination }

procedure TdxAbstractNumberingListUniqueIdDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val', TdxNumberStyles.HexNumber, MinInt);
  if AValue <> MinInt then
    List.SetId(Abs(AValue));
end;

{ TdxNumberingListMultiLevelTypeDestination }

procedure TdxNumberingListMultiLevelTypeDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AListType: TdxNumberingType;
begin
  AListType := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxNumberingType>(AReader, 'val',
    TdxOpenXmlExporter.NumberingListTypeTable, TdxNumberingType.Simple);
  if AListType <> TdxNumberingType.MultiLevel then
    TdxNumberingListHelper.SetHybridListType(List);
end;

{ TdxNumberingListNumStyleLinkDestination }

procedure TdxNumberingListNumStyleLinkDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AStyleId: string;
  AStyleInfo: TdxOpenXmlStyleInfo;
begin
  AStyleId := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  AStyleInfo := TdxWordProcessingMLBaseImporter(Importer).NumberingStyleInfos.LookupStyleById(AStyleId);
  if AStyleInfo <> nil then
    List.SetNumberingStyleReferenceIndex(AStyleInfo.StyleIndex);
end;

{ TdxAbstractNumberingListReferenceDestination }

constructor TdxAbstractNumberingListReferenceDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AListInfo: TdxOpenXmlNumberingListInfo);
begin
  inherited Create(AImporter);
  Assert(AListInfo <> nil);
  FListInfo := AListInfo;
end;

procedure TdxAbstractNumberingListReferenceDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FListInfo.AbstractNumberingListId := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
end;

{ TdxListLevelOverrideDestination }

constructor TdxListLevelOverrideDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AListInfo: TdxOpenXmlNumberingListInfo);
begin
  inherited Create(AImporter);
  Assert(AListInfo <> nil);
  FListInfo := AListInfo;
  FLevelOverride := TdxOpenXmlListLevelOverride.Create(AImporter.DocumentModel);
  AImporter.AddToGarbageCollector(FLevelOverride);
end;

class constructor TdxListLevelOverrideDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxListLevelOverrideDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxListLevelOverrideDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('lvl', OnLevelOverride);
  Result.Add('startOverride', OnLevelStartOverride);
end;

function TdxListLevelOverrideDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxListLevelOverrideDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FLevelOverride.LevelIndex := Importer.GetWpSTIntegerValue(AReader, 'ilvl', MinInt);
end;

class function TdxListLevelOverrideDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxListLevelOverrideDestination;
begin
  Result := TdxListLevelOverrideDestination(AImporter.PeekDestination);
end;

procedure TdxListLevelOverrideDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if (FLevelOverride.LevelIndex < 0) or (FLevelOverride.LevelIndex >= 9) or not FOverrideRead then
    Exit;

  FListInfo.LevelOverrides.Add(FLevelOverride);
end;

class function TdxListLevelOverrideDestination.OnLevelOverride(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AThisObject: TdxListLevelOverrideDestination;
begin
  AThisObject := GetThis(AImporter);
  AThisObject.FOverrideRead := True;
  Result := TdxListLevelOverrideLevelDestination.Create(AImporter, AThisObject.FLevelOverride);
end;

class function TdxListLevelOverrideDestination.OnLevelStartOverride(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AThisObject: TdxListLevelOverrideDestination;
begin
  AThisObject := GetThis(AImporter);
  AThisObject.FOverrideRead := True;
  Result := TdxListLevelOverrideStartDestination.Create(AImporter, AThisObject.FLevelOverride);
end;

{ TdxNumberingLevelBaseDestination }

class constructor TdxNumberingLevelBaseDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxNumberingLevelBaseDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxNumberingLevelBaseDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('isLgl', OnLegalNumbering);

  Result.Add('lvlJc', OnTextAlignment);

  Result.Add('lvlRestart', OnRestart);
  Result.Add('lvlText', OnText);
  Result.Add('start', OnStart);
  Result.Add('suff', OnSuffix);
  Result.Add('numFmt', OnNumberingFormat);
  Result.Add('pPr', OnParagraphProperties);
  Result.Add('rPr', OnRunProperties);
  Result.Add('legacy', OnLegacy);
end;

function TdxNumberingLevelBaseDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxNumberingLevelBaseDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxNumberingLevelBaseDestination;
begin
  Result := TdxNumberingLevelBaseDestination(AImporter.PeekDestination);
end;

class function TdxNumberingLevelBaseDestination.OnTextAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelTextAlignmentDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnRestart(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelRestartDestination.Create(AImporter, GetThis(AImporter).Level, GetThis(AImporter).LevelIndex);
end;

class function TdxNumberingLevelBaseDestination.OnText(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelFormatStringDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelStartDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnSuffix(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelSuffixDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnNumberingFormat(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelNumberingFormatDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelParagraphPropertiesDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelRunPropertiesDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnLegalNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelLegalNumberingDestination.Create(AImporter, GetThis(AImporter).Level);
end;

class function TdxNumberingLevelBaseDestination.OnLegacy(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelLegacyDestination.Create(AImporter, GetThis(AImporter).Level);
end;

{ TdxNumberingLevelDestination }

constructor TdxNumberingLevelDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AList: TdxAbstractNumberingList);
begin
  inherited Create(AImporter);
  Assert(AList <> nil);
  FList := AList;
end;

destructor TdxNumberingLevelDestination.Destroy;
begin
  FLevel.Free;
  inherited Destroy;
end;

class constructor TdxNumberingLevelDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxNumberingLevelDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxNumberingLevelDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxNumberingLevelBaseDestination.CreateElementHandlerTable;
  Result.Add('pStyle', OnParagraphStyleReference);
end;

function TdxNumberingLevelDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxNumberingLevelDestination.GetLevel: IdxListLevel;
begin
  Result := FLevel;
end;

function TdxNumberingLevelDestination.GetLevelIndex: Integer;
begin
  Result := FLevelIndex;
end;

procedure TdxNumberingLevelDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FLevelIndex := Importer.GetWpSTIntegerValue(AReader, 'ilvl', 0);
  Assert(FLevel = nil);
  FLevel := FList.CreateLevel(FLevelIndex) as TdxListLevel;
  FLevel.ListLevelProperties.TemplateCode := Importer.GetWpSTIntegerValue(AReader, 'tplc', TdxNumberStyles.HexNumber, 0);
end;

procedure TdxNumberingLevelDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if (FLevelIndex < 0) or (FLevelIndex >= 9) then
    Exit;
  if FList.NumberingStyleReferenceIndex >= 0 then
    Exit;
  FList.Levels[FLevelIndex] := FLevel;
  FLevel := nil;
end;

class function TdxNumberingLevelDestination.OnParagraphStyleReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxListLevelParagraphStyleReferenceDestination.Create(AImporter, TdxListLevel(GetThis(AImporter).Level));
end;

{ TdxListLevelOverrideLevelDestination }

constructor TdxListLevelOverrideLevelDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  ALevelOverride: TdxOpenXmlListLevelOverride);
begin
  inherited Create(AImporter);
  Assert(ALevelOverride <> nil);
  FLevelOverride := ALevelOverride;
  FLevel := ALevelOverride.GetOverrideListLevel;
end;

destructor TdxListLevelOverrideLevelDestination.Destroy;
begin
  FLevel := nil;
  inherited Destroy;
end;

function TdxListLevelOverrideLevelDestination.GetLevel: IdxListLevel;
begin
  Result := FLevel;
end;

function TdxListLevelOverrideLevelDestination.GetLevelIndex: Integer;
begin
  Result := FLevelOverride.LevelIndex;
end;

procedure TdxListLevelOverrideLevelDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  inherited ProcessElementOpen(AReader);
  FLevel.ListLevelProperties.TemplateCode := Importer.GetWpSTIntegerValue(AReader, 'tplc', TdxNumberStyles.HexNumber, 0);
end;

{ TdxListLevelOverrideStartDestination }

constructor TdxListLevelOverrideStartDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  ALevelOverride: TdxOpenXmlListLevelOverride);
begin
  inherited Create(AImporter);
  Assert(ALevelOverride <> nil);
  FLevelOverride := ALevelOverride;
end;

procedure TdxListLevelOverrideStartDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FLevelOverride.NewStart := Importer.GetWpSTIntegerValue(AReader, 'val', 0);
end;

{ TdxListLevelElementDestination }

constructor TdxListLevelElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ALevel: IdxListLevel);
begin
  inherited Create(AImporter);
  Assert(ALevel <> nil);
  FLevel := ALevel;
end;

{ TdxListLevelTextAlignmentDestination }

procedure TdxListLevelTextAlignmentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Level.ListLevelProperties.Alignment := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxListNumberAlignment>(
    AReader, 'val', TdxOpenXmlExporter.ListNumberAlignmentTable, TdxListNumberAlignment.Left);
end;

{ TdxListLevelStartDestination }

procedure TdxListLevelStartDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Level.ListLevelProperties.Start := Max(0, Importer.GetWpSTIntegerValue(AReader, 'val'));
end;

{ TdxListLevelLegacyDestination }

procedure TdxListLevelLegacyDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Level.ListLevelProperties.Legacy := True;
  Level.ListLevelProperties.LegacySpace := UnitConverter.TwipsToModelUnits(Importer.GetWpSTIntegerValue(AReader, 'legacySpace', 0));
  Level.ListLevelProperties.LegacyIndent := UnitConverter.TwipsToModelUnits(Importer.GetWpSTIntegerValue(AReader, 'legacyIndent', 0));
end;

{ TdxListLevelFormatStringDestination }

procedure TdxListLevelFormatStringDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AFormatString: string;
begin
  AFormatString := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);

  if not TdxDocumentFormatsHelper.ShouldInsertBulletedNumbering(DocumentModel) then
    AFormatString := ReplaceStringFormatInBulletNumbering(AFormatString);
  if TdxDocumentFormatsHelper.NeedReplaceSimpleToBulletNumbering(DocumentModel) then
  begin
    AFormatString := ReplaceStringFormatInSimpleNumbering;
    Level.CharacterProperties.FontName := 'Symbol';
  end;
  Level.ListLevelProperties.DisplayFormatString := ConvertFormatString(AFormatString);
end;

function TdxListLevelFormatStringDestination.ReplaceStringFormatInBulletNumbering(const AValue: string): string;
var
  AResult: string;
begin
  AResult := ConvertFormatString(AValue);
  if AResult = AValue then
    Exit('%1.');
  Result := AValue;
end;

function TdxListLevelFormatStringDestination.ReplaceStringFormatInSimpleNumbering: string;
begin
  Result := TdxCharacters.MiddleDot;
end;

function TdxListLevelFormatStringDestination.ConvertFormatString(const AValue: string): string;
var
  ARegex: TRegex;
  AConvertMatch: TMatchEvaluator;
begin
  ARegex := TRegex.Create(Pattern);
  AConvertMatch := ConvertMatch;
  Result := ARegex.Replace(AValue, AConvertMatch);
end;

function TdxListLevelFormatStringDestination.ConvertMatch(const AMatch: TMatch): string;
var
  AValue: string;
  ANumber: Integer;
begin
  AValue := AMatch.Groups['number'].Value;
  if TdxNumber.TryParse(AValue, ANumber) and (ANumber > 0) then
    AValue := IntToStr(ANumber - 1);
  Result := '%' + AValue + ':s';
end;

{ TdxListLevelRestartDestination }

constructor TdxListLevelRestartDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const ALevel: IdxListLevel; ALevelIndex: Integer);
begin
  inherited Create(AImporter, ALevel);
  FLevelIndex := ALevelIndex;
end;

procedure TdxListLevelRestartDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AIndex: Integer;
begin
  AIndex := Min(9, Math.Max(1, Importer.GetWpSTIntegerValue(AReader, 'val'))) - 1;
  Level.ListLevelProperties.RelativeRestartLevel := FLevelIndex - AIndex;
end;

{ TdxListLevelSuffixDestination }

procedure TdxListLevelSuffixDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Level.ListLevelProperties.Separator := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<Char>(
    AReader, 'val', TdxOpenXmlExporter.ListNumberSeparatorTable, TdxCharacters.TabMark);
end;

{ TdxListLevelNumberingFormatDestination }

procedure TdxListLevelNumberingFormatDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AFormat: TdxNumberingFormat;
begin
  AFormat := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxNumberingFormat>(AReader, 'val',
    TdxOpenXmlExporter.PageNumberingFormatTable, TdxNumberingFormat.Decimal);
  if (AFormat = TdxNumberingFormat.Bullet) and TdxDocumentFormatsHelper.NeedReplaceBulletedLevelsToDecimal(DocumentModel) then
    if TdxDocumentFormatsHelper.ShouldInsertSimpleNumbering(DocumentModel) then
      AFormat := TdxNumberingFormat.Decimal
    else
      AFormat := TdxNumberingFormat.None
  else
    if (AFormat = TdxNumberingFormat.Decimal) and TdxDocumentFormatsHelper.NeedReplaceSimpleToBulletNumbering(DocumentModel) then
      if TdxDocumentFormatsHelper.ShouldInsertBulletedNumbering(DocumentModel) then
        AFormat := TdxNumberingFormat.Bullet
      else
        AFormat := TdxNumberingFormat.None;

  Level.ListLevelProperties.Format := AFormat;
end;

{ TdxListLevelLegalNumberingDestination }

procedure TdxListLevelLegalNumberingDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Level.ListLevelProperties.ConvertPreviousLevelNumberingToDecimal := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxListLevelParagraphStyleReferenceDestination }

procedure TdxListLevelParagraphStyleReferenceDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AStyleIndex: Integer;
begin
  AStyleIndex := TdxWordProcessingMLBaseImporter(Importer).LookupParagraphStyleIndex(AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst));
  if AStyleIndex >= 0 then
    TdxListLevel(Level).ParagraphStyleIndex := AStyleIndex;
end;

{ TdxListLevelRunPropertiesDestination }

constructor TdxListLevelRunPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ALevel: IdxListLevel);
begin
  inherited Create(AImporter, ALevel.CharacterProperties);
end;

{ TdxListLevelParagraphPropertiesDestination }

constructor TdxListLevelParagraphPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const ALevel: IdxListLevel);
begin
  inherited Create(AImporter, ALevel.ParagraphProperties, TdxTabFormattingInfo.Create);
  FLevel := ALevel;
end;

destructor TdxListLevelParagraphPropertiesDestination.Destroy;
begin
  Tabs.Free;
  inherited Destroy;
end;

function TdxListLevelParagraphPropertiesDestination.GetNumberingId: Integer;
begin
  Result := -1;
end;

procedure TdxListLevelParagraphPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  inherited ProcessElementClose(AReader);
  if Tabs.Count > 0 then
    FLevel.Tabs.SetTabs(Tabs);
end;

procedure TdxListLevelParagraphPropertiesDestination.SetNumberingId(const AValue: Integer);
begin
end;

function TdxListLevelParagraphPropertiesDestination.GetListLevelIndex: Integer;
begin
  Result := -1;
end;

procedure TdxListLevelParagraphPropertiesDestination.SetListLevelIndex(const AValue: Integer);
begin
end;

{ TdxOpenXmlAbstractNumberingInfoCollection }

function TdxOpenXmlAbstractNumberingInfoCollection.FindById(const AId: string): TdxOpenXmlAbstractNumberingInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].AbstractNumberingListId = AId then
      Exit(Items[I]);

  Result := nil;
end;

{ TdxOpenXmlNumberingListInfoCollection }

function TdxOpenXmlNumberingListInfoCollection.FindById(AId: Integer): TdxOpenXmlNumberingListInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Id = AId then
      Exit(Items[I]);

  Result := nil;
end;

end.

