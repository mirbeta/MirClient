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

unit dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import.Core,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationNumbering,
  dxRichEdit.Import.OpenXML.DestinationParagraph,
  dxRichEdit.Import.OpenXML.DestinationRun,
  dxRichEdit.Import.OpenXML.DestinationSection,
  dxRichEdit.Import.OpenXML.DestinationStyles,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter;

type

  { TdxWordProcessingMLBaseImporter }

  TdxWordProcessingMLBaseImporter = class abstract(TdxRichEditDestinationAndXmlBasedImporter)
  strict private
    FParagraphStyleInfos: TdxOpenXmlStyleInfoCollection;
    FCharacterStyleInfos: TdxOpenXmlStyleInfoCollection;
    FTableStyleInfosStack: TdxObjectStack<TdxOpenXmlStyleInfoCollection>;
    FTableCellStyleInfosStack: TdxObjectStack<TdxOpenXmlStyleInfoCollection>;
    FAbstractListInfos: TdxOpenXmlAbstractNumberingInfoCollection;
    FListInfos: TdxOpenXmlNumberingListInfoCollection;
    FNumberingStyleInfos: TdxOpenXmlStyleInfoCollection;
    FLockAspectRatioTable: TdxLockAspectRatioTable;
    FTableDepth: Integer;
    function GetTableStyleInfos: TdxOpenXmlStyleInfoCollection;
    function GetTableCellStyleInfos: TdxOpenXmlStyleInfoCollection;
    function GetInsideTable: Boolean;
  protected
    function GetOfficeNamespace: string; virtual; abstract;

    function GetColorValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor; overload;
    function GetPresetColorValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor; overload;

    function ParseColor(const AValue: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor; virtual;
    function ParsePresetColor(const AValue: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor; virtual;
    function LookupStyleIndexCore(const AStyleId: string; AStyleInfos: TdxOpenXmlStyleInfoCollection): Integer; virtual;
    procedure CreateStylesHierarchy; virtual;
    procedure CreateStylesHierarchyCore(AStyles: TdxStyleCollectionBase; AStyleInfos: TdxOpenXmlStyleInfoCollection); virtual;
    procedure LinkStyles; virtual;
    procedure CreateNumberingLists; virtual;
    procedure AppendNumberingList(AListInfo: TdxOpenXmlNumberingListInfo; AAbstractNumberingListIndex: Integer); virtual;
    procedure LinkParagraphStylesWithNumberingLists; virtual;
    procedure LinkNumberingListStyles; virtual;
    procedure LinkParagraphStyleWithNumberingLists(AStyleInfo: TdxOpenXmlStyleInfo); virtual;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions); override;
    destructor Destroy; override;

    function CreateOpenXmlSectionTextDirectionDestination: TdxSectionTextDirectionDestination; virtual; abstract;
    function CreateWordMLSectionTextDirectionDestination: TdxSectionTextDirectionDestination; virtual; abstract;
    function CreateBookmarkStartElementDestination(AReader: TdxXmlReader): TdxDestination; virtual;
    function CreateBookmarkEndElementDestination(AReader: TdxXmlReader): TdxDestination; virtual;

    function CreateRunDestination: TdxRunDestination; virtual;
    function CreateVersionDestination(AReader: TdxXmlReader): TdxDestination; virtual;
    function CreateParagraphDestination: TdxParagraphDestination; virtual;
    function CreateStyleParagraphPropertiesDestination(AStyleDestination: TdxStyleDestinationBase;
      AParagraphFormatting: TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo): TdxParagraphPropertiesBaseDestination; virtual; abstract;

    function GetEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TDictionary<T, TdxWordProcessingMLValue>; ADefaultValue: T): T; overload;
    function GetEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxMLDictionary<T>; ADefaultValue: T): T; overload;

    function GetColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor; overload;
    function GetPresetColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor; overload;

    function GetWpEnumValueCore<T>(const AValue: string; ATable: TDictionary<T, TdxWordProcessingMLValue>; ADefaultValue: T): T; overload;
    function GetWpEnumValueCore<T>(const AValue: string; ATable: TdxMLDictionary<T>; ADefaultValue: T): T; overload;

    function GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TDictionary<T, TdxWordProcessingMLValue>; ADefaultValue: T): T; overload;
    function GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string; ATable: TdxMLDictionary<T>; ADefaultValue: T): T; overload;

    function GetWpSTColorValue(AReader: TdxXmlReader; const AAttributeName: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor; overload;
    function GetWpSTColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor; overload;
    function GetMSWordColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor;
    function GetWpSTColorOrNullValue(AReader: TdxXmlReader; const AAttributeName: string): TdxNullableValue<TdxAlphaColor>;

    function GetShadingPatternDef(AReader: TdxXmlReader; const AAttributeName: string; ADefault: TdxShadingPattern): TdxShadingPattern;

    function ReadToRootElement(AReader: TdxXmlReader; const AName: string): Boolean; override;
    function ReadAttribute(AReader: TdxXmlReader; const AAttributeName: string): string; override;
    function ReadAttribute(AReader: TdxXmlReader; const AAttributeName: string; const ANs: string): string; override;
    procedure LockAspectRatioTableAddValue(const AKey: string; const AValue: string);
    function LockAspectRatioTableGetValue(const AKey: string; out AUseLockAspectRatio: Boolean): Boolean;
    function LookupCharacterStyleIndex(const AStyleId: string): Integer; override;
    function LookupParagraphStyleIndex(const AStyleId: string): Integer; virtual;
    function LookupTableStyleIndex(const AStyleId: string): Integer; virtual;
    function LookupTableCellStyleIndex(const AStyleId: string): Integer; virtual;
    function GetBoolValue(const AValue: string): Boolean;
    procedure BeginTable;
    procedure EndTable;
    function RegisterFootNote(ANote: TdxFootNote; const AId: string): Integer; virtual; abstract;
    function RegisterEndNote(ANote: TdxEndNote; const AId: string): Integer; virtual; abstract;

    property ParagraphStyleInfos: TdxOpenXmlStyleInfoCollection read FParagraphStyleInfos;
    property CharacterStyleInfos: TdxOpenXmlStyleInfoCollection read FCharacterStyleInfos;
    property TableStyleInfosStack: TdxObjectStack<TdxOpenXmlStyleInfoCollection> read FTableStyleInfosStack;
    property TableStyleInfos: TdxOpenXmlStyleInfoCollection read GetTableStyleInfos;
    property TableCellStyleInfosStack: TdxObjectStack<TdxOpenXmlStyleInfoCollection> read FTableCellStyleInfosStack;
    property TableCellStyleInfos: TdxOpenXmlStyleInfoCollection read GetTableCellStyleInfos;
    property NumberingStyleInfos: TdxOpenXmlStyleInfoCollection read FNumberingStyleInfos;
    property AbstractListInfos: TdxOpenXmlAbstractNumberingInfoCollection read FAbstractListInfos;
    property ListInfos: TdxOpenXmlNumberingListInfoCollection read FListInfos;
    property LockAspectRatioTable: TdxLockAspectRatioTable read FLockAspectRatioTable;
    property OfficeNamespace: string read GetOfficeNamespace;
    property InsideTable: Boolean read GetInsideTable;
  end;

implementation

uses
  Contnrs, Math, StrUtils,
  dxStringHelper,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Import.OpenXML.DestinationDocument,
  dxRichEdit.Import.OpenXML.DestinationBookmark,
  dxRichEdit.Utils.NumberParser;

{ TdxWordProcessingMLBaseImporter }

constructor TdxWordProcessingMLBaseImporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FLockAspectRatioTable := TdxLockAspectRatioTable.Create;
  FParagraphStyleInfos := TdxOpenXmlStyleInfoCollection.Create;
  FCharacterStyleInfos := TdxOpenXmlStyleInfoCollection.Create;
  FAbstractListInfos := TdxOpenXmlAbstractNumberingInfoCollection.Create;
  FListInfos := TdxOpenXmlNumberingListInfoCollection.Create;
  FTableStyleInfosStack := TdxObjectStack<TdxOpenXmlStyleInfoCollection>.Create(True);
  FTableStyleInfosStack.Push(TdxOpenXmlStyleInfoCollection.Create);
  FTableCellStyleInfosStack := TdxObjectStack<TdxOpenXmlStyleInfoCollection>.Create(True);
  FTableCellStyleInfosStack.Push(TdxOpenXmlStyleInfoCollection.Create);
  FNumberingStyleInfos := TdxOpenXmlStyleInfoCollection.Create;
end;

destructor TdxWordProcessingMLBaseImporter.Destroy;
begin
  FreeAndNil(FParagraphStyleInfos);
  FreeAndNil(FCharacterStyleInfos);
  FreeAndNil(FTableStyleInfosStack);
  FreeAndNil(FTableCellStyleInfosStack);
  FreeAndNil(FAbstractListInfos);
  FreeAndNil(FListInfos);
  FreeAndNil(FNumberingStyleInfos);
  FreeAndNil(FLockAspectRatioTable);
  inherited Destroy
end;

function TdxWordProcessingMLBaseImporter.GetTableStyleInfos: TdxOpenXmlStyleInfoCollection;
begin
  Result := FTableStyleInfosStack.Peek;
end;

function TdxWordProcessingMLBaseImporter.GetTableCellStyleInfos: TdxOpenXmlStyleInfoCollection;
begin
  Result := FTableCellStyleInfosStack.Peek;
end;

function TdxWordProcessingMLBaseImporter.GetInsideTable: Boolean;
begin
  Result := FTableDepth > 0;
end;

function TdxWordProcessingMLBaseImporter.ReadAttribute(AReader: TdxXmlReader; const AAttributeName: string): string;
begin
  Result := ReadAttribute(AReader, AAttributeName, WordProcessingNamespaceConst);
end;

function TdxWordProcessingMLBaseImporter.ReadAttribute(AReader: TdxXmlReader; const AAttributeName: string; const ANs: string): string;
begin
  Result := AReader.GetAttribute(AAttributeName, ANs);
end;

function TdxWordProcessingMLBaseImporter.GetWpSTColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor;
begin
  Result := GetWpSTColorValue(AReader, AAttributeName, TdxAlphaColors.Empty);
end;

function TdxWordProcessingMLBaseImporter.GetWpSTColorValue(AReader: TdxXmlReader; const AAttributeName: string;
  ADefaultValue: TdxAlphaColor): TdxAlphaColor;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName, WordProcessingNamespaceConst);
  if AValue <> '' then
    Result := ParseColor(AValue, ADefaultValue)
  else
    Result := ADefaultValue;
end;

function TdxWordProcessingMLBaseImporter.GetWpSTColorOrNullValue(AReader: TdxXmlReader; const AAttributeName: string): TdxNullableValue<TdxAlphaColor>;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName, WordProcessingNamespaceConst);
  if AValue <> '' then
    Result := ParseColor(AValue, TdxAlphaColors.Empty)
  else
    Result.Reset;
end;

function TdxWordProcessingMLBaseImporter.GetColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor;
begin
  Result := GetColorValue(AReader, AAttributeName, TdxAlphaColors.Empty);
end;

function TdxWordProcessingMLBaseImporter.GetPresetColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor;
begin
  Result := GetPresetColorValue(AReader, AAttributeName, TdxAlphaColors.Empty);
end;

function TdxWordProcessingMLBaseImporter.GetMSWordColorValue(AReader: TdxXmlReader; const AAttributeName: string): TdxAlphaColor;
var
  AValue: string;
  AMLValue: TdxWordProcessingMLValue;
  AKey: TdxAlphaColor;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue = '' then
    Exit(TdxAlphaColors.Empty);

  AValue := StringReplace(AValue, '#', '', [rfReplaceAll]);
  AValue := Trim(TdxStringHelper.RemoveEnclosedPart(AValue, '[', ']'));

  AMLValue := TdxWordProcessingMLValue.Create(AValue);
  for AKey in TdxWordProcessingMLBaseExporter.PredefinedBackgroundColors.Keys do
    if AMLValue.OpenXmlValue = TdxWordProcessingMLBaseExporter.PredefinedBackgroundColors[AKey].OpenXmlValue then
      Exit(AKey);

  if Length(AValue) >= 6 then
    AValue := TdxStringHelper.Substring(AValue, 0, 6)
  else
    if Length(AValue) = 3 then
      AValue := Format('%0:s%0:s%1:s%1:s%2:s%2:s', [AValue[1], AValue[2], AValue[3]])
    else
      AValue := DupeString('0', 6 - Length(AValue)) + AValue;

  Result := ParseColor(AValue, TdxAlphaColors.Empty);
end;

function TdxWordProcessingMLBaseImporter.GetShadingPatternDef(AReader: TdxXmlReader; const AAttributeName: string;
  ADefault: TdxShadingPattern): TdxShadingPattern;
begin
  Result := GetWpEnumValue<TdxShadingPattern>(AReader, AAttributeName, TdxWordProcessingMLBaseExporter.ShadingPatternTable, ADefault);
end;

function TdxWordProcessingMLBaseImporter.GetColorValue(AReader: TdxXmlReader; const AAttributeName: string;
  ADefaultValue: TdxAlphaColor): TdxAlphaColor;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue <> '' then
    Result := ParseColor(AValue, ADefaultValue)
  else
    Result := ADefaultValue;
end;

function TdxWordProcessingMLBaseImporter.GetPresetColorValue(AReader: TdxXmlReader; const AAttributeName: string;
  ADefaultValue: TdxAlphaColor): TdxAlphaColor;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue <> '' then
    Result := ParsePresetColor(AValue, ADefaultValue)
  else
    Result := ADefaultValue;
end;

function TdxWordProcessingMLBaseImporter.GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TDictionary<T, TdxWordProcessingMLValue>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName, WordProcessingNamespaceConst);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxWordProcessingMLBaseImporter.GetWpEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TdxMLDictionary<T>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName, WordProcessingNamespaceConst);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxWordProcessingMLBaseImporter.GetEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TDictionary<T, TdxWordProcessingMLValue>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxWordProcessingMLBaseImporter.GetEnumValue<T>(AReader: TdxXmlReader; const AAttributeName: string;
  ATable: TdxMLDictionary<T>; ADefaultValue: T): T;
var
  AValue: string;
begin
  AValue := AReader.GetAttribute(AAttributeName);
  if AValue = '' then
    Result := ADefaultValue
  else
    Result := GetWpEnumValueCore<T>(AValue, ATable, ADefaultValue);
end;

function TdxWordProcessingMLBaseImporter.GetWpEnumValueCore<T>(const AValue: string;
  ATable: TDictionary<T, TdxWordProcessingMLValue>; ADefaultValue: T): T;
var
  AKey: T;
  AValueString: TdxWordProcessingMLValue;
begin
  for AKey in ATable.Keys do
  begin
    AValueString := ATable[AKey];
    if (AValue = AValueString.OpenXmlValue) or (AValue = AValueString.WordMLValue) then
      Exit(AKey);
  end;
  Result := ADefaultValue;
end;

function TdxWordProcessingMLBaseImporter.GetWpEnumValueCore<T>(const AValue: string;
  ATable: TdxMLDictionary<T>; ADefaultValue: T): T;
begin
  Result := ATable.GetKeyByStringDef(AValue, ADefaultValue);
end;

function TdxWordProcessingMLBaseImporter.ParseColor(const AValue: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor;
var
  AResult: TdxAlphaColor;
begin
  if AValue = 'auto' then
    Exit(ADefaultValue);

  AResult := TdxMarkupLanguageColorParser.ParseColor(AValue);
  if AResult = TdxAlphaColors.Empty then
    Result := ADefaultValue
  else
    Result := AResult;
end;

function TdxWordProcessingMLBaseImporter.ParsePresetColor(const AValue: string; ADefaultValue: TdxAlphaColor): TdxAlphaColor;
begin
  if not TdxWordProcessingMLBaseExporter.PresetColors.TryGetValue(AValue, Result) then
    Result := ADefaultValue;
end;

function TdxWordProcessingMLBaseImporter.ReadToRootElement(AReader: TdxXmlReader; const AName: string): Boolean;
begin
  Result := ReadToRootElement(AReader, AName, WordProcessingNamespaceConst);
end;

function TdxWordProcessingMLBaseImporter.LookupParagraphStyleIndex(const AStyleId: string): Integer;
begin
  Result := LookupStyleIndexCore(AStyleId, ParagraphStyleInfos);
end;

function TdxWordProcessingMLBaseImporter.LookupCharacterStyleIndex(const AStyleId: string): Integer;
begin
  Result := LookupStyleIndexCore(AStyleId, CharacterStyleInfos);
end;

function TdxWordProcessingMLBaseImporter.LookupTableStyleIndex(const AStyleId: string): Integer;
begin
  Result := LookupStyleIndexCore(AStyleId, TableStyleInfos);
end;

function TdxWordProcessingMLBaseImporter.LookupTableCellStyleIndex(const AStyleId: string): Integer;
begin
  Result := LookupStyleIndexCore(AStyleId, TableCellStyleInfos);
end;

function TdxWordProcessingMLBaseImporter.LookupStyleIndexCore(const AStyleId: string; AStyleInfos: TdxOpenXmlStyleInfoCollection): Integer;
var
  AStyleInfo: TdxOpenXmlStyleInfo;
begin
  AStyleInfo := AStyleInfos.LookupStyleById(AStyleId);
  if AStyleInfo <> nil then
    Result := AStyleInfo.StyleIndex
  else
    Result := -1;
end;

procedure TdxWordProcessingMLBaseImporter.CreateStylesHierarchy;
begin
  CreateStylesHierarchyCore(DocumentModel.CharacterStyles, CharacterStyleInfos);
  CreateStylesHierarchyCore(DocumentModel.ParagraphStyles, ParagraphStyleInfos);
  CreateStylesHierarchyCore(DocumentModel.TableStyles, TableStyleInfos);
  CreateStylesHierarchyCore(DocumentModel.TableCellStyles, TableCellStyleInfos);
end;

procedure TdxWordProcessingMLBaseImporter.CreateStylesHierarchyCore(AStyles: TdxStyleCollectionBase; AStyleInfos: TdxOpenXmlStyleInfoCollection);
var
  I: Integer;
  AStyleInfo, AParentStyleInfo: TdxOpenXmlStyleInfo;
begin
  for I := 0 to AStyleInfos.Count - 1 do
  begin
    AStyleInfo := AStyleInfos[I];
    AParentStyleInfo := AStyleInfos.LookupStyleById(AStyleInfo.ParentStyleId);
    if AParentStyleInfo <> nil then
      AStyles[AStyleInfo.StyleIndex].Parent := AStyles[AParentStyleInfo.StyleIndex];
  end;
end;

procedure TdxWordProcessingMLBaseImporter.LinkStyles;
var
  ACharacterStyles: TdxCharacterStyleCollection;
  AParagraphStyles: TdxParagraphStyleCollection;
  I: Integer;
  AStyleInfo, ALinkedStyleInfo, ANextStyleInfo: TdxOpenXmlStyleInfo;
begin
  ACharacterStyles := DocumentModel.CharacterStyles;
  AParagraphStyles := DocumentModel.ParagraphStyles;

  for I := 0 to CharacterStyleInfos.Count - 1 do
  begin
    AStyleInfo := CharacterStyleInfos[I];
    ALinkedStyleInfo := ParagraphStyleInfos.LookupStyleById(AStyleInfo.LinkedStyleId);
    if ALinkedStyleInfo <> nil then
      DocumentModel.StyleLinkManager.CreateLink(AParagraphStyles[ALinkedStyleInfo.StyleIndex], ACharacterStyles[AStyleInfo.StyleIndex]);
  end;
  for I := 0 to ParagraphStyleInfos.Count - 1 do
  begin
    AStyleInfo := ParagraphStyleInfos[I];
    ANextStyleInfo := ParagraphStyleInfos.LookupStyleById(AStyleInfo.NextStyleId);
    if ANextStyleInfo <> nil then
      AParagraphStyles[AStyleInfo.StyleIndex].NextParagraphStyle := AParagraphStyles[ANextStyleInfo.StyleIndex];
  end;
end;

procedure TdxWordProcessingMLBaseImporter.CreateNumberingLists;
var
  AListInfo: TdxOpenXmlNumberingListInfo;
  AAbstractNumberingInfo: TdxOpenXmlAbstractNumberingInfo;
  I: Integer;
begin
  for I := 0 to ListInfos.Count - 1 do
  begin
    AListInfo := ListInfos[I];
    AAbstractNumberingInfo := AbstractListInfos.FindById(AListInfo.AbstractNumberingListId);
    if AAbstractNumberingInfo <> nil then
      AppendNumberingList(AListInfo, AAbstractNumberingInfo.AbstractNumberingIndex);
  end;
end;

procedure TdxWordProcessingMLBaseImporter.AppendNumberingList(AListInfo: TdxOpenXmlNumberingListInfo;
  AAbstractNumberingListIndex: Integer);
var
  AList: TdxNumberingList;
  AListLevelOverride: TdxOpenXmlListLevelOverride;
  ACount, I, AOverrideLevelIndex: Integer;
  AReferenceLevel: TdxNumberingListReferenceLevel;
begin
  AList := TdxNumberingList.Create(DocumentModel, AAbstractNumberingListIndex);
  ACount := AListInfo.LevelOverrides.Count;

  for I := 0 to ACount - 1 do
  begin
    AListLevelOverride := AListInfo.LevelOverrides[I];
    AOverrideLevelIndex := AListLevelOverride.LevelIndex;
    AReferenceLevel := TdxNumberingListReferenceLevel(AList.Levels[AOverrideLevelIndex]);
    AList.Levels[AOverrideLevelIndex] := AListLevelOverride.GetOverrideListLevelCore(AReferenceLevel);
  end;

  AListInfo.ListIndex := DocumentModel.NumberingLists.Count;
  DocumentModel.AddNumberingListUsingHistory(AList);
end;

procedure TdxWordProcessingMLBaseImporter.LinkParagraphStylesWithNumberingLists;
var
  I: Integer;
begin
  for I := 0 to FParagraphStyleInfos.Count- 1 do
    LinkParagraphStyleWithNumberingLists(FParagraphStyleInfos[I]);
end;

procedure TdxWordProcessingMLBaseImporter.LinkNumberingListStyles;
var
  AStyleInfo: TdxOpenXmlStyleInfo;
  AListInfo: TdxOpenXmlNumberingListInfo;
  AStyle: TdxNumberingListStyle;
  I: Integer;
begin
  for I := 0 to FNumberingStyleInfos.Count - 1 do
  begin
    AStyleInfo := FNumberingStyleInfos[I];
    AListInfo := ListInfos.FindById(AStyleInfo.NumberingId);
    if AListInfo = nil then
      Continue;
    AStyle := DocumentModel.NumberingListStyles[AStyleInfo.StyleIndex];
    AStyle.SetNumberingListIndex(AListInfo.ListIndex);
    DocumentModel.NumberingLists[AListInfo.ListIndex].AbstractNumberingList.SetStyleLinkIndex(AStyleInfo.StyleIndex);
  end;
end;

procedure TdxWordProcessingMLBaseImporter.LinkParagraphStyleWithNumberingLists(AStyleInfo: TdxOpenXmlStyleInfo);
var
  AStyle: TdxParagraphStyle;
  AListInfo: TdxOpenXmlNumberingListInfo;
begin
  if AStyleInfo.StyleIndex < 0 then
    Exit;

  AStyle := DocumentModel.ParagraphStyles[AStyleInfo.StyleIndex];
  if AStyleInfo.NumberingId >= 0 then
  begin
    AListInfo := ListInfos.FindById(AStyleInfo.NumberingId);
    if AListInfo = nil then
      Exit;
    AStyle.SetNumberingListIndex(AListInfo.ListIndex);
  end;

  AStyle.SetNumberingListLevelIndex(AStyleInfo.ListLevelIndex);
end;

procedure TdxWordProcessingMLBaseImporter.LockAspectRatioTableAddValue(const AKey: string; const AValue: string);
begin
  if AKey <> '' then
    if (AValue = 't') or (AValue = 'f') then
      LockAspectRatioTable.AddOrSetValue('#' + AKey, AValue);
end;

function TdxWordProcessingMLBaseImporter.LockAspectRatioTableGetValue(const AKey: string; out AUseLockAspectRatio: Boolean): Boolean;
begin
  if LockAspectRatioTable.ContainsKey(AKey) then
  begin
    AUseLockAspectRatio := True;
    Result := GetBoolValue(LockAspectRatioTable[AKey]);
  end
  else
  begin
    AUseLockAspectRatio := False;
    Result := False;
  end;
end;

function TdxWordProcessingMLBaseImporter.GetBoolValue(const AValue: string): Boolean;
begin
  Result := AValue = 't';
end;

function TdxWordProcessingMLBaseImporter.CreateBookmarkStartElementDestination(AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxBookmarkStartElementDestination.Create(Self);
end;

function TdxWordProcessingMLBaseImporter.CreateBookmarkEndElementDestination(AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxBookmarkEndElementDestination.Create(Self);
end;

function TdxWordProcessingMLBaseImporter.CreateVersionDestination(AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDocumentVersionDestination.Create(Self);
end;

function TdxWordProcessingMLBaseImporter.CreateRunDestination: TdxRunDestination;
begin
  Result := TdxRunDestination.Create(Self);
end;

function TdxWordProcessingMLBaseImporter.CreateParagraphDestination: TdxParagraphDestination;
begin
  Result := TdxParagraphDestination.Create(Self);
end;

procedure TdxWordProcessingMLBaseImporter.BeginTable;
begin
  Inc(FTableDepth);
end;

procedure TdxWordProcessingMLBaseImporter.EndTable;
begin
  Dec(FTableDepth);
end;

end.

