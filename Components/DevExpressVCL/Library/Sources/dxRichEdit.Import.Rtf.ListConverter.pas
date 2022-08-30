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

unit dxRichEdit.Import.Rtf.ListConverter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes,
  Generics.Defaults, Generics.Collections,
  dxRichEdit.Import.Rtf,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxGenerics,
  dxRichEdit.Utils.Types;

type

  { TdxRtfListConverter }

  TdxRtfListConverter = class
  private
    FStyleCrossTable: TDictionary<TdxRtfListId, Integer>;
    FImporter: TdxRtfImporter;
    function GetDocumentModel: TdxDocumentModel; inline;
    procedure CreateNumberingListsCore(AListOverrideTable: TdxListOverrideTable; AListTable: TdxRtfListTable);
    procedure CreateAbstractNumberingLists(AListTable: TdxRtfListTable);
    function CreateAbstractNumberingList(ARtfList: TdxRtfNumberingList): TdxAbstractNumberingList;
    class procedure ConvertPropertyRtfToNumbering(ARtfLevel: TdxRtfListLevel; const ALevel: IdxListLevelProperties;
      ARestart, AReformat: Boolean); static;
    function FindAbstractNumberingListByStyle(AStyleIndex: Integer): TdxAbstractNumberingListIndex;
    procedure FixBrokenListStyles;
    procedure LinkNumberingListStyles(AListOverrideTable: TdxListOverrideTable; AListTable: TdxRtfListTable);
  protected
    function IsHybridList(ARtfList: TdxRtfNumberingList): Boolean;
    function GetStyleIndex(ARtfParentStyleId: TdxRtfListId): Integer;
    function GetListIndex(AListId: Integer; ALists: TdxAbstractNumberingListCollection): TdxAbstractNumberingListIndex;
    procedure ConvertRtfListToNumberingList(ARtfLevels: TdxObjectList<TdxRtfListLevel>; AList: TdxAbstractNumberingList);

  public
    constructor Create(AImporter: TdxRtfImporter);
    destructor Destroy; override;
    procedure Convert(AListTable: TdxRtfListTable; AListOverrideTable: TdxListOverrideTable);
    procedure ConvertRtfOverrideToNumbering(AList: TdxNumberingList; ARtfOverride: TdxRtfNumberingListOverride);

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Importer: TdxRtfImporter read FImporter;
  end;

implementation

uses
  Contnrs, dxRichEdit.DocumentModel.ParagraphFormatting;

{ TdxRtfListConverter }

constructor TdxRtfListConverter.Create(AImporter: TdxRtfImporter);
begin
  inherited Create;
  FImporter := AImporter;
  FStyleCrossTable := TDictionary<TdxRtfListId, Integer>.Create;
end;

destructor TdxRtfListConverter.Destroy;
begin
  FreeAndNil(FStyleCrossTable);
  inherited Destroy;
end;

function TdxRtfListConverter.GetDocumentModel: TdxDocumentModel;
begin
  Result := Importer.DocumentModel;
end;

function TdxRtfListConverter.FindAbstractNumberingListByStyle(
  AStyleIndex: Integer): TdxAbstractNumberingListIndex;
var
  ALists: TdxAbstractNumberingListCollection;
  ACount, I: Integer;
begin
  Result := AbstractNumberingListIndexInvalidValue;
  ALists := DocumentModel.AbstractNumberingLists;
  ACount := ALists.Count;
  for I := 0 to ACount - 1 do
  begin
    if ALists[I].StyleLinkIndex = AStyleIndex then
      Exit(I);
  end;
end;

procedure TdxRtfListConverter.FixBrokenListStyles;
var
  AStyles: TdxNumberingListStyleCollection;
  ACount, I: Integer;
  AStyle: TdxNumberingListStyle;
  AAbstractListIndex: TdxAbstractNumberingListIndex;
  AList: TdxNumberingList;
  ANumberingListIndex: TdxNumberingListIndex;
begin
  AStyles := DocumentModel.NumberingListStyles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
  begin
    AStyle := AStyles[i];
    if AStyle.NumberingListIndex >= NumberingListIndexMinValue then
      Continue;
    AAbstractListIndex := FindAbstractNumberingListByStyle(I);
    if AAbstractListIndex = AbstractNumberingListIndexInvalidValue then
      Continue;
    AList := TdxNumberingList.Create(DocumentModel, AAbstractListIndex);
    DocumentModel.AddNumberingListUsingHistory(AList);
    ANumberingListIndex := DocumentModel.NumberingLists.Count - 1;
    AStyle.SetNumberingListIndex(ANumberingListIndex);
  end;
end;

procedure TdxRtfListConverter.Convert(AListTable: TdxRtfListTable; AListOverrideTable: TdxListOverrideTable);
begin
  CreateAbstractNumberingLists(AListTable);
  LinkNumberingListStyles(AListOverrideTable, AListTable);
  FixBrokenListStyles;
  CreateNumberingListsCore(AListOverrideTable, AListTable);
end;

procedure TdxRtfListConverter.CreateNumberingListsCore(AListOverrideTable: TdxListOverrideTable; AListTable: TdxRtfListTable);
var
  I, ACount, AOverrideId: Integer;
  AAbstractNumberingLists: TdxAbstractNumberingListCollection;
  ARtfList: TdxRtfNumberingListOverride;
  ASourceListIndex: TdxAbstractNumberingListIndex;
  AList: TdxNumberingList;
  ANumberingListIndex: TdxNumberingListIndex;
  AAbstractList: TdxAbstractNumberingList;
begin
  ACount := AListOverrideTable.Count;
  AAbstractNumberingLists := DocumentModel.AbstractNumberingLists;
  for I := 0 to ACount - 1 do
  begin
    ARtfList := AListOverrideTable[I];
    ASourceListIndex := GetListIndex(ARtfList.ListId, AAbstractNumberingLists);
    if ASourceListIndex < 0 then
      Continue;
    AAbstractList := DocumentModel.AbstractNumberingLists[ASourceListIndex];
    if AAbstractList.StyleLinkIndex >= 0 then
      Continue;
    AList := TdxNumberingList.Create(DocumentModel, ASourceListIndex);
    AOverrideId := ARtfList.Id;
    DocumentModel.AddNumberingListUsingHistory(AList);
    ConvertRtfOverrideToNumbering(AList, ARtfList);
    ANumberingListIndex := DocumentModel.NumberingLists.Count - 1;
    Importer.ListOverrideIndexToNumberingListIndexMap.AddOrSetValue(AOverrideId, ANumberingListIndex);
    AList.SetId(DocumentModel.NumberingLists.Count);
  end;
end;

procedure TdxRtfListConverter.CreateAbstractNumberingLists(AListTable: TdxRtfListTable);
var
  I, ACount: Integer;
  ARtfList: TdxRtfNumberingList;
  ALists: TdxList<TdxAbstractNumberingList>;
  AList: TdxAbstractNumberingList;
  AStyle: TdxNumberingListStyle;
  AStyleIndex: Integer;
begin
  ACount := AListTable.Count;
  ALists := TdxList<TdxAbstractNumberingList>.Create;
  try
    for I := 0 to ACount - 1 do
    begin
      ARtfList := AListTable[I];
      AList := CreateAbstractNumberingList(ARtfList);
      ALists.Add(AList);
      if ARtfList.StyleName <> '' then
      begin
        if DocumentModel.NumberingListStyles.GetStyleIndexByName(ARtfList.StyleName) < 0 then
        begin
          AStyle := TdxNumberingListStyle.Create(DocumentModel, ARtfList.StyleName);
          AStyleIndex := DocumentModel.NumberingListStyles.Add(AStyle);
          AList.SetStyleLinkIndex(AStyleIndex);
          FStyleCrossTable.Add(ARtfList.Id, AStyleIndex);
        end;
      end;
    end;
    for I := 0 to ACount - 1 do
    begin
      ARtfList := AListTable[I];
      AList := ALists[I];
      if ARtfList.ParentStyleId <> 0 then
      begin
        AStyleIndex := GetStyleIndex(ARtfList.ParentStyleId);
        if AStyleIndex >= 0 then
          AList.SetNumberingStyleReferenceIndex(AStyleIndex);
      end;
    end;
  finally
    ALists.Free;
  end;
end;

function TdxRtfListConverter.CreateAbstractNumberingList(ARtfList: TdxRtfNumberingList): TdxAbstractNumberingList;
begin
  Result := TdxAbstractNumberingList.Create(DocumentModel);
  ConvertRtfListToNumberingList(ARtfList.Levels, Result);
  if IsHybridList(ARtfList) then
      TdxNumberingListHelper.SetHybridListType(Result);
  Result.SetId(ARtfList.Id);
  DocumentModel.AddAbstractNumberingListUsingHistory(Result);
end;

function TdxRtfListConverter.IsHybridList(ARtfList: TdxRtfNumberingList): Boolean;
var
  ALevels: TdxObjectList<TdxRtfListLevel>;
  I, ACount: Integer;
begin
  if ARtfList.NumberingListType <> TdxRtfNumberingListType.Unknown then
    Exit(True);
  ALevels := ARtfList.Levels;
  ACount := ALevels.Count;
  for I := 0 to ACount - 1 do
    if ALevels[I].ListLevelProperties.TemplateCode <> 0 then
      Exit(True);
  Result := False;
end;

procedure TdxRtfListConverter.LinkNumberingListStyles(
  AListOverrideTable: TdxListOverrideTable; AListTable: TdxRtfListTable);
var
  ACount, I: Integer;
  AAbstractNumberingLists: TdxAbstractNumberingListCollection;
  ARtfList: TdxRtfNumberingListOverride;
  ASourceListIndex: TdxAbstractNumberingListIndex;
  AAbstractList: TdxAbstractNumberingList;
  AList: TdxNumberingList;
  ANumberingListIndex: TdxNumberingListIndex;
  AOverrideId: Integer;
begin
  ACount := AListOverrideTable.Count;
  AAbstractNumberingLists := DocumentModel.AbstractNumberingLists;
  for I := 0 to ACount -  1 do
  begin
    ARtfList := AListOverrideTable[I];
    ASourceListIndex := GetListIndex(ARtfList.ListId, AAbstractNumberingLists);
    if ASourceListIndex < 0 then
      Continue;
    AAbstractList := DocumentModel.AbstractNumberingLists[ASourceListIndex];
    if AAbstractList.StyleLinkIndex >= 0 then
    begin
      AList := TdxNumberingList.Create(DocumentModel, ASourceListIndex);
      DocumentModel.AddNumberingListUsingHistory(AList);
      ANumberingListIndex := DocumentModel.NumberingLists.Count - 1;
      AList.SetId(DocumentModel.NumberingLists.Count);
      AOverrideId := ARtfList.Id;
      Importer.ListOverrideIndexToNumberingListIndexMap.AddOrSetValue(AOverrideId, ANumberingListIndex);
      DocumentModel.NumberingListStyles[AAbstractList.StyleLinkIndex].SetNumberingListIndex(ANumberingListIndex);
    end;
  end;
end;

function TdxRtfListConverter.GetStyleIndex(ARtfParentStyleId: TdxRtfListId): Integer;
begin
  if not FStyleCrossTable.TryGetValue(ARtfParentStyleId, Result) then
    Result := -1;
end;

function TdxRtfListConverter.GetListIndex(AListId: Integer; ALists: TdxAbstractNumberingListCollection): TdxAbstractNumberingListIndex;
var
  I, ACount: TdxAbstractNumberingListIndex;
begin
  ACount := ALists.Count;
  for I := 0 to ACount - 1 do
  begin
    if ALists[I].GetId = AListId then
      Exit(I);
  end;
  Result := -1;
end;

procedure TdxRtfListConverter.ConvertRtfOverrideToNumbering(AList: TdxNumberingList; ARtfOverride: TdxRtfNumberingListOverride);
var
  I, ACount: Integer;
  ARestart, AReformat: Boolean;
  ALevel: TdxOverrideListLevel;
  AReferenceLevel: TdxNumberingListReferenceLevel;
begin
  ACount := ARtfOverride.Levels.Count;
  for I := 0 to ACount - 1 do
  begin
    ARestart := ARtfOverride.Levels[I].OverrideStartAt;
    AReformat := ARtfOverride.Levels[I].OverrideFormat;
    if AReformat then
    begin
      ALevel := TdxOverrideListLevel.Create(DocumentModel);
      ALevel.ListLevelProperties.BeginInit;
      try
        ALevel.ListLevelProperties.CopyFrom(AList.Levels[I].ListLevelProperties);
        ALevel.CharacterProperties.CopyFrom(ARtfOverride.Levels[I].Level.CharacterProperties);
        ALevel.ParagraphProperties.CopyFrom(ARtfOverride.Levels[I].Level.ParagraphProperties);
        ConvertPropertyRtfToNumbering(ARtfOverride.Levels[I].Level, ALevel.ListLevelProperties, ARestart, AReformat);
      finally
        ALevel.ListLevelProperties.EndInit;
      end;
      AList.SetLevel(I, ALevel);
      if ARestart then
        ALevel.SetOverrideStart(True);
    end
    else
    begin
      if ARestart then
      begin
        AReferenceLevel := TdxNumberingListReferenceLevel(AList.Levels[I]);
        AReferenceLevel.SetOverrideStart(True);
        AReferenceLevel.NewStart := ARtfOverride.Levels[I].StartAt;
      end;
    end;
  end;
end;

procedure TdxRtfListConverter.ConvertRtfListToNumberingList(ARtfLevels: TdxObjectList<TdxRtfListLevel>; AList: TdxAbstractNumberingList);
var
  I, ACount: Integer;
  ALevel: TdxListLevel;
begin
  ACount := ARtfLevels.Count;
  for I := 0 to ACount - 1 do
  begin
    ALevel := AList.Levels[I] as TdxListLevel;
    ALevel.ParagraphProperties.CopyFrom(ARtfLevels[I].ParagraphProperties);
    ALevel.CharacterProperties.CopyFrom(ARtfLevels[I].CharacterProperties);
    ALevel.ParagraphStyleIndex := ARtfLevels[I].ParagraphStyleIndex;
    ConvertPropertyRtfToNumbering(ARtfLevels[I], ALevel.ListLevelProperties, True, True);
  end;
end;

class procedure TdxRtfListConverter.ConvertPropertyRtfToNumbering(ARtfLevel: TdxRtfListLevel; const ALevel: IdxListLevelProperties; ARestart, AReformat: Boolean);
var
  ALevelProperties: TdxListLevelProperties;
begin
  ALevelProperties := ARtfLevel.ListLevelProperties;
  if ARestart then
    ALevel.Start := ALevelProperties.Start;
  if AReformat then
  begin
    ALevel.Format := ALevelProperties.Format;
    ALevel.Alignment := ALevelProperties.Alignment;
    ALevel.SuppressBulletResize := ALevelProperties.SuppressBulletResize;
    ALevel.SuppressRestart := ALevelProperties.SuppressRestart;
    ALevel.Separator := ALevelProperties.Separator;
    ALevel.ConvertPreviousLevelNumberingToDecimal := ALevelProperties.ConvertPreviousLevelNumberingToDecimal;
    ALevel.DisplayFormatString := ARtfLevel.CreateDisplayFormatString;
    ALevel.TemplateCode := ALevelProperties.TemplateCode;
  end;
  if ALevelProperties.Legacy then
  begin
    ALevel.Legacy := ALevelProperties.Legacy;
    ALevel.LegacySpace := ALevelProperties.LegacySpace;
    ALevel.LegacyIndent := ALevelProperties.LegacyIndent;
  end;
end;

end.

