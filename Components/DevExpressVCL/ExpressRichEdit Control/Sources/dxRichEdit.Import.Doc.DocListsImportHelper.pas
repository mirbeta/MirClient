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

unit dxRichEdit.Import.Doc.DocListsImportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreGraphics, dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Doc.ListFormatInformation,
  dxRichEdit.Import.Doc.DocContentBuilder,
  dxRichEdit.Import.Doc.ListFormatOverride,
  dxRichEdit.Import.Doc.DocStylesImportHelper,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxDocListsImportHelper }

  TdxDocListsImportHelper = class
  strict private
    FContentBuilder: TdxDocContentBuilder;
    FDocumentModel: TdxDocumentModel;
    FAbstractListMapping: TDictionary<Integer, TdxAbstractNumberingListIndex>;
  protected
    property ContentBuilder: TdxDocContentBuilder read FContentBuilder;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property AbstractListMapping: TDictionary<Integer, TdxAbstractNumberingListIndex> read FAbstractListMapping;
  public
    constructor Create(AContentBuilder: TdxDocContentBuilder; ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    procedure InitializeAbstractLists;
    procedure InitializeLists;
    procedure CreateAbstractList(ACurrentListData: TdxDocListData);
    procedure InitializeListLevel(AListLevel: TdxListLevel; ADocListLevel: TdxDocListLevel; AListLevelIndex: Integer);
    procedure SetListLevelFormatting(AListLevel: TdxListLevel; ADocListLevel: TdxDocListLevel);
    procedure SetListLevelProperties(AListLevel: TdxListLevel; ADocListLevel: TdxDocListLevel; AListLevelIndex: Integer);
    procedure CreateList(AListOverrideFormat: TdxDocListOverrideFormat; AListOverrideLevelInformation: TdxDocListOverrideLevelInformation);
    procedure LinkNumberingListStyles(AHelper: TdxDocStylesImportHelper);
  end;

implementation

uses
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocumentFileRecords,
  dxRichEdit.Import.Doc.DocCommandHelper;

{ TdxDocListsImportHelper }

constructor TdxDocListsImportHelper.Create(AContentBuilder: TdxDocContentBuilder; ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FContentBuilder := AContentBuilder;
  FDocumentModel := ADocumentModel;
  FAbstractListMapping := TDictionary<Integer, TdxAbstractNumberingListIndex>.Create;
end;

destructor TdxDocListsImportHelper.Destroy;
begin
  FreeAndNil(FAbstractListMapping);
  inherited Destroy;
end;

procedure TdxDocListsImportHelper.InitializeAbstractLists;
var
  AListData: TdxObjectList<TdxDocListData>;
  ACount, I: Integer;
begin
  FAbstractListMapping.Free;
  FAbstractListMapping := TDictionary<Integer, TdxAbstractNumberingListIndex>.Create;
  AListData := ContentBuilder.ListInfo.ListData;
  ACount := AListData.Count;
  for I := 0 to ACount - 1 do
    CreateAbstractList(AListData[I]);
end;

procedure TdxDocListsImportHelper.InitializeLists;
var
  AListOverrideInfo: TdxDocListOverrideFormatInformation;
  ACount, I: Integer;
begin
  AListOverrideInfo := ContentBuilder.ListOverrideInfo;
  ACount := AListOverrideInfo.FormatOverride.Count;
  for I := 0 to ACount - 1 do
    CreateList(AListOverrideInfo.FormatOverride[I], AListOverrideInfo.FormatOverrideData[I]);
end;

procedure TdxDocListsImportHelper.CreateAbstractList(ACurrentListData: TdxDocListData);
var
  AAbstractList: TdxAbstractNumberingList;
  ALevelsFormatting: TdxObjectList<TdxDocListLevel>;
  ACount, I: Integer;
begin
  if FAbstractListMapping.ContainsKey(ACurrentListData.ListFormatting.ListIdentifier) then
    Exit;
  AAbstractList := TdxAbstractNumberingList.Create(DocumentModel);
  AAbstractList.SetId(ACurrentListData.ListFormatting.ListIdentifier);
  FAbstractListMapping.Add(ACurrentListData.ListFormatting.ListIdentifier, DocumentModel.AbstractNumberingLists.Count);
  DocumentModel.AddAbstractNumberingListUsingHistory(AAbstractList);
  ALevelsFormatting := ACurrentListData.LevelsFormatting;
  ACount := ALevelsFormatting.Count;
  for I := 0 to ACount - 1 do
    InitializeListLevel(TdxListLevel(AAbstractList.Levels[I]), ALevelsFormatting[I], I);
end;

procedure TdxDocListsImportHelper.InitializeListLevel(AListLevel: TdxListLevel; ADocListLevel: TdxDocListLevel; AListLevelIndex: Integer);
begin
  SetListLevelFormatting(AListLevel, ADocListLevel);
  SetListLevelProperties(AListLevel, ADocListLevel, AListLevelIndex);
end;

procedure TdxDocListsImportHelper.SetListLevelFormatting(AListLevel: TdxListLevel; ADocListLevel: TdxDocListLevel);
var
  AContainer: TdxDocPropertyContainer;
  ADefaultFormattingInfo, ACharacterFormattingInfo: TdxCharacterFormattingInfo;
  ATabs: TdxTabFormattingInfo;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  AContainer := TdxDocCommandHelper.Traverse(ADocListLevel.CharacterUPX, ContentBuilder.Factory, ContentBuilder.DataReader);
  ContentBuilder.AddToGC(AContainer);

  TdxDocCommandHelper.Traverse(ADocListLevel.ParagraphUPX, AContainer, ContentBuilder.DataReader);
  ContentBuilder.FontManager.SetFontName(AContainer);

  if AContainer.CharacterInfo <> nil then
  begin
    ADefaultFormattingInfo := DocumentModel.Cache.CharacterFormattingCache[TdxCharacterFormattingInfoCache.DefaultItemIndex].Info;
    ACharacterFormattingInfo := TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(AContainer.CharacterInfo.FormattingInfo, ADefaultFormattingInfo, DocumentModel);
    try
      AListLevel.CharacterProperties.BeginInit;
      AMergedCharacterProperties := TdxMergedCharacterProperties.Create(ACharacterFormattingInfo, AContainer.CharacterInfo.FormattingOptions);
      try
        AListLevel.CharacterProperties.CopyFrom(AMergedCharacterProperties);
      finally
        AMergedCharacterProperties.Free;
      end;
      AListLevel.CharacterProperties.EndInit;
      AListLevel.ListLevelProperties.SuppressBulletResize := AContainer.CharacterInfo.PictureBulletInformation.SuppressBulletResize;
    finally
      ACharacterFormattingInfo.Free;
    end;
  end;
  if AContainer.ParagraphInfo <> nil then
  begin
    AMergedParagraphProperties := TdxMergedParagraphProperties.Create(AContainer.ParagraphInfo.FormattingInfo, AContainer.ParagraphInfo.FormattingOptions);
    try
      AListLevel.ParagraphProperties.CopyFrom(AMergedParagraphProperties);
    finally
      AMergedParagraphProperties.Free;
    end;
    ATabs := AContainer.ParagraphInfo.Tabs;
    if ATabs.Count > 0 then
      AListLevel.Tabs.SetTabs(ATabs);
  end;
end;

procedure TdxDocListsImportHelper.SetListLevelProperties(AListLevel: TdxListLevel; ADocListLevel: TdxDocListLevel; AListLevelIndex: Integer);
var
  AListLevelProperties: TdxListLevelProperties;
  ADocListLevelProperties: TdxDocListLevelProperties;
begin
  AListLevelProperties := AListLevel.ListLevelProperties;
  ADocListLevelProperties := ADocListLevel.ListLevelProperties;
  AListLevelProperties.Alignment := ADocListLevelProperties.Alignment;
  AListLevelProperties.ConvertPreviousLevelNumberingToDecimal := ADocListLevelProperties.ConvertPreviousLevelNumberingToDecimal;
  AListLevelProperties.DisplayFormatString := ADocListLevel.GetDisplayFormatString;
  AListLevelProperties.Format := ADocListLevelProperties.NumberingFormat;
  AListLevelProperties.Legacy := ADocListLevelProperties.Legacy;
  AListLevelProperties.LegacyIndent := ADocListLevelProperties.LegacyIndent;
  AListLevelProperties.LegacySpace := ADocListLevelProperties.LegacySpace;

  if ADocListLevelProperties.RestartAfterLevelLimit then
    AListLevelProperties.RelativeRestartLevel := AListLevelIndex - ADocListLevelProperties.RestartLevelLimit
  else
    AListLevelProperties.RelativeRestartLevel := 0;

  AListLevelProperties.Separator := ADocListLevelProperties.Separator;
  AListLevelProperties.Start := ADocListLevelProperties.Start;
  if ADocListLevelProperties.BulletedList then
  begin
    AListLevelProperties.Format := TdxNumberingFormat.Bullet;
    AListLevelProperties.TemplateCode := TdxNumberingListHelper.GenerateNewTemplateCode(FDocumentModel);
  end;
end;

procedure TdxDocListsImportHelper.CreateList(AListOverrideFormat: TdxDocListOverrideFormat; AListOverrideLevelInformation: TdxDocListOverrideLevelInformation);
var
  AIndex: TdxAbstractNumberingListIndex;
  AList: TdxNumberingList;
  ACount, I, AOverriddenLevel: Integer;
  ALevelFormat: TdxDocListOverrideLevelFormat;
  AOverrideLevel: TdxOverrideListLevel;
begin
  if not AbstractListMapping.TryGetValue(AListOverrideFormat.ListIdentifier, AIndex) then
    Exit;
  AList := TdxNumberingList.Create(DocumentModel, AIndex);
  DocumentModel.AddNumberingListUsingHistory(AList);
  ACount := AListOverrideFormat.LevelsCount;
  for I := 0 to ACount - 1 do
  begin
    ALevelFormat := AListOverrideLevelInformation.LevelFormatOverrideData[I];
    AOverriddenLevel := ALevelFormat.OverriddenLevel;
    if ALevelFormat.OverrideFormatting then
    begin
      AOverrideLevel := TdxOverrideListLevel.Create(DocumentModel);
      InitializeListLevel(AOverrideLevel, ALevelFormat.OverrideLevelFormatting, AOverriddenLevel);
      AList.Levels[AOverriddenLevel] := AOverrideLevel;
      AOverrideLevel.SetOverrideStart(ALevelFormat.OverrideStart);
    end
    else
      if ALevelFormat.OverrideStart then
      begin
        (AList.Levels[AOverriddenLevel] as IdxOverrideListLevel).SetOverrideStart(True);
        (AList.Levels[AOverriddenLevel] as IdxOverrideListLevel).NewStart := ALevelFormat.StartAt;
      end;
  end;
end;

procedure TdxDocListsImportHelper.LinkNumberingListStyles(AHelper: TdxDocStylesImportHelper);
var
  AStyles: TList<TdxListStylesRecordItem>;
  ACount, AAbstractListsCount, ANumberingListStylesCount, I, AStyleIndex: Integer;
  AAbstractLists: TdxAbstractNumberingListCollection;
  AItem: TdxListStylesRecordItem;
  AListIndex: TdxAbstractNumberingListIndex;
  AList: TdxAbstractNumberingList;
begin
  AStyles := ContentBuilder.DocFileRecords.ListStyles;
  ACount := AStyles.Count;
  AAbstractLists := DocumentModel.AbstractNumberingLists;
  AAbstractListsCount := AAbstractLists.Count;
  ANumberingListStylesCount := DocumentModel.NumberingListStyles.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := AStyles[I];
    if (AItem.ListIndex < 0) or (AItem.ListIndex >= AAbstractListsCount) then
      Continue;
    AListIndex := AItem.ListIndex;
    AList := AAbstractLists[AListIndex];
    AStyleIndex := AHelper.GetNumberingStyleIndex(AItem.StyleIndex);
    if (AStyleIndex < 0) or (AStyleIndex >= ANumberingListStylesCount) then
      Continue;
    if AItem.StyleDefinition then
      AList.SetStyleLinkIndex(AStyleIndex)
    else
      AList.SetNumberingStyleReferenceIndex(AStyleIndex);
  end;
end;

end.
