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
unit dxRichEdit.Import.Doc.DocStylesImportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.Import.Doc.DocStyleSheet,
  dxRichEdit.Import.Doc.DocStyles,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocContentBuilder;

type
  { TdxDocStylesImportHelper }

  TdxDocStylesImportHelper = class
  public const
    DefaultFontSize = 10;
  strict private
    FDefaultStyleName: string;
    FContentBuilder: TdxDocContentBuilder;
    FDocumentModel: TdxDocumentModel;
    FStyleMapping: TDictionary<Integer, string>;
    FIsContainsParagraphFrame: Boolean;
  protected
    property ContentBuilder: TdxDocContentBuilder read FContentBuilder;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property StyleMapping: TDictionary<Integer, string> read FStyleMapping;
    property DefaultParagraphStyleName: string read FDefaultStyleName;
  public
    constructor Create(AContentBuilder: TdxDocContentBuilder; ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    function GetStyleIndex(ADocStyleIndex: Integer; AStyleType: TdxStyleType): Integer;
    function GetNumberingStyleIndex(ADocStyleIndex: Integer): Integer;
    function GetStyleIndexCore(ADocStyleIndex: Integer; AStyleType: TdxStyleType): Integer;
    procedure SetDocunentDefaults;
    procedure InitializeStyles;
    procedure GenerateStyleMapping;
    procedure DeleteStyleLinks;
    procedure RegisterStyle(AStyleDescription: TdxStyleDescriptionBase);
    procedure AddParagraphStyle(AStyleDescription: TdxParagraphStyleDescription);
    procedure SetParagraphStyleProperties(AStyle: TdxParagraphStyle; ADescription: TdxParagraphStyleDescription);
    procedure SetParagraphStyleOutlineLevel(ADescription: TdxParagraphStyleDescription; APropertyContainer: TdxDocPropertyContainer);
    procedure ProcessParagraphListInfoIndex(AStyle: TdxParagraphStyle; AInfo: TdxParagraphInfo);
    procedure AddCharacterStyle(AStyleDescription: TdxCharacterStyleDescription);
    procedure SetCharacterStyleProperties(AStyle: TdxCharacterStyle; ADescription: TdxCharacterStyleDescription);
    procedure AddTableStyle(AStyleDescription: TdxTableStyleDescription);
    procedure SetTableStyleProperties(AStyle: TdxTableStyle; ADescription: TdxTableStyleDescription);
    procedure AddListStyle(AStyleDescription: TdxListStyleDescription);
    procedure SetNextParagraphStyles;
    procedure CreateStyleLinks;
    procedure UpdateDefaultFontSize;
    function GetCharacterFormattingInfo(AInfo: TdxDocCharacterFormattingInfo): TdxCharacterFormattingInfo;
    function GetCharacterStyleFormattingInfo(ABaseStyleIndex: SmallInt; APropertyContainer: TdxDocPropertyContainer): TdxCharacterFormattingInfo;
    function GetFormattingInfoCore(ACharacterStyleIndex: Integer): TdxCharacterFormattingInfo;
    function GetParagraphStyleFormattingInfo(ABaseStyleIndex: SmallInt; APropertyContainer: TdxDocPropertyContainer): TdxCharacterFormattingInfo;
    function GetTableStyleFormattingInfo(ABaseStyleIndex: SmallInt; APropertyContainer: TdxDocPropertyContainer): TdxCharacterFormattingInfo;

    property IsContainsParagraphFrame: Boolean read FIsContainsParagraphFrame;
  end;

implementation

uses
  Math,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocCommandHelper;

{ TdxDocStylesImportHelper }

constructor TdxDocStylesImportHelper.Create(AContentBuilder: TdxDocContentBuilder; ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FContentBuilder := AContentBuilder;
  FDocumentModel := ADocumentModel;
  FStyleMapping := TDictionary<Integer, string>.Create;
end;

destructor TdxDocStylesImportHelper.Destroy;
begin
  FStyleMapping.Free;
  inherited Destroy;
end;

function TdxDocStylesImportHelper.GetStyleIndex(ADocStyleIndex: Integer; AStyleType: TdxStyleType): Integer;
begin
  Result := Max(0, GetStyleIndexCore(ADocStyleIndex, AStyleType));
end;

function TdxDocStylesImportHelper.GetNumberingStyleIndex(ADocStyleIndex: Integer): Integer;
begin
  Result := GetStyleIndexCore(ADocStyleIndex, TdxStyleType.NumberingListStyle);
end;

function TdxDocStylesImportHelper.GetStyleIndexCore(ADocStyleIndex: Integer; AStyleType: TdxStyleType): Integer;
const
  AResultMap: array[Boolean] of Integer = (-1, 0);
var
  AStyleName: string;
begin
  if not StyleMapping.TryGetValue(ADocStyleIndex, AStyleName) then
    Exit(AResultMap[AStyleType <> TdxStyleType.NumberingListStyle]);
  case AStyleType of
    TdxStyleType.ParagraphStyle:
      Result := DocumentModel.ParagraphStyles.GetStyleIndexByName(AStyleName);
    TdxStyleType.CharacterStyle:
      Result := DocumentModel.CharacterStyles.GetStyleIndexByName(AStyleName);
    TdxStyleType.TableStyle:
      Result := DocumentModel.TableStyles.GetStyleIndexByName(AStyleName);
    TdxStyleType.NumberingListStyle:
      Result := DocumentModel.NumberingListStyles.GetStyleIndexByName(AStyleName);
    else
      Result := 0;
  end;
end;

procedure TdxDocStylesImportHelper.SetDocunentDefaults;
var
  ACharacterProperties: TdxCharacterProperties;
begin
  ACharacterProperties := DocumentModel.DefaultCharacterProperties;
  ACharacterProperties.BeginUpdate;
  ACharacterProperties.FontName := ContentBuilder.FontManager.GetFontName(ContentBuilder.StyleSheet.StylesInformation.DefaultASCIIFont);
  ACharacterProperties.EndUpdate;
end;

procedure TdxDocStylesImportHelper.InitializeStyles;
var
  AStyles: TdxStyleDescriptionCollection;
  ASortedStyles: TdxList<TdxStyleDescriptionBase>;
  ACount, I: Integer;
  AStyle: TdxStyleDescriptionBase;
begin
  AStyles := ContentBuilder.StyleSheet.Styles;
  GenerateStyleMapping;
  DeleteStyleLinks;
  ASortedStyles := TdxTopologicalSorter<TdxStyleDescriptionBase>.Sort(AStyles, TdxStyleDescriptionTopologicalComparer.Create);
  try
    ACount := ASortedStyles.Count;
    for I := 0 to ACount - 1 do
    begin
      AStyle := ASortedStyles[I];
      if AStyle <> nil then
        RegisterStyle(AStyle);
    end;
  finally
    ASortedStyles.Free;
  end;
  UpdateDefaultFontSize;
  SetNextParagraphStyles;
  CreateStyleLinks;
end;

procedure TdxDocStylesImportHelper.GenerateStyleMapping;
var
  AStyles: TdxStyleDescriptionCollection;
  I: Integer;
begin
  FStyleMapping.Free;
  FStyleMapping := TDictionary<Integer, string>.Create;
  AStyles := ContentBuilder.StyleSheet.Styles;
  if (AStyles.Count > 0) and (AStyles[0] is TdxParagraphStyleDescription) then
    FDefaultStyleName := AStyles[0].StyleName;
  for I := 0 to AStyles.Count - 1 do
  begin
    if AStyles[I] <> nil then
      StyleMapping.Add(AStyles[I].StyleIndex, AStyles[I].StyleName);
  end;
end;

procedure TdxDocStylesImportHelper.DeleteStyleLinks;
var
  ACount, I: Integer;
begin
  ACount := DocumentModel.CharacterStyles.Count;
  for I := 0 to ACount - 1 do
  begin
    if DocumentModel.CharacterStyles[I].HasLinkedStyle then
      DocumentModel.StyleLinkManager.DeleteLink(DocumentModel.CharacterStyles[I]);
  end;
end;

procedure TdxDocStylesImportHelper.RegisterStyle(AStyleDescription: TdxStyleDescriptionBase);
begin
  case AStyleDescription.StyleType of
    TdxStyleType.ParagraphStyle:
      AddParagraphStyle(TdxParagraphStyleDescription(AStyleDescription));
    TdxStyleType.CharacterStyle:
      AddCharacterStyle(TdxCharacterStyleDescription(AStyleDescription));
    TdxStyleType.TableStyle:
      AddTableStyle(TdxTableStyleDescription(AStyleDescription));
    TdxStyleType.NumberingListStyle:
      AddListStyle(TdxListStyleDescription(AStyleDescription));
  end;
end;

procedure TdxDocStylesImportHelper.AddParagraphStyle(AStyleDescription: TdxParagraphStyleDescription);
var
  AParagraphStyle, AParent: TdxParagraphStyle;
begin
  AParagraphStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(AStyleDescription.StyleName));
  if AParagraphStyle = nil then
  begin
    AParagraphStyle := TdxParagraphStyle.Create(DocumentModel);
    AParagraphStyle.StyleName := AStyleDescription.StyleName;
    AParagraphStyle.Hidden := AStyleDescription.Hidden;
    DocumentModel.ParagraphStyles.Add(AParagraphStyle);
  end;
  if AStyleDescription.BaseStyleIndex <> TdxStyleDescriptionBase.EmptyStyleIdentifier then
  begin
    AParent := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(StyleMapping[AStyleDescription.BaseStyleIndex]));
    AParagraphStyle.Parent := AParent;
  end;
  AParagraphStyle.Primary := AStyleDescription.QFormat;
  SetParagraphStyleProperties(AParagraphStyle, AStyleDescription);
end;

procedure TdxDocStylesImportHelper.SetParagraphStyleProperties(AStyle: TdxParagraphStyle; ADescription: TdxParagraphStyleDescription);
var
  AContainer: TdxDocPropertyContainer;
  ACharacterFormattingInfo: TdxCharacterFormattingInfo;
  AParagraphInfo: TdxParagraphInfo;
  AFrameInfo: TdxFrameInfo;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
  AMergedFrameProperties: TdxMergedFrameProperties;
  AFrameProperties: TdxFrameProperties;
begin
  AContainer := TdxDocCommandHelper.Traverse(ADescription.ParagraphUPX, ContentBuilder.Factory, ContentBuilder.DataReader);
  ContentBuilder.AddToGC(AContainer);

  TdxDocCommandHelper.Traverse(ADescription.CharacterUPX, AContainer, ContentBuilder.DataReader);
  SetParagraphStyleOutlineLevel(ADescription, AContainer);
  ContentBuilder.FontManager.SetFontName(AContainer);
  if AContainer.CharacterInfo <> nil then
  begin
    ACharacterFormattingInfo := GetParagraphStyleFormattingInfo(ADescription.BaseStyleIndex, AContainer);
    try
      AMergedCharacterProperties := TdxMergedCharacterProperties.Create(ACharacterFormattingInfo, AContainer.CharacterInfo.FormattingOptions);
      try
        AStyle.CharacterProperties.CopyFrom(AMergedCharacterProperties);
      finally
        AMergedCharacterProperties.Free;
      end;
    finally
      ACharacterFormattingInfo.Free;
    end;
  end;
  AParagraphInfo := AContainer.ParagraphInfo;
  if AParagraphInfo <> nil then
  begin
    AMergedParagraphProperties := TdxMergedParagraphProperties.Create(AParagraphInfo.FormattingInfo, AParagraphInfo.FormattingOptions);
    try
      AStyle.ParagraphProperties.CopyFrom(AMergedParagraphProperties);
    finally
      AMergedParagraphProperties.Free;
    end;
    AStyle.Tabs.SetTabs(AParagraphInfo.Tabs);
    ProcessParagraphListInfoIndex(AStyle, AParagraphInfo);
  end;
  AFrameInfo := AContainer.FrameInfo;
  if (AFrameInfo <> nil) and DocumentModel.DocumentCapabilities.ParagraphFramesAllowed then
  begin
    FIsContainsParagraphFrame := True;
    if AStyle.FrameProperties = nil then
    begin
      AFrameProperties := TdxFrameProperties.Create(AStyle);
      AStyle.FrameProperties := AFrameProperties;
    end;
    AMergedFrameProperties := TdxMergedFrameProperties.Create(AFrameInfo.FormattingInfo, AFrameInfo.FormattingOptions);
    try
      AStyle.FrameProperties.CopyFrom(AMergedFrameProperties);
    finally
      AMergedFrameProperties.Free;
    end;
  end;
end;

procedure TdxDocStylesImportHelper.SetParagraphStyleOutlineLevel(ADescription: TdxParagraphStyleDescription; APropertyContainer: TdxDocPropertyContainer);
var
  AInfo: TdxParagraphInfo;
begin
  AInfo := APropertyContainer.ParagraphInfo;
  if ((AInfo <> nil) and (ADescription.StyleIndex >= 1)) and (ADescription.StyleIndex <= 9) then
  begin
    AInfo.FormattingInfo.OutlineLevel := ADescription.StyleIndex;
    AInfo.FormattingOptions.UseOutlineLevel := True;
  end;
end;

procedure TdxDocStylesImportHelper.ProcessParagraphListInfoIndex(AStyle: TdxParagraphStyle; AInfo: TdxParagraphInfo);
var
  AInfoListLevel, AListInfoIndex: Integer;
begin
  AInfoListLevel := AInfo.ListLevel;
  if (AInfoListLevel >= 0) and (AInfoListLevel < 9) then
    AStyle.SetNumberingListLevelIndex(AInfo.ListLevel)
  else
    AStyle.SetNumberingListLevelIndex(0);
  AListInfoIndex := AInfo.ListInfoIndex - 1;
  if (AListInfoIndex < -1) or (AListInfoIndex >= DocumentModel.NumberingLists.Count) then
    Exit;
  if AListInfoIndex = -1 then
    AStyle.SetNumberingListIndex(NumberingListIndexNoNumberingList)
  else
    AStyle.SetNumberingListIndex(AListInfoIndex);
end;

procedure TdxDocStylesImportHelper.AddCharacterStyle(AStyleDescription: TdxCharacterStyleDescription);
var
  ACharacterStyle, AParent: TdxCharacterStyle;
begin
  ACharacterStyle := TdxCharacterStyle(DocumentModel.CharacterStyles.GetStyleByName(AStyleDescription.StyleName));

  if ACharacterStyle = nil then
  begin
    ACharacterStyle := TdxCharacterStyle.Create(DocumentModel);
    ACharacterStyle.StyleName := AStyleDescription.StyleName;
    ACharacterStyle.Hidden := AStyleDescription.Hidden;
    DocumentModel.CharacterStyles.Add(ACharacterStyle);
  end;
  if AStyleDescription.BaseStyleIndex <> TdxStyleDescriptionBase.EmptyStyleIdentifier then
  begin
    AParent := TdxCharacterStyle(DocumentModel.CharacterStyles.GetStyleByName(StyleMapping[AStyleDescription.BaseStyleIndex]));
    ACharacterStyle.Parent := AParent;
  end;
  ACharacterStyle.Primary := AStyleDescription.QFormat;
  SetCharacterStyleProperties(ACharacterStyle, AStyleDescription);
end;

procedure TdxDocStylesImportHelper.SetCharacterStyleProperties(AStyle: TdxCharacterStyle; ADescription: TdxCharacterStyleDescription);
var
  AContainer: TdxDocPropertyContainer;
  AFormattingInfo: TdxCharacterFormattingInfo;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  AContainer := TdxDocCommandHelper.Traverse(ADescription.CharacterUPX, ContentBuilder.Factory, ContentBuilder.DataReader);
  ContentBuilder.AddToGC(AContainer);

  ContentBuilder.FontManager.SetFontName(AContainer);
  AFormattingInfo := GetCharacterStyleFormattingInfo(ADescription.BaseStyleIndex, AContainer);
  try
    if AContainer.CharacterInfo <> nil then
    begin
      AMergedCharacterProperties := TdxMergedCharacterProperties.Create(AFormattingInfo, AContainer.CharacterInfo.FormattingOptions);
      try
        AStyle.CharacterProperties.CopyFrom(AMergedCharacterProperties);
      finally
        AMergedCharacterProperties.Free;
      end;
    end;
  finally
    AFormattingInfo.Free;
  end;
end;

procedure TdxDocStylesImportHelper.AddTableStyle(AStyleDescription: TdxTableStyleDescription);
var
  ATableStyle, AParent: TdxTableStyle;
begin
  ATableStyle := TdxTableStyle(DocumentModel.TableStyles.GetStyleByName(AStyleDescription.StyleName));

  if ATableStyle = nil then
  begin
    ATableStyle := TdxTableStyle.Create(DocumentModel);
    ATableStyle.StyleName := AStyleDescription.StyleName;
    ATableStyle.Hidden := AStyleDescription.Hidden;
    DocumentModel.TableStyles.Add(ATableStyle);
  end;
  if AStyleDescription.BaseStyleIndex <> TdxStyleDescriptionBase.EmptyStyleIdentifier then
  begin
    AParent := TdxTableStyle(DocumentModel.TableStyles.GetStyleByName(StyleMapping[AStyleDescription.BaseStyleIndex]));
    ATableStyle.Parent := AParent;
  end;
  ATableStyle.Primary := AStyleDescription.QFormat;
  SetTableStyleProperties(ATableStyle, AStyleDescription);
end;

procedure TdxDocStylesImportHelper.SetTableStyleProperties(AStyle: TdxTableStyle; ADescription: TdxTableStyleDescription);
var
  AContainer: TdxDocPropertyContainer;
  ACharacterFormattingInfo: TdxCharacterFormattingInfo;
  AParagraphInfo: TdxParagraphInfo;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  AContainer := TdxDocCommandHelper.Traverse(ADescription.CharacterUPX, ContentBuilder.Factory, ContentBuilder.DataReader);
  ContentBuilder.AddToGC(AContainer);

  TdxDocCommandHelper.Traverse(ADescription.ParagraphUPX, AContainer, ContentBuilder.DataReader);
  TdxDocCommandHelper.Traverse(ADescription.TableUPX, AContainer, ContentBuilder.DataReader);
  ContentBuilder.FontManager.SetFontName(AContainer);

  ACharacterFormattingInfo := GetTableStyleFormattingInfo(ADescription.BaseStyleIndex, AContainer);
  try
    if AContainer.CharacterInfo <> nil then
    begin
      AMergedCharacterProperties := TdxMergedCharacterProperties.Create(ACharacterFormattingInfo, AContainer.CharacterInfo.FormattingOptions);
      try
        AStyle.CharacterProperties.CopyFrom(AMergedCharacterProperties);
      finally
        AMergedCharacterProperties.Free;
      end;
    end;
  finally
    ACharacterFormattingInfo.Free;
  end;
  AParagraphInfo := AContainer.ParagraphInfo;
  if AParagraphInfo <> nil then
  begin
    AMergedParagraphProperties := TdxMergedParagraphProperties.Create(AParagraphInfo.FormattingInfo, AParagraphInfo.FormattingOptions);
    try
      AStyle.ParagraphProperties.CopyFrom(AMergedParagraphProperties);
    finally
      AMergedParagraphProperties.Free;
    end;
    AStyle.Tabs.SetTabs(AParagraphInfo.Tabs);
  end;
  if AContainer.TableInfo <> nil then
    AStyle.TableProperties.CopyFrom(AContainer.TableInfo.TableProperties);
  if AContainer.TableRowInfo <> nil then
    AStyle.TableRowProperties.CopyFrom(AContainer.TableRowInfo.TableRowProperties);
  if AContainer.TableCellInfo <> nil then
    AStyle.TableCellProperties.CopyFrom(AContainer.TableCellInfo.TableCellProperties);
end;

procedure TdxDocStylesImportHelper.AddListStyle(AStyleDescription: TdxListStyleDescription);
var
  AListStyle: TdxNumberingListStyle;
  AContainer: TdxDocPropertyContainer;
  AIndex: Integer;
begin
  AListStyle := TdxNumberingListStyle(DocumentModel.NumberingListStyles.GetStyleByName(AStyleDescription.StyleName));

  AContainer := TdxDocCommandHelper.Traverse(AStyleDescription.ParagraphUPX, ContentBuilder.Factory, ContentBuilder.DataReader);
  ContentBuilder.AddToGC(AContainer);

  TdxDocCommandHelper.Traverse(AStyleDescription.ParagraphUPX, AContainer, ContentBuilder.DataReader);
  if AContainer.ParagraphInfo = nil then
    Exit;
  AIndex := AContainer.ParagraphInfo.ListInfoIndex;
  if AIndex <= 0 then
    Exit;
  Dec(AIndex);
  if AIndex > DocumentModel.NumberingLists.Count - 1 then
    Exit;
  if AListStyle = nil then
  begin
    AListStyle := TdxNumberingListStyle.Create(FDocumentModel, AStyleDescription.StyleName);
    AListStyle.Hidden := AStyleDescription.Hidden;
    DocumentModel.NumberingListStyles.Add(AListStyle);
  end;
  AListStyle.SetNumberingListIndex(AIndex);
  AListStyle.Primary := AStyleDescription.QFormat;
end;

procedure TdxDocStylesImportHelper.SetNextParagraphStyles;
var
  AStyles: TdxStyleDescriptionCollection;
  ACount, I: Integer;
  AStyle: TdxStyleDescriptionBase;
  ACurrentStyle, ANextStyle: TdxParagraphStyle;
  ANextStyleName: string;
begin
  AStyles := ContentBuilder.StyleSheet.Styles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
  begin
    AStyle := AStyles[I];
    if (AStyle is TdxParagraphStyleDescription) and (AStyle.NextStyleIndex <> TdxStyleDescriptionBase.EmptyStyleIdentifier) then
    begin
      ACurrentStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(AStyle.StyleName));
      if StyleMapping.TryGetValue(AStyle.NextStyleIndex, ANextStyleName) then
      begin
        ANextStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(ANextStyleName));
        ACurrentStyle.NextParagraphStyle := ANextStyle;
      end;
    end;
  end;
end;

procedure TdxDocStylesImportHelper.CreateStyleLinks;
var
  AStyles: TdxStyleDescriptionCollection;
  ACount, I: Integer;
  AParagraphStyle: TdxParagraphStyle;
  ACharacterStyle: TdxCharacterStyle;
  ACurrent: TdxStyleDescriptionBase;
  ACharacterStyleName, AParagraphStyleName: string;
begin
  AStyles := ContentBuilder.StyleSheet.Styles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
  begin
    AParagraphStyle := nil;
    ACharacterStyle := nil;
    ACurrent := AStyles[I];
    if (ACurrent is TdxParagraphStyleDescription) and (ACurrent.LinkedStyleIndex <> 0) then
    begin
      AParagraphStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(ACurrent.StyleName));
      if StyleMapping.TryGetValue(ACurrent.LinkedStyleIndex, ACharacterStyleName) then
        ACharacterStyle := TdxCharacterStyle(DocumentModel.CharacterStyles.GetStyleByName(ACharacterStyleName));
    end
    else
      if (ACurrent is TdxCharacterStyleDescription) and (ACurrent.LinkedStyleIndex <> 0) then
      begin
        ACharacterStyle := TdxCharacterStyle(DocumentModel.CharacterStyles.GetStyleByName(ACurrent.StyleName));
        if StyleMapping.TryGetValue(ACurrent.LinkedStyleIndex, AParagraphStyleName) then
          AParagraphStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(AParagraphStyleName));
      end;
    if (AParagraphStyle <> nil) and (ACharacterStyle <> nil) then
      DocumentModel.StyleLinkManager.CreateLink(AParagraphStyle, ACharacterStyle);
  end;
end;

procedure TdxDocStylesImportHelper.UpdateDefaultFontSize;
var
  ADefaultStyle: TdxParagraphStyle;
begin
  if DefaultParagraphStyleName = '' then
    Exit;
  ADefaultStyle := TdxParagraphStyle(DocumentModel.ParagraphStyles.GetStyleByName(DefaultParagraphStyleName));
  if not ADefaultStyle.CharacterProperties.UseDoubleFontSize then
    ADefaultStyle.CharacterProperties.DoubleFontSize := DefaultFontSize * 2;
end;

function TdxDocStylesImportHelper.GetCharacterFormattingInfo(AInfo: TdxDocCharacterFormattingInfo): TdxCharacterFormattingInfo;
var
  ACharacterStyleIndex: Integer;
  AFormattingInfo: TdxCharacterFormattingInfo;
begin
  ACharacterStyleIndex := GetStyleIndex(AInfo.StyleIndex, TdxStyleType.CharacterStyle);
  AFormattingInfo := GetFormattingInfoCore(ACharacterStyleIndex);
  try
    Result := TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(AInfo, AFormattingInfo, DocumentModel);
  finally
    AFormattingInfo.Free;
  end;
end;

function TdxDocStylesImportHelper.GetCharacterStyleFormattingInfo(ABaseStyleIndex: SmallInt; APropertyContainer: TdxDocPropertyContainer): TdxCharacterFormattingInfo;
var
  AParentStyleIndex: Integer;
  AFormattingInfo: TdxCharacterFormattingInfo;
begin
  AParentStyleIndex := GetStyleIndex(ABaseStyleIndex, TdxStyleType.CharacterStyle);
  AFormattingInfo := GetFormattingInfoCore(AParentStyleIndex);
  if APropertyContainer.CharacterInfo = nil then
    Exit(AFormattingInfo);
  try
    Result := TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(APropertyContainer.CharacterInfo.FormattingInfo, AFormattingInfo, DocumentModel);
  finally
    AFormattingInfo.Free;
  end;
end;

function TdxDocStylesImportHelper.GetFormattingInfoCore(ACharacterStyleIndex: Integer): TdxCharacterFormattingInfo;
var
  AStyle: TdxCharacterStyle;
begin
  if ACharacterStyleIndex >= 0 then
  begin
    AStyle := DocumentModel.CharacterStyles[ACharacterStyleIndex];
    Result := AStyle.CharacterProperties.Info.Info.Clone;
  end
  else
    Result := DocumentModel.Cache.CharacterFormattingCache[TdxCharacterFormattingInfoCache.DefaultItemIndex].Info.Clone;
end;

function TdxDocStylesImportHelper.GetParagraphStyleFormattingInfo(ABaseStyleIndex: SmallInt; APropertyContainer: TdxDocPropertyContainer): TdxCharacterFormattingInfo;
var
  AFormattingInfo: TdxCharacterFormattingInfo;
  AParentStyleIndex: Integer;
  AParent: TdxParagraphStyle;
begin
  AParentStyleIndex := GetStyleIndex(ABaseStyleIndex, TdxStyleType.ParagraphStyle);
  if ABaseStyleIndex <> TdxStyleDescriptionBase.EmptyStyleIdentifier then
  begin
    AParent := DocumentModel.ParagraphStyles[AParentStyleIndex];
    AFormattingInfo := AParent.CharacterProperties.Info.Info.Clone;
  end
  else
    AFormattingInfo := DocumentModel.Cache.CharacterFormattingCache[TdxCharacterFormattingInfoCache.DefaultItemIndex].Info.Clone;
  if APropertyContainer.CharacterInfo = nil then
    Exit(AFormattingInfo);
  try
    Result := TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(APropertyContainer.CharacterInfo.FormattingInfo, AFormattingInfo, DocumentModel);
  finally
    AFormattingInfo.Free;
  end;
end;

function TdxDocStylesImportHelper.GetTableStyleFormattingInfo(ABaseStyleIndex: SmallInt; APropertyContainer: TdxDocPropertyContainer): TdxCharacterFormattingInfo;
var
  AFormattingInfo: TdxCharacterFormattingInfo;
  AParent: TdxTableStyle;
begin
  if ABaseStyleIndex <> TdxStyleDescriptionBase.EmptyStyleIdentifier then
  begin
    AParent := DocumentModel.TableStyles[GetStyleIndex(ABaseStyleIndex, TdxStyleType.TableStyle)];
    AFormattingInfo := AParent.CharacterProperties.Info.Info.Clone;
  end
  else
    AFormattingInfo := DocumentModel.Cache.CharacterFormattingCache[TdxCharacterFormattingInfoCache.DefaultItemIndex].Info.Clone;
  if APropertyContainer.CharacterInfo = nil then
    Exit(AFormattingInfo);
  try
    Result := TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(APropertyContainer.CharacterInfo.FormattingInfo, AFormattingInfo, DocumentModel);
  finally
    AFormattingInfo.Free;
  end;
end;

end.
