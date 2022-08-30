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

unit dxRichEdit.Import.Html.HtmlTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, dxCore,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.Html.DocumentTags;

type

  { TdxAnchorTag }

  TdxAnchorTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
  protected
    FAnchorName: string;
    FTooltip: string;
    FTarget: string;
    FBookmark: string;
    FNavigateUri: string;
    FHasHrefKeyword: Boolean;
    FOldPosition: TdxHtmlInputPosition;
    class procedure HrefKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure NameKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure HrefLangKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TypeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure RelKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure RefKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure CharsetKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TitleKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TargetKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure OpenTagProcessCore; override;
    procedure SaveCurrentPosition;
    procedure RemoveEmptyHyperlink(AIndexOfDeletedTag: Integer; AFieldInfo: TdxImportFieldInfo);
    function CreateHyperlinkInfo: TdxHyperlinkInfo; virtual;

    property OldPosition: TdxHtmlInputPosition read FOldPosition;
  public
    destructor Destroy; override;
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
    procedure EmptyTagProcess; override;
    procedure FunctionalTagProcess; override;

    property AnchorName: string read FAnchorName write FAnchorName;
    property Tooltip: string read FTooltip write FTooltip;
    property Target: string read FTarget write FTarget;
    property Bookmark: string read FBookmark write FBookmark;
    property NavigateUri: string read FNavigateUri write FNavigateUri;
    property HasHrefKeyword: Boolean read FHasHrefKeyword write FHasHrefKeyword;
  end;

 { TdxHtmlBookmarkInfo }

  TdxHtmlBookmarkInfo = class(TdxImportBookmarkInfo);

  { TdxHtmlTableInfo }

  TdxHtmlTableInfo = class(TdxTableInfo)
  strict private
    FCellsRowSpanCollection: TdxCellsRowSpanCollection;
    FColumnIndex: Integer;
  public
    constructor Create(ATable: TdxTable);
    destructor Destroy; override;
    procedure MoveNextRow;

    property CellsRowSpanCollection: TdxCellsRowSpanCollection read FCellsRowSpanCollection;
    property ColumnIndex: Integer read FColumnIndex write FColumnIndex;
  end;

  { TdxBaseTag }

  TdxBaseTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FBaseUri: string;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure BaseUriKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    class procedure TargetUriKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    procedure ApplyTagProperties; override;
    procedure OpenTagProcessCore; override;
    property BaseUri: string read FBaseUri write FBaseUri;
  end;

  { TdxBgsoundTag }

  TdxBgsoundTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxScriptTag }

  TdxScriptTag = class(TdxIgnoredTag);

  { TdxNoScriptTag }

  TdxNoScriptTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

implementation

uses
  StrUtils,
  dxStringHelper,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Html,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Core;

type
  TdxAnchorTagHelper = class helper for TdxAnchorTag
  private
    function GetImporter: TdxHtmlImporter;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxAnchorTagHelper }

function TdxAnchorTagHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxAnchorTag }

destructor TdxAnchorTag.Destroy;
begin
  FreeAndNil(FOldPosition);
  inherited Destroy;
end;

class constructor TdxAnchorTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxAnchorTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxAnchorTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('href'), HrefKeyword);
  Result.Add(ConvertKeyToUpper('title'), TitleKeyword);
  Result.Add(ConvertKeyToUpper('target'), TargetKeyword);
  Result.Add(ConvertKeyToUpper('name'), NameKeyword);
  Result.Add(ConvertKeyToUpper('hreflang'), HrefLangKeyword);
  Result.Add(ConvertKeyToUpper('type'), TypeKeyword);
  Result.Add(ConvertKeyToUpper('rel'), RelKeyword);
  Result.Add(ConvertKeyToUpper('ref'), RefKeyword);
  Result.Add(ConvertKeyToUpper('charset'), CharsetKeyword);
end;

class procedure TdxAnchorTag.HrefKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AAnchor: TdxAnchorTag;
  N: Integer;
  AHier, AFragment: string;
begin
  AAnchor := TdxAnchorTag(ATag);
  AAnchor.HasHrefKeyword := True;
  N := TdxStringHelper.IndexOf(AValue, '#');
  if N = 0 then
    AAnchor.Bookmark := TdxStringHelper.TrimStart(AValue, ['#'])
  else
  begin
    if N = -1 then
    begin
      AHier := AValue;
      AFragment := '';
    end
    else
    begin
      AHier := TdxStringHelper.Substring(AValue, 0, N);
      AFragment := TdxStringHelper.Substring(AValue, N);
    end;
    AAnchor.NavigateUri := TdxHyperlinkUriHelper.EnsureUriIsValid(AHier);
    AAnchor.Bookmark := TdxStringHelper.TrimStart(AFragment, ['#']);
  end;
end;

class procedure TdxAnchorTag.NameKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
  TdxAnchorTag(ATag).AnchorName := AValue;
end;

class procedure TdxAnchorTag.HrefLangKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxAnchorTag.TypeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxAnchorTag.RelKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxAnchorTag.RefKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxAnchorTag.CharsetKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxAnchorTag.TitleKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
  TdxAnchorTag(ATag).Tooltip := AValue;
end;

class procedure TdxAnchorTag.TargetKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
  (TdxAnchorTag(ATag)).Target := AValue;
end;

function TdxAnchorTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxAnchorTag.ApplyTagProperties;
begin
end;

procedure TdxAnchorTag.FunctionalTagProcess;
begin
  Importer.IsEmptyLine := False;
end;

function TdxAnchorTag.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AStyleIndex: Integer;
  AStyleProperties: TdxMergedCharacterProperties;
  AStyleOptions: TdxUsedCharacterFormattingOptions;
  AResult: TdxParagraphFormattingOptions;
begin
  if FHasHrefKeyword then
  begin
    AStyleIndex := DocumentModel.CharacterStyles.GetStyleIndexByName(TdxCharacterStyleCollection.HyperlinkStyleName);
    AStyleProperties := DocumentModel.CharacterStyles[AStyleIndex].GetMergedCharacterProperties;
    try
      AStyleOptions := AStyleProperties.Options.Value;
      Importer.Position.CharacterStyleIndex := AStyleIndex;
      Importer.Position.CharacterFormatting.ResetUse(AStyleOptions);
    finally
      AStyleProperties.Free;
    end;
  end;
  AResult := inherited ApplyCssProperties;
  Result := AResult;
end;

procedure TdxAnchorTag.OpenTagProcessCore;
var
  AHyperlinkInfo: TdxHyperlinkInfo;
begin
  if (FNavigateUri <> '') or (FBookmark <> '') then
    Importer.LastOpenAnchorTagIndex := Importer.TagsStack.Count - 1;
  if FAnchorName = '' then
    FAnchorName := Id;
  if Importer.ProcessHyperlink <> nil then
    Importer.ProcessHyperlinkEnd;
  if FAnchorName <> '' then
    Importer.ProcessBookmarkStart(FAnchorName);
  if (FNavigateUri <> '') or (FBookmark <> '') then
  begin
    SaveCurrentPosition;
    AHyperlinkInfo := CreateHyperlinkInfo;
    try
      Importer.ProcessHyperlinkStart(AHyperlinkInfo);
    finally
      AHyperlinkInfo.Free;
    end;
  end;
end;

procedure TdxAnchorTag.SaveCurrentPosition;
begin
  FOldPosition := TdxHtmlInputPosition.Create(Importer.PieceTable);
  FOldPosition.CopyFrom(Importer.Position);
end;

procedure TdxAnchorTag.EmptyTagProcess;
var
  APieceTable: TdxPieceTable;
  AEndLogPosition: TdxDocumentLogPosition;
begin
  FindKeywordInAttributeTable;
  if Id = '' then
    Exit;
  APieceTable := Importer.PieceTable;
  AEndLogPosition := APieceTable.DocumentEndLogPosition;
  APieceTable.CreateBookmark(AEndLogPosition, 0, Id);
end;

procedure TdxAnchorTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  AFieldInfo: TdxImportFieldInfo;
begin
  AFieldInfo := Importer.ProcessHyperlink;
  try
    if AFieldInfo <> nil then
    begin
      Importer.ProcessHyperlinkEnd;
      if AFieldInfo.CodeEndIndex + 1 = AFieldInfo.ResultEndIndex then
        RemoveEmptyHyperlink(AIndexOfDeletedTag, AFieldInfo);
    end;
  finally
    if Importer.ProcessHyperlink <> AFieldInfo then
      AFieldInfo.Free;
  end;

  if Importer.ProcessBookmarks.Count > 0 then
    Importer.ProcessBookmarkEnd;
end;

procedure TdxAnchorTag.RemoveEmptyHyperlink(AIndexOfDeletedTag: Integer; AFieldInfo: TdxImportFieldInfo);
var
  ACount: Integer;
  APos: TdxDocumentModelPosition;
  APosition: TdxHtmlInputPosition;
  ATag: TdxAnchorTag;
begin
  Importer.PieceTable.RemoveField(AFieldInfo.Field);
  ACount := AFieldInfo.ResultEndIndex - AFieldInfo.CodeStartIndex + 1;
  Importer.DocumentModel.UnsafeEditor.DeleteRuns(Importer.PieceTable, AFieldInfo.CodeStartIndex, ACount);
  APos := TdxDocumentModelPosition.FromRunStart(Importer.PieceTable, AFieldInfo.CodeStartIndex);
  APosition := Importer.Position;
  APosition.LogPosition := APos.LogPosition;
  APosition.ParagraphIndex := APos.ParagraphIndex;
  ATag := Safe<TdxAnchorTag>.Cast(Importer.TagsStack[AIndexOfDeletedTag].Tag);
  if ATag <> nil then
    APosition.CopyFrom(ATag.OldPosition);
end;

function TdxAnchorTag.CreateHyperlinkInfo: TdxHyperlinkInfo;
begin
  Result := TdxHyperlinkInfo.Create;
  Result.Anchor := Bookmark;
  Result.NavigateUri := NavigateUri;
  Result.Target := Target;
  Result.ToolTip := Tooltip;
end;

{ TdxHtmlTableInfo }

constructor TdxHtmlTableInfo.Create(ATable: TdxTable);
begin
  inherited Create(ATable);
  FCellsRowSpanCollection := TdxCellsRowSpanCollection.Create;
end;

destructor TdxHtmlTableInfo.Destroy;
begin
  FreeAndNil(FCellsRowSpanCollection);
  inherited Destroy;
end;

procedure TdxHtmlTableInfo.MoveNextRow;
begin
  FColumnIndex := 0;
end;

{ TdxBaseTag }

class constructor TdxBaseTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxBaseTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxBaseTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('href'), BaseUriKeyword);
  Result.Add(ConvertKeyToUpper('target'), TargetUriKeyword);
end;

class procedure TdxBaseTag.BaseUriKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ABaseTag: TdxBaseTag;
begin
  ABaseTag := TdxBaseTag(ATag);
  ABaseTag.BaseUri := AValue;
end;

function TdxBaseTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

class procedure TdxBaseTag.TargetUriKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

procedure TdxBaseTag.ApplyTagProperties;
begin
end;

procedure TdxBaseTag.OpenTagProcessCore;
begin
  if FBaseUri <> '' then
    TdxHtmlImporter(Importer).BaseUri := FBaseUri;
end;

{ TdxBgsoundTag }

procedure TdxBgsoundTag.ApplyTagProperties;
begin
end;

{ TdxNoScriptTag }

procedure TdxNoScriptTag.ApplyTagProperties;
begin
end;

end.
