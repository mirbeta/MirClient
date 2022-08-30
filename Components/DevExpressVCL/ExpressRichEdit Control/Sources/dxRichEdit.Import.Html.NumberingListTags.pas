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

unit dxRichEdit.Import.Html.NumberingListTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, dxCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.CSSParser;

type

  { TdxNumberingListTagBase }

  TdxNumberingListTagBase = class abstract(TdxTagBase)
  protected const
    BulletTypes: array[0..2] of string = ('disc', 'circle', 'square');
  protected
    class function SetBulletNumberingFormat(const AValue: string;
      AListLevelProperties: TdxHtmlListLevelProperties): TdxHtmlListLevelProperties; static;
    class function SetNumberigFormat(const AValue: string;
      AListLevelProperties: TdxHtmlListLevelProperties): TdxHtmlListLevelProperties; static;
    function GetListLevelProperties: TdxHtmlListLevelProperties; virtual; abstract;
    procedure ApplyTagProperties; override;
    function ApplyLeftIndent: TdxParagraphFormattingOptions;
    procedure OpenTagProcessCore; override;
    procedure CreateNewAbstractNumberingList; virtual; abstract;
    procedure CreateNewBulletAbstractNumberingList;
    procedure CreateNewSimpleAbstractNumberingList;

    property ListLevelProperties: TdxHtmlListLevelProperties read GetListLevelProperties;
  public
    procedure DeleteOldOpenTag; override;
    procedure FunctionalTagProcess; override;
  end;

  { TdxNumberingListTag }

  TdxNumberingListTag = class(TdxNumberingListTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FListLevelProperties: TdxHtmlListLevelProperties;
    FOldListLevelProperties: TdxHtmlListLevelProperties;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure TypeAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure StartAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    procedure CreateNewAbstractNumberingList; override;
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    function GetListLevelProperties: TdxHtmlListLevelProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;

    property ListLevelProperties: TdxHtmlListLevelProperties read FListLevelProperties;
    property OldListLevelProperties: TdxHtmlListLevelProperties read FOldListLevelProperties;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure ApplyProperties; override;
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
    procedure DeleteOldOpenTag; override;
  end;

  { TdxBulletListTag }

  TdxBulletListTag = class(TdxNumberingListTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FListLevelProperties: TdxHtmlListLevelProperties;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure TypeAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    procedure CreateNewAbstractNumberingList; override;
    function GetListLevelProperties: TdxHtmlListLevelProperties; override;
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure DeleteOldOpenTag; override;
  end;

  { TdxLevelTag }

  TdxLevelTag = class(TdxNumberingListTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    DefaultSpacing: Integer;
    FListLevelProperties: TdxHtmlListLevelProperties;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure TypeAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ValueAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    function GetListLevelProperties: TdxHtmlListLevelProperties; override;
    procedure FindKeywordInAttributeTable; override;
    procedure ApplyTagProperties; override;
    procedure ApplyAutoSpacing;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure CreateNewAbstractNumberingList; override;
    procedure OpenTagProcessCore; override;
    procedure AppendText(const AText: string); override;
    function IsPreviousCreatedListUsedBefore: Boolean;
    procedure MarkNumberingListUsed(AIndex: TdxNumberingListIndex);
    procedure SetDisplayFormatString(const ALevel: IdxListLevel);
    procedure ReplaceSimpleNumberingToBulleted(const ALevel: IdxListLevel); virtual;
    procedure CorrectLevelDisplayFormatString(const ALevel: IdxListLevel); virtual;
    procedure SetDotCharacter(const ALevel: IdxListLevel);
    procedure SetDotCharacterCore(const ALevel: IdxListLevel);

  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;

    procedure FunctionalTagProcess; override;
    function GetStartIndexAllowedSearchScope: Integer; override;
  end;

  { TdxDtTag }

  TdxDtTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxDlTag }

  TdxDlTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxDdTag }

  TdxDdTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

implementation

uses
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Import.Html,
  dxRichEdit.Import.Html.HTMLParser,
  dxCharacters,
  dxStringHelper;


type
  TdxNumberingListTagBaseHelper = class helper for TdxNumberingListTagBase
  private
    function GetImporter: TdxHtmlImporter; inline;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxNumberingListTagBaseHelper }

function TdxNumberingListTagBaseHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxNumberingListTagBase }

class function TdxNumberingListTagBase.SetBulletNumberingFormat(const AValue: string;
  AListLevelProperties: TdxHtmlListLevelProperties): TdxHtmlListLevelProperties;
var
  AValueName: string;
begin
  AValueName := LowerCase(AValue);
  if AValueName = 'disc' then
  begin
    AListLevelProperties.Format := TdxNumberingFormat.Bullet;
    AListLevelProperties.BulletFontName := 'Symbol';
  end
  else
    if AValueName = 'circle' then
    begin
      AListLevelProperties.Format := TdxNumberingFormat.Bullet;
      AListLevelProperties.BulletFontName := 'Courier New';
    end
    else
      if AValueName = 'square' then
      begin
        AListLevelProperties.Format := TdxNumberingFormat.Bullet;
        AListLevelProperties.BulletFontName := 'Wingdings';
      end;
  Result := AListLevelProperties;
end;

class function TdxNumberingListTagBase.SetNumberigFormat(const AValue: string;
  AListLevelProperties: TdxHtmlListLevelProperties): TdxHtmlListLevelProperties;
var
  ACh: Char;
begin
  if Length(AValue) = 1 then
  begin
    ACh := AValue[1];
    case ACh of
      'A':
        AListLevelProperties.Format := TdxNumberingFormat.UpperLetter;
      'a':
        AListLevelProperties.Format := TdxNumberingFormat.LowerLetter;
      'I':
        AListLevelProperties.Format := TdxNumberingFormat.UpperRoman;
      'i':
        AListLevelProperties.Format := TdxNumberingFormat.LowerRoman;
      '1':
        AListLevelProperties.Format := TdxNumberingFormat.Decimal;
    end;
  end;
  Result := AListLevelProperties;
end;


procedure TdxNumberingListTagBase.ApplyTagProperties;
var
  AProperties: TdxHtmlListLevelProperties;
begin
  if TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
  begin
    AProperties := ListLevelProperties.MergeWith(Importer.Position.ListLevelProperties);
    try
      Importer.Position.ListLevelProperties.CopyFrom(AProperties);
    finally
      AProperties.Free;
    end;
  end;
end;

function TdxNumberingListTagBase.ApplyLeftIndent: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
begin
  Importer.Position.ParagraphFormatting.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
  Importer.Position.ParagraphFormatting.FirstLineIndent := DocumentModel.DocumentProperties.DefaultTabWidth div 2;
  Importer.Position.ParagraphFormatting.LeftIndent := Importer.Position.ParagraphFormatting.LeftIndent +
    DocumentModel.DocumentProperties.DefaultTabWidth;
  AOptions := TdxParagraphFormattingOptions.Create([TdxUsedParagraphFormattingOption.UseFirstLineIndent,
    TdxUsedParagraphFormattingOption.UseLeftIndent]);
  Result := AOptions;
end;

procedure TdxNumberingListTagBase.FunctionalTagProcess;
begin
  if Importer.DocumentModel.ActivePieceTable.Paragraphs[Importer.Position.ParagraphIndex].IsInList then
    Importer.IsEmptyParagraph := False;
  ParagraphFunctionalProcess;
end;

procedure TdxNumberingListTagBase.OpenTagProcessCore;
var
  ALevel: TdxListLevel;
begin
  if not TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
    Exit;
  if Importer.Position.LevelIndex < 0 then
    CreateNewAbstractNumberingList;
  ALevel := TdxListLevel.Create(DocumentModel);
  if TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
    Importer.Position.LevelIndex := Importer.Position.LevelIndex + 1
  else
    Importer.Position.LevelIndex := 0;

  ALevel.CopyFrom(DocumentModel.AbstractNumberingLists.Last.Levels[Importer.Position.LevelIndex] as TdxListLevel);
  ALevel := Importer.Position.ListLevelProperties.ApplyPropertiesToListLevel(ALevel);
  DocumentModel.AbstractNumberingLists.Last.Levels[Importer.Position.LevelIndex] := ALevel;
end;

procedure TdxNumberingListTagBase.CreateNewBulletAbstractNumberingList;
begin
  TdxDefaultNumberingListHelper.InsertDefaultBulletNumberingList(DocumentModel, DocumentModel.UnitConverter, DocumentModel.DocumentProperties.DefaultTabWidth);
end;

procedure TdxNumberingListTagBase.CreateNewSimpleAbstractNumberingList;
begin
  TdxDefaultNumberingListHelper.InsertDefaultSimpleNumberingList(DocumentModel, DocumentModel.UnitConverter, DocumentModel.DocumentProperties.DefaultTabWidth);
end;

procedure TdxNumberingListTagBase.DeleteOldOpenTag;
var
  ACount, I: Integer;
  ATag: TdxTagBase;
  ALevelTag: TdxLevelTag;
begin
  ACount := Importer.TagsStack.Count;
  for I := ACount - 1 downto 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;
    if (ATag.Name = TdxHtmlTagNameID.NumberingList) or (ATag.Name = TdxHtmlTagNameID.BulletList) then
      Exit;
    if Importer.TagsStack[I].Tag.Name = TdxHtmlTagNameID.LI then
    begin
      ALevelTag := TdxLevelTag.Create(Importer);
      try
        Importer.CloseUnClosedTag(ALevelTag, I);
      finally
        ALevelTag.Free;
      end;
      Exit;
    end;
  end;
end;

{ TdxNumberingListTag }

constructor TdxNumberingListTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  FListLevelProperties := TdxHtmlListLevelProperties.Create;
end;

destructor TdxNumberingListTag.Destroy;
begin
  FreeAndNil(FListLevelProperties);
  FreeAndNil(FOldListLevelProperties);
  inherited Destroy;
end;

class constructor TdxNumberingListTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxNumberingListTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxNumberingListTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('type'), TypeAttributeKeyword);
  Result.Add(ConvertKeyToUpper('start'), StartAttributeKeyword);
end;

class procedure TdxNumberingListTag.TypeAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ANumberingListTag: TdxNumberingListTag;
begin
  ANumberingListTag := TdxNumberingListTag(ATag);
  SetNumberigFormat(AValue, ANumberingListTag.ListLevelProperties);
end;

class procedure TdxNumberingListTag.StartAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ANumberingListTag: TdxNumberingListTag;
begin
  ANumberingListTag := TdxNumberingListTag(ATag);
  ANumberingListTag.ListLevelProperties.Start := StrToInt(AValue);
end;

procedure TdxNumberingListTag.CreateNewAbstractNumberingList;
begin
  CreateNewSimpleAbstractNumberingList;
end;

function TdxNumberingListTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

function TdxNumberingListTag.GetListLevelProperties: TdxHtmlListLevelProperties;
begin
  Result := FListLevelProperties;
end;

function TdxNumberingListTag.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
begin
  AOptions := TdxParagraphFormattingOptions.EmptyParagraphFormattingOption;
  if TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
    AOptions.Value := AOptions.Value + inherited ApplyCssProperties.Value;
  if TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
    AOptions.Value := AOptions.Value + ApplyLeftIndent.Value;
  Result := AOptions;
end;

procedure TdxNumberingListTag.DeleteOldOpenTag;
begin
  if Tag.ElementType = TdxHtmlElementType.OpenTag then
    Exit;
  inherited DeleteOldOpenTag;
end;

procedure TdxNumberingListTag.ApplyProperties;
var
  AProperties: TdxHtmlListLevelProperties;
begin
  FOldListLevelProperties := TdxHtmlListLevelProperties.Create;
  FOldListLevelProperties.CopyFrom(Importer.Position.ListLevelProperties);
  AProperties := TdxHtmlListLevelProperties.Create;
  try
    Importer.Position.ListLevelProperties.CopyFrom(AProperties);
  finally
    AProperties.Free;
  end;
  inherited ApplyProperties;
end;

procedure TdxNumberingListTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  ATag: TdxNumberingListTag;
begin
  ATag := Safe<TdxNumberingListTag>.Cast(Importer.TagsStack[AIndexOfDeletedTag].Tag);
  if ATag <> nil then
    Importer.Position.ListLevelProperties.CopyFrom(ATag.OldListLevelProperties);
  inherited BeforeDeleteTagFromStack(AIndexOfDeletedTag);
end;

{ TdxBulletListTag }

constructor TdxBulletListTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  FListLevelProperties := TdxHtmlListLevelProperties.Create;
end;

destructor TdxBulletListTag.Destroy;
begin
  FListLevelProperties.Free;
  inherited Destroy;
end;

class constructor TdxBulletListTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxBulletListTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxBulletListTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
var
  ATable: TdxAttributeKeywordTranslatorTable;
begin
  ATable := CreateAttributeTable;
  ATable.Add(ConvertKeyToUpper('type'), TypeAttributeKeyword);
  Result := ATable;
end;

class procedure TdxBulletListTag.TypeAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ABulletListTag: TdxBulletListTag;
begin
  ABulletListTag := TdxBulletListTag(ATag);
  SetBulletNumberingFormat(AValue, ABulletListTag.ListLevelProperties);
end;

procedure TdxBulletListTag.CreateNewAbstractNumberingList;
begin
  CreateNewBulletAbstractNumberingList;
end;

function TdxBulletListTag.GetListLevelProperties: TdxHtmlListLevelProperties;
begin
  Result := FListLevelProperties;
end;

function TdxBulletListTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxBulletListTag.ApplyTagProperties;
begin
  if TdxDocumentFormatsHelper.NeedReplaceBulletedLevelsToDecimal(DocumentModel) then
  begin
    FListLevelProperties.BulletFontName := DocumentModel.DefaultCharacterProperties.FontName;
    FListLevelProperties.Format := TdxNumberingFormat.Decimal;
  end
  else
    if TdxDocumentFormatsHelper.ShouldInsertBulletedNumbering(DocumentModel) then
    begin
      if FListLevelProperties.Format <> TdxNumberingFormat.Bullet then
      begin
        FListLevelProperties.BulletFontName := 'Symbol';
        FListLevelProperties.Format := TdxNumberingFormat.Bullet;
      end;
    end
    else
      FListLevelProperties.Format := TdxNumberingFormat.None;
  inherited ApplyTagProperties;
end;

function TdxBulletListTag.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
begin
  AOptions := TdxParagraphFormattingOptions.EmptyParagraphFormattingOption;
  if TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
    AOptions.Value := AOptions.Value + inherited ApplyCssProperties.Value;
  if TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
    AOptions.Value := AOptions.Value + ApplyLeftIndent.Value;
  Result := AOptions;
end;

procedure TdxBulletListTag.DeleteOldOpenTag;
begin
  if Tag.ElementType = TdxHtmlElementType.OpenTag then
    Exit;
  inherited DeleteOldOpenTag;
end;

{ TdxLevelTag }

constructor TdxLevelTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  DefaultSpacing := 0;
  FListLevelProperties := TdxHtmlListLevelProperties.Create;
end;

destructor TdxLevelTag.Destroy;
begin
  FreeAndNil(FListLevelProperties);
  inherited Destroy;
end;

class constructor TdxLevelTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxLevelTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxLevelTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('type'), TypeAttributeKeyword);
  REsult.Add(ConvertKeyToUpper('value'), ValueAttributeKeyword);
end;

class procedure TdxLevelTag.TypeAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ALevelTag: TdxLevelTag;
begin
  ALevelTag := TdxLevelTag(ATag);
  SetNumberigFormat(AValue, ALevelTag.ListLevelProperties);
  SetBulletNumberingFormat(AValue, ALevelTag.ListLevelProperties);
end;

class procedure TdxLevelTag.ValueAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ALevelTag: TdxLevelTag;
begin
  ALevelTag := TdxLevelTag(ATag);
  try
    ALevelTag.ListLevelProperties.Start := StrToInt(AValue);
  except
  end;
end;

function TdxLevelTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

function TdxLevelTag.GetListLevelProperties: TdxHtmlListLevelProperties;
begin
  Result := FListLevelProperties;
end;

procedure TdxLevelTag.FindKeywordInAttributeTable;
begin
  if ((Importer.Position.LevelIndex >= 0) and (Tag.ElementType = TdxHtmlElementType.OpenTag)) and
    (Importer.Position.ListLevelProperties.Format = TdxNumberingFormat.Bullet) then
  begin
    Importer.Position.ListLevelProperties.UseBulletFontName := False;
    SetBulletNumberingFormat(BulletTypes[Importer.Position.LevelIndex mod Length(BulletTypes)], FListLevelProperties);
  end;
  inherited FindKeywordInAttributeTable;
end;

procedure TdxLevelTag.ApplyTagProperties;
var
  AProperties: TdxHtmlListLevelProperties;
begin
  ApplyAutoSpacing;
  AProperties := Importer.Position.ListLevelProperties.MergeWith(FListLevelProperties);
  try
    Importer.Position.ListLevelProperties.CopyFrom(AProperties);
  finally
    AProperties.Free;
  end;
end;

procedure TdxLevelTag.ApplyAutoSpacing;
var
  AFormatting: TdxParagraphFormattingBase;
begin
  AFormatting := Importer.Position.ParagraphFormatting;
  AFormatting.SpacingAfter := DefaultSpacing;
  AFormatting.SpacingBefore := DefaultSpacing;
end;

function TdxLevelTag.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
  AOldLeftIndentFromParentTags: Integer;
begin
  AOptions := TdxParagraphFormattingOptions.EmptyParagraphFormattingOption;
  if TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
  begin
    AOldLeftIndentFromParentTags := Importer.Position.ParagraphFormatting.LeftIndent;

    AOptions.Value := AOptions.Value + inherited ApplyCssProperties.Value;

    if (Importer.Position.LevelIndex >= 0) and AOptions.UseLeftIndent then
    begin

      Importer.Position.ParagraphFormatting.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
      Importer.Position.ParagraphFormatting.FirstLineIndent := DocumentModel.DocumentProperties.DefaultTabWidth div 2;
      Importer.Position.ParagraphFormatting.LeftIndent := AOldLeftIndentFromParentTags + Importer.Position.ParagraphFormatting.LeftIndent;
    end;
  end;
  Result := AOptions;
end;

procedure TdxLevelTag.FunctionalTagProcess;
begin
  if Importer.IsEmptyListItem then
  begin
    Importer.IsEmptyListItem := False;
    Importer.IsEmptyParagraph := False;
  end;
  ParagraphFunctionalProcess;
end;

function TdxLevelTag.GetStartIndexAllowedSearchScope: Integer;
var
  ACount, I: Integer;
  ATag: TdxTagBase;
begin
  ACount := Importer.TagsStack.Count;
  for I := ACount - 1 downto 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;
    if (ATag is TdxNumberingListTag) or (ATag is TdxBulletListTag) then
      Exit(I);
  end;
  Result := inherited GetStartIndexAllowedSearchScope;
end;

procedure TdxLevelTag.CreateNewAbstractNumberingList;
begin
  if Importer.Position.ListLevelProperties.Format = TdxNumberingFormat.Bullet then
    CreateNewBulletAbstractNumberingList
  else
    CreateNewSimpleAbstractNumberingList;
end;

procedure TdxLevelTag.OpenTagProcessCore;
var
  AParagraph: TdxParagraph;
  AAbstractLastLevel: TdxAbstractListLevel;
  ALevel: TdxListLevel;
  ALevelIndex: Integer;
  ANumberingListIndex: TdxNumberingListIndex;
begin
  if (not DocumentModel.DocumentCapabilities.ParagraphsAllowed) or
    (not TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel)) then
    Exit;
  AParagraph := DocumentModel.MainPieceTable.Paragraphs[Importer.Position.ParagraphIndex];
  if Importer.Position.LevelIndex < 0 then
    Importer.AppendText(DocumentModel.MainPieceTable, TdxCharacters.Bullet + '  ')
  else
  begin
    AAbstractLastLevel := DocumentModel.AbstractNumberingLists.Last.Levels[Importer.Position.LevelIndex];

    ALevel := TdxListLevel.Create(DocumentModel);
    try
      ALevel.ListLevelProperties.CopyFrom(AAbstractLastLevel.ListLevelProperties);
      ALevel.CharacterProperties.BeginInit;
      ALevel.CharacterProperties.CopyFrom(AAbstractLastLevel.CharacterProperties);
      ALevel.CharacterProperties.EndInit;

      if Importer.Position.ListLevelProperties.UseFormat then
        ALevel.ListLevelProperties.Format := Importer.Position.ListLevelProperties.Format;

      if Importer.Position.ListLevelProperties.UseStart then
        ALevel.ListLevelProperties.Start := Importer.Position.ListLevelProperties.Start;

      if not ALevel.ListLevelProperties.Info.Equals(AAbstractLastLevel.ListLevelProperties.Info) then
      begin
        if IsPreviousCreatedListUsedBefore then
          CreateNewAbstractNumberingList;
        SetDisplayFormatString(ALevel);
        DocumentModel.AbstractNumberingLists.Last.Levels[Importer.Position.LevelIndex] := ALevel;
        ALevel := nil;
      end
      else
        SetDisplayFormatString(AAbstractLastLevel);
      try
        ALevelIndex := Importer.Position.LevelIndex;
        if not TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
        begin
          ALevelIndex := 0;
          Importer.Position.LevelIndex := 0;
        end;
        ANumberingListIndex := DocumentModel.NumberingLists.Count - 1;
        DocumentModel.MainPieceTable.AddNumberingListToParagraph(AParagraph, ANumberingListIndex, ALevelIndex);
        MarkNumberingListUsed(ANumberingListIndex);

        Importer.SetAppendObjectProperty;
        Importer.IsEmptyListItem := True;
      except

      end;
    finally
      ALevel.Free;
    end;
  end;
end;

procedure TdxLevelTag.AppendText(const AText: string);
begin
  Importer.AppendText(TdxStringHelper.TrimStart(AText, [TdxCharacters.Space]));
end;

function TdxLevelTag.IsPreviousCreatedListUsedBefore: Boolean;
begin
  Result := Importer.UsedNumberingLists.Contains(DocumentModel.NumberingLists.Count - 1);
end;

procedure TdxLevelTag.MarkNumberingListUsed(AIndex: TdxNumberingListIndex);
begin
  if not Importer.UsedNumberingLists.Contains(AIndex) then
    Importer.UsedNumberingLists.Add(AIndex);
end;

procedure TdxLevelTag.SetDisplayFormatString(const ALevel: IdxListLevel);
begin
  if TdxDocumentFormatsHelper.NeedReplaceSimpleToBulletNumbering(DocumentModel) then
  begin
    ReplaceSimpleNumberingToBulleted(ALevel);
    Exit;
  end;
  CorrectLevelDisplayFormatString(ALevel);
end;

procedure TdxLevelTag.ReplaceSimpleNumberingToBulleted(const ALevel: IdxListLevel);
begin
  ALevel.CharacterProperties.FontName := 'Symbol';
  SetDotCharacterCore(ALevel);
end;

procedure TdxLevelTag.CorrectLevelDisplayFormatString(const ALevel: IdxListLevel);
begin
  if Importer.Position.ListLevelProperties.Format <> TdxNumberingFormat.Bullet then
    ALevel.ListLevelProperties.DisplayFormatString := Format('%%%d:s.', [Importer.Position.LevelIndex])
  else
    SetDotCharacter(ALevel);
end;

procedure TdxLevelTag.SetDotCharacter(const ALevel: IdxListLevel);
begin
  ALevel.CharacterProperties.FontName := Importer.Position.ListLevelProperties.BulletFontName;
  SetDotCharacterCore(ALevel);
end;

procedure TdxLevelTag.SetDotCharacterCore(const ALevel: IdxListLevel);
begin
  if UpperCase(ALevel.CharacterProperties.FontName) = 'COURIER NEW' then
    ALevel.ListLevelProperties.DisplayFormatString := 'o'
  else
    if UpperCase(ALevel.CharacterProperties.FontName) = 'WINGDINGS' then
      ALevel.ListLevelProperties.DisplayFormatString := #$00A7
    else
      ALevel.ListLevelProperties.DisplayFormatString := TdxCharacters.MiddleDot;
end;

{ TdxDtTag }

procedure TdxDtTag.ApplyTagProperties;
begin
end;

{ TdxDlTag }

procedure TdxDlTag.ApplyTagProperties;
begin
end;

{ TdxDdTag }

procedure TdxDdTag.ApplyTagProperties;
begin
end;

end.
