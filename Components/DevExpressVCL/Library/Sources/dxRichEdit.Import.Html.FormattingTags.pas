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

unit dxRichEdit.Import.Html.FormattingTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, dxCore,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.Html.DocumentTags,
  dxRichEdit.DocumentModel.ParagraphFormatting;

type

  { TdxLineBreakTag }

  TdxLineBreakTag = class(TdxTagBase)
  protected
    function GetCanAppendToTagStack: Boolean; override;
    procedure ApplyTagProperties; override;
  public
    procedure EmptyTagProcess; override;
    procedure FunctionalTagProcess; override;
  end;

  { TdxParagraphTagBase }

  TdxParagraphTagBase = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    DefaultSpacingInPoints: Integer;
    FDefaultSpacing: Integer;
    FAlignment: TdxHtmlParagraphAlignment;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure ParagraphAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure InitializeAlignment;
    procedure ApplyTagProperties; override;
    function GetActualAlignmentValue: TdxParagraphAlignment; virtual;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;

    property Alignment: TdxHtmlParagraphAlignment read FAlignment;
    property DefaultSpacing: Integer read FDefaultSpacing;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure FunctionalTagProcess; override;
  end;

  { TdxDivisionTag }

  TdxDivisionTag = class(TdxParagraphTagBase)
  protected
    function GetActualAlignmentValue: TdxParagraphAlignment; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
  end;

  { TdxHeadingTag }

  TdxHeadingTag = class(TdxParagraphTagBase)
  strict private
    FFontSize: Integer;
    FOutlineLevel: Integer;
  protected
    procedure ApplyTagProperties; override;
    procedure ResetParagraphFormattingProperties(AParagraphFormatting: TdxParagraphFormattingBase);
  public
    constructor Create(AImporter: TdxCustomHtmlImporter; AFontSize: Integer; AOutlineLevel: Integer);
  end;

  { TdxMarqueeTag }

  TdxMarqueeTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxParagraphTag }

  TdxParagraphTag = class(TdxParagraphTagBase)
  protected
    procedure ApplyTagProperties; override;
    procedure OpenTagProcessCore; override;
    procedure ApplyAutoSpacing(AParagraphIndex: TdxParagraphIndex); virtual;
  public
    procedure FunctionalTagProcess; override;
  end;

  { TdxPreformattedTag }

  TdxPreformattedTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  public
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
    procedure FunctionalTagProcess; override;
  end;

  { TdxSpanTag }

  TdxSpanTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxHrTag }

  TdxHrTag = class(TdxTagBase)
  protected
    function GetCanAppendToTagStack: Boolean; override;
    function GetApplyStylesToInnerHtml: Boolean; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure ApplyTagProperties; override;
  public
    procedure ApplyProperties; override;
    procedure EmptyTagProcess; override;
  end;


implementation

uses
  dxRichEdit.NativeApi,
  dxRichEdit.Import.Html,
  dxRichEdit.Import.Html.NumberingListTags,
  dxCharacters,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Core;

type
  TdxTagBaseHelper = class helper for TdxTagBase
  private
    function GetImporter: TdxHtmlImporter; inline;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxTagBaseHelper }

function TdxTagBaseHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxLineBreakTag }

function TdxLineBreakTag.GetCanAppendToTagStack: Boolean;
begin
  Result := False;
end;

procedure TdxLineBreakTag.EmptyTagProcess;
begin
  Importer.CloseProcess(Self);
end;

procedure TdxLineBreakTag.ApplyTagProperties;
begin
end;

procedure TdxLineBreakTag.FunctionalTagProcess;
begin
  case Importer.Options.LineBreakSubstitute of
    TdxLineBreakSubstitute.Space:
      Importer.AppendText(TdxCharacters.Space);
    TdxLineBreakSubstitute.Paragraph:
      ParagraphFunctionalProcess;
    else
      Importer.AppendText(TdxCharacters.LineBreak);
  end;
  Importer.IsEmptyLine := True;
end;


{ TdxParagraphTagBase }

constructor TdxParagraphTagBase.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  DefaultSpacingInPoints := 12;
  FDefaultSpacing := AImporter.UnitConverter.PointsToModelUnits(DefaultSpacingInPoints);
  InitializeAlignment;
end;

destructor TdxParagraphTagBase.Destroy;
begin
  FreeAndNil(FAlignment);
  inherited Destroy;
end;

class constructor TdxParagraphTagBase.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxParagraphTagBase.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxParagraphTagBase.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('align'), ParagraphAlignmentKeyword);
end;

class procedure TdxParagraphTagBase.ParagraphAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string;
  ATag: TdxTagBase);
var
  AParagraphTag: TdxParagraphTagBase;
  AResultAlignment: TdxParagraphAlignment;
begin
  AParagraphTag := TdxParagraphTagBase(ATag);
  AResultAlignment := TdxParagraphAlignment.Left;
  if ReadParagraphAlignment(AValue, AResultAlignment) then
    AParagraphTag.Alignment.AlignmentValue := AResultAlignment;
end;

function TdxParagraphTagBase.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxParagraphTagBase.InitializeAlignment;
begin
  FAlignment := TdxHtmlParagraphAlignment.Create;
  if Importer.Position.DefaultAlignment.UseAlignment then
    FAlignment.AlignmentValue := Importer.Position.DefaultAlignment.AlignmentValue;
end;

procedure TdxParagraphTagBase.ApplyTagProperties;
begin
  if Alignment.UseAlignment then
    Importer.Position.ParagraphFormatting.Alignment := GetActualAlignmentValue;
  Importer.Position.ParagraphFormatting.OutlineLevel := 0;
end;

function TdxParagraphTagBase.GetActualAlignmentValue: TdxParagraphAlignment;
begin
  Result := Alignment.AlignmentValue;
end;

function TdxParagraphTagBase.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
  ACount, ADelta: Integer;
  APrevTag: TdxOpenHtmlTag;
  ALevelTag: TdxLevelTag;
begin
  AOptions := inherited ApplyCssProperties;
  ACount := Importer.TagsStack.Count;
  if ACount < 2 then
    Exit(AOptions);
  APrevTag := Importer.TagsStack[ACount - 2];
  ALevelTag := Safe<TdxLevelTag>.Cast(APrevTag.Tag);
  if ALevelTag <> nil then
  begin
    if AOptions.UseLeftIndent then
      ADelta := Importer.Position.ParagraphFormatting.LeftIndent
    else
      ADelta := 0;
    Importer.Position.ParagraphFormatting.LeftIndent :=
      Importer.TagsStack[ACount - 1].OldPosition.ParagraphFormatting.LeftIndent + ADelta;
  end;
  Result := AOptions;
end;

procedure TdxParagraphTagBase.FunctionalTagProcess;
begin
  if Id <> '' then
  begin
    Importer.ProcessBookmarkStart(Id);
    Importer.ProcessBookmarkEnd;
  end;
  ParagraphFunctionalProcess;
end;

{ TdxDivisionTag }

function TdxDivisionTag.GetActualAlignmentValue: TdxParagraphAlignment;
begin
  if Alignment.AlignmentValue = TdxParagraphAlignment.Justify then
    Result := Importer.Position.DefaultAlignment.AlignmentValue
  else
    Result := inherited GetActualAlignmentValue;
end;

function TdxDivisionTag.ApplyCssProperties: TdxParagraphFormattingOptions;
begin
  Result := inherited ApplyCssProperties;
end;

{ TdxHeadingTag }

constructor TdxHeadingTag.Create(AImporter: TdxCustomHtmlImporter; AFontSize: Integer; AOutlineLevel: Integer);
begin
  inherited Create(AImporter);
  FFontSize := AFontSize;
  FOutlineLevel := AOutlineLevel;
end;

procedure TdxHeadingTag.ApplyTagProperties;
var
  AParagraphFormatting: TdxParagraphFormattingBase;
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  AParagraphFormatting := Importer.Position.ParagraphFormatting;
  ACharacterFormatting := Importer.Position.CharacterFormatting;
  if Alignment.UseAlignment then
  begin
    Importer.Position.DefaultAlignment.AlignmentValue := Alignment.AlignmentValue;
    AParagraphFormatting.Alignment := Alignment.AlignmentValue;
  end;
  ACharacterFormatting.DoubleFontSize := Importer.HtmlFontSize.GetDoubleFontSize(FFontSize);
  ACharacterFormatting.FontBold := True;
  AParagraphFormatting.OutlineLevel := FOutlineLevel;
  ResetParagraphFormattingProperties(AParagraphFormatting);
end;

procedure TdxHeadingTag.ResetParagraphFormattingProperties(AParagraphFormatting: TdxParagraphFormattingBase);
begin
  if AParagraphFormatting.Options.UseSpacingAfter then
    AParagraphFormatting.SpacingAfter := DefaultSpacing;
  if AParagraphFormatting.Options.UseSpacingBefore then
    AParagraphFormatting.SpacingBefore := DefaultSpacing;
  if AParagraphFormatting.Options.UseLeftIndent then
    AParagraphFormatting.LeftIndent := 0;
end;

{ TdxMarqueeTag }

procedure TdxMarqueeTag.ApplyTagProperties;
begin
end;

{ TdxParagraphTag }

procedure TdxParagraphTag.ApplyTagProperties;
var
  AFormatting: TdxParagraphFormattingBase;
begin
  AFormatting := Importer.Position.ParagraphFormatting;
  if Alignment.UseAlignment then
    AFormatting.Alignment := Alignment.AlignmentValue;
  AFormatting.SpacingAfter := DefaultSpacing;
  AFormatting.SpacingBefore := DefaultSpacing;
end;

procedure TdxParagraphTag.FunctionalTagProcess;
begin
  if not Importer.IsEmptyParagraph then
    ApplyAutoSpacing(Importer.Position.ParagraphIndex);
  inherited FunctionalTagProcess;
end;

procedure TdxParagraphTag.OpenTagProcessCore;
begin
  Importer.LastOpenParagraphTagIndex := Importer.TagsStack.Count - 1;
end;

procedure TdxParagraphTag.ApplyAutoSpacing(AParagraphIndex: TdxParagraphIndex);
var
  AParagraphProperties: TdxParagraphProperties;
begin
  AParagraphProperties := Importer.PieceTable.Paragraphs[AParagraphIndex].ParagraphProperties;
  if not AParagraphProperties.UseSpacingAfter then
    AParagraphProperties.SpacingAfter := DefaultSpacing;
  if not AParagraphProperties.UseSpacingBefore then
    AParagraphProperties.SpacingBefore := DefaultSpacing;
end;

{ TdxPreformattedTag }

procedure TdxPreformattedTag.ApplyTagProperties;
var
  ATabs: TArray<TdxTabInfo>;
  AConverter: TdxDocumentModelUnitConverter;
begin
  if not Importer.Position.CharacterFormatting.Options.UseDoubleFontSize then
    Importer.Position.CharacterFormatting.DoubleFontSize := 20;
  Importer.Position.CharacterFormatting.FontName := 'Courier New';

  AConverter := Importer.UnitConverter;
  ATabs := TArray<TdxTabInfo>.Create(
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(916)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(1832)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(2748)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(3664)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(4580)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(5496)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(6412)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(7328)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(8244)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(9160)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(10076)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(11908)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(12824)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(13740)),
    TdxTabInfo.Create(AConverter.TwipsToModelUnits(14656)));
  Importer.Position.ParagraphTabs.AddRange(ATabs);
end;

procedure TdxPreformattedTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

procedure TdxPreformattedTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
begin
  inherited BeforeDeleteTagFromStack(AIndexOfDeletedTag);
  Importer.Position.ParagraphTabs.Clear;
end;

{ TdxSpanTag }

procedure TdxSpanTag.ApplyTagProperties;
begin
end;

{ TdxHrTag }

function TdxHrTag.GetCanAppendToTagStack: Boolean;
begin
  Result := False;
end;

function TdxHrTag.GetApplyStylesToInnerHtml: Boolean;
begin
  Result := False;
end;

procedure TdxHrTag.ApplyProperties;
begin
end;

function TdxHrTag.ApplyCssProperties: TdxParagraphFormattingOptions;
begin

  Result := TdxParagraphFormattingOptions.EmptyParagraphFormattingOption;
end;

procedure TdxHrTag.ApplyTagProperties;
begin
end;

procedure TdxHrTag.EmptyTagProcess;
begin
  Importer.CloseProcess(Self);
end;

end.
