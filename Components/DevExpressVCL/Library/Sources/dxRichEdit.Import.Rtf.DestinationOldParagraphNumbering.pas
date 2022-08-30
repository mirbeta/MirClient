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

unit dxRichEdit.Import.Rtf.DestinationOldParagraphNumbering;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, Math, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Import,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Utils.Types;

type
  { TdxTextAfterDestination }

  TdxTextAfterDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxTextBeforeDestination }

  TdxTextBeforeDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  TdxDestinationOldParagraphNumberingBase = class abstract(TdxRichEditRtfDestinationBase)
  private
    class procedure SetUnderlineType(AImporter: TdxRtfImporter; AUnderlineType: TdxUnderlineType); inline; static;

    class procedure AllCapsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CardinalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure CenterAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ContinuousUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DashDotDottedUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DashDottedUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DashedUndrelineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DecimalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DottedUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure DoubleUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FontBoldKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FontNumberKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure FontSizeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ForegroundColorKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HairlineUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure HangingIndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure IndentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ItalicKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LeftAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LowerCaseAlphabeticalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure LowerCaseRomanKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure NoneUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure NumberingInCircleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure OrdinalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure OrdinalTextKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure RestartOnSectionBreakKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure RightAlignmentKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SmallCapsKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SpaceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StartAtKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure StrikeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure TextAfterKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure TextBeforeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ThickUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure UpperCaseAlphabeticalKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure UpperCaseRomanKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure UsePrevKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure WaveUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure WordUnderlineKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    FTextAfter: string;
    FTextBefore: string;

    class procedure AppendParagraphNumberingDescKeywords(ATable: TdxKeywordTranslatorTable); static;
    class procedure SetNumberingListFormat(AImporter: TdxRtfImporter; AFormat: TdxNumberingFormat); inline; static;
  public
    procedure BeforePopRtfState; override;
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;
  end;

  { TdxDestinationOldParagraphNumbering }

  TdxDestinationOldParagraphNumbering = class(TdxDestinationOldParagraphNumberingBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FIsOldNumberingListCreated: Boolean;
    FListLevelIndex: Integer;
    FMultiLevelListIndex: TdxNumberingListIndex;
    FOldLevelNumber: Integer;
    FSimpleList: Boolean;
    FSimpleListIndex: TdxNumberingListIndex;
    FSkipNumbering: Boolean;
    function AreSameInfo(AExistingListLevelInfo: TdxRtfOldListLevelInfo; ARtfOldListLevelInfo: TdxRtfOldListLevelInfo): Boolean;
    procedure CreateBulletedListLevels;
    procedure CreateMultilevelListLevels;
    procedure CreateNewList;
    procedure CreateSimpleNumberingListLevels;
    procedure SetLegacyProperties(const ALevel: IdxListLevel; ALegacyIndent, ALegacySpace: Integer);

    class procedure BulletedParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ParagraphLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SimpleNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure SkipNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    FExplicitNumberingListIndex: Boolean;
    FExplicitListLevelIndex: Boolean;
    FNumberingListIndex: TdxNumberingListIndex;

    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;

    function IsMultilevelList: Boolean;
    function IsNewListLevelInfoPresent: Boolean;
    function IsSimpleList: Boolean;
    function IsSkipNumbering: Boolean;
    function SectionMultiLevelListCreated: Boolean;
    procedure SetDisplayFormatString(const ALevel: IdxListLevel; const ADisplayFormatString: string);
    procedure SetFirstLineIndent(const ALevel: IdxListLevel; ALineIndent: Integer);
    procedure SetTemplateCode(const ALevel: IdxListLevel; ATemplateCode: Integer);
    function ShouldCreateNewAbstractSimpleList(out AExistingNumberingListIndex: TdxNumberingListIndex): Boolean;
    function ShouldCreateNewList: Boolean;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
    procedure AfterPopRtfState; override;
    procedure BeforePopRtfState; override;

    property IsOldNumberingListCreated: Boolean read FIsOldNumberingListCreated write FIsOldNumberingListCreated;
    property ListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
    property MultiLevelListIndex: TdxNumberingListIndex read FMultiLevelListIndex write FMultiLevelListIndex;
    property OldLevelNumber: Integer read FOldLevelNumber write FOldLevelNumber;
		property SimpleList: Boolean read FSimpleList write FSimpleList;
    property SimpleListIndex: TdxNumberingListIndex read FSimpleListIndex write FSimpleListIndex;
    property SkipNumbering: Boolean read FSkipNumbering write FSkipNumbering;
  end;

  { TdxDestinationOldSectionNumberingLevel }

  TdxDestinationOldSectionNumberingLevel = class(TdxDestinationOldParagraphNumberingBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordTable: TdxKeywordTranslatorTable; static;
  strict private
    FLevelNumber: Integer;
  protected
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    function CreateRtfOldListLevelInfo: TdxRtfOldListLevelInfo; virtual;

    property LevelNumber: Integer read FLevelNumber;
  public
    constructor Create(AImporter: TdxRtfImporter; ALevelNumber: Integer); reintroduce;
    procedure BeforePopRtfState; override;
  end;

implementation

uses
  dxCoreGraphics,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Import.Rtf.ParagraphFormatting,
  dxRichEdit.Utils.Exceptions;

{ TdxTextAfterDestination }

function TdxTextAfterDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxTextAfterDestination.Create(Importer);
end;

{ TdxTextBeforeDestination }

function TdxTextBeforeDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxTextBeforeDestination.Create(Importer);
end;

{ TdxDestinationOldParagraphNumberingBase }

procedure TdxDestinationOldParagraphNumberingBase.BeforePopRtfState;
begin
  inherited BeforePopRtfState;
  Importer.Position.OldListLevelInfo.TextAfter := FTextAfter;
  Importer.Position.OldListLevelInfo.TextBefore := FTextBefore;
end;

procedure TdxDestinationOldParagraphNumberingBase.NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
begin
  inherited NestedGroupFinished(ADestination);
  if ADestination is TdxTextBeforeDestination then
    FTextBefore := TdxTextBeforeDestination(ADestination).Value
  else
    if ADestination is TdxTextAfterDestination then
      FTextAfter := TdxTextAfterDestination(ADestination).Value;
end;

class procedure TdxDestinationOldParagraphNumberingBase.AppendParagraphNumberingDescKeywords(
  ATable: TdxKeywordTranslatorTable);
begin
  ATable.Add('pncard', CardinalKeywordHandler);
  ATable.Add('pndec', DecimalKeywordHandler);
  ATable.Add('pnucltr', UpperCaseAlphabeticalKeywordHandler);
  ATable.Add('pnucrm', UpperCaseRomanKeywordHandler);
  ATable.Add('pnlcltr', LowerCaseAlphabeticalKeywordHandler);
  ATable.Add('pnlcrm', LowerCaseRomanKeywordHandler);
  ATable.Add('pnord', OrdinalKeywordHandler);
  ATable.Add('pnordt', OrdinalTextKeywordHandler);
  ATable.Add('pncnum', NumberingInCircleKeywordHandler);
  ATable.Add('pnuldash', DashedUndrelineKeywordHandler);
  ATable.Add('pnuldashd', DashDottedUnderlineKeywordHandler);
  ATable.Add('pnuldashdd', DashDotDottedUnderlineKeywordHandler);
  ATable.Add('pnulhair', HairlineUnderlineKeywordHandler);
  ATable.Add('pnulth', ThickUnderlineKeywordHandler);
  ATable.Add('pnulwave', WaveUnderlineKeywordHandler);
  ATable.Add('pnul', ContinuousUnderlineKeywordHandler);
  ATable.Add('pnuld', DottedUnderlineKeywordHandler);
  ATable.Add('pnuldb', DoubleUnderlineKeywordHandler);
  ATable.Add('pnulnone', NoneUnderlineKeywordHandler);
  ATable.Add('pnulw', WordUnderlineKeywordHandler);
  ATable.Add('pnf', FontNumberKeywordHandler);
  ATable.Add('pnfs', FontSizeKeywordHandler);
  ATable.Add('pnb', FontBoldKeywordHandler);
  ATable.Add('pni', ItalicKeywordHandler);
  ATable.Add('pncaps', AllCapsKeywordHandler);
  ATable.Add('pnscaps', SmallCapsKeywordHandler);
  ATable.Add('pnstrike', StrikeKeywordHandler);
  ATable.Add('pncf', ForegroundColorKeywordHandler);
  ATable.Add('pnindent', IndentKeywordHandler);
  ATable.Add('pnsp', SpaceKeywordHandler);
  ATable.Add('pnprev', UsePrevKeywordHandler);
  ATable.Add('pnstart', StartAtKeywordHandler);
  ATable.Add('pnhang', HangingIndentKeywordHandler);
  ATable.Add('pnrestart', RestartOnSectionBreakKeywordHandler);
  ATable.Add('pnqc', CenterAlignmentKeywordHandler);
  ATable.Add('pnql', LeftAlignmentKeywordHandler);
  ATable.Add('pnqr', RightAlignmentKeywordHandler);
  ATable.Add('pntxtb', TextBeforeKeywordHandler);
  ATable.Add('pntxta', TextAfterKeywordHandler);
end;

class procedure TdxDestinationOldParagraphNumberingBase.SetNumberingListFormat(AImporter: TdxRtfImporter; AFormat: TdxNumberingFormat);
begin
  AImporter.Position.OldListLevelInfo.ListLevelProperties.Format := AFormat;
end;

class procedure TdxDestinationOldParagraphNumberingBase.SetUnderlineType(AImporter: TdxRtfImporter; AUnderlineType: TdxUnderlineType);
begin
  AImporter.Position.CharacterFormatting.FontUnderlineType := AUnderlineType;
end;

class procedure TdxDestinationOldParagraphNumberingBase.CardinalKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.CardinalText);
end;

class procedure TdxDestinationOldParagraphNumberingBase.CenterAlignmentKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.ListLevelProperties.Alignment := TdxListNumberAlignment.Center;
end;

class procedure TdxDestinationOldParagraphNumberingBase.ContinuousUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.UnderlineWordsOnly := False;
end;

class procedure TdxDestinationOldParagraphNumberingBase.DashDotDottedUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.DashDotDotted);
end;

class procedure TdxDestinationOldParagraphNumberingBase.DashDottedUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.DashDotted);
end;

class procedure TdxDestinationOldParagraphNumberingBase.DashedUndrelineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.Dashed);
end;

class procedure TdxDestinationOldParagraphNumberingBase.DecimalKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.Decimal);
end;

class procedure TdxDestinationOldParagraphNumberingBase.DottedUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.Dotted);
end;

class procedure TdxDestinationOldParagraphNumberingBase.DoubleUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.Double);
end;

class procedure TdxDestinationOldParagraphNumberingBase.FontBoldKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.FontBold := not AHasParameter or
    (AParameterValue <> 0);
end;

class procedure TdxDestinationOldParagraphNumberingBase.FontNumberKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := AImporter.DocumentProperties.DefaultFontNumber;
  AImporter.SetFont(AImporter.DocumentProperties.Fonts.GetRtfFontInfoById(AParameterValue));
end;

class procedure TdxDestinationOldParagraphNumberingBase.FontSizeKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 24;
  AImporter.Position.CharacterFormatting.DoubleFontSize := Max(TdxPredefinedFontSizeCollection.MinFontSize, AParameterValue);
end;

class procedure TdxDestinationOldParagraphNumberingBase.ForegroundColorKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  AColor: TdxAlphaColor;
begin
  if not AHasParameter then
    AParameterValue := 0;
  AColor := AImporter.DocumentProperties.Colors[AParameterValue];
  AImporter.Position.CharacterFormatting.ForeColor := AColor;
end;

class procedure TdxDestinationOldParagraphNumberingBase.AllCapsKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.AllCaps := not AHasParameter or
    (AParameterValue > 0);
end;

class procedure TdxDestinationOldParagraphNumberingBase.HairlineUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.ThickSingle);
end;

class procedure TdxDestinationOldParagraphNumberingBase.HangingIndentKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
end;

class procedure TdxDestinationOldParagraphNumberingBase.IndentKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  AInfo: TdxRtfParagraphFormattingInfo;
begin
  AInfo := AImporter.Position.ParagraphFormattingInfo;
  if AParameterValue < 0 then
  begin
    AInfo.FirstLineIndent := AImporter.UnitConverter.TwipsToModelUnits(-AParameterValue);
    AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
  end
  else
  begin
    AInfo.FirstLineIndent := AImporter.UnitConverter.TwipsToModelUnits(AParameterValue);
    AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Indented;
  end;
end;

class procedure TdxDestinationOldParagraphNumberingBase.ItalicKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.FontItalic := not AHasParameter or
    (AParameterValue <> 0);
end;

class procedure TdxDestinationOldParagraphNumberingBase.LeftAlignmentKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.ListLevelProperties.Alignment := TdxListNumberAlignment.Left;
end;

class procedure TdxDestinationOldParagraphNumberingBase.LowerCaseAlphabeticalKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.LowerLetter);
end;

class procedure TdxDestinationOldParagraphNumberingBase.LowerCaseRomanKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.LowerRoman);
end;

class procedure TdxDestinationOldParagraphNumberingBase.NoneUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.None);
end;

class procedure TdxDestinationOldParagraphNumberingBase.NumberingInCircleKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.DecimalEnclosedCircle);
end;

class procedure TdxDestinationOldParagraphNumberingBase.OrdinalKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.Ordinal);
end;

class procedure TdxDestinationOldParagraphNumberingBase.OrdinalTextKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.OrdinalText);
end;

class procedure TdxDestinationOldParagraphNumberingBase.RestartOnSectionBreakKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxDestinationOldParagraphNumberingBase.RightAlignmentKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.ListLevelProperties.Alignment := TdxListNumberAlignment.Right;
end;

class procedure TdxDestinationOldParagraphNumberingBase.SmallCapsKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.AllCaps := not AHasParameter or
    (AParameterValue <> 0);
end;

class procedure TdxDestinationOldParagraphNumberingBase.SpaceKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxDestinationOldParagraphNumberingBase.StartAtKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.ListLevelProperties.Start := AParameterValue;
end;

class procedure TdxDestinationOldParagraphNumberingBase.StrikeKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
const
  AStrikeoutTypeMap: array[Boolean] of TdxStrikeoutType = (TdxStrikeoutType.Single, TdxStrikeoutType.None);
begin
  AImporter.Position.CharacterFormatting.FontStrikeoutType := AStrikeoutTypeMap[not AHasParameter or (AParameterValue <> 0)];
end;

class procedure TdxDestinationOldParagraphNumberingBase.TextAfterKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxTextAfterDestination.Create(AImporter);
end;

class procedure TdxDestinationOldParagraphNumberingBase.TextBeforeKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxTextBeforeDestination.Create(AImporter);
end;

class procedure TdxDestinationOldParagraphNumberingBase.ThickUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.ThickSingle);
end;

class procedure TdxDestinationOldParagraphNumberingBase.UpperCaseAlphabeticalKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.UpperLetter);
end;

class procedure TdxDestinationOldParagraphNumberingBase.UpperCaseRomanKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetNumberingListFormat(AImporter, TdxNumberingFormat.UpperRoman);
end;

class procedure TdxDestinationOldParagraphNumberingBase.UsePrevKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.IncludeInformationFromPreviousLevel := True;
end;

class procedure TdxDestinationOldParagraphNumberingBase.WaveUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  SetUnderlineType(AImporter, TdxUnderlineType.Wave);
end;

class procedure TdxDestinationOldParagraphNumberingBase.WordUnderlineKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Position.CharacterFormatting.UnderlineWordsOnly := True;
end;

{ TdxDestinationOldParagraphNumbering }

constructor TdxDestinationOldParagraphNumbering.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FOldLevelNumber := -1;
  if Importer.Position.OldListLevelInfo = nil then
    Importer.Position.OldListLevelInfo := TdxRtfOldListLevelInfo.Create(Importer.DocumentModel);
  if Importer.Position <> nil then
    Importer.Position.CharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  FMultiLevelListIndex := NumberingListIndexListIndexNotSetted;
  FSimpleListIndex := NumberingListIndexListIndexNotSetted;
end;

function TdxDestinationOldParagraphNumbering.AreSameInfo(AExistingListLevelInfo: TdxRtfOldListLevelInfo; ARtfOldListLevelInfo: TdxRtfOldListLevelInfo): Boolean;
begin
  Result := (AExistingListLevelInfo.TextAfter = ARtfOldListLevelInfo.TextAfter) and
    (AExistingListLevelInfo.TextBefore = ARtfOldListLevelInfo.TextBefore);
end;

procedure TdxDestinationOldParagraphNumbering.AfterPopRtfState;
var
  AActualNumberingListIndex: TdxNumberingListIndex;
begin
  inherited AfterPopRtfState;
  Importer.Position.ParagraphFormattingInfo.ListLevelIndex := ListLevelIndex;
  AActualNumberingListIndex := IfThen(IsSimpleList, SimpleListIndex, MultiLevelListIndex);
  Importer.Position.CurrentOldListSkipNumbering := SkipNumbering;
  if IsOldNumberingListCreated then
  begin
    Importer.Position.CurrentOldMultiLevelListIndex := MultiLevelListIndex;
    Importer.Position.CurrentOldSimpleListIndex := SimpleListIndex;
    Importer.Position.CurrentOldSimpleList := SimpleList;
  end;
  if FExplicitListLevelIndex and FExplicitNumberingListIndex then
  begin
    Importer.Position.ParagraphFormattingInfo.NumberingListIndex := FNumberingListIndex;
    Importer.Position.ParagraphFormattingInfo.ListLevelIndex := ListLevelIndex;
  end
  else
    Importer.Position.ParagraphFormattingInfo.NumberingListIndex := IfThen(not SkipNumbering, AActualNumberingListIndex, NumberingListIndexNoNumberingList);
end;

procedure TdxDestinationOldParagraphNumbering.BeforePopRtfState;
begin
  inherited BeforePopRtfState;
  if ShouldCreateNewList then
    CreateNewList
  else
  begin
    SkipNumbering := IsSkipNumbering;
    SimpleListIndex := Importer.Position.CurrentOldSimpleListIndex;
    MultiLevelListIndex := Importer.Position.CurrentOldMultiLevelListIndex;
  end;
  if FExplicitNumberingListIndex and FExplicitListLevelIndex then
  begin
    ListLevelIndex := Importer.Position.ParagraphFormattingInfo.ListLevelIndex;
    FNumberingListIndex := Importer.Position.ParagraphFormattingInfo.NumberingListIndex;
  end
  else
    ListLevelIndex := IfThen(OldLevelNumber >= 0, OldLevelNumber - 1, 0);
end;

function TdxDestinationOldParagraphNumbering.IsMultilevelList: Boolean;
begin
  Result := (OldLevelNumber >= 0) and not SimpleList;
end;

function TdxDestinationOldParagraphNumbering.IsNewListLevelInfoPresent: Boolean;
begin
  if FExplicitListLevelIndex and FExplicitNumberingListIndex then
    Exit(True);
  if IsSimpleList then
    Result := SimpleListIndex >= NumberingListIndexMinValue
  else
    Result := MultiLevelListIndex >= NumberingListIndexMinValue;
end;

function TdxDestinationOldParagraphNumbering.IsSimpleList: Boolean;
begin
  Result := SimpleList;
end;

function TdxDestinationOldParagraphNumbering.IsSkipNumbering: Boolean;
begin
  Result := Importer.Position.OldListLevelInfo.SkipNumbering;
end;

function TdxDestinationOldParagraphNumbering.SectionMultiLevelListCreated: Boolean;
begin
  Result := Importer.Position.CurrentOldMultiLevelListIndex >= NumberingListIndexMinValue;
end;

procedure TdxDestinationOldParagraphNumbering.SetDisplayFormatString(const ALevel: IdxListLevel; const ADisplayFormatString: string);
begin
  ALevel.ListLevelProperties.BeginInit;
  try
    ALevel.ListLevelProperties.DisplayFormatString := ADisplayFormatString;
  finally
    ALevel.ListLevelProperties.EndInit;
  end;
end;

procedure TdxDestinationOldParagraphNumbering.SetFirstLineIndent(const ALevel: IdxListLevel; ALineIndent: Integer);
begin
  ALevel.ParagraphProperties.BeginInit;
  try
    ALevel.ParagraphProperties.LeftIndent := ALineIndent;
  finally
    ALevel.ParagraphProperties.EndInit;
  end;
end;

procedure TdxDestinationOldParagraphNumbering.SetTemplateCode(const ALevel: IdxListLevel; ATemplateCode: Integer);
begin
  ALevel.ListLevelProperties.BeginInit;
  try
    ALevel.ListLevelProperties.TemplateCode := ATemplateCode;
  finally
    ALevel.ListLevelProperties.EndInit;
  end;
end;

function TdxDestinationOldParagraphNumbering.ShouldCreateNewAbstractSimpleList(out AExistingNumberingListIndex: TdxNumberingListIndex): Boolean;
var
  AOldListLevelInfo, APrevOldListLevelInfo: TdxRtfOldListLevelInfo;
  APrevParagraphIndex: TdxParagraphIndex;
  APrevParagraph: TdxSimpleParagraph;
  ANumberingListIndex: TdxNumberingListIndex;
begin
  if Importer.Position.CurrentOldSimpleList and (Importer.Position.CurrentOldSimpleListIndex >= NumberingListIndexMinValue) then
  begin
    AExistingNumberingListIndex := Importer.Position.CurrentOldSimpleListIndex;
    if not Importer.NumberingListToOldListLevelInfoMap.TryGetValue(AExistingNumberingListIndex, AOldListLevelInfo) then
      Result := True
    else
      Result := not AreSameInfo(AOldListLevelInfo, Importer.Position.OldListLevelInfo);
    Exit;
  end;
  AExistingNumberingListIndex := NumberingListIndexListIndexNotSetted;
  APrevParagraphIndex := Importer.Position.ParagraphIndex - 1;
  while APrevParagraphIndex >= 0 do
  begin
    APrevParagraph := Importer.Position.PieceTable.Paragraphs[APrevParagraphIndex];
    if not APrevParagraph.IsInList then
    begin
      if APrevParagraph.NumberingListIndex = NumberingListIndexNoNumberingList then
      begin
        Dec(APrevParagraphIndex);
        Continue;
      end;
      Exit(True);
    end;
    ANumberingListIndex := APrevParagraph.NumberingListIndex;
    if not Importer.NumberingListToOldListLevelInfoMap.TryGetValue(ANumberingListIndex, APrevOldListLevelInfo) then
      Exit(True);
    if not AreSameInfo(APrevOldListLevelInfo, Importer.Position.OldListLevelInfo) then
      Exit(True);
    AExistingNumberingListIndex := ANumberingListIndex;
    Exit(False);
    Dec(APrevParagraphIndex);
  end;
  Result := True;
end;

function TdxDestinationOldParagraphNumbering.ShouldCreateNewList: Boolean;
begin
  Result := not IsNewListLevelInfoPresent;
  if Result then
  begin
    if IsMultilevelList then
      Result := not SectionMultiLevelListCreated
    else
      Result := IsSimpleList or not IsSkipNumbering;
  end;
end;

procedure TdxDestinationOldParagraphNumbering.CreateNewList;
var
  AOldListLevelInfo: TdxRtfOldListLevelInfo;
begin
  AOldListLevelInfo := Importer.Position.OldListLevelInfo;
  if SimpleList then
  begin
    CreateSimpleNumberingListLevels;
    SimpleListIndex := Importer.DocumentModel.NumberingLists.Count - 1;
    MultiLevelListIndex := Importer.Position.CurrentOldMultiLevelListIndex;
  end
  else
    if AOldListLevelInfo.ListLevelProperties.Format = TdxNumberingFormat.Bullet then
    begin
      CreateBulletedListLevels;
      MultiLevelListIndex := Importer.DocumentModel.NumberingLists.Count - 1;
      SimpleListIndex := Importer.Position.CurrentOldSimpleListIndex;
    end
    else
    begin
      CreateMultilevelListLevels;
      MultiLevelListIndex := Importer.DocumentModel.NumberingLists.Count - 1;
      SimpleListIndex := Importer.Position.CurrentOldSimpleListIndex;
    end;
end;

procedure TdxDestinationOldParagraphNumbering.CreateBulletedListLevels;
var
  ADocumentModel: TdxDocumentModel;
  ALevelOffset, I, AFirstLineIndent: Integer;
  AAbstractNumberingList: TdxAbstractNumberingList;
  ALevel: TdxListLevel;
begin
  ADocumentModel := Importer.DocumentModel;
  ALevelOffset := ADocumentModel.UnitConverter.DocumentsToModelUnits(150);
  AAbstractNumberingList := TdxAbstractNumberingList.Create(ADocumentModel);

  ADocumentModel.AddAbstractNumberingListUsingHistory(AAbstractNumberingList);
  for I := 0 to AAbstractNumberingList.Levels.Count - 1 do
  begin
    ALevel := TdxListLevel.Create(ADocumentModel);
    AAbstractNumberingList.Levels[I] := ALevel;
    ALevel.CharacterProperties.BeginInit;
    ALevel.CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);
    ALevel.CharacterProperties.EndInit;
    AFirstLineIndent := ALevelOffset * I + Importer.Position.ParagraphFormattingInfo.FirstLineIndent;
    SetFirstLineIndent(ALevel, AFirstLineIndent);
    SetDisplayFormatString(ALevel, Format('%0:s%1:s', [Importer.Position.OldListLevelInfo.TextBefore, Importer.Position.OldListLevelInfo.TextAfter]));
    SetTemplateCode(ALevel, TdxNumberingListHelper.GenerateNewTemplateCode(ADocumentModel));
    SetLegacyProperties(ALevel, 0, 0);
  end;
  FIsOldNumberingListCreated := True;
  ADocumentModel.AddNumberingListUsingHistory(TdxNumberingList.Create(ADocumentModel, ADocumentModel.AbstractNumberingLists.Count - 1));
end;

function TdxDestinationOldParagraphNumbering.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxDestinationOldParagraphNumbering.Create(Importer);
end;

class function TdxDestinationOldParagraphNumbering.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxDestinationOldParagraphNumbering.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxDestinationOldParagraphNumbering.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxDestinationOldParagraphNumbering.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AppendParagraphNumberingDescKeywords(Result);
  Result.Add('pnlvl', ParagraphLevelKeywordHandler);
  Result.Add('pnlvlblt', BulletedParagraphKeywordHandler);
  Result.Add('pnlvlbody', SimpleNumberingKeywordHandler);
  Result.Add('pnlvlcont', SkipNumberingKeywordHandler);
  Result.Add('ilvl', ListLevelKeywordHandler);
  Result.Add('ls', ListOverrideKeywordHandler);
end;

procedure TdxDestinationOldParagraphNumbering.CreateMultilevelListLevels;
var
  ADocumentModel: TdxDocumentModel;
  AAbstractNumberingList: TdxAbstractNumberingList;
  I, AFirstLineIndent: Integer;
  ALevelInfo: TdxRtfOldListLevelInfo;
  ALevel: TdxListLevel;
  AFormatString: string;
begin
  ADocumentModel := Importer.DocumentModel;
  AAbstractNumberingList := TdxAbstractNumberingList.Create(ADocumentModel);
  ADocumentModel.AddAbstractNumberingListUsingHistory(AAbstractNumberingList);
  for I := 0 to AAbstractNumberingList.Levels.Count - 1 do
  begin
    ALevelInfo := Importer.Position.OldListLevelInfoCollection[I + 1];
    ALevel := TdxListLevel.Create(ADocumentModel);
    AAbstractNumberingList.Levels[I] := ALevel;
    AFirstLineIndent := ALevelInfo.Indent;
    SetFirstLineIndent(AAbstractNumberingList.Levels[I], AFirstLineIndent);
    ALevel.CharacterProperties.BeginInit;
    ALevel.CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);
    ALevel.CharacterProperties.EndInit;

    AFormatString := Format('%0:s%%%1:d:s%2:s', [ALevelInfo.TextBefore, I, ALevelInfo.TextAfter]);
    if (I > 0) and ALevelInfo.IncludeInformationFromPreviousLevel then
      AFormatString := AAbstractNumberingList.Levels[I - 1].ListLevelProperties.DisplayFormatString + AFormatString;
    SetDisplayFormatString(ALevel, AFormatString);
    SetLegacyProperties(ALevel, 0, 0);
  end;
  FIsOldNumberingListCreated := True;
  ADocumentModel.AddNumberingListUsingHistory(TdxNumberingList.Create(ADocumentModel, ADocumentModel.AbstractNumberingLists.Count - 1));
end;

procedure TdxDestinationOldParagraphNumbering.CreateSimpleNumberingListLevels;
var
  ADocumentModel: TdxDocumentModel;
  AExistingNumberingListIndex: TdxNumberingListIndex;
  AAbstractNumberingList: TdxAbstractNumberingList;
  I, AFirstLineIndent, AStart: Integer;
  ALevel: TdxListLevel;
  ANumberingFormat: TdxNumberingFormat;
  AAbstractNumberingListIndex: TdxAbstractNumberingListIndex;
  APrevNumberingList, ANewList: TdxNumberingList;
  AOverrideListLevel: TdxOverrideListLevel;
begin
  ADocumentModel := Importer.DocumentModel;
  if ShouldCreateNewAbstractSimpleList(AExistingNumberingListIndex) then
  begin
    AAbstractNumberingList := TdxAbstractNumberingList.Create(ADocumentModel);

    ADocumentModel.AddAbstractNumberingListUsingHistory(AAbstractNumberingList);
    for I := 0 to AAbstractNumberingList.Levels.Count - 1 do
    begin
      ALevel := TdxListLevel.Create(ADocumentModel);
      AAbstractNumberingList.Levels[I] := ALevel;
      ALevel.CharacterProperties.BeginInit;
      ALevel.CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);
      ALevel.CharacterProperties.EndInit;
      AFirstLineIndent := Importer.Position.ParagraphFormattingInfo.FirstLineIndent + 150 * I;
      SetFirstLineIndent(ALevel, AFirstLineIndent);
      SetDisplayFormatString(ALevel, Format('%0:s%%%1:d:s%2:s', [Importer.Position.OldListLevelInfo.TextBefore, I, Importer.Position.OldListLevelInfo.TextAfter]));
      SetTemplateCode(ALevel, TdxNumberingListHelper.GenerateNewTemplateCode(ADocumentModel));
      SetLegacyProperties(ALevel, 0, 0);
      AStart := Importer.Position.OldListLevelInfo.ListLevelProperties.Start;
      if AStart > 0 then
        ALevel.ListLevelProperties.Start := AStart;
      ANumberingFormat := Importer.Position.OldListLevelInfo.ListLevelProperties.Format;
      if ANumberingFormat <> TdxNumberingFormat.Decimal then
        ALevel.ListLevelProperties.Format := ANumberingFormat;
    end;
    FIsOldNumberingListCreated := True;
    ADocumentModel.AddNumberingListUsingHistory(TdxNumberingList.Create(ADocumentModel, ADocumentModel.AbstractNumberingLists.Count - 1));
    Importer.NumberingListToOldListLevelInfoMap.Add(ADocumentModel.NumberingLists.Count - 1, Importer.Position.OldListLevelInfo.Clone);
  end
  else
  begin
    AAbstractNumberingListIndex := ADocumentModel.NumberingLists[AExistingNumberingListIndex].AbstractNumberingListIndex;
    APrevNumberingList := ADocumentModel.NumberingLists.Last;

    ANewList := TdxNumberingList.Create(ADocumentModel, AAbstractNumberingListIndex);
    AOverrideListLevel := TdxOverrideListLevel.Create(ADocumentModel);

    AStart := Importer.Position.OldListLevelInfo.ListLevelProperties.Start;
    if AStart >= 0 then
      AOverrideListLevel.ListLevelProperties.Start := AStart;
    ANumberingFormat := Importer.Position.OldListLevelInfo.ListLevelProperties.Format;
    if ANumberingFormat <> TdxNumberingFormat.Decimal then
      AOverrideListLevel.ListLevelProperties.Format := ANumberingFormat;
    if (APrevNumberingList <> nil) and (APrevNumberingList.Levels[0].ListLevelProperties.Format <> AOverrideListLevel.ListLevelProperties.Format) then
      AOverrideListLevel.SetOverrideStart(True);
    AOverrideListLevel.CharacterProperties.BeginInit;
    AOverrideListLevel.CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);
    AOverrideListLevel.CharacterProperties.EndInit;
    AFirstLineIndent := Importer.Position.ParagraphFormattingInfo.FirstLineIndent;
    SetFirstLineIndent(AOverrideListLevel, AFirstLineIndent);
    SetDisplayFormatString(AOverrideListLevel, Format('%0:s%%%1:d:s%2:s', [Importer.Position.OldListLevelInfo.TextBefore, 0, Importer.Position.OldListLevelInfo.TextAfter]));
    SetTemplateCode(AOverrideListLevel, TdxNumberingListHelper.GenerateNewTemplateCode(ADocumentModel));
    SetLegacyProperties(AOverrideListLevel, 0, 0);
    ANewList.Levels[0] := AOverrideListLevel;
    ADocumentModel.AddNumberingListUsingHistory(ANewList);
    Importer.NumberingListToOldListLevelInfoMap.Add(ADocumentModel.NumberingLists.Count - 1, Importer.Position.OldListLevelInfo.Clone);
  end;
end;

procedure TdxDestinationOldParagraphNumbering.SetLegacyProperties(const ALevel: IdxListLevel; ALegacyIndent, ALegacySpace: Integer);
begin
  ALevel.ListLevelProperties.Legacy := True;
  ALevel.ListLevelProperties.LegacySpace := ALegacyIndent;
  ALevel.ListLevelProperties.LegacyIndent := ALegacySpace;
end;

class procedure TdxDestinationOldParagraphNumbering.BulletedParagraphKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.ListLevelProperties.Format := TdxNumberingFormat.Bullet;
end;

class procedure TdxDestinationOldParagraphNumbering.ListLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.ParagraphFormattingInfo.ListLevelIndex := AParameterValue;
  TdxDestinationOldParagraphNumbering(AImporter.Destination).FExplicitListLevelIndex := True;
end;

class procedure TdxDestinationOldParagraphNumbering.ListOverrideKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AIndex: TdxNumberingListIndex;
begin
  if AImporter.ListOverrideIndexToNumberingListIndexMap.TryGetValue(AParameterValue, AIndex) then
    AImporter.Position.ParagraphFormattingInfo.NumberingListIndex := AIndex;
  (TdxDestinationOldParagraphNumbering(AImporter.Destination)).FExplicitNumberingListIndex := True;
end;

class procedure TdxDestinationOldParagraphNumbering.SimpleNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  TdxDestinationOldParagraphNumbering(AImporter.Destination).SimpleList := True;
end;

class procedure TdxDestinationOldParagraphNumbering.ParagraphLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter and (AParameterValue = 10) then
  begin
    SimpleNumberingKeywordHandler(AImporter, 0, False);
    Exit;
  end;
  TdxDestinationOldParagraphNumbering(AImporter.Destination).OldLevelNumber := AParameterValue;
  TdxDestinationOldParagraphNumbering(AImporter.Destination).SimpleList := False;
  AImporter.Position.OldListLevelInfo.CopyFrom(AImporter.Position.OldListLevelInfoCollection[AParameterValue]);
end;

class procedure TdxDestinationOldParagraphNumbering.SkipNumberingKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.OldListLevelInfo.SkipNumbering := True;
end;

{ TdxDestinationOldSectionNumberingLevel }

class constructor TdxDestinationOldSectionNumberingLevel.Initialize;
begin
  FKeywordHT := CreateKeywordTable;
end;

class destructor TdxDestinationOldSectionNumberingLevel.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

constructor TdxDestinationOldSectionNumberingLevel.Create(AImporter: TdxRtfImporter; ALevelNumber: Integer);
begin
  inherited Create(AImporter);
  AImporter.Position.OldListLevelInfo := TdxRtfOldListLevelInfo.Create(AImporter.DocumentModel);
  FLevelNumber := ALevelNumber;
end;

class function TdxDestinationOldSectionNumberingLevel.CreateKeywordTable: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AppendParagraphNumberingDescKeywords(Result);
end;

class function TdxDestinationOldSectionNumberingLevel.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

function TdxDestinationOldSectionNumberingLevel.CreateClone: TdxRichEditRtfDestinationBase;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxDestinationOldSectionNumberingLevel.Create(Importer, LevelNumber);
end;

procedure TdxDestinationOldSectionNumberingLevel.BeforePopRtfState;
var
  AInfo: TdxRtfOldListLevelInfo;
begin
  inherited BeforePopRtfState;
  AInfo := CreateRtfOldListLevelInfo;
  try
    Importer.Position.OldListLevelInfoCollection[LevelNumber].CopyFrom(AInfo);
  finally
    AInfo.Free;
  end;
end;

function TdxDestinationOldSectionNumberingLevel.CreateRtfOldListLevelInfo: TdxRtfOldListLevelInfo;
begin
  Result := TdxRtfOldListLevelInfo.Create(Importer.DocumentModel);
  Result.CopyFrom(Importer.Position.OldListLevelInfo);
  Result.CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);
  Result.Indent := Importer.Position.ParagraphFormattingInfo.FirstLineIndent;
end;

end.
