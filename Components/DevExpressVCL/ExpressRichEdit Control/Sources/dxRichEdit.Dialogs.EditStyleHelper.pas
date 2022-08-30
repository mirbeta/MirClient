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

unit dxRichEdit.Dialogs.EditStyleHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses

  dxCoreGraphics, dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Types;

type
  TdxEditStyleHelper = class abstract
  public
    class procedure ChangeConditionalType(ATable: TdxTable; AStyleType: TdxConditionalTableStyleFormattingTypes); static;
    class function IsFontSizeValid(AEditValue: Variant; out AText: string; out AValue: Integer): Boolean; overload;
    class function IsFontSizeValid(AEditValue: Variant; out AText: string): Boolean; overload;
    class function IsValidFontSize(AFontSize: Integer): Boolean;
    class function IsValidStyleName(const AName: string; ASourceStyle: TdxStyleBase; ACollection: TdxStyleCollectionBase): Boolean; static;
  end;

  { TdxCharacterPropertyAccessor }

  TdxCharacterPropertyAccessor = class
  private
    FCharacterProperties: TdxCharacterProperties;
    FParentCharacterProperties: TdxCharacterFormattingInfo;
  public
    constructor Create(ACharacterProperties: TdxCharacterProperties; AParentCharacterProperties: TdxCharacterFormattingInfo);
    destructor Destroy; override;

    procedure ChangeFontBold(const AValue: Boolean);
    procedure ChangeFontItalic(const AValue: Boolean);
    procedure ChangeFontName(const AValue: string);
    procedure ChangeForeColor(const AValue: TdxAlphaColor);
    procedure ChangeDoubleFontSize(const AValue: Integer);
    procedure ChangeFontUnderlineType(const AValue: TdxUnderlineType);
    procedure ChangeUnderlineColor(const AValue: TdxAlphaColor);
    procedure ChangeFontStrikeoutType(const AValue: TdxStrikeoutType);
    procedure ChangeUnderlineWordsOnly(const AValue: Boolean);
    procedure ChangeScript(const AValue: TdxCharacterFormattingScript);
    procedure ChangeAllCaps(const AValue: Boolean);
    procedure ChangeHidden(const AValue: Boolean);

    property CharacterProperties: TdxCharacterProperties read FCharacterProperties;
    property ParentCharacterProperties: TdxCharacterFormattingInfo read FParentCharacterProperties;
  end;

  { TdxParagraphPropertyAccessor }

  TdxParagraphPropertyAccessor = class
  private
    FParagraphProperties: TdxParagraphProperties;
    FParentParagraphProperties: TdxParagraphFormattingInfo;
    FParentParagraphOptions: TdxNullableValue<TdxParagraphFormattingOptions>;
  protected
    function GetOptionsUseValue(AUseOption: TdxUsedParagraphFormattingOption): Boolean;
  public
    constructor Create(AParagraphProperties: TdxParagraphProperties; AParentParagraphProperties: TdxParagraphFormattingInfo); overload;
    constructor Create(AParagraphProperties: TdxParagraphProperties; AParentParagraphProperties: TdxParagraphFormattingInfo;
      const AParentParagraphOptions: TdxParagraphFormattingOptions); overload;
    destructor Destroy; override;

    procedure ChangeAlignment(const AValue: TdxParagraphAlignment);
    procedure ChangeOutlineLevel(const AValue: Integer);
    procedure ChangeLeftIndent(const AValue: Integer);
    procedure ChangeRightIndent(const AValue: Integer);
    procedure ChangeFirstLineIndent(const AValue: Integer);
    procedure ChangeSpacingBefore(const AValue: Integer);
    procedure ChangeSpacingAfter(const AValue: Integer);
    procedure ChangeLineSpacing(const AValue: Single);
    procedure ChangeBeforeAutoSpacing(const AValue: Boolean);
    procedure ChangeAfterAutoSpacing(const AValue: Boolean);
    procedure ChangeContextualSpacing(const AValue: Boolean);
    procedure ChangeKeepLinesTogether(const AValue: Boolean);
    procedure ChangeKeepWithNext(const AValue: Boolean);
    procedure ChangePageBreakBefore(const AValue: Boolean);
    procedure ChangeSuppressLineNumbers(const AValue: Boolean);
    procedure ChangeSuppressHyphenation(const AValue: Boolean);
    procedure ChangeWidowOrphanControl(const AValue: Boolean);
    procedure ChangeBackColor(const AValue: TdxAlphaColor);

    property ParagraphProperties: TdxParagraphProperties read FParagraphProperties;
    property ParentParagraphProperties: TdxParagraphFormattingInfo read FParentParagraphProperties;
  end;

  { TdxTableCellPropertyAccessor }

  TdxTableCellPropertyAccessor = class
  private
    FParentTableCellOptions: TdxTableCellPropertiesOptions;
    FTableCellProperties: TdxTableCellProperties;
    FParentCellProperties: TdxTableCellGeneralSettingsInfo;
  public
    constructor Create(ATableCellProperties: TdxTableCellProperties;
      AParentCellProperties: TdxTableCellGeneralSettingsInfo; AParentTableCellOptions: TdxTableCellPropertiesOptions);
    destructor Destroy; override;

    procedure ChangeBackgroundColor(const AValue: TdxAlphaColor);
    procedure ChangeVerticalAlignment(const AValue: TdxVerticalAlignment);

    property ParentTableCellOptions: TdxTableCellPropertiesOptions read FParentTableCellOptions;
    property TableCellProperties: TdxTableCellProperties read FTableCellProperties;
    property ParentCellProperties: TdxTableCellGeneralSettingsInfo read FParentCellProperties;
  end;

implementation

uses
  SysUtils, dxCore,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.FontsHelper,
  dxRichEdit.Utils.PredefinedFontSizeCollection;

{ TdxEditStyleHelper }

class function TdxEditStyleHelper.IsFontSizeValid(AEditValue: Variant; out AText: string; out AValue: Integer): Boolean;
var
  AIsIntValue: Boolean;
begin
  AText := '';
  AIsIntValue := TdxRichEditFontSizeHelper.TryGetHalfSizeValue(AEditValue, AValue);
  Result := True;
  if AIsIntValue then
  begin
    if not IsValidFontSize(AValue) then
    begin
      AText := Format(cxGetResourceString(@sdxRichEditInvalidSize),
        [TdxPredefinedFontSizeCollection.MinFontSize, TdxPredefinedFontSizeCollection.MaxFontSize]);
      Result := False;
    end;
  end
  else
  begin
    AText := cxGetResourceString(@sdxRichEditInvalidNumber);
    Result := False;
  end;
end;

class function TdxEditStyleHelper.IsFontSizeValid(AEditValue: Variant; out AText: string): Boolean;
var
  AValue: Integer;
begin
  Result := IsFontSizeValid(AEditValue, AText, AValue);
end;

class function TdxEditStyleHelper.IsValidFontSize(AFontSize: Integer): Boolean;
begin
  Result := (AFontSize >= TdxPredefinedFontSizeCollection.MinFontSize) and
    (AFontSize <= TdxPredefinedFontSizeCollection.MaxFontSize * 2);
end;

class function TdxEditStyleHelper.IsValidStyleName(const AName: string; ASourceStyle: TdxStyleBase;
  ACollection: TdxStyleCollectionBase): Boolean;
var
  AStyle: TdxStyleBase;
  I: Integer;
begin
  if not ASourceStyle.Deleted and (ASourceStyle.StyleName <> AName) then
  begin
    for I := 0 to ACollection.Count - 1 do
    begin
      AStyle := ACollection.Items[I];
      if AStyle.StyleName = AName then
        Exit(False);
    end;
  end;
  Result := True;
end;

class procedure TdxEditStyleHelper.ChangeConditionalType(ATable: TdxTable; AStyleType: TdxConditionalTableStyleFormattingTypes);
var
  ATableLook: TdxTableLookTypes;
begin
  ATableLook := ATable.TableLook;
  if TdxConditionalTableStyleFormattingType.FirstRow in AStyleType then
    Include(ATableLook, TdxTableLookType.ApplyFirstRow);

  if TdxConditionalTableStyleFormattingType.LastRow in AStyleType then
    Include(ATableLook, TdxTableLookType.ApplyLastRow);

  if (TdxConditionalTableStyleFormattingType.EvenRowBanding in AStyleType) or
    (TdxConditionalTableStyleFormattingType.OddRowBanding in AStyleType) then
    Include(ATableLook, TdxTableLookType.DoNotApplyRowBanding);

  if TdxConditionalTableStyleFormattingType.FirstColumn in AStyleType then
    Include(ATableLook, TdxTableLookType.ApplyFirstColumn);

  if TdxConditionalTableStyleFormattingType.LastColumn in AStyleType then
    Include(ATableLook, TdxTableLookType.ApplyLastColumn);

  if TdxConditionalTableStyleFormattingType.TopLeftCell in AStyleType then
    ATableLook := ATableLook + [TdxTableLookType.ApplyFirstColumn, TdxTableLookType.ApplyFirstRow];

  if TdxConditionalTableStyleFormattingType.TopRightCell in AStyleType then
    ATableLook := ATableLook + [TdxTableLookType.ApplyLastColumn, TdxTableLookType.ApplyFirstRow];

  if TdxConditionalTableStyleFormattingType.BottomLeftCell in AStyleType then
    ATableLook := ATableLook + [TdxTableLookType.ApplyFirstColumn, TdxTableLookType.ApplyLastRow];

  if TdxConditionalTableStyleFormattingType.BottomRightCell in AStyleType then
    ATableLook := ATableLook + [TdxTableLookType.ApplyLastColumn, TdxTableLookType.ApplyLastRow];

  ATable.TableLook := ATableLook;
end;

{ TdxCharacterPropertyAccessor }

constructor TdxCharacterPropertyAccessor.Create(ACharacterProperties: TdxCharacterProperties;
  AParentCharacterProperties: TdxCharacterFormattingInfo);
begin
  inherited Create;
  FCharacterProperties := ACharacterProperties;
  FParentCharacterProperties := AParentCharacterProperties;
end;

destructor TdxCharacterPropertyAccessor.Destroy;
begin
  FreeAndNil(FParentCharacterProperties);
  inherited Destroy;
end;

procedure TdxCharacterPropertyAccessor.ChangeAllCaps(const AValue: Boolean);
begin
  CharacterProperties.AllCaps := TdxDocumentModel(CharacterProperties.DocumentModel).DefaultCharacterProperties.AllCaps;
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseAllCaps);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.AllCaps) then
    CharacterProperties.AllCaps := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeDoubleFontSize(const AValue: Integer);
begin
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseDoubleFontSize);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.DoubleFontSize) then
    CharacterProperties.DoubleFontSize := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeFontBold(const AValue: Boolean);
begin
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontBold);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.FontBold) then
    CharacterProperties.FontBold := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeForeColor(const AValue: TdxAlphaColor);
begin
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseForeColor);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.ForeColor) then
    CharacterProperties.ForeColor := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeHidden(const AValue: Boolean);
begin
  CharacterProperties.Hidden := TdxDocumentModel(CharacterProperties.DocumentModel).DefaultCharacterProperties.Hidden;
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseHidden);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.Hidden) then
    CharacterProperties.Hidden := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeScript(const AValue: TdxCharacterFormattingScript);
begin
  CharacterProperties.Script := TdxDocumentModel(CharacterProperties.DocumentModel).DefaultCharacterProperties.Script;
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseScript);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.Script) then
    CharacterProperties.Script := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeUnderlineColor(const AValue: TdxAlphaColor);
begin
  CharacterProperties.UnderlineColor := TdxDocumentModel(CharacterProperties.DocumentModel).DefaultCharacterProperties.UnderlineColor;
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseUnderlineColor);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.UnderlineColor) then
    CharacterProperties.UnderlineColor := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeUnderlineWordsOnly(const AValue: Boolean);
begin
  CharacterProperties.UnderlineWordsOnly := TdxDocumentModel(CharacterProperties.DocumentModel).DefaultCharacterProperties.UnderlineWordsOnly;
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseUnderlineWordsOnly);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.UnderlineWordsOnly) then
    CharacterProperties.UnderlineWordsOnly := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeFontItalic(const AValue: Boolean);
begin
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontItalic);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.FontItalic) then
    CharacterProperties.FontItalic := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeFontName(const AValue: string);
begin
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontName);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.FontName) then
    CharacterProperties.FontName := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeFontStrikeoutType(const AValue: TdxStrikeoutType);
begin
  CharacterProperties.FontStrikeoutType := TdxDocumentModel(CharacterProperties.DocumentModel).DefaultCharacterProperties.FontStrikeoutType;
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontStrikeoutType);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.FontStrikeoutType) then
    CharacterProperties.FontStrikeoutType := AValue;
end;

procedure TdxCharacterPropertyAccessor.ChangeFontUnderlineType(const AValue: TdxUnderlineType);
begin
  CharacterProperties.ResetUse(TdxUsedCharacterFormattingOption.UseFontUnderlineType);
  if (ParentCharacterProperties = nil) or (AValue <> ParentCharacterProperties.FontUnderlineType) then
    CharacterProperties.FontUnderlineType := AValue;
end;

{ TdxParagraphPropertyAccessor }

procedure TdxParagraphPropertyAccessor.ChangeAfterAutoSpacing(const AValue: Boolean);
begin
  ParagraphProperties.AfterAutoSpacing := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.AfterAutoSpacing;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseAfterAutoSpacing]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.AfterAutoSpacing) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseAfterAutoSpacing) then
    ParagraphProperties.AfterAutoSpacing := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeAlignment(const AValue: TdxParagraphAlignment);
begin
  ParagraphProperties.Alignment := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.Alignment;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseAlignment]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.Alignment) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseAlignment) then
    ParagraphProperties.Alignment := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeBackColor(const AValue: TdxAlphaColor);
begin
  ParagraphProperties.BackColor := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.BackColor;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseBackColor]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.BackColor) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseBackColor) then
    ParagraphProperties.BackColor := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeBeforeAutoSpacing(const AValue: Boolean);
begin
  ParagraphProperties.BeforeAutoSpacing := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.BeforeAutoSpacing;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseBeforeAutoSpacing]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.BeforeAutoSpacing) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseBeforeAutoSpacing) then
    ParagraphProperties.BeforeAutoSpacing := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeContextualSpacing(const AValue: Boolean);
begin
  ParagraphProperties.ContextualSpacing := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.ContextualSpacing;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseContextualSpacing]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.ContextualSpacing) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseContextualSpacing) then
    ParagraphProperties.ContextualSpacing := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeFirstLineIndent(const AValue: Integer);
begin
  ParagraphProperties.FirstLineIndent := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.FirstLineIndent;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseFirstLineIndent]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.FirstLineIndent) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseFirstLineIndent) then
    ParagraphProperties.FirstLineIndent := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeKeepLinesTogether(const AValue: Boolean);
begin
  ParagraphProperties.KeepLinesTogether := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.KeepLinesTogether;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseKeepLinesTogether]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.KeepLinesTogether) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseKeepLinesTogether) then
    ParagraphProperties.KeepLinesTogether := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeKeepWithNext(const AValue: Boolean);
begin
  ParagraphProperties.KeepWithNext := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.KeepWithNext;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseKeepWithNext]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.KeepWithNext) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseKeepWithNext) then
    ParagraphProperties.KeepWithNext := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeLeftIndent(const AValue: Integer);
begin
  ParagraphProperties.LeftIndent := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.LeftIndent;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseLeftIndent]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.LeftIndent) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseLeftIndent) then
    ParagraphProperties.LeftIndent := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeLineSpacing(const AValue: Single);
begin
  ParagraphProperties.LineSpacing := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.LineSpacing;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseLineSpacing]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.LineSpacing) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseLineSpacing) then
    ParagraphProperties.LineSpacing := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeOutlineLevel(const AValue: Integer);
begin
  ParagraphProperties.OutlineLevel := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.OutlineLevel;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseOutlineLevel]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.OutlineLevel) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseOutlineLevel) then
    ParagraphProperties.OutlineLevel := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangePageBreakBefore(const AValue: Boolean);
begin
  ParagraphProperties.PageBreakBefore := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.PageBreakBefore;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UsePageBreakBefore]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.PageBreakBefore) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UsePageBreakBefore) then
    ParagraphProperties.PageBreakBefore := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeRightIndent(const AValue: Integer);
begin
  ParagraphProperties.RightIndent := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.RightIndent;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseRightIndent]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.RightIndent) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseRightIndent) then
    ParagraphProperties.RightIndent := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeSpacingAfter(const AValue: Integer);
begin
  ParagraphProperties.SpacingAfter := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.SpacingAfter;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseSpacingAfter]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.SpacingAfter) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseSpacingAfter) then
    ParagraphProperties.SpacingAfter := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeSpacingBefore(const AValue: Integer);
begin
  ParagraphProperties.SpacingBefore := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.SpacingBefore;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseSpacingBefore]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.SpacingBefore) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseSpacingBefore) then
    ParagraphProperties.SpacingBefore := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeSuppressHyphenation(const AValue: Boolean);
begin
  ParagraphProperties.SuppressHyphenation := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.SuppressHyphenation;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseSuppressHyphenation]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.SuppressHyphenation) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseSuppressHyphenation) then
    ParagraphProperties.SuppressHyphenation := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeSuppressLineNumbers(const AValue: Boolean);
begin
  ParagraphProperties.SuppressLineNumbers := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.SuppressLineNumbers;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseSuppressLineNumbers]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.SuppressLineNumbers) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseSuppressLineNumbers) then
    ParagraphProperties.SuppressLineNumbers := AValue;
end;

procedure TdxParagraphPropertyAccessor.ChangeWidowOrphanControl(const AValue: Boolean);
begin
  ParagraphProperties.WidowOrphanControl := TdxDocumentModel(ParagraphProperties.DocumentModel).DefaultParagraphProperties.WidowOrphanControl;
  ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseWidowOrphanControl]);
  if (ParentParagraphProperties = nil) or (AValue <> ParentParagraphProperties.WidowOrphanControl) or
    not GetOptionsUseValue(TdxUsedParagraphFormattingOption.UseWidowOrphanControl) then
    ParagraphProperties.WidowOrphanControl := AValue;
end;

constructor TdxParagraphPropertyAccessor.Create(AParagraphProperties: TdxParagraphProperties;
  AParentParagraphProperties: TdxParagraphFormattingInfo; const AParentParagraphOptions: TdxParagraphFormattingOptions);
begin
  Create(AParagraphProperties, AParentParagraphProperties);
  FParentParagraphOptions.Value := AParentParagraphOptions;
end;

constructor TdxParagraphPropertyAccessor.Create(AParagraphProperties: TdxParagraphProperties;
  AParentParagraphProperties: TdxParagraphFormattingInfo);
begin
  inherited Create;
  FParagraphProperties := AParagraphProperties;
  FParentParagraphProperties := AParentParagraphProperties;
end;

destructor TdxParagraphPropertyAccessor.Destroy;
begin
  FParentParagraphProperties.Free;
  inherited Destroy;
end;

function TdxParagraphPropertyAccessor.GetOptionsUseValue(AUseOption: TdxUsedParagraphFormattingOption): Boolean;
begin
  if not FParentParagraphOptions.IsNull then
    Result := FParentParagraphOptions.Value.GetValue(AUseOption)
  else
    Result := False;
end;

{ TdxTableCellPropertyAccessor }

constructor TdxTableCellPropertyAccessor.Create(ATableCellProperties: TdxTableCellProperties;
  AParentCellProperties: TdxTableCellGeneralSettingsInfo; AParentTableCellOptions: TdxTableCellPropertiesOptions);
begin
  inherited Create;
  FTableCellProperties := ATableCellProperties;
  FParentCellProperties := AParentCellProperties;
  FParentTableCellOptions := AParentTableCellOptions;
end;

destructor TdxTableCellPropertyAccessor.Destroy;
begin
  FParentCellProperties.Free;
  inherited Destroy;
end;

procedure TdxTableCellPropertyAccessor.ChangeBackgroundColor(const AValue: TdxAlphaColor);
begin
  TableCellProperties.BackgroundColor := TdxDocumentModel(TableCellProperties.DocumentModel).DefaultTableCellProperties.BackgroundColor;
  TableCellProperties.ResetUse(TdxTableCellPropertiesOptions.MaskUseBackgroundColor);
  if (ParentCellProperties = nil) or (AValue <> ParentCellProperties.BackgroundColor) or
    (not ParentTableCellOptions.UseBackgroundColor) then
    TableCellProperties.BackgroundColor := AValue;
end;

procedure TdxTableCellPropertyAccessor.ChangeVerticalAlignment(const AValue: TdxVerticalAlignment);
begin
  TableCellProperties.VerticalAlignment := TdxDocumentModel(TableCellProperties.DocumentModel).DefaultTableCellProperties.VerticalAlignment;
  TableCellProperties.ResetUse(TdxTableCellPropertiesOptions.MaskUseVerticalAlignment);
  if (ParentCellProperties = nil) or (AValue <> ParentCellProperties.VerticalAlignment) or
    (not ParentTableCellOptions.UseVerticalAlignment) then
    TableCellProperties.VerticalAlignment := AValue;
end;

end.
