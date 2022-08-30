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

unit dxRichEdit.Dialogs.TableStyleFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Graphics, Types, Generics.Defaults, Generics.Collections, dxCoreGraphics,
  dxRichEdit.NativeApi,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.Platform.Font,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.EditStyleHelper;

type
  { TdxTableStyleFormControllerParametersBase }

  TdxTableStyleFormControllerParametersBase = class abstract(TdxFormControllerParameters)
  strict private
    FCurrentStyleEnabled: Boolean;
  public
    constructor Create(const AControl: IdxRichEditControl);
    property CurrentStyleEnabled: Boolean read FCurrentStyleEnabled write FCurrentStyleEnabled;
  end;

  { TdxTableStyleFormControllerParameters }

  TdxTableStyleFormControllerParameters = class(TdxTableStyleFormControllerParametersBase)
  strict private
    FTableSourceStyle: TdxTableStyle;
  public
    constructor Create(const AControl: IdxRichEditControl; ASourceStyle: TdxTableStyle);
    property TableSourceStyle: TdxTableStyle read FTableSourceStyle write FTableSourceStyle;
  end;

  { TdxTableStyleFormControllerBase }

  TdxTableStyleFormControllerBase = class abstract(TdxFormController)
  strict private
    FTableContent: TArray<string>;
  strict private
    FParameters: TdxFormControllerParameters;
    FControl: IdxRichEditControl;
    FModel: TdxDocumentModel;
  private
    FConditionalStyleType: TdxConditionalTableStyleFormattingType;
  protected
    function GetStyleIndex: Integer; virtual; abstract;
    function GetStyleName: string; virtual; abstract;
    procedure SetStyleName(const AValue: string); virtual; abstract;
    function GetCharacterProperties: TdxCharacterProperties; virtual; abstract;
    function GetParagraphProperties: TdxParagraphProperties; virtual; abstract;
    function GetTableCellProperties: TdxTableCellProperties; virtual; abstract;
    function GetTableProperties: TdxTableProperties; virtual; abstract;
    function GetConditionalStyleProperties: TdxTableConditionalStyleProperties; virtual; abstract;
    function GetTabs: TdxTabProperties; virtual; abstract;
    function GetColumnWidth(const ARichEditControl: IdxRichEditControl): Integer;
    function InitializeTable(const ARichEditControl: IdxRichEditControl): TdxTable;
    procedure SetCurrentStyle(ATempTable: TdxTable); virtual; abstract;
  public
    constructor Create(const APreviewStyleControl: IdxRichEditControl; AParameters: TdxFormControllerParameters);
    function GetTabInfo: TdxTabFormattingInfo; virtual; abstract;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; virtual; abstract;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; virtual; abstract;
    function GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties; virtual; abstract;
    function GetMergedTableCellProperties: TdxMergedTableCellProperties; virtual; abstract;
    function GetParentCharacterProperties: TdxCharacterFormattingInfo; virtual; abstract;
    function GetParentParagraphProperties: TdxParagraphFormattingInfo; virtual; abstract;
    function GetParentParagraphPropertiesOptions: TdxParagraphFormattingOptions; virtual; abstract;
    function GetParentTableCellProperties: TdxTableCellGeneralSettingsInfo; virtual; abstract;
    function GetParentTableCellPropertiesOptions: TdxTableCellPropertiesOptions; virtual; abstract;
    procedure AddStyle; virtual; abstract;
    procedure ChangeConditionalCurrentBackgroundColorValue(AIsChecked: TdxAlphaColor);
    procedure ChangeConditionalCurrentFontBoldValue(AIsChecked: Boolean);
    procedure ChangeConditionalCurrentDoubleFontSizeValue(AIsChecked: Integer);
    procedure ChangeConditionalCurrentFontItalicValue(AIsChecked: Boolean);
    procedure ChangeConditionalCurrentFontUnderlineTypeValue(AIsChecked: TdxUnderlineType);
    procedure ChangeConditionalCurrentFontNameValue(const AIsChecked: string);
    procedure ChangeConditionalCurrentForeColorValue(AIsChecked: TdxAlphaColor);
    procedure ChangeConditionalCurrentAlignmentValue(AVerticalAlignment: TdxVerticalAlignment; AParagraphAlignment: TdxParagraphAlignment);
    procedure CopyCharacterPropertiesFromMerged(AMergedProperties: TdxMergedCharacterProperties); overload;
    procedure CopyParagraphPropertiesFromMerged(AMergedProperties: TdxMergedParagraphProperties); overload;
    procedure FillTempRichEdit(const ARichEditControl: IdxRichEditControl);
    function IsValidName(const AName: string): Boolean; virtual; abstract;
    function GetCharacterPropertyAccessor: TdxCharacterPropertyAccessor;
    function GetParagraphPropertyAccessor: TdxParagraphPropertyAccessor;
    function GetTableCellPropertyAccessor: TdxTableCellPropertyAccessor;

    property Control: IdxRichEditControl read FControl;
    property Model: TdxDocumentModel read FModel;
    property Parameters: TdxFormControllerParameters read FParameters;
    property StyleIndex: Integer read GetStyleIndex;
    property StyleName: string read GetStyleName write SetStyleName;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
    property TableProperties: TdxTableProperties read GetTableProperties;
    property ConditionalStyleType: TdxConditionalTableStyleFormattingType read FConditionalStyleType write FConditionalStyleType;
    property ConditionalStyleProperties: TdxTableConditionalStyleProperties read GetConditionalStyleProperties;
    property Tabs: TdxTabProperties read GetTabs;
  end;

  { TdxTableStyleFormController }

  TdxTableStyleFormController = class(TdxTableStyleFormControllerBase)
  strict private
    FEditedTableStyle: TdxTableStyle;
    FEditedTableStyleIndex: Integer;
    function GetTableSourceStyle: TdxTableStyle;
    function GetCurrentConditionalStyle: TdxTableConditionalStyle;
    function GetConditionalStyleParent: TdxTableStyle;
  private
    function GetCurrentBorders: TdxBordersBase;
  protected
    function GetStyleName: string; override;
    procedure SetStyleName(const AValue: string); override;
    function GetStyleIndex: Integer; override;
    function GetCharacterProperties: TdxCharacterProperties; override;
    function GetParagraphProperties: TdxParagraphProperties; override;
    function GetTableCellProperties: TdxTableCellProperties; override;
    function GetTableProperties: TdxTableProperties; override;
    function GetConditionalStyleProperties: TdxTableConditionalStyleProperties; override;
    function GetTabs: TdxTabProperties; override;
    procedure SetCurrentStyle(ATempTable: TdxTable); override;

  public
    constructor Create(const APreviewStyleControl: IdxRichEditControl; AParameters: TdxFormControllerParameters);
    procedure AddStyle; override;
    procedure ApplyChanges; override;
    function GetTabInfo: TdxTabFormattingInfo; override;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; override;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; override;
    function GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties; override;
    function GetMergedTableCellProperties: TdxMergedTableCellProperties; override;
    function GetParentCharacterProperties: TdxCharacterFormattingInfo; override;
    function GetParentParagraphProperties: TdxParagraphFormattingInfo; override;
    function GetParentParagraphPropertiesOptions: TdxParagraphFormattingOptions; override;
    function GetParentTableCellProperties: TdxTableCellGeneralSettingsInfo; override;
    function GetParentTableCellPropertiesOptions: TdxTableCellPropertiesOptions; override;
    function GetParentConditionalCharacterProperties: TdxCharacterFormattingInfo;
    function GetParentConditionalParagraphProperties: TdxParagraphFormattingInfo;
    function GetParentConditionalParagraphPropertiesOptions: TdxParagraphFormattingOptions;
    function GetParentConditionalTableCellProperties: TdxTableCellGeneralSettingsInfo;
    function GetParentConditionalTableCellPropertiesOptions: TdxTableCellPropertiesOptions;
    procedure UnsubscribeConditionalStyleEvents;
    procedure SubscribeConditionalStyleEvents;
    function IsValidName(const AName: string): Boolean; override;

    property TableSourceStyle: TdxTableStyle read GetTableSourceStyle;
    property IntermediateTableStyle: TdxTableStyle read FEditedTableStyle;
    property CurrentConditionalStyle: TdxTableConditionalStyle read GetCurrentConditionalStyle;
    property ConditionalStyleParent: TdxTableStyle read GetConditionalStyleParent;
    property CurrentBorders: TdxBordersBase read GetCurrentBorders;
  end;

implementation

uses
  Math, dxCore, dxTypeHelpers,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.History.Table,
  dxRichEdit.Commands.Selection;

{ TdxTableStyleFormControllerParametersBase }

constructor TdxTableStyleFormControllerParametersBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FCurrentStyleEnabled := True;
end;

{ TdxTableStyleFormControllerParameters }

constructor TdxTableStyleFormControllerParameters.Create(const AControl: IdxRichEditControl; ASourceStyle: TdxTableStyle);
begin
  inherited Create(AControl);
  Assert(ASourceStyle <> nil);
  FTableSourceStyle := ASourceStyle;
end;

{ TdxTableStyleFormControllerBase }

constructor TdxTableStyleFormControllerBase.Create(const APreviewStyleControl: IdxRichEditControl; AParameters: TdxFormControllerParameters);
begin
  inherited  Create;
  Assert(AParameters <> nil, 'parameters');
  FControl := APreviewStyleControl;
  FParameters := AParameters;
  FModel := APreviewStyleControl.DocumentModel;
  FTableContent := TArray<string>.Create(
    ' ', cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableColumn1),
         cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableColumn2),
         cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableColumn3),
         cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableTotal),
    cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableRow1),   '7',   '7',   '5',    '19',
    cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableRow2),   '6',   '4',   '7',    '17',
    cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableRow3),   '8',   '7',   '9',    '24',
    cxGetResourceString(@sdxRichEditTableStyleDialogPreviewTableTotal),  '21',  '18',  '21',    '60');
end;

function TdxTableStyleFormControllerBase.GetCharacterPropertyAccessor: TdxCharacterPropertyAccessor;
begin
  Result := TdxCharacterPropertyAccessor.Create(CharacterProperties, GetParentCharacterProperties);
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentAlignmentValue(AVerticalAlignment: TdxVerticalAlignment;
  AParagraphAlignment: TdxParagraphAlignment);
var
  ATableCellPropertyAccessor: TdxTableCellPropertyAccessor;
  AParagraphPropertyAccessor: TdxParagraphPropertyAccessor;
begin
  AddStyle;
  ATableCellPropertyAccessor := GetTableCellPropertyAccessor;
  try
    AParagraphPropertyAccessor := GetParagraphPropertyAccessor;
    try
      ATableCellPropertyAccessor.ChangeVerticalAlignment(AVerticalAlignment);
      AParagraphPropertyAccessor.ChangeAlignment(AParagraphAlignment);
    finally
      AParagraphPropertyAccessor.Free;
    end;
  finally
    ATableCellPropertyAccessor.Free;
  end;
end;

function TdxTableStyleFormControllerBase.GetColumnWidth(const ARichEditControl: IdxRichEditControl): Integer;
var
  AActiveView: TdxRichEditView;
  AModel: TdxDocumentModel;
  ACaretPosition: TdxCaretPosition;
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ALayoutPosition: TdxDocumentLayoutPosition;
  AWidth: Integer;
begin
  AActiveView := ARichEditControl.InnerControl.ActiveView;
  AModel := ARichEditControl.InnerControl.DocumentModel;
  AActiveView.EnsureFormattingCompleteForSelection;
  ACaretPosition := AActiveView.CaretPosition;
  AUnitConverter := AModel.ToDocumentLayoutUnitConverter;
  if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.TableCell) then
  begin
    ALayoutPosition := ACaretPosition.LayoutPosition;
    if ALayoutPosition.TableCell = nil then
      AWidth := ALayoutPosition.Column.Bounds.Width
    else
      AWidth := ALayoutPosition.TableCell.TextWidth;
    Result := AUnitConverter.ToModelUnits(AWidth);
  end
  else
    Result := 0;
end;

function TdxTableStyleFormControllerBase.GetParagraphPropertyAccessor: TdxParagraphPropertyAccessor;
begin
  Result := TdxParagraphPropertyAccessor.Create(ParagraphProperties, GetParentParagraphProperties, GetParentParagraphPropertiesOptions);
end;

function TdxTableStyleFormControllerBase.GetTableCellPropertyAccessor: TdxTableCellPropertyAccessor;
begin
  Result := TdxTableCellPropertyAccessor.Create(TableCellProperties, GetParentTableCellProperties, GetParentTableCellPropertiesOptions);
end;

function TdxTableStyleFormControllerBase.InitializeTable(const ARichEditControl: IdxRichEditControl): TdxTable;
var
  AModel: TdxDocumentModel;
  ATarget: TdxPieceTable;
  ATempTable: TdxTable;
  APos: TdxDocumentLogPosition;
  I: Integer;
begin
  AModel := ARichEditControl.DocumentModel;
  ATarget := AModel.ActivePieceTable;
  ATempTable := ATarget.InsertTable(AModel.Selection.&End, 5, 5, TdxTableAutoFitBehaviorType.FixedColumnWidth, MinInt,
    GetColumnWidth(ARichEditControl), True, False);
  for I := 0 to 25 - 1 do
  begin
    APos := TdxDocumentModelPosition.FromParagraphStart(ATarget, I).LogPosition;
    ATarget.InsertText(APos, FTableContent[I]);
  end;
  Result := ATempTable;
end;

procedure TdxTableStyleFormControllerBase.FillTempRichEdit(const ARichEditControl: IdxRichEditControl);
var
  AModel: TdxDocumentModel;
  ATarget: TdxPieceTable;
  ATempTable: TdxTable;
  AStartOfDocumentCommand: TdxStartOfDocumentCommand;
  AAllTypes: TdxConditionalTableStyleFormattingTypes;
begin
  AModel := ARichEditControl.DocumentModel;
  ATarget := AModel.ActivePieceTable;

  if ATarget.Tables.Count = 0 then
    ATempTable := InitializeTable(ARichEditControl)
  else
    ATempTable := ATarget.Tables[0];

  SetCurrentStyle(ATempTable);
  AStartOfDocumentCommand := TdxStartOfDocumentCommand.Create(ARichEditControl);
  try
    AStartOfDocumentCommand.Execute;
  finally
    AStartOfDocumentCommand.Free;
  end;
  AAllTypes := [TdxConditionalTableStyleFormattingType.WholeTable .. TdxConditionalTableStyleFormattingType.BottomLeftCell];
  TdxEditStyleHelper.ChangeConditionalType(ATempTable, AAllTypes);
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentBackgroundColorValue(AIsChecked: TdxAlphaColor);
var
  AAccessor: TdxParagraphPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetParagraphPropertyAccessor;
  try
    AAccessor.ChangeBackColor(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentDoubleFontSizeValue(AIsChecked: Integer);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeDoubleFontSize(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentFontBoldValue(AIsChecked: Boolean);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeFontBold(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentFontItalicValue(AIsChecked: Boolean);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeFontItalic(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentFontNameValue(const AIsChecked: string);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeFontName(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentFontUnderlineTypeValue(AIsChecked: TdxUnderlineType);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeFontUnderlineType(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.ChangeConditionalCurrentForeColorValue(AIsChecked: TdxAlphaColor);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AddStyle;
  AAccessor := GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeForeColor(AIsChecked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.CopyCharacterPropertiesFromMerged(AMergedProperties: TdxMergedCharacterProperties);
var
  AMergedCharacterProperties: TdxCharacterFormattingInfo;
  ACharacterProperties: TdxMergedCharacterProperties;
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AMergedCharacterProperties := AMergedProperties.Info;
  AddStyle;
  ACharacterProperties := GetMergedCharacterProperties;
  try
    AAccessor := GetCharacterPropertyAccessor;
    try
      if ACharacterProperties.Info.FontName <> AMergedCharacterProperties.FontName then
        AAccessor.ChangeFontName(AMergedCharacterProperties.FontName);
      if ACharacterProperties.Info.FontBold <> AMergedCharacterProperties.FontBold then
        AAccessor.ChangeFontBold(AMergedCharacterProperties.FontBold);
      if ACharacterProperties.Info.FontItalic <> AMergedCharacterProperties.FontItalic then
        AAccessor.ChangeFontItalic(AMergedCharacterProperties.FontItalic);
      if ACharacterProperties.Info.DoubleFontSize <> AMergedCharacterProperties.DoubleFontSize then
        AAccessor.ChangeDoubleFontSize(AMergedCharacterProperties.DoubleFontSize);
      if ACharacterProperties.Info.ForeColor <> AMergedCharacterProperties.ForeColor then
        AAccessor.ChangeForeColor(AMergedCharacterProperties.ForeColor);
      if ACharacterProperties.Info.FontUnderlineType <> AMergedCharacterProperties.FontUnderlineType then
        AAccessor.ChangeFontUnderlineType(AMergedCharacterProperties.FontUnderlineType);
      if ACharacterProperties.Info.UnderlineColor <> AMergedCharacterProperties.UnderlineColor then
        AAccessor.ChangeUnderlineColor(AMergedCharacterProperties.UnderlineColor);
      if ACharacterProperties.Info.FontStrikeoutType <> AMergedCharacterProperties.FontStrikeoutType then
        AAccessor.ChangeFontStrikeoutType(AMergedCharacterProperties.FontStrikeoutType);
      if ACharacterProperties.Info.UnderlineWordsOnly <> AMergedCharacterProperties.UnderlineWordsOnly then
        AAccessor.ChangeUnderlineWordsOnly(AMergedCharacterProperties.UnderlineWordsOnly);
      if ACharacterProperties.Info.Script <> AMergedCharacterProperties.Script then
        AAccessor.ChangeScript(AMergedCharacterProperties.Script);
      if ACharacterProperties.Info.AllCaps <> AMergedCharacterProperties.AllCaps then
        AAccessor.ChangeAllCaps(AMergedCharacterProperties.AllCaps);
      if ACharacterProperties.Info.Hidden <> AMergedCharacterProperties.Hidden then
        AAccessor.ChangeHidden(AMergedCharacterProperties.Hidden);
    finally
      AAccessor.Free;
    end;
  finally
    ACharacterProperties.Free;
  end;
end;

procedure TdxTableStyleFormControllerBase.CopyParagraphPropertiesFromMerged(AMergedProperties: TdxMergedParagraphProperties);
var
  AMergedParagraphProperties: TdxParagraphFormattingInfo;
  AParagraphProperties: TdxMergedParagraphProperties;
  AAccessor: TdxParagraphPropertyAccessor;
begin
  AMergedParagraphProperties := AMergedProperties.Info;
  AddStyle;
  AParagraphProperties := GetMergedParagraphProperties;
  try
    AAccessor := GetParagraphPropertyAccessor;
    try
      if AParagraphProperties.Info.Alignment <> AMergedParagraphProperties.Alignment then
        AAccessor.ChangeAlignment(AMergedParagraphProperties.Alignment);
      if AParagraphProperties.Info.OutlineLevel <> AMergedParagraphProperties.OutlineLevel then
        AAccessor.ChangeOutlineLevel(AMergedParagraphProperties.OutlineLevel);
      if AParagraphProperties.Info.LeftIndent <> AMergedParagraphProperties.LeftIndent then
        AAccessor.ChangeLeftIndent(AMergedParagraphProperties.LeftIndent);
      if AParagraphProperties.Info.RightIndent <> AMergedParagraphProperties.RightIndent then
        AAccessor.ChangeRightIndent(AMergedParagraphProperties.RightIndent);
      if AParagraphProperties.Info.FirstLineIndentType <> AMergedParagraphProperties.FirstLineIndentType then
        ParagraphProperties.FirstLineIndentType := AMergedParagraphProperties.FirstLineIndentType;
      if AParagraphProperties.Info.FirstLineIndent <> AMergedParagraphProperties.FirstLineIndent then
        AAccessor.ChangeFirstLineIndent(AMergedParagraphProperties.FirstLineIndent);
      if AParagraphProperties.Info.SpacingBefore <> AMergedParagraphProperties.SpacingBefore then
        AAccessor.ChangeSpacingBefore(AMergedParagraphProperties.SpacingBefore);
      if AParagraphProperties.Info.SpacingAfter <> AMergedParagraphProperties.SpacingAfter then
        AAccessor.ChangeSpacingAfter(AMergedParagraphProperties.SpacingAfter);
      if AParagraphProperties.Info.LineSpacing <> AMergedParagraphProperties.LineSpacing then
        AAccessor.ChangeLineSpacing(AMergedParagraphProperties.LineSpacing);
      if AParagraphProperties.Info.LineSpacingType <> AMergedParagraphProperties.LineSpacingType then
        ParagraphProperties.LineSpacingType := AMergedParagraphProperties.LineSpacingType;

      if AParagraphProperties.Info.BeforeAutoSpacing <> AMergedParagraphProperties.BeforeAutoSpacing then
        AAccessor.ChangeBeforeAutoSpacing(AMergedParagraphProperties.BeforeAutoSpacing);
      if AParagraphProperties.Info.AfterAutoSpacing <> AMergedParagraphProperties.AfterAutoSpacing then
        AAccessor.ChangeAfterAutoSpacing(AMergedParagraphProperties.AfterAutoSpacing);
      if AParagraphProperties.Info.ContextualSpacing <> AMergedParagraphProperties.ContextualSpacing then
        AAccessor.ChangeContextualSpacing(AMergedParagraphProperties.ContextualSpacing);
      if AParagraphProperties.Info.KeepLinesTogether <> AMergedParagraphProperties.KeepLinesTogether then
        AAccessor.ChangeKeepLinesTogether(AMergedParagraphProperties.KeepLinesTogether);
      if AParagraphProperties.Info.KeepWithNext <> AMergedParagraphProperties.KeepWithNext then
        AAccessor.ChangeKeepWithNext(AMergedParagraphProperties.KeepWithNext);
      if AParagraphProperties.Info.PageBreakBefore <> AMergedParagraphProperties.PageBreakBefore then
        AAccessor.ChangePageBreakBefore(AMergedParagraphProperties.PageBreakBefore);
      if AParagraphProperties.Info.SuppressLineNumbers <> AMergedParagraphProperties.SuppressLineNumbers then
        AAccessor.ChangeSuppressLineNumbers(AMergedParagraphProperties.SuppressLineNumbers);
      if AParagraphProperties.Info.SuppressHyphenation <> AMergedParagraphProperties.SuppressHyphenation then
        AAccessor.ChangeSuppressHyphenation(AMergedParagraphProperties.SuppressHyphenation);
      if AParagraphProperties.Info.WidowOrphanControl <> AMergedParagraphProperties.WidowOrphanControl then
        AAccessor.ChangeWidowOrphanControl(AMergedParagraphProperties.WidowOrphanControl);
      if AParagraphProperties.Info.BackColor <> AMergedParagraphProperties.BackColor then
        AAccessor.ChangeBackColor(AMergedParagraphProperties.BackColor);
    finally
      AAccessor.Free;
    end;
  finally
    AParagraphProperties.Free;
  end;
end;

{ TdxTableStyleFormController }

constructor TdxTableStyleFormController.Create(const APreviewStyleControl: IdxRichEditControl; AParameters: TdxFormControllerParameters);
var
  ASourceTableStyles: TdxTableStyleCollection;
  APreviewModel: TdxDocumentModel;
  ATables: TdxTableCollection;
  I: Integer;
  AStyle, ASourceStyle: TdxTableStyle;
begin
  inherited Create(APreviewStyleControl, AParameters);
  ASourceTableStyles := AParameters.Control.InnerControl.DocumentModel.TableStyles;
  APreviewModel := APreviewStyleControl.DocumentModel;

  ATables := APreviewModel.ActivePieceTable.Tables;
  for I := 0 to ATables.Count - 1 do
    APreviewModel.ActivePieceTable.DeleteTableCore(ATables[I]);

  while APreviewModel.TableStyles.Count > 0 do
    APreviewModel.TableStyles.RemoveLastStyle;

  for I := 0 to ASourceTableStyles.Count - 1 do
  begin
    AStyle := ASourceTableStyles[I];
    if not AStyle.Deleted then
    begin
      APreviewModel.BeginUpdate;
      try
        AStyle.Copy(APreviewModel);
      finally
        APreviewModel.EndUpdate;
      end;
    end;
  end;
  for I := 0 to APreviewModel.TableStyles.Count - 1 do
  begin
    AStyle := APreviewModel.TableStyles[I];
    ASourceStyle := TdxTableStyle(ASourceTableStyles.GetStyleByName(AStyle.StyleName));
    AStyle.ConditionalStyleProperties.CopyFrom(ASourceStyle.ConditionalStyleProperties);
  end;
  FEditedTableStyleIndex := (TdxTableStyleFormControllerParameters(AParameters)).TableSourceStyle.Copy(APreviewModel);
  FEditedTableStyle := APreviewModel.TableStyles[FEditedTableStyleIndex];
end;

function TdxTableStyleFormController.GetStyleName: string;
begin
  Result := IntermediateTableStyle.LocalizedStyleName;
end;

procedure TdxTableStyleFormController.SetStyleName(const AValue: string);
begin
  IntermediateTableStyle.StyleName := AValue;
end;

function TdxTableStyleFormController.GetStyleIndex: Integer;
begin
  Result := FEditedTableStyleIndex;
end;

function TdxTableStyleFormController.GetTableSourceStyle: TdxTableStyle;
begin
  Result := (TdxTableStyleFormControllerParameters(Parameters)).TableSourceStyle;
end;

function TdxTableStyleFormController.GetCharacterProperties: TdxCharacterProperties;
begin
  if CurrentConditionalStyle <> nil then
    Result := CurrentConditionalStyle.CharacterProperties
  else
    Result := IntermediateTableStyle.CharacterProperties;
end;

function TdxTableStyleFormController.GetParagraphProperties: TdxParagraphProperties;
begin
  if CurrentConditionalStyle <> nil then
    Result := CurrentConditionalStyle.ParagraphProperties
  else
    Result := IntermediateTableStyle.ParagraphProperties;
end;

function TdxTableStyleFormController.GetTableCellProperties: TdxTableCellProperties;
begin
  if CurrentConditionalStyle <> nil then
    Result := CurrentConditionalStyle.TableCellProperties
  else
    Result := IntermediateTableStyle.TableCellProperties;
end;

function TdxTableStyleFormController.GetTableProperties: TdxTableProperties;
begin
  if CurrentConditionalStyle <> nil then
    Result := CurrentConditionalStyle.TableProperties
  else
    Result := IntermediateTableStyle.TableProperties;
end;

function TdxTableStyleFormController.GetConditionalStyleProperties: TdxTableConditionalStyleProperties;
begin
  Result := FEditedTableStyle.ConditionalStyleProperties;
end;

function TdxTableStyleFormController.GetCurrentBorders: TdxBordersBase;
begin
  if ConditionalStyleType = TdxConditionalTableStyleFormattingType.WholeTable then
    Result := IntermediateTableStyle.TableProperties.Borders
  else
    Result := TableCellProperties.Borders;
end;

function TdxTableStyleFormController.GetCurrentConditionalStyle: TdxTableConditionalStyle;
begin
  Result := ConditionalStyleProperties[ConditionalStyleType];
end;

function TdxTableStyleFormController.GetConditionalStyleParent: TdxTableStyle;
begin
  Result := CurrentConditionalStyle.Parent;
end;

function TdxTableStyleFormController.GetTabs: TdxTabProperties;
begin
  Result := IntermediateTableStyle.Tabs;
end;

function TdxTableStyleFormController.GetTabInfo: TdxTabFormattingInfo;
begin
  Result := IntermediateTableStyle.GetTabs;
end;

function TdxTableStyleFormController.GetMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := CurrentConditionalStyle.GetMergedWithDefaultCharacterProperties;
end;

function TdxTableStyleFormController.GetMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := CurrentConditionalStyle.GetMergedWithDefaultParagraphProperties;
end;

function TdxTableStyleFormController.GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  if CurrentConditionalStyle <> nil then
    Result := CurrentConditionalStyle.GetMergedWithDefaultCharacterProperties
  else
    Result := IntermediateTableStyle.GetMergedWithDefaultCharacterProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTableStyleFormController.GetMergedTableCellProperties: TdxMergedTableCellProperties;
begin
  if CurrentConditionalStyle <> nil then
    Result := CurrentConditionalStyle.GetMergedTableCellProperties
  else
    Result := IntermediateTableStyle.GetMergedTableCellProperties;
end;

procedure TdxTableStyleFormController.ApplyChanges;
begin
  TableSourceStyle.BeginUpdate;
  try
    TableSourceStyle.CopyProperties(IntermediateTableStyle);
    TableSourceStyle.ConditionalStyleProperties.CopyFrom(IntermediateTableStyle.ConditionalStyleProperties);
    TableSourceStyle.StyleName := IntermediateTableStyle.StyleName;
    if IntermediateTableStyle.Parent <> nil then
      TableSourceStyle.Parent := TdxTableStyle(TdxDocumentModel(TableSourceStyle.DocumentModel).TableStyles.GetStyleByName(IntermediateTableStyle.Parent.StyleName))
    else
      TableSourceStyle.Parent := nil;
  finally
    TableSourceStyle.EndUpdate;
  end;
end;

function TdxTableStyleFormController.GetParentCharacterProperties: TdxCharacterFormattingInfo;
begin
  if IntermediateTableStyle.Parent <> nil then
    Result := GetParentConditionalCharacterProperties
  else
    Result := nil;
end;

function TdxTableStyleFormController.GetParentParagraphProperties: TdxParagraphFormattingInfo;
begin
  if IntermediateTableStyle.Parent <> nil then
    Result := GetParentConditionalParagraphProperties
  else
    Result := nil;
end;

function TdxTableStyleFormController.GetParentParagraphPropertiesOptions: TdxParagraphFormattingOptions;
begin
  if IntermediateTableStyle.Parent <> nil then
    Result := GetParentConditionalParagraphPropertiesOptions
  else
    Result := Default(TdxParagraphFormattingOptions) //nil;
end;

function TdxTableStyleFormController.GetParentTableCellProperties: TdxTableCellGeneralSettingsInfo;
begin
  if IntermediateTableStyle.Parent <> nil then
    Result := GetParentConditionalTableCellProperties
  else
    Result := nil;
end;

function TdxTableStyleFormController.GetParentTableCellPropertiesOptions: TdxTableCellPropertiesOptions;
begin
  if IntermediateTableStyle.Parent <> nil then
    Result := GetParentConditionalTableCellPropertiesOptions
  else
    Result := Default(TdxTableCellPropertiesOptions) //nil;
end;

function TdxTableStyleFormController.GetParentConditionalCharacterProperties: TdxCharacterFormattingInfo;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  if ConditionalStyleParent <> nil then
  begin
    ACharacterProperties := ConditionalStyleParent.GetMergedCharacterProperties;
    try
      Result := ACharacterProperties.Info.Clone;
    finally
      ACharacterProperties.Free;
    end;
  end
  else
    Result := nil;
end;

function TdxTableStyleFormController.GetParentConditionalParagraphProperties: TdxParagraphFormattingInfo;
var
  AParagraphProperties: TdxMergedParagraphProperties;
begin
  if ConditionalStyleParent <> nil then
  begin
    AParagraphProperties := ConditionalStyleParent.GetMergedParagraphProperties;
    try
      Result := AParagraphProperties.Info.Clone;
    finally
      AParagraphProperties.Free;
    end;
  end
  else
    Result := nil;
end;

function TdxTableStyleFormController.GetParentConditionalParagraphPropertiesOptions: TdxParagraphFormattingOptions;
var
  AParagraphProperties: TdxMergedParagraphProperties;
begin
  if ConditionalStyleParent <> nil then
  begin
    AParagraphProperties := ConditionalStyleParent.GetMergedParagraphProperties;
    try
      Result := AParagraphProperties.Options
    finally
      AParagraphProperties.Free;
    end;
  end
  else
    Result := Default(TdxParagraphFormattingOptions)
end;

function TdxTableStyleFormController.GetParentConditionalTableCellProperties: TdxTableCellGeneralSettingsInfo;
var
  ATableCellProperties: TdxMergedTableCellProperties;
begin
  if ConditionalStyleParent <> nil then
  begin
    ATableCellProperties := ConditionalStyleParent.GetMergedTableCellProperties;
    try
      Result := ATableCellProperties.Info.GeneralSettings.Clone;
    finally
      ATableCellProperties.Free;
    end;
  end
  else
    Result := nil;
end;

function TdxTableStyleFormController.GetParentConditionalTableCellPropertiesOptions: TdxTableCellPropertiesOptions;
var
  ACellProperties: TdxMergedTableCellProperties;
begin
  if ConditionalStyleParent <> nil then
  begin
    ACellProperties := ConditionalStyleParent.GetMergedTableCellProperties;
    try
      Result := ACellProperties.Options
    finally
      ACellProperties.Free;
    end;
  end
  else
    Result := Default(TdxTableCellPropertiesOptions)
end;

procedure TdxTableStyleFormController.UnsubscribeConditionalStyleEvents;
begin
  if CurrentConditionalStyle <> nil then
  begin
    CurrentConditionalStyle.CharacterProperties.OnObtainAffectedRange.Remove(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.ParagraphProperties.OnObtainAffectedRange.Remove(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.TableCellProperties.OnObtainAffectedRange.Remove(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.TableProperties.OnObtainAffectedRange.Remove(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.TableRowProperties.OnObtainAffectedRange.Remove(IntermediateTableStyle.OnObtainAffectedRange);
  end;
end;

procedure TdxTableStyleFormController.SubscribeConditionalStyleEvents;
begin
  if CurrentConditionalStyle <> nil then
  begin
    CurrentConditionalStyle.CharacterProperties.OnObtainAffectedRange.Add(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.ParagraphProperties.OnObtainAffectedRange.Add(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.TableCellProperties.OnObtainAffectedRange.Add(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.TableProperties.OnObtainAffectedRange.Add(IntermediateTableStyle.OnObtainAffectedRange);
    CurrentConditionalStyle.TableRowProperties.OnObtainAffectedRange.Add(IntermediateTableStyle.OnObtainAffectedRange);
  end;
end;

procedure TdxTableStyleFormController.SetCurrentStyle(ATempTable: TdxTable);
begin
  ATempTable.StyleIndex := StyleIndex;
end;

procedure TdxTableStyleFormController.AddStyle;
var
  AController: TdxTableConditionalFormattingController;
begin
  if CurrentConditionalStyle = nil then
  begin
    UnsubscribeConditionalStyleEvents;
    ConditionalStyleProperties.AddStyle(TdxTableConditionalStyle.Create(IntermediateTableStyle, ConditionalStyleType));
    AController.Init(Control.DocumentModel.MainPieceTable.Tables[0]);
    AController.ResetCachedProperties(0);
    SubscribeConditionalStyleEvents;
  end;
end;

function TdxTableStyleFormController.IsValidName(const AName: string): Boolean;
var
  AModel: TdxDocumentModel;
begin
  AModel := TdxDocumentModel(TableSourceStyle.DocumentModel);
  Result := TdxEditStyleHelper.IsValidStyleName(AName, TableSourceStyle, AModel.TableStyles);
end;

end.
