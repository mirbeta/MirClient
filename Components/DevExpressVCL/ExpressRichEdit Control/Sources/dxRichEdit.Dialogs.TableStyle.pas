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

unit dxRichEdit.Dialogs.TableStyle;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Generics.Defaults, Generics.Collections,
  cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  cxContainer, cxEdit, Menus, dxCore, dxCoreClasses, dxGDIPlusAPI,
  dxGDIPlusClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, ActnList, ImgList, StdCtrls, cxMaskEdit, cxTextEdit,
  cxImageComboBox, cxButtons, cxDropDownEdit, dxColorEdit,
  cxFontNameComboBox,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Options,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Types,
  dxRichEdit.Actions,
  dxRichEdit.Control,

  dxRichEditDialogsSimpleControl,
  dxRichEditFontNameComboBox,
  dxRichEditBorderLineWeightComboBox,
  dxRichEdit.Dialogs.TableStyleFormController,
  dxRichEdit.Dialogs.BorderShadingHelper,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.Commands.Tables.Cells,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.TabFormatting;

type
  { TdxToggleConditionalTableCellsBordersCommandBase }

  TdxToggleConditionalTableCellsBordersCommandBaseClass = class of TdxToggleConditionalTableCellsBordersCommandBase;

  TdxToggleConditionalTableCellsBordersCommandBase = class(TdxToggleTableCellsBordersCommandBase)
  strict private
    function GetCurrentBorders: TdxBordersBase; inline;
  private
    FTableStyleController: TdxTableStyleFormController;
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders; ABorders: TdxBorderBaseList); override;

    property CurrentBorders: TdxBordersBase read GetCurrentBorders;
    property TableStyleController: TdxTableStyleFormController read FTableStyleController write FTableStyleController;
  end;

  { TdxRichEditTableStyleDialogForm }

  TdxRichEditTableStyleDialogForm = class(TdxRichEditCustomDialogForm)
    alActions: TActionList;
    AlignBottomCenter1: TMenuItem;
    AlignBottomLeft1: TMenuItem;
    AlignBottomRight1: TMenuItem;
    AlignCenter1: TMenuItem;
    AlignCenterLeft1: TMenuItem;
    AlignCenterRight1: TMenuItem;
    AlignTopCenter1: TMenuItem;
    AlignTopLeft1: TMenuItem;
    AlignTopRight1: TMenuItem;
    aResetTableCellsBorders: TAction;
    aToggleFontBold: TAction;
    aToggleFontItalic: TAction;
    aToggleFontUnderline: TAction;
    aToggleTableCellsAllBorders: TAction;
    aToggleTableCellsBottomBorder: TAction;
    aToggleTableCellsBottomCenterAlignment: TAction;
    aToggleTableCellsBottomLeftAlignment: TAction;
    aToggleTableCellsBottomRightAlignment: TAction;
    aToggleTableCellsInsideBorder: TAction;
    aToggleTableCellsInsideHorizontalBorder: TAction;
    aToggleTableCellsInsideVerticalBorder: TAction;
    aToggleTableCellsLeftBorder: TAction;
    aToggleTableCellsMiddleCenterAlignment: TAction;
    aToggleTableCellsMiddleLeftAlignment: TAction;
    aToggleTableCellsMiddleRightAlignment: TAction;
    aToggleTableCellsOutsideBorder: TAction;
    aToggleTableCellsRightBorder: TAction;
    aToggleTableCellsTopBorder: TAction;
    aToggleTableCellsTopCenterAlignment: TAction;
    aToggleTableCellsTopLeftAlignment: TAction;
    aToggleTableCellsTopRightAlignment: TAction;
    btnCancel: TcxButton;
    btnFormat: TcxButton;
    btnOk: TcxButton;
    btnTableAlignment: TcxButton;
    btnTableBorders: TcxButton;
    btnToggleFontBold: TcxButton;
    btnToggleFontItalic: TcxButton;
    btnToggleFontUnderline: TcxButton;
    cmbApplyTo: TcxComboBox;
    cmbCurrentStyle: TcxComboBox;
    cmbFontColor: TdxColorEdit;
    cmbFontEdit: TdxRichEditFontNameComboBox;
    cmbFontSize: TcxComboBox;
    cmbParent: TcxComboBox;
    cmbTableBorderLineColor: TdxColorEdit;
    cmbTableBorderLineStyle: TcxComboBox;
    cmbTableBorderLineWeight: TdxBorderLineWeightComboBox;
    cmbTableCellsShading: TdxColorEdit;
    dxLayoutBarLookAndFeel: TdxLayoutStandardLookAndFeel;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    dxLayoutControl1Item12: TdxLayoutItem;
    dxLayoutControl1Item13: TdxLayoutItem;
    dxLayoutControl1Item14: TdxLayoutSeparatorItem;
    dxLayoutControl1Item24: TdxLayoutItem;
    dxLayoutControl1Item25: TdxLayoutItem;
    dxLayoutControl1Item26: TdxLayoutItem;
    dxLayoutControl1Item27: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutSeparatorItem4: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem5: TdxLayoutSeparatorItem;
    edtName: TcxTextEdit;
    ilActions: TcxImageList;
    lblFormatting: TdxLayoutSeparatorItem;
    lblProperties: TdxLayoutSeparatorItem;
    lblSelectedStyle: TdxLayoutSeparatorItem;
    lcgBarFontFormatting: TdxLayoutGroup;
    lciApplyFormattingTo: TdxLayoutItem;
    lciCurrentStyle: TdxLayoutItem;
    lcilName: TdxLayoutItem;
    lcilStyleBasedOn: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    miAllBorders: TMenuItem;
    miBottomBorder: TMenuItem;
    miFontDialog: TMenuItem;
    miInsideBorders: TMenuItem;
    miInsideHorizontalBorder: TMenuItem;
    miInsideVerticalBorder: TMenuItem;
    miLeftBorder: TMenuItem;
    miNoBorder: TMenuItem;
    miOutsideBorders: TMenuItem;
    miParagraphDialog: TMenuItem;
    miRightBorder: TMenuItem;
    miSeparator1: TMenuItem;
    miSeparator2: TMenuItem;
    miTabsDialog: TMenuItem;
    miTopBorder: TMenuItem;
    pmFormat: TPopupMenu;
    pmTableAlignment: TPopupMenu;
    pmTableBorders: TPopupMenu;
    PreviewRichEditControl: TdxRichEditControl;
    ilBorderLineStyle: TcxImageList;
    aFontDialog: TAction;
    aParagraphDialog: TAction;
    aTabsDialog: TAction;
    procedure edtNamePropertiesChange(Sender: TObject);
    procedure cmbCurrentStylePropertiesChange(Sender: TObject);
    procedure cmbParentPropertiesChange(Sender: TObject);
    procedure cmbTableCellsShadingPropertiesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aToggleFontBoldExecute(Sender: TObject);
    procedure aToggleFontItalicExecute(Sender: TObject);
    procedure aToggleFontUnderlineExecute(Sender: TObject);
    procedure cmbFontEditPropertiesChange(Sender: TObject);
    procedure cmbFontColorPropertiesChange(Sender: TObject);
    procedure cmbFontSizePropertiesChange(Sender: TObject);
    procedure cmbApplyToPropertiesChange(Sender: TObject);
    procedure cmbTableBorderLineColorPropertiesChange(Sender: TObject);
    procedure cmbTableBorderLineWeightPropertiesChange(Sender: TObject);
    procedure cmbTableBorderLineStylePropertiesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas;
      AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure aToggleTableCellsBottomBorderExecute(Sender: TObject);
    procedure aToggleTableCellsTopBorderExecute(Sender: TObject);
    procedure aToggleTableCellsAllBordersExecute(Sender: TObject);
    procedure aResetTableCellsBordersExecute(Sender: TObject);
    procedure aToggleTableCellsOutsideBorderExecute(Sender: TObject);
    procedure aToggleTableCellsInsideBorderExecute(Sender: TObject);
    procedure aToggleTableCellsLeftBorderExecute(Sender: TObject);
    procedure aToggleTableCellsRightBorderExecute(Sender: TObject);
    procedure aToggleTableCellsInsideHorizontalBorderExecute(Sender: TObject);
    procedure aToggleTableCellsInsideVerticalBorderExecute(Sender: TObject);
    procedure ChangeConditionalCurrentAlignment(Sender: TObject);
    procedure cmbTableBorderLineStylePropertiesChange(Sender: TObject);
    procedure aParagraphDialogExecute(Sender: TObject);
    procedure aFontDialogExecute(Sender: TObject);
    procedure aTabsDialogExecute(Sender: TObject);
  private
    type
      TAlignment = record
        Vertical: TdxVerticalAlignment;
        Paragraph: TdxParagraphAlignment;
        constructor Create(AVertical: TdxVerticalAlignment; AParagraph: TdxParagraphAlignment);
      end;
      TAlignments = TDictionary<TAction, TAlignment>;
  private
    FAlignments: TAlignments;
    FBorderShadingDialogHelper: TdxRichEditBorderShadingDialogHelper;
    FBorderInfo: TdxBorderInfo;
    function GetController: TdxTableStyleFormController; inline;

    procedure AddApplyToComboItem(AItems: TStrings; AItem: TdxConditionalTableStyleFormattingType);
    procedure PopulateApplyTo;
    procedure PopulateFontSize;
    procedure PopulateCurrentStyleComboCore(AComboBoxEdit: TcxCustomComboBox);
    procedure PopulateCurrentStyleCombo(AComboBox: TcxCustomComboBox; AStyles: TdxStyleCollectionBase);
    procedure PopulateParentStyleCombo; overload;
    procedure PopulateParentStyleCombo(AComboBox: TcxCustomComboBox; AStyles: TdxStyleCollectionBase); overload;
    procedure InitializeAlignmentDictionary;
    procedure InitializeBorderInfo;
    procedure InitializeConditionalStyleType;

    procedure ApplyCharacterProperties(AProperties: TdxMergedCharacterProperties; AData: TObject);
    procedure ApplyParagraphProperties(AProperties: TdxMergedParagraphProperties; AData: TObject);
    procedure ApplyTabsProperties(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; AData: TObject);
    procedure ChangeConditionalType;
    procedure SetBorderLineStyleIndex(AStyle: TdxBorderLineStyle);
    procedure SetNewStyleToController(const AStyleName: string);
    procedure SetParentStyle;

    function GetSourceStyle: TdxTableStyle; inline;
    function GetConditionalStyleType: TdxConditionalTableStyleFormattingType; inline;
    function GetIntermediateTableStyle: TdxTableStyle; inline;
    function GetCharacterProperties: TdxCharacterProperties; inline;
    function GetCurrentConditionalStyle: TdxTableConditionalStyle; inline;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure InitializeForm; override;

    procedure ChangesTableCellsBorder(ACommandClass: TdxToggleConditionalTableCellsBordersCommandBaseClass);

    procedure SubscribeControlsEvents; override;
    procedure SubscribeToggleButtonsEvents;
    procedure SubscribeCharacterPropertiesEvents;
    procedure UnsubscribeControlsEvents; override;
    procedure UnsubscribeToggleButtonsEvents;
    procedure UnsubscribeCharacterPropertiesEvents;
    procedure UpdateRichEditBars;
    procedure UpdateCharacterBars(AMergedCharacterProperties: TdxCharacterFormattingInfo);
    procedure UpdateTableBars(AMergedProperties: TdxCombinedCellPropertiesInfo);
    procedure UpdateFormCore; override;

    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property ConditionalStyleType: TdxConditionalTableStyleFormattingType read GetConditionalStyleType;
    property CurrentConditionalStyle: TdxTableConditionalStyle read GetCurrentConditionalStyle;
    property SourceStyle: TdxTableStyle read GetSourceStyle;
    property IntermediateTableStyle: TdxTableStyle read GetIntermediateTableStyle;
  public
    destructor Destroy; override;

    property Controller: TdxTableStyleFormController read GetController;
  end;

implementation

uses
  Contnrs, Math, dxCoreGraphics,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.Dialogs.EditStyleHelper,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.View.Core;

{$R *.dfm}

{$REGION 'Table Cells Border commands'}
type
  { TdxToggleConditionalTableCellsTopBorderCommand }

  TdxToggleConditionalTableCellsTopBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsBottomBorderCommand }

  TdxToggleConditionalTableCellsBottomBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsLeftBorderCommand }

  TdxToggleConditionalTableCellsLeftBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsRightBorderCommand }

  TdxToggleConditionalTableCellsRightBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxResetConditionalTableCellsBordersCommand }

  TdxResetConditionalTableCellsBordersCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
  end;

  { TdxToggleConditionalTableCellsAllBordersBorderCommand }

  TdxToggleConditionalTableCellsAllBordersBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsOutsideBorderCommand }

  TdxToggleConditionalTableCellsOutsideBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsInsideBorderCommand }

  TdxToggleConditionalTableCellsInsideBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsInsideHorizontalBorderCommand }

  TdxToggleConditionalTableCellsInsideHorizontalBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

  { TdxToggleConditionalTableCellsInsideVerticalBorderCommand }

  TdxToggleConditionalTableCellsInsideVerticalBorderCommand = class(TdxToggleConditionalTableCellsBordersCommandBase)
  protected
    procedure ResetBorder(ABorder: TdxBorderBase); override;
    procedure ModifyBorder(ABorder: TdxBorderBase); override;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  end;

{ TdxToggleConditionalTableCellsBordersCommandBase }

function TdxToggleConditionalTableCellsBordersCommandBase.GetCurrentBorders: TdxBordersBase;
begin
  Result := TableStyleController.CurrentBorders as TdxBordersBase;
end;

procedure TdxToggleConditionalTableCellsBordersCommandBase.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleConditionalTableCellsBordersCommandBase.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
end;

{ TdxToggleConditionalTableCellsTopBorderCommand }

procedure TdxToggleConditionalTableCellsTopBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsTopBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsTopBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.TopBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsBottomBorderCommand }

procedure TdxToggleConditionalTableCellsBottomBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsBottomBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsBottomBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.BottomBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsLeftBorderCommand }

procedure TdxToggleConditionalTableCellsLeftBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsLeftBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsLeftBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.LeftBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsRightBorderCommand }

procedure TdxToggleConditionalTableCellsRightBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsRightBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsRightBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.RightBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxResetConditionalTableCellsBordersCommand }

procedure TdxResetConditionalTableCellsBordersCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ResetBorder(ALocalBorder);
end;

{ TdxToggleConditionalTableCellsAllBordersBorderCommand }

procedure TdxToggleConditionalTableCellsAllBordersBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsAllBordersBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsAllBordersBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.LeftBorder);
    ALocalBorders.Add(CurrentBorders.RightBorder);
    ALocalBorders.Add(CurrentBorders.TopBorder);
    ALocalBorders.Add(CurrentBorders.BottomBorder);
    ALocalBorders.Add(CurrentBorders.InsideHorizontalBorder);
    ALocalBorders.Add(CurrentBorders.InsideVerticalBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsOutsideBorderCommand }

procedure TdxToggleConditionalTableCellsOutsideBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsOutsideBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.LeftBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.RightBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.TopBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.BottomBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsOutsideBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.LeftBorder);
    ALocalBorders.Add(CurrentBorders.RightBorder);
    ALocalBorders.Add(CurrentBorders.TopBorder);
    ALocalBorders.Add(CurrentBorders.BottomBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsInsideBorderCommand }

procedure TdxToggleConditionalTableCellsInsideBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ResetBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsInsideBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ModifyBorder(ALocalBorder);
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsInsideBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.InsideHorizontalBorder);
    ALocalBorders.Add(CurrentBorders.InsideVerticalBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsInsideHorizontalBorderCommand }

procedure TdxToggleConditionalTableCellsInsideHorizontalBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsInsideHorizontalBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.InsideHorizontalBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsInsideHorizontalBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.InsideHorizontalBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{ TdxToggleConditionalTableCellsInsideVerticalBorderCommand }

procedure TdxToggleConditionalTableCellsInsideVerticalBorderCommand.ResetBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ResetBorder(ALocalBorder);
end;

procedure TdxToggleConditionalTableCellsInsideVerticalBorderCommand.ModifyBorder(ABorder: TdxBorderBase);
var
  ALocalBorder: TdxBorderBase;
begin
  ALocalBorder := CurrentBorders.InsideVerticalBorder;
  inherited ModifyBorder(ALocalBorder);
end;

function TdxToggleConditionalTableCellsInsideVerticalBorderCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ALocalBorders: TdxBorderBaseList;
begin
  ALocalBorders := TdxBorderBaseList.Create;
  try
    ALocalBorders.Add(CurrentBorders.InsideVerticalBorder);
    Result := inherited CalculateIsChecked(ALocalBorders);
  finally
    ALocalBorders.Free;
  end;
end;

{$ENDREGION}

{ TdxRichEditTableStyleDialogForm }

function TdxRichEditTableStyleDialogForm.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := Controller.CharacterProperties;
end;

function TdxRichEditTableStyleDialogForm.GetConditionalStyleType: TdxConditionalTableStyleFormattingType;
begin
  Result := Controller.ConditionalStyleType;
end;

function TdxRichEditTableStyleDialogForm.GetController: TdxTableStyleFormController;
begin
  Result := TdxTableStyleFormController(inherited Controller);
end;

function TdxRichEditTableStyleDialogForm.GetCurrentConditionalStyle: TdxTableConditionalStyle;
begin
  if Controller.ConditionalStyleProperties <> nil then
    Result := Controller.ConditionalStyleProperties[ConditionalStyleType]
  else
    Result := nil;
end;

function TdxRichEditTableStyleDialogForm.GetIntermediateTableStyle: TdxTableStyle;
begin
  Result := Controller.IntermediateTableStyle;
end;

function TdxRichEditTableStyleDialogForm.GetSourceStyle: TdxTableStyle;
begin
  Result := Controller.TableSourceStyle;
end;

procedure TdxRichEditTableStyleDialogForm.AddApplyToComboItem(AItems: TStrings;
  AItem: TdxConditionalTableStyleFormattingType);
var
  AText: string;
begin
  AText := cxGetResourceString(dxConditionalTableStyleFormattingTypeNames[AItem]);
  AddItemValue(AItems, AText, Ord(AItem));
end;

procedure TdxRichEditTableStyleDialogForm.aFontDialogExecute(Sender: TObject);
var
  AInfo: TdxCharacterFormattingInfo;
  AMergedProperties: TdxMergedCharacterProperties;
  AControl: IdxRichEditControl;
begin
  ChangeConditionalType;
  if CurrentConditionalStyle = nil then
  begin
    AInfo := TdxCharacterFormattingInfo.Create;
    try
      AMergedProperties := TdxMergedCharacterProperties.Create(AInfo,
        TdxCharacterFormattingOptions.EmptyCharacterFormattingOption)
    finally
      AInfo.Free;
    end;
  end
  else
    AMergedProperties := Controller.GetMergedCharacterProperties;
  AControl := Controller.Control;
  try
    AControl.ShowFontForm(AMergedProperties, ApplyCharacterProperties, nil);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.aParagraphDialogExecute(Sender: TObject);
var
  AInfo: TdxParagraphFormattingInfo;
  AMergedProperties: TdxMergedParagraphProperties;
  AControl: IdxRichEditControl;
begin
  ChangeConditionalType;
  if CurrentConditionalStyle = nil then
  begin
    AInfo := TdxParagraphFormattingInfo.Create;
    try
      AMergedProperties := TdxMergedParagraphProperties.Create(AInfo,
        TdxParagraphFormattingOptions.EmptyParagraphFormattingOption)
    finally
      AInfo.Free;
    end;
  end
  else
    AMergedProperties := Controller.GetMergedParagraphProperties;
  AControl := Controller.Control;
  try
    AControl.ShowParagraphForm(AMergedProperties, ApplyParagraphProperties, nil);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditTableStyleDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  btnFormat.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogButtonFormat);
  lblSelectedStyle.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogSelectedStyle);
  lblProperties.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogProperties);
  lcilName.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogName);
  lcilStyleBasedOn.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogStyleBasedOn);
  lblFormatting.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogFormatting);
  lciCurrentStyle.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogCurrentStyle);
  lciApplyFormattingTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTableStyleDialogApplyFormattingTo);
  aFontDialog.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogFontDialog);
  aParagraphDialog.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogParagraphDialog);
  aTabsDialog.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogTabsDialog);
  aToggleTableCellsAllBorders.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsAllBorders);
  aResetTableCellsBorders.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogResetTableCellsBorders);
  aToggleFontBold.Hint := cxGetResourceString(@sdxRichEditTableStyleDialogToggleFontBoldHint);
  aToggleFontItalic.Hint := cxGetResourceString(@sdxRichEditTableStyleDialogToggleFontItalicHint);
  aToggleFontUnderline.Hint := cxGetResourceString(@sdxRichEditTableStyleDialogToggleFontUnderlineHint);
  aToggleTableCellsOutsideBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsOutsideBorder);
  aToggleTableCellsInsideBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsInsideBorder);
  aToggleTableCellsTopBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsTopBorder);
  aToggleTableCellsBottomBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsBottomBorder);
  aToggleTableCellsLeftBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsLeftBorder);
  aToggleTableCellsRightBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsRightBorder);
  aToggleTableCellsInsideHorizontalBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsInsideHorizontalBorder);
  aToggleTableCellsInsideVerticalBorder.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsInsideVerticalBorder);
  aToggleTableCellsTopLeftAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsTopLeftAlignment);
  aToggleTableCellsMiddleLeftAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsMiddleLeftAlignment);
  aToggleTableCellsBottomLeftAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsBottomLeftAlignment);
  aToggleTableCellsTopCenterAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsTopCenterAlignment);
  aToggleTableCellsMiddleCenterAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsMiddleCenterAlignment);
  aToggleTableCellsBottomCenterAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsBottomCenterAlignment);
  aToggleTableCellsTopRightAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsTopRightAlignment);
  aToggleTableCellsMiddleRightAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsMiddleRightAlignment);
  aToggleTableCellsBottomRightAlignment.Caption := cxGetResourceString(@sdxRichEditTableStyleDialogToggleTableCellsBottomRightAlignment);
end;


procedure TdxRichEditTableStyleDialogForm.ApplyCharacterProperties(AProperties: TdxMergedCharacterProperties;
  AData: TObject);
begin
  Controller.AddStyle;
  Controller.CopyCharacterPropertiesFromMerged(AProperties);
  UpdateRichEditBars;
end;

procedure TdxRichEditTableStyleDialogForm.ApplyParagraphProperties(AProperties: TdxMergedParagraphProperties;
  AData: TObject);
begin
  Controller.AddStyle;
  Controller.CopyParagraphPropertiesFromMerged(AProperties);
  UpdateRichEditBars;
end;

procedure TdxRichEditTableStyleDialogForm.ApplyTabsProperties(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer;
  AData: TObject);
begin
  Controller.AddStyle;
  Controller.Tabs.SetTabs(ATabInfo);
  UpdateRichEditBars;
end;

procedure TdxRichEditTableStyleDialogForm.aResetTableCellsBordersExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxResetConditionalTableCellsBordersCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aTabsDialogExecute(Sender: TObject);
var
  AControl: IdxRichEditControl;
  AInfo: TdxTabFormattingInfo;
  AModel: TdxDocumentModel;
  ADefaultTabWidth: Integer;
begin
  ChangeConditionalType;
  AControl := Controller.Control;
  AModel := AControl.InnerControl.DocumentModel;
  ADefaultTabWidth := AModel.DocumentProperties.DefaultTabWidth;
  AInfo := Controller.GetTabInfo;
  try
    AControl.ShowTabsForm(AInfo, ADefaultTabWidth, ApplyTabsProperties, nil);
  finally
    AInfo.Free;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.aToggleFontBoldExecute(Sender: TObject);
begin
  Controller.ChangeConditionalCurrentFontBoldValue(TAction(Sender).Checked);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleFontItalicExecute(Sender: TObject);
begin
  Controller.ChangeConditionalCurrentFontItalicValue(TAction(Sender).Checked);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleFontUnderlineExecute(Sender: TObject);
begin
  if TAction(Sender).Checked then
    Controller.ChangeConditionalCurrentFontUnderlineTypeValue(TdxUnderlineType.Single)
  else
    Controller.ChangeConditionalCurrentFontUnderlineTypeValue(TdxUnderlineType.None);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsAllBordersExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsAllBordersBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsBottomBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsBottomBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsInsideBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsInsideBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsInsideHorizontalBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsInsideHorizontalBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsInsideVerticalBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsInsideVerticalBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsLeftBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsLeftBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsOutsideBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsOutsideBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsRightBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsRightBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.aToggleTableCellsTopBorderExecute(Sender: TObject);
begin
  ChangesTableCellsBorder(TdxToggleConditionalTableCellsTopBorderCommand);
  btnTableBorders.Action := TAction(Sender);
end;

procedure TdxRichEditTableStyleDialogForm.ChangeConditionalCurrentAlignment(Sender: TObject);
var
  AAlignment: TAlignment;
begin
  ChangeConditionalType;
  btnTableAlignment.Action := Sender as TAction;
  AAlignment := FAlignments[TAction(Sender)];
  Controller.ChangeConditionalCurrentAlignmentValue(AAlignment.Vertical, AAlignment.Paragraph);
end;

procedure TdxRichEditTableStyleDialogForm.ChangeConditionalType;
var
  ACurrentTable: TdxTable;
begin
  ACurrentTable := PreviewRichEditControl.DocumentModel.ActivePieceTable.Tables[0];
  TdxEditStyleHelper.ChangeConditionalType(ACurrentTable, [ConditionalStyleType]);
end;

procedure TdxRichEditTableStyleDialogForm.ChangesTableCellsBorder(ACommandClass: TdxToggleConditionalTableCellsBordersCommandBaseClass);
var
  ACommand: TdxToggleConditionalTableCellsBordersCommandBase;
begin
  Controller.AddStyle;
  Controller.SubscribeConditionalStyleEvents;
  ChangeConditionalType;

  PreviewRichEditControl.Enabled := True;
  ACommand := ACommandClass.Create(PreviewRichEditControl);
  try
    ACommand.TableStyleController := Controller;
    ACommand.NewBorder := FBorderInfo;
    ACommand.Execute;
  finally
    ACommand.Free;
    PreviewRichEditControl.Enabled := False;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.cmbApplyToPropertiesChange(Sender: TObject);
var
  AApplyType: Integer;
begin
  Controller.UnsubscribeConditionalStyleEvents;
  if TryGetItemValue(cmbApplyTo, AApplyType) then
    Controller.ConditionalStyleType := TdxConditionalTableStyleFormattingType(AApplyType);
  ChangeConditionalType;
  Controller.SubscribeConditionalStyleEvents;
  UpdateRichEditBars;
end;

procedure TdxRichEditTableStyleDialogForm.cmbCurrentStylePropertiesChange(Sender: TObject);
var
  AStyle: IdxStyle;
  AStyleName: string;
begin
  Supports(cmbCurrentStyle.ItemObject, IdxStyle, AStyle);
  cmbCurrentStyle.Properties.OnChange := nil;
  AStyleName := AStyle.StyleName;
  SetNewStyleToController(AStyleName);
  SetController(CreateController(Controller.Parameters));
  cmbCurrentStyle.Properties.Items.Clear;
  cmbParent.Properties.Items.Clear;
  InitializeForm;
  cmbCurrentStyle.Properties.OnChange := cmbCurrentStylePropertiesChange;
  UpdateForm;
end;

procedure TdxRichEditTableStyleDialogForm.cmbFontColorPropertiesChange(Sender: TObject);
begin
  Controller.ChangeConditionalCurrentForeColorValue(TdxAlphaColors.FromColor(cmbFontColor.ColorValue));
end;

procedure TdxRichEditTableStyleDialogForm.cmbFontEditPropertiesChange(Sender: TObject);
begin
  Controller.ChangeConditionalCurrentFontNameValue(cmbFontEdit.FontName);
end;

procedure TdxRichEditTableStyleDialogForm.cmbFontSizePropertiesChange(Sender: TObject);
var
  AText: string;
  AValue: Integer;
begin
  if TdxEditStyleHelper.IsFontSizeValid(cmbFontSize.EditValue, AText, AValue) then
    Controller.ChangeConditionalCurrentDoubleFontSizeValue(AValue)
  else
  begin
    Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
    cmbFontSize.EditValue := CharacterProperties.DoubleFontSize / 2;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.cmbParentPropertiesChange(Sender: TObject);
begin
  ChangeConditionalType;
  SetParentStyle;
  UpdateRichEditBars;
end;

procedure TdxRichEditTableStyleDialogForm.cmbTableBorderLineColorPropertiesChange(Sender: TObject);
var
  AColor: TdxAlphaColor;
begin
  AColor := TdxAlphaColors.FromColor(cmbTableBorderLineColor.ColorValue);
  if FBorderShadingDialogHelper.Color = AColor then
    Exit;
  FBorderShadingDialogHelper.Color := AColor;
  FBorderInfo.Color := AColor;
  FBorderShadingDialogHelper.PopulateBorderLineStyle(ilBorderLineStyle, cmbTableBorderLineStyle.Properties.Items);
  TdxBorderLineWeightPainter.PopulateBorderLineWeight(cmbTableBorderLineWeight.Properties,
    FBorderInfo.Style, FBorderShadingDialogHelper.Color,
    UnitConverter, TdxRichEditBorderShadingDialogHelper.BordersLineWeights);
end;

procedure TdxRichEditTableStyleDialogForm.cmbTableBorderLineStylePropertiesChange(Sender: TObject);
begin
//
  FBorderInfo.Style := TdxBorderInfo(cmbTableBorderLineStyle.ItemObject).Style;
  TdxBorderLineWeightPainter.PopulateBorderLineWeight(cmbTableBorderLineWeight.Properties,
    FBorderInfo.Style, FBorderShadingDialogHelper.Color,
    UnitConverter, TdxRichEditBorderShadingDialogHelper.BordersLineWeights);
end;

procedure TdxRichEditTableStyleDialogForm.cmbTableBorderLineStylePropertiesDrawItem(AControl: TcxCustomComboBox;
  ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var
  ABorderInfo: TdxBorderInfo;
begin
  ACanvas.FillRect(ARect);
  ABorderInfo := AControl.Properties.Items.Objects[AIndex] as TdxBorderInfo;
  TdxBorderLineWeightPainter.DrawBorderLineItem(ABorderInfo, ACanvas, ARect, UnitConverter, False);

  if (AIndex = AControl.ItemIndex) and not AControl.Focused then
    ACanvas.DrawFocusRect(ARect);
end;

procedure TdxRichEditTableStyleDialogForm.cmbTableBorderLineWeightPropertiesChange(Sender: TObject);
var
  AItem: TcxImageComboBoxItem;
begin
  AItem := cmbTableBorderLineWeight.Properties.Items[cmbTableBorderLineWeight.ItemIndex];
  if FBorderShadingDialogHelper.Width = AItem.Value then
    Exit;
  FBorderShadingDialogHelper.Width := AItem.Value;
  FBorderInfo.Width := FBorderShadingDialogHelper.Width;
  FBorderShadingDialogHelper.PopulateBorderLineStyle(ilBorderLineStyle, cmbTableBorderLineStyle.Properties.Items);
end;

procedure TdxRichEditTableStyleDialogForm.cmbTableCellsShadingPropertiesChange(Sender: TObject);
begin
  Controller.ChangeConditionalCurrentBackgroundColorValue(TdxAlphaColors.FromColor(cmbTableCellsShading.ColorValue));
end;

function TdxRichEditTableStyleDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxTableStyleFormController.Create(PreviewRichEditControl, AControllerParameters as TdxFormControllerParameters);
end;

destructor TdxRichEditTableStyleDialogForm.Destroy;
begin
  FAlignments.Free;
  FBorderShadingDialogHelper.Free;
  FBorderInfo.Free;
  inherited Destroy;
end;

procedure TdxRichEditTableStyleDialogForm.edtNamePropertiesChange(Sender: TObject);
begin
  Controller.StyleName := edtName.Text;
end;

procedure TdxRichEditTableStyleDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AText: string;
begin
  if ModalResult <> mrOk then
    Exit;
  CanClose := Controller.IsValidName(edtName.Text);
  if CanClose then
    Controller.ApplyChanges
  else
  begin
    AText := cxGetResourceString(@sdxRichEditExceptionParagraphStyleNameAlreadyExists);
    Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
  end;
end;

procedure TdxRichEditTableStyleDialogForm.InitializeAlignmentDictionary;
begin
  FAlignments := TAlignments.Create;
  FAlignments.Add(aToggleTableCellsTopLeftAlignment, TAlignment.Create(TdxVerticalAlignment.Top, TdxParagraphAlignment.Left));
  FAlignments.Add(aToggleTableCellsMiddleLeftAlignment, TAlignment.Create(TdxVerticalAlignment.Center, TdxParagraphAlignment.Left));
  FAlignments.Add(aToggleTableCellsBottomLeftAlignment, TAlignment.Create(TdxVerticalAlignment.Bottom, TdxParagraphAlignment.Left));
  FAlignments.Add(aToggleTableCellsTopCenterAlignment, TAlignment.Create(TdxVerticalAlignment.Top, TdxParagraphAlignment.Center));
  FAlignments.Add(aToggleTableCellsMiddleCenterAlignment, TAlignment.Create(TdxVerticalAlignment.Center, TdxParagraphAlignment.Center));
  FAlignments.Add(aToggleTableCellsBottomCenterAlignment, TAlignment.Create(TdxVerticalAlignment.Bottom, TdxParagraphAlignment.Center));
  FAlignments.Add(aToggleTableCellsTopRightAlignment, TAlignment.Create(TdxVerticalAlignment.Top, TdxParagraphAlignment.Right));
  FAlignments.Add(aToggleTableCellsMiddleRightAlignment, TAlignment.Create(TdxVerticalAlignment.Center, TdxParagraphAlignment.Right));
  FAlignments.Add(aToggleTableCellsBottomRightAlignment, TAlignment.Create(TdxVerticalAlignment.Bottom, TdxParagraphAlignment.Right));
end;

procedure TdxRichEditTableStyleDialogForm.InitializeBorderInfo;
begin
  FBorderInfo := TdxBorderInfo.Create;
  FBorderInfo.Frame := false;
  FBorderInfo.Offset := 0;
  FBorderInfo.Shadow := False;
  FBorderInfo.Style := TdxBorderLineStyle.Single;
  FBorderInfo.Width := 10;
end;

procedure TdxRichEditTableStyleDialogForm.InitializeConditionalStyleType;
begin
  Controller.ConditionalStyleType := TdxConditionalTableStyleFormattingType.WholeTable;
end;

procedure TdxRichEditTableStyleDialogForm.InitializeForm;
begin
  InitializeAlignmentDictionary;
  InitializeBorderInfo;
  FBorderShadingDialogHelper := TdxRichEditBorderShadingDialogHelper.Create;
  FBorderShadingDialogHelper.Control := Control as TdxCustomRichEditControl;
  FBorderShadingDialogHelper.Border := FBorderInfo;
  FBorderShadingDialogHelper.PopulateBorderLineStyle(ilBorderLineStyle, cmbTableBorderLineStyle.Properties.Items);

  PreviewRichEditControl.Enabled := False;
  Controller.FillTempRichEdit(PreviewRichEditControl);
  if Controller.Parameters.Control.InnerControl <> nil then
    PreviewRichEditControl.MeasurementUnit := Controller.Parameters.Control.InnerControl.UIUnit;
  PopulateApplyTo;
  PopulateFontSize;
  InitializeConditionalStyleType;
  edtName.Text := Controller.StyleName;
  Populate(cmbCurrentStyle, PopulateCurrentStyleComboCore);
  cmbParent.Properties.Items.Add(cxGetResourceString(@sdxRichEditEditStyleDialogEmptyParentStyle));
  PopulateParentStyleCombo;
  UpdateSelectedIndex(cmbApplyTo, Ord(TdxConditionalTableStyleFormattingType.WholeTable));
  TdxBorderLineWeightPainter.PopulateBorderLineWeight(cmbTableBorderLineWeight.Properties,
    TdxBorderLineStyle.Single, TdxAlphaColors.Black,
    UnitConverter, TdxRichEditBorderShadingDialogHelper.BordersLineWeights);
  UpdateRichEditBars;
end;

procedure TdxRichEditTableStyleDialogForm.PopulateApplyTo;
begin
  Populate(cmbApplyTo, procedure (AComboBox: TcxCustomComboBox)
  var
    AItems: TStrings;
  begin
    AItems := AComboBox.Properties.Items;
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.WholeTable);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.FirstRow);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.LastRow);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.FirstColumn);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.LastColumn);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.OddRowBanding);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.EvenRowBanding);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.OddColumnBanding);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.EvenColumnBanding);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.TopLeftCell);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.TopRightCell);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.BottomLeftCell);
    AddApplyToComboItem(AItems, TdxConditionalTableStyleFormattingType.BottomRightCell);
  end);
end;

procedure TdxRichEditTableStyleDialogForm.PopulateCurrentStyleCombo(AComboBox: TcxCustomComboBox;
  AStyles: TdxStyleCollectionBase);
var
  I: Integer;
  AStyle: TdxStyleBase;
begin
  for I := 0 to AStyles.Count - 1 do
  begin
    AStyle := AStyles[I];
    if AStyle <> AStyles.DefaultItem then
      AComboBox.Properties.Items.AddObject(AStyle.StyleName, AStyle);
  end;
end;

procedure TdxRichEditTableStyleDialogForm.PopulateCurrentStyleComboCore(AComboBoxEdit: TcxCustomComboBox);
begin
  PopulateCurrentStyleCombo(AComboBoxEdit, TdxDocumentModel(SourceStyle.DocumentModel).TableStyles);
end;

procedure TdxRichEditTableStyleDialogForm.PopulateFontSize;
begin
  Populate(cmbFontSize, procedure(AComboBox: TcxCustomComboBox)
    var
      AFontSizes: TdxPredefinedFontSizeCollection;
      AFontSize: Integer;
      I: Integer;
    begin
      AFontSizes := Control.InnerControl.PredefinedFontSizeCollection;
      for I:= 0 to AFontSizes.Count - 1 do
      begin
        AFontSize := AFontSizes[I];
        AComboBox.Properties.Items.Add(IntToStr(AFontSize));
      end;
    end);
end;

procedure TdxRichEditTableStyleDialogForm.PopulateParentStyleCombo(AComboBox: TcxCustomComboBox;
  AStyles: TdxStyleCollectionBase);
var
  ACollection: TStrings;
  ACount, I: Integer;
  AStyle: TdxStyleBase;
begin
  ACollection := AComboBox.Properties.Items;
  ACollection.BeginUpdate;
  try
    ACount := AStyles.Count;
    for I := 0 to ACount - 1 do
    begin
      AStyle := AStyles[I];
      if SourceStyle.IsParentValid(AStyle) then
        ACollection.AddObject(AStyle.StyleName, AStyle);
    end;
  finally
    ACollection.EndUpdate;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.SetBorderLineStyleIndex(AStyle: TdxBorderLineStyle);
var
  I: Integer;
  AIndex: Integer;
  ABorder: TdxBorderInfo;
  FItems: TStrings;
begin
  AIndex := -1;
  FItems := cmbTableBorderLineStyle.Properties.Items;
  for I := 0 to FItems.Count - 1 do
  begin
    ABorder := TdxBorderInfo(FItems.Objects[I]);
    if ABorder.Style = AStyle then
    begin
      AIndex := I;
      Break;
    end;
    if ABorder.Style = TdxBorderLineStyle.Single then
      AIndex := I;
  end;
  cmbTableBorderLineStyle.ItemIndex := AIndex;
end;

procedure TdxRichEditTableStyleDialogForm.SetNewStyleToController(const AStyleName: string);
var
  AParameters: TdxTableStyleFormControllerParameters;
  ADocumentModel: TdxDocumentModel;
begin
  AParameters := TdxTableStyleFormControllerParameters(Controller.Parameters);
  ADocumentModel := TdxDocumentModel(AParameters.TableSourceStyle.DocumentModel);
  AParameters.TableSourceStyle := TdxTableStyle(ADocumentModel.TableStyles.GetStyleByName(AStyleName));
end;

procedure TdxRichEditTableStyleDialogForm.SetParentStyle;
begin
  if cmbParent.ItemIndex <> 0 then
    IntermediateTableStyle.Parent := TdxTableStyle(cmbParent.ItemObject)
  else
    IntermediateTableStyle.Parent := nil;
end;

procedure TdxRichEditTableStyleDialogForm.SubscribeCharacterPropertiesEvents;
begin
  cmbFontEdit.Properties.OnChange := cmbFontEditPropertiesChange;
  cmbFontColor.Properties.OnChange := cmbFontColorPropertiesChange;
  cmbFontSize.Properties.OnChange := cmbFontSizePropertiesChange;
end;

procedure TdxRichEditTableStyleDialogForm.SubscribeControlsEvents;
begin
  edtName.Properties.OnChange := edtNamePropertiesChange;
  cmbCurrentStyle.Properties.OnChange := cmbCurrentStylePropertiesChange;
  cmbParent.Properties.OnChange := cmbParentPropertiesChange;
  cmbTableCellsShading.Properties.OnChange := cmbTableCellsShadingPropertiesChange;
  cmbTableBorderLineStyle.Properties.OnChange := cmbTableBorderLineStylePropertiesChange;
  cmbTableBorderLineWeight.Properties.OnChange := cmbTableBorderLineWeightPropertiesChange;
  cmbTableBorderLineStyle.Properties.OnDrawItem := cmbTableBorderLineStylePropertiesDrawItem;
  cmbApplyTo.Properties.OnChange := cmbApplyToPropertiesChange;
end;

procedure TdxRichEditTableStyleDialogForm.SubscribeToggleButtonsEvents;
begin
  aToggleFontBold.OnExecute := aToggleFontBoldExecute;
  aToggleFontItalic.OnExecute := aToggleFontItalicExecute;
  aToggleFontUnderline.OnExecute := aToggleFontUnderlineExecute;
end;

procedure TdxRichEditTableStyleDialogForm.UnsubscribeCharacterPropertiesEvents;
begin
  cmbFontEdit.Properties.OnChange := nil;
  cmbFontColor.Properties.OnChange := nil;
  cmbFontSize.Properties.OnChange := nil;
end;

procedure TdxRichEditTableStyleDialogForm.UnsubscribeControlsEvents;
begin
  edtName.Properties.OnChange := nil;
  cmbCurrentStyle.Properties.OnChange := nil;
  cmbParent.Properties.OnChange := nil;
  cmbTableCellsShading.Properties.OnChange := nil;
  cmbTableBorderLineStyle.Properties.OnChange := nil;
  cmbTableBorderLineWeight.Properties.OnChange := nil;
  cmbTableBorderLineStyle.Properties.OnDrawItem := nil;
  cmbApplyTo.Properties.OnChange := nil;
end;

procedure TdxRichEditTableStyleDialogForm.UnsubscribeToggleButtonsEvents;
begin
  aToggleFontBold.OnExecute := nil;
  aToggleFontItalic.OnExecute := nil;
  aToggleFontUnderline.OnExecute := nil;
end;

procedure TdxRichEditTableStyleDialogForm.UpdateCharacterBars(AMergedCharacterProperties: TdxCharacterFormattingInfo);
begin
  UnsubscribeToggleButtonsEvents;
  aToggleFontBold.Checked := AMergedCharacterProperties.FontBold;
  aToggleFontItalic.Checked := AMergedCharacterProperties.FontItalic;
  aToggleFontUnderline.Checked := (AMergedCharacterProperties.FontUnderlineType = TdxUnderlineType.Single);
  SubscribeToggleButtonsEvents;
  UnsubscribeCharacterPropertiesEvents;
  cmbFontEdit.EditValue := AMergedCharacterProperties.FontName;
  cmbFontColor.ColorValue := TdxAlphaColors.ToColor(AMergedCharacterProperties.ForeColor);
  cmbFontSize.EditValue := FloatToStr(AMergedCharacterProperties.DoubleFontSize / 2);
  SubscribeCharacterPropertiesEvents;
end;

procedure TdxRichEditTableStyleDialogForm.UpdateFormCore;
var
  AParent: TdxTableStyle;
begin
  AParent := SourceStyle.Parent;
  cmbCurrentStyle.ItemObject := SourceStyle;
  if AParent = nil then
    cmbParent.ItemIndex := 0
  else
    cmbParent.ItemObject := AParent;
end;

procedure TdxRichEditTableStyleDialogForm.UpdateRichEditBars;
var
  ACharacterProperties: TdxMergedCharacterProperties;
  ATableCellProperties: TdxMergedTableCellProperties;
begin
  ACharacterProperties := Controller.GetMergedWithDefaultCharacterProperties;
  try
    UpdateCharacterBars(ACharacterProperties.Info);
  finally
    ACharacterProperties.Free;
  end;
  ATableCellProperties := Controller.GetMergedTableCellProperties;
  try
    UpdateTableBars(ATableCellProperties.Info);
  finally
    ATableCellProperties.Free;
  end;
end;

procedure TdxRichEditTableStyleDialogForm.UpdateTableBars(AMergedProperties: TdxCombinedCellPropertiesInfo);
var
  AWidth: Integer;
  AStyle: TdxBorderLineStyle;
begin
  FBorderInfo.Color := AMergedProperties.Borders.LeftBorder.Color;
  AWidth := AMergedProperties.Borders.LeftBorder.Width;
  if AWidth = 0 then
    AWidth := Max(1, Round(UnitConverter.PointsToModelUnitsF(0.5)));
  FBorderInfo.Width := AWidth;
  AStyle := AMergedProperties.Borders.LeftBorder.Style;
  if AStyle = TdxBorderLineStyle.&Nil then
    AStyle := TdxBorderLineStyle.Single;
  FBorderInfo.Style := AStyle;
  FBorderShadingDialogHelper.PopulateBorderLineStyle(ilBorderLineStyle, cmbTableBorderLineStyle.Properties.Items);
  TdxBorderLineWeightPainter.PopulateBorderLineWeight(cmbTableBorderLineWeight.Properties,
    FBorderInfo.Style, FBorderShadingDialogHelper.Color,
    UnitConverter, TdxRichEditBorderShadingDialogHelper.BordersLineWeights);
  SetBorderLineStyleIndex(FBorderInfo.Style);
  cmbTableBorderLineWeight.EditValue := FBorderInfo.Width;
  cmbTableBorderLineColor.ColorValue := TdxAlphaColors.ToColor(FBorderInfo.Color);
end;

procedure TdxRichEditTableStyleDialogForm.PopulateParentStyleCombo;
begin
  PopulateParentStyleCombo(cmbParent, TdxDocumentModel(SourceStyle.DocumentModel).TableStyles);
end;

{ TdxRichEditTableStyleDialogForm.TAlignment }

constructor TdxRichEditTableStyleDialogForm.TAlignment.Create(AVertical: TdxVerticalAlignment;
  AParagraph: TdxParagraphAlignment);
begin
  Vertical := AVertical;
  Paragraph := AParagraph;
end;

end.
