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

unit dxRichEdit.Dialogs.TableProperties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActnList,
  Generics.Defaults, Generics.Collections, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, dxLayoutControlAdapters, dxLayoutContainer, StdCtrls, cxButtons, dxLayoutControl,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit,
  cxCheckBox, cxDropDownEdit, cxRadioGroup, dxLayoutLookAndFeels, cxClasses, dxCoreClasses,
  dxRichEdit.Types,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Dialogs.TablePropertiesFormController,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.UnitConverter;

type
  TdxRichEditTablePropertiesDialogForm = class;
  TdxOnSetValueToEditor = procedure (AEditor: TdxMeasurementUnitEdit; const AValue: Variant) of object;
  TdxOnGetValueFromEditor = function (AEditor: TdxMeasurementUnitEdit; ACorrectRange: Boolean = True): Variant of object;

  TdxCustomTableControlHelper<T> = class
  private
    FOwner: TdxRichEditTablePropertiesDialogForm;
    FLockCount: Integer;
    FValueTypes: TdxOrdinalList<T>;
    FMinValue: Integer;
    FMaxValue: Integer;
    FValue: TdxNullableInteger;
    FUseDefaultValue: TdxNullableBoolean;
    FOnGetValueFromEditor: TdxOnGetValueFromEditor;
    FOnSetValueToEditor: TdxOnSetValueToEditor;
    FOnPropertiesChanged: TNotifyEvent;
    FUnitType: TdxMeasurementUnit;
    FDefferedUpdateControl: Boolean;
    FMeasurementType: TdxMeasurementType;
    FOnControlChanged: TNotifyEvent;
    function CheckStateToNullableBool(ACheckState: TcxCheckBoxState): TdxNullableBoolean;
    function CheckVariantToNullableInteger(AValue: Variant): TdxNullableInteger;
    function GetIsUpdateLocked: Boolean;
    function GetValueType: TdxNullableValue<T>;
    procedure InternalPopulateComboBox;
    procedure SetMaxValue(const AValue: Integer);
    procedure SetMinValue(const AValue: Integer);
    procedure SetUnitType(const AValue: TdxMeasurementUnit);
    procedure SetUseDefaultValue(const AValue: TdxNullableBoolean);
    procedure SetValue(const AValue: TdxNullableInteger);
    procedure SetValueType(const AValue: TdxNullableValue<T>);
    function GetController: TdxTablePropertiesFormController;
  protected
    FCheckBox: TcxCheckBox;
    FComboBox: TcxComboBox;
    FEditor: TdxMeasurementUnitEdit;
    procedure AddItem(const ADisplayName: string; const AValue: T);
    procedure CheckBoxClick(Sender: TObject);
    procedure ComboBoxValueChange(Sender: TObject); virtual; abstract;
    procedure EditorValueChange(Sender: TObject);
    procedure PopulateComboBox; virtual; abstract;
    procedure SetValueToEditor(const AValue: TdxNullableInteger);
    procedure SubscribeEvents;
    procedure UnsubscribeEvents;
    procedure UpdateEnabledControls(AEnabled: Boolean);
    procedure UpdateCheckox;
    procedure UpdateControl; virtual; abstract;

    procedure DoControlChanged;
    procedure DoPropertiesChanged;
    procedure PropertiesChanged(Sender: TObject);
    procedure BeginUpdate;
    procedure EndUpdate;

    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property Value: TdxNullableInteger read FValue write SetValue;
    property OnPropertiesChanged: TNotifyEvent read FOnPropertiesChanged write FOnPropertiesChanged;
  public
    constructor Create(AOwner: TdxRichEditTablePropertiesDialogForm; const ACheckBox: TcxCheckBox; const AEditor: TdxMeasurementUnitEdit;
      const AComboBox: TcxComboBox);
    destructor Destroy; override;
    procedure RefreshMeasurementUnit; virtual;

    property ComboBox: TcxComboBox read FComboBox;
    property Controller: TdxTablePropertiesFormController read GetController;
    property CheckBox: TcxCheckBox read FCheckBox;
    property DefferedUpdateControl: Boolean read FDefferedUpdateControl write FDefferedUpdateControl;
    property Editor: TdxMeasurementUnitEdit read FEditor;
    property MaxValue: Integer read FMaxValue write SetMaxValue;
    property MinValue: Integer read FMinValue write SetMinValue;
    property ValueType: TdxNullableValue<T> read GetValueType write SetValueType;
    property UseDefaultValue: TdxNullableBoolean read FUseDefaultValue write SetUseDefaultValue;
    property UnitType: TdxMeasurementUnit read FUnitType write SetUnitType;
    property OnSetValueToEditor: TdxOnSetValueToEditor read FOnSetValueToEditor write FOnSetValueToEditor;
    property OnGetValueFromEditor: TdxOnGetValueFromEditor read FOnGetValueFromEditor write FOnGetValueFromEditor;
    property OnControlChanged: TNotifyEvent read FOnControlChanged write FOnControlChanged;
  end;

  TdxTableSizeHelper = class(TdxCustomTableControlHelper<TdxWidthUnitType>)
  private
    FIsValueInPercent: Boolean;
    FMaxPercentValue: Integer;
    function GetCheckBox: TcxCheckBox; inline;
    function GetComboBox: TcxComboBox; inline;
    function GetEditor: TdxMeasurementUnitEdit; inline;
    function GetWidth: TdxNullableInteger; inline;
    function GetWidthType: TdxNullableValue<TdxWidthUnitType>; inline;
    procedure SetIsValueInPercent(const Value: Boolean);
    procedure SetWidth(const AValue: TdxNullableInteger);
    procedure SetWidthType(const Value: TdxNullableValue<TdxWidthUnitType>);
    procedure SetMaxPercentValue(const Value: Integer);
  protected
    function CalculateActualWidth: TdxNullableInteger;
    procedure ComboBoxValueChange(Sender: TObject); override;
    procedure PopulateComboBox; override;
    procedure UpdateControl; override;
  public
    procedure RefreshMeasurementUnit; override;
    property CheckBoxPreferredWidth: TcxCheckBox read GetCheckBox;
    property EditPreferredWidth: TdxMeasurementUnitEdit read GetEditor;
    property ComboBoxWidthType: TcxComboBox read GetComboBox;
    property IsValueInPercent: Boolean read FIsValueInPercent write SetIsValueInPercent;
    property MaxPercentValue: Integer read FMaxPercentValue write SetMaxPercentValue;
    property Width: TdxNullableInteger read GetWidth write SetWidth;
    property WidthType: TdxNullableValue<TdxWidthUnitType> read GetWidthType write SetWidthType;
  end;

  TdxTableRowHeightHelper = class(TdxCustomTableControlHelper<TdxHeightUnitType>)
  private
    function GetHeight: TdxNullableInteger; inline;
    procedure SetHeight(const AValue: TdxNullableInteger); inline;
    function GetCheckBox: TcxCheckBox; inline;
    function GetComboBox: TcxComboBox; inline;
    function GetEditor: TdxMeasurementUnitEdit; inline;
  protected
    procedure ComboBoxValueChange(Sender: TObject); override;
    procedure PopulateComboBox; override;
    procedure UpdateControl; override;
  public
    property CheckBoxHeight: TcxCheckBox read GetCheckBox;
    property EditHeight: TdxMeasurementUnitEdit read GetEditor;
    property ComboBoxHeightType: TcxComboBox read GetComboBox;
    property Height: TdxNullableInteger read GetHeight write SetHeight;
  end;

  TdxRichEditTablePropertiesDialogForm = class(TdxRichEditCustomDialogForm)
    btnBorder: TcxButton;
    btnCancel: TcxButton;
    btnCellOptions: TcxButton;
    btnNextColumn: TcxButton;
    btnNextRow: TcxButton;
    btnOk: TcxButton;
    btnPreviousColumn: TcxButton;
    btnPreviousRow: TcxButton;
    btnTableOptions: TcxButton;
    cbCantSplit: TcxCheckBox;
    cbCellPreferredWidth: TcxCheckBox;
    cbColumnPreferredWidth: TcxCheckBox;
    cbHeader: TcxCheckBox;
    cbSpecifyHeight: TcxCheckBox;
    cbTablePreferredWidth: TcxCheckBox;
    cmbCellWidthType: TcxComboBox;
    cmbColumnWidthType: TcxComboBox;
    cmbRowHeightType: TcxComboBox;
    cmbTableWidthType: TcxComboBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group10: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group5: TdxLayoutGroup;
    dxLayoutControl1Group6: TdxLayoutGroup;
    dxLayoutControl1Group7: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group8: TdxLayoutGroup;
    dxLayoutControl1Group9: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item15: TdxLayoutItem;
    dxLayoutControl1Item16: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item22: TdxLayoutItem;
    dxLayoutControl1Item23: TdxLayoutItem;
    dxLayoutControl1Item28: TdxLayoutItem;
    dxLayoutControl1Item29: TdxLayoutItem;
    dxLayoutControl1Item30: TdxLayoutItem;
    dxLayoutControl1Item31: TdxLayoutItem;
    dxLayoutControl1Item32: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    lblCellSize: TdxLayoutSeparatorItem;
    lblCellVerticalAlighment: TdxLayoutSeparatorItem;
    lblColumnSize: TdxLayoutSeparatorItem;
    lblRowOptions: TdxLayoutSeparatorItem;
    lblRowSize: TdxLayoutSeparatorItem;
    lblTableAlignment: TdxLayoutSeparatorItem;
    lblTableSize: TdxLayoutSeparatorItem;
    lcgCell: TdxLayoutGroup;
    lcgCellVerticalAlignment: TdxLayoutGroup;
    lcgColumn: TdxLayoutGroup;
    lcgRow: TdxLayoutGroup;
    lcgTabControl: TdxLayoutGroup;
    lcgTable: TdxLayoutGroup;
    lcgTableAlignment: TdxLayoutGroup;
    lciBorder: TdxLayoutItem;
    lciCantSplit: TdxLayoutItem;
    lciCellOptions: TdxLayoutItem;
    lciCellWidthType: TdxLayoutItem;
    lciColumnNumber: TdxLayoutItem;
    lciColumnWidthType: TdxLayoutItem;
    lciHeader: TdxLayoutItem;
    lciIndentFromLeft: TdxLayoutItem;
    lcilRowNumber: TdxLayoutItem;
    lciNextColumn: TdxLayoutItem;
    lciNextRow: TdxLayoutItem;
    lciPreviousColumn: TdxLayoutItem;
    lciPreviousRow: TdxLayoutItem;
    lciRowHeightType: TdxLayoutItem;
    lciTableOptions: TdxLayoutItem;
    lciTableWidthType: TdxLayoutItem;
    rbCellVerticalAlignmentBottom: TcxRadioButton;
    rbCellVerticalAlignmentCenter: TcxRadioButton;
    rbCellVerticalAlignmentTop: TcxRadioButton;
    rgTableAlignmenCenter: TcxRadioButton;
    rgTableAlignmenRight: TcxRadioButton;
    rgTableAlignmentLeft: TcxRadioButton;
    seCellPreferredWidth: TdxMeasurementUnitEdit;
    seColumnPreferredWidth: TdxMeasurementUnitEdit;
    seHeight: TdxMeasurementUnitEdit;
    seIndentFromLeft: TdxMeasurementUnitEdit;
    seTablePreferredWidth: TdxMeasurementUnitEdit;
    procedure btnTableOptionsClick(Sender: TObject);
    procedure btnBorderClick(Sender: TObject);
    procedure btnCellOptionsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FTableSizeControl: TdxTableSizeHelper;
    FColumnSizeControl: TdxTableSizeHelper;
    FCellSizeControl: TdxTableSizeHelper;
    FRowHeightControl: TdxTableRowHeightHelper;
    function GetController: TdxTablePropertiesFormController; inline;
    procedure SetCellVerticalAlignment(const AValue: TdxNullableValue<TdxVerticalAlignment>);
    procedure SetTableAlignment(const AValue: TdxNullableValue<TdxTableRowAlignment>);
    function GetCellVerticalAlignment: TdxNullableValue<TdxVerticalAlignment>;
    function GetTableAlignment: TdxNullableValue<TdxTableRowAlignment>;
  protected
    procedure ApplyLocalization; override;
    procedure InitializeForm; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure UpdateFormCore; override;

    procedure UpdateCellTab;
    procedure UpdateColumnTab;
    procedure UpdateRowTab;
    procedure UpdateTableTab;

    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateEnabledControls;

    procedure CellSizeControlChanged(Sender: TObject);
    procedure CellVerticalAlignmentSelectedIndexChanged(Sender: TObject);
    procedure ColumnSizeControlChanged(Sender: TObject);
    procedure IndentFromLeftValueChanged(Sender: TObject);
    procedure TableAlignmentSelectedIndexChanged(Sender: TObject);
    procedure TableRowHeightControlChanged(Sender: TObject);
    procedure TableSizeControlChanged(Sender: TObject);

    procedure ShowBorderShadingForm(ASelectedCells: TdxSelectedCellsCollection);
    procedure ShowCellOptionsForm;
    procedure ShowTableOptionsForm;

    class procedure UpdateCheckBox(ACheckBox: TcxCheckBox; const ACheck: TdxNullableBoolean); static;
  public
    destructor Destroy; override;
    property Controller: TdxTablePropertiesFormController read GetController;
    property CellVerticalAlignment: TdxNullableValue<TdxVerticalAlignment> read GetCellVerticalAlignment write SetCellVerticalAlignment;
    property TableAlignment: TdxNullableValue<TdxTableRowAlignment> read GetTableAlignment write SetTableAlignment;
  end;

implementation

uses
  Math, dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.BorderShading,
  dxRichEdit.Dialogs.BorderShadingController,
  dxRichEdit.Dialogs.TableCellOptions,
  dxRichEdit.Dialogs.TableCellOptionsFormController,
  dxRichEdit.Strs,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.PieceTable;

{$R *.dfm}

{ TdxRichEditTablePropertiesDialogForm }

procedure TdxRichEditTablePropertiesDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  btnBorder.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonBorder);
  btnCellOptions.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonCellOptions);
  btnTableOptions.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonTableOptions);
  lblTableSize.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogSize);
  cbTablePreferredWidth.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogPreferredWidth);
  lblTableAlignment.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogTableAlignment);
  lblRowSize.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogSize);
  cbSpecifyHeight.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogSpecifyHeight);
  lblRowOptions.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogRowOptions);
  cbCantSplit.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogCantSplit);
  cbHeader.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogHeader);
  btnPreviousRow.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonPreviousRow);
  btnNextRow.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonNextRow);
  lblColumnSize.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogSize);
  cbColumnPreferredWidth.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogPreferredWidth);
  btnPreviousColumn.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonPreviousColumn);
  btnNextColumn.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogButtonNextColumn);
  lblCellSize.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogSize);
  cbCellPreferredWidth.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogPreferredWidth);
  lblCellVerticalAlighment.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogCellVerticalAlighment);
  rbCellVerticalAlignmentTop.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogCellVerticalAlignmentTop);
  rbCellVerticalAlignmentCenter.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogCellVerticalAlignmentCenter);
  rbCellVerticalAlignmentBottom.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogCellVerticalAlignmentBottom);
  rgTableAlignmentLeft.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogTableAlignmentLeft);
  rgTableAlignmenCenter.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogTableAlignmenCenter);
  rgTableAlignmenRight.Caption := cxGetResourceString(@sdxRichEditTablePropertiesDialogTableAlignmenRight);
  lcgTable.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogTable);
  lcgRow.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogRow);
  lcgColumn.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogColumn);
  lcgCell.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogCell);
  lciTableWidthType.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogWidthType);
  lciIndentFromLeft.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogIndentFromLeft);
  lciRowHeightType.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogRowHeightType);
  lcilRowNumber.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogRowNumber);
  lciColumnNumber.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogColumnNumber);
  lciColumnWidthType.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogWidthType);
  lciCellWidthType.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTablePropertiesDialogWidthType);
end;

procedure TdxRichEditTablePropertiesDialogForm.btnBorderClick(Sender: TObject);
var
  AControllerTable: TdxTable;
  ADocumentModel: TdxDocumentModel;
  AOldSelection: TdxDocumentLogPosition;
  AFirstCell, ALastCell: TdxTableCell;
  ASelectedCells: TdxSelectedCellsCollection;
begin
  AControllerTable := Controller.Table;
  ADocumentModel := TdxDocumentModel(AControllerTable.DocumentModel);
  if ADocumentModel.Selection.Start = ADocumentModel.Selection.&End then
  begin
    AOldSelection := ADocumentModel.Selection.Start;
    ADocumentModel.BeginUpdate;
    try
      AFirstCell := AControllerTable.FirstRow.FirstCell;
      ALastCell := AControllerTable.LastRow.LastCell;
      ADocumentModel.Selection.ManuallySetTableSelectionStructureAndChangeSelection(AFirstCell, ALastCell);
      ASelectedCells := ADocumentModel.Selection.SelectedCells as TdxSelectedCellsCollection;
      try
        ShowBorderShadingForm(ASelectedCells);
      finally
        ADocumentModel.Selection.ClearSelectionInTable;
        ADocumentModel.Selection.SetInterval(AOldSelection, AOldSelection);
      end;
    finally
      ADocumentModel.EndUpdate;
    end;
  end
  else
    ShowBorderShadingForm(Controller.SourceSelectedCells);
end;

procedure TdxRichEditTablePropertiesDialogForm.btnCellOptionsClick(Sender: TObject);
begin
  ShowCellOptionsForm;
end;

procedure TdxRichEditTablePropertiesDialogForm.btnTableOptionsClick(Sender: TObject);
begin
  ShowTableOptionsForm;
end;

procedure TdxRichEditTablePropertiesDialogForm.CellSizeControlChanged(Sender: TObject);
begin
  Controller.UseDefaultCellWidth := FCellSizeControl.UseDefaultValue;
  if not FCellSizeControl.Width.IsNull then
    Controller.CellWidth := FCellSizeControl.Width.Value;
  Controller.CellWidthUnitType := FCellSizeControl.WidthType.Value;
  FCellSizeControl.MaxValue := Controller.GetColumnWidthMaxValueConsiderWidthUnitType;
  if FCellSizeControl.WidthType <> FColumnSizeControl.WidthType then
  begin
    Controller.ColumnWidthUnitType := FCellSizeControl.WidthType.Value;
    Controller.ColumnWidth := Controller.CalculateValue(Controller.ColumnWidth, Controller.ColumnWidthUnitType).Value;
    UpdateColumnTab;
  end;
end;

procedure TdxRichEditTablePropertiesDialogForm.CellVerticalAlignmentSelectedIndexChanged(Sender: TObject);
begin
  Controller.CellVerticalAlignment := CellVerticalAlignment;
end;

procedure TdxRichEditTablePropertiesDialogForm.ColumnSizeControlChanged(Sender: TObject);
begin
  Controller.UseDefaultColumnWidth := FColumnSizeControl.UseDefaultValue;
  if not FColumnSizeControl.Width.IsNull then
    Controller.ColumnWidth := FColumnSizeControl.Width.Value;
  Controller.ColumnWidthUnitType := FColumnSizeControl.WidthType.Value;

  FColumnSizeControl.MaxValue := Controller.GetColumnWidthMaxValueConsiderWidthUnitType;
  if FCellSizeControl.WidthType <> FColumnSizeControl.WidthType then
  begin
    Controller.CellWidthUnitType := FColumnSizeControl.WidthType.Value;
    Controller.CellWidth := Controller.CalculateValue(Controller.CellWidth, Controller.CellWidthUnitType).Value;
    UpdateCellTab;
  end;
end;

function TdxRichEditTablePropertiesDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxTablePropertiesFormController.Create(AControllerParameters as TdxTablePropertiesFormControllerParameters);
end;

destructor TdxRichEditTablePropertiesDialogForm.Destroy;
begin
  FreeAndNil(FTableSizeControl);
  FreeAndNil(FColumnSizeControl);
  FreeAndNil(FCellSizeControl);
  FreeAndNil(FRowHeightControl);
  inherited Destroy;
end;

procedure TdxRichEditTablePropertiesDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  function IsValidEdit(AEdit: TcxCustomEdit): Boolean;
  begin
    if AEdit.Enabled then
      Result := AEdit.ValidateEdit(True)
    else
      Result := True;
  end;

begin
  if ModalResult <> mrOk then
    Exit;
  CanClose := IsValidEdit(seTablePreferredWidth) and IsValidEdit(seIndentFromLeft) and
    IsValidEdit(seHeight) and IsValidEdit(seColumnPreferredWidth) and IsValidEdit(seCellPreferredWidth);
  if CanClose then
    Controller.ApplyChanges;
end;

function TdxRichEditTablePropertiesDialogForm.GetCellVerticalAlignment: TdxNullableValue<TdxVerticalAlignment>;
begin
  if rbCellVerticalAlignmentTop.Checked then
    Result := TdxVerticalAlignment.Top
  else
    if rbCellVerticalAlignmentCenter.Checked then
      Result := TdxVerticalAlignment.Center
    else
      if rbCellVerticalAlignmentBottom.Checked then
        Result := TdxVerticalAlignment.Bottom
      else
        Result := TdxNullableValue<TdxVerticalAlignment>.Null;
end;

function TdxRichEditTablePropertiesDialogForm.GetController: TdxTablePropertiesFormController;
begin
  Result := TdxTablePropertiesFormController(inherited Controller);
end;

function TdxRichEditTablePropertiesDialogForm.GetTableAlignment: TdxNullableValue<TdxTableRowAlignment>;
begin
  if rgTableAlignmenCenter.Checked then
    Result := TdxTableRowAlignment.Center
  else
    if rgTableAlignmentLeft.Checked then
      Result := TdxTableRowAlignment.Left
    else
      if rgTableAlignmenRight.Checked then
        Result := TdxTableRowAlignment.Right
      else
        Result := TdxNullableValue<TdxTableRowAlignment>.Null;
end;

procedure TdxRichEditTablePropertiesDialogForm.IndentFromLeftValueChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(seIndentFromLeft);
  if not VarIsNull(AValue) then
    Controller.TableIndent := AValue;
end;

procedure TdxRichEditTablePropertiesDialogForm.InitializeForm;
var
  AMeasurementType: TdxMeasurementType;
begin
  AMeasurementType := ToMeasurementType(UnitType);
  InitializeMeasurementUnitEdit(seIndentFromLeft, AMeasurementType,
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(TdxRichEditTablePropertiesHelper.MinTableIndentByDefault), ModelUnitsToUIUnit(TdxRichEditTablePropertiesHelper.MaxTableIndentByDefault)));

  FTableSizeControl := TdxTableSizeHelper.Create(Self, cbTablePreferredWidth,
    seTablePreferredWidth, cmbTableWidthType);
  FTableSizeControl.OnSetValueToEditor := SetValueToEditor;
  FTableSizeControl.OnGetValueFromEditor := GetValueFromEditor;
  FTableSizeControl.UnitType := UnitType;
  FTableSizeControl.MinValue := TdxRichEditTablePropertiesHelper.MinTableWidthByDefault;
  FTableSizeControl.MaxValue := TdxRichEditTablePropertiesHelper.MaxTableWidthInModelUnitsByDefault;
  FTableSizeControl.MaxPercentValue := TdxRichEditTablePropertiesHelper.MaxTableWidthInPercentByDefault;
  FTableSizeControl.RefreshMeasurementUnit;

  FRowHeightControl := TdxTableRowHeightHelper.Create(Self, cbSpecifyHeight,
    seHeight, cmbRowHeightType);
  FRowHeightControl.OnSetValueToEditor := SetValueToEditor;
  FRowHeightControl.OnGetValueFromEditor := GetValueFromEditor;
  FRowHeightControl.UnitType := UnitType;
  FRowHeightControl.MinValue := TdxRichEditTablePropertiesHelper.MinRowHeightByDefault;
  FRowHeightControl.MaxValue := TdxRichEditTablePropertiesHelper.MaxRowHeightByDefault;
  FRowHeightControl.RefreshMeasurementUnit;

  FColumnSizeControl := TdxTableSizeHelper.Create(Self, cbColumnPreferredWidth,
    seColumnPreferredWidth, cmbColumnWidthType);
  FColumnSizeControl.OnSetValueToEditor := SetValueToEditor;
  FColumnSizeControl.OnGetValueFromEditor := GetValueFromEditor;
  FColumnSizeControl.UnitType := UnitType;
  FColumnSizeControl.MinValue := TdxRichEditTablePropertiesHelper.MinColumnWidthByDefault;
  FColumnSizeControl.MaxValue := TdxRichEditTablePropertiesHelper.MaxColumnWidthInModelUnitsByDefault;
  FColumnSizeControl.MaxPercentValue := TdxRichEditTablePropertiesHelper.MaxColumnWidthInPercentByDefault;
  FColumnSizeControl.RefreshMeasurementUnit;

  FCellSizeControl := TdxTableSizeHelper.Create(Self, cbCellPreferredWidth,
    seCellPreferredWidth, cmbCellWidthType);
  FCellSizeControl.OnSetValueToEditor := SetValueToEditor;
  FCellSizeControl.OnGetValueFromEditor := GetValueFromEditor;
  FCellSizeControl.UnitType := UnitType;
  FCellSizeControl.MinValue := TdxRichEditTablePropertiesHelper.MinCellWidthByDefault;
  FCellSizeControl.MaxValue := TdxRichEditTablePropertiesHelper.MaxCellWidthInModelUnitsByDefault;
  FCellSizeControl.MaxPercentValue := TdxRichEditTablePropertiesHelper.MaxCellWidthInPercentByDefault;
  FCellSizeControl.RefreshMeasurementUnit;
end;

procedure TdxRichEditTablePropertiesDialogForm.SetCellVerticalAlignment(
  const AValue: TdxNullableValue<TdxVerticalAlignment>);
begin
  if AValue.IsNull then
  begin
    rbCellVerticalAlignmentTop.Checked := False;
    rbCellVerticalAlignmentCenter.Checked := False;
    rbCellVerticalAlignmentBottom.Checked := False;
  end
  else
    case AValue.Value of
      TdxVerticalAlignment.Top: rbCellVerticalAlignmentTop.Checked := True;
      TdxVerticalAlignment.Center: rbCellVerticalAlignmentCenter.Checked := True;
      TdxVerticalAlignment.Bottom: rbCellVerticalAlignmentBottom.Checked := True;
    end;
end;

procedure TdxRichEditTablePropertiesDialogForm.SetTableAlignment(const AValue: TdxNullableValue<TdxTableRowAlignment>);
begin
  if AValue.IsNull then
  begin
    rgTableAlignmentLeft.Checked := False;
    rgTableAlignmenCenter.Checked := False;
    rgTableAlignmenRight.Checked := False;
  end
  else
    case AValue.Value of
      TdxTableRowAlignment.Center: rgTableAlignmenCenter.Checked := True;
      TdxTableRowAlignment.Left: rgTableAlignmentLeft.Checked := True;
      TdxTableRowAlignment.Right: rgTableAlignmenRight.Checked := True;
    end;
end;

procedure TdxRichEditTablePropertiesDialogForm.ShowBorderShadingForm(ASelectedCells: TdxSelectedCellsCollection);
var
  AForm: TdxRichEditBorderShadingDialogForm;
  AControllerParameters: TdxBorderShadingFormControllerParameters;
begin
  AControllerParameters := TdxBorderShadingFormControllerParameters.Create(Control, TdxDocumentModel(Controller.Table.DocumentModel),
    ASelectedCells);
  try
    AForm := TdxRichEditBorderShadingDialogForm.Create(Self);
    try
      AForm.Initialize(AControllerParameters);
      AForm.ShowModal;
    finally
      AForm.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxRichEditTablePropertiesDialogForm.ShowCellOptionsForm;
var
  AForm: TdxRichEditTableCellOptionsDialogForm;
  AControllerParameters: TdxTableCellOptionsFormControllerParameters;
begin
  AControllerParameters := TdxTableCellOptionsFormControllerParameters.Create(Control, Controller.SelectedCells);
  try
    AForm := TdxRichEditTableCellOptionsDialogForm.Create(Self);
    try
      AForm.Initialize(AControllerParameters);
      AForm.ShowModal;
    finally
      AForm.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxRichEditTablePropertiesDialogForm.ShowTableOptionsForm;
begin
  Control.ShowTableOptionsForm(Controller.Table);
end;

procedure TdxRichEditTablePropertiesDialogForm.SubscribeControlsEvents;
begin
  FTableSizeControl.OnControlChanged := TableSizeControlChanged;
  seIndentFromLeft.Properties.OnChange := IndentFromLeftValueChanged;
  rgTableAlignmentLeft.OnClick := TableAlignmentSelectedIndexChanged;
  rgTableAlignmenCenter.OnClick := TableAlignmentSelectedIndexChanged;
  rgTableAlignmenRight.OnClick := TableAlignmentSelectedIndexChanged;

  FRowHeightControl.OnControlChanged := TableRowHeightControlChanged;
  FColumnSizeControl.OnControlChanged := ColumnSizeControlChanged;

  FCellSizeControl.OnControlChanged := CellSizeControlChanged;
  rbCellVerticalAlignmentTop.OnClick := CellVerticalAlignmentSelectedIndexChanged;
  rbCellVerticalAlignmentCenter.OnClick := CellVerticalAlignmentSelectedIndexChanged;
  rbCellVerticalAlignmentBottom.OnClick := CellVerticalAlignmentSelectedIndexChanged;
end;

procedure TdxRichEditTablePropertiesDialogForm.TableAlignmentSelectedIndexChanged(Sender: TObject);
begin
  Controller.TableAlignment := TableAlignment;
  UpdateEnabledControls;
end;

procedure TdxRichEditTablePropertiesDialogForm.TableRowHeightControlChanged(Sender: TObject);
begin
  Controller.UseDefaultRowHeight := FRowHeightControl.UseDefaultValue;
  if not FRowHeightControl.Height.IsNull then
    Controller.RowHeight := FRowHeightControl.Height.Value;
  Controller.RowHeightType := FRowHeightControl.ValueType.Value;
end;

procedure TdxRichEditTablePropertiesDialogForm.TableSizeControlChanged(Sender: TObject);
begin
  Controller.UseDefaultTableWidth := FTableSizeControl.UseDefaultValue.Value;
  if not FTableSizeControl.Width.IsNull then
    Controller.TableWidth := FTableSizeControl.Width.Value;
  Controller.TableWidthUnitType := FTableSizeControl.WidthType.Value;
  FTableSizeControl.MaxValue := Controller.GetTableWidthMaxValueConsiderWidthUnitType;
end;

procedure TdxRichEditTablePropertiesDialogForm.UnsubscribeControlsEvents;
begin
  FTableSizeControl.OnControlChanged := nil;
  seIndentFromLeft.Properties.OnChange := nil;
  rgTableAlignmentLeft.OnClick := nil;
  rgTableAlignmenCenter.OnClick := nil;
  rgTableAlignmenRight.OnClick := nil;

  FRowHeightControl.OnControlChanged := nil;
  FColumnSizeControl.OnControlChanged := nil;

  FCellSizeControl.OnControlChanged := nil;
  rbCellVerticalAlignmentTop.OnClick := nil;
  rbCellVerticalAlignmentCenter.OnClick := nil;
  rbCellVerticalAlignmentBottom.OnClick := nil;
end;

procedure TdxRichEditTablePropertiesDialogForm.UpdateCellTab;
begin
  FCellSizeControl.BeginUpdate;
  try
    FCellSizeControl.UseDefaultValue := Controller.UseDefaultCellWidth;
    FCellSizeControl.Width := Controller.CellWidth;
    FCellSizeControl.WidthType := Controller.CellWidthUnitType;
    FCellSizeControl.MaxValue := Controller.GetCellWidthMaxValueConsiderWidthUnitType;
  finally
    FCellSizeControl.EndUpdate;
  end;
  CellVerticalAlignment := Controller.CellVerticalAlignment;
end;

class procedure TdxRichEditTablePropertiesDialogForm.UpdateCheckBox(ACheckBox: TcxCheckBox;
  const ACheck: TdxNullableBoolean);
begin
  if ACheck.IsNull then
  begin
    ACheckBox.Properties.AllowGrayed := True;
    ACheckBox.State := cbsGrayed;
  end
  else
    if ACheck.Value then
      ACheckBox.State := cbsChecked
    else
      ACheckBox.State := cbsUnchecked;
end;

procedure TdxRichEditTablePropertiesDialogForm.UpdateColumnTab;
begin
  FColumnSizeControl.BeginUpdate;
  try
    FColumnSizeControl.UseDefaultValue := Controller.UseDefaultColumnWidth;
    FColumnSizeControl.Width := Controller.ColumnWidth;
    FColumnSizeControl.WidthType := Controller.ColumnWidthUnitType;
    FColumnSizeControl.MaxValue := Controller.GetColumnWidthMaxValueConsiderWidthUnitType;
  finally
    FColumnSizeControl.EndUpdate;
  end;
end;

procedure TdxRichEditTablePropertiesDialogForm.UpdateEnabledControls;
begin
  seIndentFromLeft.Enabled := Controller.TableAlignment = TdxTableRowAlignment.Left;
end;

procedure TdxRichEditTablePropertiesDialogForm.UpdateFormCore;
begin
  UpdateTableTab;
  UpdateRowTab;
  UpdateColumnTab;
  UpdateCellTab;
end;

procedure TdxRichEditTablePropertiesDialogForm.UpdateRowTab;
var
  ANotCanSplit: TdxNullableBoolean;
begin
  FRowHeightControl.BeginUpdate;
  try
    FRowHeightControl.UseDefaultValue := Controller.UseDefaultRowHeight;
    FRowHeightControl.Height := Controller.RowHeight;
    FRowHeightControl.ValueType := Controller.RowHeightType;
  finally
    FRowHeightControl.EndUpdate;
  end;
  ANotCanSplit := not Controller.RowCantSplit;
  UpdateCheckBox(cbCantSplit, ANotCanSplit);
  UpdateCheckBox(cbHeader, Controller.RowHeader);
  cbHeader.Enabled := Controller.IsSelectedFirstRowInTable;
end;

procedure TdxRichEditTablePropertiesDialogForm.UpdateTableTab;
begin
  FTableSizeControl.BeginUpdate();
  try
    FTableSizeControl.UseDefaultValue := Controller.UseDefaultTableWidth;
    FTableSizeControl.Width := Controller.TableWidth;
    FTableSizeControl.WidthType := Controller.TableWidthUnitType;
    FTableSizeControl.MaxValue := Controller.GetTableWidthMaxValueConsiderWidthUnitType;
  finally
    FTableSizeControl.EndUpdate();
  end;
  UpdateEnabledControls;

  SetValueToEditor(seIndentFromLeft, Controller.TableIndent);
  TableAlignment := Controller.TableAlignment;
end;

{ TdxCustomRichEditTablePropertiesSizeHelper }

procedure TdxCustomTableControlHelper<T>.AddItem(const ADisplayName: string; const AValue: T);
begin
  FComboBox.Properties.Items.Add(ADisplayName);
  FValueTypes.Add(AValue);
end;

procedure TdxCustomTableControlHelper<T>.BeginUpdate;
begin
  if FLockCount = 0 then
    FDefferedUpdateControl := False;
  Inc(FLockCount);
end;

procedure TdxCustomTableControlHelper<T>.CheckBoxClick(Sender: TObject);
var
  ACheck: TdxNullableBoolean;
  AEnabledControls: Boolean;
begin
  ACheck := CheckStateToNullableBool(CheckBox.State);
  UseDefaultValue := not ACheck;
  AEnabledControls := not ACheck.IsNull and ACheck.Value;
  UpdateEnabledControls(AEnabledControls);
end;

function TdxCustomTableControlHelper<T>.CheckStateToNullableBool(ACheckState: TcxCheckBoxState): TdxNullableBoolean;
begin
  case ACheckState of
    cbsUnchecked:
      Result := False;
    cbsChecked:
      Result := True;
  else
    Result := TdxNullableBoolean.Null;
  end;
end;

function TdxCustomTableControlHelper<T>.CheckVariantToNullableInteger(AValue: Variant): TdxNullableInteger;
begin
  if VarIsNull(AValue) then
    Result := TdxNullableInteger.Null
  else
    Result := AValue;
end;

constructor TdxCustomTableControlHelper<T>.Create(AOwner: TdxRichEditTablePropertiesDialogForm; const ACheckBox: TcxCheckBox;
  const AEditor: TdxMeasurementUnitEdit; const AComboBox: TcxComboBox);
begin
  inherited Create;
  FOwner := AOwner;
  FValueTypes := TdxOrdinalList<T>.Create;
  FCheckBox := ACheckBox;
  FEditor := AEditor;
  FComboBox := AComboBox;

  InternalPopulateComboBox;
  SubscribeEvents;
  UpdateControl;
end;

destructor TdxCustomTableControlHelper<T>.Destroy;
begin
  FValueTypes.Free;
  inherited;
end;

procedure TdxCustomTableControlHelper<T>.DoControlChanged;
begin
  if Assigned(FOnControlChanged) then
    FOnControlChanged(Self);
end;

procedure TdxCustomTableControlHelper<T>.DoPropertiesChanged;
begin
  if IsUpdateLocked then
    DefferedUpdateControl := True
  else
  begin
    UpdateControl;
    DoControlChanged;
  end;
end;

procedure TdxCustomTableControlHelper<T>.EditorValueChange(Sender: TObject);
begin
  if Assigned(FOnGetValueFromEditor) then
    Value := CheckVariantToNullableInteger(FOnGetValueFromEditor(Editor));
end;

procedure TdxCustomTableControlHelper<T>.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and FDefferedUpdateControl then
    DoPropertiesChanged;
end;

function TdxCustomTableControlHelper<T>.GetController: TdxTablePropertiesFormController;
begin
  Result := FOwner.Controller;
end;

function TdxCustomTableControlHelper<T>.GetIsUpdateLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TdxCustomTableControlHelper<T>.GetValueType: TdxNullableValue<T>;
begin
  if FComboBox.ItemIndex >= 0 then
    Result := FValueTypes[FComboBox.ItemIndex]
  else
    Result.Reset;
end;

procedure TdxCustomTableControlHelper<T>.InternalPopulateComboBox;
begin
  FComboBox.Properties.Items.BeginUpdate;
  try
    FComboBox.Properties.Items.Clear;
    PopulateComboBox;
  finally
    FComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxCustomTableControlHelper<T>.PropertiesChanged(Sender: TObject);
begin
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.RefreshMeasurementUnit;
begin
  FOwner.InitializeMeasurementUnitEdit(FEditor, FMeasurementType,
    TdxMeasurementUnitEditHelper.Create(FOwner.UnitTypeDescription, 0.1, 2,
      FOwner.ModelUnitsToUIUnit(MinValue),
      FOwner.ModelUnitsToUIUnit(MaxValue)));
end;

procedure TdxCustomTableControlHelper<T>.SetMaxValue(const AValue: Integer);
var
  AMeasurementUnitEditHelper: TdxMeasurementUnitEditHelper;
begin
  if FMaxValue = AValue then
    Exit;
  FMaxValue := AValue;
  AMeasurementUnitEditHelper := FOwner.GetMeasurementUnitEditHelper(FEditor);
  if Assigned(AMeasurementUnitEditHelper) then
    AMeasurementUnitEditHelper.MaxValue := AValue;
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.SetMinValue(const AValue: Integer);
var
  AMeasurementUnitEditHelper: TdxMeasurementUnitEditHelper;
begin
  if FMinValue = AValue then
    Exit;
  FMinValue := AValue;
  if Assigned(AMeasurementUnitEditHelper) then
    AMeasurementUnitEditHelper.MinValue := AValue;
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.SetValueToEditor(const AValue: TdxNullableInteger);
begin
  if Assigned(FOnSetValueToEditor) then
    if AValue.IsNull then
      FOnSetValueToEditor(FEditor, Null)
    else
      FOnSetValueToEditor(FEditor, AValue.Value);
end;

procedure TdxCustomTableControlHelper<T>.SetValueType(const AValue: TdxNullableValue<T>);
{$IFNDEF DELPHIXE3}
var
  V: TdxNullableValue<T>;
{$ENDIF}
begin
{$IFDEF DELPHIXE3}
  if ValueType = AValue then
    Exit;
{$ELSE}
  V := AValue;
  if ValueType = V then
    Exit;
{$ENDIF}
  if AValue.IsNull then
    FComboBox.ItemIndex := -1
  else
    FComboBox.ItemIndex := FValueTypes.IndexOf(AValue.Value);
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.SetUnitType(const AValue: TdxMeasurementUnit);
begin
  if FUnitType = AValue then
    Exit;
  FUnitType := AValue;
  FMeasurementType := TdxRichEditTablePropertiesDialogForm.ToMeasurementType(AValue);
  InternalPopulateComboBox;
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.SetUseDefaultValue(const AValue: TdxNullableBoolean);
begin
  if FUseDefaultValue = AValue then
    Exit;
  FUseDefaultValue := AValue;
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.SetValue(const AValue: TdxNullableInteger);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  DoPropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.SubscribeEvents;
begin
  FCheckBox.Properties.OnChange := CheckBoxClick;
  FEditor.Properties.OnEditValueChanged := EditorValueChange;
  FComboBox.Properties.OnChange := ComboBoxValueChange;
  OnPropertiesChanged := PropertiesChanged;
end;

procedure TdxCustomTableControlHelper<T>.UnsubscribeEvents;
begin
  FCheckBox.Properties.OnChange := nil;
  FEditor.Properties.OnChange := nil;
  FComboBox.Properties.OnChange := nil;
  OnPropertiesChanged := nil;
end;

procedure TdxCustomTableControlHelper<T>.UpdateCheckox;
var
  ACheck: TdxNullableBoolean;
begin
  ACheck := UseDefaultValue;
  if ACheck.IsNull then
  begin
    CheckBox.Properties.AllowGrayed := True;
    CheckBox.State := cbsGrayed;
  end
  else
  begin
    CheckBox.Properties.AllowGrayed := False;
    CheckBox.Checked := not ACheck.Value;
  end;
end;

procedure TdxCustomTableControlHelper<T>.UpdateEnabledControls(AEnabled: Boolean);
begin
  FComboBox.Enabled := AEnabled;
  FEditor.Enabled := AEnabled;
end;

{ TdxRichEditTablePropertiesDialogTableSizeHelper }

procedure TdxTableSizeHelper.SetIsValueInPercent(const Value: Boolean);
begin
  FIsValueInPercent := Value;
end;

procedure TdxTableSizeHelper.SetMaxPercentValue(const Value: Integer);
begin
  FMaxPercentValue := Value;
end;

procedure TdxTableSizeHelper.SetWidth(const AValue: TdxNullableInteger);
begin
  Value := AValue;
end;

procedure TdxTableSizeHelper.SetWidthType(const Value: TdxNullableValue<TdxWidthUnitType>);
begin
  if ValueType = Value then
    Exit;
  inherited ValueType := Value;
  RefreshMeasurementUnit;
end;

procedure TdxTableSizeHelper.UpdateControl;
var
  AEnabledControls: Boolean;
  AWidthType: TdxNullableValue<TdxWidthUnitType>;
  AActualWidhtUnitType: TdxWidthUnitType;
begin
  UnsubscribeEvents;
  try
    AEnabledControls := not UseDefaultValue.IsNull and  not UseDefaultValue.Value;
    UpdateEnabledControls(AEnabledControls);
    UpdateCheckox;

    AWidthType := ValueType;
    if not AWidthType.IsNull and (AWidthType.Value = TdxWidthUnitType.FiftiethsOfPercent) then
      AActualWidhtUnitType := AWidthType.Value
    else
      AActualWidhtUnitType := TdxWidthUnitType.ModelUnits;
    IsValueInPercent := AActualWidhtUnitType = TdxWidthUnitType.FiftiethsOfPercent;
    SetValueToEditor(Width);
    ValueType := AActualWidhtUnitType;
  finally
    SubscribeEvents;
  end;
end;

function TdxTableSizeHelper.CalculateActualWidth: TdxNullableInteger;
begin
  Result := Controller.CalculateValue(Width, WidthType);
end;

procedure TdxTableSizeHelper.ComboBoxValueChange(Sender: TObject);
begin
  if IsUpdateLocked then
    Exit;
  BeginUpdate;
  try
    RefreshMeasurementUnit;
    Width := CalculateActualWidth;
  finally
    EndUpdate;
  end;
end;

function TdxTableSizeHelper.GetCheckBox: TcxCheckBox;
begin
  Result := inherited CheckBox;
end;

function TdxTableSizeHelper.GetComboBox: TcxComboBox;
begin
  Result := inherited ComboBox;
end;

function TdxTableSizeHelper.GetEditor: TdxMeasurementUnitEdit;
begin
  Result := inherited Editor;
end;

function TdxTableSizeHelper.GetWidth: TdxNullableInteger;
begin
  Result := Value;
end;

function TdxTableSizeHelper.GetWidthType: TdxNullableValue<TdxWidthUnitType>;
begin
  Result := inherited ValueType;
end;

procedure TdxTableSizeHelper.PopulateComboBox;
var
  S: string;
begin
  AddItem(cxGetResourceString(@sdxRichEditCaptionUnitPercent), TdxWidthUnitType.FiftiethsOfPercent);
  case UnitType of
    TdxMeasurementUnit.Inch:
      S := cxGetResourceString(@sdxRichEditCaptionUnitInches);
    TdxMeasurementUnit.Millimeter:
      S := cxGetResourceString(@sdxRichEditCaptionUnitMillimeters);
    TdxMeasurementUnit.Centimeter:
      S := cxGetResourceString(@sdxRichEditCaptionUnitCentimeters);
    TdxMeasurementUnit.Point:
      S := cxGetResourceString(@sdxRichEditCaptionUnitPoints);
    else
      S := '';
  end;
  AddItem(S, TdxWidthUnitType.ModelUnits);
end;


procedure TdxTableSizeHelper.RefreshMeasurementUnit;
var
  AWidthUnitType: TdxNullableValue<TdxWidthUnitType>;
begin
  AWidthUnitType := ValueType;
  if AWidthUnitType.IsNull then
    Exit;
  if AWidthUnitType.Value = TdxWidthUnitType.FiftiethsOfPercent then
    FOwner.InitializeMeasurementUnitEdit(FEditor, TdxMeasurementType.mtCustom,
      TdxMeasurementUnitEditHelper.Create(' %', 1.0, 2, MinValue, MaxPercentValue))
  else
    inherited RefreshMeasurementUnit;
end;

{ TdxRichEditTablePropertiesDialogTableRowHeightHelper }

procedure TdxTableRowHeightHelper.ComboBoxValueChange(Sender: TObject);
begin
end;

function TdxTableRowHeightHelper.GetCheckBox: TcxCheckBox;
begin
  Result := inherited CheckBox;
end;

function TdxTableRowHeightHelper.GetComboBox: TcxComboBox;
begin
  Result := inherited ComboBox;
end;

function TdxTableRowHeightHelper.GetEditor: TdxMeasurementUnitEdit;
begin
  Result := inherited Editor;
end;

function TdxTableRowHeightHelper.GetHeight: TdxNullableInteger;
begin
  Result := Value;
end;

procedure TdxTableRowHeightHelper.PopulateComboBox;
begin
  AddItem(cxGetResourceString(@sdxRichEditTablePropertiesHeightTypeExact), TdxHeightUnitType.Exact);
  AddItem(cxGetResourceString(@sdxRichEditTablePropertiesHeightTypeMinimum), TdxHeightUnitType.Minimum);
end;

procedure TdxTableRowHeightHelper.SetHeight(const AValue: TdxNullableInteger);
begin
  if Value = AValue then
    Exit;
  Value := AValue;
  DoPropertiesChanged;
end;

procedure TdxTableRowHeightHelper.UpdateControl;
var
  AHeightType: TdxNullableValue<TdxHeightUnitType>;
  AEnabledControls: Boolean;
begin
  UnsubscribeEvents;
  try
    AEnabledControls := not UseDefaultValue.IsNull and not UseDefaultValue.Value;
    UpdateEnabledControls(AEnabledControls);
    UpdateCheckox;

    SetValueToEditor(Height);
    AHeightType := ValueType;
    if not AHeightType.IsNull and (AHeightType.Value = TdxHeightUnitType.Exact) then
      ValueType := AHeightType.Value
    else
      ValueType := TdxHeightUnitType.Minimum;
  finally
    SubscribeEvents;
  end;
end;

end.
