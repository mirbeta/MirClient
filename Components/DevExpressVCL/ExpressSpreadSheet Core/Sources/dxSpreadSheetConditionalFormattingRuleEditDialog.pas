{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetConditionalFormattingRuleEditDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, ImgList,
  dxCore, cxGraphics, dxForms, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  cxButtons, cxContainer, cxLabel, dxSpreadSheetTypes,
  cxCustomData, cxStyles, cxTL, cxInplaceContainer, cxMaskEdit, cxDropDownEdit, cxTextEdit, cxEdit,
  cxRadioGroup, cxCheckBox, cxSpinEdit, cxColorComboBox, cxImageComboBox, cxEditRepositoryItems,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetConditionalFormattingRuleEditDialogHelpers, dxSpreadSheetStyles,
  dxSpreadSheetClasses, dxThreading;

type

  { TdxSpreadSheetConditionalFormattingRuleEditDialogForm }

  TfrmSpreadSheetConditionalFormattingRuleEditDialog = class(TdxForm)
    btnCancel: TcxButton;
    btnFormat: TcxButton;
    btnOk: TcxButton;
    cbbAverageComparisonOperator: TcxComboBox;
    cbbOperator: TcxComboBox;
    cbbScaleMaxStopValueType: TcxComboBox;
    cbbScaleMidStopValueType: TcxComboBox;
    cbbScaleMinStopValueType: TcxComboBox;
    cbbTopBottomValues: TcxComboBox;
    cbReverseIconOrder: TcxCheckBox;
    cbShowBarOnly: TcxCheckBox;
    cbShowIconOnly: TcxCheckBox;
    cbTopBottomValuesPercents: TcxCheckBox;
    ccbColorScaleMaxStopColor: TcxColorComboBox;
    ccbColorScaleMidStopColor: TcxColorComboBox;
    ccbColorScaleMinStopColor: TcxColorComboBox;
    dxLayoutcxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    EditRepository: TcxEditRepository;
    EditRepositoryFloatMask: TcxEditRepositoryMaskItem;
    EditRepositoryPercentsMask: TcxEditRepositoryMaskItem;
    icbIconStyle: TcxImageComboBox;
    lcgIconSet: TdxLayoutGroup;
    lcgPreview: TdxLayoutGroup;
    lcgRuleExpression: TdxLayoutAutoCreatedGroup;
    lcgScale: TdxLayoutAutoCreatedGroup;
    lcgScaleMaxStop: TdxLayoutGroup;
    lcgScaleMidStop: TdxLayoutGroup;
    lcgScaleMinStop: TdxLayoutGroup;
    lcgTopBottomValues: TdxLayoutAutoCreatedGroup;
    lciAboveOrBelowAverage: TdxLayoutItem;
    lciColorScaleMaxStopColor: TdxLayoutItem;
    lciColorScaleMidStopColor: TdxLayoutItem;
    lciColorScaleMinStopColor: TdxLayoutItem;
    lciExpression1: TdxLayoutItem;
    lciExpression2: TdxLayoutItem;
    lciFormatButton: TdxLayoutItem;
    lciIconStyle: TdxLayoutItem;
    lciOperators: TdxLayoutItem;
    lciPreview: TdxLayoutItem;
    lciRuleType: TdxLayoutItem;
    lciShowBarOnly: TdxLayoutItem;
    lclPreview: TdxLayoutLabeledItem;
    lclRuleDescription: TdxLayoutLabeledItem;
    lclScaleMaxStop: TdxLayoutLabeledItem;
    lclScaleMidStop: TdxLayoutLabeledItem;
    lclScaleMinStop: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutAutoCreatedGroup;
    lcMainGroup2: TdxLayoutAutoCreatedGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutAutoCreatedGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem10: TdxLayoutItem;
    lcMainItem11: TdxLayoutItem;
    lcMainItem13: TdxLayoutItem;
    lcMainItem14: TdxLayoutItem;
    lcMainItem15: TdxLayoutItem;
    lcMainItem16: TdxLayoutItem;
    lcMainItem17: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    meScaleMaxStopValue: TcxMaskEdit;
    meScaleMidStopValue: TcxMaskEdit;
    meScaleMinStopValue: TcxMaskEdit;
    pbPreview: TPaintBox;
    seTopBottomValuesRank: TcxSpinEdit;
    teExpression1: TcxMaskEdit;
    teExpression2: TcxMaskEdit;
    tlIconSet: TcxTreeList;
    tlIconSetColumnComparisonOperation: TcxTreeListColumn;
    tlIconSetColumnIcon: TcxTreeListColumn;
    tlIconSetColumnInfo: TcxTreeListColumn;
    tlIconSetColumnStopValue: TcxTreeListColumn;
    tlIconSetColumnStopValueType: TcxTreeListColumn;
    tlRuleType: TcxTreeList;
    tlRuleTypeColumn1: TcxTreeListColumn;

    procedure btnFormatClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbbScaleStopValueTypePropertiesChange(Sender: TObject);
    procedure cbReverseIconOrderChanged(Sender: TObject);
    procedure cbShowBarOnlyChanged(Sender: TObject);
    procedure cbShowIconOnlyChanged(Sender: TObject);
    procedure ccbColorScaleStopColorPropertiesChange(Sender: TObject);
    procedure ccbOperatorPropertiesChange(Sender: TObject);
    procedure icbIconStyleChanged(Sender: TObject);
    procedure meScaleStopValueChanged(Sender: TObject);
    procedure pbPreviewPaint(Sender: TObject);
    procedure tlIconSetColumnStopValueGetEditingProperties(Sender: TcxTreeListColumn;
      ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    procedure tlIconSetColumnComparisonOperationChanged(Sender: TObject);
    procedure tlIconSetColumnGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
    procedure tlIconSetColumnIconChanged(Sender: TObject);
    procedure tlIconSetColumnStopValueTypeChanged(Sender: TObject);
    procedure tlIconSetEdited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
    procedure tlIconSetEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
    procedure tlRuleTypeFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
  strict private
    FHelper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper;
    FHelperIconSet: TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper;
    FHelperScale: TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper;
    FIsLoading: Boolean;
    FIsSelecting: Boolean;
    FOwner: IdxDialogOwner;

    procedure DoRuleTypeSelected(ANode: TcxTreeListNode);
    function GetEditMask(AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType): TcxEditRepositoryItem;

    function CanShowFormatDialog: Boolean;
    procedure ShowFormatCellDialog(AStyle: TdxSpreadSheetCellStyle);

    procedure RestoreRuleInfo;
    procedure RestoreRuleAboveOrBelowAverage(ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    procedure RestoreRuleCellIsInfo(ARule: TdxSpreadSheetConditionalFormattingRuleCellIs);
    procedure RestoreRuleExpressionInfo(ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
    procedure RestoreRuleIconSet(ARule: TdxSpreadSheetConditionalFormattingRuleIconSet);
    procedure RestoreRuleScaleInfo(ARule: TdxSpreadSheetConditionalFormattingRuleCustomScale);
    procedure RestoreRuleTopBottomInfo(ARule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues);

    procedure StoreRuleInfo;
    procedure StoreRuleAboveOrBelowAverage(ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    procedure StoreRuleCellIsInfo(ARule: TdxSpreadSheetConditionalFormattingRuleCellIs);
    procedure StoreRuleExpressionInfo(ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
    procedure StoreRuleTopBottomInfo(ARule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
  protected
    procedure ApplyLocalizations;
    procedure Initialize(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting; ALookAndFeel: TcxLookAndFeel);
    procedure SelectRuleType(ANode: TcxTreeListNode);
    procedure UpdateRuleDescription;
    //
    procedure Load(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    procedure Save(var ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    //
    property Helper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper read FHelper;
    property HelperIconSet: TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper read FHelperIconSet;
    property HelperScale: TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper read FHelperScale;
  public
    constructor Create(AOwner: IdxDialogOwner); reintroduce;
    destructor Destroy; override;
  end;

function ShowConditionalFormattingRuleEditDialog(AOwner: IdxDialogOwner;
  var ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean; overload;
function ShowConditionalFormattingRuleEditDialog(AOwner: TObject;
  var ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean; overload; // Special for CBuilder
implementation

uses
  Math, cxFormats,
  dxSpreadSheetCoreDialogsStrs,
  dxSpreadSheetCellStyleEditDialog,
  dxSpreadSheetCellStyleEditDialogController,
  dxSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog,
  dxSpreadSheetConditionalFormattingIconSet;

{$R *.dfm}

type
  TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess = class(TdxSpreadSheetConditionalFormattingRuleCustomColorScale);
  TdxSpreadSheetCustomConditionalFormattingAccess = class(TdxSpreadSheetCustomConditionalFormatting);

  { TdxSpreadSheetConditionalFormattingCellStyleEditDialogSingleStyleController }

  TdxSpreadSheetConditionalFormattingCellStyleEditDialogSingleStyleController = class(TdxSpreadSheetCellStyleEditDialogSingleStyleController)
  public
    constructor Create(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting; AStyle: TdxSpreadSheetCellStyle); reintroduce;
  end;

function ShowConditionalFormattingRuleEditDialog(
  AOwner: TObject; var ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
var
  AOwnerIntf: IdxDialogOwner;
begin
  Result := Supports(AOwner, IdxDialogOwner, AOwnerIntf);
  Assert(Result);
  Result := Result and ShowConditionalFormattingRuleEditDialog(AOwnerIntf, ARule);
end;

function ShowConditionalFormattingRuleEditDialog(
  AOwner: IdxDialogOwner; var ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
var
  ADialog: TfrmSpreadSheetConditionalFormattingRuleEditDialog;
  AIntf: IdxSpreadSheetConditionalFormatting;
begin
  Result := False;
  if Supports(AOwner, IdxSpreadSheetConditionalFormatting, AIntf) then
  begin
    ADialog := TfrmSpreadSheetConditionalFormattingRuleEditDialog.Create(AOwner);
    try
      ADialog.Initialize(AIntf.GetConditionalFormatting, AOwner.GetLookAndFeel);
      if ARule <> nil then
        ADialog.Load(ARule);

      Result := ADialog.ShowModal = mrOk;
      if Result then
        ADialog.Save(ARule);
    finally
      ADialog.Free;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingCellStyleEditDialogSingleStyleController }

constructor TdxSpreadSheetConditionalFormattingCellStyleEditDialogSingleStyleController.Create(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting; AStyle: TdxSpreadSheetCellStyle);
begin
  inherited Create(AConditionalFormatting.CellStyles, AStyle);
  if not TdxSpreadSheetCustomConditionalFormattingAccess(AConditionalFormatting).IsStyleBorderSupported then
    Exclude(FCapabilities, csecBorder);
  if not TdxSpreadSheetCustomConditionalFormattingAccess(AConditionalFormatting).IsValueFormattingSupported then
    Exclude(FCapabilities, csecNumber);
end;

{ TdxSpreadSheetConditionalFormattingRuleEditDialogForm }

constructor TfrmSpreadSheetConditionalFormattingRuleEditDialog.Create(AOwner: IdxDialogOwner);
begin
  inherited Create(AOwner.GetParentForm);
  FOwner := AOwner;
end;

destructor TfrmSpreadSheetConditionalFormattingRuleEditDialog.Destroy;
begin
  TdxUIThreadSyncService.Unsubscribe(Self);
  FreeAndNil(FHelperIconSet);
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.ApplyLocalizations;
begin
  Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogCaption);

  lciExpression2.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogAnd);
  lciRuleType.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogRuleType);
  lclPreview.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogPreview);
  lciIconStyle.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogIconStyle);

  cbReverseIconOrder.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogReverseIconOrder);
  cbShowBarOnly.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogShowBarOnly);
  cbShowIconOnly.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogShowIconOnly);
  cbTopBottomValuesPercents.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogPercentsOfSelectedRange);

  lclScaleMinStop.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogScaleMinStop);
  lclScaleMidStop.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogScaleMidStop);
  lclScaleMaxStop.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogScaleMaxStop);

  btnOk.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogButtonCancel);
  btnFormat.Caption := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogButtonFormat);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.Initialize(
  AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
  ALookAndFeel: TcxLookAndFeel);
begin
  SetControlLookAndFeel(Self, ALookAndFeel);

  FHelper := TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.Create(AConditionalFormatting);
  FHelperIconSet := TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.Create(tlIconSet);
  EditRepositoryFloatMask.Properties.EditMask := '\-?\d+(\' + dxFormatSettings.DecimalSeparator + '\d{0,})?([eE][-+]?\d+)?';
  ApplyLocalizations;

  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateAboveOrBelowAverageOperators(cbbAverageComparisonOperator.Properties.Items);
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateCellIsOperators(cbbOperator.Properties.Items);
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateRuleTypes(tlRuleType, Helper);
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateTopBottomDirections(cbbTopBottomValues.Properties.Items);
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateScaleStopTypes(cbbScaleMinStopValueType);
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateScaleStopTypes(cbbScaleMidStopValueType);
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateScaleStopTypes(cbbScaleMaxStopValueType);

  HelperScale.BindEditors(cbbScaleMinStopValueType, meScaleMinStopValue, ccbColorScaleMinStopColor);
  HelperScale.BindEditors(cbbScaleMidStopValueType, meScaleMidStopValue, ccbColorScaleMidStopColor);
  HelperScale.BindEditors(cbbScaleMaxStopValueType, meScaleMaxStopValue, ccbColorScaleMaxStopColor);

  HelperIconSet.PopulateIconSets(icbIconStyle);
  HelperIconSet.PopulateComparisonOperators(tlIconSetColumnComparisonOperation.Properties as TcxComboBoxProperties);
  HelperIconSet.PopulateStopValueTypes(tlIconSetColumnStopValueType.Properties as TcxComboBoxProperties);
  HelperIconSet.PopulateIcons(tlIconSetColumnIcon.Properties as TcxImageComboBoxProperties);
  icbIconStyle.Properties.Images := ConditionalFormattingIconSet.PresetPreviews;

  teExpression1.Properties.EditMask := TdxSpreadSheetCustomConditionalFormattingAccess(Helper.ConditionalFormatting).GetFormulaEditMask;
  teExpression2.Properties.EditMask := teExpression1.Properties.EditMask;

  SelectRuleType(tlRuleType.Root.getFirstChild);
  RestoreRuleInfo;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.SelectRuleType(ANode: TcxTreeListNode);
begin
  FIsSelecting := True;
  try
    tlRuleType.FocusedNode := ANode;
    DoRuleTypeSelected(ANode);
  finally
    FIsSelecting := False;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.UpdateRuleDescription;
begin
  if Helper.SelectedRuleInfo <> nil then
    lclRuleDescription.Caption := Helper.SelectedRuleInfo.Description;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.Load(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
var
  ANode: TcxTreeListNode;
begin
  if TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.FindRuleType(tlRuleType, ARule.ClassType, ANode) then
  begin
    SelectRuleType(ANode);
    Helper.SelectedArea := ARule.Area;
    Helper.SelectedRulePreview.Assign(ARule);
    RestoreRuleInfo;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.meScaleStopValueChanged(Sender: TObject);
var
  AEdit: TcxMaskEdit;
begin
  if (Helper.SelectedRuleInfo <> nil) and not FIsLoading then
  begin
    AEdit := Sender as TcxMaskEdit;
    HelperScale.Stops[AEdit.Tag].Value := AEdit.EditingValue;
    AEdit.EditValue := HelperScale.Stops[AEdit.Tag].Value;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.Save(var ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  StoreRuleInfo;
  Helper.SaveChanges(ARule);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.DoRuleTypeSelected(ANode: TcxTreeListNode);
begin
  StoreRuleInfo;
  Helper.SelectedRuleInfo := TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo(ANode.Data);
  RestoreRuleInfo
end;

function TfrmSpreadSheetConditionalFormattingRuleEditDialog.GetEditMask(
  AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType): TcxEditRepositoryItem;
begin
  case AValueType of
    cssvtPercent, cssvtPercentile:
      Result := EditRepositoryPercentsMask;
    cssvtValue:
      Result := EditRepositoryFloatMask;
  else
    Result := nil;
  end;
end;

function TfrmSpreadSheetConditionalFormattingRuleEditDialog.CanShowFormatDialog: Boolean;
var
  AStyle: TdxSpreadSheetCellStyle;
begin
  Result := (Helper.SelectedRuleInfo <> nil) and
    (Helper.SelectedRuleInfo.GetStyle(AStyle) or (Helper.SelectedRuleInfo.RuleType = cfrtDataBar));
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.ShowFormatCellDialog(AStyle: TdxSpreadSheetCellStyle);
var
  ADialog: TdxSpreadSheetCellStyleEditDialogForm;
begin
  ADialog := TdxSpreadSheetCellStyleEditDialogForm.Create(FOwner);
  try
    ADialog.Initialize(TdxSpreadSheetConditionalFormattingCellStyleEditDialogSingleStyleController.Create(Helper.ConditionalFormatting, AStyle));
    ADialog.Load;
    if ADialog.ShowModal = mrOk then
      ADialog.Save;
  finally
    ADialog.Free;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleInfo;
const
  Map: array[Boolean] of TdxLayoutAlignHorz = (ahClient, ahLeft);
begin
  FIsLoading := True;
  lcMain.BeginUpdate;
  try
    lcgIconSet.Visible := False;
    lcgPreview.Visible := False;
    lcgRuleExpression.Visible := False;
    lcgScale.Visible := False;
    lcgTopBottomValues.Visible := False;
    lciAboveOrBelowAverage.Visible := False;
    lciExpression1.Visible := False;
    lciExpression2.Visible := False;
    lciOperators.Visible := False;
    lciShowBarOnly.Visible := False;
    lclRuleDescription.Visible := False;

    if Helper.SelectedRuleInfo <> nil then
      case Helper.SelectedRuleInfo.RuleType of
        cfrtTwoColorScale, cfrtThreeColorScale, cfrtDataBar:
          RestoreRuleScaleInfo(TdxSpreadSheetConditionalFormattingRuleCustomColorScale(Helper.SelectedRulePreview));
        cfrtCellIs:
          RestoreRuleCellIsInfo(TdxSpreadSheetConditionalFormattingRuleCellIs(Helper.SelectedRulePreview));
        cfrtExpression:
          RestoreRuleExpressionInfo(TdxSpreadSheetConditionalFormattingRuleExpression(Helper.SelectedRulePreview));
        cfrtAboveOrBelowAverage:
          RestoreRuleAboveOrBelowAverage(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage(Helper.SelectedRulePreview));
        cfrtTopBottomValues:
          RestoreRuleTopBottomInfo(TdxSpreadSheetConditionalFormattingRuleTopBottomValues(Helper.SelectedRulePreview));
        cfrtIconSet:
          RestoreRuleIconSet(TdxSpreadSheetConditionalFormattingRuleIconSet(Helper.SelectedRulePreview));
        cfrtDuplicateValues, cfrtUniqueValues:
          lcgPreview.Visible := True;
      end;

    lciFormatButton.Visible := CanShowFormatDialog;
    lciPreview.AlignHorz := Map[lciFormatButton.Visible];
    lciPreview.Visible := Helper.SelectedRuleInfo <> nil;
    lclPreview.Visible := Helper.SelectedRuleInfo <> nil;
    UpdateRuleDescription;
  finally
    lcMain.EndUpdate(False);
    FIsLoading := False;
  end;
  pbPreview.Invalidate;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleAboveOrBelowAverage(
  ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
begin
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.LoadAboveOrBelowAverageRuleInfo(cbbAverageComparisonOperator, ARule);
  lciAboveOrBelowAverage.Visible := True;
  lclRuleDescription.Visible := True;
  lcgPreview.Visible := True;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleCellIsInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleCellIs);
begin
  cbbOperator.ItemIndex := Ord(ARule.ComparisonOperator);
  teExpression1.Text := ARule.Expression;
  teExpression2.Text := ARule.Expression2;

  lcgRuleExpression.Visible := True;
  lciExpression1.Visible := True;
  lciOperators.Visible := True;
  lclRuleDescription.Visible := True;
  lcgPreview.Visible := True;

  ccbOperatorPropertiesChange(nil);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleExpressionInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
begin
  lcgPreview.Visible := True;
  lcgRuleExpression.Visible := True;
  lciExpression1.Visible := True;
  lclRuleDescription.Visible := True;
  teExpression1.EditValue := ARule.Expression;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleIconSet(
  ARule: TdxSpreadSheetConditionalFormattingRuleIconSet);
begin
  icbIconStyle.ItemIndex := HelperIconSet.GetIconSetIndex(icbIconStyle.Properties.Items, ARule.PresetName);
  cbShowIconOnly.Checked := not ARule.ShowValue;
  cbReverseIconOrder.Checked := ARule.Order = isioReversed;
  HelperIconSet.Load(ARule);
  lclRuleDescription.Visible := True;
  lcgIconSet.Visible := True;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleScaleInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleCustomScale);
begin
  HelperScale.Initialize(ARule);
  lcgScaleMidStop.Visible := HelperScale.Stops[1] <> nil;
  lciColorScaleMinStopColor.Visible := HelperScale.Stops[0] is TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
  lciColorScaleMidStopColor.Visible := HelperScale.Stops[1] is TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
  lciColorScaleMaxStopColor.Visible := HelperScale.Stops[2] is TdxSpreadSheetConditionalFormattingRuleColorScaleStop;

  lciShowBarOnly.Visible := ARule is TdxSpreadSheetConditionalFormattingRuleDataBar;
  if lciShowBarOnly.Visible then
    cbShowBarOnly.Checked := not TdxSpreadSheetConditionalFormattingRuleDataBar(Helper.SelectedRulePreview).ShowValue;

  lcgPreview.Visible := True;
  lcgScale.Visible := True;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.RestoreRuleTopBottomInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
begin
  lcgPreview.Visible := True;
  lcgTopBottomValues.Visible := True;
  cbbTopBottomValues.ItemIndex := Ord(ARule.Direction);
  cbTopBottomValuesPercents.Checked := ARule.ValueType = tbvvtPercent;
  seTopBottomValuesRank.Value := ARule.Value;
  lclRuleDescription.Visible := True;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.StoreRuleInfo;
begin
  if Helper.SelectedRuleInfo <> nil then
    case Helper.SelectedRuleInfo.RuleType of
      cfrtCellIs:
        StoreRuleCellIsInfo(TdxSpreadSheetConditionalFormattingRuleCellIs(Helper.SelectedRulePreview));
      cfrtExpression:
        StoreRuleExpressionInfo(TdxSpreadSheetConditionalFormattingRuleExpression(Helper.SelectedRulePreview));
      cfrtAboveOrBelowAverage:
        StoreRuleAboveOrBelowAverage(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage(Helper.SelectedRulePreview));
      cfrtTopBottomValues:
        StoreRuleTopBottomInfo(TdxSpreadSheetConditionalFormattingRuleTopBottomValues(Helper.SelectedRulePreview));
    end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.StoreRuleAboveOrBelowAverage(
  ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
begin
  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.SaveAboveOrBelowAverageRuleInfo(cbbAverageComparisonOperator, ARule);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.StoreRuleCellIsInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleCellIs);
begin
  ARule.ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator(cbbOperator.ItemObject);
  ARule.Expression := teExpression1.Text;
  ARule.Expression2 := teExpression2.Text;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.StoreRuleExpressionInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleExpression);
begin
  ARule.Expression := teExpression1.Text;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.StoreRuleTopBottomInfo(
  ARule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
begin
  if cbTopBottomValuesPercents.Checked then
    ARule.ValueType := tbvvtPercent
  else
    ARule.ValueType := tbvvtRank;

  ARule.Direction := TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection(cbbTopBottomValues.ItemIndex);
  ARule.Value := seTopBottomValuesRank.Value;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.btnFormatClick(Sender: TObject);
var
  ADataBar: TdxSpreadSheetConditionalFormattingRuleDataBar;
  AStyle: TdxSpreadSheetCellStyle;
begin
  if Helper.SelectedRuleInfo.RuleType = cfrtDataBar then
  begin
    ADataBar := TdxSpreadSheetConditionalFormattingRuleDataBar(Helper.SelectedRulePreview);
    ADataBar.BeginUpdate;
    try
      ShowConditionalFormattingDataBarRuleStyleEditDialog(FOwner, ADataBar.Style);
    finally
      ADataBar.EndUpdate;
    end;
  end
  else
    if Helper.SelectedRuleInfo.GetStyle(AStyle) then
      ShowFormatCellDialog(AStyle);

  pbPreview.Invalidate;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.btnOkClick(Sender: TObject);

  procedure Check(AValue: Boolean; AMessage: Pointer);
  begin
    if not AValue then
      raise EdxSpreadSheetError.Create(cxGetResourceString(AMessage));
  end;

begin
  try
    StoreRuleInfo;
    if Helper.SelectedRuleInfo <> nil then
      case Helper.SelectedRuleInfo.RuleType of
        cfrtIconSet:
          Check(HelperIconSet.AreActualValuesInValidOrder, @sdxConditionalFormattingRuleEditDialogErrorIncorrectStopsOrder);
        cfrtDataBar, cfrtTwoColorScale, cfrtThreeColorScale:
          Check(HelperScale.AreActualValuesInValidOrder, @sdxConditionalFormattingRuleEditDialogErrorIncorrectStopsOrder);
        cfrtExpression:
          Check(Helper.AreExpressionsValid(TdxSpreadSheetConditionalFormattingRuleExpression(Helper.SelectedRulePreview)),
            @sdxConditionalFormattingRuleEditDialogErrorInvalidExpression);
        cfrtCellIs:
          Check(Helper.AreExpressionsValid(TdxSpreadSheetConditionalFormattingRuleCellIs(Helper.SelectedRulePreview)),
            @sdxConditionalFormattingRuleEditDialogErrorInvalidExpression);
      end;
    ModalResult := mrOk;
  except
    on E: EdxSpreadSheetError do
      MessageDlg(E.Message, mtError, [mbOK], 0)
    else
      raise;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.cbbScaleStopValueTypePropertiesChange(Sender: TObject);
var
  AEdit: TcxComboBox;
  AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  AEdit := Sender as TcxComboBox;

  if HelperScale.Stops[AEdit.Tag] <> nil then
  begin
    AValueType := TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.GetScaleStopValueType(AEdit);
    HelperScale.Stops[AEdit.Tag].ValueType := AValueType;
    HelperScale.EditorValue[AEdit.Tag].RepositoryItem := GetEditMask(AValueType);
    HelperScale.EditorValue[AEdit.Tag].Enabled := AValueType <> cssvtLimitValue;
    HelperScale.EditorValue[AEdit.Tag].EditValue := HelperScale.Stops[AEdit.Tag].Value;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.cbReverseIconOrderChanged(Sender: TObject);
begin
  if not FIsLoading then
    HelperIconSet.SetReverseIconOrder(cbReverseIconOrder.Checked);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.cbShowBarOnlyChanged(Sender: TObject);
begin
  if not FIsLoading then
    TdxSpreadSheetConditionalFormattingRuleDataBar(Helper.SelectedRulePreview).ShowValue := not cbShowBarOnly.Checked;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.cbShowIconOnlyChanged(Sender: TObject);
begin
  if not FIsLoading then
    HelperIconSet.SetShowIconsOnly(cbShowIconOnly.Checked);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.ccbColorScaleStopColorPropertiesChange(Sender: TObject);
var
  AEdit: TcxColorComboBox;
begin
  if (Helper.SelectedRuleInfo <> nil) and not FIsLoading then
  begin
    AEdit := Sender as TcxColorComboBox;
    HelperScale.SetStopColor(AEdit.Tag, AEdit.ColorValue);
    pbPreview.Invalidate;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.ccbOperatorPropertiesChange(Sender: TObject);
begin
  lciExpression2.Visible := TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator(cbbOperator.ItemObject) in [cicoBetween, cicoNotBetween];
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.pbPreviewPaint(Sender: TObject);
begin
  if Helper.SelectedRuleInfo <> nil then
  begin
    cxPaintCanvas.BeginPaint(pbPreview.Canvas);
    try
      Helper.SelectedRuleInfo.DrawPreview(cxPaintCanvas, pbPreview.ClientRect);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.icbIconStyleChanged(Sender: TObject);
begin
  if not FIsLoading then
    HelperIconSet.SetIconSet(icbIconStyle.EditingValue);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetColumnComparisonOperationChanged(Sender: TObject);
begin
  HelperIconSet.SetComparisonOperator(tlIconSet.FocusedNode, tlIconSetColumnComparisonOperation);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetColumnGetDisplayText(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
begin
  if ANode.IsLast then
    Value := '';
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetColumnIconChanged(Sender: TObject);
begin
  HelperIconSet.SetIconIndex(tlIconSet.FocusedNode, tlIconSetColumnIcon);
  icbIconStyle.ItemIndex := -1;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetColumnStopValueGetEditingProperties(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
var
  AItem: TcxEditRepositoryItem;
begin
  AItem := GetEditMask(HelperIconSet.GetStopType(ANode, tlIconSetColumnStopValueType));
  if AItem <> nil then
    EditProperties := AItem.Properties;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetColumnStopValueTypeChanged(Sender: TObject);
begin
  HelperIconSet.SetStopValueType(tlIconSet.FocusedNode,
    HelperIconSet.GetStopType(tlIconSet.FocusedNode, tlIconSetColumnStopValueType));
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetEdited(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  if not FIsLoading and (AColumn = tlIconSetColumnStopValue) then
    HelperIconSet.SetStopValue(Sender.FocusedNode, Sender.FocusedNode.Values[AColumn.ItemIndex]);
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlIconSetEditing(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  Allow := (AColumn = tlIconSetColumnIcon) or not Sender.FocusedNode.IsLast;
end;

procedure TfrmSpreadSheetConditionalFormattingRuleEditDialog.tlRuleTypeFocusedNodeChanged(
  Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  if not FIsSelecting then
    TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self,
      procedure
      begin
        DoRuleTypeSelected(AFocusedNode);
      end);
end;

end.
