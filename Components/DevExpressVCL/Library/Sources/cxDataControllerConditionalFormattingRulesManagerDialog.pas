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

unit cxDataControllerConditionalFormattingRulesManagerDialog;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Controls, Variants, Menus, StdCtrls, Classes, ActnList, ImgList, Forms,
  cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxCustomData, cxStyles, cxTL,
  cxTextEdit, cxDropDownEdit, dxForms, dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxLayoutContainer, cxCheckBox,
  cxEditRepositoryItems, cxImageList, cxClasses, dxLayoutLookAndFeels, cxInplaceContainer, cxMaskEdit, cxButtons,
  dxLayoutControl, dxBuiltInPopupMenu,
  cxDataControllerConditionalFormatting,
  dxSpreadSheetConditionalFormatting,
  dxSpreadSheetConditionalFormattingRulesManagerDialog,
  dxSpreadSheetConditionalFormattingRulesManagerDialogHelpers;

type
  { TcxDataControllerConditionalFormattingRulesManagerDialogHelper }

  TcxDataControllerConditionalFormattingRulesManagerDialogHelper = class(TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper)
  strict private
    function GetConditionalFormatting: TcxDataControllerConditionalFormatting;
  protected
    procedure EndEditing(ACanceled: Boolean); override;
  public
    function ApplyToTheRowSupported(ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
    procedure SetArea(ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo; const AArea: TRect);

    property ConditionalFormatting: TcxDataControllerConditionalFormatting read GetConditionalFormatting;
  end;

  { TfrmDataControllerConditionalFormattingRulesManagerDialog }

  TfrmDataControllerConditionalFormattingRulesManagerDialog = class(TfrmSpreadSheetConditionalFormattingRulesManagerDialog)
    cxEditRepositoryCheckBoxApplyToTheRow: TcxEditRepositoryCheckBoxItem;
    cxEditRepositoryDataController: TcxEditRepository;
    tlcRuleApplyToTheRecord: TcxTreeListColumn;

    procedure cxEditRepositoryCheckBoxApplyToTheRowPropertiesEditValueChanged(Sender: TObject);
    procedure tlcRuleApplyToTheRecordGetEditProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    procedure tlRulesEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
  strict private
    function GetConditionalFormatting: TcxDataControllerConditionalFormatting;
    function GetHelper: TcxDataControllerConditionalFormattingRulesManagerDialogHelper;
    procedure RuleAreaPropertiesEditValueChangedHandler(Sender: TObject);
  protected
    procedure ApplyLocalization; override;
    procedure DoInitialize; override;
    procedure DoPopulateRules; override;
    function GetHelperClass: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperClass; override;
    procedure PopulateRulesInArea(const AArea: TRect); override;

    property ConditionalFormatting: TcxDataControllerConditionalFormatting read GetConditionalFormatting;
  public
    property Helper: TcxDataControllerConditionalFormattingRulesManagerDialogHelper read GetHelper;
  end;

implementation

uses
  dxTypeHelpers, dxCore, cxGeometry,
  dxSpreadSheetTypes,
  dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetCoreDialogsStrs;

{$R *.dfm}

type
  TcxCustomDataControllerAccess = class(TcxCustomDataController);
  TdxSpreadSheetCustomConditionalFormattingRuleAccess = class(TdxSpreadSheetCustomConditionalFormattingRule);
  TdxSpreadSheetConditionalFormattingRuleExpressionAccess = class(TdxSpreadSheetConditionalFormattingRuleExpression);
  TcxDataControllerConditionalFormattingAccess = class(TcxDataControllerConditionalFormatting);
  TcxDataControllerConditionalFormattingProviderAccess = class(TcxDataControllerConditionalFormattingProvider);

  { TcxDataControllerConditionalFormattingRulesManagerDialogAdapter }

  TcxDataControllerConditionalFormattingRulesManagerDialogAdapter = class(TcxDataControllerConditionalFormattingRulesManagerDialogProvider.TAdapter)
  public
    procedure Execute(AProvider: TcxDataControllerConditionalFormattingProvider); override;
  end;

procedure TcxDataControllerConditionalFormattingRulesManagerDialogAdapter.Execute(AProvider: TcxDataControllerConditionalFormattingProvider);
var
  ADialog: TfrmDataControllerConditionalFormattingRulesManagerDialog;
  AIntf: IdxDialogOwner;
  AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Supports(AProvider, IdxDialogOwner, AIntf);
  ADialog := TfrmDataControllerConditionalFormattingRulesManagerDialog.Create(AProvider);
  try
    AConditionalFormatting := AProvider.ConditionalFormatting;
    ADialog.Initialize(AConditionalFormatting, AIntf.GetLookAndFeel);
    if ADialog.ShowModal = mrOk then
      ADialog.Helper.ApplyChanges;
  finally
    ADialog.Free;
  end;
end;

{ TcxDataControllerConditionalFormattingRulesManagerDialogHelper }

function TcxDataControllerConditionalFormattingRulesManagerDialogHelper.ApplyToTheRowSupported(ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
begin
  Result := TdxSpreadSheetCustomConditionalFormattingRuleAccess(ARule).ApplyToTheRowSupported;;
end;

procedure TcxDataControllerConditionalFormattingRulesManagerDialogHelper.SetArea(
  ARuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo; const AArea: TRect);
var
  AAreaList: TdxSpreadSheetAreaList;
begin
  AAreaList := TdxSpreadSheetAreaList.Create;
  try
    AAreaList.Capacity := 1;
    AAreaList.Add(AArea);
    SetAreas(ARuleInfo, AAreaList);
  finally
    AAreaList.Free;
  end;
end;

function TcxDataControllerConditionalFormattingRulesManagerDialogHelper.GetConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := TcxDataControllerConditionalFormatting(inherited ConditionalFormatting);
end;

procedure TcxDataControllerConditionalFormattingRulesManagerDialogHelper.EndEditing(ACanceled: Boolean);
begin
  inherited EndEditing(ACanceled);
  Application.ProcessMessages;
end;

{ TfrmDataControllerConditionalFormattingRulesManagerDialog }

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.ApplyLocalization;
begin
  inherited ApplyLocalization;
  cbDisplayMode.Properties.Items[1] := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogDisplayModeAll);
  tlcRuleApplyToTheRecord.Caption.Text := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogColumnApplyToTheRecord);
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.cxEditRepositoryCheckBoxApplyToTheRowPropertiesEditValueChanged(
  Sender: TObject);

  function GetToken(AExpression: TdxSpreadSheetConditionalFormattingExpression; out AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    if AExpression = nil then
      Exit(False);
    AToken := AExpression.Tokens;
    while AToken <> nil do
    begin
      if AToken is TcxDataControllerConditionalFormattingFormulaFieldReference then
        Break;
      AToken := AToken.Next;
    end;
    Result := AToken <> nil;
  end;

var
  AArea: TRect;
  ADataController: TcxCustomDataControllerAccess;
  AField: TcxCustomDataField;
  ARule: TdxSpreadSheetConditionalFormattingRuleExpressionAccess;
  AToken: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  if not Helper.ApplyToTheRowSupported(FocusedRuleInfo.Rule) then
    Exit;
  if (Sender as TcxCheckBox).EditValue then
    AArea := TRect.Create(0, 0, MaxInt, MaxInt)
  else
  begin
    ARule := TdxSpreadSheetConditionalFormattingRuleExpressionAccess(FocusedRuleInfo.Rule);
    if GetToken(ARule.Formulas[0], AToken) or GetToken(ARule.Formulas[1], AToken) then
    begin
      with TcxDataControllerConditionalFormattingFormulaFieldReference(AToken) do
        AArea := TRect.Create(ActualColumn, 0, ActualColumn, MaxInt);
    end
    else
    begin
      AArea := Helper.ConditionalFormatting.Owner.GetSelectionArea;
      if AArea.Left <> AArea.Right then
      begin
        ADataController := TcxCustomDataControllerAccess(ConditionalFormatting.DataController);
        for I := 0 to ADataController.Fields.Count - 1 do
        begin
          AField := ADataController.Fields[I];
          if ConditionalFormatting.IsFieldVisible(AField) then
          begin
            AArea := TRect.Create(AField.Index, 0, AField.Index, MaxInt);
            Break;
          end;
        end;
        if AArea.Left <> AArea.Right then
          AArea := TcxDataControllerConditionalFormattingProvider.DefaultArea;
      end;
    end;
  end;
  Helper.SetArea(FocusedRuleInfo, AArea);
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.DoInitialize;
var
  I: Integer;
  ADataController: TcxCustomDataControllerAccess;
  AProperties: TcxComboBoxProperties;
  AField: TcxCustomDataField;
begin
  inherited DoInitialize;

  tlcRuleArea.PropertiesClass := TcxComboBoxProperties;
  AProperties := tlcRuleArea.Properties as TcxComboBoxProperties;
  AProperties.DropDownListStyle := lsFixedList;
  AProperties.OnEditValueChanged := RuleAreaPropertiesEditValueChangedHandler;
  AProperties.Items.Clear;
  ADataController := TcxCustomDataControllerAccess(ConditionalFormatting.DataController);
  for I := 0 to ADataController.Fields.Count - 1 do
  begin
    AField := ADataController.Fields[I];
    if ConditionalFormatting.IsFieldVisible(AField) then
    begin
      AProperties.Items.AddObject(ConditionalFormatting.GetFieldDisplayName(AField), AField);
      cbDisplayMode.Properties.Items.AddObject(ConditionalFormatting.GetFieldDisplayName(AField), AField);
    end;
  end;

  if csDesigning in FOwner.GetOwner.ComponentState then
  begin
    lciDisplayMode.Visible := False;
    lcMainSpaceItem1.Visible := False;
  end;

  cbDisplayMode.ItemIndex := 1;
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.DoPopulateRules;
var
  AArea: TRect;
  AItemIndex: Integer;
begin
  if cbDisplayMode.ItemIndex in [0, 1] then
    AArea := cxNullRect
  else
  begin
    AItemIndex := TcxCustomDataField(cbDisplayMode.Properties.Items.Objects[cbDisplayMode.ItemIndex]).Index;
    AArea := TRect.Create(AItemIndex, 0, AItemIndex, MaxInt);
  end;
  TcxDataControllerConditionalFormattingProviderAccess(TcxDataControllerConditionalFormattingAccess(ConditionalFormatting).Provider).CustomSelectedArea := AArea;
  if cbDisplayMode.ItemIndex in [0, 1] then
    inherited DoPopulateRules
  else
    PopulateRulesInArea(AArea);
end;

function TfrmDataControllerConditionalFormattingRulesManagerDialog.GetConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := Helper.ConditionalFormatting;
end;

function TfrmDataControllerConditionalFormattingRulesManagerDialog.GetHelper: TcxDataControllerConditionalFormattingRulesManagerDialogHelper;
begin
  Result := TcxDataControllerConditionalFormattingRulesManagerDialogHelper(inherited Helper);
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.RuleAreaPropertiesEditValueChangedHandler(Sender: TObject);
var
  AComboBox: TcxComboBox;
  AItemIndex: Integer;
begin
  AComboBox := Sender as TcxComboBox;
  AItemIndex := TcxCustomDataField(AComboBox.Properties.Items.Objects[AComboBox.ItemIndex]).Index;
  Helper.SetArea(FocusedRuleInfo, TRect.Create(AItemIndex, 0, AItemIndex, MaxInt));
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.tlcRuleApplyToTheRecordGetEditProperties(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
  var EditProperties: TcxCustomEditProperties);
begin
  if Helper.ApplyToTheRowSupported(TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(ANode.Data).Rule) then
    EditProperties := cxEditRepositoryCheckBoxApplyToTheRow.Properties
  else
    EditProperties := nil;
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.tlRulesEditing(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  inherited;
  Allow := Allow or (AColumn = tlcRuleApplyToTheRecord) and Helper.ApplyToTheRowSupported(FocusedRuleInfo.Rule);
end;

function TfrmDataControllerConditionalFormattingRulesManagerDialog.GetHelperClass: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperClass;
begin
  Result := TcxDataControllerConditionalFormattingRulesManagerDialogHelper;
end;

procedure TfrmDataControllerConditionalFormattingRulesManagerDialog.PopulateRulesInArea(const AArea: TRect);
begin
  Helper.PopulateRulesInArea(AArea,
    procedure (AInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo)
    var
      ANode: TcxTreeListNode;
      R: TRect;
    begin
      ANode := tlRules.AddChild(nil, AInfo);
      ANode.Values[0] := AInfo.Details;
      R := AInfo.Rule.Area;
      if (R.Left <> R.Right) and Helper.ApplyToTheRowSupported(AInfo.Rule) then
      begin
        ANode.Values[2] := '';
        ANode.Values[4] := True;
      end
      else
      begin
        ANode.Values[2] := Helper.ConditionalFormatting.ReferencesToString(AInfo.Rule.Areas);
        if Helper.ApplyToTheRowSupported(AInfo.Rule) then
          ANode.Values[4] := False
        else
          ANode.Values[4] := Null;
      end;

      if AInfo.StopIfTrueSupported then
        ANode.Values[3] := Ord(AInfo.StopIfTrue);
    end);
end;

initialization
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RegisterRulesManagerDialog(TcxDataControllerConditionalFormattingRulesManagerDialogAdapter);

finalization
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.UnregisterRulesManagerDialog(TcxDataControllerConditionalFormattingRulesManagerDialogAdapter);
end.
