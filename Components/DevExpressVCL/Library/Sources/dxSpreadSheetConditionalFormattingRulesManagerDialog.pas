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

unit dxSpreadSheetConditionalFormattingRulesManagerDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ImgList, ActnList, Generics.Defaults, Generics.Collections,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxClasses, cxContainer, cxStyles,
  cxCustomData, cxData, cxDataStorage, dxForms, cxImageList,
  cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutControl,
  dxLayoutcxEditAdapters,  dxLayoutContainer,
  cxEdit, cxButtons, cxListBox, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxMCListBox,
  cxFilter, cxNavigator, cxCheckBox, cxInplaceContainer,
  cxTL, cxEditRepositoryItems,
  dxSpreadSheetConditionalFormatting,
  dxSpreadSheetConditionalFormattingRulesManagerDialogHelpers;

type

  { TdxSpreadSheetConditionalFormattingRulesManagerDialogForm }

  TfrmSpreadSheetConditionalFormattingRulesManagerDialog = class(TdxForm,
    IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener
  )
    acClone: TAction;
    acCreate: TAction;
    acDelete: TAction;
    acEdit: TAction;
    acMoveDown: TAction;
    acMoveUp: TAction;
    alActions: TActionList;
    btnApply: TcxButton;
    btnCancel: TcxButton;
    btnCreate: TcxButton;
    btnEdit: TcxButton;
    btnMoveDown: TcxButton;
    btnMoveUp: TcxButton;
    btnOk: TcxButton;
    btnRemove: TcxButton;
    cbDisplayMode: TcxComboBox;
    cxEditRepository: TcxEditRepository;
    cxEditRepositoryCheckBoxItem: TcxEditRepositoryCheckBoxItem;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    ilImages: TcxImageList;
    lciDisplayMode: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutAutoCreatedGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    lcMainItem8: TdxLayoutItem;
    lcMainSpaceItem1: TdxLayoutEmptySpaceItem;
    miClone: TMenuItem;
    miDelete: TMenuItem;
    miEdit: TMenuItem;
    miLine2: TMenuItem;
    pmRules: TPopupMenu;
    tlcRuleArea: TcxTreeListColumn;
    tlcRuleFormat: TcxTreeListColumn;
    tlcRuleName: TcxTreeListColumn;
    tlcRuleStopIfTrue: TcxTreeListColumn;
    tlRules: TcxTreeList;

    procedure acCloneExecute(Sender: TObject);
    procedure acCloneUpdate(Sender: TObject);
    procedure acCreateExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acEditUpdate(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveDownUpdate(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveUpUpdate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure cbDisplayModePropertiesChange(Sender: TObject);
    procedure tlcRuleAreaPropertiesEditValueChanged(Sender: TObject);
    procedure tlcRuleStopIfTrueGetEditProperties(Sender: TcxTreeListColumn;
      ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    procedure tlcRuleStopIfTruePropertiesEditValueChanged(Sender: TObject);
    procedure tlRulesCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
      AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure tlRulesDblClick(Sender: TObject);
    procedure tlRulesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tlRulesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tlRulesEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
    procedure tlRulesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tlRulesMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
  strict private
    FIsInitializing: Boolean;

    procedure AdapterClickHandler(Sender: TObject);
    function GetFocusedRuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo;
    procedure WMUpdateSelection(var AMessage: TMessage); message WM_USER;
  protected
    FHelper: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper;
    FOwner: IdxDialogOwner;

    procedure ApplyLocalization; virtual;
    function GetHelperClass: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperClass; virtual;
    procedure DoInitialize; virtual;
    procedure DoPopulateRules; virtual;
    procedure Initialize(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting; ALookAndFeel: TcxLookAndFeel);
    procedure PopulateRules;
    procedure PopulateRulesInArea(const AArea: TRect); virtual;

    // TreeList
    procedure RestoreFocus(AIndex, ATopIndex: Integer; AData: Pointer);
    procedure SaveFocus(out AIndex: Integer; out ATopIndex: Integer; out AData: Pointer);

    // IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener
    procedure IdxSpreadSheetConditionalFormattingRulesManagerDialogHelperListener.NotifyDataChanged = PopulateRules;
    procedure NotifyModified;
  public
    constructor Create(AOwner: IdxDialogOwner); reintroduce;
    destructor Destroy; override;
    //
    property FocusedRuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo read GetFocusedRuleInfo;
    property Helper: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper read FHelper;
  end;

procedure ShowConditionalFormattingRulesManagerDialog(AOwner: IdxDialogOwner); overload;
procedure ShowConditionalFormattingRulesManagerDialog(AOwner: TObject); overload; // Special for CBuilder

implementation

uses
  dxSpreadSheetCoreDialogsStrs, cxGeometry, dxSpreadSheetUtils, Math, dxSpreadSheetConditionalFormattingRuleEditDialog,
  dxSpreadSheetTypes, dxBuiltInPopupMenu;

{$R *.dfm}

procedure ShowConditionalFormattingRulesManagerDialog(AOwner: TObject);
var
  AOwnerIntf: IdxDialogOwner;
  AResult: Boolean;
begin
  AResult := Supports(AOwner, IdxDialogOwner, AOwnerIntf);
  Assert(AResult);
  if AResult then
    ShowConditionalFormattingRulesManagerDialog(AOwnerIntf);
end;

procedure ShowConditionalFormattingRulesManagerDialog(AOwner: IdxDialogOwner);
var
  ADialog: TfrmSpreadSheetConditionalFormattingRulesManagerDialog;
  AIntf: IdxSpreadSheetConditionalFormatting;
begin
  if Supports(AOwner, IdxSpreadSheetConditionalFormatting, AIntf) then
  begin
    ADialog := TfrmSpreadSheetConditionalFormattingRulesManagerDialog.Create(AOwner);
    try
      ADialog.Initialize(AIntf.GetConditionalFormatting, AOwner.GetLookAndFeel);
      if ADialog.ShowModal = mrOk then
        ADialog.Helper.ApplyChanges;
    finally
      ADialog.Free;
    end;
  end;
end;

{ TdxSpreadSheetUnhideSheetDialogForm }

constructor TfrmSpreadSheetConditionalFormattingRulesManagerDialog.Create(AOwner: IdxDialogOwner);
begin
  inherited Create(AOwner.GetParentForm);
  FOwner := AOwner;
end;

destructor TfrmSpreadSheetConditionalFormattingRulesManagerDialog.Destroy;
begin
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.ApplyLocalization;
var
  AIndex: Integer;
begin
  Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogCaption);

  // Display Mode
  AIndex := cbDisplayMode.ItemIndex;
  lciDisplayMode.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogDisplayMode);
  cbDisplayMode.Properties.Items[0] := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogDisplayModeSelectedArea);
  cbDisplayMode.Properties.Items[1] := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogDisplayModeSheet);
  cbDisplayMode.ItemIndex := AIndex;

  // Actions
  acCreate.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogActionCreate);
  acDelete.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogActionDelete);
  acEdit.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogActionEdit);
  acClone.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogActionClone);

  // Table
  tlcRuleName.Caption.Text := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogColumnName);
  tlcRuleFormat.Caption.Text := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogColumnFormat);
  tlcRuleArea.Caption.Text := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogColumnArea);
  tlcRuleStopIfTrue.Caption.Text := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogColumnStopIfTrue);

  // Buttons
  btnApply.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogButtonApply);
  btnCancel.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxConditionalFormattingRulesManagerDialogButtonOk);
end;

function TfrmSpreadSheetConditionalFormattingRulesManagerDialog.GetHelperClass: TdxSpreadSheetConditionalFormattingRulesManagerDialogHelperClass;
begin
  Result := TdxSpreadSheetConditionalFormattingRulesManagerDialogHelper;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.DoInitialize;
begin
// do nothing
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.DoPopulateRules;
begin
  if cbDisplayMode.ItemIndex = 0 then
    PopulateRulesInArea(Helper.ConditionalFormatting.Owner.GetSelectionArea)
  else
    PopulateRulesInArea(cxRect(0, 0, MaxInt, MaxInt));
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.Initialize(
  AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
  ALookAndFeel: TcxLookAndFeel);
begin
  FIsInitializing := True;
  try
    FHelper := GetHelperClass.Create(AConditionalFormatting, Self);
    SetControlLookAndFeel(Self, ALookAndFeel);
    ApplyLocalization;
    DoInitialize;
  finally
    FIsInitializing := False;
  end;
  PopulateRules;
  NotifyModified;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.PopulateRules;
var
  APrevData: Pointer;
  APrevIndex: Integer;
  ATopIndex: Integer;
begin
  if FIsInitializing then
    Exit;
  SaveFocus(APrevIndex, ATopIndex, APrevData);
  try
    tlRules.BeginUpdate;
    try
      tlRules.Clear;
      DoPopulateRules;
    finally
      tlRules.EndUpdate;
    end;
  finally
    RestoreFocus(APrevIndex, ATopIndex, APrevData);
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.PopulateRulesInArea(const AArea: TRect);
begin
  Helper.PopulateRulesInArea(AArea,
    procedure (AInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo)
    var
      ANode: TcxTreeListNode;
    begin
      ANode := tlRules.AddChild(nil, AInfo);
      ANode.Values[0] := AInfo.Details;
      ANode.Values[2] := Helper.ConditionalFormatting.ReferencesToString(AInfo.Rule.Areas);
      if AInfo.StopIfTrueSupported then
        ANode.Values[3] := Ord(AInfo.StopIfTrue);
    end);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.RestoreFocus(AIndex, ATopIndex: Integer; AData: Pointer);
var
  I: Integer;
begin
  for I := 0 to tlRules.Root.Count - 1 do
    if tlRules.Root[I].Data = AData then
    begin
      AIndex := I;
      Break;
    end;

  if InRange(ATopIndex, 0, tlRules.Root.Count - 1) then
    tlRules.TopVisibleNode := tlRules.Root[ATopIndex];

  AIndex := Min(Max(AIndex, 0), tlRules.Root.Count - 1);
  if AIndex >= 0 then
  begin
    tlRules.FocusedNode := tlRules.Root[AIndex];
    tlRules.FocusedNode.MakeVisible;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.SaveFocus(out AIndex: Integer; out ATopIndex: Integer; out AData: Pointer);
begin
  if tlRules.TopVisibleNode <> nil then
    ATopIndex := tlRules.TopVisibleNode.Index
  else
    ATopIndex := -1;

  if tlRules.FocusedNode <> nil then
  begin
    AData := tlRules.FocusedNode.Data;
    AIndex := tlRules.FocusedNode.Index;
  end
  else
    AIndex := -1;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.NotifyModified;
begin
  btnApply.Enabled := Helper.HasChanges;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.AdapterClickHandler(Sender: TObject);
begin
  TMenuItem((Sender as TComponent).Tag).Click;
end;

function TfrmSpreadSheetConditionalFormattingRulesManagerDialog.GetFocusedRuleInfo: TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo;
begin
  Result := TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(tlRules.FocusedNode.Data);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.WMUpdateSelection(var AMessage: TMessage);
begin
  if tlRules.FocusedNode = nil then
  begin
    if tlRules.SelectionCount > 0 then
      tlRules.FocusedNode := tlRules.Selections[0];
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acCloneExecute(Sender: TObject);
begin
  if tlRules.FocusedNode <> nil then
  begin
    Helper.Add(FocusedRuleInfo.Rule.Clone);
    tlRules.FocusedNode := tlRules.AbsoluteVisibleItems[tlRules.AbsoluteVisibleCount - 1];
    tlRules.FocusedNode.MakeVisible;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acCloneUpdate(Sender: TObject);
begin
  acClone.Enabled := tlRules.FocusedNode <> nil;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acCreateExecute(Sender: TObject);
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  ARule := nil;
  if ShowConditionalFormattingRuleEditDialog(FOwner, ARule) then
    Helper.Add(ARule);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acDeleteExecute(Sender: TObject);
begin
  Helper.Delete(FocusedRuleInfo);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acDeleteUpdate(Sender: TObject);
begin
  acDelete.Enabled := tlRules.FocusedNode <> nil;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acEditExecute(Sender: TObject);
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  ARule := FocusedRuleInfo.Rule.Clone;
  if ShowConditionalFormattingRuleEditDialog(FOwner, ARule) then
    Helper.Replace(FocusedRuleInfo, ARule)
  else
    ARule.Free;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acEditUpdate(Sender: TObject);
begin
  acEdit.Enabled := tlRules.FocusedNode <> nil;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acMoveDownExecute(Sender: TObject);
begin
  Helper.MoveTo(FocusedRuleInfo,
    TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(tlRules.FocusedNode.getNextSibling.Data), True);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acMoveDownUpdate(Sender: TObject);
begin
  acMoveDown.Enabled := (tlRules.FocusedNode <> nil) and not tlRules.FocusedNode.IsLast;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acMoveUpExecute(Sender: TObject);
begin
  Helper.MoveTo(FocusedRuleInfo,
    TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(tlRules.FocusedNode.getPrevSibling.Data), False);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.acMoveUpUpdate(Sender: TObject);
begin
  acMoveUp.Enabled := (tlRules.FocusedNode <> nil) and not tlRules.FocusedNode.IsFirst;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.btnApplyClick(Sender: TObject);
begin
  Helper.ApplyChanges;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.cbDisplayModePropertiesChange(Sender: TObject);
begin
  PopulateRules;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlcRuleAreaPropertiesEditValueChanged(Sender: TObject);
var
  AAreaList: TdxSpreadSheetAreaList;
  AAreaListAsString: string;
begin
  AAreaList := TdxSpreadSheetAreaList.Create;
  try
    AAreaListAsString := (Sender as TcxCustomTextEdit).EditValue;
    AAreaListAsString := StringReplace(AAreaListAsString, '$', '', [rfReplaceAll]);
    AAreaList.AssignFromString(AAreaListAsString);
    if AAreaList.Count > 0 then
      Helper.SetAreas(FocusedRuleInfo, AAreaList)
    else
      PopulateRules;
  finally
    AAreaList.Free;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlcRuleStopIfTrueGetEditProperties(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
begin
  if TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(ANode.Data).StopIfTrueSupported then
    EditProperties := cxEditRepositoryCheckBoxItem.Properties
  else
    EditProperties := nil;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlcRuleStopIfTruePropertiesEditValueChanged(Sender: TObject);
begin
  FocusedRuleInfo.StopIfTrue := (Sender as TcxCheckBox).State = cbsChecked;
  Helper.HasChanges := True;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  AAdapter: TdxCustomBuiltInPopupMenuAdapter;
begin
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
  begin
    AAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(Self);
    try
      dxCallNotify(pmRules.OnPopup, pmRules);
      TdxBuiltInPopupMenuAdapterHelper.AddMenu(AAdapter, pmRules, AdapterClickHandler);
      AAdapter.Popup(tlRules.ClientToScreen(MousePos));
    finally
      AAdapter.Free;
    end;
    Handled := True;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesCustomDrawDataCell(
  Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  ADone := AViewInfo.Column = tlcRuleFormat;
  if ADone then
    TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(AViewInfo.Node.Data).DrawPreview(
      ACanvas, cxRectInflate(AViewInfo.BoundsRect, -cxTextOffset));
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesDblClick(Sender: TObject);
begin
  if not tlRules.IsEditing then
    acEdit.Execute;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := not (tlRules.HitTest.HitAtNodePreview or tlRules.HitTest.HitAtColumn);
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesEditing(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  Allow := (AColumn = tlcRuleArea) or (AColumn = tlcRuleStopIfTrue) and FocusedRuleInfo.StopIfTrueSupported;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    if not tlRules.IsEditing then
      acDelete.Execute;
  end;
end;

procedure TfrmSpreadSheetConditionalFormattingRulesManagerDialog.tlRulesMoveTo(Sender: TcxCustomTreeList;
  AttachNode: TcxTreeListNode; AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
begin
  case AttachMode of
    tlamAddChild:
      Helper.MoveTo(
        TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(TcxTreeListNode(Nodes[0]).Data),
        TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(AttachNode.GetLastChild.Data), True);

    tlamInsert:
      Helper.MoveTo(
        TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(TcxTreeListNode(Nodes[0]).Data),
        TdxSpreadSheetConditionalFormattingRulesManagerDialogRuleInfo(AttachNode.Data), False);

  else
    dxTestCheck(False, ClassName + '.MoveTo');
  end;
  PostMessage(Handle, WM_USER, 0, 0);
  Done := True;
end;

end.
