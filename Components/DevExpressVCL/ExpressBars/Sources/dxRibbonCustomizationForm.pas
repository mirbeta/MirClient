{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonCustomizationForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ActnList, Math,
  dxCore, cxClasses, cxControls, cxGraphics, cxStyles, cxContainer, cxInplaceContainer,
  dxRibbon, dxBar, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutControl, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters, cxEdit, cxButtons, cxTextEdit,
  cxDropDownEdit, cxCustomData, cxTL, cxMaskEdit, cxCheckBox, cxBarEditItem, dxLayoutLookAndFeels,
  dxRibbonCustomizationFormHelper, dxBarStrs, cxLabel, dxInputDialogs, dxForms;

type
  { TdxRibbonCustomizationForm }

  TdxRibbonCustomizationForm = class(TdxForm)
    acAdd: TAction;
    acAddNewContext: TAction;
    acAddNewGroup: TAction;
    acAddNewTab: TAction;
    acMoveDown: TAction;
    acMoveUp: TAction;
    acRemove: TAction;
    acRename: TAction;
    acResetAllCustomizations: TAction;
    acResetSelectedTab: TAction;
    acShowTab: TAction;
    acUpdateActionsState: TAction;
    alActions: TActionList;
    bbtnExportAllCustomizations: TdxBarButton;
    bbtnImportCustomizationFile: TdxBarButton;
    bbtnMoveDown: TdxBarButton;
    bbtnMoveUp: TdxBarButton;
    bbtnNewContext: TdxBarButton;
    bbtnNewGroup: TdxBarButton;
    bbtnNewTab: TdxBarButton;
    bbtnRemove: TdxBarButton;
    bbtnRename: TdxBarButton;
    bbtnResetAllCustomizations: TdxBarButton;
    bbtnResetOnlySelectedTab: TdxBarButton;
    bbtnResetTab: TdxBarButton;
    bbtnShowTab: TdxBarButton;
    bmMain: TdxBarManager;
    bpmImportExport: TdxBarPopupMenu;
    bpmNewElement: TdxBarPopupMenu;
    bpmReset: TdxBarPopupMenu;
    bpmRibbon: TdxBarPopupMenu;
    bsLowerSeparator: TdxBarSeparator;
    bsUpperSeparator: TdxBarSeparator;
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnImportExport: TcxButton;
    btnMoveDown: TcxButton;
    btnMoveUp: TcxButton;
    btnNewElement: TcxButton;
    btnOK: TcxButton;
    btnRemove: TcxButton;
    btnRename: TcxButton;
    btnReset: TcxButton;
    cbCommands: TcxComboBox;
    cbRibbon: TcxComboBox;
    chbShowQATBelowRibbon: TcxCheckBox;
    cxStyleRepository: TcxStyleRepository;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lbRibbonQAT: TcxLabel;
    lcgActions: TdxLayoutGroup;
    lcgCommands: TdxLayoutGroup;
    lcgControlling: TdxLayoutGroup;
    lcgEditing: TdxLayoutGroup;
    lcgReordering: TdxLayoutGroup;
    lcgRibbon: TdxLayoutGroup;
    lcgRibbonActions: TdxLayoutGroup;
    lciAdd: TdxLayoutItem;
    lciCancel: TdxLayoutItem;
    lciCommands: TdxLayoutItem;
    lciCommandsSource: TdxLayoutItem;
    lciImportExport: TdxLayoutItem;
    lciMoveDown: TdxLayoutItem;
    lciMoveUp: TdxLayoutItem;
    lciNewElement: TdxLayoutItem;
    lciOK: TdxLayoutItem;
    lciRemove: TdxLayoutItem;
    lciRename: TdxLayoutItem;
    lciReset: TdxLayoutItem;
    lciRibbon: TdxLayoutItem;
    lciRibbonQAT: TdxLayoutItem;
    lciRibbonSource: TdxLayoutItem;
    lciSeparator: TdxLayoutSeparatorItem;
    lciShowQATBelowRibbon: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    stNodeDisabled: TcxStyle;
    tlCommands: TcxTreeList;
    tlCommandsMainColumn: TcxTreeListColumn;
    tlRibbon: TcxTreeList;
    tlRibbonMainColumn: TcxTreeListColumn;
    procedure acAddExecute(Sender: TObject);
    procedure acAddNewContextExecute(Sender: TObject);
    procedure acAddNewGroupExecute(Sender: TObject);
    procedure acAddNewTabExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acRemoveExecute(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure acResetAllCustomizationsExecute(Sender: TObject);
    procedure acResetSelectedTabExecute(Sender: TObject);
    procedure acShowTabExecute(Sender: TObject);
    procedure acUpdateActionsStateExecute(Sender: TObject);
    procedure cbCommandsPropertiesChange(Sender: TObject);
    procedure cbRibbonPropertiesChange(Sender: TObject);
    procedure chbShowQATBelowRibbonPropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tlCommandsBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure tlCommandsCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure tlCommandsCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
      AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure tlCommandsDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
    procedure tlCommandsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tlCommandsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure tlCommandsFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure tlRibbonDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tlRibbonMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode;
      AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
    procedure tlRibbonNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
    procedure tlRibbonStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn;
      ANode: TcxTreeListNode; var AStyle: TcxStyle);
  private
    FHelper: TdxCustomRibbonCustomizationFormHelper;
    FIsRibbonCustomization: Boolean;
    FRibbon: TdxCustomRibbon;

    function GetRibbonCustomizationHelper: TdxRibbonCustomizationFormHelper;
    function GetRibbonQATCustomizationHelper: TdxRibbonQATCustomizationFormHelper;
  protected
    procedure ApplyChanges;
    procedure ApplyLocalization;
    procedure Initialize;
    procedure UpdateActionsState(const AChanged: Boolean = True);
    procedure UpdateElementsVisibility;

    property Helper: TdxCustomRibbonCustomizationFormHelper read FHelper;
    property IsRibbonCustomization: Boolean read FIsRibbonCustomization;
    property Ribbon: TdxCustomRibbon read FRibbon write FRibbon;
    property RibbonCustomizationHelper: TdxRibbonCustomizationFormHelper read GetRibbonCustomizationHelper;
    property RibbonQATCustomizationHelper: TdxRibbonQATCustomizationFormHelper read GetRibbonQATCustomizationHelper;
  public
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce; overload;
    constructor Create(ARibbonQAT: TdxRibbonQuickAccessToolbar); reintroduce; overload;
    destructor Destroy; override;
  end;

function ShowRibbonCustomizationForm(ARibbon: TdxCustomRibbon; const AMode: TdxRibbonCustomizationFormMode): Boolean;

implementation

{$R *.dfm}

function ShowRibbonCustomizationForm(ARibbon: TdxCustomRibbon; const AMode: TdxRibbonCustomizationFormMode): Boolean;

  procedure DoShowCustomizationForm;
  begin
    if Assigned(ARibbon.BarManager.OnShowCustomizingForm) then
      ARibbon.BarManager.OnShowCustomizingForm(ARibbon.BarManager);
  end;

  procedure DoHideCustomizationForm;
  begin
    if Assigned(ARibbon.BarManager.OnHideCustomizingForm) then
      ARibbon.BarManager.OnHideCustomizingForm(ARibbon.BarManager);
  end;

var
  AForm: TdxRibbonCustomizationForm;
begin
  DoShowCustomizationForm;
  if AMode = rcfmCustomizeRibbon then
    AForm := TdxRibbonCustomizationForm.Create(ARibbon)
  else
    AForm := TdxRibbonCustomizationForm.Create(ARibbon.QuickAccessToolbar);
  try
    Result := AForm.ShowModal = mrOk;
    if Result then
      AForm.ApplyChanges;
  finally
    AForm.Free;
    DoHideCustomizationForm;
  end;
end;

{ TdxRibbonCustomizationForm }

constructor TdxRibbonCustomizationForm.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(nil);
  FRibbon := ARibbon;
  FIsRibbonCustomization := True;
  FHelper := TdxRibbonCustomizationFormHelper.Create(Ribbon, tlCommands, tlRibbon, cbCommands, cbRibbon);
end;

constructor TdxRibbonCustomizationForm.Create(ARibbonQAT: TdxRibbonQuickAccessToolbar);
begin
  inherited Create(nil);
  FRibbon := ARibbonQAT.Ribbon;
  FIsRibbonCustomization := False;
  FHelper := TdxRibbonQATCustomizationFormHelper.Create(Ribbon, tlCommands, tlRibbon, cbCommands, cbRibbon);
end;

destructor TdxRibbonCustomizationForm.Destroy;
begin
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TdxRibbonCustomizationForm.ApplyChanges;
begin
  Helper.BeginApplyChanges;
  try
    Helper.CreateAddedElements;
    Helper.DeleteRemovedElements;
    Helper.SynchronizeElementCaptions;
    Helper.ReorderElements;
    if not IsRibbonCustomization then
      RibbonQATCustomizationHelper.ChangeQATPosition(chbShowQATBelowRibbon.Checked);
    Helper.UpdateRibbonActiveTab;
  finally
    Helper.EndApplyChanges;
    SetDesignerModified(Ribbon);
  end;
end;

procedure TdxRibbonCustomizationForm.ApplyLocalization;
begin
  if IsRibbonCustomization then
    Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionRibbonTitle)
  else
    Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionQuickAccessToolbarTitle);

  acAdd.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionAdd);
  acAddNewContext.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionAddNewContext);
  acAddNewGroup.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionAddNewGroup);
  acAddNewTab.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionAddNewTab);
  acMoveDown.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionMoveDown);
  acMoveUp.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionMoveUp);
  acRemove.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionRemove);
  acRename.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionRename);
  acResetAllCustomizations.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionResetAllCustomizations);
  acResetSelectedTab.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionResetSelectedTab);
  acShowTab.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionShowTab);

  bbtnResetOnlySelectedTab.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionResetOnlySelectedTab);
  btnAdd.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionAdd);
  btnCancel.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionCancel);
  btnNewElement.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionNewElement);
  btnOK.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionOK);
  btnReset.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionReset);

  lciCommandsSource.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionCommandsSource);
  lciRibbonSource.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionRibbonSource);
  lbRibbonQAT.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionQuickAccessToolbar);
  chbShowQATBelowRibbon.Caption := cxGetResourceString(@sdxRibbonCustomizationFormCaptionQuickAccessToolbarShowBelowRibbon);
end;

procedure TdxRibbonCustomizationForm.Initialize;
begin
  Helper.Initialize;
  stNodeDisabled.TextColor := Ribbon.BarManager.LookAndFeel.Painter.DefaultCommandLinkTextColor(cxbsDisabled);
  if not IsRibbonCustomization then
    RibbonQATCustomizationHelper.InitializeQATPositionIndicator(chbShowQATBelowRibbon);
  ApplyLocalization;
  UpdateElementsVisibility;
  UpdateActionsState(False);
end;

procedure TdxRibbonCustomizationForm.UpdateActionsState(const AChanged: Boolean = True);
var
  ALevel: Integer;
begin
  if Helper.CanUpdateContent then
  begin
    btnOK.Enabled := btnOK.Enabled or AChanged;
    Helper.CheckFocusedNode;
    acAdd.Enabled := Helper.CanMoveNodeTo(tlCommands.FocusedNode, tlRibbon.FocusedNode);
    acMoveDown.Enabled := Helper.CanMoveFocusedNodeDown;
    acMoveUp.Enabled := Helper.CanMoveFocusedNodeUp;
    acRemove.Enabled := Helper.CanRemoveFocusedNode;
    acRename.Enabled := Helper.CanRenameFocusedNode;
    if IsRibbonCustomization then
    begin
      ALevel := Helper.GetNodeLevel(tlRibbon.FocusedNode);
      acAddNewGroup.Enabled := ALevel > dxrcfContextLevel;
      acAddNewTab.Enabled := ALevel >= dxrcfContextLevel;
      acResetSelectedTab.Enabled := RibbonCustomizationHelper.CanResetFocusedNode;
      acShowTab.Visible := IsRibbonCustomization and (ALevel = dxrcfTabLevel);
      acShowTab.Checked := acShowTab.Visible and tlRibbon.FocusedNode.Checked;
      RibbonCustomizationHelper.ExpandAllContexts;
    end;
  end;
end;

procedure TdxRibbonCustomizationForm.UpdateElementsVisibility;
begin
  lciNewElement.Visible := IsRibbonCustomization;
  lciRename.Visible := IsRibbonCustomization;
  lciRibbonSource.Visible := IsRibbonCustomization;
  lciRibbonQAT.Visible := not IsRibbonCustomization;
  lciShowQATBelowRibbon.Visible := not IsRibbonCustomization;

  acAddNewContext.Visible := IsRibbonCustomization;
  acAddNewGroup.Visible := IsRibbonCustomization;
  acAddNewTab.Visible := IsRibbonCustomization;
  bsUpperSeparator.Visible := VisibleTodxBarVisible(IsRibbonCustomization);
  acRename.Visible := IsRibbonCustomization;
  acResetSelectedTab.Visible := IsRibbonCustomization;
  acShowTab.Visible := IsRibbonCustomization;
end;

function TdxRibbonCustomizationForm.GetRibbonCustomizationHelper: TdxRibbonCustomizationFormHelper;
begin
  Result := TdxRibbonCustomizationFormHelper(Helper);
end;

function TdxRibbonCustomizationForm.GetRibbonQATCustomizationHelper: TdxRibbonQATCustomizationFormHelper;
begin
  Result := TdxRibbonQATCustomizationFormHelper(Helper);
end;

{ Events }

procedure TdxRibbonCustomizationForm.acAddExecute(Sender: TObject);
begin
  Helper.AddNode(tlCommands.FocusedNode);
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acAddNewContextExecute(Sender: TObject);
begin
  RibbonCustomizationHelper.AddNewContext;
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acAddNewGroupExecute(Sender: TObject);
begin
  RibbonCustomizationHelper.AddNewGroup;
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acAddNewTabExecute(Sender: TObject);
begin
  RibbonCustomizationHelper.AddNewTab;
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acMoveDownExecute(Sender: TObject);
begin
  Helper.MoveFocusedNodeDown;
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acMoveUpExecute(Sender: TObject);
begin
  Helper.MoveFocusedNodeUp;
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acRemoveExecute(Sender: TObject);
begin
  Helper.RemoveNode(tlRibbon.FocusedNode);
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acRenameExecute(Sender: TObject);
var
  ACaption: string;
  AFocusedNode: TcxTreeListNode;
begin
  AFocusedNode := tlRibbon.FocusedNode;
  ACaption := AFocusedNode.Texts[0];
  if dxInputQuery(cxGetResourceString(@sdxRibbonCustomizationFormRename),
    cxGetResourceString(@sdxRibbonCustomizationFormDisplayName), ACaption) then
  begin
    Helper.RenameNode(AFocusedNode, ACaption);
    UpdateActionsState;
  end;
end;

procedure TdxRibbonCustomizationForm.acResetAllCustomizationsExecute(Sender: TObject);
begin
  Helper.PopulateRibbonTreeListContent(True);
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acResetSelectedTabExecute(Sender: TObject);
var
  AFocusedTabNode: TcxTreeListNode;
begin
  AFocusedTabNode := tlRibbon.FocusedNode;
  while AFocusedTabNode.Level > dxrcfTabLevel do
    AFocusedTabNode := AFocusedTabNode.Parent;
  RibbonCustomizationHelper.ResetTabNode(AFocusedTabNode);
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.acShowTabExecute(Sender: TObject);
begin
  tlRibbon.FocusedNode.Checked := acShowTab.Checked;
end;

procedure TdxRibbonCustomizationForm.acUpdateActionsStateExecute(Sender: TObject);
begin
  UpdateActionsState(False);
end;

procedure TdxRibbonCustomizationForm.cbCommandsPropertiesChange(Sender: TObject);
begin
  if Helper.CanUpdateContent then
    Helper.PopulateCommandsTreeListContent;
end;

procedure TdxRibbonCustomizationForm.cbRibbonPropertiesChange(Sender: TObject);
begin
  if IsRibbonCustomization then
    RibbonCustomizationHelper.UpdateContextsVisibility;
  UpdateActionsState(False);
end;

procedure TdxRibbonCustomizationForm.chbShowQATBelowRibbonPropertiesChange(Sender: TObject);
begin
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.FormShow(Sender: TObject);
begin
  Initialize;
end;

procedure TdxRibbonCustomizationForm.tlCommandsCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := Helper.GetNodeLevel(ANode) <> dxrcfContextLevel;
end;

procedure TdxRibbonCustomizationForm.tlCommandsCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  case Helper.GetNodeLevel(AViewInfo.Node, True) of
    dxrcfContextLevel:
      ADone := RibbonCustomizationHelper.DrawContextNode(Sender, ACanvas, AViewInfo, dxLayoutCxLookAndFeel.LookAndFeelPainter);
    dxrcfItemLevel:
      ADone := Helper.DrawItemNode(Sender, ACanvas, AViewInfo, dxLayoutCxLookAndFeel.LookAndFeelPainter, ScaleFactor);
  end;
end;

procedure TdxRibbonCustomizationForm.tlCommandsFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  UpdateActionsState(False);
end;

procedure TdxRibbonCustomizationForm.tlCommandsBeginDragNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := ((ANode.Data <> nil) or (Sender = tlRibbon)) and (Helper.GetNodeLevel(ANode) > dxrcfContextLevel);
  if Allow then
    Helper.InitializeMovedNode(ANode);
end;

procedure TdxRibbonCustomizationForm.tlCommandsDeletion(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
begin
  Helper.FreeNodeData(ANode);
end;

procedure TdxRibbonCustomizationForm.tlCommandsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TdxRibbonCustomizationForm.tlCommandsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if (Target = tlRibbon) and (Sender = tlCommands) then
    Helper.DropMovedNodeTo(tlRibbon.HitTest.HitNode, Y);
  Helper.ReleaseMovedNode;
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.tlRibbonDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Helper.CanMoveNodeTo(Helper.MovedNode, tlRibbon.HitTest.HitNode, Y);
end;

procedure TdxRibbonCustomizationForm.tlRibbonMoveTo(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode;
  AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean);
begin
  Done := True;
  if AttachNode.TreeList = tlRibbon then
    Helper.DropMovedNodeTo(tlRibbon.HitTest.HitNode, tlRibbon.ScreenToClient(GetMouseCursorPos).Y);
end;

procedure TdxRibbonCustomizationForm.tlRibbonNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
  AState: TcxCheckBoxState);
begin
  UpdateActionsState;
end;

procedure TdxRibbonCustomizationForm.tlRibbonStylesGetContentStyle(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
begin
  if not ANode.Enabled then
    AStyle := stNodeDisabled;
end;

initialization
  FShowRibbonCustomizationFormFunc := ShowRibbonCustomizationForm;

finalization
  FShowRibbonCustomizationFormFunc := nil;

end.
