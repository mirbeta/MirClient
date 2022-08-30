{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl customize form                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutCustomizeForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Contnrs, Controls, Forms, Dialogs,
  ComCtrls, ActnList, Menus, ToolWin, StdCtrls, ImgList,
  cxClasses, cxContainer, cxControls, cxGraphics, dxComCtrlsUtils, cxLookAndFeels, cxLookAndFeelPainters,
  cxButtons, cxEdit, cxGroupBox, cxTreeView, cxCheckBox,
  dxLayoutLookAndFeels, dxLayoutControl, dxLayoutDragAndDrop, dxLayoutcxEditAdapters, dxLayoutContainer,
  dxLayoutControlAdapters, cxImageList;

type
  TdxLayoutControlCustomizeForm = class(TdxLayoutControlCustomCustomizeForm)
    alMain: TActionList;
    acAddGroup: TAction;
    acAddItem: TAction;
    acClose: TAction;
    acTreeViewExpandAll: TAction;
    acTreeViewCollapseAll: TAction;
    acTreeViewItemsDelete: TAction;
    acAlignLeftSide: TAction;
    acAlignRightSide: TAction;
    acAlignTopSide: TAction;
    acAlignBottomSide: TAction;
    acAlignNone: TAction;
    ilActions: TcxImageList;
    ilItems: TcxImageList;
    pmTreeViewActions: TPopupMenu;
    miExpandAll: TMenuItem;
    miCallapseAll: TMenuItem;
    miSeparator1: TMenuItem;
    miTreeViewDelete: TMenuItem;
    miSeparator2: TMenuItem;
    miAlignBy: TMenuItem;
    pmAvailableItemsActions: TPopupMenu;
    AddGroup1: TMenuItem;
    AddItem1: TMenuItem;
    Delete1: TMenuItem;
    pmAlign: TPopupMenu;
    Left1: TMenuItem;
    Right1: TMenuItem;
    op1: TMenuItem;
    Bottom1: TMenuItem;
    miSeparator3: TMenuItem;
    miSeparator4: TMenuItem;
    None1: TMenuItem;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    lcgTreeView: TdxLayoutGroup;
    lcgAvailableItems: TdxLayoutGroup;
    acAvailableItemsDelete: TAction;
    acAvailableItemsExpandAll: TAction;
    acAvailableItemsCollapseAll: TAction;
    acAvailableItemsViewAsList: TAction;
    acTabbedView: TAction;
    acHighlightRoot: TAction;
    lcMainGroup1: TdxLayoutGroup;
    acShowDesignSelectors: TAction;
    acStore: TAction;
    acRestore: TAction;
    ilHelper: TcxImageList;
    acTreeViewItemRename: TAction;
    miTreeViewItemRename: TMenuItem;
    Rename2: TMenuItem;
    acAvailableItemRename: TAction;
    acUndo: TAction;
    acRedo: TAction;
    N1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N2: TMenuItem;
    Undo2: TMenuItem;
    Redo2: TMenuItem;
    acAlignBy: TAction;
    tvVisibleItems: TcxTreeView;
    lcMainItem6: TdxLayoutItem;
    tvAvailableItems: TcxTreeView;
    lcMainItem8: TdxLayoutItem;
    btnClose: TcxButton;
    lcMainItem1: TdxLayoutItem;
    lcMainGroup3: TdxLayoutGroup;
    cbTabbedView: TcxCheckBox;
    lcMainItem4: TdxLayoutItem;
    liShowDesignSelectors: TdxLayoutItem;
    btnShowDesignSelectors: TcxButton;
    liHighlightRoot: TdxLayoutItem;
    btnHighlightRoot: TcxButton;
    liRestore: TdxLayoutItem;
    btnRestore: TcxButton;
    liStore: TdxLayoutItem;
    btnStore: TcxButton;
    liRedo: TdxLayoutItem;
    btnRedo: TcxButton;
    liUndo: TdxLayoutItem;
    btnUndo: TcxButton;
    lcMainGroup2: TdxLayoutGroup;
    liAlignBy: TdxLayoutItem;
    btnAlignBy: TcxButton;
    lcMainItem7: TdxLayoutItem;
    btnTreeViewItemsDelete: TcxButton;
    lcMainItem9: TdxLayoutItem;
    btnTreeViewCollapseAll: TcxButton;
    lcMainItem10: TdxLayoutItem;
    btnTreeViewExpandAll: TcxButton;
    lgTreeView: TdxLayoutGroup;
    lgAvailableItems: TdxLayoutGroup;
    lcMainItem3: TdxLayoutItem;
    btnAvailableItemsViewAsList: TcxButton;
    lcMainItem11: TdxLayoutItem;
    btnAvailableItemsDelete: TcxButton;
    liAddCustomItem: TdxLayoutItem;
    btnAddItem: TcxButton;
    lcMainItem13: TdxLayoutItem;
    btnAddGroup: TcxButton;
    lcMainItem14: TdxLayoutItem;
    btnAvailableItemsCollapseAll: TcxButton;
    lcMainItem15: TdxLayoutItem;
    btnAvailableItemsExpandAll: TcxButton;
    acHAlignLeft: TAction;
    acHAlignCenter: TAction;
    acHAlignRight: TAction;
    acHAlignClient: TAction;
    acHAlignParent: TAction;
    miAlignHorz: TMenuItem;
    miHLeft: TMenuItem;
    miHCenter: TMenuItem;
    miHRight: TMenuItem;
    miHClient: TMenuItem;
    miHParentManaged: TMenuItem;
    acVAlignTop: TAction;
    acVAlignBottom: TAction;
    acVAlignCenter: TAction;
    acVAlignClient: TAction;
    acVAlignParent: TAction;
    miAlignVert: TMenuItem;
    miVAlignTop: TMenuItem;
    miVAlignCenter: TMenuItem;
    miVAlignBottom: TMenuItem;
    miVAlignClient: TMenuItem;
    miVAlignParent: TMenuItem;
    acDirectionHorizontal: TAction;
    acDirectionVertical: TAction;
    acDirectionTabbed: TAction;
    miDirection: TMenuItem;
    miDirectionHorizontal: TMenuItem;
    miDirectionVertical: TMenuItem;
    miDirectionTabbed: TMenuItem;
    acAddEmptySpaceItem: TAction;
    acBorder: TAction;
    miBorder: TMenuItem;
    acAddSeparator: TAction;
    AddEmptySpaceItem1: TMenuItem;
    acAddSeparator1: TMenuItem;
    acAddSplitter: TAction;
    acAddLabeledItem: TAction;
    pmAddCustomItem: TPopupMenu;
    AddEmptySpaceItem2: TMenuItem;
    acAddLabeledItem1: TMenuItem;
    acAddSeparator2: TMenuItem;
    acAddSplitter1: TMenuItem;
    acAddCustomItem: TAction;
    AddSplitter1: TMenuItem;
    AddLabel1: TMenuItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcMainSeparatorItem2: TdxLayoutSeparatorItem;
    N3: TMenuItem;
    lcMainSeparatorItem3: TdxLayoutSeparatorItem;
    lsAlignBy: TdxLayoutSeparatorItem;
    N4: TMenuItem;
    ExpandAll1: TMenuItem;
    CallapseAll1: TMenuItem;
    liAddItem: TdxLayoutItem;
    cxButton1: TcxButton;
    lsSeparator4: TdxLayoutSeparatorItem;
    acExpandButton: TAction;
    miExpandButton: TMenuItem;
    miTextPosition: TMenuItem;
    miTextPositionLeft: TMenuItem;
    miTextPositionTop: TMenuItem;
    miTextPositionRight: TMenuItem;
    miTextPositionBottom: TMenuItem;
    acTextPositionLeft: TAction;
    acTextPositionTop: TAction;
    acTextPositionRight: TAction;
    acTextPositionBottom: TAction;
    miCaptionAlignHorz: TMenuItem;
    miCaptionAlignHorzLeft: TMenuItem;
    miCaptionAlignHorzCenter: TMenuItem;
    miCaptionAlignHorzRight: TMenuItem;
    acCaptionAlignHorzLeft: TAction;
    acCaptionAlignHorzCenter: TAction;
    acCaptionAlignHorzRight: TAction;
    miCaption: TMenuItem;
    acCaption: TAction;
    miCaptionAlignVert: TMenuItem;
    miCaptionAlignVertTop: TMenuItem;
    miCaptionAlignVertCenter: TMenuItem;
    miCaptionAlignVertBottom: TMenuItem;
    acCaptionAlignVertTop: TAction;
    acCaptionAlignVertCenter: TAction;
    acCaptionAlignVertBottom: TAction;
    miGroup: TMenuItem;
    miUngroup: TMenuItem;
    acGroup: TAction;
    acUngroup: TAction;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    AddImage1: TMenuItem;
    acAddImage: TAction;
    AddImage2: TMenuItem;
    siMainSplitter: TdxLayoutSplitterItem;
    acVisibleItemsMakeFloat: TAction;
    acAvailableItemsMakeFloat: TAction;
    cxButton2: TcxButton;
    liVisibleItemsMakeFloat: TdxLayoutItem;
    liAvailableItemsMakeFloat: TdxLayoutItem;
    cxButton3: TcxButton;
    liShowItemNames: TdxLayoutItem;
    cxButton4: TcxButton;
    acShowItemNames: TAction;
    miCollapsible: TMenuItem;
    acCollapsible: TAction;

    procedure acCloseExecute(Sender: TObject);
    procedure acAddGroupExecute(Sender: TObject);
    procedure acAddItemExecute(Sender: TObject);

    procedure tvVisibleItemsEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure tvVisibleItemsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvVisibleItemsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tvVisibleItemsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvVisibleItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvVisibleItemsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure tvVisibleItemsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvVisibleItemsCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvVisibleItemsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure tvVisibleItemsDeletion(Sender: TObject; Node: TTreeNode);

    procedure tvAvailableItemsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure pmTreeViewActionsPopup(Sender: TObject);

    procedure AlignExecute(Sender: TObject);
    procedure acTreeViewItemsDeleteExecute(Sender: TObject);
    procedure acAvailableItemsDeleteExecute(Sender: TObject);
    procedure acAvailableItemsExpandAllExecute(Sender: TObject);
    procedure acAvailableItemsCollapseAllExecute(Sender: TObject);
    procedure acTreeViewExpandAllExecute(Sender: TObject);
    procedure acTreeViewCollapseAllExecute(Sender: TObject);
    procedure acAvailableItemsViewAsListExecute(Sender: TObject);
    procedure acTabbedViewExecute(Sender: TObject);
    procedure acHighlightRootExecute(Sender: TObject);
    procedure acShowDesignSelectorsExecute(Sender: TObject);
    procedure acStoreExecute(Sender: TObject);
    procedure acRestoreExecute(Sender: TObject);
    procedure acTreeViewItemRenameExecute(Sender: TObject);
    procedure acAvailableItemRenameExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acAlignByExecute(Sender: TObject);
    procedure acHAlignExecute(Sender: TObject);
    procedure acVAlignExecute(Sender: TObject);
    procedure acDirectionsExecute(Sender: TObject);
    procedure acAddEmptySpaceItemExecute(Sender: TObject);
    procedure acBorderExecute(Sender: TObject);
    procedure acAddSeparatorExecute(Sender: TObject);
    procedure acAddCustomItemExecute(Sender: TObject);
    procedure acAddLabeledItemExecute(Sender: TObject);
    procedure acAddSplitterExecute(Sender: TObject);
    procedure acExpandButtonExecute(Sender: TObject);
    procedure acTextPositionExecute(Sender: TObject);
    procedure acCaptionAlignHorzExecute(Sender: TObject);
    procedure acCaptionExecute(Sender: TObject);
    procedure acCaptionAlignVertExecute(Sender: TObject);
    procedure acGroupExecute(Sender: TObject);
    procedure acUngroupExecute(Sender: TObject);
    procedure acAddImageExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acVisibleItemsMakeFloatExecute(Sender: TObject);
    procedure acAvailableItemsFloatExecute(Sender: TObject);
    procedure acShowItemNamesExecute(Sender: TObject);
    procedure pmAvailableItemsActionsPopup(Sender: TObject);
    procedure acCollapsibleExecute(Sender: TObject);
  private
    FDragHelper: TdxLayoutDragAndDropHelper;
    FAvailableItemsWindowProcLinkedObject: TcxWindowProcLinkedObject;
    FItemsWindowProcLinkedObject: TcxWindowProcLinkedObject;
    FOriginalImageCount: Integer;

    procedure AddItemNode(ANodes: TTreeNodes; AParentNode: TTreeNode; AItem: TdxCustomLayoutItem; AAddChildren: Boolean = True);
    procedure AvailableItemsWndProc(var Message: TMessage);
    function CreateItem(AClass: TdxCustomLayoutItemClass; const ACaption: string): TdxCustomLayoutItem;
    function DoCreateItem(AClass: TdxCustomLayoutItemClass; const ACaption: string): TdxCustomLayoutItem;
    procedure DoAfterInsertionItem(AItem: TdxCustomLayoutItem);
    function GetImageIndex(AItem: TdxCustomLayoutItem): Integer;
    function GetMenuItems: TdxLayoutCustomizeFormMenuItems;
    function HasClassInSelection(AClass: TClass): Boolean;
    procedure InitializePopupMenu;
    function IsHiddenGroup(AItem: TdxCustomLayoutItem): Boolean;
    procedure ItemsWndProc(var Message: TMessage);
    procedure RefreshImages;
    procedure RefreshLists(ARefreshSelection: Boolean = False);
    procedure RefreshNode(ANode: TTreeNode);
    procedure RefreshButtonStates;
    procedure RefreshView;

    // Selection
    function CanSelectItem(AItem: TdxCustomLayoutItem): Boolean;
    procedure SelectItem(AItem: TdxCustomLayoutItem);
    procedure SetItemsSelections(AList: TList);
    procedure SetLayoutItemsSelections(ATreeView: TTreeView);

    function TreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage): Boolean;

    procedure RestoreCollapsedNodes(ATreeView: TcxTreeView; AList: TcxComponentList);
    procedure StoreCollapsedNodes(ATreeView: TcxTreeView; AList: TcxComponentList);
    procedure RestoreFirstNode(ATreeView: TcxTreeView; AItem: TdxCustomLayoutItem);
    procedure StoreFirstNode(ATreeView: TcxTreeView; out AItem: TdxCustomLayoutItem);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function CanModify: Boolean; override;
    procedure DoInitializeControl; override;
    procedure ItemChanged(AItem: TdxCustomLayoutItem); override;
    procedure InitializeControl; override;
    function GetLayoutPopupMenu: TPopupMenu; override;
    function CanDeleteItems(ATreeView: TcxTreeView): Boolean;
    procedure DeleteItems(AList: TComponentList; ATreeView: TTreeView);
    function CanFloatItems(ATreeView: TcxTreeView; out ANeedCheck: Boolean): Boolean;
    procedure MakeBreakFloat(AItems: TcxTreeView);
    procedure Changed; override;
    procedure ResetDragAndDropObjects; override;
    function GetCustomizationCaption(AItem: TdxCustomLayoutItem): string; override;
    procedure SetCustomizationCaption(AItem: TdxCustomLayoutItem; const ACaption: string); override;

    function FindNodeByItem(AItem: TdxCustomLayoutItem; out ANode: TTreeNode): Boolean;
    procedure Localize; virtual;

    procedure RefreshItemsTreeView(ATreeView: TcxTreeView; AList: Tlist; AViewKind: TdxLayoutAvailableItemsViewKind; ANeedSort, AShowRoot: Boolean);
    procedure RefreshAvailableItems;
    procedure RefreshVisibleItems;

    procedure RefreshEnableds; virtual;
    procedure RefreshLayoutLookAndFeel; override;
    procedure RefreshStoring; virtual;

    // popup menu
    procedure CalculateTreeViewPopupActionEnables; virtual;
    procedure CalculateTreeViewPopupActionVisibilities; virtual;
    function HasDirectionalItemInSelection: Boolean;
    function HasGroupInSelection: Boolean;
    function HasGroupsOnly: Boolean;
    function HasHiddenGroupInSelection: Boolean;
    function HasLabeledItemInSelection: Boolean;
    function HasLockedGroupInSelection: Boolean;
    function HasLockedItemInSelection: Boolean;
    function HasRootInSelection: Boolean;
    function HasSplitterOnly: Boolean;
    procedure SynchronizeTreeViewPopupActionStates; virtual;

    procedure StoreTreeViewWndProc(ATreeView: TTreeView; out AWindowProcObject: TcxWindowProcLinkedObject; ANewWndMethod: TWndMethod);
    procedure RestoreTreeViewWndProc(var AWindowProcObject: TcxWindowProcLinkedObject);

    procedure SaveToUndo;

    property DragHelper: TdxLayoutDragAndDropHelper read FDragHelper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize; override;
    function GetHitTest(const P: TPoint): TdxCustomLayoutHitTest; override;
    procedure ToggleHotTrackState(AItem: TdxCustomLayoutItem); override;

    procedure UpdateAvailableItems; override;
    procedure UpdateCaption; override;
    procedure UpdateContent; override;
    procedure UpdateDragAndDropState; override;
    procedure UpdateSelection; override;
    procedure UpdateView; override;
    procedure UpdateVisibleItems; override;
  end;

implementation

{$R *.DFM}

uses
  Types, Math, CommCtrl, dxLayoutCommon, dxLayoutEditForm, cxGeometry,
  dxLayoutStrs, dxCore, dxOffice11, dxLayoutSelection;

type
  TdxLayoutDragAndDropObjectAccess = class(TdxLayoutDragAndDropObject);
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TdxLayoutContainerAccess = class(TdxLayoutContainer);
  TdxCustomLayoutItemCaptionOptionsAccess = class(TdxCustomLayoutItemCaptionOptions);
  TToolBarAccess = class(TToolBar);
  TdxLayoutCustomizeFormHitTestAccess = class(TdxLayoutCustomizeFormHitTest);
  TdxLayoutSeparatorItemAccess = class(TdxLayoutSeparatorItem);
  TdxCustomLayoutGroupAccess = class(TdxCustomLayoutGroup);
  TdxLayoutSplitterItemAccess = class(TdxLayoutSplitterItem);

function GetCaptionOptions(AItem: TdxCustomLayoutItem): TdxCustomLayoutItemCaptionOptionsAccess;
begin
  Result := TdxCustomLayoutItemCaptionOptionsAccess(AItem.CaptionOptions);
end;

function CompareItemsByClass(Item1, Item2: Pointer): Integer;
var
  AItem1, AItem2: TdxCustomLayoutItem;
begin
  AItem1 := TdxCustomLayoutItem(Item1);
  AItem2 := TdxCustomLayoutItem(Item2);
  Result := Integer(AItem1.ClassType) - Integer(AItem2.ClassType);
end;

function CompareItemsByName(Item1, Item2: Pointer): Integer;

  procedure SplitNameByTrailingDigits(const AName: string; out ATrailingDigits: Integer;
    out ANameWithoutTrailingDigitals: string);
  var
    ACurrentLength: Integer;
  begin
    ANameWithoutTrailingDigitals := AName;
    ACurrentLength := Length(ANameWithoutTrailingDigitals);
    while (ACurrentLength > 0) and
      dxCharIsNumeric(ANameWithoutTrailingDigitals[ACurrentLength]) do
    begin
      ANameWithoutTrailingDigitals := Copy(ANameWithoutTrailingDigitals, 1, ACurrentLength - 1);
      ACurrentLength := Length(ANameWithoutTrailingDigitals);
    end;
    if (ACurrentLength = Length(AName)) or
        not TryStrToInt(Copy(AName, ACurrentLength + 1, Length(AName) - ACurrentLength), ATrailingDigits) then
      ATrailingDigits := -1;
  end;

var
  AName1, AName2: string;
  ANameWithoutLastDigitals1, ANameWithoutLastDigitals2: string;
  ANumber1, ANumber2: Integer;
begin
  Result := CompareItemsByClass(Item1, Item2);
  if Result = 0 then
  begin
    AName1 := TdxCustomLayoutItem(Item1).CaptionForCustomizeForm;
    AName2 := TdxCustomLayoutItem(Item2).CaptionForCustomizeForm;
    if AName1 = AName2 then
    begin
      AName1 := AName1 + TdxCustomLayoutItem(Item1).Name;
      AName2 := AName2 + TdxCustomLayoutItem(Item2).Name;
    end;
    SplitNameByTrailingDigits(AName1, ANumber1, ANameWithoutLastDigitals1);
    SplitNameByTrailingDigits(AName2, ANumber2, ANameWithoutLastDigitals2);
    Result := CompareText(ANameWithoutLastDigitals1, ANameWithoutLastDigitals2);
    if Result = 0 then
      Result := ANumber1 - ANumber2;
  end;
end;

function CompareItemsByIndex(Item1, Item2: Pointer): Integer;
var
  AItem1, AItem2: TdxCustomLayoutItem;
begin
  AItem1 := TdxCustomLayoutItem(Item1);
  AItem2 := TdxCustomLayoutItem(Item2);
  Result := AItem1.Index - AItem2.Index;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
begin
  Result := CompareItemsByClass(Item1, Item2);
  if (Result = 0) and (TObject(Item1) is TdxCustomLayoutGroup) then
    Result := Integer(TdxCustomLayoutGroup(Item1).Count > 0) - Integer(TdxCustomLayoutGroup(Item2).Count > 0);
  if Result = 0 then
    Result := CompareItemsByName(Item1, Item2);
end;

{ TdxLayoutDesignForm }

constructor TdxLayoutControlCustomizeForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  tvVisibleItems.InnerTreeView.PopupMenu := pmTreeViewActions;
  tvAvailableItems.InnerTreeView.PopupMenu := pmAvailableItemsActions;

  StoreTreeViewWndProc(tvVisibleItems.InnerTreeView, FItemsWindowProcLinkedObject, ItemsWndProc);
  StoreTreeViewWndProc(tvAvailableItems.InnerTreeView, FAvailableItemsWindowProcLinkedObject, AvailableItemsWndProc);
end;

destructor TdxLayoutControlCustomizeForm.Destroy;
begin
  RestoreTreeViewWndProc(FItemsWindowProcLinkedObject);
  RestoreTreeViewWndProc(FAvailableItemsWindowProcLinkedObject);

  FreeAndNil(FDragHelper);
  inherited Destroy;
end;

procedure TdxLayoutControlCustomizeForm.Initialize;
begin
  RefreshImages;
  acShowItemNames.Checked := Container.IsDesigning;
  pmTreeViewActions.OnPopup(nil);
  pmAvailableItemsActions.OnPopup(nil);

  inherited;

  lcMain.BeginUpdate;
  try
    Localize;
  finally
    lcMain.EndUpdate;
  end;
  InitializePopupMenu;
end;

function TdxLayoutControlCustomizeForm.GetHitTest(const P: TPoint): TdxCustomLayoutHitTest;

  function IsMenuKeyDown: Boolean;
  begin
    Result := GetAsyncKeyState(VK_MENU) <> 0;
  end;

  function GetDropAreaPart(AHitTestArea: Integer): TdxLayoutDropAreaPart;
  begin
    case AHitTestArea of
      -1:
        Result := apBefore;
      1:
        Result := apAfter;
    else
      Result := apLastChild;
    end;
  end;

  function GetHitTestArea(ANode: TTreeNode; AItem: TdxCustomLayoutItem; P: TPoint): Integer;
  var
    ANodeRect: TRect;
    H: Integer;
  begin
    P := ANode.TreeView.ParentToClient(P, Self);
    ANodeRect := ANode.DisplayRect(False);
    if AItem is TdxCustomLayoutGroup then
    begin
      if AItem.IsRoot then
        Result := 0
      else
      begin
        H := cxRectHeight(ANodeRect) div 4;
        if P.Y < ANodeRect.Top + H then
          Result := -1
        else
          if p.Y > ANodeRect.Bottom - H - 2 then
            Result := 1
          else
            Result := 0;
      end;
    end
    else
    begin
      H := cxRectHeight(ANodeRect) div 2;
      if P.Y < ANodeRect.Top + H then
        Result := -1
      else
        Result := 1
    end;
  end;

var
  AItem: TdxCustomLayoutItem;
  ANode: TTreeNode;
  AHitTest: TdxCustomLayoutHitTest;
  AHitTestArea: Integer;
begin
  ANode := nil;
  if tvVisibleItems.Visible and PtInRect(tvVisibleItems.BoundsRect, P) then
  begin
    Result := TdxLayoutCustomizeFormTreeViewItemsHitTest.Instance;
    ANode := cxTreeViewGetHitNode(tvVisibleItems, P);
  end
  else
    if tvAvailableItems.Visible and PtInRect(tvAvailableItems.BoundsRect, P) then
    begin
      Result := TdxLayoutCustomizeFormAvailableItemsHitTest.Instance;
      ANode := cxTreeViewGetHitNode(tvAvailableItems, P);
    end
    else
      Result := inherited GetHitTest(P);

  if ANode <> nil then
  begin
    AItem := TdxCustomLayoutItem(ANode.Data);
    TdxLayoutCustomizeFormHitTest(Result).Item := AItem;
    if TdxCustomLayoutItemAccess(AItem).IsAvailable then
      AHitTestArea := 0
    else
      AHitTestArea := GetHitTestArea(ANode, AItem, P);
    TdxLayoutCustomizeFormHitTestAccess(Result).FHitTestArea := AHitTestArea;
    TdxLayoutCustomizeFormHitTest(Result).DropAreaPart := GetDropAreaPart(AHitTestArea);
  end
  else
  begin
    TdxLayoutCustomizeFormHitTest(Result).Item := nil;
    AHitTest := lcMain.GetHitTest(P);
    if (AHitTest <> nil) and ((AHitTest.GetDestinationItem = lcgTreeView) or (AHitTest.GetDestinationItem = lcgAvailableItems)) then
      AHitTest.GetDestinationItem.MakeVisible;
  end;

  TdxLayoutCustomizeFormHitTest(Result).Container := Container;
end;

procedure TdxLayoutControlCustomizeForm.ToggleHotTrackState(AItem: TdxCustomLayoutItem);
var
  ANode: TTreeNode;
  R: TRect;
begin
  if FindNodeByItem(AItem, ANode) and ANode.IsVisible then
  begin
    R := ANode.DisplayRect(False);
    if ANode.HasChildren and not ANode.Expanded then
      ANode.Expand(False)
    else
      InvalidateRect(ANode.TreeView.Handle, @R, True);
  end;
end;

procedure TdxLayoutControlCustomizeForm.UpdateAvailableItems;
begin
  RefreshAvailableItems;
end;

procedure TdxLayoutControlCustomizeForm.UpdateCaption;
begin
  if Container.IsDesigning then
    Caption := Format(cxGetResourceString(@sdxLayoutControlDesignerCaptionFormat),
      [cxGetFullComponentName(TdxLayoutContainerAccess(Container).ItemsParentComponent)])
  else
    Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaption);
end;

procedure TdxLayoutControlCustomizeForm.UpdateContent;
begin
  RefreshLists;
end;

procedure TdxLayoutControlCustomizeForm.UpdateDragAndDropState;
begin
  if FDragHelper.DragItem <> nil then
  begin
    if Container.ItemsParent.DragAndDropState = ddsNone then
      FDragHelper.Reset;
  end;
end;

procedure TdxLayoutControlCustomizeForm.UpdateSelection;
var
  AList: TList;
  AIntf: IdxLayoutDesignerHelper;
begin
  if IsLocked then
    Exit;
  AList := TList.Create;
  try
    if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
    begin
      AIntf.GetSelection(AList);
      AIntf := nil;
    end;
    SetItemsSelections(AList);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.UpdateView;
begin
  RefreshView;
end;

procedure TdxLayoutControlCustomizeForm.UpdateVisibleItems;
begin
  RefreshVisibleItems;
end;

procedure TdxLayoutControlCustomizeForm.RefreshStoring;
begin
  acStore.Visible := not Container.IsDesigning;
  liStore.Visible := acStore.Visible;
  acRestore.Visible := not Container.IsDesigning;
  liRestore.Visible := acRestore.Visible;
end;

procedure TdxLayoutControlCustomizeForm.Loaded;
begin
  inherited Loaded;
  Constraints.MinHeight := Height div 2;
  Constraints.MinWidth := Width;
end;

procedure TdxLayoutControlCustomizeForm.Notification(AComponent: TComponent; Operation: TOperation);
var
  ANode: TTreeNode;
  AItem: TdxCustomLayoutItem;
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) and (AComponent is TdxCustomLayoutItem) then
  begin
    AItem := AComponent as TdxCustomLayoutItem;
    if FindNodeByItem(AItem, ANode) then
      ANode.Delete;
  end;
end;

function TdxLayoutControlCustomizeForm.CanDeleteItems(ATreeView: TcxTreeView): Boolean;
var
  AList: TList;
  I: Integer;
  AIntf: IdxLayoutDesignerHelper;
begin
  Result := not ATreeView.IsEditing;
  if Result then
  begin
    AList := TList.Create;
    try
      cxTreeViewGetSelection(ATreeView.InnerTreeView, AList);
      Result := AList.Count > 0;
      if Result then
        if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
        begin
          for I := 0 to AList.Count - 1 do
          begin
            Result := TdxCustomLayoutItemAccess(AList[I]).CanDelete and
              AIntf.CanDeleteComponent(TComponent(AList[I]));
            if not Result then
              Break;
          end;
        end;
    finally
      AList.Free;
    end;
  end;
end;

procedure TdxLayoutControlCustomizeForm.DeleteItems(AList: TComponentList; ATreeView: TTreeView);
var
  AIntf: IdxLayoutDesignerHelper;
begin
  if AList.Count = 0 then
    Exit;
  SaveToUndo;
  BeginUpdate;
  ATreeView.Items.BeginUpdate;
  try
    Container.BeginUpdate;
    try
      if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
        AIntf.DeleteComponents(AList);
    finally
      Container.EndUpdate;
    end;
  finally
    ATreeView.Items.EndUpdate;
    CancelUpdate;
  end;
end;

function TdxLayoutControlCustomizeForm.CanFloatItems(ATreeView: TcxTreeView; out ANeedCheck: Boolean): Boolean;
var
  AList: TList;
  I: Integer;
begin
  ANeedCheck := False;
  Result := False;
  AList := TList.Create;
  try
    cxTreeViewGetSelection(ATreeView.InnerTreeView, AList);
    for I := 0 to AList.Count - 1 do
    begin
      ANeedCheck := TdxCustomLayoutItemAccess(AList[I]).FIsFloat;
      Result := TdxCustomLayoutItemAccess(AList[I]).CanFloat;
      if not Result then
        Break;
    end;
  finally
    AList.Free;
  end;
end;

function TdxLayoutControlCustomizeForm.FindNodeByItem(AItem: TdxCustomLayoutItem; out ANode: TTreeNode): Boolean;
begin
  Result := (Container <> nil) and (TdxCustomLayoutItemAccess(AItem).RealContainer = Container);
  if Result then
    Result := cxTreeViewFindNodeByData(tvAvailableItems.InnerTreeView, AItem, ANode) or
      cxTreeViewFindNodeByData(tvVisibleItems.InnerTreeView, AItem, ANode);
end;

function TdxLayoutControlCustomizeForm.GetLayoutPopupMenu: TPopupMenu;
begin
  Result := pmTreeViewActions;
end;

function TdxLayoutControlCustomizeForm.CanModify: Boolean;
begin
  Result := inherited CanModify and (Container.IsDesigning or not HasLockedItemInSelection);
end;

procedure TdxLayoutControlCustomizeForm.DoInitializeControl;
begin
  FDragHelper.Free;
  FDragHelper := TdxLayoutDragAndDropHelper.Create(Container);
  inherited DoInitializeControl;
  tvVisibleItems.FullExpand;
  tvAvailableItems.FullExpand;
end;

procedure TdxLayoutControlCustomizeForm.ItemChanged(AItem: TdxCustomLayoutItem);
var
  ANode: TTreeNode;
begin
  if FindNodeByItem(AItem, ANode) and not IsLocked then
    RefreshNode(ANode);
  if not IsLocked then
    RefreshButtonStates;
end;

procedure TdxLayoutControlCustomizeForm.InitializeControl;
begin
  lcMain.BeginUpdate;
  try
    inherited InitializeControl;
  finally
    lcMain.EndUpdate;
  end;
end;

procedure TdxLayoutControlCustomizeForm.Localize;
begin
  acAddGroup.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddGroup);
  acAddGroup.Hint := StripHotKey(acAddGroup.Caption);

  acAddItem.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddItem);
  acAddItem.Hint := StripHotKey(acAddItem.Caption);

  acAddCustomItem.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddAuxiliaryItem);
  acAddCustomItem.Hint := StripHotKey(acAddCustomItem.Caption);

  acAddEmptySpaceItem.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddEmptySpaceItem);
  acAddEmptySpaceItem.Hint := StripHotKey(acAddEmptySpaceItem.Caption);

  acAddSeparator.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddSeparatorItem);
  acAddSeparator.Hint := StripHotKey(acAddSeparator.Caption);

  acAddSplitter.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddSplitterItem);
  acAddSplitter.Hint := StripHotKey(acAddSplitter.Caption);

  acAddLabeledItem.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddLabeledItem);
  acAddLabeledItem.Hint := StripHotKey(acAddLabeledItem.Caption);

  acAddImage.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAddImageItem);
  acAddImage.Hint := StripHotKey(acAddImage.Caption);

  acAvailableItemsDelete.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormDelete);
  acAvailableItemsDelete.Hint := cxGetResourceString(@sdxLayoutControlCustomizeFormDeleteHint);

  acTreeViewItemsDelete.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormDelete);
  acTreeViewItemsDelete.Hint := cxGetResourceString(@sdxLayoutControlCustomizeFormDeleteHint);

  acAlignBy.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAlignBy);
  acAlignBy.Hint := StripHotKey(acAlignBy.Caption);

  acClose.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormClose);
  acClose.Hint := StripHotKey(acClose.Caption);

  acTreeViewExpandAll.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormExpandAll);
  acTreeViewExpandAll.Hint := StripHotKey(acTreeViewExpandAll.Caption);

  acAvailableItemsExpandAll.Caption := acTreeViewExpandAll.Caption;
  acAvailableItemsExpandAll.Hint := acTreeViewExpandAll.Hint;

  acTreeViewCollapseAll.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCollapseAll);
  acTreeViewCollapseAll.Hint := StripHotKey(acTreeViewCollapseAll.Caption);

  acAvailableItemsCollapseAll.Caption := acTreeViewCollapseAll.Caption;
  acAvailableItemsCollapseAll.Hint := StripHotKey(acTreeViewCollapseAll.Hint);

  acAlignLeftSide.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAlignLeftSide);
  acAlignRightSide.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAlignRightSide);
  acAlignTopSide.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAlignTopSide);
  acAlignBottomSide.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAlignBottomSide);
  acAlignNone.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormAlignNone);

  miAlignHorz.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormHAlign);
  acHAlignLeft.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormHAlignLeft);
  acHAlignCenter.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormHAlignCenter);
  acHAlignRight.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormHAlignRight);
  acHAlignClient.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormHAlignClient);
  acHAlignParent.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormHAlignParent);

  miAlignVert.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormVAlign);
  acVAlignTop.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormVAlignTop);
  acVAlignCenter.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormVAlignCenter);
  acVAlignBottom.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormVAlignBottom);
  acVAlignClient.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormVAlignClient);
  acVAlignParent.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormVAlignParent);

  miDirection.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormDirection);
  acDirectionHorizontal.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormDirectionHorizontal);
  acDirectionVertical.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormDirectionVertical);
  acDirectionTabbed.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormDirectionTabbed);

  miTextPosition.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTextPosition);
  acTextPositionLeft.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTextPositionLeft);
  acTextPositionTop.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTextPositionTop);
  acTextPositionRight.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTextPositionRight);
  acTextPositionBottom.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTextPositionBottom);

  miCaptionAlignHorz.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignHorz);
  acCaptionAlignHorzLeft.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignHorzLeft);
  acCaptionAlignHorzCenter.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignHorzCenter);
  acCaptionAlignHorzRight.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignHorzRight);

  miCaptionAlignVert.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignVert);
  acCaptionAlignVertTop.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignVertTop);
  acCaptionAlignVertCenter.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignVertCenter);
  acCaptionAlignVertBottom.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormCaptionAlignVertBottom);

  acCaption.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormItemCaption);

  acBorder.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormGroupBorder);
  acExpandButton.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormGroupExpandButton);

  lcgTreeView.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTreeViewGroup);
  lcgAvailableItems.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormListViewGroup);

  acTabbedView.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTabbedView);
  acTabbedView.Hint := StripHotKey(acTabbedView.Caption);

  acAvailableItemsViewAsList.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormTreeView);
  acAvailableItemsViewAsList.Hint := StripHotKey(acAvailableItemsViewAsList.Caption);

  acStore.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormStore);
  acStore.Hint := StripHotKey(acStore.Caption);

  acRestore.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormRestore);
  acRestore.Hint := StripHotKey(acRestore.Caption);

  acTreeViewItemRename.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormRename);
  acTreeViewItemRename.Hint := acTreeViewItemRename.Caption;

  acAvailableItemRename.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormRename);
  acAvailableItemRename.Hint := acAvailableItemRename.Caption;

  acUndo.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormUndo);
  acUndo.Hint := acUndo.Caption;

  acRedo.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormRedo);
  acRedo.Hint := acRedo.Caption;

  acGroup.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormGroup);
  acUngroup.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormUngroup);

  miCollapsible.Caption := cxGetResourceString(@sdxLayoutControlCustomizeFormSplitterCollapsible);
end;

procedure TdxLayoutControlCustomizeForm.RefreshItemsTreeView(ATreeView: TcxTreeView;
  AList: Tlist; AViewKind: TdxLayoutAvailableItemsViewKind; ANeedSort, AShowRoot: Boolean);
var
  I: Integer;
  ACollapsedItems: TcxComponentList;
  ATopItem: TdxCustomLayoutItem;
begin
  BeginUpdate;
  ATreeView.Items.BeginUpdate;
  ACollapsedItems := TcxComponentList.Create;
  try
    StoreFirstNode(ATreeView, ATopItem);
    StoreCollapsedNodes(ATreeView, ACollapsedItems);

    EnableWindow(ATreeView.Handle, False);
    try
      ATreeView.Items.Clear;
    finally
      EnableWindow(ATreeView.Handle, True);
    end;

    if ANeedSort then
      case AViewKind of
        aivkList:
          AList.Sort(CompareItemsByName);
        aivkTree:
          AList.Sort(CompareItems);
      end;
      for I := 0 to AList.Count - 1 do
        AddItemNode(ATreeView.Items, nil, TdxCustomLayoutItem(AList[I]), AViewKind = aivkTree);
    RestoreCollapsedNodes(ATreeView, ACollapsedItems);
    RestoreFirstNode(ATreeView, ATopItem);
  finally
    ACollapsedItems.Free;
    ATreeView.Items.EndUpdate;
    RestoreFirstNode(ATreeView, ATopItem); // treeviewbug
    CancelUpdate;
  end;
  ATreeView.ShowRoot := AShowRoot;
end;

procedure TdxLayoutControlCustomizeForm.RefreshAvailableItems;

  procedure PopulateItemChildren(AList: TList; AItem: TdxCustomLayoutItem);
  var
    I: Integer;
  begin
    if AItem is TdxCustomLayoutGroup then
    begin
      for I := 0 to TdxCustomLayoutGroup(AItem).Count - 1 do
        if CanShowItem(TdxCustomLayoutGroup(AItem).Items[I]) then
        begin
          AList.Add(TdxCustomLayoutGroup(AItem).Items[I]);
          PopulateItemChildren(AList, TdxCustomLayoutGroup(AItem).Items[I]);
        end;
    end;
  end;

  procedure PopulateByAvailableItems(AList: TList);
  var
    I: Integer;
    AItem: TdxCustomLayoutItem;
  begin
    for I := 0 to Container.AvailableItemCount - 1 do
    begin
      AItem := Container.AvailableItems[I];
      if CanShowItem(AItem) then
      begin
        AList.Add(AItem);
        if Container.CustomizeAvailableItemsViewKind = aivkList then
          PopulateItemChildren(AList, AItem);
      end;
    end;
  end;

var
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateByAvailableItems(AList);
    RefreshItemsTreeView(tvAvailableItems, AList, Container.CustomizeAvailableItemsViewKind, True, Container.CustomizeAvailableItemsViewKind = aivkTree);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.RefreshVisibleItems;

  procedure PopulateByVisibleItems(AList: TList);
  var
    I: Integer;
  begin
    AList.Add(Container.Root);
    for I := 0 to TdxLayoutContainerAccess(Container).FloatContainers.Count - 1 do
      AList.Add(TdxLayoutContainer(TdxLayoutContainerAccess(Container).FloatContainers[I]).Root.Items[0]);
  end;

var
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateByVisibleItems(AList);
    RefreshItemsTreeView(tvVisibleItems, AList, aivkTree, False, True);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.RefreshEnableds;
var
  ACanModify: Boolean;
  ACanAddItem: Boolean;
  AStoringSupports: Boolean;
  ACanRestore: Boolean;
  ANeedCheckFloatButton: Boolean;
begin
  if not IsLocked then
  begin
    ACanAddItem := CanAddItem;
    ACanModify := CanModify;
    AStoringSupports := TdxLayoutContainerAccess(Container).StoringSupports;
    ACanRestore := TdxLayoutContainerAccess(Container).CanRestore;
    acAvailableItemsExpandAll.Enabled := Container.CustomizeAvailableItemsViewKind = aivkTree;
    acAvailableItemsCollapseAll.Enabled := Container.CustomizeAvailableItemsViewKind = aivkTree;
    acStore.Enabled := AStoringSupports;
    acRestore.Enabled := AStoringSupports and ACanRestore;
    acTreeViewItemRename.Enabled := ACanModify and (tvVisibleItems.Selected <> nil) and (tvVisibleItems.SelectionCount = 1) and
      not (TObject(tvVisibleItems.Selected.Data) as TdxCustomLayoutItem).IsRoot and
      not TdxCustomLayoutItemAccess(tvVisibleItems.Selected.Data).IsParentLocked and TdxLayoutContainerAccess(Container).AllowRename;
    acAvailableItemRename.Enabled := ACanModify and (tvAvailableItems.Selected <> nil) and (tvAvailableItems.SelectionCount = 1) and
      not TdxCustomLayoutItemAccess(tvAvailableItems.Selected.Data).IsParentLocked and TdxLayoutContainerAccess(Container).AllowRename;

    acUndo.Enabled := Container.UndoRedoManager.CanUndo;
    acRedo.Enabled := Container.UndoRedoManager.CanRedo;

    acTreeViewItemsDelete.Enabled := CanDeleteItems(tvVisibleItems) and ACanAddItem;
    acAvailableItemsDelete.Enabled := CanDeleteItems(tvAvailableItems) and ACanAddItem;
    acVisibleItemsMakeFloat.Enabled := CanFloatItems(tvVisibleItems, ANeedCheckFloatButton);
    if acVisibleItemsMakeFloat.Enabled then
      acVisibleItemsMakeFloat.Checked := ANeedCheckFloatButton;
    acAvailableItemsMakeFloat.Enabled := CanFloatItems(tvAvailableItems, ANeedCheckFloatButton);
    if acAvailableItemsMakeFloat.Enabled then
      acAvailableItemsMakeFloat.Checked := ANeedCheckFloatButton;

    acAddGroup.Enabled := ACanModify and ACanAddItem;
    acAddItem.Enabled := Container.IsDesigning and ACanModify and ACanAddItem;
    acAddSeparator.Enabled := ACanModify and ACanAddItem;
    acAddSplitter.Enabled := ACanModify and ACanAddItem;
    acAddImage.Enabled := ACanModify and ACanAddItem;
    acAddLabeledItem.Enabled := ACanModify and ACanAddItem;
    acAddEmptySpaceItem.Enabled := ACanModify and ACanAddItem;
    acAddCustomItem.Enabled := acAddSeparator.Enabled or acAddSplitter.Enabled or acAddLabeledItem.Enabled or acAddEmptySpaceItem.Enabled;

    acAlignBy.Enabled := (tvVisibleItems.SelectionCount > 1) and ACanModify and ACanAddItem;
    acAlignLeftSide.Enabled := ACanModify and ACanAddItem;
    acAlignRightSide.Enabled := ACanModify and ACanAddItem;
    acAlignTopSide.Enabled := ACanModify and ACanAddItem;
    acAlignBottomSide.Enabled := ACanModify and ACanAddItem;
    acAlignNone.Enabled := ACanModify and ACanAddItem;

    acAvailableItemsViewAsList.Enabled := ACanModify;
    acTabbedView.Enabled := ACanModify;
    acHighlightRoot.Enabled := ACanModify;
    acShowDesignSelectors.Enabled := ACanModify;

    acHAlignLeft.Enabled := ACanModify;
    acHAlignRight.Enabled := ACanModify;
    acHAlignCenter.Enabled := ACanModify;
    acHAlignClient.Enabled := ACanModify;
    acHAlignRight.Enabled := ACanModify;
    acHAlignParent.Enabled := ACanModify;
    acVAlignTop.Enabled := ACanModify;
    acVAlignBottom.Enabled := ACanModify;
    acVAlignCenter.Enabled := ACanModify;
    acVAlignClient.Enabled := ACanModify;
    acVAlignParent.Enabled := ACanModify;
    acDirectionHorizontal.Enabled := ACanModify;
    acDirectionVertical.Enabled := ACanModify;
    acDirectionTabbed.Enabled := ACanModify;
    acBorder.Enabled := ACanModify;
    acExpandButton.Enabled := ACanModify;
    miAlignHorz.Enabled := ACanModify;
    miDirection.Enabled := ACanModify;
    miAlignVert.Enabled := ACanModify;
  end;
end;

procedure TdxLayoutControlCustomizeForm.RefreshLayoutLookAndFeel;
begin
  lcMain.BeginUpdate;
  try
    inherited RefreshLayoutLookAndFeel;
    lcMain.LayoutLookAndFeel := LayoutLookAndFeel;
  finally
    lcMain.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.StoreTreeViewWndProc(ATreeView: TTreeView;
  out AWindowProcObject: TcxWindowProcLinkedObject; ANewWndMethod: TWndMethod);
begin
  AWindowProcObject := cxWindowProcController.Add(ATreeView, ANewWndMethod);
end;

procedure TdxLayoutControlCustomizeForm.RestoreTreeViewWndProc(var AWindowProcObject: TcxWindowProcLinkedObject);
begin
  cxWindowProcController.Remove(AWindowProcObject);
end;

procedure TdxLayoutControlCustomizeForm.AddItemNode(ANodes: TTreeNodes; AParentNode: TTreeNode; AItem: TdxCustomLayoutItem; AAddChildren: Boolean = True);
var
  I: Integer;
  AThisNode: TTreeNode;
  AGroup: TdxCustomLayoutGroup;
begin
  AThisNode := ANodes.AddChild(AParentNode, '');
  AThisNode.Data := AItem;
  AItem.FreeNotification(Self);
  RefreshNode(AThisNode);
  AThisNode.Selected := False;
  AThisNode.Focused := False;
  if AAddChildren and (AItem is TdxCustomLayoutGroup) then
  begin
    AGroup := TdxCustomLayoutGroup(AItem);
    for I := 0 to AGroup.Count - 1 do
      if CanShowItem(AGroup[I]) then
        AddItemNode(ANodes, AThisNode, AGroup[I]);
  end;
end;

procedure TdxLayoutControlCustomizeForm.AvailableItemsWndProc(var Message: TMessage);
begin
  if not TreeViewWndProcHandler(tvAvailableItems.InnerTreeView, Message) then
    FAvailableItemsWindowProcLinkedObject.DefaultProc(Message);
end;

function TdxLayoutControlCustomizeForm.DoCreateItem(AClass: TdxCustomLayoutItemClass; const ACaption: string): TdxCustomLayoutItem;
begin
  Result := Container.CreateItem(AClass);
  Result.Caption := ACaption;
end;

function TdxLayoutControlCustomizeForm.CreateItem(AClass: TdxCustomLayoutItemClass; const ACaption: string): TdxCustomLayoutItem;
begin
  SaveToUndo;
  BeginUpdate;
  try
    Result := DoCreateItem(AClass, ACaption);
    DoAfterInsertionItem(Result);
  finally
    CancelUpdate;
  end;
end;

procedure TdxLayoutControlCustomizeForm.DoAfterInsertionItem(AItem: TdxCustomLayoutItem);
var
  AIntf: IdxLayoutDesignerHelper;
begin
  if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
  begin
    AIntf.SelectComponent(AItem);
    AIntf := nil;
  end;
end;

function TdxLayoutControlCustomizeForm.GetImageIndex(AItem: TdxCustomLayoutItem): Integer;
const
  AGroupIndexMap: array[Boolean, Boolean, TdxLayoutDirection] of Integer = ((
    (7, 8, 9), (10, 11, 12)), ((14, 15, 16), (17, 18, 19)));
  ASeparatorIndexMap: array[Boolean] of Integer = (2, 3);
  ASplitterIndexMap: array[Boolean] of Integer = (4, 5);

var
  AGroup: TdxCustomLayoutGroup;
begin
  if AItem is TdxCustomLayoutGroup then
  begin
    AGroup := AItem as TdxCustomLayoutGroup;
    Result := AGroupIndexMap[TdxCustomLayoutItemAccess(AGroup).FIsFloat,  AGroup.Locked, AGroup.LayoutDirection];
    if AGroup.Hidden and not AGroup.IsRoot then
      Result := Result + FOriginalImageCount;
  end
  else
    if AItem is TdxLayoutImageItem then
      Result := 13
    else
      if AItem is TdxLayoutEmptySpaceItem then
        Result := 6
      else
        if AItem is TdxLayoutLabeledItem then
          Result := 1
        else
          if AItem is TdxLayoutSeparatorItem then
            Result := ASeparatorIndexMap[TdxLayoutSeparatorItemAccess(AItem).IsVertical]
          else
            if AItem is TdxLayoutSplitterItem then
              Result := ASplitterIndexMap[TdxLayoutSplitterItem(AItem).IsVertical]
            else
              Result := 0;
end;

function TdxLayoutControlCustomizeForm.GetMenuItems: TdxLayoutCustomizeFormMenuItems;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    Result := DoGetMenuItems(AList);
  finally
    AList.Free;
  end;
end;

function TdxLayoutControlCustomizeForm.HasClassInSelection(AClass: TClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to tvVisibleItems.SelectionCount - 1 do
  begin
    Result := TObject(tvVisibleItems.Selections[I].Data) is AClass;
    if Result then
      Break;
  end;
end;

procedure TdxLayoutControlCustomizeForm.InitializePopupMenu;
var
  I: Integer;
  AMenuItem: TMenuItem;
begin
  for I := 0 to pmAlign.Items.Count - 1 do
  begin
    AMenuItem := TMenuItem.Create(miAlignBy);
    AMenuItem.Caption := pmAlign.Items[I].Caption;
    AMenuItem.Action := pmAlign.Items[I].Action;
    miAlignBy.Add(AMenuItem);
  end;
end;

function TdxLayoutControlCustomizeForm.IsHiddenGroup(AItem: TdxCustomLayoutItem): Boolean;
begin
  Result := (AItem is TdxCustomLayoutGroup) and TdxCustomLayoutGroup(AItem).Hidden;
end;

procedure TdxLayoutControlCustomizeForm.ItemsWndProc(var Message: TMessage);
begin
  if not TreeViewWndProcHandler(tvVisibleItems.InnerTreeView, Message) then
    FItemsWindowProcLinkedObject.DefaultProc(Message);
end;

procedure TdxLayoutControlCustomizeForm.MakeBreakFloat(AItems: TcxTreeView);
var
  AList: TList;
  I: Integer;
  P: TPoint;
begin
  AList := TList.Create;
  try
    cxTreeViewGetSelection(AItems.InnerTreeView, AList);
    Container.BeginUpdate;
    try
      for I := 0 to AList.Count - 1 do
      begin
        if TdxCustomLayoutItemAccess(AList[I]).FIsFloat then
          TdxCustomLayoutItemAccess(AList[I]).LandingFloat
        else
        begin
          P := TdxCustomLayoutItemAccess(AList[I]).FFloatPos;
          if cxPointIsEqual(P, cxNullPoint) then
            P  := Container.ClientToScreen(cxNullPoint);
          TdxCustomLayoutItemAccess(AList[I]).MakeFloat(P, False);
        end;
      end;
    finally
      Container.EndUpdate;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.Changed;
begin
  RefreshLists(True);
end;

procedure TdxLayoutControlCustomizeForm.ResetDragAndDropObjects;
begin
  DragHelper.Reset;
  inherited;
end;

function TdxLayoutControlCustomizeForm.GetCustomizationCaption(AItem: TdxCustomLayoutItem): string;
begin
  if acShowItemNames.Checked then
    Result := AItem.Name
  else
    Result := RemoveAccelChars(AItem.Caption);
  if Result = '' then
    Result := cxGetResourceString(@sdxLayoutControlEmptyCaption);
end;

procedure TdxLayoutControlCustomizeForm.SetCustomizationCaption(AItem: TdxCustomLayoutItem; const ACaption: string);
begin
  if acShowItemNames.Checked then
    AItem.Name := ACaption
  else
    TdxCustomLayoutItemAccess(AItem).SetInplaceRenameCaption(ACaption);
  Container.Modified;
end;

procedure TdxLayoutControlCustomizeForm.RefreshImages;
begin
  FOriginalImageCount := ilItems.Count;
  cxTransformImages(ilItems, ilHelper, clWindow, False);
  if not IsXPManifestEnabled then
    cxTransformImages(ilItems, clWindow);
  ilItems.AddImages(ilHelper);
  ilHelper.Clear;
end;

procedure TdxLayoutControlCustomizeForm.RefreshLists(ARefreshSelection: Boolean = False);
begin
  if IsLocked then
    Exit;
  BeginUpdate;
  try
    RefreshAvailableItems;
    RefreshVisibleItems;
  finally
    CancelUpdate;
  end;
  if ARefreshSelection then
    UpdateSelection;
end;

procedure TdxLayoutControlCustomizeForm.RefreshNode(ANode: TTreeNode);
var
  AItem: TdxCustomLayoutItem;
begin
  AItem := ANode.Data;
  if AItem.IsRoot then
    ANode.Text := cxGetResourceString(@sdxLayoutControlRoot)
  else
    ANode.Text := AItem.CaptionForCustomizeForm;
  ANode.ImageIndex := GetImageIndex(AItem);
  ANode.SelectedIndex := ANode.ImageIndex;
end;

procedure TdxLayoutControlCustomizeForm.RefreshButtonStates;
begin
  acAvailableItemsViewAsList.Checked := Container.CustomizeAvailableItemsViewKind = aivkTree;
  acTabbedView.Checked := Container.CustomizeFormTabbedView;
  acHighlightRoot.Checked := Container.HighlightRoot;
  acShowDesignSelectors.Checked := Container.ShowDesignSelectors;

  RefreshEnableds;
end;

procedure TdxLayoutControlCustomizeForm.RefreshView;

  function AllowAnyFloat: Boolean;
  var
    I: Integer;
  begin
    if not Container.IsDesigning then
    begin
      Result := TdxLayoutContainerAccess(Container).AllowFloatingGroups;
      for I := 0 to TdxLayoutContainerAccess(Container).ManagedItemCount - 1 do
      begin
        if Result then
          Break;
        Result := Result or TdxCustomLayoutItemAccess(TdxLayoutContainerAccess(Container).ManagedItems[I]).AllowFloating;
      end;
    end
    else
      Result := False;
  end;

const
  MainGroupDirectionMap: array[Boolean] of TdxLayoutDirection = (ldHorizontal, ldTabbed);
begin
  lcMainGroup1.LayoutDirection := MainGroupDirectionMap[Container.CustomizeFormTabbedView];

  acShowDesignSelectors.Visible := Container.IsDesigning;
  liShowDesignSelectors.Visible := acShowDesignSelectors.Visible;
  acHighlightRoot.Visible := Container.IsDesigning;
  liHighlightRoot.Visible := acHighlightRoot.Visible;
  acShowItemNames.Visible := Container.IsDesigning;
  liShowItemNames.Visible := acShowItemNames.Visible;

  RefreshStoring;

  acUndo.Visible := not Container.IsDesigning;
  liUndo.Visible := acUndo.Visible;
  acRedo.Visible := not Container.IsDesigning;
  liRedo.Visible := acRedo.Visible;

  lsSeparator4.Visible := (liUndo.Visible or liRedo.Visible) and
    (liShowDesignSelectors.Visible or liHighlightRoot.Visible or liStore.Visible or liRestore.Visible);

  acAddItem.Visible := Container.IsDesigning;
  liAddItem.Visible := acAddItem.Visible;

  acAlignBy.Visible := Container.IsDesigning;
  lsAlignBy.Visible := acAlignBy.Visible;
  liAlignBy.Visible := acAlignBy.Visible;
  acAddImage.Visible := Container.IsDesigning;

  acVisibleItemsMakeFloat.Visible := AllowAnyFloat;
  liVisibleItemsMakeFloat.Visible := acVisibleItemsMakeFloat.Visible;
  acAvailableItemsMakeFloat.Visible := AllowAnyFloat;
  liAvailableItemsMakeFloat.Visible := acAvailableItemsMakeFloat.Visible;

  RefreshLayoutLookAndFeel;
  RefreshButtonStates;
end;

procedure TdxLayoutControlCustomizeForm.SaveToUndo;
begin
  Container.SaveToUndo;
end;

procedure TdxLayoutControlCustomizeForm.CalculateTreeViewPopupActionEnables;

  function CanMakeGroup: Boolean;
  var
    I: Integer;
  begin
    Result := tvVisibleItems.SelectionCount = 1;
    if not Result and (tvVisibleItems.SelectionCount > 1) then
      for I := 1 to tvVisibleItems.SelectionCount - 1 do
      begin
        Result := TdxCustomLayoutItem(tvVisibleItems.Selections[I - 1].Data).Parent =
          TdxCustomLayoutItem(tvVisibleItems.Selections[I].Data).Parent;
        if not Result then
          Break;
      end;
  end;

var
  ACanModify: Boolean;
  AHasLockedGroupInSelection: Boolean;
begin
  ACanModify := CanModify;
  AHasLockedGroupInSelection := HasLockedGroupInSelection;

  miDirection.Enabled := ACanModify and not AHasLockedGroupInSelection;
  miAlignHorz.Enabled := ACanModify;
  miAlignVert.Enabled := ACanModify;
  miTextPosition.Enabled := ACanModify;
  miCaptionAlignHorz.Enabled := ACanModify;
  miCaptionAlignVert.Enabled := ACanModify;
  acCaption.Enabled := ACanModify;
  acBorder.Enabled := ACanModify;
  acExpandButton.Enabled := ACanModify;
  acCollapsible.Enabled := ACanModify;

  acGroup.Enabled := ACanModify and CanMakeGroup;
  acUngroup.Enabled := ACanModify and not AHasLockedGroupInSelection;
end;

procedure TdxLayoutControlCustomizeForm.CalculateTreeViewPopupActionVisibilities;
var
  AHitTest: TdxCustomLayoutHitTest;
  APopupForTree: Boolean;
  AHasGroup: Boolean;
  AHasLabeledItem: Boolean;
  AHasDirectionalItem: Boolean;
  AHasGroupsOnly: Boolean;
  AHasSpecialGroup: Boolean;
  AMenuItems: TdxLayoutCustomizeFormMenuItems;
begin
  AMenuItems := GetMenuItems;
  AHitTest := Container.GetHitTest;
  AHasGroup := HasGroupInSelection;
  AHasLabeledItem := HasLabeledItemInSelection;
  AHasDirectionalItem := HasDirectionalItemInSelection;
  AHasGroupsOnly := HasGroupsOnly;
  AHasSpecialGroup := HasRootInSelection or HasHiddenGroupInSelection;
  APopupForTree := AHitTest.HitTestCode = htTreeViewItems;

  miExpandAll.Visible := APopupForTree;
  miCallapseAll.Visible := APopupForTree;
  miSeparator1.Visible := APopupForTree;
  acTreeViewItemRename.Visible := APopupForTree and (cfmiRename in AMenuItems);
  miDirection.Visible := AHasGroupsOnly and (cfmiDirection in AMenuItems);
  miBorder.Visible := AHasGroupsOnly and not AHasSpecialGroup and (cfmiBorder in AMenuItems);
  acExpandButton.Visible := AHasGroupsOnly and not AHasSpecialGroup and (cfmiExpandButton in AMenuItems);
  miTextPosition.Visible := (AHasGroup or AHasLabeledItem) and not AHasDirectionalItem and
    not AHasSpecialGroup and (cfmiCaptionLayout in AMenuItems);
  miCaptionAlignHorz.Visible := (AHasGroup or AHasLabeledItem) and not AHasDirectionalItem and
    not AHasSpecialGroup and (cfmiCaptionAlignHorz in AMenuItems);
  miCaptionAlignVert.Visible := (AHasGroup or AHasLabeledItem) and not AHasDirectionalItem and
    not AHasSpecialGroup and (cfmiCaptionAlignVert in AMenuItems);
  acCaption.Visible := (AHasGroup or AHasLabeledItem) and not AHasDirectionalItem and
    not AHasSpecialGroup and (cfmiCaption in AMenuItems);
  miAlignHorz.Visible := (cfmiAlignHorz in AMenuItems);
  miAlignVert.Visible := (cfmiAlignVert in AMenuItems);
  acGroup.Visible := not AHasSpecialGroup and (cfmiGrouping in AMenuItems);
  acUngroup.Visible := AHasGroupsOnly and not AHasSpecialGroup and
    (tvVisibleItems.SelectionCount = 1) and not TdxCustomLayoutItem(tvVisibleItems.Selected.Data).IsRoot and
    (cfmiGrouping in AMenuItems);

  acCollapsible.Visible := HasSplitterOnly;
end;

function TdxLayoutControlCustomizeForm.HasDirectionalItemInSelection: Boolean;
begin
  Result := HasClassInSelection(TdxLayoutDirectionalItem);
end;

function TdxLayoutControlCustomizeForm.HasGroupInSelection: Boolean;
begin
  Result := HasClassInSelection(TdxCustomLayoutGroup);
end;

function TdxLayoutControlCustomizeForm.HasGroupsOnly: Boolean;
begin
  Result := HasGroupInSelection and not HasLabeledItemInSelection and not HasDirectionalItemInSelection;
end;

function TdxLayoutControlCustomizeForm.HasHiddenGroupInSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to tvVisibleItems.SelectionCount - 1 do
  begin
    Result := IsHiddenGroup(TdxCustomLayoutItem(tvVisibleItems.Selections[I].Data));
    if Result then
      Break;
  end;
end;

function TdxLayoutControlCustomizeForm.HasLabeledItemInSelection: Boolean;
begin
  Result := HasClassInSelection(TdxCustomLayoutLabeledItem);
end;

function TdxLayoutControlCustomizeForm.HasLockedGroupInSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  if not Container.IsDesigning then
    for I := 0 to tvVisibleItems.SelectionCount - 1 do
      if TObject(tvVisibleItems.Selections[I].Data) is TdxCustomLayoutGroup then
      begin
        Result := TdxCustomLayoutGroup(tvVisibleItems.Selections[I].Data).Locked;
        if Result then
          Break;
      end;
end;

function TdxLayoutControlCustomizeForm.HasLockedItemInSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to tvVisibleItems.SelectionCount - 1 do
  begin
    Result := TdxCustomLayoutItemAccess(tvVisibleItems.Selections[I].Data).IsParentLocked;
    if Result then
      Break;
  end;
end;

function TdxLayoutControlCustomizeForm.HasRootInSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to tvVisibleItems.SelectionCount - 1 do
  begin
    Result := TdxCustomLayoutItem(tvVisibleItems.Selections[I].Data).IsRoot;
    if Result then
      Break;
  end;
end;

function TdxLayoutControlCustomizeForm.HasSplitterOnly: Boolean;
begin
  Result := HasClassInSelection(TdxLayoutSplitterItem) and not HasGroupInSelection and not HasLabeledItemInSelection;
end;

procedure TdxLayoutControlCustomizeForm.SynchronizeTreeViewPopupActionStates;
const
  CaptionAlignHorzMap: array[TAlignment] of Integer = (0, 2, 1);

  procedure ResetSubMenu(AMenu: TMenuItem);
  var
    I: Integer;
  begin
    for I := 0 to AMenu.Count - 1 do
      TAction(AMenu.Items[I].Action).Checked := False;
  end;

var
  AItem: TdxCustomLayoutItem;
  ACaptionOptions: TdxCustomLayoutItemCaptionOptionsAccess;
begin
  ResetSubMenu(miAlignHorz);
  ResetSubMenu(miAlignVert);
  ResetSubMenu(miDirection);
  ResetSubMenu(miTextPosition);
  ResetSubMenu(miCaptionAlignHorz);
  ResetSubMenu(miCaptionAlignVert);

  if tvVisibleItems.SelectionCount >= 1 then
  begin
    AItem := TdxCustomLayoutItem(tvVisibleItems.Selected.Data);
    ACaptionOptions := GetCaptionOptions(AItem);

    TAction(miTextPosition.Items[Integer(ACaptionOptions.Layout)].Action).Checked := True;
    TAction(miCaptionAlignHorz.Items[CaptionAlignHorzMap[ACaptionOptions.AlignHorz]].Action).Checked := True;
    TAction(miCaptionAlignVert.Items[Integer(ACaptionOptions.AlignVert)].Action).Checked := True;
    TAction(miAlignHorz.Items[Integer(AItem.AlignHorz)].Action).Checked := True;
    TAction(miAlignVert.Items[Integer(AItem.AlignVert)].Action).Checked := True;
    acCaption.Checked := ACaptionOptions.Visible;
    if AItem is TdxLayoutSplitterItem then
      acCollapsible.Checked := TdxLayoutSplitterItem(AItem).AllowCloseOnClick;
    if AItem is TdxCustomLayoutGroup then
    begin
      TAction(miDirection.Items[Integer((AItem as TdxCustomLayoutGroup).LayoutDirection)].Action).Checked := True;
      acBorder.Checked := (AItem as TdxCustomLayoutGroup).ShowBorder;
      acExpandButton.Checked := (AItem as TdxCustomLayoutGroup).ButtonOptions.ShowExpandButton;
    end;
  end;
end;

function TdxLayoutControlCustomizeForm.CanSelectItem(AItem: TdxCustomLayoutItem): Boolean;
begin
  Result := (AItem <> nil) and not TdxCustomLayoutItemAccess(AItem).IsDestroying and
    Supports(Container, IdxLayoutDesignerHelper);
end;

procedure TdxLayoutControlCustomizeForm.SelectItem(AItem: TdxCustomLayoutItem);
var
  AIntf: IdxLayoutDesignerHelper;
begin
  if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
  begin
    if AItem <> nil then
      AIntf.SelectComponent(AItem)
    else
      AIntf.SelectComponent(Container);
    AIntf := nil;
  end;
  RefreshEnableds;
end;

procedure TdxLayoutControlCustomizeForm.SetItemsSelections(AList: TList);

  procedure SetActiveControl(AControl: TWinControl);
  begin
    GetParentForm(Self).ActiveControl := AControl;
  end;

  procedure SetTreeViewAsActiveControl(ATreeView: TcxTreeView);
  begin
    if ATreeView.Visible then
      SetActiveControl(ATreeView.InnerTreeView)
    else
      SetActiveControl(nil);
  end;

var
  AItem: TdxCustomLayoutItemAccess;
begin
  if IsLocked then
    Exit;
  BeginUpdate;
  try
    cxTreeViewSetSelection(tvVisibleItems.InnerTreeView, AList);
    cxTreeViewSetSelection(tvAvailableItems.InnerTreeView, AList);
    if (AList.Count > 0) and (TObject(AList.Last) is TdxCustomLayoutItem) then
    begin
      AItem := TdxCustomLayoutItemAccess(AList.Last);
      if AItem.IsAvailable then
        SetTreeViewAsActiveControl(tvAvailableItems)
      else
        SetTreeViewAsActiveControl(tvVisibleItems);
    end;
  finally
    CancelUpdate;
  end;
  RefreshEnableds;
end;

procedure TdxLayoutControlCustomizeForm.SetLayoutItemsSelections(ATreeView: TTreeView);
var
  AList: TList;
  AIntf: IdxLayoutDesignerHelper;
begin
  AList := TList.Create;
  try
    cxTreeViewGetSelection(ATreeView, AList);
    if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
    begin
      if (AList.Count = 1) and (TObject(AList[0]) is TdxCustomLayoutItem) then
        TdxCustomLayoutItem(AList[0]).MakeVisible;
      AIntf.SetSelection(AList);
      AIntf := nil;
    end;
  finally
    AList.Free;
  end;
end;

function TdxLayoutControlCustomizeForm.TreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage): Boolean;

  procedure ShowContextMenu;
  var
    AHitTest: THitTests;
  begin
    AHitTest := ATreeView.GetHitTestInfoAt(Message.LParamLo, Message.LParamHi);
    if (htOnItem in AHitTest) then
      ATreeView.Perform(WM_CONTEXTMENU, ATreeView.Handle, dxPointToLParam(GetMouseCursorPos));
  end;

var
  ANode: TTreeNode;
  AShift: TShiftState;
begin
  Result := False;
  case Message.Msg of
    CN_NOTIFY:
      case TWMNotify(Message).NMHdr^.code of
        NM_RCLICK:
          begin
            ShowContextMenu;
            Message.Result := 1;
            Result := True;
          end;
        end;
    WM_RBUTTONDOWN:
      begin
        if ATreeView.Selected <> nil then
          ATreeView.Selected.EndEdit(False);
        ANode := ATreeView.GetNodeAt(Message.LParamLo, Message.LParamHi);
        if ANode <> nil then
        begin
          ANode.Focused := True;
          AShift := KeysToShiftState(Message.WParam);
          if not ANode.Selected then
          begin
            if [ssShift, ssCtrl] * AShift <> [] then
              AShift := [];
            BeginUpdate;
            try
              ATreeView.Select(ANode, AShift);
            finally
              CancelUpdate;
            end;
            SetLayoutItemsSelections(ATreeView);
          end;
        end;
        Message.Result := 1;
        Result := True;
      end;
    WM_LBUTTONDOWN:
      begin
        ANode := ATreeView.GetNodeAt(Message.LParamLo, Message.LParamHi);
        if (ANode <> nil) and (ATreeView.SelectionCount > 1) then
        begin
          ANode.Focused := True;
          AShift := KeysToShiftState(Message.WParam);
          if ANode.Selected and ([ssCtrl, ssShift] * AShift = []) then
          begin
            BeginUpdate;
            try
              ATreeView.ClearSelection;
            finally
              CancelUpdate;
            end;
          end;
        end;
      end;
  end;
end;

procedure TdxLayoutControlCustomizeForm.RestoreCollapsedNodes(ATreeView: TcxTreeView; AList: TcxComponentList);
var
  I: Integer;
  ANode: TTreeNode;
begin
  ATreeView.FullExpand;
  for I := 0 to AList.Count - 1 do
    if FindNodeByItem(AList[I] as TdxCustomLayoutItem, ANode) then
      ANode.Expanded := False;
end;

procedure TdxLayoutControlCustomizeForm.StoreCollapsedNodes(ATreeView: TcxTreeView; AList: TcxComponentList);

  procedure CheckNode(ANode: TTreeNode);
  var
    I: Integer;
  begin
    if ANode.HasChildren then
    begin
      if not ANode.Expanded then
        AList.Add(TComponent(ANode.Data));
      for I := 0 to ANode.Count - 1 do
        CheckNode(ANode[I]);
    end;
  end;

  procedure CheckTreeView(ATreeView: TcxTreeView);
  var
    I: Integer;
  begin
    for I := 0 to ATreeView.Items.Count - 1 do
      CheckNode(ATreeView.Items[I]);
  end;

begin
  if AList.Count > 0 then
    Exit;
  CheckTreeView(ATreeView);
end;

procedure TdxLayoutControlCustomizeForm.RestoreFirstNode(ATreeView: TcxTreeView; AItem: TdxCustomLayoutItem);
var
  ANode: TTreeNode;
begin
  if cxTreeViewFindNodeByData(ATreeView.InnerTreeView, AItem, ANode) then
    ATreeView.TopItem := ANode;
end;

procedure TdxLayoutControlCustomizeForm.StoreFirstNode(ATreeView: TcxTreeView; out AItem: TdxCustomLayoutItem);
begin
  if ATreeView.TopItem <> nil then
    AItem := ATreeView.TopItem.Data
  else
    AItem := nil;
end;

procedure TdxLayoutControlCustomizeForm.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TdxLayoutControlCustomizeForm.acCollapsibleExecute(Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    acCollapsible.Checked := not acCollapsible.Checked;
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      for I := 0 to AList.Count - 1 do
        TdxLayoutSplitterItem(AList[I]).AllowCloseOnClick := acCollapsible.Checked;
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.acAddGroupExecute(Sender: TObject);
begin
  CreateItem(TdxLayoutContainerAccess(Container).GetDefaultGroupClass, cxGetResourceString(@sdxLayoutControlNewGroupCaption));
end;

procedure TdxLayoutControlCustomizeForm.acAddItemExecute(Sender: TObject);
begin
  CreateItem(TdxLayoutContainerAccess(Container).GetDefaultItemClass, cxGetResourceString(@sdxLayoutControlNewItemCaption));
end;

procedure TdxLayoutControlCustomizeForm.acAddEmptySpaceItemExecute(Sender: TObject);
var
  AItem: TdxCustomLayoutItem;
begin
  AItem := CreateItem(TdxLayoutEmptySpaceItem, cxGetResourceString(@sdxLayoutControlNewEmptySpaceItemCaption));
  AItem.Width := 10;
  AItem.Height := 10;
end;

procedure TdxLayoutControlCustomizeForm.acAddSeparatorExecute(Sender: TObject);
begin
  CreateItem(TdxLayoutSeparatorItem, cxGetResourceString(@sdxLayoutControlNewSeparatorItemCaption));
end;

procedure TdxLayoutControlCustomizeForm.acAddCustomItemExecute(
  Sender: TObject);
begin
//
end;

procedure TdxLayoutControlCustomizeForm.acAddLabeledItemExecute(
  Sender: TObject);
begin
  CreateItem(TdxLayoutContainerAccess(Container).GetDefaultLabelClass, cxGetResourceString(@sdxLayoutControlNewLabeledItemCaption));
end;

procedure TdxLayoutControlCustomizeForm.acAddSplitterExecute(
  Sender: TObject);
var
  AItem: TdxCustomLayoutItem;
begin
  SaveToUndo;
  AItem := Container.CreateItem(TdxLayoutSplitterItem);
  AItem.Caption := cxGetResourceString(@sdxLayoutControlNewSplitterItemCaption);
  DoAfterInsertionItem(AItem);
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  if TdxCustomLayoutItem(Node.Data).IsRoot then
    Exit;
  RefreshButtonStates;
  BeginUpdate;
  try
    SetCustomizationCaption(TdxCustomLayoutItem(Node.Data), S);
    S := GetCustomizationCaption(TdxCustomLayoutItem(Node.Data));
    Node.Text := S;
  finally
    CancelUpdate;
  end;
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ANode: TTreeNode;
  ATreeView: TcxTreeView;
  AHitTests: THitTests;
begin
  ATreeView := TcxTreeView(Sender);
  AHitTests := ATreeView.GetHitTestInfoAt(X, Y);
  if htOnItem in AHitTests then
  begin
    ANode := ATreeView.GetNodeAt(X, Y);
    if ANode = nil then
      Exit;
    if (Button = mbLeft) and ANode.Selected and (ANode = ATreeView.Selected) then
      DragHelper.InitializeDragItem(ATreeView.Selected.Data, X, Y);
  end;
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    DragHelper.TryBeginDragAndDrop(X, Y);
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    DragHelper.Reset;
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsChange(Sender: TObject; Node: TTreeNode);
begin
  if not IsLocked then
    SetLayoutItemsSelections(Sender as TTreeView);
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := (Node = nil) or (Node.Data <> nil);
end;

procedure TdxLayoutControlCustomizeForm.AlignExecute(Sender: TObject);
var
  AIntf: IdxLayoutDesignerHelper;
  AList: TList;
  ATag: TcxTag;
  I: Integer;
begin
  //todo: TdxLayoutControlAccess(Control).SaveToUndo;
  ATag := (Sender as TAction).Tag;
  AList := TList.Create;
  try
    if Supports(Container, IdxLayoutDesignerHelper, AIntf) then
    begin
      AIntf.GetSelection(AList);
      AIntf := nil;
    end;
    BeginUpdate;
    try
      Container.BeginUpdate;
      try
        if ATag = -1 then
          for I := 0 to AList.Count - 1 do
            TdxCustomLayoutItem(AList[I]).AlignmentConstraint := nil
        else
          with Container.CreateAlignmentConstraint do
          begin
            Kind := TdxLayoutAlignmentConstraintKind(ATag);
            for I := 0 to AList.Count - 1 do
              AddItem(TdxCustomLayoutItem(AList[I]));
          end;
      finally
        Container.EndUpdate;
      end;
    finally
      CancelUpdate;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.acTreeViewItemsDeleteExecute(Sender: TObject);
var
  AList: TComponentList;
begin
  if tvVisibleItems.IsEditing then
    tvVisibleItems.Selected.EndEdit(True);
  AList := TComponentList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    DeleteItems(AList, tvVisibleItems.InnerTreeView);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
//  acAvailableItemsDelete.Enabled := False;
end;

procedure TdxLayoutControlCustomizeForm.acAvailableItemsDeleteExecute(
  Sender: TObject);
var
  AList: TComponentList;
begin
  AList := TComponentList.Create;
  try
    cxTreeViewGetSelection(tvAvailableItems.InnerTreeView, AList);
    DeleteItems(AList, tvAvailableItems.InnerTreeView);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);

  function IsDestination(AItem: TdxCustomLayoutItem): Boolean;
  begin
    Result := (dxLayoutDragAndDropObject <> nil) and
      (TdxLayoutDragAndDropObjectAccess(dxLayoutDragAndDropObject).DestinationItem = AItem);
  end;

begin
  if Node.Deleting then
    Exit;
  if (TObject(Node.Data) is TdxCustomLayoutGroup) and TdxCustomLayoutGroupAccess(Node.Data).AutoCreated and
      not TdxCustomLayoutGroup(Node.Data).IsRoot then
  begin
    Sender.Canvas.Font.Style := [fsItalic];
  end;
  if IsDestination(Node.Data) then
  begin
    Sender.Canvas.Font.Color := clHighlightText;
    Sender.Canvas.Brush.Color := clHighlight;
  end;
end;

procedure TdxLayoutControlCustomizeForm.tvAvailableItemsContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
//  acTreeViewItemsDelete.Enabled := False;
end;

procedure TdxLayoutControlCustomizeForm.acAvailableItemsExpandAllExecute(
  Sender: TObject);
begin
  tvAvailableItems.FullExpand;
end;

procedure TdxLayoutControlCustomizeForm.acAvailableItemsCollapseAllExecute(
  Sender: TObject);
begin
  tvAvailableItems.FullCollapse;
end;

procedure TdxLayoutControlCustomizeForm.acTreeViewExpandAllExecute(
  Sender: TObject);
begin
  if tvVisibleItems.Selected <> nil then
    tvVisibleItems.Selected.EndEdit(True);
  tvVisibleItems.FullExpand;
end;

procedure TdxLayoutControlCustomizeForm.acTreeViewCollapseAllExecute(
  Sender: TObject);
begin
  if tvVisibleItems.Selected <> nil then
    tvVisibleItems.Selected.EndEdit(True);
  tvVisibleItems.FullCollapse;
  if tvVisibleItems.Selected <> nil then
    tvVisibleItems.InnerTreeView.Select(tvVisibleItems.Selected);
  SetLayoutItemsSelections(tvVisibleItems.InnerTreeView);
end;

procedure TdxLayoutControlCustomizeForm.acAvailableItemsViewAsListExecute(
  Sender: TObject);
const
  AViewKindMap: array [Boolean] of TdxLayoutAvailableItemsViewKind = (aivkList, aivkTree);
begin
  Container.CustomizeAvailableItemsViewKind := AViewKindMap[acAvailableItemsViewAsList.Checked];
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);

  function IsKeyDown(AShift: TShiftState): Boolean;
  begin
    Result := (AShift * KeyboardStateToShiftState) <> [];
  end;

var
  AEditWnd: THandle;
  AEditingText: string;
begin
  AllowEdit := not (TdxCustomLayoutItem(Node.Data).IsRoot or (DragHelper.DragItem <> nil) or
    IsKeyDown([ssCtrl, ssShift]) or TdxCustomLayoutItemAccess(Node.Data).IsParentLocked) and
    TdxLayoutContainerAccess(Container).AllowRename;

  if not AllowEdit or Container.IsDesigning then
    Exit;
  AEditingText := TdxCustomLayoutItemAccess(Node.Data).GetInplaceRenameCaption;
  if AEditingText <> Node.Text then
  begin
    AEditWnd := SendMessage(Node.Owner.Owner.Handle, TVM_GETEDITCONTROL, 0, 0);
    if AEditWnd <> 0 then
      SetWindowText(AEditWnd, PChar(AEditingText));
  end;
  RefreshEnableds;
end;

procedure TdxLayoutControlCustomizeForm.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  case Msg.CharCode of
    VK_ESCAPE:
      begin
        Handled := tvVisibleItems.IsEditing or tvAvailableItems.IsEditing;
        if Handled then
          if tvVisibleItems.Selected <> nil then
            tvVisibleItems.Selected.EndEdit(True)
          else
            if tvAvailableItems.Selected <> nil then
              tvAvailableItems.Selected.EndEdit(True);
      end;
  end;
end;

procedure TdxLayoutControlCustomizeForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    if tvVisibleItems.Focused and not tvVisibleItems.IsEditing then
      acTreeViewItemsDelete.Execute;
    if tvAvailableItems.Focused and not tvAvailableItems.IsEditing then
      acAvailableItemsDelete.Execute;
  end;
end;

procedure TdxLayoutControlCustomizeForm.acTabbedViewExecute(Sender: TObject);
begin
  Container.CustomizeFormTabbedView := acTabbedView.Checked;
end;

procedure TdxLayoutControlCustomizeForm.acHighlightRootExecute(Sender: TObject);
begin
  Container.HighlightRoot := acHighlightRoot.Checked;
end;

procedure TdxLayoutControlCustomizeForm.acShowDesignSelectorsExecute(Sender: TObject);
begin
  Container.ShowDesignSelectors := acShowDesignSelectors.Checked;
end;

procedure TdxLayoutControlCustomizeForm.acShowItemNamesExecute(Sender: TObject);
begin
  UpdateContent;
end;

procedure TdxLayoutControlCustomizeForm.acStoreExecute(Sender: TObject);
begin
  TdxLayoutContainerAccess(Container).Store;
  RefreshEnableds;
end;

procedure TdxLayoutControlCustomizeForm.acRestoreExecute(Sender: TObject);
begin
  SaveToUndo;
  TdxLayoutContainerAccess(Container).Restore;
end;

procedure TdxLayoutControlCustomizeForm.acTreeViewItemRenameExecute(
  Sender: TObject);
begin
  tvVisibleItems.Selected.EditText;
end;

procedure TdxLayoutControlCustomizeForm.acAvailableItemRenameExecute(
  Sender: TObject);
begin
  tvAvailableItems.Selected.EditText;
end;

procedure TdxLayoutControlCustomizeForm.acUndoExecute(Sender: TObject);
begin
  Container.UndoRedoManager.Undo;
  RefreshButtonStates;
end;

procedure TdxLayoutControlCustomizeForm.acRedoExecute(Sender: TObject);
begin
  Container.UndoRedoManager.Redo;
  RefreshButtonStates;
end;

procedure TdxLayoutControlCustomizeForm.acAlignByExecute(Sender: TObject);
begin
// for popup
end;

procedure TdxLayoutControlCustomizeForm.acHAlignExecute(Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  AList := TList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    for I := 0 to AList.Count - 1 do
      TdxCustomLayoutItem(AList[I]).AlignHorz := TdxLayoutAlignHorz((Sender as TAction).Tag);
    Container.Modified;
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.pmAvailableItemsActionsPopup(Sender: TObject);
begin
  acAvailableItemRename.Visible := cfmiRename in GetMenuItems;
end;

procedure TdxLayoutControlCustomizeForm.pmTreeViewActionsPopup(Sender: TObject);
begin
  CalculateTreeViewPopupActionVisibilities;
  CalculateTreeViewPopupActionEnables;
  SynchronizeTreeViewPopupActionStates;
end;

procedure TdxLayoutControlCustomizeForm.acVAlignExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  AList := TList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    for I := 0 to AList.Count - 1 do
      TdxCustomLayoutItem(AList[I]).AlignVert := TdxLayoutAlignVert((Sender as TAction).Tag);
    Container.Modified;
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.acDirectionsExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
  ANewDirection: TdxLayoutDirection;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  AList := TList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    ANewDirection := TdxLayoutDirection((Sender as TAction).Tag);
    for I := 0 to AList.Count - 1 do
      if TObject(AList[I]) is TdxCustomLayoutGroup then
        TdxCustomLayoutGroup(AList[I]).LayoutDirection := ANewDirection;
    Container.Modified;
  finally
    AList.Free;
  end;
  tvVisibleItems.Invalidate;
end;

procedure TdxLayoutControlCustomizeForm.acBorderExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  acBorder.Checked := not acBorder.Checked;
  AList := TList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    for I := 0 to AList.Count - 1 do
    begin
      if TObject(AList[I]) is TdxCustomLayoutGroup then
        TdxCustomLayoutGroup(AList[I]).ShowBorder := (Sender as TAction).Checked;
    end;
    Container.Modified;
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutControlCustomizeForm.tvVisibleItemsDeletion(
  Sender: TObject; Node: TTreeNode);
var
  ANode: TTreeNode;
begin
  if (csDestroying in ComponentState) or not Container.IsDesigning and IsLocked then
    Exit;

  if Node = Node.TreeView.Selected then
  begin
    ANode := Node.GetNext;
    if ANode = nil then
      ANode := Node.GetPrev;
    if (ANode <> nil) and CanSelectItem(ANode.Data) then
    begin
      ANode.Selected := True;
      SelectItem(ANode.Data);
    end;
  end;
end;

procedure TdxLayoutControlCustomizeForm.acExpandButtonExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    acExpandButton.Checked := not acExpandButton.Checked;
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      for I := 0 to AList.Count - 1 do
      begin
        if TObject(AList[I]) is TdxCustomLayoutGroup then
          TdxCustomLayoutGroup(AList[I]).ButtonOptions.ShowExpandButton := acExpandButton.Checked;
      end;
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.acTextPositionExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      for I := 0 to AList.Count - 1 do
        GetCaptionOptions(TdxCustomLayoutItem(AList[I])).Layout := TdxCaptionLayout((Sender as TAction).Tag);
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.acCaptionAlignHorzExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      for I := 0 to AList.Count - 1 do
        GetCaptionOptions(TdxCustomLayoutItem(AList[I])).AlignHorz :=
          TAlignment((Sender as TAction).Tag);
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.acCaptionExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    acCaption.Checked := not acCaption.Checked;
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      for I := 0 to AList.Count - 1 do
        GetCaptionOptions(TdxCustomLayoutItem(AList[I])).Visible := acCaption.Checked;
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.acCaptionAlignVertExecute(
  Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      for I := 0 to AList.Count - 1 do
        TdxCustomLayoutItemCaptionOptionsAccess(TdxCustomLayoutItem(AList[I]).CaptionOptions).AlignVert := TdxAlignmentVert((Sender as TAction).Tag);
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutControlCustomizeForm.acGroupExecute(Sender: TObject);
const
  LayoutDirectionMap: array [TdxLayoutDirection] of TdxLayoutDirection = (ldHorizontal, ldVertical, ldVertical);

  function GetIndexByItem(AParent: TdxCustomLayoutGroup; AItem: TdxCustomLayoutItem): Integer;
  begin
    if AParent = AItem.Parent then
      Result := AItem.Index
    else
      Result := GetIndexByItem(AParent, AItem.Parent);
  end;

  function GetIndex(AParent: TdxCustomLayoutGroup; AList: TList): Integer;
  var
    I: Integer;
  begin
    Result := AParent.Count;
    for I := 0 to AList.Count - 1 do
      Result := Min(Result, GetIndexByItem(AParent, TdxCustomLayoutItem(AList[I])));
  end;

var
  AList: TList;
  I: Integer;
  AParent: TdxCustomLayoutGroup;
  AGroup: TdxCustomLayoutGroup;
  AIndex: Integer;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
      AList.Sort(CompareItemsByIndex);
      AParent := TdxCustomLayoutItem(AList[0]).Parent;
      AIndex := GetIndex(AParent, AList);
      AGroup := DoCreateItem(TdxLayoutContainerAccess(Container).GetDefaultGroupClass, cxGetResourceString(@sdxLayoutControlNewGroupCaption)) as TdxCustomLayoutGroup;
      AGroup.LayoutDirection := LayoutDirectionMap[AParent.LayoutDirection];
      AGroup.MoveTo(AParent, AIndex);
      for I := 0 to AList.Count - 1 do
        TdxCustomLayoutItem(AList[I]).Parent := AGroup;
      Container.Modified;
    finally
      AList.Free;
    end;
  finally
    Container.EndUpdate;
  end;
end;

procedure TdxLayoutControlCustomizeForm.acUngroupExecute(Sender: TObject);
var
  AGroup: TdxCustomLayoutGroup;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  Container.BeginUpdate;
  try
    AGroup := TdxCustomLayoutGroup(tvVisibleItems.Selected.Data);
    AGroup.MoveChildrenToParent;
    AGroup.Free;
  finally
    Container.EndUpdate;
  end;
end;

procedure TdxLayoutControlCustomizeForm.acAddImageExecute(Sender: TObject);
begin
  CreateItem(TdxLayoutImageItem, cxGetResourceString(@sdxLayoutControlNewImageItemCaption));
end;

procedure TdxLayoutControlCustomizeForm.acVisibleItemsMakeFloatExecute(
  Sender: TObject);
begin
  MakeBreakFloat(tvVisibleItems);
end;

procedure TdxLayoutControlCustomizeForm.acAvailableItemsFloatExecute(
  Sender: TObject);
begin
  MakeBreakFloat(tvAvailableItems);
end;

end.
