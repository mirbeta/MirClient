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

unit dxRibbonCustomizationFormHelper;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, Types, Controls, Graphics, Dialogs, Math, IniFiles, dxCore, cxClasses, cxGeometry,
  cxGraphics, cxTL, dxBar, dxRibbon, cxDropDownEdit, cxLookAndFeelPainters, dxBarStrs, cxCheckBox;

const
  dxrcfContextLevel = 0;
  dxrcfTabLevel = 1;
  dxrcfGroupLevel = 2;
  dxrcfItemLevel = 3;

  dxrcfCommandsCommandsNotInTheRibbon = 0;
  dxrcfCommandsAllCommands = 1;
  dxrcfCommandsAllTabs = 2;
  dxrcfCommandsMainTabs = 3;
  dxrcfCommandsToolTabs = 4;
  dxrcfCommandsCustomTabsAndGroups = 5;

  dxrcfRibbonAllTabs = 0;
  dxrcfRibbonMainTabs = 1;
  dxrcfRibbonToolTabs = 2;

type
  { TdxRibbonCustomizationFormItemNodeData }

  TdxRibbonCustomizationFormItemNodeData = class
  private
    FBaseIniSection: string;
    FIsNewItemNeeded: Boolean;
    FIsNewItemLinkNeeded: Boolean;
    FItem: TdxBarItem;
    FItemLink: TdxBarItemLink;

    function GetIsNewItemLinkNeeded: Boolean;
    function GetItemAsSubItem: TCustomdxBarSubItem;
    procedure SetIsNewItemNeeded(AValue: Boolean);
  public
    constructor Create(AItem: TdxBarItem; AItemLink: TdxBarItemLink); reintroduce;
    function Clone: TdxRibbonCustomizationFormItemNodeData;
    function CreateEqualItem: TdxBarItem;
    function CreateEqualItemLink(AOwner: TdxBarItemLinks): TdxBarItemLink;
    procedure DrawImage(ACanvas: TcxCanvas; var ARect: TRect; AScaleFactor: TdxScaleFactor = nil);
    function IsBeginGroup: Boolean;
    function IsSubItem: Boolean;
    procedure LoadItemLinkFromIni(AItemLink: TdxBarItemLink);

    property BaseIniSection: string read FBaseIniSection write FBaseIniSection;
    property IsNewItemNeeded: Boolean read FIsNewItemNeeded write SetIsNewItemNeeded;
    property IsNewItemLinkNeeded: Boolean read GetIsNewItemLinkNeeded write FIsNewItemLinkNeeded;
    property Item: TdxBarItem read FItem write FItem;
    property ItemAsSubItem: TCustomdxBarSubItem read GetItemAsSubItem;
    property ItemLink: TdxBarItemLink read FItemLink write FItemLink;
  end;

  { TdxRibbonCustomizationFormMergedTabInfo }

  TdxRibbonCustomizationFormMergedTabInfo = class
  private
    FMergedTabs: TcxObjectList;
    FOwnerTab: TdxRibbonTab;
  public
    constructor Create(AOwnerTab: TdxRibbonTab); virtual;
    destructor Destroy; override;

    procedure StoreMergedState;
    procedure RestoreMergedState;

    property MergedTabs: TcxObjectList read FMergedTabs;
    property OwnerTab: TdxRibbonTab read FOwnerTab;
  end;

  { TdxRibbonCustomizationFormMergedTabsList }

  TdxRibbonCustomizationFormMergedTabsList = class(TcxObjectList)
  private
    FTabs: TdxRibbonTabCollection;

    function GetItem(AIndex: Integer): TdxRibbonCustomizationFormMergedTabInfo;
  protected
    function AddTabInfo(ATab: TdxRibbonTab): TdxRibbonCustomizationFormMergedTabInfo;

    property Items[Index: Integer]: TdxRibbonCustomizationFormMergedTabInfo read GetItem; default;
    property Tabs: TdxRibbonTabCollection read FTabs;
  public
    constructor Create(ATabs: TdxRibbonTabCollection); virtual;

    procedure StoreMergedState;
    procedure RestoreMergedState;
  end;

  { TdxCustomRibbonCustomizationFormHelper }

  TdxCustomRibbonCustomizationFormHelper = class
  private
    FCommandsComboBox: TcxComboBox;
    FCommandsTreeList: TcxTreeList;
    FLockCount: Integer;
    FMergedTabsList: TdxRibbonCustomizationFormMergedTabsList;
    FMovedNode: TcxTreeListNode;
    FRibbon: TdxCustomRibbon;
    FRibbonComboBox: TcxComboBox;
    FRibbonTreeList: TcxTreeList;
    FSavedState: TCustomIniFile;

    procedure CompareNodes(Sender: TcxCustomTreeList; ANode1, ANode2: TcxTreeListNode; var ACompare: Integer);
    function GetBarIndexInIni(ABar: TdxBar; ASource: TCustomIniFile): Integer;
    function GetFocusedNode: TcxTreeListNode;
    function GetItemNodeCaption(AItem: TdxBarItem; AItemLink: TdxBarItemLink): string;
    function GetRealBounds(ANode: TcxTreeListNode): TRect;
  protected
    procedure AfterNodeMoved(ASourceNode, ANewNode: TcxTreeListNode); virtual; abstract;
    procedure ConvertItemNodeToNew(ANode: TcxTreeListNode);
    procedure CopyNode(ASourceNode, ATargetNode: TcxTreeListNode; ACopyData: Boolean = True);
    procedure DeleteItemLink(AItemLink: TdxBarItemLink);
    function GetItemData(AItemNode: TcxTreeListNode): TdxRibbonCustomizationFormItemNodeData;
    function GetItemNodeVisible(AItem: TdxBarItem): Boolean; virtual;
    function GetNodeFor(AData: TPersistent; AParentNode: TcxTreeListNode): TcxTreeListNode;
    function GetTargetNodeForMovingFocusedNodeDown: TcxTreeListNode; virtual; abstract;
    function GetTargetNodeForMovingFocusedNodeUp: TcxTreeListNode; virtual; abstract;
    procedure LoadItemLinksContentFormIni(AItemLinks: TdxBarItemLinks; ATargetNode: TcxTreeListNode;
      ATargetTreeList: TcxTreeList; ASource: TCustomIniFile);
    procedure PopulateAllItems(ASource: TdxBarManager; const AOnlyMissingItems: Boolean = False);
    procedure PopulateCommandsComboBoxContent; virtual; abstract;
    procedure PopulateItemNodeContent(ATargetNode: TcxTreeListNode; AItem: TdxBarItem; AItemLink: TdxBarItemLink;
      const ANeedBeginGroupNode: Boolean = False; ASource: TCustomIniFile = nil); virtual;
    procedure PopulateRibbonComboBoxContent; virtual; abstract;
    procedure SetFocusedNode(ANode: TcxTreeListNode);
    procedure SynchronizeMatchingNodesWith(APatternNode: TcxTreeListNode);

    procedure LockContentUpdating;
    procedure UnlockContentUpdating;

    procedure DoAddNewItemLink(AItemLinkNode: TcxTreeListNode; AItemLinks: TdxBarItemLinks);
    procedure DoDeleteMissingItemLinks(AItemLinks: TdxBarItemLinks; AParentNode: TcxTreeListNode);
    procedure DoDropMovedNodeTo(ATargetNode: TcxTreeListNode; ACoordinate: Integer); virtual;
    procedure DoMoveNode(ASourceNode, ATargetNode: TcxTreeListNode; const AInsertBeforeTargetNode: Boolean;
      const ATargetNodeIsParent: Boolean = False);

    function IsAboveNodeCenter(ANode: TcxTreeListNode; const ACoordinate: Integer): Boolean;
    function IsCenterNodePart(ANode: TcxTreeListNode; const ACoordinate: Integer): Boolean;
    function IsTopNodePart(ANode: TcxTreeListNode; const ACoordinate: Integer): Boolean;

    function IsDuplicatedIn(ASourceNode, ATargetNode: TcxTreeListNode; ACoordinate: Integer): Boolean; virtual;
    function IsPartOfItemsStructure(AItemNode, AStructureOwnerNode: TcxTreeListNode): Boolean;
    function IsRecursiveInclusion(ASourceNode, ATargetNode: TcxTreeListNode; ACoordinate: Integer): Boolean;
    function IsRibbonContainsItem(AItem: TdxBarItem): Boolean;
    function IsSameElement(ASourceNode, ATargetNode: TcxTreeListNode): Boolean; overload;
    function IsSameElement(ANode: TcxTreeListNode; AData: TPersistent): Boolean; overload;
    function IsSourceNodeValid(ASourceNode: TcxTreeListNode): Boolean; virtual;
    function IsTargetNodeValid(ASourceNode, ATargetNode: TcxTreeListNode): Boolean; virtual;

    property CommandsComboBox: TcxComboBox read FCommandsComboBox;
    property CommandsTreeList: TcxTreeList read FCommandsTreeList;
    property FocusedNode: TcxTreeListNode read GetFocusedNode;
    property Ribbon: TdxCustomRibbon read FRibbon;
    property RibbonComboBox: TcxComboBox read FRibbonComboBox;
    property RibbonTreeList: TcxTreeList read FRibbonTreeList;
  public
    constructor Create(ARibbon: TdxCustomRibbon; ACommandsTreeList, ARibbonTreeList: TcxTreeList;
      ACommandsComboBox, ARibbonComboBox: TcxComboBox); reintroduce; virtual;
    destructor Destroy; override;
    procedure Initialize;

    procedure BeginApplyChanges; virtual; abstract;
    procedure CreateAddedElements; virtual; abstract;
    procedure DeleteRemovedElements; virtual; abstract;
    procedure EndApplyChanges; virtual; abstract;
    procedure ReorderElements; virtual; abstract;
    procedure SynchronizeElementCaptions; virtual; abstract;

    procedure AddNode(ANode: TcxTreeListNode);
    procedure CheckFocusedNode;
    procedure DropMovedNodeTo(ATargetNode: TcxTreeListNode; const ACoordinate: Integer = -1);
    procedure FreeNodeData(ANode: TcxTreeListNode);
    function GetNodeLevel(ANode: TcxTreeListNode; const AAsTyped: Boolean = False): Integer; virtual; abstract;
    procedure InitializeMovedNode(ANode: TcxTreeListNode);
    procedure MoveFocusedNodeDown; virtual; abstract;
    procedure MoveFocusedNodeUp; virtual; abstract;
    procedure PopulateCommandsTreeListContent; virtual; abstract;
    procedure PopulateRibbonTreeListContent(const AIsReseting: Boolean = False); virtual; abstract;
    procedure ReleaseMovedNode;
    procedure RemoveNode(ANode: TcxTreeListNode); virtual; abstract;
    procedure RenameNode(ANode: TcxTreeListNode; const ANewCaption: string); virtual;
    procedure UpdateRibbonActiveTab;

    function DrawItemNode(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo;
      ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor = nil): Boolean;

    function CanMoveFocusedNodeDown: Boolean; virtual;
    function CanMoveFocusedNodeUp: Boolean; virtual;
    function CanMoveNodeTo(ASourceNode, ATargetNode: TcxTreeListNode; const ACoordinate: Integer = -1): Boolean; virtual; abstract;
    function CanRemoveFocusedNode: Boolean; virtual;
    function CanRenameFocusedNode: Boolean; virtual;
    function CanUpdateContent: Boolean;

    property MovedNode: TcxTreeListNode read FMovedNode;
  end;

  { TdxRibbonCustomizationFormHelper }

  TdxRibbonCustomizationFormHelper = class(TdxCustomRibbonCustomizationFormHelper)
  strict private
    function GetContextCaption(AContext: TdxRibbonContext): string;
  protected
    function AddNewNodeToFocusedNode(const ATargetLevel: Integer; const ACaption: string): TcxTreeListNode;
    procedure AfterNodeMoved(ASourceNode, ANewNode: TcxTreeListNode); override;
    procedure ConvertGroupNodeToNew(ANode: TcxTreeListNode);
    procedure ConvertTabNodeToNew(ANode: TcxTreeListNode);
    function CreateContextNode(AContext: TdxRibbonContext;
      ATargetTreeList: TcxTreeList; AIsReseting: Boolean = False): TcxTreeListNode;
    function CreateTabNode(ATab: TdxRibbonTab; AContextNode: TcxTreeListNode;
      ATargetTreeList: TcxTreeList; AIsReseting: Boolean = False): TcxTreeListNode;
    function GetContextNode(ASourceNode: TcxTreeListNode): TcxTreeListNode;
    function GetItemNodeVisible(AItem: TdxBarItem): Boolean; override;
    function GetTargetNodeForMovingFocusedNodeDown: TcxTreeListNode; override;
    function GetTargetNodeForMovingFocusedNodeUp: TcxTreeListNode; override;
    function LoadExistingGroupNodeContentFormIni(AToolBar: TdxBar; AParentTabNode: TcxTreeListNode;
      ATargetTreeList: TcxTreeList; ASource: TCustomIniFile): TcxTreeListNode;
    function LoadMissingGroupNodeContentFormIni(AToolBar: TdxBar; AParentTabNode: TcxTreeListNode;
      ATargetTreeList: TcxTreeList; ASource: TCustomIniFile): TcxTreeListNode;
    procedure LoadTabNodeContentFormIni(ANode: TcxTreeListNode; ATargetTreeList: TcxTreeList; ASource: TCustomIniFile);
    procedure PopulateCommandsComboBoxContent; override;
    procedure PopulateCustomTabsAndGroups;
    procedure PopulateGroupNodeContent(ANode: TcxTreeListNode; ATargetTreeList: TcxTreeList);
    procedure PopulateRibbonComboBoxContent; override;
    procedure PopulateTabNodeContent(ANode: TcxTreeListNode; ATargetTreeList: TcxTreeList);
    procedure RestoreTabNodeExpanding(ANode: TcxTreeListNode; AStorage: TList);
    procedure StoreTabNodeExpanding(ANode: TcxTreeListNode; AStorage: TList);

    procedure DoDropMovedNodeTo(ATargetNode: TcxTreeListNode; ACoordinate: Integer); override;

    procedure CheckAndAddContextNodeToRibbon(ANode: TcxTreeListNode);
    procedure CheckAndAddGroupNodeToRibbon(ANode: TcxTreeListNode);
    procedure CheckAndAddItemNodeToRibbon(ANode: TcxTreeListNode);
    procedure CheckAndAddTabNodeToRibbon(ANode: TcxTreeListNode);
    procedure CheckAndDeleteContextsFromRibbon;
    procedure CheckAndDeleteGroupsFromRibbon(AGroups: TdxRibbonTabGroups; ATabNode: TcxTreeListNode);
    procedure CheckAndDeleteTabsFromRibbon(AContext: TdxRibbonContext);

    function IsGroupCustomizingAllowed(ANode: TcxTreeListNode): Boolean;
    function IsGroupHiddenInTab(AGroupNode, ATabNode: TcxTreeListNode): Boolean;
    function IsParentNodeTheSame(ASourceNode, ATargetNode: TcxTreeListNode): Boolean;
    function IsRibbonGroupNodeWithSharedToolBar(ANode: TcxTreeListNode): Boolean;
    function IsSourceNodeValid(ASourceNode: TcxTreeListNode): Boolean; override;
    function IsTargetNodeValid(ASourceNode, ATargetNode: TcxTreeListNode): Boolean; override;
  public
    procedure BeginApplyChanges; override;
    procedure CreateAddedElements; override;
    procedure DeleteRemovedElements; override;
    procedure EndApplyChanges; override;
    procedure ReorderElements; override;
    procedure SynchronizeElementCaptions; override;

    procedure AddNewContext;
    procedure AddNewGroup;
    procedure AddNewTab;
    procedure ExpandAllContexts;
    function GetNodeLevel(ANode: TcxTreeListNode; const AAsTyped: Boolean = False): Integer; override;
    function IsTabsInCommands: Boolean;
    procedure MoveFocusedNodeDown; override;
    procedure MoveFocusedNodeUp; override;
    procedure PopulateCommandsTreeListContent; override;
    procedure PopulateRibbonTreeListContent(const AIsReseting: Boolean = False); override;
    procedure RemoveNode(ANode: TcxTreeListNode); override;
    procedure RenameNode(ANode: TcxTreeListNode; const ANewCaption: string); override;
    procedure ResetTabNode(ANode: TcxTreeListNode);
    procedure UpdateContextsVisibility;

    function CanMoveFocusedNodeDown: Boolean; override;
    function CanMoveFocusedNodeUp: Boolean; override;
    function CanMoveNodeTo(ASourceNode, ATargetNode: TcxTreeListNode; const ACoordinate: Integer = -1): Boolean; override;
    function CanRemoveFocusedNode: Boolean; override;
    function CanResetFocusedNode: Boolean;

    function DrawContextNode(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo;
      ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Boolean;
  end;

  { TdxRibbonQATCustomizationFormHelper }

  TdxRibbonQATCustomizationFormHelper = class(TdxCustomRibbonCustomizationFormHelper)
  private
    function GetContextPrefix(AContext: TdxRibbonContext): string;
  protected
    procedure AfterNodeMoved(ASourceNode, ANewNode: TcxTreeListNode); override;
    function GetTargetNodeForMovingFocusedNodeDown: TcxTreeListNode; override;
    function GetTargetNodeForMovingFocusedNodeUp: TcxTreeListNode; override;
    procedure PopulateBeginGroupNodeContent(ATargetNode: TcxTreeListNode);
    procedure PopulateContextTabs(AContext: TdxRibbonContext; AItems: TStrings);
    procedure PopulateItemNodeContent(ATargetNode: TcxTreeListNode; AItem: TdxBarItem; AItemLink: TdxBarItemLink;
      const ANeedBeginGroupNode: Boolean = False; ASource: TCustomIniFile = nil); override;
    procedure PopulateTabItems(ATab: TdxRibbonTab);

    procedure DoReorderItemLinks(AOwnerNode: TcxTreeListNode);

    function IsDuplicatedIn(ASourceNode, ATargetNode: TcxTreeListNode; ACoordinate: Integer): Boolean; override;
    function IsSourceNodeValid(ASourceNode: TcxTreeListNode): Boolean; override;
    function IsTargetNodeValid(ASourceNode, ATargetNode: TcxTreeListNode): Boolean; override;
  public
    procedure BeginApplyChanges; override;
    procedure CreateAddedElements; override;
    procedure DeleteRemovedElements; override;
    procedure EndApplyChanges; override;
    procedure ReorderElements; override;
    procedure SynchronizeElementCaptions; override;

    procedure ChangeQATPosition(const AShowBelowRibbon: Boolean);
    function GetNodeLevel(ANode: TcxTreeListNode; const AAsTyped: Boolean = False): Integer; override;
    procedure InitializeQATPositionIndicator(AIndicator: TObject);
    procedure MoveFocusedNodeDown; override;
    procedure MoveFocusedNodeUp; override;
    procedure PopulateCommandsComboBoxContent; override;
    procedure PopulateCommandsTreeListContent; override;
    procedure PopulateRibbonComboBoxContent; override;
    procedure PopulateRibbonTreeListContent(const AIsReseting: Boolean = False); override;
    procedure RemoveNode(ANode: TcxTreeListNode); override;

    function CanMoveNodeTo(ASourceNode, ATargetNode: TcxTreeListNode; const ACoordinate: Integer = -1): Boolean; override;
    function CanRemoveFocusedNode: Boolean; override;
    function CanRenameFocusedNode: Boolean; override;
  end;

implementation

uses
  dxGDIPlusClasses;

type
  TCustomdxBarSubItemAccess = class(TCustomdxBarSubItem);
  TdxBarAccess = class(TdxBar);
  TdxBarItemLinkAccess = class(TdxBarItemLink);
  TdxBarManagerAccess = class(TdxBarManager);
  TdxCustomRibbonAccess = class(TdxCustomRibbon);
  TdxRibbonTabAccess = class(TdxRibbonTab);
  TdxRibbonTabCollectionAccess = class(TdxRibbonTabCollection);
  TdxRibbonTabGroupAccess = class(TdxRibbonTabGroup);
  TdxRibbonTabGroupsAccess = class(TdxRibbonTabGroups);
  TcxTreeListNodeAccess = class(TcxTreeListNode);

{ INI sections }

function GetBarManagerBaseIniSection(ABarManager: TdxBarManager): string;
begin
  Result := TdxBarManagerAccess(ABarManager).GetBaseIniSection;
end;

function GetBarManagerMainIniSection(ABarManager: TdxBarManager): string;
begin
  Result := TdxBarManagerAccess(ABarManager).GetBarManagerSection(GetBarManagerBaseIniSection(ABarManager), skIni);
end;

function GetBarIniSection(ABarManager: TdxBarManager; const AIndex: Integer): string;
begin
  Result := TdxBarAccess.GetIniSection(GetBarManagerBaseIniSection(ABarManager), AIndex);
end;

function GetItemLinkIniSection(const ABaseSection: string; const AIndex: Integer): string;
begin
  Result := TdxBarItemLinkAccess.GetIniSection(ABaseSection, AIndex, skIni);
end;

function GetSubItemIniSection(ABarManager: TdxBarManager; ASubItem: TCustomdxBarSubItem): string;
begin
  Result := TCustomdxBarSubItemAccess(ASubItem).GetIniSection(GetBarManagerBaseIniSection(ABarManager));
end;

{ TdxRibbonCustomizationFormItemNodeData }

constructor TdxRibbonCustomizationFormItemNodeData.Create(AItem: TdxBarItem; AItemLink: TdxBarItemLink);
begin
  inherited Create;
  FBaseIniSection := '';
  FIsNewItemNeeded := False;
  FIsNewItemLinkNeeded := AItemLink = nil;
  FItem := AItem;
  FItemLink := AItemLink;
end;

function TdxRibbonCustomizationFormItemNodeData.Clone: TdxRibbonCustomizationFormItemNodeData;
begin
  Result := TdxRibbonCustomizationFormItemNodeData.Create(FItem, FItemLink);
  Result.FBaseIniSection := FBaseIniSection;
  Result.FIsNewItemNeeded := FIsNewItemNeeded;
  Result.FIsNewItemLinkNeeded := FIsNewItemLinkNeeded;
end;

function TdxRibbonCustomizationFormItemNodeData.CreateEqualItem: TdxBarItem;
begin
  Result := Item;
  if Result <> nil then
  begin
    Result := Item.BarManager.AddItem(TdxBarItemClass(Item.ClassType));
    Result.Name := Result.BarManager.GetUniqueItemName(TdxBarItemClass(Result.ClassType));
    Result.Assign(Item);
    FIsNewItemNeeded := False;
  end;
end;

function TdxRibbonCustomizationFormItemNodeData.CreateEqualItemLink(AOwner: TdxBarItemLinks): TdxBarItemLink;
begin
  Result := AOwner.Add(Item);
  if (ItemLink <> nil) and (BaseIniSection = '') then
    Result.Assign(ItemLink)
  else
    LoadItemLinkFromIni(Result);
  FIsNewItemLinkNeeded := False;
end;

procedure TdxRibbonCustomizationFormItemNodeData.DrawImage(ACanvas: TcxCanvas; var ARect: TRect;
  AScaleFactor: TdxScaleFactor = nil);
var
  AImageWidth: Integer;
  AImageRect: TRect;
  AGlyph: TdxSmartGlyph;
begin
  if Item <> nil then
  begin
    AImageWidth := cxRectHeight(ARect);
    if (Item is TdxRibbonQuickAccessGroupButton) and (TdxRibbonQuickAccessGroupButton(Item).Toolbar <> nil) then
      AGlyph := TdxRibbonQuickAccessGroupButton(Item).Toolbar.Glyph
    else
      AGlyph := Item.Glyph;
    if not ACanvas.UseRightToLeftAlignment then
    begin
      AImageRect := cxRectSetWidth(ARect, AImageWidth);
      Inc(ARect.Left, AImageWidth);
    end
    else
    begin
      AImageRect := ARect;
      AImageRect.Left := AImageRect.Right - AImageWidth;
      Dec(ARect.Right, AImageWidth);
    end;
    cxDrawImage(ACanvas, AImageRect, AGlyph, Item.BarManager.Images, Item.ImageIndex, ifmFit, idmNormal, True, nil, AScaleFactor);
  end;
end;

function TdxRibbonCustomizationFormItemNodeData.IsBeginGroup: Boolean;
begin
  Result := Item = nil;
end;

function TdxRibbonCustomizationFormItemNodeData.IsSubItem: Boolean;
begin
  Result := Item is TCustomdxBarSubItem;
end;

procedure TdxRibbonCustomizationFormItemNodeData.LoadItemLinkFromIni(AItemLink: TdxBarItemLink);
var
  AItemLinks: TdxBarItemLinks;
  ASection: string;
  ASource: TCustomIniFile;
  AIndexInIni: Integer;
begin
  AItemLinks := AItemLink.Collection;

  if BaseIniSection <> '' then
    ASection := BaseIniSection
  else
    if AItemLinks.Owner is TdxBar then
      ASection := GetBarIniSection(AItemLinks.BarManager, (AItemLinks.Owner as TdxBar).Index)
    else
      if AItemLinks.Owner is TCustomdxBarSubItem then
        ASection := GetSubItemIniSection(AItemLinks.BarManager, AItemLinks.Owner as TCustomdxBarSubItem)
      else
        ASection := '';

  ASource := TdxBarManagerAccess(AItemLinks.BarManager).ReadSavedState;
  try
    for AIndexInIni := 0 to ASource.ReadInteger(ASection, 'ItemLinkCount', 0) - 1 do
    begin
      if SameText(Item.Name, ASource.ReadString(GetItemLinkIniSection(ASection, AIndexInIni), 'ItemName', '')) then
        Break;
    end;
    TdxBarItemLinkAccess(AItemLink).LoadFromIni(ASource, ASection, AIndexInIni, skIni);
  finally
    ASource.Free;
  end;
end;

function TdxRibbonCustomizationFormItemNodeData.GetIsNewItemLinkNeeded: Boolean;
begin
  Result := FIsNewItemLinkNeeded or (ItemLink = nil);
end;

function TdxRibbonCustomizationFormItemNodeData.GetItemAsSubItem: TCustomdxBarSubItem;
begin
  Result := Item as TCustomdxBarSubItem;
end;

procedure TdxRibbonCustomizationFormItemNodeData.SetIsNewItemNeeded(AValue: Boolean);
begin
  if IsNewItemNeeded <> AValue then
  begin
    FIsNewItemNeeded := AValue;
    if IsNewItemNeeded then
      ItemLink := nil;
  end;
end;

{ TdxRibbonCustomizationFormMergedTabInfo }

constructor TdxRibbonCustomizationFormMergedTabInfo.Create(AOwnerTab: TdxRibbonTab);
begin
  inherited Create;
  FOwnerTab := TdxRibbonTabAccess(AOwnerTab);
  FMergedTabs := TcxObjectList.Create(False);
end;

destructor TdxRibbonCustomizationFormMergedTabInfo.Destroy;
begin
  FreeAndNil(FMergedTabs);
  inherited Destroy;
end;

procedure TdxRibbonCustomizationFormMergedTabInfo.StoreMergedState;
begin
  MergedTabs.Assign(TdxRibbonTabAccess(OwnerTab).MergeData.Children);
  OwnerTab.Unmerge;
end;

procedure TdxRibbonCustomizationFormMergedTabInfo.RestoreMergedState;
var
  I: Integer;
begin
  for I := 0 to MergedTabs.Count - 1 do
    OwnerTab.Merge(TdxRibbonTab(MergedTabs[I]));
  MergedTabs.Clear;
end;

{ TdxRibbonCustomizationFormMergedTabsList }

constructor TdxRibbonCustomizationFormMergedTabsList.Create(ATabs: TdxRibbonTabCollection);
begin
  inherited Create;
  FTabs := ATabs;
end;

procedure TdxRibbonCustomizationFormMergedTabsList.StoreMergedState;
var
  ATab: TdxRibbonTabAccess;
  I: Integer;
begin
  for I := 0 to Tabs.Count - 1 do
  begin
    ATab := TdxRibbonTabAccess(Tabs[I]);
    if ATab.IsMerged and not ATab.MergeData.CreatedByMerging then
      AddTabInfo(ATab).StoreMergedState;
  end;
end;

procedure TdxRibbonCustomizationFormMergedTabsList.RestoreMergedState;
var
  ATabInfo: TdxRibbonCustomizationFormMergedTabInfo;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    ATabInfo := Items[I];
    if Tabs.IndexOf(ATabInfo.OwnerTab) >= 0 then
      ATabInfo.RestoreMergedState;
  end;
  Clear;
end;

function TdxRibbonCustomizationFormMergedTabsList.AddTabInfo(ATab: TdxRibbonTab): TdxRibbonCustomizationFormMergedTabInfo;
var
  AIndex: Integer;
begin
  AIndex := Add(TdxRibbonCustomizationFormMergedTabInfo.Create(ATab));
  Result := Items[AIndex];
end;

function TdxRibbonCustomizationFormMergedTabsList.GetItem(AIndex: Integer): TdxRibbonCustomizationFormMergedTabInfo;
begin
  Result := TdxRibbonCustomizationFormMergedTabInfo(inherited Items[AIndex]);
end;

{ TdxCustomRibbonCustomizationFormHelper }

constructor TdxCustomRibbonCustomizationFormHelper.Create(ARibbon: TdxCustomRibbon;
  ACommandsTreeList, ARibbonTreeList: TcxTreeList; ACommandsComboBox, ARibbonComboBox: TcxComboBox);
begin
  inherited Create;
  FRibbon := ARibbon;
  FCommandsComboBox := ACommandsComboBox;
  FCommandsTreeList := ACommandsTreeList;
  FRibbonComboBox := ARibbonComboBox;
  FRibbonTreeList := ARibbonTreeList;
  FMergedTabsList := TdxRibbonCustomizationFormMergedTabsList.Create(Ribbon.Tabs);
  FMergedTabsList.StoreMergedState;
  FSavedState := TdxBarManagerAccess(Ribbon.BarManager).ReadSavedState;
end;

destructor TdxCustomRibbonCustomizationFormHelper.Destroy;
begin
  CommandsTreeList.Root.DeleteChildren;
  RibbonTreeList.Root.DeleteChildren;
  FMergedTabsList.RestoreMergedState;
  FreeAndNil(FMergedTabsList);
  FreeAndNil(FSavedState);
  inherited Destroy;
end;

procedure TdxCustomRibbonCustomizationFormHelper.Initialize;
begin
  PopulateCommandsComboBoxContent;
  PopulateRibbonComboBoxContent;
  PopulateCommandsTreeListContent;
  PopulateRibbonTreeListContent;
end;

procedure TdxCustomRibbonCustomizationFormHelper.AddNode(ANode: TcxTreeListNode);
begin
  InitializeMovedNode(ANode);
  DropMovedNodeTo(FocusedNode);
  ReleaseMovedNode;
  repeat
    ANode := ANode.getNextSibling;
  until (ANode = nil) or ANode.Visible;
  if ANode <> nil then
    ANode.Focused := True;
end;

procedure TdxCustomRibbonCustomizationFormHelper.CheckFocusedNode;
begin
  if (RibbonTreeList.FocusedNode = nil) and (RibbonTreeList.Count > 0) and (FocusedNode <> nil) then
    RibbonTreeList.FocusedNode := FocusedNode;
end;

procedure TdxCustomRibbonCustomizationFormHelper.DropMovedNodeTo(ATargetNode: TcxTreeListNode;
  const ACoordinate: Integer = -1);
begin
  if CanMoveNodeTo(MovedNode, ATargetNode, ACoordinate) then
    DoDropMovedNodeTo(ATargetNode, ACoordinate);
end;

procedure TdxCustomRibbonCustomizationFormHelper.FreeNodeData(ANode: TcxTreeListNode);
begin
  if TObject(ANode.Data) is TdxRibbonCustomizationFormItemNodeData then
  begin
    TObject(ANode.Data).Free;
    ANode.Data := nil;
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.InitializeMovedNode(ANode: TcxTreeListNode);
begin
  LockContentUpdating;
  FMovedNode := ANode;
end;

procedure TdxCustomRibbonCustomizationFormHelper.ReleaseMovedNode;
begin
  FMovedNode := nil;
  UnlockContentUpdating;
end;

procedure TdxCustomRibbonCustomizationFormHelper.RenameNode(ANode: TcxTreeListNode; const ANewCaption: string);
begin
  ANode.Texts[0] := ANewCaption;
end;

procedure TdxCustomRibbonCustomizationFormHelper.UpdateRibbonActiveTab;
var
  I: Integer;
begin
  if Ribbon.ActiveTab = nil then
    for I := 0 to Ribbon.TabCount - 1 do
      if TdxRibbonTabAccess(Ribbon.Tabs[I]).IsVisible then
      begin
        Ribbon.ActiveTab := Ribbon.Tabs[I];
        Break;
      end;
end;

function TdxCustomRibbonCustomizationFormHelper.DrawItemNode(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListEditCellViewInfo; ALookAndFeelPainter: TcxCustomLookAndFeelPainter;
  AScaleFactor: TdxScaleFactor = nil): Boolean;
const
  AHorzAlignment: array[Boolean] of Cardinal = (DT_LEFT, DT_RIGHT);
var
  ARect: TRect;
begin
  ARect := AViewInfo.BoundsRect;
  GetItemData(AViewInfo.Node).DrawImage(ACanvas, ARect, AScaleFactor);
  ARect := cxRectInflate(ARect, -cxHeaderTextOffset);
  ARect := cxRectCenterVertically(ARect, cxTextHeight(ACanvas.Handle));
  cxDrawText(ACanvas, AViewInfo.Node.Texts[0], ARect, AHorzAlignment[AViewInfo.IsRightToLeftConverted]);
  Result := True;
end;

function TdxCustomRibbonCustomizationFormHelper.CanMoveFocusedNodeDown: Boolean;
begin
  Result := IsSourceNodeValid(FocusedNode) and IsTargetNodeValid(FocusedNode, GetTargetNodeForMovingFocusedNodeDown);
end;

function TdxCustomRibbonCustomizationFormHelper.CanMoveFocusedNodeUp: Boolean;
begin
  Result := IsSourceNodeValid(FocusedNode) and IsTargetNodeValid(FocusedNode, GetTargetNodeForMovingFocusedNodeUp);
end;

function TdxCustomRibbonCustomizationFormHelper.CanRemoveFocusedNode: Boolean;
begin
  Result := FocusedNode <> nil;
end;

function TdxCustomRibbonCustomizationFormHelper.CanRenameFocusedNode: Boolean;
begin
  Result := (FocusedNode <> nil) and FocusedNode.Enabled and ((GetNodeLevel(FocusedNode) <> dxrcfContextLevel) or
    not SameText(FocusedNode.Texts[0], cxGetResourceString(@sdxRibbonCustomizationFormMainTabs)));
end;

function TdxCustomRibbonCustomizationFormHelper.CanUpdateContent: Boolean;
begin
  Result := FLockCount = 0;
end;

procedure TdxCustomRibbonCustomizationFormHelper.ConvertItemNodeToNew(ANode: TcxTreeListNode);
begin
  ANode.Enabled := True;
  GetItemData(ANode).IsNewItemLinkNeeded := True;
  GetItemData(ANode).IsNewItemNeeded := GetItemData(ANode).IsSubItem;
  ANode := ANode.getFirstChild;
  while ANode <> nil do
  begin
    ConvertItemNodeToNew(ANode);
    ANode := ANode.getNextSibling;
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.CopyNode(ASourceNode, ATargetNode: TcxTreeListNode; ACopyData: Boolean = True);
var
  I: Integer;
  ASourceNodeLevel: Integer;
  ASourceNodeData, ATargetNodeData: TdxRibbonCustomizationFormItemNodeData;
begin
  LockContentUpdating;
  try
    ATargetNode.Assign(ASourceNode);
    ATargetNode.Enabled := ASourceNode.Enabled and ATargetNode.Parent.Enabled;
    ASourceNodeLevel := GetNodeLevel(ASourceNode, True);
    ASourceNodeData := GetItemData(ASourceNode);
    if ACopyData then
      if ASourceNodeLevel = dxrcfItemLevel then
      begin
        ATargetNode.Data := ASourceNodeData.Clone;
        ATargetNodeData := GetItemData(ATargetNode);
        ATargetNodeData.IsNewItemLinkNeeded := ATargetNodeData.IsNewItemLinkNeeded or (ASourceNode.Parent <> ATargetNode.Parent);
      end
      else
        ATargetNode.Data := ASourceNode.Data;
    if (ASourceNodeLevel < dxrcfItemLevel) or ASourceNodeData.IsSubItem then
      for I := 0 to ASourceNode.Count - 1 do
        CopyNode(ASourceNode.Items[I], ATargetNode.AddChild);
  finally
    UnlockContentUpdating;
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.DeleteItemLink(AItemLink: TdxBarItemLink);
var
  AItem: TdxBarItem;
  I: Integer;
begin
  AItem := AItemLink.Item;
  if (AItem.LinkCount > 1) or FSavedState.ReadBool(GetBarManagerBaseIniSection(Ribbon.BarManager) + 'Items', AItem.Name, False) then
    AItemLink.Collection.Delete(AItemLink.Index)
  else
  begin
    if AItem is TCustomdxBarSubItem then
      for I := (AItem as TCustomdxBarSubItem).ItemLinks.Count - 1 downto 0 do
        DeleteItemLink((AItem as TCustomdxBarSubItem).ItemLinks[I]);
    AItem.Free;
  end;
end;

function TdxCustomRibbonCustomizationFormHelper.GetItemData(AItemNode: TcxTreeListNode): TdxRibbonCustomizationFormItemNodeData;
begin
  if GetNodeLevel(AItemNode, True) = dxrcfItemLevel then
    Result := TdxRibbonCustomizationFormItemNodeData(AItemNode.Data)
  else
    Result := nil;
end;

function TdxCustomRibbonCustomizationFormHelper.GetItemNodeVisible(AItem: TdxBarItem): Boolean;
begin
  Result := AItem.VisibleForCustomization and not (AItem is TdxBarSeparator);
end;

function TdxCustomRibbonCustomizationFormHelper.GetNodeFor(AData: TPersistent; AParentNode: TcxTreeListNode): TcxTreeListNode;
begin
  Result := AParentNode;
  if Result <> nil then
  begin
    Result := AParentNode.getFirstChild;
    while (Result <> nil) and not IsSameElement(Result, AData) do
      Result := Result.getNextSibling;
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.LoadItemLinksContentFormIni(AItemLinks: TdxBarItemLinks;
  ATargetNode: TcxTreeListNode; ATargetTreeList: TcxTreeList; ASource: TCustomIniFile);
var
  ABaseSection, AItemLinkSection: string;
  AItem: TdxBarItem;
  AItemLink: TdxBarItemLink;
  AItemNode: TcxTreeListNode;
  AUserCaption: string;
  I: Integer;
begin
  ABaseSection := '';
  if AItemLinks.Owner is TdxBar then
    ABaseSection := GetBarIniSection(AItemLinks.BarManager, GetBarIndexInIni(AItemLinks.Owner as TdxBar, ASource))
  else
    if AItemLinks.Owner is TCustomdxBarSubItem then
      ABaseSection := GetSubItemIniSection(AItemLinks.BarManager, AItemLinks.Owner as TCustomdxBarSubItem);
  for I := 0 to ASource.ReadInteger(ABaseSection, 'ItemLinkCount', 0) - 1 do
  begin
    AItemLinkSection := GetItemLinkIniSection(ABaseSection, I);
    AItem := Ribbon.BarManager.GetItemByName(ASource.ReadString(AItemLinkSection, 'ItemName', ''));
    if AItem <> nil then
    begin
      AItemNode := ATargetTreeList.AddChild(ATargetNode);
      AItemLink := AItemLinks.FindByItem(AItem);
      PopulateItemNodeContent(AItemNode, AItem, AItemLink, ASource.ReadBool(AItemLinkSection, 'BeginGroup', False), ASource);
      AUserCaption := ASource.ReadString(AItemLinkSection, 'UserCaption', '');
      if AUserCaption <> '' then
        AItemNode.Texts[0] := AUserCaption
      else
        AItemNode.Texts[0] := AItem.Caption;
      GetItemData(AItemNode).BaseIniSection := ABaseSection;
    end;
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.PopulateAllItems(ASource: TdxBarManager;
  const AOnlyMissingItems: Boolean = False);
var
  I: Integer;
begin
  CommandsTreeList.Columns[0].SortOrder := soAscending;
  CommandsTreeList.OnCompare := CompareNodes;
  CommandsTreeList.OptionsView.ShowRoot := True;
  for I := 0 to ASource.ItemCount - 1 do
    if not AOnlyMissingItems or not IsRibbonContainsItem(ASource.Items[I]) then
      PopulateItemNodeContent(CommandsTreeList.Add, ASource.Items[I], nil);
end;

procedure TdxCustomRibbonCustomizationFormHelper.PopulateItemNodeContent(ATargetNode: TcxTreeListNode;
  AItem: TdxBarItem; AItemLink: TdxBarItemLink; const ANeedBeginGroupNode: Boolean = False; ASource: TCustomIniFile = nil);
var
  ASubItem: TCustomdxBarSubItem;
  ASubItemLink: TdxBarItemLink;
  I: Integer;
begin
  ATargetNode.Data := TdxRibbonCustomizationFormItemNodeData.Create(AItem, AItemLink);
  ATargetNode.Texts[0] := GetItemNodeCaption(AItem, AItemLink);
  ATargetNode.Enabled := ATargetNode.Parent.Enabled;
  ATargetNode.Visible := GetItemNodeVisible(AItem);
  if GetItemData(ATargetNode).IsSubItem then
  begin
    ASubItem := TCustomdxBarSubItem(AItem);
    if ASource = nil then
      for I := 0 to ASubItem.ItemLinks.Count - 1 do
      begin
        ASubItemLink := ASubItem.ItemLinks[I];
        PopulateItemNodeContent(ATargetNode.AddChild, ASubItemLink.Item, ASubItemLink, ASubItemLink.BeginGroup);
      end
    else
      LoadItemLinksContentFormIni(ASubItem.ItemLinks, ATargetNode, ATargetNode.TreeList as TcxTreeList, ASource);
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.SetFocusedNode(ANode: TcxTreeListNode);
begin
  ANode.Focused := True;
  ANode.MakeVisible;
end;

procedure TdxCustomRibbonCustomizationFormHelper.SynchronizeMatchingNodesWith(APatternNode: TcxTreeListNode);
var
  AFocusedNode, ANode: TcxTreeListNode;
  AIsNodeExpanded: Boolean;
begin
  AFocusedNode := FocusedNode;
  APatternNode.TreeList.BeginUpdate;
  try
    ANode := APatternNode.Root.getFirstChild;
    while (ANode <> nil) and (ANode.GetNext <> nil) do
    begin
      if (GetNodeLevel(ANode, True) = GetNodeLevel(APatternNode, True)) and IsSameElement(ANode, APatternNode) and
        (ANode <> APatternNode) and ((GetNodeLevel(ANode, True) <> dxrcfItemLevel) or not GetItemData(ANode).IsNewItemNeeded) then
      begin
        AIsNodeExpanded := ANode.Expanded;
        ANode.DeleteChildren;
        CopyNode(APatternNode, ANode, False);
        ANode.Expanded := AIsNodeExpanded;
      end;
      ANode := ANode.GetNext;
    end;
  finally
    APatternNode.TreeList.EndUpdate;
  end;
  if AFocusedNode <> nil then
    AFocusedNode.Focused := True;
end;

procedure TdxCustomRibbonCustomizationFormHelper.LockContentUpdating;
begin
  Inc(FLockCount);
end;

procedure TdxCustomRibbonCustomizationFormHelper.UnlockContentUpdating;
begin
  Dec(FLockCount);
end;

procedure TdxCustomRibbonCustomizationFormHelper.DoAddNewItemLink(AItemLinkNode: TcxTreeListNode; AItemLinks: TdxBarItemLinks);
var
  ANodeData: TdxRibbonCustomizationFormItemNodeData;
begin
  ANodeData := GetItemData(AItemLinkNode);
  if (ANodeData.IsNewItemNeeded or ANodeData.IsNewItemLinkNeeded) and (AItemLinks.FindByItem(ANodeData.Item) = nil) then
  begin
    if ANodeData.IsNewItemNeeded then
      ANodeData.Item := ANodeData.CreateEqualItem;
    ANodeData.ItemLink := ANodeData.CreateEqualItemLink(AItemLinks);
    SynchronizeMatchingNodesWith(AItemLinkNode.Parent);
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.DoDeleteMissingItemLinks(AItemLinks: TdxBarItemLinks; AParentNode: TcxTreeListNode);
var
  AItemNode: TcxTreeListNode;
  I: Integer;
begin
  AItemLinks.BeginUpdate;
  try
    for I := AItemLinks.Count - 1 downto 0 do
    begin
      AItemNode := GetNodeFor(AItemLinks[I].Item, AParentNode);
      if AItemNode = nil then
        DeleteItemLink(AItemLinks[I])
      else
        if GetItemData(AItemNode).IsSubItem then
          DoDeleteMissingItemLinks(GetItemData(AItemNode).ItemAsSubItem.ItemLinks, AItemNode);
    end;
  finally
    AItemLinks.EndUpdate;
  end;
end;

procedure TdxCustomRibbonCustomizationFormHelper.DoDropMovedNodeTo(ATargetNode: TcxTreeListNode; ACoordinate: Integer);
begin
  if ACoordinate = -1 then
    ACoordinate := cxRectCenter(GetRealBounds(ATargetNode)).Y;
  if GetNodeLevel(ATargetNode, True) = GetNodeLevel(MovedNode, True) then
    if (GetNodeLevel(ATargetNode, True) = dxrcfItemLevel) and GetItemData(ATargetNode).IsSubItem then
      DoMoveNode(MovedNode, ATargetNode, IsTopNodePart(ATargetNode, ACoordinate), IsCenterNodePart(ATargetNode, ACoordinate))
    else
      DoMoveNode(MovedNode, ATargetNode, IsAboveNodeCenter(ATargetNode, ACoordinate))
  else
    DoMoveNode(MovedNode, ATargetNode, False, True);
end;

procedure TdxCustomRibbonCustomizationFormHelper.DoMoveNode(ASourceNode, ATargetNode: TcxTreeListNode;
  const AInsertBeforeTargetNode: Boolean; const ATargetNodeIsParent: Boolean = False);
var
  ANewNode: TcxTreeListNode;
begin
  if (ATargetNode <> nil) and ATargetNodeIsParent then
    ANewNode := RibbonTreeList.AddChild(ATargetNode)
  else
    if (ATargetNode = nil) or AInsertBeforeTargetNode then
      ANewNode := RibbonTreeList.Insert(ATargetNode)
    else
      if ATargetNode.getNextSibling = nil then
        ANewNode := RibbonTreeList.AddChild(ATargetNode.Parent)
      else
        ANewNode := RibbonTreeList.Insert(ATargetNode.getNextSibling);
  CopyNode(ASourceNode, ANewNode);
  AfterNodeMoved(ASourceNode, ANewNode);
  if ASourceNode.TreeList = RibbonTreeList then
    RemoveNode(ASourceNode);
end;

function TdxCustomRibbonCustomizationFormHelper.IsAboveNodeCenter(ANode: TcxTreeListNode; const ACoordinate: Integer): Boolean;
var
  ARealBounds: TRect;
begin
  ARealBounds := GetRealBounds(ANode);
  Result := not cxRectIsInvalid(ARealBounds) and (ACoordinate < cxRectCenter(ARealBounds).Y);
end;

function TdxCustomRibbonCustomizationFormHelper.IsCenterNodePart(ANode: TcxTreeListNode; const ACoordinate: Integer): Boolean;
var
  ARealBounds: TRect;
begin
  ARealBounds := GetRealBounds(ANode);
  Result := not cxRectIsInvalid(ARealBounds) and (ACoordinate >= (ARealBounds.Top + cxRectHeight(ARealBounds) / 3)) and
    (ACoordinate <= (ARealBounds.Top + 2 * cxRectHeight(ARealBounds) / 3));
end;

function TdxCustomRibbonCustomizationFormHelper.IsTopNodePart(ANode: TcxTreeListNode; const ACoordinate: Integer): Boolean;
var
  ARealBounds: TRect;
begin
  ARealBounds := GetRealBounds(ANode);
  Result := not cxRectIsInvalid(ARealBounds) and (ACoordinate < (ARealBounds.Top + cxRectHeight(ARealBounds) / 3));
end;

function TdxCustomRibbonCustomizationFormHelper.IsDuplicatedIn(ASourceNode, ATargetNode: TcxTreeListNode;
  ACoordinate: Integer): Boolean;
var
  I: Integer;
begin
  Result := GetNodeLevel(ASourceNode) >= dxrcfItemLevel;
  if Result then
  begin
    if ACoordinate = -1 then
      ACoordinate := cxRectCenter(GetRealBounds(ATargetNode)).Y;
    if (GetItemData(ATargetNode) <> nil) and not (GetItemData(ATargetNode).IsSubItem and
      IsCenterNodePart(ATargetNode, ACoordinate)) then
      ATargetNode := ATargetNode.Parent;
    Result := (ATargetNode <> ASourceNode.Parent) and (ATargetNode.Count > 0);
    if Result then
      for I := 0 to ATargetNode.Count - 1 do
      begin
        Result := IsSameElement(ASourceNode, ATargetNode.Items[I]);
        if Result then
          Break;
      end;
  end;
end;

function TdxCustomRibbonCustomizationFormHelper.IsPartOfItemsStructure(AItemNode, AStructureOwnerNode: TcxTreeListNode): Boolean;
var
  AStructurePartNode: TcxTreeListNode;
  I: Integer;
begin
  Result := (GetNodeLevel(AItemNode, True) = dxrcfItemLevel) and (GetNodeLevel(AStructureOwnerNode, True) = dxrcfItemLevel) and
    GetItemData(AStructureOwnerNode).IsSubItem and (AStructureOwnerNode.Count > 0);
  if Result then
    for I := 0 to AStructureOwnerNode.Count - 1 do
    begin
      AStructurePartNode := AStructureOwnerNode.Items[I];
      Result := IsSameElement(AItemNode, AStructurePartNode);
      if not Result and GetItemData(AStructurePartNode).IsSubItem then
        Result := IsPartOfItemsStructure(AItemNode, AStructurePartNode);
      if Result then
        Break;
    end;
end;

function TdxCustomRibbonCustomizationFormHelper.IsRecursiveInclusion(ASourceNode, ATargetNode: TcxTreeListNode;
  ACoordinate: Integer): Boolean;
begin
  if ACoordinate = -1 then
    ACoordinate := cxRectCenter(GetRealBounds(ATargetNode)).Y;
  Result := (GetNodeLevel(ASourceNode, True) = dxrcfItemLevel) and (GetNodeLevel(ATargetNode, True) = dxrcfItemLevel) and
    ((GetNodeLevel(ATargetNode) > dxrcfItemLevel) or IsCenterNodePart(ATargetNode, ACoordinate));
  if Result then
  begin
    if not GetItemData(ATargetNode).IsSubItem then
      ATargetNode := ATargetNode.Parent;
    repeat
      Result := IsSameElement(ASourceNode, ATargetNode) or IsPartOfItemsStructure(ATargetNode, ASourceNode);
      ATargetNode := ATargetNode.Parent;
    until Result or (GetNodeLevel(ATargetNode) < dxrcfItemLevel);
  end;
end;

function TdxCustomRibbonCustomizationFormHelper.IsRibbonContainsItem(AItem: TdxBarItem): Boolean;
var
  ATabIndex, AGroupIndex: Integer;
  ATab: TdxRibbonTab;
  AToolBar: TdxBar;
begin
  Result := False;
  ATabIndex := 0;
  while not Result and (ATabIndex < Ribbon.TabCount) do
  begin
    ATab := Ribbon.Tabs[ATabIndex];
    AGroupIndex := 0;
    while not Result and (AGroupIndex < ATab.Groups.Count) do
    begin
      AToolBar := ATab.Groups[AGroupIndex].ToolBar;
      Result := (AToolBar <> nil) and AToolBar.ItemLinks.HasItem(AItem);
      Inc(AGroupIndex);
    end;
    Inc(ATabIndex);
  end;
end;

function TdxCustomRibbonCustomizationFormHelper.IsSameElement(ASourceNode, ATargetNode: TcxTreeListNode): Boolean;
begin
  Result := (ASourceNode.Data <> nil) and (ATargetNode.Data <> nil) and
    (GetNodeLevel(ASourceNode, True) = GetNodeLevel(ATargetNode, True));
  case GetNodeLevel(ATargetNode, True) of
    dxrcfContextLevel, dxrcfTabLevel:
      Result := ASourceNode.Data = ATargetNode.Data;
    dxrcfGroupLevel:
      Result := Result and (TdxRibbonTabGroup(ASourceNode.Data).ToolBar = TdxRibbonTabGroup(ATargetNode.Data).ToolBar);
    dxrcfItemLevel:
      Result := Result and (GetItemData(ASourceNode).Item = GetItemData(ATargetNode).Item);
  end;
end;

function TdxCustomRibbonCustomizationFormHelper.IsSameElement(ANode: TcxTreeListNode; AData: TPersistent): Boolean;
const
  NodeClassMap: array [0..3] of TClass = (TdxRibbonContext, TdxRibbonTab, TdxRibbonTabGroup, TdxBarItem);
begin
  Result := (ANode.Data <> nil) and (AData <> nil) and AData.InheritsFrom(NodeClassMap[GetNodeLevel(ANode, True)]);
  case GetNodeLevel(ANode, True) of
    dxrcfContextLevel, dxrcfTabLevel:
      Result := TPersistent(ANode.Data) = AData;
    dxrcfGroupLevel:
      Result := Result and (TdxRibbonTabGroup(ANode.Data).ToolBar = TdxRibbonTabGroup(AData).ToolBar);
    dxrcfItemLevel:
      Result := Result and (GetItemData(ANode).Item = TdxBarItem(AData));
  end;
end;

function TdxCustomRibbonCustomizationFormHelper.IsSourceNodeValid(ASourceNode: TcxTreeListNode): Boolean;
begin
  Result := ASourceNode <> nil;
end;

function TdxCustomRibbonCustomizationFormHelper.IsTargetNodeValid(ASourceNode, ATargetNode: TcxTreeListNode): Boolean;
begin
  Result := (ATargetNode <> nil) and ATargetNode.Visible;
end;

procedure TdxCustomRibbonCustomizationFormHelper.CompareNodes(Sender: TcxCustomTreeList;
  ANode1, ANode2: TcxTreeListNode; var ACompare: Integer);

  function GetCaptionForComparison(ANode: TcxTreeListNode): string;
  begin
    Result := AnsiUpperCase(ANode.Texts[0]);
    if Pos('&', Result) = 1 then
      Result := Copy(Result, 2, Length(Result) - 1);
  end;

begin
  ACompare := CompareStr(GetCaptionForComparison(ANode1), GetCaptionForComparison(ANode2));
end;

function TdxCustomRibbonCustomizationFormHelper.GetBarIndexInIni(ABar: TdxBar; ASource: TCustomIniFile): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ASource.ReadInteger(GetBarManagerMainIniSection(ABar.BarManager), 'BarCount', 0) - 1 do
    if ASource.ReadString(GetBarIniSection(ABar.BarManager, I), 'Name', '') = ABar.Name then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
    for I := 0 to ASource.ReadInteger(GetBarManagerMainIniSection(ABar.BarManager), 'BarCount', 0) - 1 do
      if ASource.ReadString(GetBarIniSection(ABar.BarManager, I), 'Caption', '') = ABar.Caption then
      begin
        Result := I;
        Break;
      end;
end;

function TdxCustomRibbonCustomizationFormHelper.GetFocusedNode: TcxTreeListNode;
begin
  Result := RibbonTreeList.FocusedNode;
  if (Result = nil) and (RibbonTreeList.SelectionCount > 0) then
    Result := RibbonTreeList.Selections[0];
end;

function TdxCustomRibbonCustomizationFormHelper.GetItemNodeCaption(AItem: TdxBarItem; AItemLink: TdxBarItemLink): string;
begin
  if AItemLink <> nil then
    Result := AItemLink.Caption
  else
    Result := AItem.Caption;
end;

function TdxCustomRibbonCustomizationFormHelper.GetRealBounds(ANode: TcxTreeListNode): TRect;
begin
  if (ANode <> nil) and (TcxTreeListNodeAccess(ANode).ViewData <> nil) then
    Result := TcxTreeListNodeAccess(ANode).ViewData.GetRealBounds
  else
    Result := cxInvalidRect;
end;

{ TdxRibbonCustomizationFormHelper }

procedure TdxRibbonCustomizationFormHelper.BeginApplyChanges;
begin
  CommandsComboBox.ItemIndex := dxrcfCommandsAllTabs;
  SetFocusedNode(RibbonTreeList.Root.Items[0]);
  Ribbon.BeginUpdate;
end;

procedure TdxRibbonCustomizationFormHelper.CreateAddedElements;
var
  ANode: TcxTreeListNode;
  I: Integer;
begin
  for I := 1 to RibbonTreeList.AbsoluteCount - 1 do
  begin
    ANode := RibbonTreeList.AbsoluteItems[I];
    case GetNodeLevel(ANode, True) of
      dxrcfContextLevel:
        CheckAndAddContextNodeToRibbon(ANode);
      dxrcfTabLevel:
        CheckAndAddTabNodeToRibbon(ANode);
      dxrcfGroupLevel:
        CheckAndAddGroupNodeToRibbon(ANode);
      dxrcfItemLevel:
        CheckAndAddItemNodeToRibbon(ANode);
    end;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.DeleteRemovedElements;
begin
  CheckAndDeleteContextsFromRibbon;
end;

procedure TdxRibbonCustomizationFormHelper.EndApplyChanges;
begin
  TdxCustomRibbonAccess(Ribbon).RecalculateBars;
  if Ribbon.ActiveTab <> nil then
    TdxRibbonTabAccess(Ribbon.ActiveTab).Activate;
  Ribbon.EndUpdate;
end;

procedure TdxRibbonCustomizationFormHelper.ReorderElements;
var
  ANode: TcxTreeListNode;
  I: Integer;
begin
  for I := 1 to RibbonTreeList.AbsoluteCount - 1 do
  begin
    ANode := RibbonTreeList.AbsoluteItems[I];
    case GetNodeLevel(ANode, True) of
      dxrcfContextLevel:
        TdxRibbonContext(ANode.Data).Index := ANode.Index - 1;
      dxrcfTabLevel:
        begin
          TdxRibbonTab(ANode.Data).Visible := ANode.Checked;
          TdxRibbonTab(ANode.Data).Index := ANode.Index;
        end;
      dxrcfGroupLevel:
        TdxRibbonTabGroup(ANode.Data).Index := ANode.Index;
      dxrcfItemLevel:
        GetItemData(ANode).ItemLink.Index := ANode.Index;
    end;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.SynchronizeElementCaptions;
var
  ANode: TcxTreeListNode;
  I: Integer;
begin
  for I := 1 to RibbonTreeList.AbsoluteCount - 1 do
  begin
    ANode := RibbonTreeList.AbsoluteItems[I];
    case GetNodeLevel(ANode, True) of
      dxrcfContextLevel:
        TdxRibbonContext(ANode.Data).Caption := ANode.Texts[0];
      dxrcfTabLevel:
        TdxRibbonTab(ANode.Data).Caption := ANode.Texts[0];
      dxrcfGroupLevel:
        TdxRibbonTabGroup(ANode.Data).Caption := ANode.Texts[0];
      dxrcfItemLevel:
        if GetItemData(ANode).ItemLink.Caption <> ANode.Texts[0] then
          GetItemData(ANode).ItemLink.UserCaption := ANode.Texts[0];
    end;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.AddNewContext;
var
  ANewContextNode: TcxTreeListNode;
begin
  LockContentUpdating;
  try
    ANewContextNode := RibbonTreeList.Add;
    ANewContextNode.Texts[0] := cxGetResourceString(@sdxRibbonCustomizationFormNewContext);
    ANewContextNode.Focused := True;
    ANewContextNode.CheckGroupType := ncgCheckGroup;
    AddNewTab;
    ANewContextNode.Expand(True);
  finally
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.AddNewGroup;
begin
  LockContentUpdating;
  try
    AddNewNodeToFocusedNode(dxrcfGroupLevel, cxGetResourceString(@sdxRibbonCustomizationFormNewGroup));
  finally
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.AddNewTab;
var
  ANewTabNode: TcxTreeListNode;
begin
  LockContentUpdating;
  try
    ANewTabNode := AddNewNodeToFocusedNode(dxrcfTabLevel, cxGetResourceString(@sdxRibbonCustomizationFormNewTab));
    AddNewGroup;
    ANewTabNode.Expand(True);
  finally
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.ExpandAllContexts;
var
  I: Integer;
begin
  for I := 0 to RibbonTreeList.Root.Count - 1 do
    RibbonTreeList.Root.Items[I].Expand(False);
  if IsTabsInCommands then
    for I := 0 to CommandsTreeList.Root.Count - 1 do
      CommandsTreeList.Root.Items[I].Expand(False);
end;

function TdxRibbonCustomizationFormHelper.GetNodeLevel(ANode: TcxTreeListNode; const AAsTyped: Boolean = False): Integer;
begin
  if ANode = nil then
    Result := -1
  else
    if ANode.TreeList = RibbonTreeList then
      Result := ANode.Level
    else
      case CommandsComboBox.ItemIndex of
        dxrcfCommandsCommandsNotInTheRibbon, dxrcfCommandsAllCommands:
          Result := dxrcfItemLevel;
        dxrcfCommandsCustomTabsAndGroups:
          Result := ANode.Level + 1;
      else
        Result := ANode.Level;
      end;
  if AAsTyped then
    Result := Min(Result, dxrcfItemLevel);
end;

function TdxRibbonCustomizationFormHelper.IsTabsInCommands: Boolean;
begin
  Result := CommandsComboBox.ItemIndex in [dxrcfCommandsAllTabs, dxrcfCommandsMainTabs, dxrcfCommandsToolTabs];
end;

procedure TdxRibbonCustomizationFormHelper.MoveFocusedNodeDown;
var
  ASourceNode, ATargetNode: TcxTreeListNode;
begin
  ASourceNode := FocusedNode;
  ATargetNode := GetTargetNodeForMovingFocusedNodeDown;
  if GetNodeLevel(ATargetNode) = GetNodeLevel(ASourceNode) then
    DoMoveNode(ASourceNode, ATargetNode, False)
  else
    if ATargetNode.Count > 0 then
      DoMoveNode(ASourceNode, ATargetNode.getFirstChild, True)
    else
      DoMoveNode(ASourceNode, ATargetNode, False, True);
end;

procedure TdxRibbonCustomizationFormHelper.MoveFocusedNodeUp;
var
  ASourceNode, ATargetNode: TcxTreeListNode;
begin
  ASourceNode := FocusedNode;
  ATargetNode := GetTargetNodeForMovingFocusedNodeUp;
  if GetNodeLevel(ATargetNode) = GetNodeLevel(ASourceNode) then
    DoMoveNode(ASourceNode, ATargetNode, ATargetNode = ASourceNode.GetPrevSiblingVisible)
  else
    if ATargetNode.Count > 0 then
      DoMoveNode(ASourceNode, ATargetNode.GetLastChild, False)
    else
      DoMoveNode(ASourceNode, ATargetNode, True, True);
end;

procedure TdxRibbonCustomizationFormHelper.PopulateCommandsTreeListContent;
var
  I: Integer;
begin
  LockContentUpdating;
  CommandsTreeList.BeginUpdate;
  try
    CommandsTreeList.Clear;
    CommandsTreeList.Columns[0].SortOrder := soNone;
    CommandsTreeList.OnCompare := nil;
    CommandsTreeList.OptionsView.ShowRoot := False;
    CommandsTreeList.OptionsBehavior.IncSearch := CommandsComboBox.ItemIndex in [dxrcfCommandsCommandsNotInTheRibbon, dxrcfCommandsAllCommands];
    case CommandsComboBox.ItemIndex of
      dxrcfCommandsCommandsNotInTheRibbon:
        PopulateAllItems(Ribbon.BarManager, True);
      dxrcfCommandsAllCommands:
        PopulateAllItems(Ribbon.BarManager);
      dxrcfCommandsAllTabs:
        begin
          CreateContextNode(nil, CommandsTreeList);
          for I := 0 to Ribbon.Contexts.Count - 1 do
            CreateContextNode(Ribbon.Contexts[I], CommandsTreeList);
        end;
      dxrcfCommandsMainTabs:
        CreateContextNode(nil, CommandsTreeList);
      dxrcfCommandsToolTabs:
        for I := 0 to Ribbon.Contexts.Count - 1 do
          CreateContextNode(Ribbon.Contexts[I], CommandsTreeList);
      dxrcfCommandsCustomTabsAndGroups:
        PopulateCustomTabsAndGroups;
    end;
  finally
    CommandsTreeList.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.PopulateRibbonTreeListContent(const AIsReseting: Boolean = False);
var
  I: Integer;
begin
  LockContentUpdating;
  RibbonTreeList.BeginUpdate;
  try
    RibbonTreeList.Clear;
    CreateContextNode(nil, RibbonTreeList, AIsReseting);
    for I := 0 to Ribbon.Contexts.Count - 1 do
      CreateContextNode(Ribbon.Contexts[I], RibbonTreeList, AIsReseting);
    UpdateContextsVisibility;
  finally
    RibbonTreeList.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.RemoveNode(ANode: TcxTreeListNode);
var
  AParentNode: TcxTreeListNode;
begin
  ANode.DeleteChildren;
  if GetNodeLevel(ANode, True) = dxrcfItemLevel then
    AParentNode := ANode.Parent
  else
    AParentNode := nil;
  ANode.Delete;
  if AParentNode <> nil then
    SynchronizeMatchingNodesWith(AParentNode);
end;

procedure TdxRibbonCustomizationFormHelper.RenameNode(ANode: TcxTreeListNode; const ANewCaption: string);
begin
  inherited RenameNode(ANode, ANewCaption);
  case GetNodeLevel(ANode, True) of
    dxrcfGroupLevel:
      SynchronizeMatchingNodesWith(ANode);
    dxrcfItemLevel:
      SynchronizeMatchingNodesWith(ANode.Parent);
  end;
end;

procedure TdxRibbonCustomizationFormHelper.ResetTabNode(ANode: TcxTreeListNode);
var
  AExpandedNodesList: TList;
begin
  LockContentUpdating;
  AExpandedNodesList := TList.Create;

  StoreTabNodeExpanding(ANode, AExpandedNodesList);
  try
    LoadTabNodeContentFormIni(ANode, RibbonTreeList, FSavedState);
    ANode.Expand(False);
    SetFocusedNode(ANode);
  finally
    RestoreTabNodeExpanding(ANode, AExpandedNodesList);
    AExpandedNodesList.Free;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.UpdateContextsVisibility;
var
  I: Integer;
begin
  if CanUpdateContent and (RibbonTreeList.Root.Count > 0) then
  begin
    RibbonTreeList.Root.Items[0].Visible := RibbonComboBox.ItemIndex in [dxrcfRibbonAllTabs, dxrcfRibbonMainTabs];
    for I := 1 to RibbonTreeList.Root.Count - 1 do
      RibbonTreeList.Root.Items[I].Visible := RibbonComboBox.ItemIndex in [dxrcfRibbonAllTabs, dxrcfRibbonToolTabs];
    RibbonTreeList.SelectionList.Clear;
    RibbonTreeList.FocusedNode := nil;
    ExpandAllContexts;
  end;
end;

function TdxRibbonCustomizationFormHelper.CanMoveFocusedNodeDown: Boolean;
begin
  Result := inherited CanMoveFocusedNodeDown and ((RibbonComboBox.ItemIndex <> dxrcfRibbonMainTabs) or
    (GetContextNode(GetTargetNodeForMovingFocusedNodeDown) = RibbonTreeList.Root.Items[0]));
end;

function TdxRibbonCustomizationFormHelper.CanMoveFocusedNodeUp: Boolean;
begin
  Result := inherited CanMoveFocusedNodeUp and ((RibbonComboBox.ItemIndex <> dxrcfRibbonToolTabs) or
    (GetContextNode(GetTargetNodeForMovingFocusedNodeUp) <> RibbonTreeList.Root.Items[0]));
end;

function TdxRibbonCustomizationFormHelper.CanMoveNodeTo(ASourceNode, ATargetNode: TcxTreeListNode;
  const ACoordinate: Integer = -1): Boolean;
begin
  Result := IsSourceNodeValid(ASourceNode) and IsTargetNodeValid(ASourceNode, ATargetNode) and
    (not IsRibbonGroupNodeWithSharedToolBar(ASourceNode) or IsParentNodeTheSame(ASourceNode, ATargetNode)) and
    not IsDuplicatedIn(ASourceNode, ATargetNode, ACoordinate) and
    not IsRecursiveInclusion(ASourceNode, ATargetNode, ACoordinate);
end;

function TdxRibbonCustomizationFormHelper.CanRemoveFocusedNode: Boolean;
begin
  Result := inherited CanRemoveFocusedNode;
  if Result then
    case GetNodeLevel(FocusedNode, True) of
      dxrcfContextLevel:
        Result := (FocusedNode.Data = nil) and (FocusedNode.Index <> 0);
      dxrcfTabLevel:
        Result := (FocusedNode.Data = nil) or not TdxRibbonTab(FocusedNode.Data).IsPredefined;
      dxrcfGroupLevel:
        Result := (FocusedNode.Data = nil) or not TdxRibbonTabGroup(FocusedNode.Data).IsToolBarShared;
      dxrcfItemLevel:
        Result := FocusedNode.Enabled;
    end;
end;

function TdxRibbonCustomizationFormHelper.CanResetFocusedNode: Boolean;
var
  AFocusedNode: TcxTreeListNode;
begin
  AFocusedNode := FocusedNode;
  Result := (AFocusedNode <> nil) and (GetNodeLevel(AFocusedNode) > dxrcfContextLevel);
  if Result then
  begin
    while GetNodeLevel(AFocusedNode) > dxrcfTabLevel do
      AFocusedNode := AFocusedNode.Parent;
    Result := (AFocusedNode.Data <> nil) and TdxRibbonTab(AFocusedNode.Data).IsPredefined;
  end;
end;

function TdxRibbonCustomizationFormHelper.DrawContextNode(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListEditCellViewInfo; ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Boolean;
var
  ATextRect: TRect;
begin
  ALookAndFeelPainter.DrawGalleryGroupHeader(ACanvas, AViewInfo.BoundsRect);
  ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
  ACanvas.Font.Color := ALookAndFeelPainter.GetGalleryGroupTextColor;
  ATextRect := cxRectCenterVertically(AViewInfo.BoundsRect, cxTextHeight(ACanvas.Handle));
  ATextRect := cxRectInflate(ATextRect, -cxHeaderTextOffset, 0);
  cxDrawText(ACanvas, AViewInfo.Node.Texts[0], ATextRect, DT_LEFT);
  Result := True;
end;

function TdxRibbonCustomizationFormHelper.AddNewNodeToFocusedNode(
  const ATargetLevel: Integer; const ACaption: string): TcxTreeListNode;
var
  AParentNode: TcxTreeListNode;
  ANodeIndex: Integer;
begin
  ANodeIndex := -1;
  AParentNode := FocusedNode;
  while GetNodeLevel(AParentNode) >= ATargetLevel do
  begin
    ANodeIndex := AParentNode.Index;
    AParentNode := AParentNode.Parent;
  end;
  if (ANodeIndex >= 0) and (ANodeIndex < AParentNode.Count - 1) then
    Result := RibbonTreeList.Insert(AParentNode.Items[ANodeIndex + 1])
  else
    Result := RibbonTreeList.AddChild(AParentNode);
  Result.Texts[0] := ACaption;
  Result.Checked := (GetNodeLevel(Result) <> dxrcfGroupLevel) or AParentNode.Checked;
  SetFocusedNode(Result);
end;

procedure TdxRibbonCustomizationFormHelper.AfterNodeMoved(ASourceNode, ANewNode: TcxTreeListNode);
var
  AConvertNodeData: Boolean;
begin
  SetFocusedNode(ANewNode);
  AConvertNodeData := (ASourceNode.TreeList = CommandsTreeList) and not IsGroupHiddenInTab(ASourceNode, ANewNode.Parent);
  case GetNodeLevel(ANewNode, True) of
    dxrcfTabLevel:
      if AConvertNodeData then
        ConvertTabNodeToNew(ANewNode);
    dxrcfGroupLevel:
      begin
        ANewNode.Checked := ANewNode.Parent.Checked;
        if AConvertNodeData then
          ConvertGroupNodeToNew(ANewNode);
      end;
    dxrcfItemLevel:
      begin
        if AConvertNodeData then
          ConvertItemNodeToNew(ANewNode);
        SynchronizeMatchingNodesWith(ANewNode.Parent);
      end;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.ConvertGroupNodeToNew(ANode: TcxTreeListNode);
var
  I: Integer;
begin
  for I := 0 to ANode.Count - 1 do
    ConvertItemNodeToNew(ANode.Items[I]);
  ANode.Data := nil;
  ANode.Texts[0] := ANode.Texts[0] + cxGetResourceString(@sdxRibbonCustomizationFormCustomElementSuffix);
  ANode.Enabled := True;
end;

procedure TdxRibbonCustomizationFormHelper.ConvertTabNodeToNew(ANode: TcxTreeListNode);
var
  I: Integer;
begin
  for I := 0 to ANode.Count - 1 do
    ConvertGroupNodeToNew(ANode.Items[I]);
  ANode.Data := nil;
  ANode.Texts[0] := ANode.Texts[0] + cxGetResourceString(@sdxRibbonCustomizationFormCustomElementSuffix);
end;

function TdxRibbonCustomizationFormHelper.CreateContextNode(
  AContext: TdxRibbonContext; ATargetTreeList: TcxTreeList; AIsReseting: Boolean = False): TcxTreeListNode;
var
  ATab: TdxRibbonTab;
  I: Integer;
begin
  Result := ATargetTreeList.Add(nil, AContext);
  Result.Texts[0] := GetContextCaption(AContext);
  Result.CheckGroupType := ncgCheckGroup;
  for I := 0 to Ribbon.TabCount - 1 do
  begin
    ATab := Ribbon.Tabs[I];
    if (ATab.Context = AContext) and ((ATargetTreeList = RibbonTreeList) and not AIsReseting or ATab.IsPredefined) then
      CreateTabNode(ATab, Result, ATargetTreeList, AIsReseting);
  end;
  ATargetTreeList.AbsoluteItems[Result.AbsoluteIndex].Expand(False);
end;

function TdxRibbonCustomizationFormHelper.CreateTabNode(ATab: TdxRibbonTab;
  AContextNode: TcxTreeListNode; ATargetTreeList: TcxTreeList; AIsReseting: Boolean = False): TcxTreeListNode;
begin
  Result := ATargetTreeList.AddChild(AContextNode, ATab);
  if (ATargetTreeList = CommandsTreeList) or AIsReseting then
    LoadTabNodeContentFormIni(Result, ATargetTreeList, FSavedState)
  else
    PopulateTabNodeContent(Result, ATargetTreeList);

  Result.Expanded := ATab.Active;
end;

function TdxRibbonCustomizationFormHelper.GetContextNode(ASourceNode: TcxTreeListNode): TcxTreeListNode;
begin
  Result := ASourceNode;
  while GetNodeLevel(Result) > dxrcfContextLevel do
    Result := Result.Parent;
end;

function TdxRibbonCustomizationFormHelper.GetItemNodeVisible(AItem: TdxBarItem): Boolean;
begin
  Result := inherited GetItemNodeVisible(AItem) and not (AItem is TdxRibbonQuickAccessGroupButton);
end;

function TdxRibbonCustomizationFormHelper.GetTargetNodeForMovingFocusedNodeDown: TcxTreeListNode;
var
  ALevel: Integer;
begin
  Result := FocusedNode;
  if Result <> nil then
  begin
    ALevel := GetNodeLevel(Result);
    repeat
      Result := Result.GetNext;
      while (Result <> nil) and (GetNodeLevel(Result) <> ALevel) and (GetNodeLevel(Result) <> (ALevel - 1)) do
        Result := Result.GetNext;
    until (Result = nil) or (CanMoveNodeTo(FocusedNode, Result, GetRealBounds(Result).Bottom) and
      ((GetNodeLevel(FocusedNode) <= dxrcfItemLevel) or IsParentNodeTheSame(FocusedNode, Result)));
  end;
end;

function TdxRibbonCustomizationFormHelper.GetTargetNodeForMovingFocusedNodeUp: TcxTreeListNode;
var
  ALevel: Integer;
begin
  Result := FocusedNode;
  if Result <> nil then
  begin
    ALevel := GetNodeLevel(Result);
    repeat
      Result := Result.GetPrev;
      while (Result <> nil) and (GetNodeLevel(Result) <> ALevel) and ((GetNodeLevel(Result) <> (ALevel - 1)) or
        (Result = FocusedNode.Parent)) do
        Result := Result.GetPrev;
    until (Result = nil) or (CanMoveNodeTo(FocusedNode, Result, GetRealBounds(Result).Top) and
      ((GetNodeLevel(FocusedNode) <= dxrcfItemLevel) or IsParentNodeTheSame(FocusedNode, Result)));
  end;
end;

function TdxRibbonCustomizationFormHelper.LoadExistingGroupNodeContentFormIni(AToolBar: TdxBar;
  AParentTabNode: TcxTreeListNode; ATargetTreeList: TcxTreeList; ASource: TCustomIniFile): TcxTreeListNode;
var
  AGroups: TdxRibbonTabGroups;
begin
  AGroups := TdxRibbonTab(AParentTabNode.Data).Groups;
  Result := ATargetTreeList.AddChild(AParentTabNode, AGroups.FindByToolBar(AToolBar));
  Result.Enabled := IsGroupCustomizingAllowed(Result);
  LoadItemLinksContentFormIni(AToolBar.ItemLinks, Result, ATargetTreeList, ASource);
end;

function TdxRibbonCustomizationFormHelper.LoadMissingGroupNodeContentFormIni(AToolBar: TdxBar;
  AParentTabNode: TcxTreeListNode; ATargetTreeList: TcxTreeList; ASource: TCustomIniFile): TcxTreeListNode;
var
  AGroups: TdxRibbonTabGroups;
  ATempGroup: TdxRibbonTabGroup;
begin
  AGroups := TdxRibbonTab(AParentTabNode.Data).Groups;
  AGroups.BeginUpdate;
  try
    ATempGroup := AGroups.Add;
    try
      ATempGroup.ToolBar := AToolBar;
      Result := ATargetTreeList.AddChild(AParentTabNode, ATempGroup);
      LoadItemLinksContentFormIni(AToolBar.ItemLinks, Result, ATargetTreeList, ASource);
      Result.Data := nil;
      Result.Enabled := True;
    finally
      AGroups.Delete(ATempGroup.Index);
    end;
  finally
    AGroups.EndUpdate;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.LoadTabNodeContentFormIni(ANode: TcxTreeListNode;
  ATargetTreeList: TcxTreeList; ASource: TCustomIniFile);
var
  ATab: TdxRibbonTab;
  AToolBar: TdxBar;
  AToolBarVisible: Boolean;
  AGroupNode: TcxTreeListNode;
  ATabSection, AGroupSection: string;
  I: Integer;
begin
  ATab := TdxRibbonTab(ANode.Data);
  ATabSection := TdxRibbonTabCollectionAccess(Ribbon.Tabs).GetIniSection(ATab.Name, ASource);
  ANode.Texts[0] := ASource.ReadString(ATabSection, 'Caption', ATab.Caption);
  ANode.DeleteChildren;

  for I := 0 to ASource.ReadInteger(ATabSection, 'GroupCount', 0) - 1 do
  begin
    AGroupSection := TdxRibbonTabGroupsAccess(ATab.Groups).GetIniSection(I, ASource);
    AToolBar := Ribbon.BarManager.BarByComponentName(ASource.ReadString(AGroupSection, 'ToolBar', ''));
    AToolBarVisible := ASource.ReadBool(AGroupSection, 'ToolBarVisible', True);
    if (AToolBar <> nil) and ((ATargetTreeList = CommandsTreeList) or AToolBarVisible) then
    begin
      if ATab.Groups.FindByToolBar(AToolBar) <> nil then
        AGroupNode := LoadExistingGroupNodeContentFormIni(AToolBar, ANode, ATargetTreeList, ASource)
      else
        AGroupNode := LoadMissingGroupNodeContentFormIni(AToolBar, ANode, ATargetTreeList, ASource);
      AGroupNode.Texts[0] := ASource.ReadString(AGroupSection, 'Caption', AToolBar.Caption);
    end;
  end;

  for I := 0 to ANode.Count - 1 do
    SynchronizeMatchingNodesWith(ANode.Items[I]);
  ANode.Checked := ASource.ReadBool(ATabSection, 'Visible', False);
end;

procedure TdxRibbonCustomizationFormHelper.PopulateCommandsComboBoxContent;
var
  AItems: TStrings;
begin
  AItems := CommandsComboBox.Properties.Items;
  LockContentUpdating;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormCommandsNotInTheRibbon));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormAllCommands));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormAllTabs));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormMainTabs));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormToolTabs));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormCustomTabsAndGroups));
    CommandsComboBox.ItemIndex := dxrcfCommandsAllTabs;
  finally
    AItems.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.PopulateCustomTabsAndGroups;
var
  ACustomGroupsNode: TcxTreeListNode;
  ATab: TdxRibbonTab;
  AGroup: TdxRibbonTabGroup;
  I, J: Integer;
begin
  CommandsTreeList.OptionsView.ShowRoot := True;
  ACustomGroupsNode := CommandsTreeList.Add;
  ACustomGroupsNode.Texts[0] := cxGetResourceString(@sdxRibbonCustomizationFormCustomGroups);
  for I := 0 to Ribbon.TabCount - 1 do
  begin
    ATab := Ribbon.Tabs[I];
    if not ATab.IsPredefined then
      CreateTabNode(ATab, nil, CommandsTreeList).Expand(False);
    for J := 0 to ATab.Groups.Count - 1 do
    begin
      AGroup := ATab.Groups[J];
      if (AGroup.ToolBar <> nil) and not AGroup.ToolBar.IsPredefined then
        PopulateGroupNodeContent(CommandsTreeList.AddChild(ACustomGroupsNode, AGroup), CommandsTreeList);
    end;
  end;
  ACustomGroupsNode.MoveTo(CommandsTreeList.Root.Items[CommandsTreeList.Root.Count - 1], tlamAdd);
  ACustomGroupsNode.Expand(False);
end;

procedure TdxRibbonCustomizationFormHelper.PopulateGroupNodeContent(ANode: TcxTreeListNode; ATargetTreeList: TcxTreeList);
var
  AItemLinks: TdxBarItemLinks;
  AGroup: TdxRibbonTabGroup;
  I: Integer;
begin
  AGroup := TdxRibbonTabGroup(ANode.Data);
  ANode.Texts[0] := AGroup.Caption;
  ANode.Enabled := IsGroupCustomizingAllowed(ANode);
  AItemLinks := AGroup.ToolBar.ItemLinks;
  for I := 0 to AItemLinks.Count - 1 do
    PopulateItemNodeContent(ATargetTreeList.AddChild(ANode), AItemLinks[I].Item, AItemLinks[I]);
end;

procedure TdxRibbonCustomizationFormHelper.PopulateRibbonComboBoxContent;
var
  AItems: TStrings;
begin
  AItems := RibbonComboBox.Properties.Items;
  LockContentUpdating;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormAllTabs));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormMainTabs));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormToolTabs));
    RibbonComboBox.ItemIndex := dxrcfRibbonAllTabs;
  finally
    AItems.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.PopulateTabNodeContent(ANode: TcxTreeListNode; ATargetTreeList: TcxTreeList);
var
  ATab: TdxRibbonTab;
  AGroup: TdxRibbonTabGroup;
  I: Integer;
begin
  ATab := TdxRibbonTab(ANode.Data);
  ANode.Texts[0] := ATab.Caption;
  for I := 0 to ATab.Groups.Count - 1 do
  begin
    AGroup := ATab.Groups[I];
    if (AGroup.ToolBar = nil) or ((ATargetTreeList = RibbonTreeList) and
      not TdxRibbonTabGroupAccess(AGroup).IsActuallyVisible) then
      Continue;
    PopulateGroupNodeContent(ATargetTreeList.AddChild(ANode, AGroup), ATargetTreeList);
  end;
  ANode.Checked := ATab.Visible;
end;

procedure TdxRibbonCustomizationFormHelper.RestoreTabNodeExpanding(ANode: TcxTreeListNode; AStorage: TList);
var
  AGroupNode: TcxTreeListNode;
  I: Integer;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    AGroupNode := ANode.Items[I];
    if AStorage.IndexOf(AGroupNode.Data) <> -1 then
      AGroupNode.Expand(False);
  end;
  AStorage.Clear;
end;

procedure TdxRibbonCustomizationFormHelper.StoreTabNodeExpanding(ANode: TcxTreeListNode; AStorage: TList);
var
  AGroupNode: TcxTreeListNode;
  I: Integer;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    AGroupNode := ANode.Items[I];
    if (AGroupNode.Data <> nil) and AGroupNode.Expanded then
      AStorage.Add(AGroupNode.Data);
  end;
end;

procedure TdxRibbonCustomizationFormHelper.DoDropMovedNodeTo(ATargetNode: TcxTreeListNode; ACoordinate: Integer);
begin
  while GetNodeLevel(ATargetNode, True) > GetNodeLevel(MovedNode, True) do
    ATargetNode := ATargetNode.Parent;
  inherited DoDropMovedNodeTo(ATargetNode, ACoordinate);
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndAddContextNodeToRibbon(ANode: TcxTreeListNode);
var
  AContext: TdxRibbonContext;
begin
  if ANode.Data = nil then
  begin
    AContext := Ribbon.Contexts.Add;
    AContext.Visible := True;
    ANode.Data := AContext;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndAddGroupNodeToRibbon(ANode: TcxTreeListNode);
var
  AGroup: TdxRibbonTabGroup;
  ATab: TdxRibbonTab;
begin
  ATab := TdxRibbonTab(ANode.Parent.Data);
  AGroup := TdxRibbonTabGroup(ANode.Data);
  if AGroup = nil then
  begin
    AGroup := ATab.Groups.Add;
    AGroup.ToolBar := Ribbon.BarManager.AddToolBar;
    ANode.Data := AGroup;
  end
  else
    if ATab.Groups.IndexOf(AGroup) < 0 then
      TdxRibbonTabGroupAccess(AGroup).MoveToTab(ATab);
  AGroup.ToolBar.Visible := True;
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndAddItemNodeToRibbon(ANode: TcxTreeListNode);
var
  AItemLinks: TdxBarItemLinks;
begin
  if TObject(ANode.Parent.Data) is TdxRibbonTabGroup then
    AItemLinks := TdxRibbonTabGroup(ANode.Parent.Data).ToolBar.ItemLinks
  else
    AItemLinks := GetItemData(ANode.Parent).ItemAsSubItem.ItemLinks;
  DoAddNewItemLink(ANode, AItemLinks);
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndAddTabNodeToRibbon(ANode: TcxTreeListNode);
var
  AContext: TdxRibbonContext;
  ATab: TdxRibbonTab;
begin
  AContext := TdxRibbonContext(ANode.Parent.Data);
  ATab := TdxRibbonTab(ANode.Data);
  if ATab = nil then
    ATab := Ribbon.Tabs.Add;
  ATab.Context := AContext;
  ATab.Visible := ANode.Checked;
  ANode.Data := ATab;
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndDeleteContextsFromRibbon;
var
  AContexts: TdxRibbonContexts;
  I: Integer;
begin
  AContexts := Ribbon.Contexts;
  AContexts.BeginUpdate;
  try
    for I := AContexts.Count - 1 downto 0 do
    begin
      CheckAndDeleteTabsFromRibbon(AContexts[I]);
      if GetNodeFor(AContexts[I], RibbonTreeList.Root) = nil then
        AContexts.Delete(I);
    end;
    CheckAndDeleteTabsFromRibbon(nil);
  finally
    AContexts.EndUpdate;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndDeleteGroupsFromRibbon(AGroups: TdxRibbonTabGroups; ATabNode: TcxTreeListNode);
var
  AGroupNode: TcxTreeListNode;
  AGroup: TdxRibbonTabGroup;
  I: Integer;
begin
  AGroups.BeginUpdate;
  try
    for I := AGroups.Count - 1 downto 0 do
    begin
      AGroup := AGroups[I];
      if AGroup.ToolBar = nil then
        Continue;
      AGroupNode := GetNodeFor(AGroup, ATabNode);
      if AGroupNode <> nil then
        DoDeleteMissingItemLinks(AGroup.ToolBar.ItemLinks, AGroupNode)
      else
        if AGroup.ToolBar.IsPredefined then
          AGroup.ToolBar.Visible := False
        else
          Ribbon.BarManager.DeleteToolBar(AGroup.ToolBar, False);
    end;
  finally
    AGroups.EndUpdate;
  end;
end;

procedure TdxRibbonCustomizationFormHelper.CheckAndDeleteTabsFromRibbon(AContext: TdxRibbonContext);
var
  ATabs: TdxRibbonTabCollection;
  AContextNode, ATabNode: TcxTreeListNode;
  ATab: TdxRibbonTab;
  I: Integer;
begin
  ATabs := Ribbon.Tabs;
  AContextNode := GetNodeFor(AContext, RibbonTreeList.Root);
  ATabs.BeginUpdate;
  try
    for I := ATabs.Count - 1 downto 0 do
    begin
      ATab := ATabs[I];
      if ATab.Context = AContext then
      begin
        ATabNode := GetNodeFor(ATab, AContextNode);
        CheckAndDeleteGroupsFromRibbon(ATab.Groups, ATabNode);
        if ATabNode = nil then
          ATabs.Delete(I);
      end;
    end;
  finally
    ATabs.EndUpdate;
  end;
end;

function TdxRibbonCustomizationFormHelper.IsGroupCustomizingAllowed(ANode: TcxTreeListNode): Boolean;
begin
  Result := (ANode <> nil) and ((ANode.Data = nil) or (TdxRibbonTabGroup(ANode.Data).ToolBar = nil) or
    TdxRibbonTabGroup(ANode.Data).ToolBar.AllowCustomizing);
end;

function TdxRibbonCustomizationFormHelper.IsGroupHiddenInTab(AGroupNode, ATabNode: TcxTreeListNode): Boolean;
var
  AGroup: TdxRibbonTabGroup;
  I: Integer;
begin
  Result := (GetNodeLevel(AGroupNode) = dxrcfGroupLevel) and (GetNodeLevel(ATabNode) = dxrcfTabLevel) and
    (AGroupNode.Data <> nil) and (ATabNode.Data <> nil);
  if Result then
  begin
    AGroup := TdxRibbonTabGroup(AGroupNode.Data);
    Result := (AGroup.ToolBar <> nil) and TdxRibbonTab(ATabNode.Data).Groups.ContainsToolBar(AGroup.ToolBar);
    if Result then
      for I := 0 to ATabNode.Count - 1 do
        if IsSameElement(ATabNode.Items[I], AGroup) then
        begin
          Result := False;
          Break;
        end;
  end;
end;

function TdxRibbonCustomizationFormHelper.IsParentNodeTheSame(ASourceNode, ATargetNode: TcxTreeListNode): Boolean;
begin
  while GetNodeLevel(ATargetNode) > GetNodeLevel(ASourceNode) - 1 do
    ATargetNode := ATargetNode.Parent;
  Result := ASourceNode.Parent = ATargetNode;
end;

function TdxRibbonCustomizationFormHelper.IsRibbonGroupNodeWithSharedToolBar(ANode: TcxTreeListNode): Boolean;
begin
  Result := (ANode.TreeList = RibbonTreeList) and (GetNodeLevel(ANode) = dxrcfGroupLevel) and (ANode.Data <> nil) and
    TdxRibbonTabGroup(ANode.Data).IsToolBarShared;
end;

function TdxRibbonCustomizationFormHelper.IsSourceNodeValid(ASourceNode: TcxTreeListNode): Boolean;
begin
  Result := inherited IsSourceNodeValid(ASourceNode) and (GetNodeLevel(ASourceNode) <> dxrcfContextLevel) and
    ((GetNodeLevel(ASourceNode) < dxrcfItemLevel) or (ASourceNode.TreeList <> RibbonTreeList) or ASourceNode.Enabled) and
    not ((ASourceNode.TreeList = CommandsTreeList) and (CommandsComboBox.ItemIndex = dxrcfCommandsCustomTabsAndGroups) and
    (ASourceNode.Data = nil));
end;

function TdxRibbonCustomizationFormHelper.IsTargetNodeValid(ASourceNode, ATargetNode: TcxTreeListNode): Boolean;
begin
  Result := inherited IsTargetNodeValid(ASourceNode, ATargetNode) and
    (GetNodeLevel(ATargetNode) >= GetNodeLevel(ASourceNode, True) - 1) and
    (ATargetNode.Enabled or (GetNodeLevel(ASourceNode) <= dxrcfGroupLevel));
end;

function TdxRibbonCustomizationFormHelper.GetContextCaption(AContext: TdxRibbonContext): string;
begin
  if AContext <> nil then
    Result := AContext.Caption
  else
    Result := cxGetResourceString(@sdxRibbonCustomizationFormMainTabs);
end;

{ TdxRibbonQATCustomizationFormHelper }

procedure TdxRibbonQATCustomizationFormHelper.BeginApplyChanges;
begin
  Ribbon.BarManager.BeginUpdate;
end;

procedure TdxRibbonQATCustomizationFormHelper.CreateAddedElements;
var
  ANode: TcxTreeListNode;
  AItemLinks: TdxBarItemLinks;
  I: Integer;
begin
  for I := 0 to RibbonTreeList.AbsoluteCount - 1 do
  begin
    ANode := RibbonTreeList.AbsoluteItems[I];
    if ANode.Parent = RibbonTreeList.Root then
      AItemLinks := Ribbon.QuickAccessToolbar.Toolbar.ItemLinks
    else
      AItemLinks := GetItemData(ANode.Parent).ItemAsSubItem.ItemLinks;
    if not GetItemData(ANode).IsBeginGroup then
      DoAddNewItemLink(ANode, AItemLinks);
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.DeleteRemovedElements;
begin
  DoDeleteMissingItemLinks(Ribbon.QuickAccessToolbar.Toolbar.ItemLinks, RibbonTreeList.Root);
end;

procedure TdxRibbonQATCustomizationFormHelper.EndApplyChanges;
begin
  Ribbon.BarManager.EndUpdate;
end;

procedure TdxRibbonQATCustomizationFormHelper.ReorderElements;
begin
  DoReorderItemLinks(RibbonTreeList.Root);
end;

procedure TdxRibbonQATCustomizationFormHelper.SynchronizeElementCaptions;
begin
  //do nothing
end;

procedure TdxRibbonQATCustomizationFormHelper.ChangeQATPosition(const AShowBelowRibbon: Boolean);
const
  QATPositionMap: array [Boolean] of TdxQuickAccessToolbarPosition = (qtpAboveRibbon, qtpBelowRibbon);
begin
  Ribbon.QuickAccessToolbar.Position := QATPositionMap[AShowBelowRibbon];
end;

function TdxRibbonQATCustomizationFormHelper.GetNodeLevel(ANode: TcxTreeListNode; const AAsTyped: Boolean = False): Integer;
begin
  if (ANode = nil) or (ANode = ANode.Root) then
    Result := -1
  else
  begin
    Result := dxrcfItemLevel;
    if not AAsTyped then
      Inc(Result, ANode.Level);
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.InitializeQATPositionIndicator(AIndicator: TObject);
begin
  LockContentUpdating;
  try
    if AIndicator is TcxCheckBox then
      TcxCheckBox(AIndicator).Checked := Ribbon.QuickAccessToolbar.Position = qtpBelowRibbon;
  finally
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.MoveFocusedNodeDown;
begin
  DoMoveNode(FocusedNode, GetTargetNodeForMovingFocusedNodeDown, False);
end;

procedure TdxRibbonQATCustomizationFormHelper.MoveFocusedNodeUp;
begin
  DoMoveNode(FocusedNode, GetTargetNodeForMovingFocusedNodeUp, True);
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateCommandsComboBoxContent;
var
  AItems: TStrings;
  I: Integer;
begin
  AItems := CommandsComboBox.Properties.Items;
  LockContentUpdating;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormCommandsNotInTheRibbon));
    AItems.Add(cxGetResourceString(@sdxRibbonCustomizationFormAllCommands));
    PopulateContextTabs(nil, AItems);
    for I := 0 to Ribbon.Contexts.Count - 1 do
      PopulateContextTabs(Ribbon.Contexts[I], AItems);
    CommandsComboBox.ItemIndex := dxrcfCommandsAllCommands;
  finally
    AItems.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateCommandsTreeListContent;
begin
  LockContentUpdating;
  CommandsTreeList.BeginUpdate;
  try
    CommandsTreeList.Clear;
    CommandsTreeList.OptionsBehavior.IncSearch := True;
    PopulateBeginGroupNodeContent(CommandsTreeList.Add);
    case CommandsComboBox.ItemIndex of
      dxrcfCommandsCommandsNotInTheRibbon:
        PopulateAllItems(Ribbon.BarManager, True);
      dxrcfCommandsAllCommands:
        PopulateAllItems(Ribbon.BarManager);
      else
        PopulateTabItems(TdxRibbonTab(CommandsComboBox.ItemObject));
    end;
  finally
    CommandsTreeList.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateRibbonComboBoxContent;
begin
  //do nothing
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateRibbonTreeListContent(const AIsReseting: Boolean = False);
var
  AItemLinks: TdxBarItemLinks;
  I: Integer;
begin
  LockContentUpdating;
  RibbonTreeList.BeginUpdate;
  try
    RibbonTreeList.Clear;
    RibbonTreeList.OptionsView.ShowRoot := True;
    if AIsReseting then
      LoadItemLinksContentFormIni(Ribbon.QuickAccessToolbar.Toolbar.ItemLinks, RibbonTreeList.Root, RibbonTreeList, FSavedState)
    else
    begin
      AItemLinks := Ribbon.QuickAccessToolbar.Toolbar.ItemLinks;
      for I := 0 to AItemLinks.Count - 1 do
        PopulateItemNodeContent(RibbonTreeList.Add, AItemLinks[I].Item, AItemLinks[I], AItemLinks[I].BeginGroup);
    end;
  finally
    RibbonTreeList.EndUpdate;
    UnlockContentUpdating;
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.RemoveNode(ANode: TcxTreeListNode);
begin
  ANode.Delete;
end;

function TdxRibbonQATCustomizationFormHelper.CanMoveNodeTo(ASourceNode, ATargetNode: TcxTreeListNode;
  const ACoordinate: Integer = -1): Boolean;
begin
  Result := IsSourceNodeValid(ASourceNode) and ((ATargetNode = nil) or IsTargetNodeValid(ASourceNode, ATargetNode)) and
    not IsDuplicatedIn(ASourceNode, ATargetNode, ACoordinate) and
    not IsRecursiveInclusion(ASourceNode, ATargetNode, ACoordinate);
end;

function TdxRibbonQATCustomizationFormHelper.CanRemoveFocusedNode: Boolean;
begin
  Result := inherited CanRemoveFocusedNode and FocusedNode.Enabled;
end;

function TdxRibbonQATCustomizationFormHelper.CanRenameFocusedNode: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonQATCustomizationFormHelper.AfterNodeMoved(ASourceNode, ANewNode: TcxTreeListNode);
begin
  SetFocusedNode(ANewNode);
  if ASourceNode.TreeList = CommandsTreeList then
    ConvertItemNodeToNew(ANewNode);
  SynchronizeMatchingNodesWith(ANewNode.Parent);
end;

function TdxRibbonQATCustomizationFormHelper.GetTargetNodeForMovingFocusedNodeDown: TcxTreeListNode;
begin
  Result := FocusedNode.getNextSibling;
end;

function TdxRibbonQATCustomizationFormHelper.GetTargetNodeForMovingFocusedNodeUp: TcxTreeListNode;
begin
  Result := FocusedNode.getPrevSibling;
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateBeginGroupNodeContent(ATargetNode: TcxTreeListNode);
begin
  ATargetNode.Data := TdxRibbonCustomizationFormItemNodeData.Create(nil, nil);
  ATargetNode.Texts[0] := cxGetResourceString(@sdxRibbonCustomizationFormBeginGroup);
  ATargetNode.Enabled := ATargetNode.Parent.Enabled;
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateContextTabs(AContext: TdxRibbonContext; AItems: TStrings);
var
  AContextPrefix, ATabSuffix: string;
  I: Integer;
begin
  AContextPrefix := GetContextPrefix(AContext);
  ATabSuffix := cxGetResourceString(@sdxRibbonCustomizationFormTabSuffix);
  for I := 0 to Ribbon.TabCount - 1 do
    if Ribbon.Tabs[I].Context = AContext then
      AItems.AddObject(AContextPrefix + Ribbon.Tabs[I].Caption + ATabSuffix, Ribbon.Tabs[I]);
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateItemNodeContent(ATargetNode: TcxTreeListNode; AItem: TdxBarItem;
  AItemLink: TdxBarItemLink; const ANeedBeginGroupNode: Boolean = False; ASource: TCustomIniFile = nil);
begin
  inherited PopulateItemNodeContent(ATargetNode, AItem, AItemLink, ANeedBeginGroupNode, ASource);
  if (ATargetNode.TreeList = RibbonTreeList) and ANeedBeginGroupNode then
    PopulateBeginGroupNodeContent(ATargetNode.Parent.InsertChild(ATargetNode));
end;

procedure TdxRibbonQATCustomizationFormHelper.PopulateTabItems(ATab: TdxRibbonTab);
var
  AToolBar: TdxBar;
  I, J: Integer;
begin
  CommandsTreeList.OptionsView.ShowRoot := True;
  for I := 0 to ATab.Groups.Count - 1 do
  begin
    AToolBar := ATab.Groups[I].ToolBar;
    if AToolBar <> nil then
      for J := 0 to AToolBar.ItemLinks.Count - 1 do
        PopulateItemNodeContent(CommandsTreeList.Add, AToolBar.ItemLinks[J].Item, AToolBar.ItemLinks[J]);
  end;
end;

procedure TdxRibbonQATCustomizationFormHelper.DoReorderItemLinks(AOwnerNode: TcxTreeListNode);
var
  ANodeData: TdxRibbonCustomizationFormItemNodeData;
  ABeginGroupCount, I: Integer;
begin
  ABeginGroupCount := 0;
  for I := 0 to AOwnerNode.Count - 1 do
  begin
    DoReorderItemLinks(AOwnerNode.Items[I]);
    ANodeData := GetItemData(AOwnerNode.Items[I]);
    if ANodeData.IsBeginGroup then
      Inc(ABeginGroupCount)
    else
    begin
      ANodeData.ItemLink.BeginGroup := (I > 0) and GetItemData(AOwnerNode.Items[I - 1]).IsBeginGroup;
      ANodeData.ItemLink.Index := I - ABeginGroupCount;
    end;
  end;
end;

function TdxRibbonQATCustomizationFormHelper.IsDuplicatedIn(ASourceNode, ATargetNode: TcxTreeListNode;
  ACoordinate: Integer): Boolean;
begin
  if ATargetNode = nil then
    ATargetNode := RibbonTreeList.Root;
  Result := not GetItemData(ASourceNode).IsBeginGroup and inherited IsDuplicatedIn(ASourceNode, ATargetNode, ACoordinate);
end;

function TdxRibbonQATCustomizationFormHelper.IsSourceNodeValid(ASourceNode: TcxTreeListNode): Boolean;
begin
  Result := inherited IsSourceNodeValid(ASourceNode) and ((ASourceNode.TreeList <> RibbonTreeList) or ASourceNode.Enabled);
end;

function TdxRibbonQATCustomizationFormHelper.IsTargetNodeValid(ASourceNode, ATargetNode: TcxTreeListNode): Boolean;
begin
  Result := inherited IsTargetNodeValid(ASourceNode, ATargetNode) and ATargetNode.Enabled;
end;

function TdxRibbonQATCustomizationFormHelper.GetContextPrefix(AContext: TdxRibbonContext): string;
begin
  if AContext <> nil then
    Result := AContext.Caption + cxGetResourceString(@sdxRibbonCustomizationFormDelimiterContextTab)
  else
    Result := '';
end;

end.
