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

unit dxBarAccessibility;

{$I cxVer.inc}

interface

uses
  Types, Windows, Forms, Classes, SysUtils, dxCore, cxAccessibility, cxClasses, dxBar;

type
  TdxBarItemLinkAccessibilityHelper = class;

  { TdxDockControlAccessibilityHelper }

  TdxDockControlAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    function GetDockControl: TdxDockControl;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    property DockControl: TdxDockControl read GetDockControl;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TCustomdxBarControlAccessibilityHelper }

  TCustomdxBarControlAccessibilityHelper = class(TdxBarAccessibilityHelper)
  protected
    function GetBarControl: TCustomdxBarControl;
    function GetItemLinks: TdxBarItemLinks;

    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function CanNavigateToChildren(AKey: Word): Boolean; override;

    procedure DoClickItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper); virtual;
    procedure DoDropDownItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper); virtual;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function Expand: TCustomdxBarControlAccessibilityHelper; virtual;
    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo); virtual;
    function GetItemScreenBounds(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper): TRect;
    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; overload; virtual;
    function IsCollapsed: Boolean; virtual;
    procedure UnselectSelectedItemControl; virtual;
    function LogicalNavigationGetNextChild(AChildIndex: Integer;
      AShift: TShiftState): TdxBarAccessibilityHelper; override;

    property BarControl: TCustomdxBarControl read GetBarControl;
    property ItemLinks: TdxBarItemLinks read GetItemLinks;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxBarControlAccessibilityHelper }

  TdxBarControlAccessibilityHelper = class(TCustomdxBarControlAccessibilityHelper)
  private
    function GetBar: TdxBar;
    function GetBarControl: TdxBarControl;
  protected
    // IdxBarAccessibilityHelper
    function GetDefaultAccessibleObject: IdxBarAccessibilityHelper; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetParent: TcxAccessibilityHelper; override;

//    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
//    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
//    function GetHitTest(AScreenX, AScreenY: Integer; out AChildIndex: Integer): TcxAccessibleObjectHitTest; override;
//    function GetName(AChildID: TcxAccessibleSimpleChildElementID): string; override;
//    function GetSupportedProperties(
//      AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties; override;
//    function NavigateToChild(ACurrentChildIndex: Integer; ADirection: TcxAccessibilityNavigationDirection): Integer; override;

    procedure GetCaptionButtonKeyTipPosition(ACaptionButton: TdxBarCaptionButton;
      out ABasePointY: Integer; out AVertAlign: TcxAlignmentVert); virtual;
    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;

    property Bar: TdxBar read GetBar;
    property BarControl: TdxBarControl read GetBarControl;
  end;

  { TdxBarControlMarkAccessibilityHelper }

  TdxBarControlMarkAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    function GetBarControl: TdxBarControl;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    procedure DropDown;
    procedure UpdateMarkState;

    property BarControl: TdxBarControl read GetBarControl;
  public
    procedure CloseUpHandler(AReason: TdxBarCloseUpReason);
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxBarCaptionButtonsAccessibilityHelper }

  TdxBarCaptionButtonsAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    function GetCaptionButtons: TdxBarCaptionButtons;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    procedure UpdateCaptionButtons;

    property CaptionButtons: TdxBarCaptionButtons read GetCaptionButtons;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxBarCaptionButtonAccessibilityHelper }

  TdxBarCaptionButtonAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    procedure DoButtonClick;
    function GetCaptionButton: TdxBarCaptionButton;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetAssignedKeyTip: string; override;
    function GetDefaultKeyTip: string; override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    procedure KeyTipHandler(Sender: TObject); override;

    property CaptionButton: TdxBarCaptionButton read GetCaptionButton;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxBarSubMenuControlAccessibilityHelper }

  TdxBarSubMenuControlAccessibilityHelper = class(TCustomdxBarControlAccessibilityHelper)
  private
    function GetBarControl: TdxBarSubMenuControl;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;

    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo); override;
    function GetNextItemLink(AItemLink: TdxBarItemLink; AGoForward: Boolean): TdxBarItemLink; virtual;
    function GetParentForKeyTip: TdxBarAccessibilityHelper; override;
    procedure HandleHorzNavigationKey(ALeftKey: Boolean); virtual;
    procedure HandleVertNavigationKey(AUpKey, AFocusItemControl: Boolean); virtual;
    function IsKeyTipContainer: Boolean; override;
    procedure KeyTipsEscapeHandler; override;
    procedure UnselectSelectedItemControl; override;

    property BarControl: TdxBarSubMenuControl read GetBarControl;
  end;

  { TdxBarItemAccessibilityHelper }

  TdxBarItemAccessibilityHelper = class(TdxBarAccessibilityHelper)
  protected
    // TdxBarAccessibilityHelper
    function GetAssignedKeyTip: string; override;
    function GetDefaultKeyTip: string; override;
    procedure KeyTipHandler(Sender: TObject); override;

    function GetItem: TdxBarItem; virtual; abstract;
    function GetShortCut: TShortCut; virtual;

    procedure DoClick; virtual; abstract;
    procedure DoDropDown; virtual; abstract;
    function IsDropDownControl: Boolean; virtual;
  public
    property Item: TdxBarItem read GetItem;
  end;

  { TdxBarItemLinkAccessibilityHelper }

  TdxBarItemLinkAccessibilityHelper = class(TdxBarItemAccessibilityHelper)
  protected
    // TdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function GetNextAccessibleObject(
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    function GetParent: TcxAccessibilityHelper; override;
    // TdxBarItemAccessibilityHelper
    function GetItem: TdxBarItem; override;

    function GetItemLink: TdxBarItemLink; virtual;
    function GetItemControl: TdxBarItemControl; virtual;

    procedure DoClick; override;
    procedure DoDropDown; override;
  public
    property ItemLink: TdxBarItemLink read GetItemLink;
    property ItemControl: TdxBarItemControl read GetItemControl;
  end;

  { TdxBarItemControlAccessibilityHelper }

  TdxBarItemControlAccessibilityHelper = class(TdxBarItemLinkAccessibilityHelper)
  private
    procedure InvisibleKeyTipHandler(Sender: TObject);
  protected
    // IdxBarAccessibilityHelper
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    // TdxBarItemAccessibilityHelper
    function GetItem: TdxBarItem; override;
    function GetShortCut: TShortCut; override;
    // TdxBarItemLinkAccessibilityHelper
    function GetItemLink: TdxBarItemLink; override;
    function GetItemControl: TdxBarItemControl; override;

    function CanSelect: Boolean;
    function NeedsVisibilityForKeyTipHandling: Boolean; virtual;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxBarButtonLikeControlAccessibilityHelper }

  TdxBarButtonLikeControlAccessibilityHelper = class(TdxBarItemControlAccessibilityHelper)
  protected
    procedure DoClick; override;
    procedure DoDropDown; override;
    function IsDropDownControl: Boolean; override;
    function NeedsVisibilityForKeyTipHandling: Boolean; override;

    function ShowDropDownWindow: Boolean; virtual; abstract;
  end;

  { TdxBarButtonControlAccessibilityHelper }

  TdxBarButtonControlAccessibilityHelper = class(TdxBarButtonLikeControlAccessibilityHelper)
  private
    function GetButtonControl: TdxBarButtonControl;
  protected
    function IsDropDownControl: Boolean; override;
    function ShowDropDownWindow: Boolean; override;

    property ButtonControl: TdxBarButtonControl read GetButtonControl;
  end;

  { TdxBarSubItemControlAccessibilityHelper }

  TdxBarSubItemControlAccessibilityHelper = class(TdxBarButtonLikeControlAccessibilityHelper)
  protected
    function IsDropDownControl: Boolean; override;
    function ShowDropDownWindow: Boolean; override;
  end;

  { TdxBarWinControlAccessibilityHelper }

  TdxBarWinControlAccessibilityHelper = class(TdxBarItemControlAccessibilityHelper)
  protected
    procedure KeyTipHandler(Sender: TObject); override;
    function NeedsVisibilityForKeyTipHandling: Boolean; override;
  end;

implementation

uses
  Messages, Controls, cxContainer, cxControls, cxGeometry, cxGraphics, Menus,
  dxBarStrs;

type
  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TdxBarButtonControlAccess = class(TdxBarButtonControl);
  TdxBarControlAccess = class(TdxBarControl);
  TdxBarItemControlAccess = class(TdxBarItemControl);
  TdxBarWinControlAccess = class(TdxBarWinControl);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);
  TdxBarManagerAccess = class(TdxBarManager);
  TdxBarSubItemAccess = class(TdxBarSubItem);
  TdxBarSubItemControlAccess = class(TdxBarSubItemControl);
  TdxBarSubMenuControlAccess = class(TdxBarSubMenuControl);
  TdxBarAccessibilityHelperAccess = class(TdxBarAccessibilityHelper);

{ TdxDockControlAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxDockControlAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := DockControl.BarManager;
end;

function TdxDockControlAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
var
  I: Integer;
begin
  I := 0;
  while AIndex >= DockControl.Rows[I].ColCount do
  begin
    Dec(AIndex, DockControl.Rows[I].ColCount);
    Inc(I);
  end;
  Result := DockControl.Rows[I].Cols[AIndex].BarControl.IAccessibilityHelper.GetHelper;
end;

function TdxDockControlAccessibilityHelper.GetChildCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to DockControl.RowCount - 1 do
    Inc(Result, DockControl.Rows[I].ColCount);
end;

function TdxDockControlAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
var
  AIndex, I, J: Integer;
begin
  Result := -1;
  AIndex := 0;
  for I := 0 to DockControl.RowCount - 1 do
    for J := 0 to DockControl.Rows[I].ColCount - 1 do
      if DockControl.Rows[I].Cols[J].BarControl.IAccessibilityHelper.GetHelper = AChild then
      begin
        Result := AIndex;
        Break;
      end
      else
        Inc(AIndex);
end;

function TdxDockControlAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  if DockControl.HandleAllocated then
    Result := DockControl.Handle
  else
    Result := 0;
end;

function TdxDockControlAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
    Result := cxGetWindowRect(GetOwnerObjectWindow)
  else
    Result := cxEmptyRect;
end;

function TdxDockControlAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
var
  AHandle: HWND;
begin
  Result := cxSTATE_SYSTEM_NORMAL;
  AHandle := GetOwnerObjectWindow;
  if (AHandle = 0) or not IsWindowVisible(AHandle) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxDockControlAccessibilityHelper.GetDockControl: TdxDockControl;
begin
  Result := TdxDockControl(FOwnerObject);
end;

{ TCustomdxBarControlAccessibilityHelper }

function TCustomdxBarControlAccessibilityHelper.GetBarControl: TCustomdxBarControl;
begin
  Result := FOwnerObject as TCustomdxBarControl;
end;

function TCustomdxBarControlAccessibilityHelper.GetItemLinks: TdxBarItemLinks;
begin
  Result := BarControl.ItemLinks;
end;

// IdxBarAccessibilityHelper
function TCustomdxBarControlAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := BarControl.BarManager;
end;

function TCustomdxBarControlAccessibilityHelper.CanNavigateToChildren(AKey: Word): Boolean;
var
  ABarControlInstance: TCustomdxBarControlAccess;
begin
  ABarControlInstance := TCustomdxBarControlAccess(BarControl);
  Result := not ((ABarControlInstance.SelectedControl is TdxBarWinControl) and
    TdxBarWinControlAccess(ABarControlInstance.SelectedControl).NeedFocusOnClick and
    TdxBarWinControl(ABarControlInstance.SelectedControl).Focused);
end;

procedure TCustomdxBarControlAccessibilityHelper.DoClickItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper);
begin
//
end;

procedure TCustomdxBarControlAccessibilityHelper.DoDropDownItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper);
begin
//
end;

function TCustomdxBarControlAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := BarControl.ViewInfo.ItemControlViewInfos[AIndex].Control.IAccessibilityHelper.GetHelper;
end;

function TCustomdxBarControlAccessibilityHelper.GetChildCount: Integer;
begin
  Result := BarControl.ViewInfo.ItemControlCount;
end;

function TCustomdxBarControlAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
begin
  if AChild is TdxBarItemControlAccessibilityHelper then
    Result := BarControl.ViewInfo.GetItemControlIndex(TdxBarItemControlAccessibilityHelper(AChild).ItemControl)
  else
    Result := -1;
end;

function TCustomdxBarControlAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  if BarControl.HandleAllocated then
    Result := BarControl.Handle
  else
    Result := 0;
end;

function TCustomdxBarControlAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
    Result := cxGetWindowRect(GetOwnerObjectWindow)
  else
    Result := cxEmptyRect;
end;

function TCustomdxBarControlAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
var
  AHandle: HWND;
begin
  Result := cxSTATE_SYSTEM_NORMAL;
  AHandle := GetOwnerObjectWindow;
  if (AHandle = 0) or not IsWindowVisible(AHandle) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TCustomdxBarControlAccessibilityHelper.Expand: TCustomdxBarControlAccessibilityHelper;
begin
  raise EdxException.Create('');
end;

procedure TCustomdxBarControlAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
begin
  AKeyTipInfo.BasePoint := cxRectCenter(GetItemScreenBounds(AItemLinkHelper));
  AKeyTipInfo.HorzAlign := taCenter;
  AKeyTipInfo.VertAlign := vaCenter;
end;

function TCustomdxBarControlAccessibilityHelper.GetItemScreenBounds(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper): TRect;
begin
  Result := AItemLinkHelper.GetScreenBounds(cxAccessibleObjectSelfID);
end;

function TCustomdxBarControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
var
  AObjects: TList;
begin
  AObjects := TList.Create;
  try
    GetChildrenForNavigation(AItemLinkHelper, Self, GetItemScreenBounds(AItemLinkHelper), ADirection, True, AObjects);
    Result := dxBar.GetNextAccessibleObject(AItemLinkHelper, AObjects, ADirection, False);
  finally
    AObjects.Free;
  end;
end;

function TCustomdxBarControlAccessibilityHelper.IsCollapsed: Boolean;
begin
  Result := False;
end;

procedure TCustomdxBarControlAccessibilityHelper.UnselectSelectedItemControl;
begin
  if TdxBarManagerAccess(BarControl.BarManager).FocusedBarControl = BarControl then
    BarControl.BarLostFocus
  else
    TCustomdxBarControlAccess(BarControl).SetKeySelectedItem(nil);
end;

function TCustomdxBarControlAccessibilityHelper.LogicalNavigationGetNextChild(AChildIndex: Integer;
  AShift: TShiftState): TdxBarAccessibilityHelper;
const
  NavigationDirection: array [Boolean, Boolean] of TcxAccessibilityNavigationDirection = (
   (andRight, andLeft), (andUp, andDown));
var
  ANextBarControl: TdxBarControl;
begin
  if ssCtrl in AShift then
  begin
    ANextBarControl := TdxBarControlAccess(BarControl).GetNextBarControl(not(ssShift in AShift));
    if ANextBarControl <> nil then
      Result := ANextBarControl.IAccessibilityHelper.GetBarHelper
    else
      Result := nil;
  end
  else
    Result := TdxBarAccessibilityHelperAccess(Childs[AChildIndex]).GetNextAccessibleObject(
      NavigationDirection[BarControl.IsRealVertical, ssShift in AShift]).GetBarHelper;
end;

{ TdxBarControlAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxBarControlAccessibilityHelper.GetDefaultAccessibleObject: IdxBarAccessibilityHelper;
begin
  Result := Self;
end;

function TdxBarControlAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
var
  ATempKey: Word;
begin
  Result := inherited HandleNavigationKey(AKey);
  if Result then
    Exit;

  ATempKey := AKey;
  if BarControl.IsRealVertical then
  begin
    case ATempKey of
      VK_LEFT:
        ATempKey := VK_UP;
      VK_RIGHT:
        ATempKey := VK_DOWN;
      VK_UP:
        ATempKey := VK_LEFT;
      VK_DOWN:
        ATempKey := VK_RIGHT;
    end;
  end;
  Result := TdxBarControlAccess(BarControl).SelectedItemWantsKey(ATempKey);
end;

procedure TdxBarControlAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  BarControl.BarGetFocus(nil);
end;

procedure TdxBarControlAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  BarControl.HideAll;
end;

function TdxBarControlAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  if AIndex = BarControl.ViewInfo.ItemControlCount then
    Result := BarControl.Bar.CaptionButtons.IAccessibilityHelper.GetHelper
  else
    Result := inherited GetChild(AIndex);
end;

function TdxBarControlAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount;
  if TdxBarControlAccess(BarControl).HasCaptionButtons then
    Inc(Result);
end;

function TdxBarControlAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
begin
  if (BarControl.Bar <> nil) and
    (AChild = BarControl.Bar.CaptionButtons.IAccessibilityHelper.GetHelper)
  then
    Result := inherited GetChildCount
  else
    Result := inherited GetChildIndex(AChild);
end;

function TdxBarControlAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  if BarControl.DockControl <> nil then
    Result := BarControl.DockControl.IAccessibilityHelper.GetHelper
  else
    Result := inherited GetParent;
end;

//function TdxBarControlAccessibilityHelper.ChildIsSimpleElement(
//  AIndex: Integer): Boolean;
//begin
//  Result := False;
//end;
//
//function TdxBarControlAccessibilityHelper.GetChildIndex(
//  AChild: TcxAccessibilityHelper): Integer;
//begin
//  Result := BarControl.ViewInfo.GetItemControlIndex(
//    TdxBarItemControlAccessibilityHelper(AChild).ItemControl);
//end;
//
//function TdxBarControlAccessibilityHelper.GetHitTest(AScreenX, AScreenY: Integer;
//  out AChildIndex: Integer): TcxAccessibleObjectHitTest;
//var
//  AItemControl: TdxBarItemControl;
//begin
//  AItemControl := BarControl.ItemAtPos(BarControl.ScreenToClient(Point(AScreenX, AScreenY)));
//  if AItemControl <> nil then
//  begin
//    AChildIndex := BarControl.ViewInfo.GetItemControlIndex(AItemControl);
//    Result := aohtChild;
//  end
//  else
//    Result := aohtSelf;
//end;
//
//function TdxBarControlAccessibilityHelper.GetName(
//  AChildID: TcxAccessibleSimpleChildElementID): string;
//begin
//  Result := BarControl.Bar.Caption;
//end;
//
//function TdxBarControlAccessibilityHelper.GetSupportedProperties(
//  AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
//begin
//  Result := [aopLocation];
//end;
//
//function TdxBarControlAccessibilityHelper.NavigateToChild(
//  ACurrentChildIndex: Integer; ADirection: TcxAccessibilityNavigationDirection): Integer;
//begin
//  Result := ACurrentChildIndex;
//  case ADirection of
//    andLeft:
//      if ACurrentChildIndex > 0 then
//        Dec(Result);
//    andRight:
//      if ACurrentChildIndex + 1 < GetChildCount then
//        Inc(Result);
//  end;
//end;

procedure TdxBarControlAccessibilityHelper.GetCaptionButtonKeyTipPosition(
  ACaptionButton: TdxBarCaptionButton; out ABasePointY: Integer;
  out AVertAlign: TcxAlignmentVert);
begin
  raise EdxException.Create('');
end;

function TdxBarControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
var
  AItemLink: TdxBarItemLink;
  ARelativeDirection: TcxAccessibilityNavigationDirection;
begin
  ARelativeDirection := ADirection;
  if BarControl.IsRealVertical then
    case ADirection of
      andLeft:
        ARelativeDirection := andUp;
      andRight:
        ARelativeDirection := andDown;
      andUp:
        ARelativeDirection := andLeft;
      andDown:
        ARelativeDirection := andRight;
    end;

  if ARelativeDirection in [andUp, andDown] then
    Result := inherited GetNextAccessibleObject(AItemLinkHelper, ADirection)
  else
  begin
    if ARelativeDirection = andLeft then
      AItemLink := TdxBarItemLinksAccess(BarControl.ItemLinks).Prev(AItemLinkHelper.ItemLink)
    else
      AItemLink := TdxBarItemLinksAccess(BarControl.ItemLinks).Next(AItemLinkHelper.ItemLink);
    if (AItemLink <> nil) and (AItemLink.Control <> nil) then
      Result := AItemLink.Control.IAccessibilityHelper
    else
      Result := nil;
  end;
end;

function TdxBarControlAccessibilityHelper.GetBar: TdxBar;
begin
  Result := BarControl.Bar;//TdxBar(FOwnerObject);
end;

function TdxBarControlAccessibilityHelper.GetBarControl: TdxBarControl;
begin
  Result := inherited GetBarControl as TdxBarControl;
end;

{ TdxBarControlMarkAccessibilityHelper }

procedure TdxBarControlMarkAccessibilityHelper.CloseUpHandler(AReason: TdxBarCloseUpReason);
begin
  if (AReason = bcrCancel) and BarNavigationController.NavigationMode then
    BarControl.MarkIAccessibilityHelper.Select(False);
end;

// IdxBarAccessibilityHelper
function TdxBarControlMarkAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := BarControl.BarManager;
end;

function TdxBarControlMarkAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if not Result then
  begin
    Result := AKey in [VK_RETURN, VK_SPACE];
    if Result then
      DropDown;
  end;
end;

function TdxBarControlMarkAccessibilityHelper.IsNavigationKey(
  AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey) or
    (AKey in [VK_ESCAPE, VK_RETURN, VK_SPACE]);
end;

procedure TdxBarControlMarkAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  inherited Select(ASetFocus);
  TdxBarControlAccess(BarControl).InvalidateMark;
end;

procedure TdxBarControlMarkAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  TdxBarControlAccess(BarControl).InvalidateMark;
end;

function TdxBarControlMarkAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxBarControlMarkAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := BarControl.IAccessibilityHelper.GetHelper;
end;

function TdxBarControlMarkAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
  begin
    Result := TCustomdxBarControlAccess(BarControl).MarkRect;
    Result.TopLeft := BarControl.ClientToScreen(Result.TopLeft);
    Result.BottomRight := BarControl.ClientToScreen(Result.BottomRight);
  end
  else
    Result := cxEmptyRect;
end;

function TdxBarControlMarkAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;
end;

function TdxBarControlMarkAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not TCustomdxBarControlAccess(BarControl).MarkExists or
    IsRectEmpty(TCustomdxBarControlAccess(BarControl).MarkRect) then
      Result := Result or STATE_SYSTEM_INVISIBLE;
end;

procedure TdxBarControlMarkAccessibilityHelper.DropDown;
begin
  Unselect(nil);
  TdxBarControlAccess(BarControl).MarkState := msPressed;
end;

procedure TdxBarControlMarkAccessibilityHelper.UpdateMarkState;
begin
  TdxBarControlAccess(BarControl).CheckMarkState(
    BarControl.ScreenToClient(GetMouseCursorPos));
end;

function TdxBarControlMarkAccessibilityHelper.GetBarControl: TdxBarControl;
begin
  Result := FOwnerObject as TdxBarControl;
end;

{ TdxBarCaptionButtonsAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxBarCaptionButtonsAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := CaptionButtons.Bar.BarManager;
end;

procedure TdxBarCaptionButtonsAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  UpdateCaptionButtons;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := CaptionButtons[CaptionButtons.Count - 1 - AIndex].IAccessibilityHelper.GetHelper;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetChildCount: Integer;
begin
  Result := CaptionButtons.Count;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ChildCount - 1 do
    if CaptionButtons[I].IAccessibilityHelper.GetHelper = AChild then
    begin
      Result := ChildCount - 1 - I;
    end;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := CaptionButtons.Bar.Control.IAccessibilityHelper.GetHelper;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
var
  R: TRect;
begin
  if Visible then
  begin
    Result := CaptionButtons.Rect;
    R := cxGetWindowRect(OwnerObjectWindow);
    OffsetRect(Result, R.Left, R.Top);
  end
  else
    Result := cxEmptyRect;
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and (IsRectEmpty(CaptionButtons.Rect) or
    TCustomdxBarControlAccessibilityHelper(Parent).IsCollapsed) then
      Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

procedure TdxBarCaptionButtonsAccessibilityHelper.UpdateCaptionButtons;
var
  P: TPoint;
  R: TRect;
begin
  P := GetMouseCursorPos;
  R := cxGetWindowRect(OwnerObjectWindow);
  CaptionButtons.UpdateButtonStates(cxPointOffset(P, -R.Left, -R.Top));
end;

function TdxBarCaptionButtonsAccessibilityHelper.GetCaptionButtons: TdxBarCaptionButtons;
begin
  Result := TdxBarCaptionButtons(FOwnerObject);
end;

{ TdxBarCaptionButtonAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxBarCaptionButtonAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := Parent.BarManager;
end;

function TdxBarCaptionButtonAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if Result then
    Exit;

  Result := True;
  case AKey of
    VK_ESCAPE:
      begin
        Unselect(nil);
        TdxBarControlAccess(CaptionButton.Collection.Bar.Control).HideAllByEscape;
      end;
    VK_RETURN, VK_SPACE:
      begin
        Unselect(nil);
        DoButtonClick;
      end
  else
    Result := False;
  end;
end;

function TdxBarCaptionButtonAccessibilityHelper.IsNavigationKey(
  AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey) or (AKey in [VK_ESCAPE, VK_RETURN, VK_SPACE]);
end;

procedure TdxBarCaptionButtonAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  inherited Select(ASetFocus);
  TdxBarCaptionButtonsAccessibilityHelper(Parent).UpdateCaptionButtons;
end;

procedure TdxBarCaptionButtonAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  TdxBarCaptionButtonsAccessibilityHelper(Parent).UpdateCaptionButtons;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := TdxBarCaptionButtons(CaptionButton.Collection).IAccessibilityHelper.GetHelper;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
var
  R: TRect;
begin
  if Visible then
  begin
    Result := CaptionButton.Rect;
    R := cxGetWindowRect(OwnerObjectWindow);
    OffsetRect(Result, R.Left, R.Top);
  end
  else
    Result := cxEmptyRect;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and IsRectEmpty(CaptionButton.Rect) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetAssignedKeyTip: string;
begin
  Result := CaptionButton.KeyTip;
end;

function TdxBarCaptionButtonAccessibilityHelper.GetDefaultKeyTip: string;
begin
  Result := 'Y';
end;

procedure TdxBarCaptionButtonAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
var
  ABasePoint: TPoint;
begin
  inherited;
  if Visible then
  begin
    with GetScreenBounds(cxAccessibleObjectSelfID) do
      ABasePoint.X := (Left + Right) div 2;
    TdxBarControlAccessibilityHelper(Parent.Parent).GetCaptionButtonKeyTipPosition(
      CaptionButton, ABasePoint.Y, AKeyTipInfo.VertAlign);
    AKeyTipInfo.BasePoint := ABasePoint;
    AKeyTipInfo.HorzAlign := taCenter;
  end;
  AKeyTipInfo.Enabled := CaptionButton.Enabled;
end;

procedure TdxBarCaptionButtonAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  BarNavigationController.StopKeyboardHandling;
  DoButtonClick;
end;

procedure TdxBarCaptionButtonAccessibilityHelper.DoButtonClick;
begin
  TdxBarControlAccess(CaptionButton.Collection.Bar.Control).CaptionButtonClick(CaptionButton.Index);
end;

function TdxBarCaptionButtonAccessibilityHelper.GetCaptionButton: TdxBarCaptionButton;
begin
  Result := TdxBarCaptionButton(FOwnerObject);
end;

{ TdxBarSubMenuControlAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxBarSubMenuControlAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := ItemLinks.BarManager;
end;

function TdxBarSubMenuControlAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;
var
  ABarControl: TdxBarSubMenuControlAccess;
  AFocusItemControl: Boolean;
  AItemLink: TdxBarItemLink;
  AShift: TShiftState;
  ATempKey: Word;
begin
  Result := inherited HandleNavigationKey(AKey);
  if Result then
    Exit;

  Result := True;
  AShift := KeyboardStateToShiftState;
  ABarControl := TdxBarSubMenuControlAccess(BarControl);
  ABarControl.TerminateAnimation(True);

  if ABarControl.MarkExists then
  begin
    if (AKey = VK_DOWN) and (AShift = [ssCtrl]) then
    begin
      ABarControl.MarkState := msPressed;
      Exit;
    end
    else
      if ((AKey = VK_DOWN) or (AKey = VK_TAB) and (AShift = [])) and (ABarControl.SelectedControl <> nil) then
      begin
        AItemLink := ABarControl.SelectedControl.ItemLink;
        if AItemLink.VisibleIndex = AItemLink.Owner.VisibleItemCount - 1 then
        begin
          ABarControl.MarkState := msPressed;
          ATempKey := VK_DOWN;
          HandleNavigationKey(ATempKey);
          Exit;
        end;
      end;
  end;
  AFocusItemControl := AKey = VK_TAB;
  if AFocusItemControl then
    if AShift = [] then
      AKey := VK_DOWN
    else
      AKey := VK_UP;

  if ABarControl.SelectedItemWantsKey(AKey) then
    Exit;
  case AKey of
    VK_LEFT, VK_RIGHT:
      HandleHorzNavigationKey(AKey = VK_LEFT);
    VK_UP, VK_DOWN:
      HandleVertNavigationKey(AKey = VK_UP, AFocusItemControl);
  end;
end;

procedure TdxBarSubMenuControlAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
var
  ABarControl: TdxBarSubMenuControlAccess;
  R: TRect;
begin
  ABarControl := TdxBarSubMenuControlAccess(BarControl);
  if ABarControl.UseRightToLeftAlignment then
    AKeyTipInfo.BasePoint.X := ABarControl.ClientOrigin.X + ABarControl.BarRect.Left - ABarControl.BandSize div 2
  else
    AKeyTipInfo.BasePoint.X := ABarControl.ClientOrigin.X + ABarControl.BarRect.Right + ABarControl.BandSize div 2;
  R := GetItemScreenBounds(AItemLinkHelper);
  AKeyTipInfo.BasePoint.Y := (R.Top + R.Bottom) div 2;
  AKeyTipInfo.HorzAlign := taRightJustify;
  AKeyTipInfo.VertAlign := vaBottom;
end;

function TdxBarSubMenuControlAccessibilityHelper.GetNextItemLink(
  AItemLink: TdxBarItemLink; AGoForward: Boolean): TdxBarItemLink;
begin
  if AItemLink = nil then
  begin
    if TdxBarItemLinksAccess(ItemLinks).RealVisibleItemCount > 0 then
      Result := TdxBarItemLinksAccess(ItemLinks).First
    else
      Result := nil;
  end
  else
    if AGoForward then
      Result := TdxBarItemLinksAccess(ItemLinks).Next(AItemLink)
    else
      Result := TdxBarItemLinksAccess(ItemLinks).Prev(AItemLink);
end;

function TdxBarSubMenuControlAccessibilityHelper.GetParentForKeyTip: TdxBarAccessibilityHelper;
begin
  if BarControl.ParentItemControl <> nil then
    Result := BarControl.ParentItemControl.IAccessibilityHelper.GetBarHelper
  else
    Result := nil;
end;

procedure TdxBarSubMenuControlAccessibilityHelper.HandleHorzNavigationKey(
  ALeftKey: Boolean);
const
  ANavigationDirectionMap: array[Boolean] of TcxAccessibilityNavigationDirection =
    (andRight, andLeft);
var
  ABarControl: TdxBarSubMenuControlAccess;
  ANextObject, ASelectedObject: IdxBarAccessibilityHelper;
  AParentBarControl: TCustomdxBarControlAccess;
begin
  ABarControl := TdxBarSubMenuControlAccess(BarControl);
  if ABarControl.SelectedLink <> nil then
  begin
    ASelectedObject := ABarControl.SelectedLink.Control.IAccessibilityHelper;
    ANextObject := ASelectedObject.GetNextAccessibleObject(
      ANavigationDirectionMap[ALeftKey]);
    if (ANextObject <> nil) and (ANextObject.GetBarHelper <> ASelectedObject.GetBarHelper) then
    begin
      ASelectedObject.Unselect(ANextObject);
      ANextObject.Select(False);
      Exit;
    end;
  end;
  if ALeftKey then
  begin
    AParentBarControl := TCustomdxBarControlAccess(ABarControl.ParentBar);
    if (AParentBarControl <> nil) and (AParentBarControl.SelectedControl <> nil) then
      if ABarControl.ParentBar is TdxBarSubMenuControl then
        TdxBarItemControlAccess(AParentBarControl.SelectedControl).ControlInactivate(True)
      else
      begin
        AParentBarControl := TCustomdxBarControlAccess(GetParentBarForBar(ABarControl));
        if AParentBarControl <> nil then
          if AParentBarControl.IsRealVertical then
            SendMessage(ABarControl.Handle, WM_KEYDOWN, VK_ESCAPE, 0)
          else
            if not (bboMouseCantUnselectNavigationItem in AParentBarControl.BehaviorOptions) then
              BarNavigationController.HandleKey(VK_LEFT, [], AParentBarControl.IAccessibilityHelper, AParentBarControl.SelectedControl.IAccessibilityHelper);
      end;
  end
  else
  begin
    AParentBarControl := TCustomdxBarControlAccess(GetParentBarForBar(ABarControl));
    if (AParentBarControl <> nil) and not (bboMouseCantUnselectNavigationItem in AParentBarControl.BehaviorOptions) then
      BarNavigationController.HandleKey(VK_RIGHT, [], AParentBarControl.IAccessibilityHelper, AParentBarControl.SelectedControl.IAccessibilityHelper);
  end;
end;

procedure TdxBarSubMenuControlAccessibilityHelper.HandleVertNavigationKey(
  AUpKey, AFocusItemControl: Boolean);

  function GetHelper(ALink: TdxBarItemLink): IdxBarAccessibilityHelper;
  begin
    if (ALink <> nil) and (ALink.Control <> nil) then
      Result := ALink.Control.IAccessibilityHelper
    else
      Result := nil;
  end;

var
  ANextItemLink, APrevSelectedLink: TdxBarItemLink;
  ANextSelectedObject, APrevSelectedObject: IdxBarAccessibilityHelper;
begin
  APrevSelectedLink := TdxBarSubMenuControlAccess(BarControl).SelectedLink;
  APrevSelectedObject := GetHelper(APrevSelectedLink);
  ANextItemLink := GetNextItemLink(APrevSelectedLink, not AUpKey);
  ANextSelectedObject := GetHelper(ANextItemLink);
  if ANextSelectedObject <> nil then
    BarNavigationController.ChangeSelectedObject(AFocusItemControl, ANextSelectedObject, APrevSelectedObject);
end;

function TdxBarSubMenuControlAccessibilityHelper.IsKeyTipContainer: Boolean;
begin
  Result := True;
end;

procedure TdxBarSubMenuControlAccessibilityHelper.KeyTipsEscapeHandler;
var
  ANewKeyTipContainer: IdxBarAccessibilityHelper;
begin
  ANewKeyTipContainer := GetKeyTipContainerParent(ParentForKeyTip);
  if ANewKeyTipContainer = nil then
    ANewKeyTipContainer := ParentForKeyTip;
  SendMessage(BarControl.Handle, WM_KEYDOWN, VK_ESCAPE, 0);
  BarNavigationController.SetKeyTipsShowingState(ANewKeyTipContainer, '');
end;

procedure TdxBarSubMenuControlAccessibilityHelper.UnselectSelectedItemControl;
begin
  TdxBarSubMenuControlAccess(BarControl).SetKeySelectedItem(nil);
end;

function TdxBarSubMenuControlAccessibilityHelper.GetBarControl: TdxBarSubMenuControl;
begin
  Result := inherited GetBarControl as TdxBarSubMenuControl;
end;

{ TdxBarItemAccessibilityHelper }

function TdxBarItemAccessibilityHelper.GetAssignedKeyTip: string;
begin
  Result := Item.KeyTip;
end;

function TdxBarItemAccessibilityHelper.GetDefaultKeyTip: string;

  function IsShortCutAcceptable(AShortCut: TShortCut): Boolean;
  begin
    Result := (AShortCut >= 16449{Ctrl+A}) and (AShortCut <= 16474{Ctrl+Z})
  end;

  function GetKeyTipByShortCut(AShortCut: TShortCut): string;
  var
    AShortCutText: string;
  begin
    AShortCutText := ShortCutToText(AShortCut);
    if AShortCutText <> '' then
      Result := AShortCutText[Length(AShortCutText)]
    else
      Result := '';
  end;

var
  AnAccelPos: Integer;
  ACaption: string;
  AShortCut: TShortCut;
begin
  AShortCut := GetShortCut;
  if IsShortCutAcceptable(AShortCut) then
    Result := GetKeyTipByShortCut(AShortCut)
  else
  begin
    ACaption := Item.Caption;
    AnAccelPos := GetAccelPos(ACaption);
    if AnAccelPos <> 0 then
      Result := AnsiUpperCase(ACaption[AnAccelPos])
    else
      Result := 'Y';
  end;
end;

procedure TdxBarItemAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  BarNavigationController.StopKeyboardHandling;
  if IsDropDownControl then
    DoDropDown
  else
    DoClick;
end;

function TdxBarItemAccessibilityHelper.GetShortCut: TShortCut;
begin
  Result := Item.ShortCut;
end;

function TdxBarItemAccessibilityHelper.IsDropDownControl: Boolean;
var
  AItemLinksOwner: IdxBarLinksOwner;
begin
  Result := Supports(Item, IdxBarLinksOwner, AItemLinksOwner) and (AItemLinksOwner.GetItemLinks <> nil);
end;

{ TdxBarItemLinkAccessibilityHelper }

function TdxBarItemLinkAccessibilityHelper.GetItem: TdxBarItem;
begin
  Result := GetItemLink.Item;
end;

function TdxBarItemLinkAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := Item.BarManager;
end;

function TdxBarItemLinkAccessibilityHelper.GetNextAccessibleObject(
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
begin
  Result := TCustomdxBarControlAccessibilityHelper(Parent).GetNextAccessibleObject(Self, ADirection);
end;

function TdxBarItemLinkAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;//GetItemLink.Item.Enabled;
end;

procedure TdxBarItemLinkAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
begin
  inherited;

  (Parent as TCustomdxBarControlAccessibilityHelper).InitializeItemKeyTipPosition(Self, AKeyTipInfo);
  AKeyTipInfo.Enabled := Item.Enabled;
end;

function TdxBarItemLinkAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  if (ItemLink.BarControl <> nil) and (ItemLink.BarControl.IAccessibilityHelper <> nil) then
    Result := ItemLink.BarControl.IAccessibilityHelper.GetHelper
  else
    Result := nil;
end;

function TdxBarItemLinkAccessibilityHelper.GetItemLink: TdxBarItemLink;
begin
  Result := TdxBarItemLink(FOwnerObject);
end;

function TdxBarItemLinkAccessibilityHelper.GetItemControl: TdxBarItemControl;
begin
  Result := ItemLink.Control;
end;

procedure TdxBarItemLinkAccessibilityHelper.DoClick;
begin
  (Parent as TCustomdxBarControlAccessibilityHelper).DoClickItem(Self);
end;

procedure TdxBarItemLinkAccessibilityHelper.DoDropDown;
begin
  (Parent as TCustomdxBarControlAccessibilityHelper).DoDropDownItem(Self);
end;

{ TdxBarItemControlAccessibilityHelper }

function TdxBarItemControlAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey) or
    TCustomdxBarControlAccessibilityHelper(Parent).HandleNavigationKey(AKey);
end;

procedure TdxBarItemControlAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  if ItemControl.Parent.IsActive then
    TCustomdxBarControlAccess(ItemControl.Parent).SetKeySelectedItem(ItemControl)
  else
    ItemControl.Parent.BarGetFocus(ItemControl);

  if ItemControl.HasWindow and (ASetFocus or not TdxBarItemControlAccess(ItemControl).AllowSelectWithoutFocusing) then
    TdxBarItemControlAccess(ItemControl).Click(False);
end;

procedure TdxBarItemControlAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  if not ((ANextSelectedObject <> nil) and
    (ANextSelectedObject.GetHelper is TdxBarItemControlAccessibilityHelper) and
    (ANextSelectedObject.GetHelper.Parent = Parent)) then
      TCustomdxBarControlAccessibilityHelper(Parent).UnselectSelectedItemControl;
end;

function TdxBarItemControlAccessibilityHelper.GetItem: TdxBarItem;
begin
  Result := ItemControl.Item;
end;

function TdxBarItemControlAccessibilityHelper.GetShortCut: TShortCut;
begin
  Result := TdxBarItemControlAccess(ItemControl).ShortCut;
end;

function TdxBarItemControlAccessibilityHelper.GetItemLink: TdxBarItemLink;
begin
  Result := ItemControl.ItemLink;
end;

function TdxBarItemControlAccessibilityHelper.GetItemControl: TdxBarItemControl;
begin
  Result := TdxBarItemControl(FOwnerObject);
end;

function TdxBarItemControlAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  if ItemControl.Parent.HandleAllocated then
    Result := ItemControl.Parent.Handle
  else
    Result := 0;
end;

function TdxBarItemControlAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  if ItemControl.Parent.IAccessibilityHelper <> nil then
    Result := ItemControl.Parent.IAccessibilityHelper.GetHelper
  else
    Result := nil;
end;

function TdxBarItemControlAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
  begin
    Result := ItemControl.ViewInfo.Bounds;
    with ItemControl.Parent do
    begin
      Result.TopLeft := ClientToScreen(Result.TopLeft);
      Result.BottomRight := ClientToScreen(Result.BottomRight);
    end;
  end
  else
    Result := cxEmptyRect;
end;

function TdxBarItemControlAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible and CanSelect;
end;

function TdxBarItemControlAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
var
  AHandle: HWND;
begin
  Result := cxSTATE_SYSTEM_NORMAL;
  AHandle := GetOwnerObjectWindow;
  if (AHandle = 0) or not IsWindowVisible(AHandle) or TCustomdxBarControlAccessibilityHelper(Parent).IsCollapsed then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

//function TdxBarItemControlAccessibilityHelper.GetDescription(
//  AChildID: TcxAccessibleSimpleChildElementID): string;
//begin
//  Result := ItemControl.Item.Description;
//end;
//
//procedure TdxBarItemControlAccessibilityHelper.GetKeyboardAccessParameters(
//  AChildID: TcxAccessibleSimpleChildElementID; out AShortCut: TShortCut;
//  out ACaptionWithAccelChars: string);
//begin
//  AShortCut := ItemControl.Item.ShortCut;
//  ACaptionWithAccelChars := ItemControl.ItemLink.Caption;
//end;
//
//function TdxBarItemControlAccessibilityHelper.GetName(
//  AChildID: TcxAccessibleSimpleChildElementID): string;
//begin
//  Result := ItemControl.ItemLink.Caption;
//end;
//
//function TdxBarItemControlAccessibilityHelper.GetSupportedProperties(
//  AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
//begin
//  Result := [aopLocation, aopShortcut];
//  if GetDescription(AChildID) <> '' then
//    Include(Result, aopDescription);
//end;

function TdxBarItemControlAccessibilityHelper.CanSelect: Boolean;
begin
  Result := TdxBarItemControlAccess(ItemControl).CanSelect;
end;

procedure TdxBarItemControlAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
begin
  inherited GetKeyTipInfo(AKeyTipInfo);
  if Visible then
    (Parent as TCustomdxBarControlAccessibilityHelper).InitializeItemKeyTipPosition(Self, AKeyTipInfo)
  else
    AKeyTipInfo.OnExecute := InvisibleKeyTipHandler;
  AKeyTipInfo.Enabled := ItemControl.Enabled;
end;

function TdxBarItemControlAccessibilityHelper.NeedsVisibilityForKeyTipHandling: Boolean;
begin
  Result := False;
end;

procedure TdxBarItemControlAccessibilityHelper.InvisibleKeyTipHandler(
  Sender: TObject);
var
  AExpandedBarControlObject: TCustomdxBarControlAccessibilityHelper;
  AIndex: Integer;
begin
  if NeedsVisibilityForKeyTipHandling then
  begin
    AExpandedBarControlObject := TCustomdxBarControlAccessibilityHelper(Parent).Expand;
    AIndex := ItemControl.Parent.ViewInfo.GetItemControlIndex(ItemControl);
    TdxBarItemControlAccessibilityHelper(AExpandedBarControlObject.BarControl.ViewInfo.ItemControlViewInfos[AIndex].Control.IAccessibilityHelper.GetHelper).KeyTipHandler(Sender);
  end
  else
    KeyTipHandler(Sender);
end;

{ TdxBarButtonLikeControlAccessibilityHelper }

procedure TdxBarButtonLikeControlAccessibilityHelper.DoClick;
var
  APrevUnclickAfterDoing: Boolean;
  AItem: TdxBarItem;
begin
  AItem := Item;
  BarNavigationController.StopKeyboardHandling;
  APrevUnclickAfterDoing := AItem.UnclickAfterDoing;
  try
    AItem.UnclickAfterDoing := False;
    TdxBarItemControlAccess(ItemControl).ControlUnclick(False);
  finally
    AItem.UnclickAfterDoing := APrevUnclickAfterDoing;
  end;
end;

procedure TdxBarButtonLikeControlAccessibilityHelper.DoDropDown;
begin
  BarNavigationController.ChangeSelectedObject(True, Self);
  if ShowDropDownWindow then
  begin
    BarNavigationController.SetKeyTipsShowingState(nil, '');
    BarNavigationController.SetKeyTipsShowingState(ActiveBarControl.IAccessibilityHelper, '');
  end;
end;

function TdxBarButtonLikeControlAccessibilityHelper.IsDropDownControl: Boolean;
begin
  Result := False;
end;

function TdxBarButtonLikeControlAccessibilityHelper.NeedsVisibilityForKeyTipHandling: Boolean;
begin
  Result := IsDropDownControl;
end;

{ TdxBarButtonControlAccessibilityHelper }

function TdxBarButtonControlAccessibilityHelper.IsDropDownControl: Boolean;
begin
  Result := ButtonControl.ButtonItem.ButtonStyle in [bsDropDown, bsCheckedDropDown];
end;

function TdxBarButtonControlAccessibilityHelper.ShowDropDownWindow: Boolean;
begin
  TdxBarButtonControlAccess(ItemControl).DropDown(False);
  Result := ItemControl.IsDroppedDown;
end;

function TdxBarButtonControlAccessibilityHelper.GetButtonControl: TdxBarButtonControl;
begin
  Result := TdxBarButtonControl(FOwnerObject);
end;

{ TdxBarSubItemControlAccessibilityHelper }

function TdxBarSubItemControlAccessibilityHelper.IsDropDownControl: Boolean;
begin
  Result := True;
end;

function TdxBarSubItemControlAccessibilityHelper.ShowDropDownWindow: Boolean;
begin
  TdxBarSubItemControlAccess(ItemControl).DropDown(True);
  Result := ItemControl.IsDroppedDown;
end;

{ TdxBarWinControlAccessibilityHelper }

procedure TdxBarWinControlAccessibilityHelper.KeyTipHandler(
  Sender: TObject);
begin
  BarNavigationController.StopKeyboardHandling;
  BarNavigationController.ChangeSelectedObject(True, Self);
  if TdxBarItemControlAccess(ItemControl).AllowSelectWithoutFocusing then
    TdxBarItemControlAccess(ItemControl).AcceleratorClick;
end;

function TdxBarWinControlAccessibilityHelper.NeedsVisibilityForKeyTipHandling: Boolean;
begin
  Result := True;
end;

end.
