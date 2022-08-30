{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxNavBarAccessibility;

{$I cxVer.inc}

interface

uses
  Windows, Classes, RTLConsts, cxAccessibility, cxClasses, cxControls, dxCoreClasses,
  dxNavBar, dxNavBarBase, dxNavBarCollns;

type
  { TdxNavBarAccessibilityHelper }

  TdxNavBarAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetNavBar: TdxCustomNavBar;
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;

    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarGroupAccessibilityHelper }

  TdxNavBarGroupAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetGroup: TdxNavBarGroup;
    function GetGroupViewInfo: TdxNavBarGroupViewInfo;
    function GetNavBar: TdxCustomNavBar;
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(
      AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;

    property Group: TdxNavBarGroup read GetGroup;
    property GroupViewInfo: TdxNavBarGroupViewInfo read GetGroupViewInfo;
    property NavBar: TdxCustomNavBar read GetNavBar;
  end;

  { TdxNavBarGroupCaptionPanelAccessibilityHelper }

  TdxNavBarGroupCaptionPanelAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetGroup: TdxNavBarGroup;
  protected
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(
      AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function CanBeFocusedByDefault: Boolean; override;
    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function GetClipBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;
    procedure MakeVisible; override;

    property Group: TdxNavBarGroup read GetGroup;
  end;

  { TdxNavBarItemLinkContainerAccessibilityHelper }

  TdxNavBarItemLinkContainerAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetGroup: TdxNavBarGroup;
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(
      AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;
    function IsScrollable(
      out AInvisiblePartHeight, AChildMinTopScreenBound: Integer): Boolean; override;

    property Group: TdxNavBarGroup read GetGroup;
  end;

  { TdxNavBarItemLinkAccessibilityHelper }

  TdxNavBarItemLinkAccessibilityHelper = class(TdxNavBarCustomAccessibilityHelper)
  private
    function GetLink: TdxNavBarItemLink;
    function GetLinkViewInfo: TdxNavBarLinkViewInfo;
  protected
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(
      AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    // IdxNavBarAccessibilityHelper
    procedure RemoveFocus; override;

    function CanBeFocusedByDefault: Boolean; override;
    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function GetClipBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;
    procedure MakeVisible; override;

    property Link: TdxNavBarItemLink read GetLink;
    property LinkViewInfo: TdxNavBarLinkViewInfo read GetLinkViewInfo;
  end;

implementation

uses
  Math, Types, cxGraphics, cxGeometry;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarViewInfoAccess = class(TdxNavBarViewInfo);

function GetWindowClientOrigin(AWnd: HWND): TPoint;
begin
  Result := cxNullPoint;
  ClientToScreen(AWnd, Result);
end;

{ TdxNavBarAccessibilityHelper }

function TdxNavBarAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
    Result := NavBar.RootGroups[AIndex - inherited GetChildCount].IAccessibilityHelper.GetHelper;
end;

function TdxNavBarAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + NavBar.RootGroupCount;
end;

function TdxNavBarAccessibilityHelper.GetBounds: TRect;
begin
  Result := cxGetWindowRect(OwnerObjectWindow);
  Result := cxRectOffset(Result, cxPointInvert(OwnerObjectControl.ClientOrigin));
end;

function TdxNavBarAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarAccessibilityHelper.GetNavBar: TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(FOwnerObject);
end;

{ TdxNavBarGroupAccessibilityHelper }

function TdxNavBarGroupAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
  begin
    Dec(AIndex, inherited GetChildCount);
    // Requires
    Assert(AIndex < 2);
    //
    if AIndex = 0 then
      Result := Group.CaptionPanelIAccessibilityHelper.GetHelper
    else
      Result := Group.LinkContainerIAccessibilityHelper.GetHelper;
  end;
end;

function TdxNavBarGroupAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + 2;
end;

function TdxNavBarGroupAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  if Group.Parent = nil then
    Result := NavBar.IAccessibilityHelper.GetHelper
  else
    Result := Group.Parent.LinkContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarGroupAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not Group.Visible or (GroupViewInfo = nil) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarGroupAccessibilityHelper.GetBounds: TRect;
begin
  Result := GroupViewInfo.Rect;
end;

function TdxNavBarGroupAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarGroupAccessibilityHelper.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(FOwnerObject);
end;

function TdxNavBarGroupAccessibilityHelper.GetNavBar: TdxCustomNavBar;
begin
  Result := TdxCustomNavBar(OwnerObjectControl);
end;

function TdxNavBarGroupAccessibilityHelper.GetGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  Result := NavBar.ViewInfo.GetGroupViewInfoByGroup(Group);
end;

{ TdxNavBarGroupCaptionPanelAccessibilityHelper }

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := Group.IAccessibilityHelper.GetHelper;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    not (Parent as TdxNavBarGroupAccessibilityHelper).GroupViewInfo.IsCaptionVisible then
      Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarGroupCaptionPanelAccessibilityHelper.Click(AKey: Word);
var
  ACanNavBarHasActiveGroup: Boolean;
begin
  inherited Click(AKey);
  ACanNavBarHasActiveGroup := TdxNavBarViewInfoAccess(TdxCustomNavBar(
    OwnerObjectControl).ViewInfo).CanHasActiveGroup;
  if ACanNavBarHasActiveGroup then
    TdxCustomNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group)
  else
    if AKey in [VK_ADD, VK_SUBTRACT] then
      Group.Expanded := AKey = VK_ADD
    else
      Group.Expanded := not Group.Expanded;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetBounds: TRect;
begin
  Result := (Parent as TdxNavBarGroupAccessibilityHelper).GroupViewInfo.CaptionRect;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetClipBounds: TRect;
var
  AGroup: TdxNavBarGroup;
begin
  AGroup := Group;
  if AGroup.Parent = nil then
    Result := inherited GetClipBounds
  else
  begin
    while AGroup.Parent <> nil do
      AGroup := AGroup.Parent;
    Result := AGroup.LinkContainerIAccessibilityHelper.GetHelper.GetScreenBounds(cxAccessibleObjectSelfID);
  end;
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.IsClickKey(
  AKey: Word): Boolean;
var
  ACanNavBarHasActiveGroup: Boolean;
begin
  Result := inherited IsClickKey(AKey);
  if Result then
    Exit;
  ACanNavBarHasActiveGroup := TdxNavBarViewInfoAccess(TdxCustomNavBar(
    OwnerObjectControl).ViewInfo).CanHasActiveGroup;
  if ACanNavBarHasActiveGroup then
    Result := AKey in [VK_RETURN, VK_SPACE]
  else
    Result := Group.Expandable and (AKey in [VK_ADD, VK_SPACE, VK_SUBTRACT]);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.IsContainer: Boolean;
begin
  Result := False;
end;

procedure TdxNavBarGroupCaptionPanelAccessibilityHelper.MakeVisible;
begin
  TdxNavBarViewInfoAccess(TdxCustomNavBar(
    OwnerObjectControl).ViewInfo).MakeGroupVisible(Group, False, False);
end;

function TdxNavBarGroupCaptionPanelAccessibilityHelper.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(FOwnerObject);
end;

{ TdxNavBarItemLinkContainerAccessibilityHelper }

function TdxNavBarItemLinkContainerAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
var
  AChild: TObject;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
  begin
    Result := nil;
    AChild := Group.Children[AIndex - inherited GetChildCount];
    if AChild is TdxNavBarItemLink then
      Result := TdxNavBarItemLink(AChild).IAccessibilityHelper.GetHelper
    else
      if AChild is TdxNavBarGroup then
        Result := TdxNavBarGroup(AChild).IAccessibilityHelper.GetHelper;
  end;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + Group.ChildCount;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := Group.IAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and (
    not (Parent as TdxNavBarGroupAccessibilityHelper).GroupViewInfo.IsItemsVisible or
    Group.UseControl and Group.ShowControl
  ) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetBounds: TRect;
begin
  Result := TdxNavBarGroupAccessibilityHelper(Parent).GroupViewInfo.ItemsRect;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.IsScrollable(
  out AInvisiblePartHeight, AChildMinTopScreenBound: Integer): Boolean;
var
  AViewInfo: TdxNavBarViewInfo;
begin
  AViewInfo := TdxCustomNavBar(OwnerObjectControl).ViewInfo;
  if TdxCustomNavBar(OwnerObjectControl).OptionsBehavior.Common.AllowChildGroups then
    Result := TdxCustomNavBarAccess(OwnerObjectControl).ActiveGroupScrollBar.Visible
  else
    Result := AViewInfo.IsTopScrollButtonVisible or AViewInfo.IsBottomScrollButtonVisible;
  if Result then
  begin
    AInvisiblePartHeight :=
      TdxNavBarItemLinkContainerAccessibilityHelper(Children[ChildCount - 1]).GetBounds.Bottom -
      TdxNavBarItemLinkContainerAccessibilityHelper(Children[0]).GetBounds.Top -
      cxRectHeight(GetBounds);
    AChildMinTopScreenBound := TdxNavBarItemLinkContainerAccessibilityHelper(
      Children[0]).GetScreenBounds(cxAccessibleObjectSelfID).Top;
  end;
end;

function TdxNavBarItemLinkContainerAccessibilityHelper.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(FOwnerObject);
end;

{ TdxNavBarItemLinkAccessibilityHelper }

function TdxNavBarItemLinkAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := Link.Group.LinkContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemLinkAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Link.Item = nil) or not Link.CanSelect then
    Result := Result or cxSTATE_SYSTEM_UNAVAILABLE;
  if (Link.Item <> nil) and not Link.Item.Visible then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

procedure TdxNavBarItemLinkAccessibilityHelper.RemoveFocus;
var
  AGroupCaptionPanelAccessibilityHelper: TdxNavBarGroupCaptionPanelAccessibilityHelper;
begin
  AGroupCaptionPanelAccessibilityHelper := TdxNavBarGroupCaptionPanelAccessibilityHelper(
    TdxNavBarItemLinkContainerAccessibilityHelper(Parent).Group.CaptionPanelIAccessibilityHelper.GetHelper);
  if AGroupCaptionPanelAccessibilityHelper.CanFocus(True) then
    TdxCustomNavBarAccess(OwnerObjectControl).FocusedAccessibleObject := AGroupCaptionPanelAccessibilityHelper
  else
    inherited RemoveFocus;
end;

function TdxNavBarItemLinkAccessibilityHelper.CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemLinkAccessibilityHelper.Click(AKey: Word);
begin
  inherited Click(AKey);
  TdxCustomNavBarAccess(OwnerObjectControl).DoLinkMouseUp(Link);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetBounds: TRect;
begin
  if (IsRectEmpty(LinkViewInfo.ImageRect) or cxRectContain(LinkViewInfo.SelectionRect, LinkViewInfo.ImageRect)) and
    (IsRectEmpty(LinkViewInfo.CaptionRect) or cxRectContain(LinkViewInfo.SelectionRect, LinkViewInfo.CaptionRect)) then
      Result := LinkViewInfo.SelectionRect
  else
    Result := cxRectUnion(LinkViewInfo.ImageRect, LinkViewInfo.CaptionRect);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetClipBounds: TRect;
var
  AGroup: TdxNavBarGroup;
begin
  AGroup := Link.Group;
  while AGroup.Parent <> nil do
    AGroup := AGroup.Parent;
  Result := AGroup.LinkContainerIAccessibilityHelper.GetHelper.GetScreenBounds(cxAccessibleObjectSelfID);
end;

function TdxNavBarItemLinkAccessibilityHelper.IsClickKey(
  AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarItemLinkAccessibilityHelper.IsContainer: Boolean;
begin
  Result := False;
end;

procedure TdxNavBarItemLinkAccessibilityHelper.MakeVisible;
begin
  TdxNavBarViewInfoAccess(TdxCustomNavBar(OwnerObjectControl).ViewInfo).MakeLinkVisible(Link, False);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetLink: TdxNavBarItemLink;
begin
  Result := TdxNavBarItemLink(FOwnerObject);
end;

function TdxNavBarItemLinkAccessibilityHelper.GetLinkViewInfo: TdxNavBarLinkViewInfo;
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  AGroupViewInfo := (Parent.Parent as TdxNavBarGroupAccessibilityHelper).GroupViewInfo;
  Result := AGroupViewInfo.GetLinkViewInfoByLink(Link);
// Ensures
  Assert(Result <> nil);
end;

end.

