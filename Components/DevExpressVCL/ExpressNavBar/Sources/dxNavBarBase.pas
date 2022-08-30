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

unit dxNavBarBase;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, cxAccessibility, cxClasses, cxControls, dxCoreClasses;

type
  TdxNavBarCustomAccessibilityHelper = class;

  TdxNavBarNexusPersistent = class(TcxOwnedPersistent)
  protected
    FNotifyComponent: TcxFreeNotificator;
    procedure FreeNotification(AComponent: TComponent); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  end;

  TdxNavBarChangeType = (doRecreate, doRecalc, doRedraw);
  TdxNavBarChangeEvent = procedure(Sender: TObject; AType: TdxNavBarChangeType) of object;

  { TdxNavBarComponentCollection  }

  TdxNavBarComponentCollection = class;
  TdxNavBarComponentCollectionItemClass = class of TdxNavBarComponentCollectionItem;
  TdxNavBarComponentCollectionItem = class(TcxComponentCollectionItem)
  private
    function InternalGetCollection: TdxNavBarComponentCollection;
    procedure InternalSetCollection(Value: TdxNavBarComponentCollection);
  protected
    procedure InitiateActions; virtual;
    procedure SetIndex(Value: Integer); override;
  public
    property Collection: TdxNavBarComponentCollection read InternalGetCollection write InternalSetCollection;
  end;

  TdxNavBarComponentCollection = class(TcxComponentCollection)
  private
    function InternalGetOwner: TComponent;
  protected
    function GetItemPrefixName: string; override;
    procedure InitiateActions; virtual;
  public
    function Add: TdxNavBarComponentCollectionItem; overload;
    function ItemByName(const AName: string): TdxNavBarComponentCollectionItem;
    property Owner: TComponent read InternalGetOwner;
  end;

  IdxNavBarAccessibilityHelper = interface(IcxAccessibilityHelper)
  ['{C8DF0BFA-B9C6-4BB8-B377-8E3FAE529855}']
    procedure AttachChild(AChild: IdxNavBarAccessibilityHelper);
    function CanFocus(AFocusingByDefault: Boolean): Boolean;
    procedure DetachChild(AChild: IdxNavBarAccessibilityHelper);
    procedure FocusedChanged(AMakeVisible: Boolean);
    function GetNavBarHelper: TdxNavBarCustomAccessibilityHelper;
    function GetNextObjectForNavigation(ADirection: TcxDirection): IdxNavBarAccessibilityHelper;
    function IsFocused: Boolean;
    function IsPressed: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState);
    procedure KeyPress(var Key: Char);
    procedure KeyUp(var Key: Word; Shift: TShiftState);
    procedure RemoveFocus;
  end;

  TdxNavBarCustomAccessibilityHelper = class(TcxAccessibilityHelper,
    IdxNavBarAccessibilityHelper)
  strict private
    function NeedMakeVisible: Boolean;
  private
    FAttachedChildList: TInterfaceList;
    FOwnerObjectControl: TWinControl;
    FIsPressed: Boolean;
    FTag: Integer;
    function InternalGetChild(AIndex: Integer): TdxNavBarCustomAccessibilityHelper;
    function InternalGetParent: TdxNavBarCustomAccessibilityHelper;
  protected
    // IcxAccessibilityHelper
    procedure OwnerObjectDestroyed; override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    // IdxNavBarAccessibilityHelper
    procedure AttachChild(AChild: IdxNavBarAccessibilityHelper);
    function CanFocus(AFocusingByDefault: Boolean): Boolean; virtual;
    procedure DetachChild(AChild: IdxNavBarAccessibilityHelper);
    procedure FocusedChanged(AMakeVisible: Boolean); virtual;
    function GetNavBarHelper: TdxNavBarCustomAccessibilityHelper;
    function GetNextObjectForNavigation(
      ADirection: TcxDirection): IdxNavBarAccessibilityHelper;
    function IsFocused: Boolean;
    function IsPressed: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure RemoveFocus; virtual;

    function CanBeFocusedByDefault: Boolean; virtual;
    procedure Click(AKey: Word); virtual;
    function GetAssociatedObject: TdxNavBarCustomAccessibilityHelper; virtual;
    function GetBounds: TRect; virtual; abstract;
    function GetClipBounds: TRect; virtual;
    function InternalGetNextObjectForNavigation(
      ADirection: TcxDirection): IdxNavBarAccessibilityHelper; virtual;
    function IsChild(AChild: TdxNavBarCustomAccessibilityHelper): Boolean;
    function IsClickKey(AKey: Word): Boolean; virtual;
    function IsContainer: Boolean; virtual; abstract;
    function IsScrollable(
      out AInvisiblePartHeight, AChildMinTopScreenBound: Integer): Boolean; virtual;
    procedure MakeVisible; virtual;
    procedure SetIsPressed(Value: Boolean);

    property AssociatedObject: TdxNavBarCustomAccessibilityHelper
      read GetAssociatedObject;
    property OwnerObjectControl: TWinControl read FOwnerObjectControl;
    property Tag: Integer read FTag write FTag;
  public
    constructor Create(AOwnerObject: TObject;
      AOwnerObjectControl: TWinControl); reintroduce; virtual;
    destructor Destroy; override;
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
    property Children[AIndex: Integer]: TdxNavBarCustomAccessibilityHelper read InternalGetChild;
    property Parent: TdxNavBarCustomAccessibilityHelper read InternalGetParent;
  end;

  TdxNavBarCustomAccessibilityHelperClass = class of TdxNavBarCustomAccessibilityHelper;

procedure NavBarAccessibleObjectOwnerObjectDestroyed(
  var AAccessibleObject: IdxNavBarAccessibilityHelper);
function NavBarGetAccessibilityHelper(
  AObject: TObject): IdxNavBarAccessibilityHelper;
function NavBarGetFocusableAccessibleObjectAtPos(
  ARootAccessibleObject: IdxNavBarAccessibilityHelper;
  const P: TPoint): IdxNavBarAccessibilityHelper;

implementation

uses
  Math, SysUtils, Types, cxGeometry, dxNavBar;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarPainterAccess = class(TdxNavBarPainter);

procedure NavBarAccessibleObjectOwnerObjectDestroyed(
  var AAccessibleObject: IdxNavBarAccessibilityHelper);
begin
  if AAccessibleObject <> nil then
    AAccessibleObject.OwnerObjectDestroyed;
  AAccessibleObject := nil;
end;

function NavBarGetAccessibilityHelper(
  AObject: TObject): IdxNavBarAccessibilityHelper;
begin
  Supports(AObject, IdxNavBarAccessibilityHelper, Result);
end;

function NavBarGetFocusableAccessibleObjectAtPos(
  ARootAccessibleObject: IdxNavBarAccessibilityHelper;
  const P: TPoint): IdxNavBarAccessibilityHelper;

  function InternalGetFocusableAccessibleObjectAtPos(
    ARootAccessibleObject: TdxNavBarCustomAccessibilityHelper;
    const P: TPoint): IdxNavBarAccessibilityHelper;
  var
    AChildHelper, AHelper: TdxNavBarCustomAccessibilityHelper;
    I: Integer;
  begin
    Result := nil;
    AHelper := ARootAccessibleObject.GetNavBarHelper;
    if not AHelper.CanFocus(False) then
      Exit;
    if not AHelper.IsContainer then
      Result := ARootAccessibleObject
    else
      for I := 0 to AHelper.ChildCount - 1 do
      begin
        AChildHelper := AHelper.Children[I];
        if AChildHelper.Visible and
          PtInRect(AChildHelper.GetScreenBounds(cxAccessibleObjectSelfID), P) then
        begin
          Result := InternalGetFocusableAccessibleObjectAtPos(AChildHelper, P);
          Break;
        end;
      end;
  end;

begin
// Requires
  Assert(ARootAccessibleObject <> nil);
//
  if not PtInRect(ARootAccessibleObject.GetNavBarHelper.GetScreenBounds(
    cxAccessibleObjectSelfID), P) then
      Result := nil
  else
    Result := InternalGetFocusableAccessibleObjectAtPos(ARootAccessibleObject.GetNavBarHelper, P);
end;

procedure RemoveRedundantFocusableAccessibleObjects(
  AFocusableAccessibleObjectList: TList;
  ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  var ACurrentObjectScreenBounds: TRect;
  ADirection: TcxDirection); forward;
procedure ReplaceContainerWithItsFocusableChildren(
  AFocusableAccessibleObjectList: TList; AContainerIndex: Integer;
  ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  var ACurrentObjectScreenBounds: TRect); forward;

procedure ChooseOptimalFocusableAccessibleObject(
  AFocusableAccessibleObjectList: TList;
  const ACurrentObjectScreenBounds: TRect; ADirection: TcxDirection;
  ADistance: Integer);
var
  ABound, AMinBound, I: Integer;
  AObject, AOptimalObject: TdxNavBarCustomAccessibilityHelper;
  AObjectScreenBounds: TRect;
begin
  AMinBound := MaxInt;
  AOptimalObject := nil;
  for I := 0 to AFocusableAccessibleObjectList.Count - 1 do
  begin
    AObject := TdxNavBarCustomAccessibilityHelper(AFocusableAccessibleObjectList[I]);
    AObjectScreenBounds := AObject.GetScreenBounds(cxAccessibleObjectSelfID);
    OffsetRect(AObjectScreenBounds, 0, AObject.Tag);
    if ADistance = 0 then
      with ACurrentObjectScreenBounds do
        if ((ADirection in [dirUp, dirDown]) and (AObjectScreenBounds.Left = Left) and (AObjectScreenBounds.Right = Right)) or
          ((ADirection in [dirLeft, dirRight]) and (AObjectScreenBounds.Top = Top) and (AObjectScreenBounds.Bottom = Bottom)) then
        begin
          AOptimalObject := AObject;
          Break;
        end;
    ABound := Abs(IfThen(ADirection in [dirUp, dirDown], ACurrentObjectScreenBounds.Left - AObjectScreenBounds.Left,
      ACurrentObjectScreenBounds.Top - AObjectScreenBounds.Top));
    if ABound < AMinBound then
    begin
      AMinBound := ABound;
      AOptimalObject := AObject;
    end;
  end;
  AFocusableAccessibleObjectList.Clear;
  AFocusableAccessibleObjectList.Add(AOptimalObject);
end;

function GetNearestFocusableAccessibleObject(
  AFocusableAccessibleObjectList: TList;
  ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  var ACurrentObjectScreenBounds: TRect;
  ADirection: TcxDirection): TdxNavBarCustomAccessibilityHelper; overload;

  function GetDistance(
    const ACurrentObjectScreenBounds, AObjectScreenBounds: TRect;
    ADirection: TcxDirection): Integer;
  begin
    Result := 0;
    case ADirection of
      dirLeft:
        Result := ACurrentObjectScreenBounds.Left - AObjectScreenBounds.Right;
      dirRight:
        Result := AObjectScreenBounds.Left - ACurrentObjectScreenBounds.Right;
      dirUp:
        Result := ACurrentObjectScreenBounds.Top - AObjectScreenBounds.Bottom;
      dirDown:
        Result := AObjectScreenBounds.Top - ACurrentObjectScreenBounds.Bottom;
    end;
    if Result < 0 then
      Result := MaxInt;
  end;

var
  AMinDistance, I: Integer;
  AObject: TdxNavBarCustomAccessibilityHelper;
  AObjectScreenBounds: TRect;
begin
  AMinDistance := MaxInt - 1;
  for I := 0 to AFocusableAccessibleObjectList.Count - 1 do
  begin
    AObject := TdxNavBarCustomAccessibilityHelper(AFocusableAccessibleObjectList[I]);
    AObjectScreenBounds := AObject.GetScreenBounds(cxAccessibleObjectSelfID);
    OffsetRect(AObjectScreenBounds, 0, AObject.Tag);
    AMinDistance := Min(AMinDistance, GetDistance(ACurrentObjectScreenBounds,
      AObjectScreenBounds, ADirection));
  end;
  I := 0;
  while I < AFocusableAccessibleObjectList.Count do
  begin
    AObject := TdxNavBarCustomAccessibilityHelper(AFocusableAccessibleObjectList[I]);
    AObjectScreenBounds := AObject.GetScreenBounds(cxAccessibleObjectSelfID);
    OffsetRect(AObjectScreenBounds, 0, AObject.Tag);
    if GetDistance(ACurrentObjectScreenBounds, AObjectScreenBounds, ADirection) > AMinDistance then
      AFocusableAccessibleObjectList.Delete(I)
    else
      Inc(I);
  end;
  if AFocusableAccessibleObjectList.Count > 1 then
    ChooseOptimalFocusableAccessibleObject(AFocusableAccessibleObjectList,
      ACurrentObjectScreenBounds, ADirection, AMinDistance);
  if AFocusableAccessibleObjectList.Count = 0 then
    Result := nil
  else
    if TdxNavBarCustomAccessibilityHelper(AFocusableAccessibleObjectList[0]).IsContainer then
    begin
      ReplaceContainerWithItsFocusableChildren(AFocusableAccessibleObjectList,
        0, ACurrentObject, ACurrentObjectScreenBounds);
      Result := GetNearestFocusableAccessibleObject(
        AFocusableAccessibleObjectList, ACurrentObject,
        ACurrentObjectScreenBounds, ADirection);
    end
    else
      Result := TdxNavBarCustomAccessibilityHelper(AFocusableAccessibleObjectList[0]);
end;

function GetNearestFocusableAccessibleObject(
  ARootAccessibleObject, ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  ACurrentObjectScreenBounds: TRect;
  ADirection: TcxDirection): TdxNavBarCustomAccessibilityHelper; overload;
var
  AFocusableAccessibleObjectList: TList;
begin
  AFocusableAccessibleObjectList := TList.Create;
  try
    AFocusableAccessibleObjectList.Add(ARootAccessibleObject);
    ARootAccessibleObject.Tag := 0;
    RemoveRedundantFocusableAccessibleObjects(AFocusableAccessibleObjectList,
      ACurrentObject, ACurrentObjectScreenBounds, ADirection);
    Result := GetNearestFocusableAccessibleObject(AFocusableAccessibleObjectList,
      ACurrentObject, ACurrentObjectScreenBounds, ADirection);
  finally
    FreeAndNil(AFocusableAccessibleObjectList);
  end;
end;

procedure RemoveRedundantFocusableAccessibleObjects(
  AFocusableAccessibleObjectList: TList;
  ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  var ACurrentObjectScreenBounds: TRect; ADirection: TcxDirection);
var
  AObject: TdxNavBarCustomAccessibilityHelper;
  I: Integer;
begin
  if (ACurrentObject = nil) or (ACurrentObject.Parent = nil) then
    Exit;
  I := 0;
  while I < AFocusableAccessibleObjectList.Count do
  begin
    AObject := TdxNavBarCustomAccessibilityHelper(AFocusableAccessibleObjectList[I]);
    if AObject = ACurrentObject then
    begin
      AFocusableAccessibleObjectList.Delete(I);
      Break;
    end
    else
      if AObject.IsContainer and AObject.IsChild(ACurrentObject) then
        ReplaceContainerWithItsFocusableChildren(AFocusableAccessibleObjectList,
          I, ACurrentObject, ACurrentObjectScreenBounds)
      else
        Inc(I);
  end;
end;

procedure ReplaceContainerWithItsFocusableChildren(
  AFocusableAccessibleObjectList: TList; AContainerIndex: Integer;
  ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  var ACurrentObjectScreenBounds: TRect);
var
  AContainer: TdxNavBarCustomAccessibilityHelper;
  AContainerBottomBound, AContainerInvisiblePartHeight,
    AContainerChildMinTopScreenBound, AContainerChildTopBoundOffset, I, J: Integer;
  AContainerScreenBounds, AObjectScreenBounds: TRect;
  AObject: TdxNavBarCustomAccessibilityHelper;
begin
  AContainer := TdxNavBarCustomAccessibilityHelper(
    AFocusableAccessibleObjectList[AContainerIndex]);
  AContainerScreenBounds := AContainer.GetScreenBounds(cxAccessibleObjectSelfID);
  AContainerBottomBound := AContainerScreenBounds.Bottom + AContainer.Tag;
  AFocusableAccessibleObjectList.Delete(AContainerIndex);
  if (ACurrentObject <> nil) and AContainer.IsScrollable(AContainerInvisiblePartHeight,
    AContainerChildMinTopScreenBound) then
  begin
    for I := 0 to AFocusableAccessibleObjectList.Count - 1 do
    begin
      AObject := TdxNavBarCustomAccessibilityHelper(
        AFocusableAccessibleObjectList[I]);
      if AObject.GetScreenBounds(cxAccessibleObjectSelfID).Top + AObject.Tag >= AContainerBottomBound then
        AObject.Tag := AObject.Tag + AContainerInvisiblePartHeight;
    end;
    if (ACurrentObject <> nil) and not AContainer.IsChild(ACurrentObject) and
      (ACurrentObjectScreenBounds.Top >= AContainerBottomBound) then
        OffsetRect(ACurrentObjectScreenBounds, 0, AContainerInvisiblePartHeight);
    AContainerChildTopBoundOffset := AContainerScreenBounds.Top -
      AContainerChildMinTopScreenBound + AContainer.Tag;
  end
  else
    AContainerChildTopBoundOffset := AContainer.Tag;
  J := AContainerIndex;
  for I := 0 to AContainer.ChildCount - 1 do
  begin
    AObject := AContainer.Children[I];
    AObjectScreenBounds := AObject.GetScreenBounds(cxAccessibleObjectSelfID);
    if (ACurrentObject = nil) and ((AObjectScreenBounds.Top < AContainerScreenBounds.Top) or
      (AObjectScreenBounds.Top >= AContainerScreenBounds.Bottom)) then
        Continue;
    if AObject.CanFocus(ACurrentObject = nil) then
    begin
      AFocusableAccessibleObjectList.Insert(J, AObject);
      Inc(J);
      AObject.Tag := AContainerChildTopBoundOffset;
    end;
  end;
  if (ACurrentObject <> nil) and AContainer.IsChild(ACurrentObject) then
    OffsetRect(ACurrentObjectScreenBounds, 0, AContainerChildTopBoundOffset - AContainer.Tag);
end;

{ TdxNavBarNexusPersistent }

constructor TdxNavBarNexusPersistent.Create(AOwner: TPersistent);
begin
  inherited;
  FNotifyComponent := TcxFreeNotificator.Create(nil);
  FNotifyComponent.OnFreeNotification := FreeNotification;
end;

destructor TdxNavBarNexusPersistent.Destroy;
begin
  FreeAndNil(FNotifyComponent);
  inherited;
end;

procedure TdxNavBarNexusPersistent.FreeNotification(AComponent: TComponent);
begin
// do nothing
end;

{ TdxComponentCollectionItem }

procedure TdxNavBarComponentCollectionItem.InitiateActions;
begin
end;

procedure TdxNavBarComponentCollectionItem.SetIndex(Value: Integer);
begin
  if Collection = nil then
    Exit;
  if Value < 0 then
    Value := 0;
  if Value > Collection.Count - 1
    then Value := Collection.Count - 1;
  inherited SetIndex(Value);
end;

function TdxNavBarComponentCollectionItem.InternalGetCollection: TdxNavBarComponentCollection;
begin
  Result := TdxNavBarComponentCollection(inherited Collection);
end;

procedure TdxNavBarComponentCollectionItem.InternalSetCollection(Value: TdxNavBarComponentCollection);
begin
  inherited Collection := Value;
end;

{ TdxComponentCollection }

function TdxNavBarComponentCollection.Add: TdxNavBarComponentCollectionItem;
begin
  Result := TdxNavBarComponentCollectionItem(inherited Add);
end;

function TdxNavBarComponentCollection.ItemByName(const AName: string): TdxNavBarComponentCollectionItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := TdxNavBarComponentCollectionItem(Items[I]);
      Break;
    end;
end;

function TdxNavBarComponentCollection.GetItemPrefixName: string;
begin
  if csDesigning in Owner.ComponentState then
    Result := ''
  else
    Result := 'TdxNavBar';
end;

procedure TdxNavBarComponentCollection.InitiateActions;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxNavBarComponentCollectionItem(Items[I]).InitiateActions;
end;

function TdxNavBarComponentCollection.InternalGetOwner: TComponent;
begin
  Result := ParentComponent;
end;

{ TdxNavBarCustomAccessibilityHelper }

constructor TdxNavBarCustomAccessibilityHelper.Create(AOwnerObject: TObject;
  AOwnerObjectControl: TWinControl);
begin
// Requires
  Assert(AOwnerObjectControl is TdxCustomNavBar);
//
  inherited Create(AOwnerObject);
  FOwnerObjectControl := AOwnerObjectControl;
  FAttachedChildList := TInterfaceList.Create;
end;

destructor TdxNavBarCustomAccessibilityHelper.Destroy;
begin
  FreeAndNil(FAttachedChildList);
  inherited Destroy;
end;

// IcxAccessibilityHelper
procedure TdxNavBarCustomAccessibilityHelper.OwnerObjectDestroyed;
begin
  inherited OwnerObjectDestroyed;
  TdxCustomNavBarAccess(
    OwnerObjectControl).AccessibleObjectOwnerObjectDestroyedNotification(Self);
end;

function TdxNavBarCustomAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := (FAttachedChildList[AIndex] as IdxNavBarAccessibilityHelper).GetHelper;
end;

function TdxNavBarCustomAccessibilityHelper.GetChildCount: Integer;
begin
  Result := FAttachedChildList.Count;
end;

function TdxNavBarCustomAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  if OwnerObjectControl.HandleAllocated then
    Result := OwnerObjectControl.Handle
  else
    Result := 0;
end;

function TdxNavBarCustomAccessibilityHelper.GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
    Result := cxRectOffset(GetBounds, OwnerObjectControl.ClientOrigin)
  else
    Result := cxNullRect;
end;

function TdxNavBarCustomAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
var
  AHandle: HWND;
begin
  Result := cxSTATE_SYSTEM_NORMAL;
  AHandle := OwnerObjectWindow;
  if (AHandle = 0) or not IsWindowVisible(AHandle) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
  if not OwnerObjectControl.Enabled then
    Result := Result or cxSTATE_SYSTEM_UNAVAILABLE;
end;

// IdxNavBarAccessibilityHelper
procedure TdxNavBarCustomAccessibilityHelper.AttachChild(
  AChild: IdxNavBarAccessibilityHelper);
begin
// Requires
  Assert(IsContainer and (AChild <> nil));
//
  FAttachedChildList.Add(AChild);
end;

function TdxNavBarCustomAccessibilityHelper.CanFocus(
  AFocusingByDefault: Boolean): Boolean;

  function HasFocusableChild: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ChildCount - 1 do
    begin
      Result := Children[I].CanFocus(AFocusingByDefault);
      if Result then
        Break;
    end;
  end;

var
  AState: Integer;
begin
  Result := not (csDestroying in OwnerObjectControl.ComponentState);
  if not Result then
    Exit;
  AState := States[cxAccessibleObjectSelfID];
  Result := (AState and (cxSTATE_SYSTEM_INVISIBLE or cxSTATE_SYSTEM_UNAVAILABLE) = 0) and
    (not IsContainer and (not AFocusingByDefault or CanBeFocusedByDefault) or IsContainer and HasFocusableChild);
end;

procedure TdxNavBarCustomAccessibilityHelper.DetachChild(
  AChild: IdxNavBarAccessibilityHelper);
begin
// Requires
  Assert(IsContainer and (AChild <> nil));
//
  FAttachedChildList.Remove(AChild);
end;

procedure TdxNavBarCustomAccessibilityHelper.FocusedChanged(
  AMakeVisible: Boolean);
begin
  TdxCustomNavBar(OwnerObjectControl).InvalidateAll(doRecalc);
  if IsFocused then
  begin
    if AMakeVisible and NeedMakeVisible then
      MakeVisible;
  end
  else
    SetIsPressed(False);
end;

function TdxNavBarCustomAccessibilityHelper.GetNavBarHelper: TdxNavBarCustomAccessibilityHelper;
begin
  Result := Self;
end;

function TdxNavBarCustomAccessibilityHelper.GetNextObjectForNavigation(
  ADirection: TcxDirection): IdxNavBarAccessibilityHelper;
begin
// Requires
  Assert((Parent = nil) and (ADirection in [dirNone, dirDown]) or
    (Parent <> nil) and (ADirection <> dirNone) and not IsContainer);
  Result := InternalGetNextObjectForNavigation(ADirection);
end;

function TdxNavBarCustomAccessibilityHelper.IsFocused: Boolean;

  function GetFocusedAccessibleObject: TdxNavBarCustomAccessibilityHelper;
  begin
    if TdxCustomNavBarAccess(OwnerObjectControl).FocusedAccessibleObject <> nil then
      Result := TdxCustomNavBarAccess(OwnerObjectControl).FocusedAccessibleObject.GetNavBarHelper
    else
      Result := nil;
  end;

var
  AFocusedAccessibleObject: TdxNavBarCustomAccessibilityHelper;
begin
  AFocusedAccessibleObject := GetFocusedAccessibleObject;
  Result := OwnerObjectControl.Focused and (AFocusedAccessibleObject <> nil) and
    ((AFocusedAccessibleObject.GetHelper = Self) or (AFocusedAccessibleObject.AssociatedObject = Self));
end;

function TdxNavBarCustomAccessibilityHelper.IsPressed: Boolean;
begin
  Result := FIsPressed;
end;

procedure TdxNavBarCustomAccessibilityHelper.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  SetIsPressed(IsClickKey(Key));
end;

procedure TdxNavBarCustomAccessibilityHelper.KeyPress(var Key: Char);
begin
end;

procedure TdxNavBarCustomAccessibilityHelper.KeyUp(var Key: Word;
  Shift: TShiftState);
begin
  SetIsPressed(False);
  if IsClickKey(Key) then
    Click(Key);
end;

function TdxNavBarCustomAccessibilityHelper.NeedMakeVisible: Boolean;
var
  R: TRect;
begin
  R := cxGetWindowRect(TdxCustomNavBar(OwnerObjectControl));
  IntersectRect(R, R, GetClipBounds);
  Result := not cxRectContain(R, GetScreenBounds(cxAccessibleObjectSelfID));
end;

procedure TdxNavBarCustomAccessibilityHelper.RemoveFocus;
begin
  TdxCustomNavBarAccess(OwnerObjectControl).FocusedAccessibleObject := nil;
end;

function TdxNavBarCustomAccessibilityHelper.CanBeFocusedByDefault: Boolean;
begin
  Result := False;
end;

procedure TdxNavBarCustomAccessibilityHelper.Click;
begin
end;

function TdxNavBarCustomAccessibilityHelper.GetAssociatedObject: TdxNavBarCustomAccessibilityHelper;
begin
  Result := nil;
end;

function TdxNavBarCustomAccessibilityHelper.GetClipBounds: TRect;
begin
  Result := Parent.GetScreenBounds(cxAccessibleObjectSelfID);
end;

function TdxNavBarCustomAccessibilityHelper.InternalGetNextObjectForNavigation(
  ADirection: TcxDirection): IdxNavBarAccessibilityHelper;
var
  ACurrentObject: TdxNavBarCustomAccessibilityHelper;
  ACurrentObjectScreenBounds: TRect;
begin
  ACurrentObjectScreenBounds := GetScreenBounds(cxAccessibleObjectSelfID);
  if Parent = nil then
    OffsetRect(ACurrentObjectScreenBounds, 0, -cxRectHeight(ACurrentObjectScreenBounds));
  if ADirection <> dirNone then
    ACurrentObject := Self
  else
  begin
    ACurrentObject := nil;
    ADirection := dirDown;
  end;
  Result := GetNearestFocusableAccessibleObject(
    GetRootHelper as TdxNavBarCustomAccessibilityHelper, ACurrentObject,
    ACurrentObjectScreenBounds, ADirection);
end;

function TdxNavBarCustomAccessibilityHelper.IsChild(
  AChild: TdxNavBarCustomAccessibilityHelper): Boolean;
begin
// Requires
  Assert(AChild <> nil);
//
  repeat
    AChild := AChild.Parent;
    Result := AChild = Self;
  until Result or (AChild = nil) or (AChild.Parent = nil);
end;

function TdxNavBarCustomAccessibilityHelper.IsClickKey(AKey: Word): Boolean;
begin
  Result := False;
end;

function TdxNavBarCustomAccessibilityHelper.IsScrollable(
  out AInvisiblePartHeight, AChildMinTopScreenBound: Integer): Boolean;
begin
  Result := False;
end;

procedure TdxNavBarCustomAccessibilityHelper.MakeVisible;
begin
end;

procedure TdxNavBarCustomAccessibilityHelper.SetIsPressed(Value: Boolean);
begin
  if Value <> FIsPressed then
  begin
    FIsPressed := Value;
    TdxCustomNavBar(OwnerObjectControl).Invalidate;
  end;
end;

function TdxNavBarCustomAccessibilityHelper.InternalGetChild(
  AIndex: Integer): TdxNavBarCustomAccessibilityHelper;
begin
  Result := GetChild(AIndex) as TdxNavBarCustomAccessibilityHelper;
end;

function TdxNavBarCustomAccessibilityHelper.InternalGetParent: TdxNavBarCustomAccessibilityHelper;
begin
  Result := GetParent as TdxNavBarCustomAccessibilityHelper;
end;

end.

