{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridCustomPopupMenu;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Menus,
{$IFDEF DELPHI103}
  Generics.Collections,
{$ENDIF}
  dxCore, cxGridCustomView, cxClasses;

const
  cxhtGridBase = 1000;

type
  EcxGridPopupMenu = class(EdxException);

  TcxGridViewHitType = (gvhtGridNone, gvhtGridTab, gvhtNone, gvhtTab, gvhtCell,
    gvhtExpandButton, gvhtRecord, gvhtNavigator, gvhtPreview, gvhtColumnHeader,
    gvhtColumnHeaderFilterButton, gvhtFilter, gvhtFooter, gvhtFooterCell,
    gvhtGroupFooter, gvhtGroupFooterCell, gvhtGroupByBox, gvhtIndicator,
    gvhtIndicatorHeader, gvhtIndicatorBandHeader, gvhtRowIndicator, gvhtRowLevelIndent,
    gvhtBand, gvhtBandHeader, gvhtRowCaption, gvhtSeparator, gvhtGroupSummary, gvhtFindPanel);

  TcxGridViewHitTypes = set of TcxGridViewHitType;

  TcxGridBeforePopupProc = procedure(ASenderMenu: TComponent;
    AHitTest: TcxCustomGridHitTest; X,Y: Integer; var AllowPopup: Boolean) of object;

  TcxGridOnPopupProc = procedure(ASenderMenu: TComponent;
    AHitTest: TcxCustomGridHitTest; X,Y: Integer) of object;

  TcxGridPopupMenuItemClickProc = procedure(ASender: TObject;
    AHitTest: TcxCustomGridHitTest; var AHandler: TNotifyEvent;
    AParams: TList; var AHandled: Boolean) of object;

  IDoPopup = interface
    ['{41999EDE-B9D9-4808-9D01-61B09DF700FA}']
    function GetPopupHandler: TcxGridOnPopupProc;
  end;

  IcxGridPopupMenu = interface
    ['{302C3B32-D753-43A6-BAE2-F513EB4F9399}']
    function CanPopup(AHitTest: TcxCustomGridHitTest): Boolean;
  end;

  { TcxPopupMenuInfo }

  TcxPopupMenuInfo = Class(TCollectionItem)
  private
    FPopupMenu: TComponent;
    FPopupProc: TcxGridOnPopupProc;
    FHitTypes: TcxGridViewHitTypes;
    FLocked: Boolean;
    FView: TcxCustomGridView;
    FViewClass: TcxCustomGridViewClass;

    function FindCorrectComponent(const AComponent: TComponent): TComponent;
    procedure SetPopupMenu(const AValue: TComponent);
    procedure SetView(const AValue: TcxCustomGridView);
    procedure SetViewClass(const Value: TcxCustomGridViewClass);
  protected
    FIsBuiltIn: Boolean;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function CanPopup(AHitTest: TcxCustomGridHitTest): Boolean;
    procedure DoChanged(const APopupMenu: TComponent; const AView: TcxCustomGridView; AViewClass: TcxCustomGridViewClass); virtual;
    property GridViewClass: TcxCustomGridViewClass read FViewClass write SetViewClass;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property IsBuiltIn: Boolean read FIsBuiltIn;
  published
    property GridView: TcxCustomGridView read FView write SetView;
    property HitTypes: TcxGridViewHitTypes read FHitTypes write FHitTypes;
    property Index;
    property OnPopup: TcxGridOnPopupProc read FPopupProc write FPopupProc;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
  end;

  { TcxPopupMenuInfos }

  TcxPopupMenuInfos = class(TCollection)
  strict private
    FOwner: TComponent;

    function GetItem(AIndex: Integer): TcxPopupMenuInfo;
    procedure SetItem(AIndex: Integer; const AValue: TcxPopupMenuInfo);
  protected
    function GetOwner: TPersistent; override;
    function IsMenuInfoShortCut(AMenuInfo: TcxPopupMenuInfo; var Message: TWMKey): Boolean; virtual;
    function IsShortCut(var Message: TWMKey): Boolean; virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TComponent); virtual;
    function GetSameMenuCount(const AMenu: TComponent): Integer;
    property Items[AIndex: Integer]: TcxPopupMenuInfo read GetItem write SetItem; default;
  end;

  { TcxCustomGridPopupMenu }

  TcxCustomGridPopupMenu = class(TcxCustomComponent)
  strict private
    FPopupMenuInfos: TcxPopupMenuInfos;

    function GetMenuInfo(AIndex: Integer): TcxPopupMenuInfo;
    function GetMenuInfoCount: Integer;
    procedure SetPopupMenuInfos(const AValue: TcxPopupMenuInfos);
  protected
    procedure AddMenuInfo(const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
      ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass = nil;
      const AView: TcxCustomGridView = nil; AIsBuiltIn: Boolean = False);
    procedure AdjustMenuInfo(AMenuInfo: TcxPopupMenuInfo;
      const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
      ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass = nil;
      const AView: TcxCustomGridView = nil; AIsBuiltIn: Boolean = False);
    procedure ClearMenuInfosGridView(const AView: TcxCustomGridView = nil);
    function GetPopupMenuInfo(const AMenu: TComponent; const AView: TcxCustomGridView;
      ASearchDefault: Boolean = True): TcxPopupMenuInfo; overload; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property PopupMenuInfos: TcxPopupMenuInfos read FPopupMenuInfos write SetPopupMenuInfos;
    property MenuInfos[AIndex: Integer]: TcxPopupMenuInfo read GetMenuInfo; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckShortCuts(var Message: TWMKeyDown): Boolean; virtual;
    function FindPopupMenuInfo(const AView: TcxCustomGridView;
      AHitType: TcxGridViewHitType; AHitTest: TcxCustomGridHitTest): TcxPopupMenuInfo; virtual;
    function GetPopupMenuInfo(const AMenu: TComponent; AViewClass: TcxCustomGridViewClass;
      ASearchDefault: Boolean = True): TcxPopupMenuInfo; overload; virtual;
    property Count: Integer read GetMenuInfoCount;
  end;

  { TcxGridDefaultPopupMenu }

  TcxGridDefaultPopupMenu = class(TcxCustomGridPopupMenu)
  protected
    function GetPopupMenuInfo(const AMenu: TComponent; const AView: TcxCustomGridView;
      ASearchDefault: Boolean = True): TcxPopupMenuInfo; overload; override;
    procedure InternalRegisterPopupMenu(const AMenu: TComponent;
      AHitTypes: TcxGridViewHitTypes; ADoPopupProc: TcxGridOnPopupProc;
      AViewClass: TcxCustomGridViewClass; AIsBuiltIn: Boolean = False); virtual;
  public
    function GetPopupMenuInfo(const AMenu: TComponent; AViewClass: TcxCustomGridViewClass;
      ASearchDefault: Boolean = True): TcxPopupMenuInfo; overload; override;
    procedure RegisterPopupMenu(const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
      ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass); virtual;
    procedure UnRegisterPopupMenu(const AMenu: TComponent; AViewClass: TcxCustomGridViewClass;
      AHitTypes: TcxGridViewHitTypes = []); virtual;
    property MenuInfos;
  end;

  TcxPopupMenuClass = class of TComponent;

  TcxPopupMenuClassInfoRec = class
  public
    MenuClass: TcxPopupMenuClass;
    HitTypes: TcxGridViewHitTypes;
    ViewClass: TcxCustomGridViewClass;
    constructor Create(AMenuClass: TcxPopupMenuClass; AHitTypes: TcxGridViewHitTypes; AViewClass: TcxCustomGridViewClass);
  end;

  { TcxBuiltInPopupMenus }

  TcxBuiltInPopupMenus = class
  strict private
    FInfoRecs: TList;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): TcxPopupMenuClassInfoRec;
  protected
    function Add(AMenuClass: TcxPopupMenuClass; AHitTypes: TcxGridViewHitTypes; AViewClass: TcxCustomGridViewClass): TcxPopupMenuClassInfoRec;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure RegisterItemTo(ADefaultPopupMenu: TcxGridDefaultPopupMenu; AItemIndex: Integer);
    procedure Remove(APopupMenuClass: TcxPopupMenuClass);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TcxPopupMenuClassInfoRec read GetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterItemsTo(ADefaultPopupMenu: TcxGridDefaultPopupMenu);
  end;

var
  BuiltInPopupMenuList: TcxBuiltInPopupMenus;

function GetHitTypeByHitCode(AHitCode: Integer): TcxGridViewHitType;
procedure RegisterPopupMenuClass(APopupMenuClass: TcxPopupMenuClass; AHitTypes: TcxGridViewHitTypes; AViewClass: TcxCustomGridViewClass);
procedure UnregisterPopupMenuClass(APopupMenuClass: TcxPopupMenuClass);
implementation

uses
  cxGridCustomTableView, cxGridTableView, Graphics, cxGridDetailsSite,
  cxGridRows, cxGridBandedTableView, cxGridCardView, cxControls;

function GetHitTypeByHitCode(AHitCode: Integer): TcxGridViewHitType;
const
  cxGridViewHitCodes: array[TcxGridViewHitType] of Integer = (htNone + cxhtGridBase,
    htTab + cxhtGridBase, htNone, htTab, htCell, htExpandButton, htRecord, htNavigator,
    htPreview, htColumnHeader, htColumnHeaderFilterButton, htFilter, htFooter, htFooterCell,
    htGroupFooter, htGroupFooterCell, htGroupByBox, htIndicator, htIndicatorHeader,
    htIndicatorBandHeader, htRowIndicator, htRowLevelIndent, htBand, htBandHeader,
    htRowCaption, htSeparator, htGroupSummary, htFindPanel);
begin
  for Result := Low(Result) to High(Result) do
    if cxGridViewHitCodes[Result] = AHitCode then
      Exit;
  Result := TcxGridViewHitType(-1);
end;

{ TcxPopupMenuInfo }

procedure TcxPopupMenuInfo.BeginUpdate;
begin
  FLocked := True;
end;

procedure TcxPopupMenuInfo.EndUpdate;
begin
  FLocked := False;
end;

function TcxPopupMenuInfo.CanPopup(AHitTest: TcxCustomGridHitTest): Boolean;
var
  APopupMenuIntf: IcxGridPopupMenu;
begin
  if (PopupMenu <> nil) and Supports(PopupMenu, IcxGridPopupMenu, APopupMenuIntf) then
    Result := APopupMenuIntf.CanPopup(AHitTest)
  else
    Result := True;
end;

procedure TcxPopupMenuInfo.DoChanged(const APopupMenu: TComponent;
  const AView: TcxCustomGridView; AViewClass: TcxCustomGridViewClass);
var
  AMenuInfo: TcxPopupMenuInfo;
  MenuInfos: TcxPopupMenuInfos;
begin
  AMenuInfo := nil;
  if FLocked then exit;
  MenuInfos := TcxPopupMenuInfos(Collection);
  if Assigned(AView) then
    AMenuInfo :=
        TcxCustomGridPopupMenu(MenuInfos.GetOwner).GetPopupMenuInfo(APopupMenu, AView)
  else
  begin
    if Assigned(AViewClass) and (MenuInfos.GetOwner is TcxGridDefaultPopupMenu) then
    AMenuInfo :=
        TcxGridDefaultPopupMenu(MenuInfos.GetOwner).GetPopupMenuInfo(APopupMenu, AViewClass);
  end;
  if (AMenuInfo <> nil) and (MenuInfos.GetOwner <> nil) and
    not (csLoading in (MenuInfos.GetOwner as TComponent).ComponentState) then
    Raise EcxGridPopupMenu.Create(
      Format('This PopupMenuInfo is already registered. Index: %d',[AMenuInfo.Index]));
end;

function TcxPopupMenuInfo.FindCorrectComponent(const AComponent: TComponent): TComponent;
var
  AOwner: TComponent;
begin
  Result := AComponent;
  if not Assigned(AComponent) then
    Exit;
  AOwner := TComponent(TcxPopupMenuInfos(Collection).GetOwner);
  while AOwner.InheritsFrom(TcxCustomGridPopupMenu) do
    AOwner := TcxCustomGridPopupMenu(AOwner).Owner;
  with AOwner do
    if (csUpdating in ComponentState) then
    begin
      Result := FindComponent(AComponent.Name);
      if not Assigned(Result) or (Result.ClassType <> AComponent.ClassType) then
        Result := AComponent;
    end;
end;

procedure TcxPopupMenuInfo.SetPopupMenu(const AValue: TComponent);
begin
  if FPopupMenu <> AValue then
  begin
    if (TcxPopupMenuInfos(Collection).GetSameMenuCount(FPopupMenu) = 1) and
      (FPopupMenu <> nil) and not (csDestroying in FPopupMenu.ComponentState) then
      FPopupMenu.RemoveFreeNotification(TComponent(TcxPopupMenuInfos(Collection).GetOwner));
    DoChanged(AValue, FView, FViewClass);
    if FIsBuiltIn then
    begin
      FIsBuiltIn := False;
      FreeAndNil(FPopupMenu);
    end;
    FPopupMenu := AValue;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(TComponent(TcxPopupMenuInfos(Collection).GetOwner));
  end;
end;

procedure TcxPopupMenuInfo.SetView(const AValue: TcxCustomGridView);
begin
  if FView <> AValue then
  begin
    if Assigned(FView) and not (csDestroying in FView.ComponentState) then
      FView.RemoveFreeNotification(TComponent(TcxPopupMenuInfos(Collection).GetOwner));
    DoChanged(FPopupMenu, AValue, FViewClass);
    FView := AValue;
    if Assigned(FView) then
      FView.FreeNotification(TComponent(TcxPopupMenuInfos(Collection).GetOwner));
  end;
end;

procedure TcxPopupMenuInfo.SetViewClass(
  const Value: TcxCustomGridViewClass);
begin
  DoChanged(FPopupMenu, FView, Value);
  FViewClass := Value;
end;

destructor TcxPopupMenuInfo.Destroy;
begin
  if FIsBuiltIn then
    FreeAndNil(FPopupMenu);
  inherited Destroy;
end;

procedure TcxPopupMenuInfo.Assign(Source: TPersistent);
var
  AMenuInfo: TcxPopupMenuInfo;
begin
  if Source is TcxPopupMenuInfo then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      BeginUpdate;
      try
        AMenuInfo := TcxPopupMenuInfo(Source);
        Index := AMenuInfo.Index;
        PopupMenu := FindCorrectComponent(AMenuInfo.PopupMenu);
        OnPopup := AMenuInfo.OnPopup;
        HitTypes := AMenuInfo.HitTypes;
        GridView := TcxCustomGridView(FindCorrectComponent(AMenuInfo.GridView));
        GridViewClass := AMenuInfo.GridViewClass;
      finally
        EndUpdate;
      end;
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

{ TcxCustomGridPopupMenu }

constructor TcxCustomGridPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupMenuInfos := TcxPopupMenuInfos.Create(Self);
end;

destructor TcxCustomGridPopupMenu.Destroy;
begin
  FreeAndNil(FPopupMenuInfos);
  inherited Destroy;
end;

function TcxCustomGridPopupMenu.CheckShortCuts(var Message: TWMKeyDown): Boolean;
begin
  Result := not (csDesigning in ComponentState) and
    FPopupMenuInfos.IsShortCut(Message);
  Message.Result := Message.Result or LRESULT(Result);
end;

function TcxCustomGridPopupMenu.FindPopupMenuInfo(const AView: TcxCustomGridView;
  AHitType: TcxGridViewHitType; AHitTest: TcxCustomGridHitTest): TcxPopupMenuInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AHitType in MenuInfos[I].HitTypes then
      if ((AHitType in [gvhtGridNone, gvhtGridTab]) or (MenuInfos[I].GridView = AView)) and
        MenuInfos[I].CanPopup(AHitTest) then
      begin
        Result := MenuInfos[I];
        Break;
      end
      else
        if AView.InheritsFrom(MenuInfos[I].GridViewClass) and
          ((Result = nil) or MenuInfos[I].GridViewClass.InheritsFrom(Result.GridViewClass)) and
          MenuInfos[I].CanPopup(AHitTest) then
          Result := MenuInfos[I];
end;

function TcxCustomGridPopupMenu.GetPopupMenuInfo(const AMenu: TComponent;
  const AView: TcxCustomGridView; ASearchDefault: Boolean): TcxPopupMenuInfo;
begin
  Result := Nil;
end;

function TcxCustomGridPopupMenu.GetPopupMenuInfo(const AMenu: TComponent;
  AViewClass: TcxCustomGridViewClass; ASearchDefault: Boolean = True): TcxPopupMenuInfo;
begin
  Result := nil;
end;

procedure TcxCustomGridPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent.InheritsFrom(TcxCustomGridView) then
       ClearMenuInfosGridView(TcxCustomGridView(AComponent))
    else
      if Assigned(FPopupMenuInfos) then
      begin
        i := FPopupMenuInfos.Count - 1;
        while i >= 0 do
          with FPopupMenuInfos[i] do
          begin
            if PopupMenu = AComponent then
              FPopupMenu := nil;
            i := i - 1;
          end;
      end;
  end;
end;

procedure TcxCustomGridPopupMenu.AddMenuInfo(const AMenu: TComponent;
  AHitTypes: TcxGridViewHitTypes; ADoPopupProc: TcxGridOnPopupProc;
  AViewClass: TcxCustomGridViewClass; const AView: TcxCustomGridView;
  AIsBuiltIn: Boolean);
begin
  with TcxPopupMenuInfo(FPopupMenuInfos.Add) do
  begin
    BeginUpdate;
    try
      PopupMenu := AMenu;
      OnPopup := ADoPopupProc;
      GridView := AView;
      GridViewClass := AViewClass;
      HitTypes := AHitTypes;
      FIsBuiltIn := AIsBuiltIn;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomGridPopupMenu.AdjustMenuInfo(AMenuInfo: TcxPopupMenuInfo;
  const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
  ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass = nil;
  const AView: TcxCustomGridView = nil; AIsBuiltIn: Boolean = False);
begin
  if AMenuInfo <> nil then
    AMenuInfo.HitTypes := AMenuInfo.HitTypes + AHitTypes
  else
    AddMenuInfo(AMenu, AHitTypes, ADoPopupProc, AViewClass, AView, AIsBuiltIn);
end;

procedure TcxCustomGridPopupMenu.ClearMenuInfosGridView(
  const AView: TcxCustomGridView = nil);
var
  I: Integer;
begin
  for I := 0 to PopupMenuInfos.Count - 1 do
    with PopupMenuInfos[I] do
    begin
      if Assigned(AView) and (GridView <> AView) then
        Continue;
      BeginUpdate;
      try
        GridView := nil;
      finally
        EndUpdate;
      end;
    end;
end;

function TcxCustomGridPopupMenu.GetMenuInfo(AIndex: Integer): TcxPopupMenuInfo;
begin
  Result := FPopupMenuInfos[AIndex];
end;

function TcxCustomGridPopupMenu.GetMenuInfoCount: Integer;
begin
  Result := FPopupMenuInfos.Count;
end;

procedure TcxCustomGridPopupMenu.SetPopupMenuInfos(const AValue: TcxPopupMenuInfos);
begin
  FPopupMenuInfos := AValue;
end;

{ TcxGridDefaultPopupMenu }

function TcxGridDefaultPopupMenu.GetPopupMenuInfo(const AMenu: TComponent;
  AViewClass: TcxCustomGridViewClass; ASearchDefault: Boolean): TcxPopupMenuInfo;
var
  I : Integer;
  AClass: TcxCustomGridViewClass;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AMenu = MenuInfos[I].PopupMenu then
    begin
      AClass := MenuInfos[I].GridViewClass;
      if AViewClass = AClass then
        Exit(MenuInfos[I]);
      if AViewClass.InheritsFrom(AClass) and ASearchDefault then
      begin
        if (Result = nil) or AClass.InheritsFrom(Result.GridViewClass) then
          Result := MenuInfos[I];
      end;
    end;
end;

procedure TcxGridDefaultPopupMenu.RegisterPopupMenu(const AMenu: TComponent;
  AHitTypes: TcxGridViewHitTypes; ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass);
begin
  InternalRegisterPopupMenu(AMenu, AHitTypes, ADoPopupProc, AViewClass);
end;

procedure TcxGridDefaultPopupMenu.UnRegisterPopupMenu(const AMenu: TComponent;
  AViewClass: TcxCustomGridViewClass; AHitTypes: TcxGridViewHitTypes);
var
  AMenuInfo: TcxPopupMenuInfo;
begin
  AMenuInfo := GetPopupMenuInfo(AMenu, AViewClass, false);
  if AMenuInfo <> nil then
    If AMenuInfo.HitTypes = AHitTypes then
      AMenuInfo.Free
    else
      AMenuInfo.HitTypes := AMenuInfo.HitTypes - AHitTypes;
end;

function TcxGridDefaultPopupMenu.GetPopupMenuInfo(const AMenu: TComponent;
  const AView: TcxCustomGridView; ASearchDefault: Boolean): TcxPopupMenuInfo;
begin
  Result := GetPopupMenuInfo(AMenu,TcxCustomGridViewClass(AView.ClassType), ASearchDefault);
end;

procedure TcxGridDefaultPopupMenu.InternalRegisterPopupMenu(
  const AMenu: TComponent; AHitTypes: TcxGridViewHitTypes;
  ADoPopupProc: TcxGridOnPopupProc; AViewClass: TcxCustomGridViewClass;
  AIsBuiltIn: Boolean);
begin
  if Assigned(AViewClass) then
    AdjustMenuInfo(GetPopupMenuInfo(AMenu, AViewClass), AMenu, AHitTypes, ADoPopupProc, AViewClass, nil ,AIsBuiltIn);
end;

{ TcxPopupMenuInfos }

constructor TcxPopupMenuInfos.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TcxPopupMenuInfo);
end;

function TcxPopupMenuInfos.GetSameMenuCount(const AMenu: TComponent): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].PopupMenu = AMenu then
      Inc(Result);
end;

function TcxPopupMenuInfos.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TcxPopupMenuInfos.IsMenuInfoShortCut(AMenuInfo: TcxPopupMenuInfo; var Message: TWMKey): Boolean;
begin
  Result := IsPopupMenuShortCut(AMenuInfo.PopupMenu, Message);
end;

function TcxPopupMenuInfos.IsShortCut(var Message: TWMKey): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := IsMenuInfoShortCut(Items[I], Message);
    if Result then
      Break;
  end;
end;

procedure TcxPopupMenuInfos.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if Action = cnDeleting then
    TcxPopupMenuInfo(Item).PopupMenu := nil;
  inherited;
end;

function TcxPopupMenuInfos.GetItem(AIndex: Integer): TcxPopupMenuInfo;
begin
  Result := TcxPopupMenuInfo(inherited GetItem(AIndex));
end;

procedure TcxPopupMenuInfos.SetItem(AIndex: Integer; const AValue: TcxPopupMenuInfo);
begin
  inherited SetItem(AIndex , AValue);
end;

{ TcxPopupMenuClassInfoRec }

constructor TcxPopupMenuClassInfoRec.Create(AMenuClass: TcxPopupMenuClass;
  AHitTypes: TcxGridViewHitTypes; AViewClass: TcxCustomGridViewClass);
begin
  inherited Create;
  MenuClass := AMenuClass;
  HitTypes := AHitTypes;
  ViewClass := AViewClass;
end;

{ TcxBuiltInPopupMenus }

constructor TcxBuiltInPopupMenus.Create;
begin
  inherited;
  FInfoRecs := TList.Create;
end;

destructor TcxBuiltInPopupMenus.Destroy;
begin
  Clear;
  FreeAndNil(FInfoRecs);
  inherited;
end;

function TcxBuiltInPopupMenus.GetCount: Integer;
begin
  Result := FInfoRecs.Count;
end;

function TcxBuiltInPopupMenus.GetItem(AIndex: Integer): TcxPopupMenuClassInfoRec;
begin
  Result := TcxPopupMenuClassInfoRec(FInfoRecs[AIndex]);
end;

function TcxBuiltInPopupMenus.Add(AMenuClass: TcxPopupMenuClass;
  AHitTypes: TcxGridViewHitTypes; AViewClass: TcxCustomGridViewClass): TcxPopupMenuClassInfoRec;
begin
  Result := TcxPopupMenuClassInfoRec.Create(AMenuClass, AHitTypes, AViewClass);
  FInfoRecs.Add(Result);
end;

procedure TcxBuiltInPopupMenus.Clear;
begin
  while Count <> 0 do
    Delete(Count - 1);
end;

procedure TcxBuiltInPopupMenus.Delete(AIndex: Integer);
begin
  Items[AIndex].Free;
  FInfoRecs.Delete(AIndex);
end;

procedure TcxBuiltInPopupMenus.RegisterItemTo(ADefaultPopupMenu: TcxGridDefaultPopupMenu; AItemIndex: Integer);
var
  ADoPopupProc: TcxGridOnPopupProc;
  AIDoPopup: IDoPopup;
  AMenu: TComponent;
begin
  AMenu := Items[AItemIndex].MenuClass.Create(nil);
  if Supports(AMenu, IDoPopup, AIDoPopup) then
    ADoPopupProc := AIDoPopup.GetPopupHandler
  else
    ADoPopupProc := nil;

  ADefaultPopupMenu.InternalRegisterPopupMenu(AMenu, Items[AItemIndex].HitTypes, ADoPopupProc, Items[AItemIndex].ViewClass, True);
end;

procedure TcxBuiltInPopupMenus.Remove(APopupMenuClass: TcxPopupMenuClass);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].MenuClass = APopupMenuClass then
      Delete(I);
end;

procedure TcxBuiltInPopupMenus.RegisterItemsTo(ADefaultPopupMenu: TcxGridDefaultPopupMenu);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    RegisterItemTo(ADefaultPopupMenu, I);
end;

procedure RegisterPopupMenuClass(APopupMenuClass: TcxPopupMenuClass;
  AHitTypes: TcxGridViewHitTypes; AViewClass: TcxCustomGridViewClass);
begin
  BuiltInPopupMenuList.Add(APopupMenuClass, AHitTypes, AViewClass);
end;

procedure UnregisterPopupMenuClass(APopupMenuClass: TcxPopupMenuClass);
begin
  BuiltInPopupMenuList.Remove(APopupMenuClass);
end;

initialization
  BuiltInPopupMenuList := TcxBuiltInPopupMenus.Create;
  StartClassGroup(TControl);
  GroupDescendentsWith(TcxCustomGridPopupMenu, TControl);

finalization
  FreeAndNil(BuiltInPopupMenuList);
end.
