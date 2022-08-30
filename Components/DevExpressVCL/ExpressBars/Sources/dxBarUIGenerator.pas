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

unit dxBarUIGenerator;

{$I cxVer.inc}

interface

uses
  Windows, Classes, ActnList, Generics.Defaults, Generics.Collections, cxClasses, dxActions, dxUIGenerator, dxBar,
  cxBarEditItem, cxEdit, cxFontNameComboBox, cxDropDownEdit, cxGeometry, Forms;

type

  { TdxUIGeneratorBarItemInfo }

  TdxUIGeneratorBarItemInfoClass = class of TdxUIGeneratorBarItemInfo;
  TdxUIGeneratorBarItemInfo = class
  public
    class function GetBarItemClass: TdxBarItemClass; virtual;
    class function GetDefaultWidth: Integer; virtual;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); virtual;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; virtual;
    class function IsBuiltInItem(AItem: TdxBarItem; AParentLinks: TdxBarItemLinks): Boolean; virtual;
  end;

  { TdxUIGeneratorBarItemInfoList }

  TdxUIGeneratorBarItemInfoList = class(TList<TdxUIGeneratorBarItemInfoClass>)
  public
    function GetInfo(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): TdxUIGeneratorBarItemInfoClass;
  end;

  { TdxUIGeneratorBarButtonInfo }

  TdxUIGeneratorBarButtonInfo = class(TdxUIGeneratorBarItemInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
  end;

  { TdxUIGeneratorBarLargeButtonInfo }

  TdxUIGeneratorBarLargeButtonInfo = class(TdxUIGeneratorBarButtonInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
  end;

  { TdxUIGeneratorBarSubItemInfo }

  TdxUIGeneratorBarSubItemInfo = class(TdxUIGeneratorBarItemInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
    class function IsBuiltInItem(AItem: TdxBarItem; AParentLinks: TdxBarItemLinks): Boolean; override;
  end;

  { TdxUIGeneratorBarEditItemInfo }

  TdxUIGeneratorBarEditItemInfo = class(TdxUIGeneratorBarItemInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
    class function GetEditProperties: TcxCustomEditPropertiesClass; virtual;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
    class function IsBuiltInItem(AItem: TdxBarItem; AParentLinks: TdxBarItemLinks): Boolean; override;
  end;

  { TdxUIGeneratorBarColorComboItemInfo }

  TdxUIGeneratorBarColorComboItemInfo = class(TdxUIGeneratorBarEditItemInfo)
  public
    class function GetDefaultWidth: Integer; override;
    class function GetEditProperties: TcxCustomEditPropertiesClass; override;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
  end;

  { TdxUIGeneratorBarFontNameEditItemInfo }

  TdxUIGeneratorBarFontNameEditItemInfo = class(TdxUIGeneratorBarEditItemInfo)
  public
    class function GetDefaultWidth: Integer; override;
    class function GetEditProperties: TcxCustomEditPropertiesClass; override;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
  end;

  { TdxUIGeneratorBarFontSizeEditItemInfo }

  TdxUIGeneratorBarFontSizeEditItemInfo = class(TdxUIGeneratorBarEditItemInfo)
  public
    class function GetDefaultWidth: Integer; override;
    class function GetEditProperties: TcxCustomEditPropertiesClass; override;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
  end;

  { TdxUIGeneratorCustomBarAdapter }

  TdxUIGeneratorCustomBarAdapter = class(TdxUIGeneratorAdapter)
  strict private
    FHasDescendants: Boolean;

    function CheckHasDescendants: Boolean;
    function ConvertViewLevels(AViewLevels: TdxUIGeneratorItemViewLevels; ABarItem: TdxBarItem): TdxBarItemViewLevels;
    function GetBarItem(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo;
      AParentItemLinks: TdxBarItemLinks; ABarItemInfo: TdxUIGeneratorBarItemInfoClass): TdxBarItem;
  protected
    FBarItems: TdxUIGeneratorBarItemInfoList;

    function FindBarItem(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo;
      AParentItemLinks: TdxBarItemLinks; ABarItemInfo: TdxUIGeneratorBarItemInfoClass): TdxBarItem;
    function FindSubItem(const ACaption: string; AItemLinks: TdxBarItemLinks): TdxBarItem;
    function GetBarManager: TdxBarManager; virtual;
    function GetBarPopupMenuClass: TdxBarCustomPopupMenuClass; virtual;
    function GetCategoryIndex(ACommandInfo: TdxUIGeneratorCommandInfo): Integer; virtual;
    procedure Initialize; override;
    procedure InitializeBarItems; virtual;
    procedure PlaceCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo); overload; override;
    procedure PlaceCommand(AItemLinks: TdxBarItemLinks; AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo); reintroduce; overload;
  public
    constructor Create(ATarget: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    //
    class function GetComponentClass: TComponentClass; override;
    //
    property BarManager: TdxBarManager read GetBarManager;
  end;

  { TdxUIGeneratorBarAdapter }

  TdxUIGeneratorBarAdapter = class(TdxUIGeneratorCustomBarAdapter)
  protected
    procedure InitializeBarItems; override;
  end;

implementation

uses
  dxCore, SysUtils, cxGraphics, cxColorComboBox, dxDPIAwareUtils;

{ TdxUIGeneratorBarItemInfo }

class function TdxUIGeneratorBarItemInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := nil;
end;

class function TdxUIGeneratorBarItemInfo.GetDefaultWidth: Integer;
begin
  Result := 0;
end;

class procedure TdxUIGeneratorBarItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
begin
  // do nothing
end;

class function TdxUIGeneratorBarItemInfo.IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := AAction <> nil;
end;

class function TdxUIGeneratorBarItemInfo.IsBuiltInItem(AItem: TdxBarItem; AParentLinks: TdxBarItemLinks): Boolean;
begin
  Result := AItem.InheritsFrom(GetBarItemClass);
end;

{ TdxUIGeneratorBarItemInfoList }

function TdxUIGeneratorBarItemInfoList.GetInfo(
  AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): TdxUIGeneratorBarItemInfoClass;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].IsBuiltInCommand(AAction, ACommandInfo) then
      Exit(Items[I]);
  end;
  raise EdxException.Create('Internal Error');
end;

{ TdxUIGeneratorBarButtonInfo }

class function TdxUIGeneratorBarButtonInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TdxBarButton;
end;

{ TdxUIGeneratorBarLargeButtonInfo }

class function TdxUIGeneratorBarLargeButtonInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TdxBarLargeButton;
end;

class function TdxUIGeneratorBarLargeButtonInfo.IsBuiltInCommand(
  AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := inherited IsBuiltInCommand(AAction, ACommandInfo) and (ugivlLargeIcon in ACommandInfo.ViewLevels);
end;

{ TdxUIGeneratorBarSubItemInfo }

class function TdxUIGeneratorBarSubItemInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TdxBarSubItem;
end;

class function TdxUIGeneratorBarSubItemInfo.IsBuiltInCommand(AAction: TAction; ACommandInfo:
  TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := (AAction = nil) and (ACommandInfo.Commands.Count > 0);
end;

class function TdxUIGeneratorBarSubItemInfo.IsBuiltInItem(AItem: TdxBarItem; AParentLinks: TdxBarItemLinks): Boolean;
begin
  Result := inherited IsBuiltInItem(AItem, AParentLinks) and (AParentLinks.FindByItem(AItem) <> nil);
end;

{ TdxUIGeneratorBarEditItemInfo }

class function TdxUIGeneratorBarEditItemInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TcxBarEditItem;
end;

class function TdxUIGeneratorBarEditItemInfo.GetEditProperties: TcxCustomEditPropertiesClass;
begin
  Result := nil;
end;

class procedure TdxUIGeneratorBarEditItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
begin
  inherited;
  TcxBarEditItem(AItem).PropertiesClass := GetEditProperties;
  TcxBarEditItem(AItem).ShowCaption := False;
end;

class function TdxUIGeneratorBarEditItemInfo.IsBuiltInCommand(AAction: TAction;
  ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := (AAction <> nil) and (ACommandInfo.Commands.Count = 0) and Supports(AAction, IdxActionValue);
end;

class function TdxUIGeneratorBarEditItemInfo.IsBuiltInItem(AItem: TdxBarItem; AParentLinks: TdxBarItemLinks): Boolean;
begin
  Result := inherited IsBuiltInItem(AItem, AParentLinks) and (TcxBarEditItem(AItem).PropertiesClass = GetEditProperties);
end;

{ TdxUIGeneratorBarColorComboItemInfo }

class function TdxUIGeneratorBarColorComboItemInfo.GetDefaultWidth: Integer;
begin
  Result := 120;
end;

class function TdxUIGeneratorBarColorComboItemInfo.GetEditProperties: TcxCustomEditPropertiesClass;
begin
  Result := TcxColorComboBoxProperties;
end;

class procedure TdxUIGeneratorBarColorComboItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
begin
  inherited;
  TcxColorComboBoxProperties(TcxBarEditItem(AItem).Properties).AllowSelectColor := True;
  TcxColorComboBoxProperties(TcxBarEditItem(AItem).Properties).ColorDialogType := cxcdtAdvanced;
  TcxColorComboBoxProperties(TcxBarEditItem(AItem).Properties).ColorDialogShowFull := True;
  TcxBarEditItem(AItem).ShowCaption := True;
end;

class function TdxUIGeneratorBarColorComboItemInfo.IsBuiltInCommand(
  AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := inherited IsBuiltInCommand(AAction, ACommandInfo) and Supports(AAction, IdxActionColorValue);
end;

{ TdxUIGeneratorBarFontNameEditItemInfo }

class function TdxUIGeneratorBarFontNameEditItemInfo.GetDefaultWidth: Integer;
begin
  Result := 120;
end;

class function TdxUIGeneratorBarFontNameEditItemInfo.GetEditProperties: TcxCustomEditPropertiesClass;
begin
  Result := TcxFontNameComboBoxProperties;
end;

class procedure TdxUIGeneratorBarFontNameEditItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
begin
  inherited;
  TcxFontNameComboBoxProperties(TcxBarEditItem(AItem).Properties).FontPreview.ShowButtons := False;
end;

class function TdxUIGeneratorBarFontNameEditItemInfo.IsBuiltInCommand(
  AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := inherited IsBuiltInCommand(AAction, ACommandInfo) and Supports(AAction, IdxActionFontNameValue);
end;

{ TdxUIGeneratorBarFontSizeEditItemInfo }

class function TdxUIGeneratorBarFontSizeEditItemInfo.GetDefaultWidth: Integer;
begin
  Result := 40;
end;

class function TdxUIGeneratorBarFontSizeEditItemInfo.GetEditProperties: TcxCustomEditPropertiesClass;
begin
  Result := TcxComboBoxProperties;
end;

class procedure TdxUIGeneratorBarFontSizeEditItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
var
  AProperties: TcxComboBoxProperties;
  I: Integer;
begin
  inherited;
  AProperties := TcxComboBoxProperties(TcxBarEditItem(AItem).Properties);
  AProperties.DropDownRows := 12;
  AProperties.Items.BeginUpdate;
  try
    for I := 0 to dxDefaultFontSizeCount - 1 do
      AProperties.Items.Add(IntToStr(dxDefaultFontSizes[I]));
  finally
    AProperties.Items.EndUpdate;
  end;
end;

class function TdxUIGeneratorBarFontSizeEditItemInfo.IsBuiltInCommand(
  AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := inherited IsBuiltInCommand(AAction, ACommandInfo) and Supports(AAction, IdxActionFontSizeValue);
end;

{ TdxUIGeneratorCustomBarAdapter }

constructor TdxUIGeneratorCustomBarAdapter.Create(ATarget: TComponent);
begin
  inherited Create(ATarget);
  FHasDescendants := CheckHasDescendants;
  FBarItems := TdxUIGeneratorBarItemInfoList.Create;
  InitializeBarItems;
end;

destructor TdxUIGeneratorCustomBarAdapter.Destroy;
begin
  FreeAndNil(FBarItems);
  inherited Destroy;
end;

procedure TdxUIGeneratorCustomBarAdapter.BeginUpdate;
begin
  inherited BeginUpdate;
  if not FHasDescendants then
    BarDesignController.LockDesignerModified;
  BarManager.BeginUpdate;
end;

procedure TdxUIGeneratorCustomBarAdapter.EndUpdate;
begin
  BarManager.EndUpdate;
  if not FHasDescendants then
    BarDesignController.UnLockDesignerModified;
  inherited EndUpdate;
end;

class function TdxUIGeneratorCustomBarAdapter.GetComponentClass: TComponentClass;
begin
  Result := TdxBarManager;
end;

function TdxUIGeneratorCustomBarAdapter.FindBarItem(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo;
  AParentItemLinks: TdxBarItemLinks; ABarItemInfo: TdxUIGeneratorBarItemInfoClass): TdxBarItem;
var
  ABarItem: TdxBarItem;
  I: Integer;
begin
  for I := 0 to BarManager.ItemCount - 1 do
  begin
    ABarItem := BarManager.Items[I];
    if (AAction <> nil) and (ABarItem.Action = AAction) or (AAction = nil) and (ABarItem.Caption = ACommandInfo.Caption) then
    begin
      if ABarItemInfo.IsBuiltInItem(ABarItem, AParentItemLinks) then
        Exit(ABarItem);
    end;
  end;
  Result := nil;
end;

function TdxUIGeneratorCustomBarAdapter.FindSubItem(const ACaption: string; AItemLinks: TdxBarItemLinks): TdxBarItem;
var
  AItemLink: TdxBarItemLink;
  I: Integer;
begin
  Result := nil;
  for I := 0 to AItemLinks.Count - 1 do
  begin
    AItemLink := AItemLinks[I];
    if (AItemLink.Caption = ACaption) and (AItemLink.Item is TdxBarSubItem) then
      Exit(TdxBarSubItem(AItemLink.Item));
  end;
end;

function TdxUIGeneratorCustomBarAdapter.GetBarManager: TdxBarManager;
begin
  Result := TdxBarManager(inherited Target);
end;

function TdxUIGeneratorCustomBarAdapter.GetBarPopupMenuClass: TdxBarCustomPopupMenuClass;
begin
  Result := TdxBarPopupMenu;
end;

function TdxUIGeneratorCustomBarAdapter.GetCategoryIndex(ACommandInfo: TdxUIGeneratorCommandInfo): Integer;
var
  ACategory: string;
begin
  ACategory := Format('%s | %s', [ACommandInfo.Category.TabName, ACommandInfo.Category.BarName]);
  Result := BarManager.Categories.IndexOf(ACategory);
  if Result < 0 then
    Result := BarManager.Categories.Add(ACategory);
end;

procedure TdxUIGeneratorCustomBarAdapter.Initialize;
begin
  inherited Initialize;

  if BarManager.ImageOptions.Images = nil then
    BarManager.ImageOptions.Images := ActionList.Images;
  if BarManager.ImageOptions.Images = nil then
    BarManager.ImageOptions.Images := TdxUIGeneratorHelper.CreateImageList(OwnerForm, 16);
  if BarManager.ImageOptions.LargeImages = nil then
    BarManager.ImageOptions.LargeImages := TdxUIGeneratorHelper.CreateImageList(OwnerForm, 32);
  if ActionList.Images = nil then
    ActionList.Images := BarManager.ImageOptions.Images;

  LargeImageList := BarManager.ImageOptions.LargeImages;
  SmallImageList := BarManager.ImageOptions.Images;
end;

procedure TdxUIGeneratorCustomBarAdapter.InitializeBarItems;
begin
  FBarItems.Add(TdxUIGeneratorBarSubItemInfo);
  FBarItems.Add(TdxUIGeneratorBarButtonInfo);
  FBarItems.Add(TdxUIGeneratorBarLargeButtonInfo);
  FBarItems.Add(TdxUIGeneratorBarFontNameEditItemInfo);
  FBarItems.Add(TdxUIGeneratorBarFontSizeEditItemInfo);
  FBarItems.Add(TdxUIGeneratorBarColorComboItemInfo);
end;

procedure TdxUIGeneratorCustomBarAdapter.PlaceCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo);

  function GetToolbar(ACategory: TdxUIGeneratorCategoryInfo): TdxBar;
  begin
    Result := BarManager.BarByCaption(ACategory.BarName);
    if Result = nil then
    begin
      Result := BarManager.AddToolBar;
      Result.Caption := ACategory.BarName;
      Result.Name := TdxUIGeneratorHelper.GenerateName(Result);
      if ACategory.RowIndex >= 0 then
        Result.Row := ACategory.RowIndex;
      TdxUIGeneratorHelper.LoadImage(Result.Glyph, ACategory.BarGlyphFileName, ImageSet);
    end;
  end;

begin
  PlaceCommand(GetToolbar(ACommandInfo.Category).ItemLinks, AAction, ACommandInfo);
end;

procedure TdxUIGeneratorCustomBarAdapter.PlaceCommand(
  AItemLinks: TdxBarItemLinks; AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo);
const
  ButtonStyleMap: array[Boolean] of TdxBarButtonStyle = (bsDropDown, bsCheckedDropDown);
var
  ABarItem: TdxBarItem;
  ABarItemInfo: TdxUIGeneratorBarItemInfoClass;
  AItemLink: TdxBarItemLink;
  I: Integer;
begin

  BarDesignController.LockDesignerModified;
  try
    ABarItemInfo := FBarItems.GetInfo(AAction, ACommandInfo);
    ABarItem := GetBarItem(AAction, ACommandInfo, AItemLinks, ABarItemInfo);
    if AItemLinks.FindByItem(ABarItem) = nil then
    begin
      AItemLinks.BeginUpdate;
      try
        AItemLink := AItemLinks.Add;
        AItemLink.Item := ABarItem;
        AItemLink.BeginGroup := ACommandInfo.BeginGroup;
        AItemLink.ButtonGroup := TdxBarButtonGroupPosition(ACommandInfo.GroupPosition);
        AItemLink.Position := TdxBarItemPosition(ACommandInfo.Position);
        AItemLink.ViewLevels := ConvertViewLevels(ACommandInfo.ViewLevels, ABarItem);
        if ABarItemInfo.GetDefaultWidth > 0 then
          AItemLink.UserWidth := dxGetScaleFactor(AItemLinks.Owner).Apply(ABarItemInfo.GetDefaultWidth);
      finally
        AItemLinks.EndUpdate;
      end;
    end;

    if ACommandInfo.Commands.Count > 0 then
    begin
      if ABarItem is TdxBarCustomButton then
      begin
        if TdxBarCustomButton(ABarItem).DropDownMenu = nil then
        begin
          TdxBarCustomButton(ABarItem).DropDownMenu := TdxBarCustomPopupMenu(
            TdxUIGeneratorHelper.CreateComponent(OwnerForm, GetBarPopupMenuClass));
          TdxBarCustomButton(ABarItem).ButtonStyle := ButtonStyleMap[TdxBarCustomButton(ABarItem).ButtonStyle = bsChecked];
        end;
        AItemLinks := TdxBarCustomButton(ABarItem).DropDownMenu.ItemLinks;
      end
      else
        AItemLinks := (ABarItem as TCustomdxBarSubItem).ItemLinks;

      AItemLinks.BeginUpdate;
      try
        for I := 0 to ACommandInfo.Commands.Count - 1 do
          PlaceCommand(AItemLinks, GetAction(ACommandInfo.Commands[I]), ACommandInfo.Commands[I]);
      finally
        AItemLinks.EndUpdate;
      end;
    end;
  finally
    BarDesignController.UnLockDesignerModified;
  end;
end;

function TdxUIGeneratorCustomBarAdapter.CheckHasDescendants: Boolean;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    if [csAncestor, csDesigning] * Screen.Forms[I].ComponentState = [csAncestor, csDesigning] then
      Exit(True);
  end;
  Result := False;
end;

function TdxUIGeneratorCustomBarAdapter.ConvertViewLevels(
  AViewLevels: TdxUIGeneratorItemViewLevels; ABarItem: TdxBarItem): TdxBarItemViewLevels;
begin
  Result := dxBarItemAllViewLevels;
  if [ugivlLargeIcon] * AViewLevels = [] then
    Result := Result - [ivlLargeControlOnly, ivlLargeIconWithText];
  if [ugivlSmallIcon] * AViewLevels = [] then
    Result := Result - [ivlSmallIcon, ivlSmallIconWithText];
  if [ugivlText] * AViewLevels = [] then
  begin
    Result := Result - [ivlLargeIconWithText, ivlSmallIconWithText];
    // ToDo: ABarItemLink.Control.PossibleViewLevels
    if (ugivlSmallIcon in AViewLevels) and (ABarItem.ImageIndex < 0) and not (ABarItem is TdxCustomBarEdit) then
      Result := Result + [ivlSmallIconWithText];
  end;
end;

function TdxUIGeneratorCustomBarAdapter.GetBarItem(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo;
  AParentItemLinks: TdxBarItemLinks; ABarItemInfo: TdxUIGeneratorBarItemInfoClass): TdxBarItem;
var
  ALargeImageIndex: Integer;
  ASmallImageIndex: Integer;
begin
  Result := FindBarItem(AAction, ACommandInfo, AParentItemLinks, ABarItemInfo);
  if Result = nil then
  begin
    Result := ABarItemInfo.GetBarItemClass.Create(BarManager.Owner);
    Result.Category := GetCategoryIndex(ACommandInfo);
    Result.Action := AAction;
    Result.Name := TdxUIGeneratorHelper.GenerateName(Result);
    ABarItemInfo.Initialize(Result, ACommandInfo, Self);

    AddImage(AAction, ACommandInfo.ImageFileName, ASmallImageIndex, ALargeImageIndex);
    Result.LargeImageIndex := ALargeImageIndex;
    Result.ImageIndex := ASmallImageIndex;

    if AAction <> nil then
    begin
      if AAction.AutoCheck and (Result is TdxBarCustomButton) then
        TdxBarCustomButton(Result).ButtonStyle := bsChecked;
    end
    else
      Result.Caption := ACommandInfo.Caption;
  end;
end;

{ TdxUIGeneratorBarAdapter }

procedure TdxUIGeneratorBarAdapter.InitializeBarItems;
begin
  inherited InitializeBarItems;
  FBarItems.Remove(TdxUIGeneratorBarLargeButtonInfo);
end;

initialization
  TdxUIGenerator.RegisterAdapter(TdxUIGeneratorBarAdapter);

finalization
  TdxUIGenerator.UnregisterAdapter(TdxUIGeneratorBarAdapter);
end.
