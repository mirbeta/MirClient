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

unit dxRibbonUIGenerator;

{$I cxVer.inc}

interface

uses
  Classes, ActnList, Controls, dxBar, dxRibbon, dxBarUIGenerator, dxUIGenerator, dxRibbonColorGallery, dxRibbonGallery;

type

  { TdxUIGeneratorGalleryItemInfo }

  TdxUIGeneratorGalleryItemInfo = class(TdxUIGeneratorBarItemInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); override;
    class procedure InitializeGallery(AGallery: TdxRibbonGalleryItem;
      ACommandInfo: TdxUIGeneratorGalleryInfo; AAdapter: TdxUIGeneratorAdapter);
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
  end;

  { TdxUIGeneratorRibbonColorGalleryItemInfo }

  TdxUIGeneratorRibbonColorGalleryItemInfo = class(TdxUIGeneratorBarItemInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
    class function IsBuiltInCommand(AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean; override;
  end;

  { TdxUIGeneratorRibbonGalleryItemInfo }

  TdxUIGeneratorRibbonGalleryItemInfo = class(TdxUIGeneratorGalleryItemInfo)
  public
    class function GetBarItemClass: TdxBarItemClass; override;
    class procedure Initialize(AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter); override;
  end;

  { TdxUIGeneratorAdvancedBarAdapter }

  TdxUIGeneratorAdvancedBarAdapter = class(TdxUIGeneratorBarAdapter)
  protected
    procedure InitializeBarItems; override;
  end;

  { TdxUIGeneratorRibbonAdapter }

  TdxUIGeneratorRibbonAdapter = class(TdxUIGeneratorCustomBarAdapter)
  strict private
    function GetTarget: TdxRibbon;
  protected
    procedure Initialize; override;
    procedure InitializeBarItems; override;
    function GetBarManager: TdxBarManager; override;
    function GetBarPopupMenuClass: TdxBarCustomPopupMenuClass; override;
    procedure PlaceCommand(AAction: TAction; AItem: TdxUIGeneratorCommandInfo); override;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    //
    class function CreateComponent(AOwner: TComponent): TComponent; override;
    class function GetComponentClass: TComponentClass; override;
    //
    property Target: TdxRibbon read GetTarget;
  end;

implementation

uses
  dxCore, SysUtils, Graphics, ImgList, dxActions, dxRibbonSkins, cxGraphics;

const
  sdxBarManagerNotAssigned = 'BarManager is not assigned to the Ribbon';

type
  TdxRibbonGalleryGroupItemAccess = class(TdxRibbonGalleryGroupItem);
  TdxUIGeneratorAdapterAccess = class(TdxUIGeneratorAdapter);
  TdxCustomRibbonGalleryOptionsAccess = class(TdxCustomRibbonGalleryOptions);

{ TdxUIGeneratorGalleryItemInfo }

class function TdxUIGeneratorGalleryItemInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TdxBarSubItem;
end;

class procedure TdxUIGeneratorGalleryItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
var
  AGalleryItem: TdxRibbonGalleryItem;
  ASubItem: TdxBarSubItem;
begin
  AGalleryItem := TdxRibbonGalleryItem.Create(AItem.BarManager.Owner);
  AGalleryItem.Name := TdxUIGeneratorHelper.GenerateName(AGalleryItem);
  AGalleryItem.Caption := AItem.Caption;
  AGalleryItem.Category := AItem.Category;

  ASubItem := AItem as TdxBarSubItem;
  ASubItem.ItemLinks.Add.Item := AGalleryItem;

  InitializeGallery(AGalleryItem, ACommandInfo as TdxUIGeneratorGalleryInfo, AAdapter);
end;

class procedure TdxUIGeneratorGalleryItemInfo.InitializeGallery(
  AGallery: TdxRibbonGalleryItem; ACommandInfo: TdxUIGeneratorGalleryInfo; AAdapter: TdxUIGeneratorAdapter);

  procedure InitializeGalleryOptions(AGalleryInfo: TdxUIGeneratorGalleryInfo; AGallery: TdxRibbonGalleryItem);
  begin
    AGallery.GalleryInMenuOptions.CollapsedInSubmenu := AGalleryInfo.Collapsed;
    AGallery.GalleryInRibbonOptions.Collapsed := AGalleryInfo.Collapsed;
    if AGalleryInfo.Resizable then
      AGallery.GalleryInMenuOptions.DropDownGalleryResizing := gsrHeight
    else
      AGallery.GalleryInMenuOptions.DropDownGalleryResizing := gsrNone;
    AGallery.GalleryOptions.ColumnCount := AGalleryInfo.ColumnCount;
    if not AGalleryInfo.ShowItemCaption then
      AGallery.GalleryInMenuOptions.ItemTextKind := itkNone
    else
      if AGalleryInfo.ShowItemDescription then
        AGallery.GalleryInMenuOptions.ItemTextKind := itkCaptionAndDescription
      else
        AGallery.GalleryInMenuOptions.ItemTextKind := itkCaption;
    TdxCustomRibbonGalleryOptionsAccess(AGallery.GalleryOptions).LongDescriptionDefaultRowCount :=
      AGalleryInfo.ItemDescriptionRowCount;
    AGallery.GalleryInMenuOptions.ItemTextAlignVert := AGalleryInfo.ItemTextAlignVert;
  end;

  procedure InitializeGalleryGroup(AGalleryGroupItemInfo: TdxUIGeneratorGalleryGroupItemInfo;
    AGalleryInfo: TdxUIGeneratorGalleryInfo; AGallery: TdxRibbonGalleryItem; out AGroup: TdxRibbonGalleryGroup);
  var
    I: Integer;
  begin
    AGroup := nil;
    for I := 0 to AGallery.GalleryCategories.Count - 1 do
      if AGallery.GalleryCategories.Groups[I].Caption = AGalleryGroupItemInfo.GroupCaption then
      begin
        AGroup := AGallery.GalleryCategories.Items[I];
        Break;
      end;

    if AGroup = nil then
      AGroup := AGallery.GalleryCategories.Add;
    AGroup.Header.Caption := AGalleryGroupItemInfo.GroupCaption;
    AGroup.Header.Visible := AGroup.Header.Caption <> '';
    if AGalleryInfo.ExternalImageList = nil then
    begin
      if AGalleryInfo.UseLargeImages then
        AGroup.Options.Images := AAdapter.LargeImageList
      else
        AGroup.Options.Images := AAdapter.SmallImageList;
    end;
  end;

  procedure InitializeGalleryGroupItem(AItemInfo: TdxUIGeneratorGalleryGroupItemInfo; AGroup: TdxRibbonGalleryGroup);

    procedure LoadGlyphFromExternalImageList(AItem: TdxRibbonGalleryGroupItem; AExternalImageList: TCustomImageList;
      AExternalImageIndex: Integer);
    var
      ABitmap: TBitmap;
    begin
      if AExternalImageList is TcxImageList then
        TcxImageList(AExternalImageList).GetImage(AExternalImageIndex, AItem.Glyph)
      else
      begin
        ABitmap := TBitmap.Create;
        try
          AExternalImageList.GetBitmap(AExternalImageIndex, ABitmap);
          AItem.Glyph.SetBitmap(ABitmap);
        finally
          FreeAndNil(ABitmap);
        end;
      end;
    end;

  var
    AItem: TdxRibbonGalleryGroupItem;
    I: Integer;
  begin
    AItem := nil;
    for I := 0 to AGroup.Items.Count - 1 do
      if TdxRibbonGalleryGroupItemAccess(AGroup.Items[I]).FActionIndex = AItemInfo.ActionIndex then
      begin
        AItem := AGroup.Items[I];
        Break;
      end;

    if AItem = nil then
      AItem := AGroup.Items.Add;
    AItem.Caption := AItemInfo.Caption;
    AItem.Description := AItemInfo.Description;
    if ACommandInfo.ExternalImageList = nil then
      AItem.ImageIndex := TdxUIGeneratorAdapterAccess(AAdapter).AddImage(AGroup.Options.Images, AItemInfo.ImageFileName)
    else
      LoadGlyphFromExternalImageList(AItem, ACommandInfo.ExternalImageList, AItemInfo.ExternalImageIndex);
    TdxRibbonGalleryGroupItemAccess(AItem).FActionIndex := AItemInfo.ActionIndex;
  end;

var
  AGalleryGroup: TdxRibbonGalleryGroup;
  AIntf: IdxActionGalleryClient;
  I: Integer;
begin
  ACommandInfo.GalleryGroups := AGallery.GalleryCategories;
  if Supports(AGallery.Action, IdxActionGalleryClient, AIntf) then
    AIntf.PopulateGalleryInfo(ACommandInfo);
  InitializeGalleryOptions(ACommandInfo, AGallery);
  for I := 0 to ACommandInfo.GroupItems.Count - 1 do
  begin
    InitializeGalleryGroup(ACommandInfo.GroupItems[I], ACommandInfo, AGallery, AGalleryGroup);
    InitializeGalleryGroupItem(ACommandInfo.GroupItems[I], AGalleryGroup);
  end;
end;

class function TdxUIGeneratorGalleryItemInfo.IsBuiltInCommand(AAction: TAction;
  ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := inherited IsBuiltInCommand(AAction, ACommandInfo) and
    Supports(AAction, IdxActionGalleryClient) and (ACommandInfo is TdxUIGeneratorGalleryInfo);
end;

{ TdxUIGeneratorRibbonColorGalleryItemInfo }

class function TdxUIGeneratorRibbonColorGalleryItemInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TdxRibbonColorGalleryItem;
end;

class function TdxUIGeneratorRibbonColorGalleryItemInfo.IsBuiltInCommand(
  AAction: TAction; ACommandInfo: TdxUIGeneratorCommandInfo): Boolean;
begin
  Result := (AAction <> nil) and (ACommandInfo.Commands.Count = 0) and Supports(AAction, IdxActionColorValue);
end;

{ TdxUIGeneratorRibbonGalleryItemInfo }

class function TdxUIGeneratorRibbonGalleryItemInfo.GetBarItemClass: TdxBarItemClass;
begin
  Result := TdxRibbonGalleryItem;
end;

class procedure TdxUIGeneratorRibbonGalleryItemInfo.Initialize(
  AItem: TdxBarItem; ACommandInfo: TdxUIGeneratorCommandInfo; AAdapter: TdxUIGeneratorAdapter);
begin
  InitializeGallery(AItem as TdxRibbonGalleryItem, ACommandInfo as TdxUIGeneratorGalleryInfo, AAdapter);
end;

{ TdxUIGeneratorRibbonAdapter }

procedure TdxUIGeneratorRibbonAdapter.BeginUpdate;
begin
  inherited BeginUpdate;
  Target.BeginUpdate;
end;

procedure TdxUIGeneratorRibbonAdapter.EndUpdate;
begin
  inherited EndUpdate;
  Target.EndUpdate;
end;

class function TdxUIGeneratorRibbonAdapter.CreateComponent(AOwner: TComponent): TComponent;
begin
  Result := inherited CreateComponent(AOwner);
  TdxRibbon(Result).Style := rs2019;
  TdxRibbon(Result).ColorSchemeAccent := rcsaBlue;
  TdxRibbon(Result).Tabs.Clear;
  TdxRibbon(Result).Parent := AOwner as TWinControl;
end;

class function TdxUIGeneratorRibbonAdapter.GetComponentClass: TComponentClass;
begin
  Result := TdxRibbon;
end;

procedure TdxUIGeneratorRibbonAdapter.Initialize;
begin
  if Target.BarManager = nil then
    Target.BarManager := GetBarManagerByComponent(Target.Owner);
  if Target.BarManager = nil then
  begin
    Target.BarManager := TdxBarManager.Create(Target.Owner);
    Target.BarManager.Name := TdxUIGeneratorHelper.GenerateName(Target.BarManager);
  end;
  inherited Initialize;
end;

procedure TdxUIGeneratorRibbonAdapter.InitializeBarItems;
begin
  inherited InitializeBarItems;
  FBarItems.Add(TdxUIGeneratorRibbonGalleryItemInfo);
  FBarItems.Add(TdxUIGeneratorRibbonColorGalleryItemInfo);
end;

function TdxUIGeneratorRibbonAdapter.GetBarManager: TdxBarManager;
begin
  Result := Target.BarManager;
  if Result = nil then
    raise EdxException.Create(sdxBarManagerNotAssigned);
end;

function TdxUIGeneratorRibbonAdapter.GetBarPopupMenuClass: TdxBarCustomPopupMenuClass;
begin
  Result := TdxRibbonPopupMenu;
end;

procedure TdxUIGeneratorRibbonAdapter.PlaceCommand(AAction: TAction; AItem: TdxUIGeneratorCommandInfo);
var
  AGroup: TdxRibbonTabGroup;
  ATab: TdxRibbonTab;
begin
  ATab := Target.Tabs.Find(AItem.Category.TabName);
  if ATab = nil then
  begin
    ATab := Target.Tabs.Add;
    ATab.Caption := AItem.Category.TabName;
    ATab.Name := TdxUIGeneratorHelper.GenerateName(ATab);
  end;

  if not ATab.Groups.Find(AItem.Category.BarName, AGroup) then
  begin
    AGroup := ATab.Groups.Add;
    AGroup.ToolBar := BarManager.AddToolBar;
    AGroup.ToolBar.Caption := AItem.Category.BarName;
    AGroup.ToolBar.Name := TdxUIGeneratorHelper.GenerateName(AGroup.ToolBar);
    TdxUIGeneratorHelper.LoadImage(AGroup.ToolBar.Glyph, AItem.Category.BarGlyphFileName, ImageSet);
    AGroup.Caption := AItem.Category.BarName;
  end;

  PlaceCommand(AGroup.ToolBar.ItemLinks, AAction, AItem);
end;

function TdxUIGeneratorRibbonAdapter.GetTarget: TdxRibbon;
begin
  Result := TdxRibbon(inherited Target);
end;

{ TdxUIGeneratorAdvancedBarAdapter }

procedure TdxUIGeneratorAdvancedBarAdapter.InitializeBarItems;
begin
  inherited;
  FBarItems.Add(TdxUIGeneratorGalleryItemInfo);
end;

initialization
  TdxUIGenerator.UnregisterAdapter(TdxUIGeneratorBarAdapter);
  TdxUIGenerator.RegisterAdapter(TdxUIGeneratorAdvancedBarAdapter);
  TdxUIGenerator.RegisterAdapter(TdxUIGeneratorRibbonAdapter);

finalization
  TdxUIGenerator.UnregisterAdapter(TdxUIGeneratorRibbonAdapter);
  TdxUIGenerator.UnregisterAdapter(TdxUIGeneratorAdvancedBarAdapter);
end.
