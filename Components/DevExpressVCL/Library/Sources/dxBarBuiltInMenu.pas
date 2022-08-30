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

unit dxBarBuiltInMenu;

{$I cxVer.inc}

interface

uses
  Windows, Menus, Classes, Graphics, ImgList, Contnrs,
  dxCore, cxClasses, dxGDIPlusClasses, cxLookAndFeels, dxBar, dxBuiltInPopupMenu, cxGraphics;

type
  TdxBarBuiltInMenuItemType = (bmitDefault, bmitChecked, bmitSubItem);

  { TdxBarBuiltInMenuHelper }

  TdxBarBuiltInMenuHelper = class(TComponent)
  private
    FBarManager: TdxBarManager;
    FItems: TObjectList;
    FPopupMenu: TdxBarPopupMenu;

    function GetBiDiMode: TBiDiMode;
    function GetImages: TCustomImageList;
    function GetLookAndFeel: TcxLookAndFeel;
    procedure SetBiDiMode(const Value: TBiDiMode);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetLookAndFeel(const Value: TcxLookAndFeel);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function CreateMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag; AEnabled: Boolean = True;
      AItemType: TdxBarBuiltInMenuItemType = bmitDefault; AChecked: Boolean = False; AImageIndex: Integer = -1;
      AWithSeparator: Boolean = False; AGlyph: TdxSmartGlyph = nil; AClickEvent: TNotifyEvent = nil; AShortCut: TShortCut = 0): TComponent;
    procedure CreatePopupMenu;
    procedure DestroyPopupMenu;

    property BarManager: TdxBarManager read FBarManager;
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property PopupMenu: TdxBarPopupMenu read FPopupMenu;
    property Images: TCustomImageList read GetImages write SetImages;
  end;

  { TdxBarBuiltInPopupMenuAdapter }

  TdxBarBuiltInPopupMenuAdapter = class(TdxCustomBuiltInPopupMenuAdapter)
  private
    FSeparatorRequired: Boolean;
  protected
    function GetCount: Integer; override;
    function GetItem(Index: Integer): TComponent; override;
    function GetPopupMenu: TComponent; override;
  public
    function Add(const ACaption: string; AClickEvent: TNotifyEvent; ATag: TcxTag = 0;
      AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0;
      AParentItem: TComponent = nil): TComponent; override;
    function AddSubMenu(const ACaption: string; AClickEvent: TNotifyEvent; ATag: TcxTag = 0;
      AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0;
      AParentItem: TComponent = nil): TComponent; override;
    procedure AddSeparator; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    function Popup(const P: TPoint): Boolean; override;
    procedure Remove(AItem: TComponent); override;
    procedure SetAction(AItem: TComponent; AValue: TBasicAction); override;
    procedure SetChecked(AItem: TComponent; AValue: Boolean); override;
    procedure SetDefault(AItem: TComponent; AValue: Boolean); override;
    procedure SetGlyph(AItem: TComponent; AGlyph: TGraphic); override;
    procedure SetImages(AImages: TCustomImageList); override;
    procedure SetImages(AItem: TComponent; AImages: TCustomImageList); override;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel); override;
  end;

function dxBarPopupHelper: TdxBarBuiltInMenuHelper;

implementation

uses
  SysUtils, Controls, Forms, dxForms, dxDPIAwareUtils;

var
  FFakeForm: TdxCustomForm;
  FFakeBarManager: TdxBarManager;
  FBarPopupHelper: TdxBarBuiltInMenuHelper;

function GetFakeForm: TCustomForm;
begin
  if FFakeForm = nil then
    FFakeForm := TdxCustomForm.CreateNew(nil);
  Result := FFakeForm;
end;

function GetFakeBarManager: TdxBarManager;
begin
  if FFakeBarManager = nil then
  begin
    FFakeBarManager := TdxBarManager.Create(GetFakeForm);
    FFakeBarManager.ImageOptions.StretchGlyphs := False;
    FFakeBarManager.Style := bmsUseLookAndFeel;
  end;
  Result := FFakeBarManager;
end;

function dxBarPopupHelper: TdxBarBuiltInMenuHelper;
begin
  if FBarPopupHelper = nil then
    FBarPopupHelper := TdxBarBuiltInMenuHelper.Create;
  Result := FBarPopupHelper;
end;

{ TdxBarBuiltInMenuHelper }

constructor TdxBarBuiltInMenuHelper.Create;
begin
  inherited Create(nil);
  FBarManager := GetFakeBarManager;
  FBarManager.FreeNotification(Self);
end;

destructor TdxBarBuiltInMenuHelper.Destroy;
begin
  if FBarManager <> nil then
    FBarManager.RemoveFreeNotification(Self);
  DestroyPopupMenu;
  inherited Destroy;
end;

function TdxBarBuiltInMenuHelper.CreateMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag;
  AEnabled: Boolean = True; AItemType: TdxBarBuiltInMenuItemType = bmitDefault; AChecked: Boolean = False;
  AImageIndex: Integer = -1; AWithSeparator: Boolean = False; AGlyph: TdxSmartGlyph = nil; AClickEvent: TNotifyEvent = nil;
  AShortCut: TShortCut = 0): TComponent;
const
  ItemClassMap: array [Boolean] of TdxBarItemClass = (TdxBarButton, TdxBarSubItem);
var
  ALinksOwner: IdxBarLinksOwner;
  ALink: TdxBarItemLink;
begin
  if Supports(AOwner, IdxBarLinksOwner, ALinksOwner) and (ALinksOwner.GetItemLinks <> nil) then
  begin
    ALink := ALinksOwner.GetItemLinks.AddItem(ItemClassMap[AItemType = bmitSubItem]);
    ALink.BeginGroup := AWithSeparator;
    Result := ALink.Item;
    BarDesignController.AddInternalItem(TdxBarItem(Result), FItems);
    TdxBarItem(Result).Enabled := AEnabled;
    TdxBarItem(Result).Caption := ACaption;
    TdxBarItem(Result).Tag := ACommand;
    TdxBarItem(Result).OnClick := AClickEvent;
    TdxBarItem(Result).ImageIndex := AImageIndex;
    TdxBarItem(Result).Glyph := AGlyph;
    TdxBarItem(Result).ShortCut := AShortCut;

    if AItemType <> bmitSubItem then
    begin
      if AItemType = bmitChecked then
        TdxBarButton(Result).ButtonStyle := bsChecked;
      TdxBarButton(Result).Down := AChecked;
    end;
  end
  else
    Result := nil;
end;

procedure TdxBarBuiltInMenuHelper.CreatePopupMenu;
begin
  DestroyPopupMenu;
  FItems := TObjectList.Create;
  FPopupMenu := TdxBarPopupMenu.Create(GetFakeForm);
  FPopupMenu.BarManager := FBarManager;
end;

procedure TdxBarBuiltInMenuHelper.DestroyPopupMenu;
begin
  FreeAndNil(FPopupMenu);
  FreeAndNil(FItems);
end;

function TdxBarBuiltInMenuHelper.GetBiDiMode: TBiDiMode;
begin
  Result := FBarManager.BiDiMode;
end;

function TdxBarBuiltInMenuHelper.GetImages: TCustomImageList;
begin
  Result := FBarManager.Images;
end;

function TdxBarBuiltInMenuHelper.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FBarManager.LookAndFeel;
end;

procedure TdxBarBuiltInMenuHelper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = BarManager) then
  begin
    FFakeBarManager := nil;
    FreeAndNil(FBarPopupHelper);
  end;
end;

procedure TdxBarBuiltInMenuHelper.SetBiDiMode(const Value: TBiDiMode);
begin
  GetFakeForm.BiDiMode := Value;
end;

procedure TdxBarBuiltInMenuHelper.SetImages(const Value: TCustomImageList);
begin
  FBarManager.Images := Value;
end;

procedure TdxBarBuiltInMenuHelper.SetLookAndFeel(const Value: TcxLookAndFeel);
begin
  FBarManager.LookAndFeel.MasterLookAndFeel := Value;
end;

{ TdxBarBuiltInPopupMenuAdapter }

function TdxBarBuiltInPopupMenuAdapter.Add(const ACaption: string; AClickEvent: TNotifyEvent;
  ATag: TcxTag = 0; AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0;
  AParentItem: TComponent = nil): TComponent;
begin
  if dxBarPopupHelper.PopupMenu = nil then
    dxBarPopupHelper.CreatePopupMenu;

  if AParentItem <> nil then
    Result := dxBarPopupHelper.CreateMenuItem(AParentItem, ACaption,
      ATag, AEnabled, bmitDefault, False, AImageIndex, FSeparatorRequired, nil, AClickEvent)
  else
    Result := dxBarPopupHelper.CreateMenuItem(dxBarPopupHelper.PopupMenu, ACaption,
      ATag, AEnabled, bmitDefault, False, AImageIndex, FSeparatorRequired, nil, AClickEvent);
  FSeparatorRequired := False;
end;

function TdxBarBuiltInPopupMenuAdapter.AddSubMenu(const ACaption: string; AClickEvent: TNotifyEvent;
  ATag: TcxTag = 0; AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0;
  AParentItem: TComponent = nil): TComponent;
begin
  if dxBarPopupHelper.PopupMenu = nil then
    dxBarPopupHelper.CreatePopupMenu;

  if AParentItem <> nil then
    Result := dxBarPopupHelper.CreateMenuItem(AParentItem, ACaption,
      ATag, AEnabled, bmitSubItem, False, AImageIndex, FSeparatorRequired, nil, AClickEvent)
  else
    Result := dxBarPopupHelper.CreateMenuItem(dxBarPopupHelper.PopupMenu, ACaption,
      ATag, AEnabled, bmitSubItem, False, AImageIndex, FSeparatorRequired, nil, AClickEvent);
  FSeparatorRequired := False;
end;

procedure TdxBarBuiltInPopupMenuAdapter.AfterConstruction;
begin
  inherited;
  Clear;
end;

procedure TdxBarBuiltInPopupMenuAdapter.BeforeDestruction;
begin
  inherited;
  Clear;
end;

procedure TdxBarBuiltInPopupMenuAdapter.AddSeparator;
begin
  FSeparatorRequired := True;
end;

procedure TdxBarBuiltInPopupMenuAdapter.Clear;
begin
  dxBarPopupHelper.DestroyPopupMenu;
end;

function TdxBarBuiltInPopupMenuAdapter.Popup(const P: TPoint): Boolean;
begin
  dxBarPopupHelper.BiDiMode := BiDiMode;
  try
    Result := inherited Popup(P);
  finally
    dxBarPopupHelper.BiDiMode := Application.BiDiMode;
  end;
end;

procedure TdxBarBuiltInPopupMenuAdapter.Remove(AItem: TComponent);
var
  AButton: TdxBarButton;
begin
  AButton := AItem as TdxBarButton;
  while AButton.LinkCount > 0 do
    AButton.Links[AButton.LinkCount - 1].Free;
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetAction(AItem: TComponent; AValue: TBasicAction);
begin
  (AItem as TdxBarItem).Action := AValue;
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetChecked(AItem: TComponent; AValue: Boolean);
var
  AButton: TdxBarButton;
begin
  AButton := AItem as TdxBarButton;
  AButton.ButtonStyle := bsChecked;
  AButton.Down := AValue;
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetDefault(AItem: TComponent; AValue: Boolean);
begin
  // do nothing
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetGlyph(AItem: TComponent; AGlyph: TGraphic);
begin
  (AItem as TdxBarItem).Glyph.Assign(AGlyph);
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetImages(AImages: TCustomImageList);
begin
  dxBarPopupHelper.Images := AImages;
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetImages(AItem: TComponent; AImages: TCustomImageList);
begin
  if AItem is TdxBarSubItem then
    TdxBarSubItem(AItem).Images := AImages;
end;

procedure TdxBarBuiltInPopupMenuAdapter.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  dxBarPopupHelper.LookAndFeel := AValue;
end;

function TdxBarBuiltInPopupMenuAdapter.GetCount: Integer;
begin
  if dxBarPopupHelper.PopupMenu <> nil then
    Result := dxBarPopupHelper.PopupMenu.ItemLinks.Count
  else
    Result := 0;
end;

function TdxBarBuiltInPopupMenuAdapter.GetItem(Index: Integer): TComponent;
begin
  Result := dxBarPopupHelper.PopupMenu.ItemLinks[Index].Item;
end;

function TdxBarBuiltInPopupMenuAdapter.GetPopupMenu: TComponent;
begin
  Result := dxBarPopupHelper.PopupMenu;
end;

initialization
  TdxBuiltInPopupMenuAdapterManager.Register(TdxBarBuiltInPopupMenuAdapter);

finalization
  TdxBuiltInPopupMenuAdapterManager.Unregister(TdxBarBuiltInPopupMenuAdapter);
  FreeAndNil(FBarPopupHelper);
  FreeAndNil(FFakeBarManager);
  FreeAndNil(FFakeForm);

end.
