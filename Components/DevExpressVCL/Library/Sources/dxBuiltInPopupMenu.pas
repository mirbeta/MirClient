{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxBuiltInPopupMenu;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Menus, Classes, Graphics, cxControls, cxClasses, dxCore, ImgList, cxGraphics, cxLookAndFeels, ActnList, Forms;

type

  { TdxCustomBuiltInPopupMenuAdapter }

  TdxCustomBuiltInPopupMenuAdapterClass = class of TdxCustomBuiltInPopupMenuAdapter;
  TdxCustomBuiltInPopupMenuAdapter = class(TComponent)
  strict private
    FBiDiMode: TBiDiMode;
    FIsBiDiModeAssigned: Boolean;
    function GetBiDiMode: TBiDiMode;
    procedure SetBiDiMode(AValue: TBiDiMode);
  protected
    function GetCount: Integer; virtual; abstract;
    function GetItem(Index: Integer): TComponent; virtual; abstract;
    function GetPopupMenu: TComponent; virtual; abstract;
  public
    function Add(const ACaption: string; AClickEvent: TNotifyEvent; ATag: TcxTag = 0;
      AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParentItem: TComponent = nil): TComponent; virtual; abstract;
    function AddSubMenu(const ACaption: string; AClickEvent: TNotifyEvent; ATag: TcxTag = 0;
      AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParentItem: TComponent = nil): TComponent; virtual; abstract;
    procedure AddSeparator; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer);
    procedure Remove(AItem: TComponent); virtual;
    procedure SetAction(AItem: TComponent; AValue: TBasicAction); virtual; abstract;
    procedure SetChecked(AItem: TComponent; AValue: Boolean); virtual; abstract;
    procedure SetDefault(AItem: TComponent; AValue: Boolean); virtual; abstract;
    procedure SetGlyph(AItem: TComponent; AGlyph: TGraphic); virtual; abstract;
    procedure SetImages(AImages: TCustomImageList); overload; virtual; abstract;
    procedure SetImages(AItem: TComponent; AImages: TCustomImageList); overload; virtual; abstract;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel); virtual;
    function Popup(const P: TPoint): Boolean; virtual;
    //
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TComponent read GetItem;
    property PopupMenu: TComponent read GetPopupMenu;
  end;

  { TdxBuiltInPopupMenuAdapterHelper }

  TdxBuiltInPopupMenuAdapterHelper = class
  public
    class procedure AddMenu(AAdapter: TdxCustomBuiltInPopupMenuAdapter; AMenu: TPopupMenu; AClickEvent: TNotifyEvent);
    class function AddMenuItem(AAdapter: TdxCustomBuiltInPopupMenuAdapter; AMenuItem: TMenuItem;
      AClickEvent: TNotifyEvent; ATag: TcxTag = 0; AParentItem: TComponent = nil): TComponent;
  end;

  { TdxStandardBuiltInPopupMenuAdapter }

  TdxStandardBuiltInPopupMenuAdapter = class(TdxCustomBuiltInPopupMenuAdapter)
  private
    FPopupMenu: TComponent;
    FSeparatorRequired: Boolean;
  protected
    function GetCount: Integer; override;
    function GetItem(Index: Integer): TComponent; override;
    function GetPopupMenu: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const ACaption: string; AClickEvent: TNotifyEvent; ATag: TcxTag = 0;
      AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParentItem: TComponent = nil): TComponent; override;
    function AddSubMenu(const ACaption: string; AClickEvent: TNotifyEvent; ATag: TcxTag = 0;
      AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParentItem: TComponent = nil): TComponent; override;
    procedure AddSeparator; override;
    procedure Clear; override;
    procedure SetAction(AItem: TComponent; AValue: TBasicAction); override;
    procedure SetChecked(AItem: TComponent; AValue: Boolean); override;
    procedure SetDefault(AItem: TComponent; AValue: Boolean); override;
    procedure SetGlyph(AItem: TComponent; AGlyph: TGraphic); override;
    procedure SetImages(AImages: TCustomImageList); override;
    procedure SetImages(AItem: TComponent; AImages: TCustomImageList); override;
  end;

  { TdxBuiltInPopupMenuAdapterManager }

  TdxBuiltInPopupMenuAdapterManager = class
  public
    class function GetActualAdapterClass: TdxCustomBuiltInPopupMenuAdapterClass;
    class function IsActualAdapterStandard: Boolean;
    class procedure Register(AClass: TdxCustomBuiltInPopupMenuAdapterClass);
    class procedure Unregister(AClass: TdxCustomBuiltInPopupMenuAdapterClass);
  end;

implementation

uses
  SysUtils;

var
  FRegisteredClasses: TList;

{ TdxCustomBuiltInPopupMenuAdapter }

procedure TdxCustomBuiltInPopupMenuAdapter.Delete(Index: Integer);
begin
  Remove(Items[Index]);
end;

procedure TdxCustomBuiltInPopupMenuAdapter.Remove(AItem: TComponent);
begin
  AItem.Free;
end;

procedure TdxCustomBuiltInPopupMenuAdapter.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  // do nothing
end;

function TdxCustomBuiltInPopupMenuAdapter.Popup(const P: TPoint): Boolean;
begin
  Result := ShowPopupMenu(nil, PopupMenu, P);
end;

function TdxCustomBuiltInPopupMenuAdapter.GetBiDiMode: TBiDiMode;
begin
  if FIsBiDiModeAssigned then
    Result := FBiDiMode
  else
    Result := Application.BiDiMode;
end;

procedure TdxCustomBuiltInPopupMenuAdapter.SetBiDiMode(AValue: TBiDiMode);
begin
  FIsBiDiModeAssigned := True;
  FBiDiMode := AValue;
end;

{ TdxBuiltInPopupMenuAdapterHelper }

class procedure TdxBuiltInPopupMenuAdapterHelper.AddMenu(
  AAdapter: TdxCustomBuiltInPopupMenuAdapter; AMenu: TPopupMenu; AClickEvent: TNotifyEvent);

  procedure PopulateLevel(AParentItem: TComponent; AMenuItem: TMenuItem);
  var
    I: Integer;
  begin
    if AMenuItem.Visible then
    begin
      AParentItem := AddMenuItem(AAdapter, AMenuItem, AClickEvent, TcxTag(AMenuItem), AParentItem);
      if AMenuItem.SubMenuImages <> nil then
        AAdapter.SetImages(AParentItem, AMenuItem.SubMenuImages);
      for I := 0 to AMenuItem.Count - 1 do
        PopulateLevel(AParentItem, AMenuItem[I]);
    end;
  end;

var
  I: Integer;
begin
  AAdapter.SetImages(AMenu.Images);
  for I := 0 to AMenu.Items.Count - 1 do
    PopulateLevel(nil, AMenu.Items[I]);
end;

class function TdxBuiltInPopupMenuAdapterHelper.AddMenuItem(AAdapter: TdxCustomBuiltInPopupMenuAdapter;
  AMenuItem: TMenuItem; AClickEvent: TNotifyEvent; ATag: TcxTag; AParentItem: TComponent): TComponent;
begin
  if AMenuItem.IsLine then
  begin
    AAdapter.AddSeparator;
    Result := nil;
  end
  else
  begin
    if AMenuItem.Count > 0 then
      Result := AAdapter.AddSubMenu(AMenuItem.Caption, AClickEvent, ATag,
        AMenuItem.ImageIndex, AMenuItem.Enabled, AMenuItem.ShortCut, AParentItem)
    else
      Result := AAdapter.Add(AMenuItem.Caption, AClickEvent, ATag,
        AMenuItem.ImageIndex, AMenuItem.Enabled, AMenuItem.ShortCut, AParentItem);

    if not AMenuItem.Bitmap.Empty then
      AAdapter.SetGlyph(Result, AMenuItem.Bitmap);
    if AMenuItem.AutoCheck or AMenuItem.Checked then
      AAdapter.SetChecked(Result, AMenuItem.Checked);
    if AMenuItem.Default then
      AAdapter.SetDefault(Result, AMenuItem.Default);
    if AMenuItem.Action <> nil then
      AAdapter.SetAction(Result, AMenuItem.Action);
  end;
end;

{ TdxStandardBuiltInPopupMenuAdapter }

constructor TdxStandardBuiltInPopupMenuAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupMenu := TPopupMenu.Create(Self);
end;

destructor TdxStandardBuiltInPopupMenuAdapter.Destroy;
begin
  FreeAndNil(FPopupMenu);
  inherited Destroy;
end;

function TdxStandardBuiltInPopupMenuAdapter.Add(const ACaption: string; AClickEvent: TNotifyEvent;
  ATag: TcxTag = 0; AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0;
  AParentItem: TComponent = nil): TComponent;
var
  AMenuItem: TMenuItem;
begin
  if FSeparatorRequired then
  begin
    FSeparatorRequired := False;
    Add('-', TNotifyEvent(nil), 0, -1, True, 0, AParentItem);
  end;

  AMenuItem := TMenuItem.Create(PopupMenu);
  AMenuItem.Caption := ACaption;
  AMenuItem.Tag := ATag;
  AMenuItem.Enabled := AEnabled;
  AMenuItem.ImageIndex := AImageIndex;
  AMenuItem.ShortCut := AShortCut;
  AMenuItem.OnClick := AClickEvent;
  if AParentItem = nil then
    TPopupMenu(PopupMenu).Items.Add(AMenuItem)
  else
    TMenuItem(AParentItem).Add(AMenuItem);
  Result := AMenuItem;
end;

function TdxStandardBuiltInPopupMenuAdapter.AddSubMenu(const ACaption: string; AClickEvent: TNotifyEvent;
  ATag: TcxTag = 0; AImageIndex: TcxImageIndex = -1; AEnabled: Boolean = True; AShortCut: TShortCut = 0;
  AParentItem: TComponent = nil): TComponent;
begin
  Result := Add(ACaption, AClickEvent, ATag, AImageIndex, AEnabled, AShortCut, AParentItem);
end;

procedure TdxStandardBuiltInPopupMenuAdapter.AddSeparator;
begin
  FSeparatorRequired := True;
end;

procedure TdxStandardBuiltInPopupMenuAdapter.Clear;
begin
  TPopupMenu(PopupMenu).Items.Clear
end;

procedure TdxStandardBuiltInPopupMenuAdapter.SetAction(AItem: TComponent; AValue: TBasicAction);
begin
  (AItem as TMenuItem).Action := AValue;
end;

procedure TdxStandardBuiltInPopupMenuAdapter.SetChecked(AItem: TComponent; AValue: Boolean);
begin
  (AItem as TMenuItem).Checked := AValue;
end;

procedure TdxStandardBuiltInPopupMenuAdapter.SetDefault(AItem: TComponent; AValue: Boolean);
begin
  (AItem as TMenuItem).Default := AValue;
end;

procedure TdxStandardBuiltInPopupMenuAdapter.SetGlyph(AItem: TComponent; AGlyph: TGraphic);
var
  ABitmap: TBitmap;
  AMenuItem: TMenuItem;
begin
  if not AGlyph.Empty then
  begin
    AMenuItem := AItem as TMenuItem;
    if AGlyph is TBitmap then
      AMenuItem.Bitmap := TBitmap(AGlyph)
    else
    begin
      ABitmap := cxGetAsBitmap(AGlyph);
      try
        AMenuItem.Bitmap := ABitmap;
      finally
        ABitmap.Free;
      end;
    end;
  end;
end;

procedure TdxStandardBuiltInPopupMenuAdapter.SetImages(AImages: TCustomImageList);
begin
  TPopupMenu(PopupMenu).Images := AImages;
end;

procedure TdxStandardBuiltInPopupMenuAdapter.SetImages(AItem: TComponent; AImages: TCustomImageList);
begin
  TMenuItem(AItem).SubMenuImages := AImages;
end;

function TdxStandardBuiltInPopupMenuAdapter.GetCount: Integer;
begin
  Result := TPopupMenu(PopupMenu).Items.Count;
end;

function TdxStandardBuiltInPopupMenuAdapter.GetItem(Index: Integer): TComponent;
begin
  Result := TPopupMenu(PopupMenu).Items[Index];
end;

function TdxStandardBuiltInPopupMenuAdapter.GetPopupMenu: TComponent;
begin
  Result := FPopupMenu;
end;

{ TdxBuiltInPopupMenuAdapterManager }

class function TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass: TdxCustomBuiltInPopupMenuAdapterClass;
begin
  Result := TdxCustomBuiltInPopupMenuAdapterClass(FRegisteredClasses.Last);
end;

class function TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard: Boolean;
begin
  Result := GetActualAdapterClass = TdxStandardBuiltInPopupMenuAdapter;
end;

class procedure TdxBuiltInPopupMenuAdapterManager.Register(AClass: TdxCustomBuiltInPopupMenuAdapterClass);
begin
  if FRegisteredClasses = nil then
    FRegisteredClasses := TList.Create;
  FRegisteredClasses.Add(AClass);
end;

class procedure TdxBuiltInPopupMenuAdapterManager.Unregister(AClass: TdxCustomBuiltInPopupMenuAdapterClass);
begin
  FRegisteredClasses.Remove(AClass);
  if FRegisteredClasses.Count = 0 then
    FreeAndNil(FRegisteredClasses);
end;

initialization
  TdxBuiltInPopupMenuAdapterManager.Register(TdxStandardBuiltInPopupMenuAdapter);

finalization
  TdxBuiltInPopupMenuAdapterManager.Unregister(TdxStandardBuiltInPopupMenuAdapter);

end.
