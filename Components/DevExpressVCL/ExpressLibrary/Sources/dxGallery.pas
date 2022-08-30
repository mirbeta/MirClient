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

unit dxGallery;

{$I cxVer.inc}

interface

uses
  Classes, Graphics, SysUtils, ImgList,
  dxCoreClasses, dxCore, cxClasses, dxGDIPlusClasses, cxGraphics;

type
  TdxGalleryItem = class;
  TdxGalleryGroup = class;
  TdxGalleryGroups = class;

  TdxCustomGalleryItem = class;
  TdxGalleryItemClass = class of TdxCustomGalleryItem;
  TdxCustomGalleryItems = class;
  TdxGalleryItemsClass = class of TdxCustomGalleryItems;
  TdxCustomGalleryGroup = class;
  TdxGalleryGroupClass = class of TdxCustomGalleryGroup;
  TdxCustomGalleryGroups = class;
  TdxGalleryGroupsClass = class of TdxCustomGalleryGroups;
  TdxCustomGalleryGroupHeader = class;
  TdxGalleryGroupHeaderClass = class of TdxCustomGalleryGroupHeader;

  TdxGalleryCustomizationAction = (gcaChangeGroupCaption, gcaChangeItemCaption);
  TdxGalleryCustomizationActions = set of TdxGalleryCustomizationAction;

  TdxGalleryItemEvent = procedure(Sender: TObject; AItem: TdxGalleryItem) of object;
  TdxGalleryItemCheckMode = (icmNone, icmSingleCheck, icmSingleRadio, icmMultiple{,
    icmSingleCheckInGroup, icmSingleRadioInGroup, icmMultipleInGroup});
  TdxGalleryChangeType = (gctLight, gctHard);
  TdxGalleryChangeEvent = procedure(Sender: TObject; AChangeType: TdxGalleryChangeType) of object;

  IdxGalleryGroups = interface;
  IdxGalleryItems = interface;

  { IdxGalleryObject }

  IdxGalleryObject = interface
  ['{DF69CAD3-7A28-480E-BC37-D412C8985FC9}']
    function GetInstance: TObject;
    property Instance: TObject read GetInstance;
  end;

  { IdxGalleryItem }

  IdxGalleryItem = interface(IdxGalleryObject)
  ['{B30DECAE-EDCD-4221-B404-5E811A866DA6}']
    function GetCaption: string;
    function GetItems: IdxGalleryItems;
    function GetIndex: Integer;
    procedure SetCaption(const AValue: string);
    procedure SetIndex(AIndex: Integer);
    procedure SetItems(AItems: IdxGalleryItems);

    property Index: Integer read GetIndex write SetIndex;
    property Items: IdxGalleryItems read GetItems write SetItems;
  end;

  { IdxGalleryItems }

  IdxGalleryItems = interface(IdxGalleryObject)
  ['{B695ABB2-86B7-403E-AF57-B0EBAABBA0FA}']
    function Add: IdxGalleryItem;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IdxGalleryItem;
    function Insert(AIndex: Integer): IdxGalleryItem;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
  end;

  { IdxGalleryGroup }

  IdxGalleryGroup = interface(IdxGalleryObject)
  ['{250CA14D-1229-45EB-938F-E2C6FED13645}']
    function GetCaption: string;
    function GetGroups: IdxGalleryGroups;
    function GetIndex: Integer;
    function GetItems: IdxGalleryItems;
    procedure SetCaption(const AValue: string);
    procedure SetIndex(AIndex: Integer);

    property Index: Integer read GetIndex write SetIndex;
    property Groups: IdxGalleryGroups read GetGroups;
  end;

  { IdxGalleryGroups }

  IdxGalleryGroups = interface(IdxGalleryObject)
  ['{C853A9B6-66EF-4397-BEEF-5A5F8CA74EDD}']
    function Add: IdxGalleryGroup;
    function GetCount: Integer;
    function GetDisplayName: string;
    function GetGroup(AIndex: Integer): IdxGalleryGroup;
    function Insert(AIndex: Integer): IdxGalleryGroup;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
  end;

  { IdxGallery }

  IdxGallery = interface
  ['{B652EE21-71B8-4C82-9D33-BA53365A3627}']
    function GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
    function GetGroups: IdxGalleryGroups;

    property Groups: IdxGalleryGroups read GetGroups;
  end;

  { IdxGallery2 }

  IdxGallery2 = interface
  ['{418FD781-56A7-4EA6-8720-1D0D479EBC2E}']
    function GetItemCheckMode: TdxGalleryItemCheckMode;
    procedure SetItemCheckMode(AValue: TdxGalleryItemCheckMode);
    function GetGroups: TdxGalleryGroups;
    function GetOnItemClick: TdxGalleryItemEvent;
    procedure SetOnItemClick(AValue: TdxGalleryItemEvent);

    function GetInstance: TObject;
    function GetParentComponent: TComponent;

    // Check operations
    procedure ClickItem(AItem: TdxGalleryItem);
    function GetCheckedItem: TdxGalleryItem;
    procedure GetCheckedItems(AList: TList);
    procedure UncheckAll;

    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode write SetItemCheckMode;
    property Groups: TdxGalleryGroups read GetGroups;
    property OnItemClick: TdxGalleryItemEvent read GetOnItemClick write SetOnItemClick;
  end;

  { IdxGalleryOwner }

  IdxGalleryOwner = interface
  ['{9E2F64DD-D370-4B24-8FF3-7E4FE08A15F1}']
    function GetGallery: IdxGallery;
    function GetGallery2: IdxGallery2;
  end;

  { TdxCustomGalleryItem }

  TdxCustomGalleryItem = class(TcxComponentCollectionItem, IdxGalleryItem)
  strict private
    FCaption: string;
    FDescription: string;
    FEnabled: Boolean;
    FGlyph: TdxSmartGlyph;
    FHint: string;
    FImageIndex: TcxImageIndex;

    procedure GlyphChangeHandler(Sender: TObject);

    function GetCaption: string;
    procedure SetCaption(const AValue: string);
    procedure SetDescription(const AValue: string);
    procedure SetEnabled(AValue: Boolean);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetImageIndex(Value: TcxImageIndex);

    // IdxGalleryItem
    function IdxGalleryItem.GetItems = GetGalleryItems;
    procedure IdxGalleryItem.SetItems = SetGalleryItems;
    function GetInstance: TObject;
    function GetGalleryItems: IdxGalleryItems;
    procedure SetGalleryItems(AItems: IdxGalleryItems);

    procedure ReadActionIndex(Reader: TReader);
    procedure WriteActionIndex(Writer: TWriter);
  protected
    FActionIndex: Variant;

    procedure ChangeScale(M, D: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Caption: string read GetCaption write SetCaption;
    property Description: string read FDescription write SetDescription;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property Hint: string read FHint write FHint;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
  end;

  { TdxCustomGalleryItems }

  TdxCustomGalleryItems = class(TcxInterfacedComponentCollection, IdxGalleryItems)
  strict private
    // IdxGalleryItems
    function IdxGalleryItems.Add = AddGalleryItem;
    function IdxGalleryItems.GetItem = GetGalleryItem;
    function IdxGalleryItems.Insert = InsertGalleryItem;
    function GetInstance: TObject;
    function AddGalleryItem: IdxGalleryItem;
    function GetGalleryItem(AIndex: Integer): IdxGalleryItem;
    function InsertGalleryItem(AIndex: Integer): IdxGalleryItem;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function GetItem(AIndex: Integer): TdxCustomGalleryItem;
    procedure SetItem(AIndex: Integer; AValue: TdxCustomGalleryItem);
  public
    function Add: TdxCustomGalleryItem;
    property Items[AIndex: Integer]: TdxCustomGalleryItem read GetItem write SetItem; default;
  end;

  { TdxCustomGalleryGroupHeader }

  TdxCustomGalleryGroupHeader = class(TcxOwnedPersistent)
  strict private
    FAlignment: TAlignment;
    FCaption: string;
    FVisible: Boolean;

    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  { TdxCustomGalleryGroup }

  TdxCustomGalleryGroup = class(TcxComponentCollectionItem, IdxGalleryGroup)
  strict private
    FHeader: TdxCustomGalleryGroupHeader;
    FItems: TdxCustomGalleryItems;
    FVisible: Boolean;

    function GetCaption: string;
    function GetItemCount: Integer;
    function GetShowCaption: Boolean;
    function GetVisible: Boolean;
    procedure SetCaption(const AValue: string);
    procedure SetItems(AValue: TdxCustomGalleryItems);
    procedure SetHeader(AValue: TdxCustomGalleryGroupHeader);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);

    // IdxGalleryGroup
    function IdxGalleryGroup.GetGroups = GetGalleryGroups;
    function IdxGalleryGroup.GetItems = GetGalleryGroupItems;
    function GetGalleryGroups: IdxGalleryGroups;
    function GetGalleryGroupItems: IdxGalleryItems;
    function GetInstance: TObject;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CreateItems;
    procedure ItemsChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);

    function GetGalleryGroupHeaderClass: TdxGalleryGroupHeaderClass; virtual;
    function GetGalleryItemClass: TdxGalleryItemClass; virtual;
    function GetGalleryItemsClass: TdxGalleryItemsClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Caption: string read GetCaption write SetCaption;
    property Items: TdxCustomGalleryItems read FItems write SetItems;
    property ItemCount: Integer read GetItemCount;
    property Header: TdxCustomGalleryGroupHeader read FHeader write SetHeader;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  { TdxCustomGallertyGroups }

  TdxCustomGalleryGroups = class(TcxInterfacedComponentCollection, IdxGalleryGroups)
  strict private
    function GetGroup(AIndex: Integer): TdxCustomGalleryGroup;
    procedure SetGroup(AIndex: Integer; AValue: TdxCustomGalleryGroup);

    // IdxGalleryGroups
    function IdxGalleryGroups.Add = AddGalleryGroup;
    function GetDisplayName: string;
    function GetInstance: TObject;
    function IdxGalleryGroups.GetGroup = GetGalleryGroup;
    function IdxGalleryGroups.Insert = InsertGalleryGroup;
    function AddGalleryGroup: IdxGalleryGroup;
    function GetGalleryGroup(AIndex: Integer): IdxGalleryGroup;
    function InsertGalleryGroup(AIndex: Integer): IdxGalleryGroup;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
  public
    function Add: TdxCustomGalleryGroup;
    //
    property Groups[AIndex: Integer]: TdxCustomGalleryGroup read GetGroup write SetGroup; default;
  end;

  { TdxGalleryItem }

  TdxGalleryItem = class(TdxCustomGalleryItem)
  private
    FChecked: Boolean;

    procedure SetChecked(AValue: Boolean);
  protected
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetGallery: IdxGallery2;
    function GetGroup: TdxGalleryGroup;
    procedure SetGroup(AGroup: TdxGalleryGroup);

    property Gallery: IdxGallery2 read GetGallery;
    property Group: TdxGalleryGroup read GetGroup write SetGroup;
  public
    property Checked: Boolean read FChecked write SetChecked default False;
  end;

  { TdxGalleryItems }

  TdxGalleryItems = class(TdxCustomGalleryItems)
  strict private
    function GetItem(AIndex: Integer): TdxGalleryItem;
    procedure SetItem(AIndex: Integer; AValue: TdxGalleryItem);
  protected
    function GetItemPrefixName: string; override;
  public
    function Add: TdxGalleryItem;
    property Items[AIndex: Integer]: TdxGalleryItem read GetItem write SetItem; default;
  end;

  { TdxGalleryGroupHeader }

  TdxGalleryGroupHeader = class(TdxCustomGalleryGroupHeader)
  strict private
    FOnChange: TNotifyEvent;
  protected
    procedure Changed; override;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxGalleryGroup }

  TdxGalleryGroup = class(TdxCustomGalleryGroup)
  strict private
    function GetItems: TdxGalleryItems;
    function GetHeader: TdxGalleryGroupHeader;
    procedure SetItems(AValue: TdxGalleryItems);
    procedure SetHeader(AValue: TdxGalleryGroupHeader);

    procedure HeaderChangeHandler(ASender: TObject);
  protected
    function GetGalleryGroupHeaderClass: TdxGalleryGroupHeaderClass; override;
    function GetGalleryItemClass: TdxGalleryItemClass; override;
    function GetGalleryItemsClass: TdxGalleryItemsClass; override;

    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetGallery: IdxGallery2;

    property Gallery: IdxGallery2 read GetGallery;
    property Header: TdxGalleryGroupHeader read GetHeader write SetHeader;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    property Items: TdxGalleryItems read GetItems write SetItems;
  end;

  { TdxGalleryGroups }

  TdxGalleryGroups = class(TdxCustomGalleryGroups)
  strict private
    function GetGroup(AIndex: Integer): TdxGalleryGroup;
    procedure SetGroup(AIndex: Integer; AValue: TdxGalleryGroup);
  protected
    function GetItemPrefixName: string; override;
  public
    function Add: TdxGalleryGroup;
    function FindByCaption(const ACaption: string; out AGroup: TdxGalleryGroup): Boolean;
    //
    property Groups[AIndex: Integer]: TdxGalleryGroup read GetGroup write SetGroup; default;
  end;

  { TdxCustomGallery }

  TdxCustomGallery = class(TcxOwnedInterfacedPersistent, IdxGallery, IdxGallery2)
  strict private
    FGroups: TdxGalleryGroups;
    FItemCheckMode: TdxGalleryItemCheckMode;

    FOnChange: TdxGalleryChangeEvent;
    FOnItemClick: TdxGalleryItemEvent;

    function GetItemCheckMode: TdxGalleryItemCheckMode;
    procedure SetItemCheckMode(AValue: TdxGalleryItemCheckMode);
    function GetGroups: TdxGalleryGroups;
    procedure SetGroups(AValue: TdxGalleryGroups);
    function GetOnChange: TdxGalleryChangeEvent;
    procedure SetOnChange(AValue: TdxGalleryChangeEvent);
    function GetOnItemClick: TdxGalleryItemEvent;
    procedure SetOnItemClick(AValue: TdxGalleryItemEvent);

    procedure GroupsChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
  protected
    function GetGroupClass: TdxGalleryGroupClass; virtual;
    function GetGroupsClass: TdxGalleryGroupsClass; virtual;

    procedure CheckItem(AItem: TdxGalleryItem; AValue: Boolean);
    procedure Changed(AType: TdxGalleryChangeType);
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DoClickItem(AItem: TdxGalleryItem);

    function GetInstance: TObject;
    function GetParentComponent: TComponent;

    // IdxGallery
    function GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
    function GetGalleryGroups: IdxGalleryGroups;
    function IdxGallery.GetGroups = GetGalleryGroups;

    property OnChange: TdxGalleryChangeEvent read GetOnChange write SetOnChange;
    property OnItemClick: TdxGalleryItemEvent read GetOnItemClick write SetOnItemClick;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);

    procedure ClickItem(AItem: TdxGalleryItem);
    procedure GetAllItems(AList: TList);
    function GetCheckedItem: TdxGalleryItem;
    function GetFirstItem: TdxGalleryItem;
    function GetFirstVisibleItem: TdxGalleryItem;
    procedure GetCheckedItems(AList: TList);
    procedure UncheckAll;

    function FindItemByTag(ATag: TcxTag): TdxGalleryItem;

    property ItemCheckMode: TdxGalleryItemCheckMode read GetItemCheckMode write SetItemCheckMode default icmNone;
    property Groups: TdxGalleryGroups read GetGroups write SetGroups;
  end;

  { TdxGallery }

  TdxGallery = class(TdxCustomGallery);

implementation

uses
  StrUtils, Controls, Variants;

{ TdxCustomGalleryItem }

constructor TdxCustomGalleryItem.Create(AOwner: TComponent);
begin
  inherited;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChangeHandler;
  FEnabled := True;
  FImageIndex := -1;
end;

destructor TdxCustomGalleryItem.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited;
end;

procedure TdxCustomGalleryItem.GlyphChangeHandler(Sender: TObject);
begin
  Changed(True);
end;

procedure TdxCustomGalleryItem.Assign(Source: TPersistent);
begin
  if Source is TdxCustomGalleryItem then
  begin
    FActionIndex := TdxCustomGalleryItem(Source).FActionIndex;
    Caption := TdxCustomGalleryItem(Source).Caption;
    Description := TdxCustomGalleryItem(Source).Description;
    Enabled := TdxCustomGalleryItem(Source).Enabled;
    Glyph := TdxCustomGalleryItem(Source).Glyph;
    Hint := TdxCustomGalleryItem(Source).Hint;
    ImageIndex := TdxCustomGalleryItem(Source).ImageIndex;
  end
  else
    inherited;
end;

procedure TdxCustomGalleryItem.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TdxCustomGalleryItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ActionIndex', ReadActionIndex, WriteActionIndex, FActionIndex <> Null);
end;

function TdxCustomGalleryItem.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TdxCustomGalleryItem.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed(True);
  end;
end;

procedure TdxCustomGalleryItem.SetDescription(const AValue: string);
begin
  if FDescription <> AValue then
  begin
    FDescription := AValue;
    Changed(True);
  end;
end;

procedure TdxCustomGalleryItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Changed(False);
  end;
end;

procedure TdxCustomGalleryItem.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
  Changed(True);
end;

procedure TdxCustomGalleryItem.SetImageIndex(Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

function TdxCustomGalleryItem.GetGalleryItems: IdxGalleryItems;
begin
  Result := Collection as TdxCustomGalleryItems;
end;

function TdxCustomGalleryItem.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TdxCustomGalleryItem.SetGalleryItems(AItems: IdxGalleryItems);
begin
  Collection := AItems.Instance as TcxComponentCollection;
end;

procedure TdxCustomGalleryItem.ReadActionIndex(Reader: TReader);
begin
  FActionIndex := Reader.ReadVariant;
end;

procedure TdxCustomGalleryItem.WriteActionIndex(Writer: TWriter);
begin
  Writer.WriteVariant(FActionIndex);
end;

{ TdxCustomGalleryItems }

function TdxCustomGalleryItems.Add: TdxCustomGalleryItem;
begin
  Result := inherited Add as TdxCustomGalleryItem;
end;

procedure TdxCustomGalleryItems.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TdxCustomGalleryItems.GetItem(AIndex: Integer): TdxCustomGalleryItem;
begin
  Result := inherited Items[AIndex] as TdxCustomGalleryItem;
end;

procedure TdxCustomGalleryItems.SetItem(AIndex: Integer; AValue: TdxCustomGalleryItem);
begin
  inherited Items[AIndex] := AValue;
end;

function TdxCustomGalleryItems.AddGalleryItem: IdxGalleryItem;
begin
  Result := Add;
end;

function TdxCustomGalleryItems.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxCustomGalleryItems.GetGalleryItem(AIndex: Integer): IdxGalleryItem;
begin
  Result := Items[AIndex];
end;

function TdxCustomGalleryItems.InsertGalleryItem(AIndex: Integer): IdxGalleryItem;
begin
  Result := Insert(AIndex) as TdxCustomGalleryItem;
end;

{ TdxGalleryItem }

function TdxGalleryItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxGalleryGroup).Items;
end;

function TdxGalleryItem.GetGallery: IdxGallery2;
begin
  Result := ((Collection as TdxGalleryItems).GetOwner as TdxGalleryGroup).Gallery;
end;

function TdxGalleryItem.GetGroup: TdxGalleryGroup;
begin
  Result := GetParentComponent as TdxGalleryGroup;
end;

procedure TdxGalleryItem.SetChecked(AValue: Boolean);
begin
  if FChecked <> AValue then
    Gallery.ClickItem(Self);
end;

procedure TdxGalleryItem.SetGroup(AGroup: TdxGalleryGroup);
begin
  Collection := AGroup.Items;
end;

{ TdxGalleryItems }

function TdxGalleryItems.Add: TdxGalleryItem;
begin
  Result := inherited Add as TdxGalleryItem;
end;

function TdxGalleryItems.GetItemPrefixName: string;
begin
  Result := LeftStr(ItemClass.ClassName, Length(ItemClass.ClassName) - Length('Item'));
end;

function TdxGalleryItems.GetItem(AIndex: Integer): TdxGalleryItem;
begin
  Result := inherited Items[AIndex] as TdxGalleryItem;
end;

procedure TdxGalleryItems.SetItem(AIndex: Integer; AValue: TdxGalleryItem);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxCustomGalleryGroupHeader }

procedure TdxGalleryGroupHeader.Changed;
begin
  dxCallNotify(OnChange, Self);
end;

{ TdxCustomGalleryGroupHeader }

constructor TdxCustomGalleryGroupHeader.Create(AOwner: TPersistent);
begin
  inherited;
  FVisible := True;
end;

procedure TdxCustomGalleryGroupHeader.Changed;
begin
  // Do nothing
end;

procedure TdxCustomGalleryGroupHeader.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TdxCustomGalleryGroupHeader.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxCustomGalleryGroupHeader then
  begin
    Alignment := TdxCustomGalleryGroupHeader(Source).Alignment;
    Caption := TdxCustomGalleryGroupHeader(Source).Caption;
    Visible := TdxCustomGalleryGroupHeader(Source).Visible;
  end;
end;

procedure TdxCustomGalleryGroupHeader.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Changed;
  end;
end;

procedure TdxCustomGalleryGroupHeader.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed;
  end;
end;

procedure TdxCustomGalleryGroupHeader.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

{ TdxCustomGalleryGroup }

constructor TdxCustomGalleryGroup.Create(AOwner: TComponent);
begin
  inherited;
  FHeader := GetGalleryGroupHeaderClass.Create(Self);
  FVisible := True;
end;

destructor TdxCustomGalleryGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FHeader);
  inherited Destroy;
end;

procedure TdxCustomGalleryGroup.Assign(Source: TPersistent);
begin
  if Source is TdxCustomGalleryGroup then
  begin
    Header := TdxCustomGalleryGroup(Source).Header;
    Items := TdxCustomGalleryGroup(Source).Items;
    Visible := TdxCustomGalleryGroup(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TdxCustomGalleryGroup.ChangeScale(M, D: Integer);
begin
  Header.ChangeScale(M, D);
  Items.ChangeScale(M, D);
end;

procedure TdxCustomGalleryGroup.CreateItems;
begin
  FItems := GetGalleryItemsClass.Create(Self, GetGalleryItemClass);
  FItems.OnChange := ItemsChangeHandler;
end;

function TdxCustomGalleryGroup.GetCaption: string;
begin
  Result := FHeader.Caption;
end;

function TdxCustomGalleryGroup.GetGalleryGroupHeaderClass: TdxGalleryGroupHeaderClass;
begin
  Result := TdxCustomGalleryGroupHeader
end;

function TdxCustomGalleryGroup.GetGalleryItemClass: TdxGalleryItemClass;
begin
  Result := TdxCustomGalleryItem;
end;

function TdxCustomGalleryGroup.GetGalleryItemsClass: TdxGalleryItemsClass;
begin
  Result := TdxCustomGalleryItems;
end;

function TdxCustomGalleryGroup.GetItemCount: Integer;
begin
  Result := Items.Count;
end;

function TdxCustomGalleryGroup.GetShowCaption: Boolean;
begin
  Result := FHeader.Visible;
end;

function TdxCustomGalleryGroup.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TdxCustomGalleryGroup.ItemsChangeHandler(Sender: TObject;
  AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  Changed(True);
end;

procedure TdxCustomGalleryGroup.SetCaption(const AValue: string);
begin
  FHeader.Caption := AValue;
end;

procedure TdxCustomGalleryGroup.SetHeader(AValue: TdxCustomGalleryGroupHeader);
begin
  FHeader.Assign(AValue);
end;

procedure TdxCustomGalleryGroup.SetItems(AValue: TdxCustomGalleryItems);
begin
  FItems.Assign(AValue);
end;

procedure TdxCustomGalleryGroup.SetShowCaption(AValue: Boolean);
begin
  FHeader.Visible := AValue;
end;

procedure TdxCustomGalleryGroup.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

function TdxCustomGalleryGroup.GetGalleryGroups: IdxGalleryGroups;
begin
  Result := Collection as TdxCustomGalleryGroups;
end;

function TdxCustomGalleryGroup.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxCustomGalleryGroup.GetGalleryGroupItems: IdxGalleryItems;
begin
  Result := Items;
end;

{ TdxGalleryGroup }

constructor TdxGalleryGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateItems;
  Header.OnChange := HeaderChangeHandler;
end;

procedure TdxGalleryGroup.Assign(Source: TPersistent);
begin
end;

procedure TdxGalleryGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Items[I].Owner = Root then Proc(Items[I]);
end;

function TdxGalleryGroup.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
var
  AGallery: IdxGallery2;
  AGalleryOwner: IdxGalleryOwner;
begin
  if Supports(AParent, IdxGallery2, AGallery) then
    Result := AGallery.Groups
  else
    if Supports(AParent, IdxGalleryOwner, AGalleryOwner) then
      Result := AGalleryOwner.GetGallery2.Groups
    else
      Result := nil;
end;

function TdxGalleryGroup.GetGallery: IdxGallery2;
var
  AGalleryOwner: IdxGalleryOwner;
begin
  if not Supports(GetParentComponent, IdxGallery2, Result) then
  begin
    if Supports(GetParentComponent, IdxGalleryOwner, AGalleryOwner) then
      Result := AGalleryOwner.GetGallery2
    else
      Result := nil;
  end;
end;

function TdxGalleryGroup.GetGalleryGroupHeaderClass: TdxGalleryGroupHeaderClass;
begin
  Result := TdxGalleryGroupHeader;
end;

function TdxGalleryGroup.GetGalleryItemClass: TdxGalleryItemClass;
begin
  Result := TdxGalleryItem;
end;

function TdxGalleryGroup.GetGalleryItemsClass: TdxGalleryItemsClass;
begin
  Result := TdxGalleryItems;
end;

function TdxGalleryGroup.GetHeader: TdxGalleryGroupHeader;
begin
  Result := inherited Header as TdxGalleryGroupHeader;
end;

function TdxGalleryGroup.GetItems: TdxGalleryItems;
begin
  Result := inherited Items as TdxGalleryItems;
end;

procedure TdxGalleryGroup.SetHeader(AValue: TdxGalleryGroupHeader);
begin
  inherited Header.Assign(AValue);
end;

procedure TdxGalleryGroup.SetItems(AValue: TdxGalleryItems);
begin
  Items.Assign(AValue);
end;

procedure TdxGalleryGroup.HeaderChangeHandler(ASender: TObject);
begin
  Changed(True);
end;

{ TdxCustomGalleryGroups }

function TdxGalleryGroups.Add: TdxGalleryGroup;
begin
  Result := inherited Add as TdxGalleryGroup;
end;

function TdxGalleryGroups.GetItemPrefixName: string;
begin
  Result := LeftStr(ItemClass.ClassName, Length(ItemClass.ClassName) - Length('Group'));
end;

function TdxGalleryGroups.FindByCaption(
  const ACaption: string; out AGroup: TdxGalleryGroup): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := SameText(Groups[I].Caption, ACaption);
    if Result then
    begin
      AGroup := Groups[I];
      Break;
    end;
  end;
end;

function TdxGalleryGroups.GetGroup(AIndex: Integer): TdxGalleryGroup;
begin
  Result := inherited Items[AIndex] as TdxGalleryGroup;
end;

procedure TdxGalleryGroups.SetGroup(AIndex: Integer; AValue: TdxGalleryGroup);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxCustomGalleryGroups }

function TdxCustomGalleryGroups.Add: TdxCustomGalleryGroup;
begin
  Result := inherited Add as TdxCustomGalleryGroup;
end;

procedure TdxCustomGalleryGroups.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Groups[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TdxCustomGalleryGroups.GetGroup(AIndex: Integer): TdxCustomGalleryGroup;
begin
  Result := inherited Items[AIndex] as TdxCustomGalleryGroup;
end;

procedure TdxCustomGalleryGroups.SetGroup(AIndex: Integer; AValue: TdxCustomGalleryGroup);
begin
  inherited Items[AIndex] := AValue;
end;

function TdxCustomGalleryGroups.AddGalleryGroup: IdxGalleryGroup;
begin
  Result := Add;
end;

function TdxCustomGalleryGroups.GetDisplayName: string;
begin
  Result := 'Group';
end;

function TdxCustomGalleryGroups.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxCustomGalleryGroups.GetGalleryGroup(AIndex: Integer): IdxGalleryGroup;
begin
  Result := Groups[AIndex];
end;

function TdxCustomGalleryGroups.InsertGalleryGroup(AIndex: Integer): IdxGalleryGroup;
begin
  Result := Insert(AIndex) as TdxCustomGalleryGroup;
end;

{ TdxCustomGallery }

constructor TdxCustomGallery.Create(AOwner: TPersistent);
begin
  inherited;
  FGroups := GetGroupsClass.Create(GetParentComponent, GetGroupClass) as TdxGalleryGroups;
  FGroups.OnChange := GroupsChangeHandler;
end;

destructor TdxCustomGallery.Destroy;
begin
  FreeAndNil(FGroups);
  inherited;
end;

procedure TdxCustomGallery.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Groups.Count - 1 do
    if Groups[I].Owner = Root then Proc(Groups[I]);
end;

function TdxCustomGallery.GetGroupClass: TdxGalleryGroupClass;
begin
  Result := TdxGalleryGroup;
end;

function TdxCustomGallery.GetGroupsClass: TdxGalleryGroupsClass;
begin
  Result := TdxGalleryGroups;
end;

procedure TdxCustomGallery.CheckItem(AItem: TdxGalleryItem; AValue: Boolean);
begin
  if (AItem <> nil) and (AItem.Checked <> AValue) then
  begin
    AItem.FChecked := AValue;
    Changed(gctLight);
  end;
end;

procedure TdxCustomGallery.Changed(AType: TdxGalleryChangeType);
begin
  if Assigned(OnChange) then
    FOnChange(Self, AType);
end;

procedure TdxCustomGallery.ChangeScale(M, D: Integer);
begin
  Groups.ChangeScale(M, D);
end;

procedure TdxCustomGallery.DoClickItem(AItem: TdxGalleryItem);
begin
  if Assigned(OnItemClick) then
    FOnItemClick(Self, AItem);
end;

function TdxCustomGallery.GetInstance: TObject;
begin
  Result := Self;
end;

function TdxCustomGallery.GetParentComponent: TComponent;
begin
  Result := Owner as TComponent;
end;

function TdxCustomGallery.GetAllowedCustomizationActions: TdxGalleryCustomizationActions;
begin
  Result := [gcaChangeGroupCaption, gcaChangeItemCaption];
end;

function TdxCustomGallery.GetGalleryGroups: IdxGalleryGroups;
begin
  Result := GetGroups;
end;

procedure TdxCustomGallery.ClickItem(AItem: TdxGalleryItem);
var
  ACheckedItem: TdxGalleryItem;
begin
  if (AItem <> nil) and AItem.Enabled then
  begin
    ACheckedItem := GetCheckedItem;
    case ItemCheckMode of
      icmNone: ;
      icmSingleCheck:
        begin
          CheckItem(ACheckedItem, False);
          if ACheckedItem <> AItem then
            CheckItem(AItem, True);
        end;
      icmSingleRadio:
        begin
          CheckItem(ACheckedItem, False);
          CheckItem(AItem, True);
        end;
      icmMultiple:
        CheckItem(AItem, not AItem.FChecked);
    end;

    DoClickItem(AItem);
  end;
end;

procedure TdxCustomGallery.GetAllItems(AList: TList);
var
  I, J: Integer;
begin
  for I := 0 to Groups.Count - 1 do
    for J := 0 to Groups[I].Items.Count - 1 do
      AList.Add(Groups[I].Items[J]);
end;

function TdxCustomGallery.GetCheckedItem: TdxGalleryItem;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    GetCheckedItems(AList);
    if AList.Count > 0 then
      Result := TdxGalleryItem(AList[0])
    else
      Result := nil;
  finally
    AList.Free;
  end;
end;

function TdxCustomGallery.GetFirstItem: TdxGalleryItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Groups.Count - 1 do
    if Groups[I].Items.Count > 0 then
    begin
      Result := Groups[I].Items[0];
      Break;
    end;
end;

function TdxCustomGallery.GetFirstVisibleItem: TdxGalleryItem;
var
  AGroup: TdxGalleryGroup;
  I: Integer;
begin
  Result := nil;
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.Visible and (AGroup.Items.Count > 0) then
    begin
      Result := AGroup.Items[0];
      Break;
    end;
  end;
end;

procedure TdxCustomGallery.GetCheckedItems(AList: TList);
var
  I, J: Integer;
begin
  for I := 0 to Groups.Count - 1 do
    for J := 0 to Groups[I].Items.Count - 1 do
      if Groups[I].Items[J].Checked then
        AList.Add(Groups[I].Items[J]);
end;

procedure TdxCustomGallery.UncheckAll;
var
  I: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    GetCheckedItems(AList);
    for I := 0 to AList.Count - 1 do
      CheckItem(TdxGalleryItem(AList[I]), False)
  finally
    AList.Free;
  end;
end;

function TdxCustomGallery.FindItemByTag(ATag: TcxTag): TdxGalleryItem;
var
  I, J: Integer;
begin
  Result := nil;
  for I := 0 to Groups.Count - 1 do
    for J := 0 to Groups[I].Items.Count - 1 do
      if Groups[I].Items[J].Tag = ATag then
      begin
        Result := Groups[I].Items[J];
        Break;
      end;
end;

function TdxCustomGallery.GetItemCheckMode: TdxGalleryItemCheckMode;
begin
  Result := FItemCheckMode;
end;

procedure TdxCustomGallery.SetItemCheckMode(AValue: TdxGalleryItemCheckMode);
begin
  if FItemCheckMode <> AValue then
  begin
    FItemCheckMode := AValue;
    UncheckAll;
  end;
end;

function TdxCustomGallery.GetGroups: TdxGalleryGroups;
begin
  Result := FGroups;
end;

procedure TdxCustomGallery.SetGroups(AValue: TdxGalleryGroups);
begin
  FGroups.Assign(AValue);
end;

function TdxCustomGallery.GetOnChange: TdxGalleryChangeEvent;
begin
  Result := FOnchange;
end;

procedure TdxCustomGallery.SetOnChange(AValue: TdxGalleryChangeEvent);
begin
  FOnchange := AValue;
end;

function TdxCustomGallery.GetOnItemClick: TdxGalleryItemEvent;
begin
  Result := FOnItemClick;
end;

procedure TdxCustomGallery.SetOnItemClick(AValue: TdxGalleryItemEvent);
begin
  FOnItemClick := AValue;
end;

procedure TdxCustomGallery.GroupsChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  Changed(gctHard);
end;

initialization
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TdxGalleryItem, TControl);
  GroupDescendentsWith(TdxGalleryGroup, TControl);

end.
