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

unit dxUIGenerator;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Windows, Types, TypInfo, Menus, Classes, Generics.Defaults, Generics.Collections, Graphics, ImgList, ActnList,
  cxClasses, dxCoreClasses, dxCore, dxGDIPlusClasses, dxIconLibrary, dxActions;

type
  TdxUIGeneratorItemGroupPosition = (ugigpNone, ugigpStart, ugigpMember);
  TdxUIGeneratorItemPosition = (ugipBeginsNewRow, ugipBeginsNewColumn, ugipContinuesRow);
  TdxUIGeneratorItemViewLevel = (ugivlLargeIcon, ugivlSmallIcon, ugivlText);
  TdxUIGeneratorItemViewLevels = set of TdxUIGeneratorItemViewLevel;

const
  dxUIGeneratorItemDefaultViewLevels = [ugivlLargeIcon, ugivlSmallIcon, ugivlText];

type
  TdxUIGeneratorComponentInfo = class;
  TdxUIGeneratorCategoryInfo = class;
  TdxUIGeneratorCommandInfo = class;
  TdxUIGeneratorGalleryInfo = class;

  { TdxUIGeneratorCommands }

  TdxUIGeneratorCommandsEnumProc = reference to procedure (AInfo: TdxUIGeneratorCommandInfo);
  TdxUIGeneratorCommands = class(TcxIUnknownObject)
  strict private
    FCommands: TObjectList<TdxUIGeneratorCommandInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AAction: TdxBasicActionClass;
      AViewLevels: TdxUIGeneratorItemViewLevels = dxUIGeneratorItemDefaultViewLevels;
      AGroupPosition: TdxUIGeneratorItemGroupPosition = ugigpNone;
      APosition: TdxUIGeneratorItemPosition = ugipBeginsNewRow;
      ABeginGroup: Boolean = False): TdxUIGeneratorCommandInfo; overload;
    function Add(const AImageFileName, ACaption: string;
      AViewLevels: TdxUIGeneratorItemViewLevels = dxUIGeneratorItemDefaultViewLevels;
      AGroupPosition: TdxUIGeneratorItemGroupPosition = ugigpNone;
      APosition: TdxUIGeneratorItemPosition = ugipBeginsNewRow;
      ABeginGroup: Boolean = False): TdxUIGeneratorCommandInfo; overload;
    function Add(AGalleryClientAction: TdxBasicActionClass; AShowItemCaption, AShowItemDescription: Boolean;
      ACollapsed: Boolean = False; AResizable: Boolean = True; AColumnCount: Integer = 5;
      AItemDescriptionRowCount: Integer = 2; AUseLargeImages: Boolean = True;
      AItemTextAlignVert: TcxAlignmentVert = vaTop;
      AViewLevels: TdxUIGeneratorItemViewLevels = dxUIGeneratorItemDefaultViewLevels;
      AGroupPosition: TdxUIGeneratorItemGroupPosition = ugigpNone;
      APosition: TdxUIGeneratorItemPosition = ugipBeginsNewRow;
      ABeginGroup: Boolean = False): TdxUIGeneratorGalleryInfo; overload;
    procedure Enum(AProc: TdxUIGeneratorCommandsEnumProc);

    property Commands: TObjectList<TdxUIGeneratorCommandInfo> read FCommands;
  end;

  { TdxUIGeneratorCommandInfo }

  TdxUIGeneratorCommandInfo = class(TdxUIGeneratorCommands)
  public
    Action: TdxBasicActionClass;
    BeginGroup: Boolean;
    Caption: string;
    Parent: TObject;
    GroupPosition: TdxUIGeneratorItemGroupPosition;
    ImageFileName: string;
    Position: TdxUIGeneratorItemPosition;
    ViewLevels: TdxUIGeneratorItemViewLevels;

    function Category: TdxUIGeneratorCategoryInfo;
  end;

  { TdxUIGeneratorCategoryInfo }

  TdxUIGeneratorCategoryInfo = class(TdxUIGeneratorCommands)
  strict private
    FBarGlyphFileName: string;
    FBarName: string;
    FOwner: TdxUIGeneratorComponentInfo;
    FRowIndex: Integer;
    FTabName: string;
  public
    constructor Create(AOwner: TdxUIGeneratorComponentInfo;
      const ATabName, ABarName, ABarGlyphFileName: string; ARowIndex: Integer = -1);
    function ToString: string; override;
    //
    property BarGlyphFileName: string read FBarGlyphFileName;
    property BarName: string read FBarName;
    property Owner: TdxUIGeneratorComponentInfo read FOwner;
    property RowIndex: Integer read FRowIndex;
    property TabName: string read FTabName;
  end;

  { TdxUIGeneratorGalleryGroupItemInfo }

  TdxUIGeneratorGalleryGroupItemInfo = class
  public
    ActionIndex: Variant;
    Caption: string;
    Description: string;
    ExternalImageIndex: Integer;
    GroupCaption: string;
    ImageFileName: string;
  end;

  { TdxUIGeneratorGalleryInfo }

  TdxUIGeneratorGalleryInfo = class(TdxUIGeneratorCommandInfo, IdxActionGalleryInfo)
  strict private
    FGroupItems: TObjectList<TdxUIGeneratorGalleryGroupItemInfo>;
  public
    Collapsed: Boolean;
    ColumnCount: Integer;
    ExternalImageList: TCustomImageList;
    GalleryGroups: TObject{TdxCustomGalleryGroups};
    ItemDescriptionRowCount: Integer;
    ItemTextAlignVert: TcxAlignmentVert;
    Resizable: Boolean;
    ShowItemCaption: Boolean;
    ShowItemDescription: Boolean;
    UseLargeImages: Boolean;

    constructor Create;
    destructor Destroy; override;

    // IdxActionGalleryInfo
    procedure Add(AActionIndex: Variant; const AGroupCaption, ACaption: string;
      const ADescription: string = ''; const AImageFileName: string = ''; AExternalImageIndex: Integer = -1); overload;
    function GetGroups: TObject;
    procedure SetExternalImageList(AImageList: TCustomImageList);

    property GroupItems: TObjectList<TdxUIGeneratorGalleryGroupItemInfo> read FGroupItems;
  end;

  { TdxUIGeneratorComponentInfo }

  TdxUIGeneratorComponentInfo = class
  strict private
    FCategories: TObjectList<TdxUIGeneratorCategoryInfo>;
    FDisplayName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const ATabName, ABarName: string; ARowIndex: Integer = -1): TdxUIGeneratorCategoryInfo; overload;
    function Add(const ATabName, ABarName, ABarGlyphFileName: string; ARowIndex: Integer = -1): TdxUIGeneratorCategoryInfo; overload;
    //
    property Categories: TObjectList<TdxUIGeneratorCategoryInfo> read FCategories;
    property DisplayName: string read FDisplayName write FDisplayName;
  end;

  { TdxUIGeneratorComponentInfoList }

  TdxUIGeneratorComponentInfoList = class(TList<TdxUIGeneratorComponentInfo>);

  { TdxUIGeneratorAdapter }

  TdxUIGeneratorAdapterClass = class of TdxUIGeneratorAdapter;
  TdxUIGeneratorAdapter = class abstract
  public const
    DefaultImageSet = 'Images';
  strict private
    FActionList: TCustomActionList;
    FImageSet: string;
    FInitialized: Boolean;
    FLargeImageList: TCustomImageList;
    FSmallImageList: TCustomImageList;
    FTarget: TComponent;

    function GetOwnerForm: TComponent;
  protected
    procedure AddImage(AAction: TAction; const AImageFileName: string; out ASmallImageIndex, ALargeImageIndex: Integer); overload;
    function AddImage(ATargetImageList: TCustomImageList; const AImageFileName: string): Integer; overload;
    function GetAction(AItem: TdxUIGeneratorCommandInfo): TAction;
    procedure PlaceCommand(AAction: TAction; AItem: TdxUIGeneratorCommandInfo); virtual; abstract;

    procedure CheckInitialized;
    procedure Initialize; virtual;

    property OwnerForm: TComponent read GetOwnerForm;
    property Target: TComponent read FTarget;
  public
    constructor Create(ATarget: TComponent); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Generate(ACategory: TdxUIGeneratorCategoryInfo); overload;
    procedure Generate(AComponent: TdxUIGeneratorComponentInfo); overload;
    procedure Generate(AComponent: TdxUIGeneratorComponentInfoList); overload;
    procedure Generate(AItem: TdxUIGeneratorCommandInfo); overload;

    // Target
    class function CanCreateComponent(AOwner: TComponent): Boolean; virtual;
    class function CreateComponent(AOwner: TComponent): TComponent; virtual;
    class function GetComponentClass: TComponentClass; virtual;

    property ActionList: TCustomActionList read FActionList write FActionList;
    property ImageSet: string read FImageSet write FImageSet;
    property LargeImageList: TCustomImageList read FLargeImageList write FLargeImageList;
    property SmallImageList: TCustomImageList read FSmallImageList write FSmallImageList;
  end;

  { TdxUIGeneratorHelper }

  TdxUIGeneratorHelper = class
  public
    class function AddImage(ATargetImageList: TCustomImageList; const AImageFileName, AImageSet: string): Integer;
    class function CreateComponent(AOwner: TComponent; AClass: TComponentClass): TComponent; static;
    class function CreateImageList(AOwner: TComponent; ASize: Integer): TCustomImageList; static;
    class function FindComponentByClass(AParent: TComponent; AClass: TComponentClass): TComponent; static;
    class function GenerateCategoryName(ACategory: TdxUIGeneratorCategoryInfo): string;
    class function GenerateName(AComponent: TComponent; const AUseCaption: Boolean = True): string;
    class procedure LoadImage(ATarget: TdxSmartImage; const AImageFileName, AImageSet: string); overload;
    class function LoadImage(const AImageFileName, AImageSet: string): TdxSmartImage; overload;
  end;

  { TdxUIGenerator }

  TdxUIGenerator = class
  strict private
    class var FAdapters: TList<TdxUIGeneratorAdapterClass>;
    class var FComponents: TObjectDictionary<TComponentClass, TdxUIGeneratorComponentInfo>;

    class function GetAdapterCount: Integer; static;
    class function GetAdapterItem(Index: Integer): TdxUIGeneratorAdapterClass; static;
  protected
    class procedure FreeResources;
  public
    class function GetAdapter(ATargetComponent: TComponent; out AAdapter: TdxUIGeneratorAdapterClass): Boolean;
    class function GetComponentInfo(ASourceComponent: TComponent; out AInfo: TdxUIGeneratorComponentInfo): Boolean; overload;
    class function GetComponentInfo(ASourceComponent: TComponent; out AInfoList: TdxUIGeneratorComponentInfoList): Boolean; overload;
    class function GetComponentInfo(ASourceComponentClass: TComponentClass; out AInfo: TdxUIGeneratorComponentInfo): Boolean; overload;
    class function GetComponentInfo(ASourceComponentClass: TComponentClass; out AInfoList: TdxUIGeneratorComponentInfoList): Boolean; overload;

    class procedure RegisterAdapter(AAdapterClass: TdxUIGeneratorAdapterClass);
    class function RegisterComponent(AComponentClass: TComponentClass; const ADisplayName: string): TdxUIGeneratorComponentInfo;
    class procedure UnregisterAdapter(AAdapterClass: TdxUIGeneratorAdapterClass);
    class procedure UnregisterComponent(AComponentClass: TComponentClass);
    //
    class property Adapters[Index: Integer]: TdxUIGeneratorAdapterClass read GetAdapterItem;
    class property AdapterCount: Integer read GetAdapterCount;
  end;

implementation

uses
  SysUtils, Math, cxControls, cxGraphics;

const
  sdxActionListNotAssigned = 'ActionList is not assigned';
  sdxAdapterNotFound = 'Adapter for "%s" was not found';

type
  TCustomActionListAccess = class(TCustomActionList);
  TdxBasicActionAccess = class(TdxBasicAction);

{ TdxUIGeneratorCategoryInfo }

constructor TdxUIGeneratorCategoryInfo.Create(AOwner: TdxUIGeneratorComponentInfo;
  const ATabName, ABarName, ABarGlyphFileName: string; ARowIndex: Integer = -1);
begin
  inherited Create;
  FOwner := AOwner;
  FBarGlyphFileName := ABarGlyphFileName;
  FRowIndex := ARowIndex;
  FTabName := ATabName;
  FBarName := ABarName;
end;

function TdxUIGeneratorCategoryInfo.ToString: string;
begin
  Result := TabName + ' - ' + BarName;
end;

{ TdxUIGeneratorGalleryInfo }

constructor TdxUIGeneratorGalleryInfo.Create;
begin
  inherited Create;
  FGroupItems := TObjectList<TdxUIGeneratorGalleryGroupItemInfo>.Create;
end;

destructor TdxUIGeneratorGalleryInfo.Destroy;
begin
  FreeAndNil(FGroupItems);
  inherited Destroy;
end;

procedure TdxUIGeneratorGalleryInfo.Add(AActionIndex: Variant; const AGroupCaption, ACaption: string;
  const ADescription: string = ''; const AImageFileName: string = ''; AExternalImageIndex: Integer = -1);
var
  AInfo: TdxUIGeneratorGalleryGroupItemInfo;
begin
  AInfo := TdxUIGeneratorGalleryGroupItemInfo.Create;
  AInfo.GroupCaption := AGroupCaption;
  AInfo.Caption := ACaption;
  AInfo.Description := ADescription;
  AInfo.ActionIndex := AActionIndex;
  AInfo.ImageFileName := AImageFileName;
  AInfo.ExternalImageIndex := AExternalImageIndex;
  FGroupItems.Add(AInfo);
end;

function TdxUIGeneratorGalleryInfo.GetGroups: TObject;
begin
  Result := GalleryGroups;
end;

procedure TdxUIGeneratorGalleryInfo.SetExternalImageList(AImageList: TCustomImageList);
begin
  ExternalImageList := AImageList;
end;

{ TdxUIGeneratorCommands }

constructor TdxUIGeneratorCommands.Create;
begin
  inherited Create;
  FCommands := TObjectList<TdxUIGeneratorCommandInfo>.Create;
end;

destructor TdxUIGeneratorCommands.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
end;

function TdxUIGeneratorCommands.Add(AAction: TdxBasicActionClass;
  AViewLevels: TdxUIGeneratorItemViewLevels = dxUIGeneratorItemDefaultViewLevels;
  AGroupPosition: TdxUIGeneratorItemGroupPosition = ugigpNone;
  APosition: TdxUIGeneratorItemPosition = ugipBeginsNewRow;
  ABeginGroup: Boolean = False): TdxUIGeneratorCommandInfo;
begin
  Result := TdxUIGeneratorCommandInfo.Create;
  Result.Action := AAction;
  Result.BeginGroup := ABeginGroup;
  Result.GroupPosition := AGroupPosition;
  Result.Position := APosition;
  Result.Parent := Self;
  Result.ViewLevels := AViewLevels;

  FCommands.Add(Result);
end;

function TdxUIGeneratorCommands.Add(const AImageFileName, ACaption: string;
  AViewLevels: TdxUIGeneratorItemViewLevels = dxUIGeneratorItemDefaultViewLevels;
  AGroupPosition: TdxUIGeneratorItemGroupPosition = ugigpNone;
  APosition: TdxUIGeneratorItemPosition = ugipBeginsNewRow;
  ABeginGroup: Boolean = False): TdxUIGeneratorCommandInfo;
begin
  Result := Add(nil, AViewLevels, AGroupPosition, APosition, ABeginGroup);
  Result.ImageFileName := AImageFileName;
  Result.Caption := ACaption;
end;

function TdxUIGeneratorCommands.Add(AGalleryClientAction: TdxBasicActionClass;
  AShowItemCaption, AShowItemDescription: Boolean;
  ACollapsed: Boolean = False; AResizable: Boolean = True; AColumnCount: Integer = 5;
  AItemDescriptionRowCount: Integer = 2; AUseLargeImages: Boolean = True;
  AItemTextAlignVert: TcxAlignmentVert = vaTop;
  AViewLevels: TdxUIGeneratorItemViewLevels = dxUIGeneratorItemDefaultViewLevels;
  AGroupPosition: TdxUIGeneratorItemGroupPosition = ugigpNone; APosition: TdxUIGeneratorItemPosition = ugipBeginsNewRow;
  ABeginGroup: Boolean = False): TdxUIGeneratorGalleryInfo;
begin
  Result := TdxUIGeneratorGalleryInfo.Create;
  Result.Action := AGalleryClientAction;
  Result.ShowItemCaption := AShowItemCaption;
  Result.ShowItemDescription := AShowItemDescription;
  Result.Collapsed := ACollapsed;
  Result.Resizable := AResizable;
  Result.ColumnCount := AColumnCount;
  Result.ItemDescriptionRowCount := AItemDescriptionRowCount;
  Result.UseLargeImages := AUseLargeImages;
  Result.ItemTextAlignVert := AItemTextAlignVert;
  Result.BeginGroup := ABeginGroup;
  Result.GroupPosition := AGroupPosition;
  Result.Position := APosition;
  Result.Parent := Self;
  Result.ViewLevels := AViewLevels;

  FCommands.Add(Result);
end;

procedure TdxUIGeneratorCommands.Enum(AProc: TdxUIGeneratorCommandsEnumProc);
var
  ACommand: TdxUIGeneratorCommandInfo;
  I: Integer;
begin
  for I := 0 to FCommands.Count - 1 do
  begin
    ACommand := FCommands[I];
    AProc(ACommand);
    ACommand.Enum(AProc);
  end;
end;

{ TdxUIGeneratorCommandInfo }

function TdxUIGeneratorCommandInfo.Category: TdxUIGeneratorCategoryInfo;
begin
  if Parent is TdxUIGeneratorCategoryInfo then
    Result := TdxUIGeneratorCategoryInfo(Parent)
  else
    if Parent is TdxUIGeneratorCommandInfo then
      Result := TdxUIGeneratorCommandInfo(Parent).Category
    else
      Result := nil;
end;

{ TdxUIGeneratorComponentInfo }

constructor TdxUIGeneratorComponentInfo.Create;
begin
  inherited Create;
  FCategories := TObjectList<TdxUIGeneratorCategoryInfo>.Create;
end;

destructor TdxUIGeneratorComponentInfo.Destroy;
begin
  FreeAndNil(FCategories);
  inherited Destroy;
end;

function TdxUIGeneratorComponentInfo.Add(const ATabName, ABarName: string; ARowIndex: Integer = -1): TdxUIGeneratorCategoryInfo;
begin
  Result := Add(ATabName, ABarName, '', ARowIndex);
end;

function TdxUIGeneratorComponentInfo.Add(const ATabName, ABarName, ABarGlyphFileName: string; ARowIndex: Integer = -1): TdxUIGeneratorCategoryInfo;
begin
  Result := TdxUIGeneratorCategoryInfo.Create(Self, ATabName, ABarName, ABarGlyphFileName, ARowIndex);
  FCategories.Add(Result);
end;

{ TdxUIGeneratorAdapter }

constructor TdxUIGeneratorAdapter.Create(ATarget: TComponent);
begin
  inherited Create;
  FTarget := ATarget;
  FImageSet := DefaultImageSet;
end;

procedure TdxUIGeneratorAdapter.BeginUpdate;
begin
  CheckInitialized;
  if SmallImageList <> nil then
    SmallImageList.BeginUpdate;
  if LargeImageList <> nil then
    LargeImageList.BeginUpdate;
end;

procedure TdxUIGeneratorAdapter.EndUpdate;
begin
  if SmallImageList <> nil then
    SmallImageList.EndUpdate;
  if LargeImageList <> nil then
    LargeImageList.EndUpdate;
end;

procedure TdxUIGeneratorAdapter.Generate(ACategory: TdxUIGeneratorCategoryInfo);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ACategory.Commands.Count - 1 do
      Generate(ACategory.Commands[I]);
  finally
    EndUpdate;
  end;
end;

procedure TdxUIGeneratorAdapter.Generate(AComponent: TdxUIGeneratorComponentInfo);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AComponent.Categories.Count - 1 do
      Generate(AComponent.Categories[I]);
  finally
    EndUpdate;
  end;
end;

procedure TdxUIGeneratorAdapter.Generate(AComponent: TdxUIGeneratorComponentInfoList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AComponent.Count - 1 do
      Generate(AComponent[I]);
  finally
    EndUpdate;
  end;
end;

procedure TdxUIGeneratorAdapter.Generate(AItem: TdxUIGeneratorCommandInfo);
begin
  CheckInitialized;
  if ActionList = nil then
    raise EdxException.Create(sdxActionListNotAssigned);
  PlaceCommand(GetAction(AItem), AItem);
end;

class function TdxUIGeneratorAdapter.CanCreateComponent(AOwner: TComponent): Boolean;
begin
  Result := TdxUIGeneratorHelper.FindComponentByClass(AOwner, GetComponentClass) = nil;
end;

class function TdxUIGeneratorAdapter.CreateComponent(AOwner: TComponent): TComponent;
begin
  Result := TdxUIGeneratorHelper.CreateComponent(AOwner, GetComponentClass);
end;

procedure TdxUIGeneratorAdapter.AddImage(AAction: TAction;
  const AImageFileName: string; out ASmallImageIndex, ALargeImageIndex: Integer);
begin
  if (AAction <> nil) and (ActionList.Images = SmallImageList) then
    ASmallImageIndex := AAction.ImageIndex
  else
    ASmallImageIndex := AddImage(SmallImageList, AImageFileName);

  if (AAction <> nil) and (ActionList.Images = LargeImageList) then
    ALargeImageIndex := AAction.ImageIndex
  else
    if SmallImageList = LargeImageList then
      ALargeImageIndex := ASmallImageIndex
    else
      ALargeImageIndex := AddImage(LargeImageList, AImageFileName)
end;

function TdxUIGeneratorAdapter.AddImage(ATargetImageList: TCustomImageList; const AImageFileName: string): Integer;
begin
  Result := TdxUIGeneratorHelper.AddImage(ATargetImageList, AImageFileName, ImageSet);
end;

function TdxUIGeneratorAdapter.GetAction(AItem: TdxUIGeneratorCommandInfo): TAction;
begin
  Result := nil;
  if AItem.Action <> nil then
  begin
    Result := TdxUIGeneratorHelper.FindComponentByClass(OwnerForm, AItem.Action) as TAction;
    if Result = nil then
    begin
      Result := AItem.Action.Create(OwnerForm);
      Result.Name := TdxUIGeneratorHelper.GenerateName(Result, False);
      if AItem.ImageFileName = '' then
        AItem.ImageFileName := TdxBasicActionAccess(Result).FDefaultImageNameInIconLibrary;
      Result.ImageIndex := AddImage(ActionList.Images, AItem.ImageFileName);
      Result.Category := TdxUIGeneratorHelper.GenerateCategoryName(AItem.Category);
      TCustomActionListAccess(ActionList).AddAction(Result);
    end;
  end;
end;

class function TdxUIGeneratorAdapter.GetComponentClass: TComponentClass;
begin
  raise EInvalidOperation.Create(ClassName); // for CBuilder
end;

procedure TdxUIGeneratorAdapter.CheckInitialized;
begin
  if not FInitialized then
  begin
    FInitialized := True;
    Initialize;
  end;
end;

procedure TdxUIGeneratorAdapter.Initialize;
begin
  if ActionList = nil then
    ActionList := TActionList(TdxUIGeneratorHelper.CreateComponent(OwnerForm, TActionList));
end;

function TdxUIGeneratorAdapter.GetOwnerForm: TComponent;
begin
  Result := Target.Owner;
end;

{ TdxUIGeneratorHelper }

class function TdxUIGeneratorHelper.AddImage(ATargetImageList: TCustomImageList; const AImageFileName, AImageSet: string): Integer;
var
  AImage: TdxSmartImage;
  ABitmap: TBitmap;
begin
  Result := -1;
  if (ATargetImageList <> nil) and (AImageFileName <> '') then
  begin
    AImage := LoadImage(ChangeFileExt(AImageFileName, Format('_%dx%d%s', [
      ATargetImageList.Width, ATargetImageList.Height, ExtractFileExt(AImageFileName)])), AImageSet);
    if AImage <> nil then
    try
      if ATargetImageList is TcxImageList then
        Result := TcxImageList(ATargetImageList).Add(AImage, nil)
      else
      begin
        ABitmap := AImage.GetAsBitmap;
        try
          Result := ATargetImageList.Add(ABitmap, nil);
        finally
          ABitmap.Free;
        end;
      end;
    finally
      AImage.Free;
    end;
  end;
end;

class function TdxUIGeneratorHelper.CreateComponent(AOwner: TComponent; AClass: TComponentClass): TComponent;
begin
  Result := AClass.Create(AOwner);
  Result.Name := GenerateName(Result);
  Result.DesignInfo := MakeLong(150, 150);
end;

class function TdxUIGeneratorHelper.CreateImageList(AOwner: TComponent; ASize: Integer): TCustomImageList;
begin
  Result := TcxImageList(CreateComponent(AOwner, TcxImageList));
  Result.Width := ASize;
  Result.Height := ASize;
end;

class function TdxUIGeneratorHelper.FindComponentByClass(AParent: TComponent; AClass: TComponentClass): TComponent;
var
  I: Integer;
begin
  Result := nil;
  if AParent <> nil then
    for I := 0 to AParent.ComponentCount - 1 do
    begin
      if AParent.Components[I].ClassType = AClass then
        Exit(AParent.Components[I]);
    end;
end;

class function TdxUIGeneratorHelper.GenerateCategoryName(ACategory: TdxUIGeneratorCategoryInfo): string;
begin
  Result := ACategory.Owner.DisplayName + '.' + ACategory.TabName + '.' + ACategory.BarName;
end;

class function TdxUIGeneratorHelper.GenerateName(AComponent: TComponent; const AUseCaption: Boolean = True): string;
var
  AInfo: PPropInfo;
  ASuffix: string;
begin
  ASuffix := '';
  if AUseCaption then
  begin
    AInfo := GetPropInfo(AComponent, 'Caption');
    if AInfo <> nil then
      ASuffix := GetStrProp(AComponent, AInfo)
  end;
  Result := CreateUniqueName(AComponent.Owner, nil, AComponent, '',
    Copy(AComponent.ClassName + ASuffix, 2, MaxInt), Ord(AUseCaption and (ASuffix = '')));
end;

class procedure TdxUIGeneratorHelper.LoadImage(ATarget: TdxSmartImage; const AImageFileName, AImageSet: string);
var
  ALoadedBitmap: TdxSmartImage;
begin
  ALoadedBitmap := LoadImage(AImageFileName, AImageSet);
  try
    ATarget.Assign(ALoadedBitmap);
  finally
    ALoadedBitmap.Free;
  end;
end;

class function TdxUIGeneratorHelper.LoadImage(const AImageFileName, AImageSet: string): TdxSmartImage;
var
  AFileName: string;
begin
  Result := nil;
  if AImageFileName <> '' then
  begin
    AFileName := dxGetIconLibraryPath + AImageSet + PathDelim + AImageFileName;
    if FileExists(AFileName) then
    begin
      Result := TdxSmartImage.Create;
      Result.LoadFromFile(AFileName);
    end;
  end;
end;

{ TdxUIGenerator }

class function TdxUIGenerator.GetAdapter(ATargetComponent: TComponent; out AAdapter: TdxUIGeneratorAdapterClass): Boolean;
var
  AClass: TComponentClass;
  I: Integer;
begin
  Result := False;
  AClass := TComponentClass(ATargetComponent.ClassType);
  for I := 0 to AdapterCount - 1 do
  begin
    Result := Adapters[I].GetComponentClass = AClass;
    if Result then
    begin
      AAdapter := Adapters[I];
      Break;
    end;
  end;
end;

class function TdxUIGenerator.GetComponentInfo(ASourceComponent: TComponent; out AInfo: TdxUIGeneratorComponentInfo): Boolean;
begin
  Result := GetComponentInfo(TComponentClass(ASourceComponent.ClassType), AInfo);
end;

class function TdxUIGenerator.GetComponentInfo(ASourceComponent: TComponent; out AInfoList: TdxUIGeneratorComponentInfoList): Boolean;
begin
  Result := GetComponentInfo(TComponentClass(ASourceComponent.ClassType), AInfoList);
end;

class function TdxUIGenerator.GetComponentInfo(ASourceComponentClass: TComponentClass; out AInfo: TdxUIGeneratorComponentInfo): Boolean;
var
  AList: TdxUIGeneratorComponentInfoList;
begin
  Result := GetComponentInfo(ASourceComponentClass, AList);
  if Result then
  try
    AInfo := AList.Last;
  finally
    AList.Free;
  end;
end;

class function TdxUIGenerator.GetComponentInfo(ASourceComponentClass: TComponentClass; out AInfoList: TdxUIGeneratorComponentInfoList): Boolean;
var
  AClassParent: TClass;
  AInfo: TdxUIGeneratorComponentInfo;
begin
  Result := False;
  if FComponents <> nil then
  begin
    AInfoList := TdxUIGeneratorComponentInfoList.Create;
    while ASourceComponentClass <> nil do
    begin
      if FComponents.TryGetValue(ASourceComponentClass, AInfo) then
        AInfoList.Insert(0, AInfo);

      AClassParent := ASourceComponentClass.ClassParent;
      if (AClassParent <> nil) and AClassParent.InheritsFrom(TComponent) then
        ASourceComponentClass := TComponentClass(AClassParent)
      else
        ASourceComponentClass := nil;
    end;
    Result := AInfoList.Count > 0;
    if not Result then
      FreeAndNil(AInfoList);
  end;
end;

class procedure TdxUIGenerator.RegisterAdapter(AAdapterClass: TdxUIGeneratorAdapterClass);
begin
  if FAdapters = nil then
    FAdapters := TList<TdxUIGeneratorAdapterClass>.Create;
  FAdapters.Add(AAdapterClass);
end;

class function TdxUIGenerator.RegisterComponent(AComponentClass: TComponentClass; const ADisplayName: string): TdxUIGeneratorComponentInfo;
begin
  Result := TdxUIGeneratorComponentInfo.Create;
  Result.DisplayName := ADisplayName;
  if FComponents = nil then
    FComponents := TObjectDictionary<TComponentClass, TdxUIGeneratorComponentInfo>.Create([doOwnsValues]);
  FComponents.AddOrSetValue(AComponentClass, Result);
end;

class procedure TdxUIGenerator.UnregisterAdapter(AAdapterClass: TdxUIGeneratorAdapterClass);
begin
  if FAdapters <> nil then
  begin
    FAdapters.Remove(AAdapterClass);
    if FAdapters.Count = 0 then
      FreeAndNil(FAdapters);
  end;
end;

class procedure TdxUIGenerator.UnregisterComponent(AComponentClass: TComponentClass);
begin
  if FComponents <> nil then
  begin
    FComponents.Remove(AComponentClass);
    if FComponents.Count = 0 then
      FreeAndNil(FComponents);
  end;
end;

class function TdxUIGenerator.GetAdapterCount: Integer;
begin
  if FAdapters <> nil then
    Result := FAdapters.Count
  else
    Result := 0;
end;

class function TdxUIGenerator.GetAdapterItem(Index: Integer): TdxUIGeneratorAdapterClass;
begin
  Result := FAdapters[Index];
end;

class procedure TdxUIGenerator.FreeResources;
begin
  FreeAndNil(FComponents);
  FreeAndNil(FAdapters);
end;

initialization

finalization
  TdxUIGenerator.FreeResources;
end.
