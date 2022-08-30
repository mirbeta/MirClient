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

unit dxSkinChooserGallery;

{$I cxVer.inc}

interface

uses
  Windows, Classes, SysUtils, Graphics,
  dxCore, dxGallery, dxRibbonGallery, cxLookAndFeelPainters, dxSkinsCore, cxClasses, cxLookAndFeels;

type

  { TdxSkinChooserOptions }

  TdxSkinChooserGalleryOptions = class(TdxCustomRibbonGalleryOptions)
  protected
    function IsItemImagePositionStored: Boolean; override;
  public
    constructor Create(AOwner: TdxCustomRibbonGalleryItem); override;
  published
    property ColumnCount;
    property ItemAllowDeselect;
    property ItemHintSource;
    property ItemImagePosition;
    property ItemImageSize;
    property ShowItemHint stored False; // deprecated
    property SpaceAfterGroupHeader;
    property SpaceBetweenGroups;
    property SpaceBetweenItemCaptionAndDescription;
    property SpaceBetweenItemImageAndText;
    property SpaceBetweenItemsAndBorder;
    property SpaceBetweenItemsHorizontally;
    property SpaceBetweenItemsVertically;
  end;

  { TdxSkinChooserGalleryGroupItem }

  TdxSkinChooserGalleryGroupItem = class(TdxRibbonGalleryGroupItem)
  private
    FLookAndFeelStyle: TcxLookAndFeelStyle;
    FSkinName: string;
    FSkinResInstance: HINST;
    FSkinResName: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplyToLookAndFeel(ALookAndFeel: TcxLookAndFeel);
    procedure ApplyToRootLookAndFeel;
    //
    property GlyphInDropDown;
    property LookAndFeelStyle: TcxLookAndFeelStyle read FLookAndFeelStyle write FLookAndFeelStyle;
    property SkinName: string read FSkinName write FSkinName;
    property SkinResInstance: HINST read FSkinResInstance write FSkinResInstance;
    property SkinResName: string read FSkinResName write FSkinResName;
  end;

  { TdxSkinChooserGalleryItem }

  TdxSkinChooserGallerySkinChangedEvent = procedure (Sender: TObject; const ASkinName: string) of object;

  TdxSkinChooserGalleryItem = class(TdxCustomRibbonGalleryItem, IcxLookAndFeelPainterListener)
  private
    FSkinIconSize: TdxSkinIconSize;
    FSkinIconSizeInDropDown: TdxSkinIconSize;
    FVisibleLookAndFeelStyles: TcxLookAndFeelStyles;

    FOnPopulate: TNotifyEvent;
    FOnSkinChanged: TdxSkinChooserGallerySkinChangedEvent;

    function GetGalleryOptions: TdxSkinChooserGalleryOptions;
    function GetGroupIndex(const AName: string): Integer;
    function GetIsInternalSkin(ASkinDetails: TdxSkinDetails): Boolean;
    function GetSelectedGroupItem: TdxSkinChooserGalleryGroupItem;
    function GetSelectedSkinName: string;
    function IsSkinIconSizeInDropDownStored: Boolean;
    function IsSkinIconSizeStored: Boolean;
    procedure SetGalleryOptions(AValue: TdxSkinChooserGalleryOptions);
    procedure SetSelectedGroupItem(AValue: TdxSkinChooserGalleryGroupItem);
    procedure SetSelectedSkinName(const AValue: string);
    procedure SetSkinIconSize(AValue: TdxSkinIconSize);
    procedure SetSkinIconSizeInDropDown(AValue: TdxSkinIconSize);
    procedure SetVisibleLookAndFeelStyles(AValue: TcxLookAndFeelStyles);
  protected
    procedure DoGroupItemClick(AItem: TdxRibbonGalleryGroupItem); override;
    procedure DoSkinChanged(const ASkinName: string); virtual;
    function GetErrorCanPlaceText: string; override;
    function GetGroupItemClass: TdxGalleryItemClass; override;
    function GetGalleryOptionsClass: TCustomdxRibbonGalleryOptionsClass; override;
    class function GetNewCaption: string; override;
    procedure Loaded; override;
    // IcxLookAndFeelPainterListener
    procedure IcxLookAndFeelPainterListener.PainterAdded = PainterChanged;
    procedure IcxLookAndFeelPainterListener.PainterRemoved = PainterChanged;
    procedure PainterChanged(APainter: TcxCustomLookAndFeelPainter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddSkin(ASkinDetails: TdxSkinDetails): TdxSkinChooserGalleryGroupItem; overload;
    function AddSkin(const ASkinName, AGroupName: string): TdxSkinChooserGalleryGroupItem; overload;
    procedure AddSkinsFromFile(const AFileName: string);
    procedure AddSkinsFromResource(AInstance: HINST; const AResourceName: string);
    procedure AddSkinsFromResources(AInstance: HINST);
    procedure AddSkinsFromStream(AStream: TStream; const ASkinResName: string; ASkinResInstance: HINST);
    function FindSkin(const ASkinName: string; out AItem: TdxSkinChooserGalleryGroupItem): Boolean;
    procedure PopulateGallery;
    //
    property SelectedGroupItem: TdxSkinChooserGalleryGroupItem read GetSelectedGroupItem write SetSelectedGroupItem;
    property SelectedSkinName: string read GetSelectedSkinName write SetSelectedSkinName;
  published
    //TCustomdxBarSubItem
    property Glyph;
    property ImageIndex;
    property LargeGlyph;
    property LargeImageIndex;
    property ShowCaption default True;
    property OnClick;

    property GalleryOptions: TdxSkinChooserGalleryOptions read GetGalleryOptions write SetGalleryOptions;
    property GalleryInRibbonOptions;
    property GalleryInMenuOptions;
    property ItemLinks;
    property ItemOptions;
    property SkinIconSize: TdxSkinIconSize read FSkinIconSize write SetSkinIconSize stored IsSkinIconSizeStored;
    property SkinIconSizeInDropDown: TdxSkinIconSize read FSkinIconSizeInDropDown write SetSkinIconSizeInDropDown stored IsSkinIconSizeInDropDownStored;
    property VisibleLookAndFeelStyles: TcxLookAndFeelStyles read FVisibleLookAndFeelStyles write SetVisibleLookAndFeelStyles default [lfsSkin];

    property OnCloseUp;
    property OnHotTrackedItemChanged;
    property OnPopulate: TNotifyEvent read FOnPopulate write FOnPopulate;
    property OnPopup;
    property OnSkinChanged: TdxSkinChooserGallerySkinChangedEvent read FOnSkinChanged write FOnSkinChanged;
  end;

implementation

uses
  dxBar, cxGraphics, dxGDIPlusClasses, dxSkinsStrs, dxBarStrs, dxDPIAwareUtils;

function EnumResNameProc(hModule: HMODULE; lpszType: LPCTSTR;
  lpszName: LPTSTR; AData: TdxSkinChooserGalleryItem): Boolean; stdcall;
begin
  AData.AddSkinsFromResource(hModule, lpszName);
  Result := True;
end;

{ TdxSkinChooserGalleryOptions }

constructor TdxSkinChooserGalleryOptions.Create(AOwner: TdxCustomRibbonGalleryItem);
begin
  inherited Create(AOwner);
  ItemImagePosition := gipTop;
end;

function TdxSkinChooserGalleryOptions.IsItemImagePositionStored: Boolean;
begin
  Result := ItemImagePosition <> gipTop;
end;

{ TdxSkinChooserGalleryGroupItem }

constructor TdxSkinChooserGalleryGroupItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookAndFeelStyle := lfsSkin;
end;

procedure TdxSkinChooserGalleryGroupItem.ApplyToLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  ALookAndFeel.BeginUpdate;
  try
    case LookAndFeelStyle of
      lfsNative:
        ALookAndFeel.NativeStyle := True;
      lfsSkin:
        begin
          ALookAndFeel.SkinName := SkinName;
          ALookAndFeel.NativeStyle := False;
        end;
    else
      begin
        ALookAndFeel.NativeStyle := False;
        ALookAndFeel.SkinName := '';
        ALookAndFeel.Kind := cxLookAndFeelKindMap[LookAndFeelStyle];
      end;
    end;
  finally
    ALookAndFeel.EndUpdate;
  end;
end;

procedure TdxSkinChooserGalleryGroupItem.ApplyToRootLookAndFeel;
begin
  ApplyToLookAndFeel(RootLookAndFeel);
end;

{ TdxSkinChooserGalleryItem }

constructor TdxSkinChooserGalleryItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkinIconSize := sis16;
  FSkinIconSizeInDropDown := sis48;
  FVisibleLookAndFeelStyles := [lfsSkin];
  cxLookAndFeelPaintersManager.AddListener(Self);
  PopulateGallery;
end;

destructor TdxSkinChooserGalleryItem.Destroy;
begin
  cxLookAndFeelPaintersManager.RemoveListener(Self);
  inherited Destroy;
end;

function TdxSkinChooserGalleryItem.AddSkin(ASkinDetails: TdxSkinDetails): TdxSkinChooserGalleryGroupItem;
begin
  Result := AddSkin(ASkinDetails.Name, ASkinDetails.GroupName);
  Result.Caption := ASkinDetails.DisplayName;
  Result.GlyphInDropDown.Assign(ASkinDetails.Icons[SkinIconSizeInDropDown]);
  Result.Glyph.Assign(ASkinDetails.Icons[SkinIconSize]);
end;

function TdxSkinChooserGalleryItem.AddSkin(const ASkinName, AGroupName: string): TdxSkinChooserGalleryGroupItem;
begin
  Result := GalleryCategories[GetGroupIndex(AGroupName)].Items.Add as TdxSkinChooserGalleryGroupItem;
  Result.GlyphInDropDown.SourceDPI := dxDefaultDPI;
  Result.Glyph.SourceDPI := dxDefaultDPI;
  Result.SkinName := ASkinName;
end;

procedure TdxSkinChooserGalleryItem.AddSkinsFromFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    AddSkinsFromStream(AStream, AFileName, 0);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkinChooserGalleryItem.AddSkinsFromResource(AInstance: HINST; const AResourceName: string);
var
  AStream: TStream;
begin
  AStream := TResourceStream.Create(AInstance, AResourceName, sdxResourceType);
  try
    AddSkinsFromStream(AStream, AResourceName, AInstance);
  finally
    AStream.Free;
  end;
end;

procedure TdxSkinChooserGalleryItem.AddSkinsFromResources(AInstance: HINST);
begin
  if AInstance <> 0 then
  begin
    GalleryCategories.BeginUpdate;
    try
      Windows.EnumResourceNames(AInstance, PChar(sdxResourceType), @EnumResNameProc, LPARAM(Self));
    finally
      GalleryCategories.EndUpdate;
    end;
  end;
end;

procedure TdxSkinChooserGalleryItem.AddSkinsFromStream(
  AStream: TStream; const ASkinResName: string; ASkinResInstance: HINST);
var
  AItem: TdxSkinChooserGalleryGroupItem;
  AReader: TdxSkinBinaryReader;
  I: Integer;
begin
  GalleryCategories.BeginUpdate;
  try
    AReader := TdxSkinBinaryReader.Create(AStream);
    try
      for I := 0 to AReader.Count - 1 do
      begin
        if not GetIsInternalSkin(AReader.SkinDetails[I]) then
        begin
          AItem := AddSkin(AReader.SkinDetails[I]);
          AItem.SkinResInstance := ASkinResInstance;
          AItem.SkinResName := ASkinResName;
        end;
      end;
    finally
      AReader.Free;
    end;
  finally
    GalleryCategories.EndUpdate;
  end;
end;

function TdxSkinChooserGalleryItem.FindSkin(const ASkinName: string; out AItem: TdxSkinChooserGalleryGroupItem): Boolean;

  function FindSkinItemInGroup(AGroup: TdxRibbonGalleryGroup; out AItem: TdxSkinChooserGalleryGroupItem): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to AGroup.Items.Count - 1 do
    begin
      Result := SameText(ASkinName, TdxSkinChooserGalleryGroupItem(AGroup.Items[I]).SkinName);
      if Result then
      begin
        AItem := TdxSkinChooserGalleryGroupItem(AGroup.Items[I]);
        Break;
      end;
    end;
  end;

var
  I: Integer;
begin
  Result := False;
  for I := 0 to GalleryGroups.Count - 1 do
  begin
    Result := FindSkinItemInGroup(GalleryGroups.Items[I], AItem);
    if Result then
      Break;
  end;
end;

procedure TdxSkinChooserGalleryItem.PopulateGallery;
var
  APainter: TcxCustomLookAndFeelPainter;
  ASelectedSkinName: string;
  ASkinDetails: TdxSkinDetails;
  I: Integer;
begin
  if not IsDesigning then
  begin
    ASelectedSkinName := SelectedSkinName;
    try
      GalleryCategories.BeginUpdate;
      try
        GalleryCategories.Clear;
        if Assigned(OnPopulate) then
          OnPopulate(Self)
        else
          for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
          begin
            APainter := cxLookAndFeelPaintersManager[I];
            if (APainter.LookAndFeelStyle in VisibleLookAndFeelStyles) and not APainter.IsInternalPainter then
            begin
              if APainter.GetPainterDetails(ASkinDetails) then
                AddSkin(ASkinDetails).LookAndFeelStyle := APainter.LookAndFeelStyle;
            end;
          end;
      finally
        GalleryCategories.EndUpdate;
      end;
    finally
      if ASelectedSkinName <> '' then
        SelectedSkinName := ASelectedSkinName;
    end;
  end;
end;

procedure TdxSkinChooserGalleryItem.DoGroupItemClick(AItem: TdxRibbonGalleryGroupItem);
var
  ASkinName: string;
begin
  if AItem <> nil then
    ASkinName := (AItem as TdxSkinChooserGalleryGroupItem).SkinName
  else
    ASkinName := '';
  DoSkinChanged(ASkinName);
end;

procedure TdxSkinChooserGalleryItem.DoSkinChanged(const ASkinName: string);
begin
  if Assigned(OnSkinChanged) then
    OnSkinChanged(Self, ASkinName);
end;

function TdxSkinChooserGalleryItem.GetErrorCanPlaceText: string;
begin
  Result := cxGetResourceString(@dxSBAR_CANTPLACESKINCHOOSERGALLERY);
end;

function TdxSkinChooserGalleryItem.GetGroupItemClass: TdxGalleryItemClass;
begin
  Result := TdxSkinChooserGalleryGroupItem;
end;

function TdxSkinChooserGalleryItem.GetGalleryOptionsClass: TCustomdxRibbonGalleryOptionsClass;
begin
  Result := TdxSkinChooserGalleryOptions;
end;

class function TdxSkinChooserGalleryItem.GetNewCaption: string;
begin
  Result := 'New Skin Chooser';
end;

procedure TdxSkinChooserGalleryItem.Loaded;
begin
  inherited Loaded;
  if Assigned(OnPopulate) then
    PopulateGallery;
end;

procedure TdxSkinChooserGalleryItem.PainterChanged(APainter: TcxCustomLookAndFeelPainter);
begin
  PopulateGallery;
end;

function TdxSkinChooserGalleryItem.GetGalleryOptions: TdxSkinChooserGalleryOptions;
begin
  Result := inherited GalleryOptions as TdxSkinChooserGalleryOptions;
end;

function TdxSkinChooserGalleryItem.GetGroupIndex(const AName: string): Integer;
begin
  Result := 0;
  while (Result < GalleryCategories.Count) and (GalleryCategories[Result].Header.Caption <> AName) do
    Inc(Result);
  if Result = GalleryCategories.Count then
    with GalleryCategories.Add do
    begin
      Header.Visible := True;
      Header.Caption := AName;
    end;
end;

function TdxSkinChooserGalleryItem.GetIsInternalSkin(ASkinDetails: TdxSkinDetails): Boolean;
begin
  Result := ASkinDetails.GroupName = '';
end;

function TdxSkinChooserGalleryItem.GetSelectedGroupItem: TdxSkinChooserGalleryGroupItem;
begin
  Result := inherited SelectedGroupItem as TdxSkinChooserGalleryGroupItem;
end;

function TdxSkinChooserGalleryItem.GetSelectedSkinName: string;
begin
  if SelectedGroupItem <> nil then
    Result := SelectedGroupItem.SkinName
  else
    Result := '';
end;

function TdxSkinChooserGalleryItem.IsSkinIconSizeStored: Boolean;
begin
  Result := FSkinIconSize <> sis16;
end;

function TdxSkinChooserGalleryItem.IsSkinIconSizeInDropDownStored: Boolean;
begin
  Result := FSkinIconSizeInDropDown <> sis48;
end;

procedure TdxSkinChooserGalleryItem.SetGalleryOptions(AValue: TdxSkinChooserGalleryOptions);
begin
  GalleryOptions.Assign(AValue);
end;

procedure TdxSkinChooserGalleryItem.SetSelectedGroupItem(AValue: TdxSkinChooserGalleryGroupItem);
begin
  inherited SelectedGroupItem := AValue;
end;

procedure TdxSkinChooserGalleryItem.SetSelectedSkinName(const AValue: string);
var
  AItem: TdxSkinChooserGalleryGroupItem;
begin
  if FindSkin(AValue, AItem) then
    SelectedGroupItem := AItem
  else
    SelectedGroupItem := nil;
end;

procedure TdxSkinChooserGalleryItem.SetSkinIconSize(AValue: TdxSkinIconSize);
begin
  if AValue <> FSkinIconSize then
  begin
    FSkinIconSize := AValue;
    PopulateGallery;
  end;
end;

procedure TdxSkinChooserGalleryItem.SetSkinIconSizeInDropDown(AValue: TdxSkinIconSize);
begin
  if AValue <> FSkinIconSizeInDropDown then
  begin
    FSkinIconSizeInDropDown := AValue;
    PopulateGallery;
  end;
end;

procedure TdxSkinChooserGalleryItem.SetVisibleLookAndFeelStyles(AValue: TcxLookAndFeelStyles);
begin
  if FVisibleLookAndFeelStyles <> AValue then
  begin
    FVisibleLookAndFeelStyles := AValue;
    PopulateGallery;
  end;
end;

initialization
  dxBarRegisterItem(TdxSkinChooserGalleryItem, TdxRibbonGalleryControl, True);

finalization
  dxBarUnregisterItem(TdxSkinChooserGalleryItem);
end.
