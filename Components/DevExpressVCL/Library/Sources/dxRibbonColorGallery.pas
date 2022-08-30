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

unit dxRibbonColorGallery;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, dxBar, Classes, cxClasses, dxCoreClasses, dxCore, Graphics, cxGraphics, dxRibbonGallery, dxActions,
  dxCoreGraphics, Contnrs, cxGeometry, dxGDIPlusClasses;

const
  dxRibbonColorGalleryAccentColorSchemeSize = 5;
  dxRibbonColorGalleryColorMapSize = 10;

type
  TdxRibbonColorGalleryColorMap = array [0..dxRibbonColorGalleryColorMapSize - 1] of TColor;
  TdxRibbonColorGalleryAccentColorScheme = array [0..dxRibbonColorGalleryAccentColorSchemeSize - 1] of TdxRibbonColorGalleryColorMap;

  TdxRibbonColorGalleryTheme = (rcptDefault, rcptTheme1, rcptTheme2, rcptTheme3, rcptTheme4, rcptTheme5);

  { TdxRibbonColorGalleryItem }

  TdxRibbonColorGalleryItem = class(TCustomdxBarContainerItem)
  strict private const
    GlyphSize = 16;
  strict private
    FAutoMenuItemColor: TColor;
    FColor: TColor;
    FCustomColors: TStrings;
    FScaleFactor: TdxScaleFactor;
    FShowItemBorders: Boolean;
    FTheme: TdxRibbonColorGalleryTheme;

    FInternalItems: TComponentList;

    FAccentColorsGroup: TdxRibbonGalleryGroup;
    FAutoColorButton: TdxBarButton;
    FColorGallery: TdxRibbonGalleryItem;
    FCustomColorsGroup: TdxRibbonGalleryGroup;
    FMoreColorsButton: TdxBarButton;
    FStandardColorsGroup: TdxRibbonGalleryGroup;
    FThemeColorsGroup: TdxRibbonGalleryGroup;

    FOnColorChanged: TNotifyEvent;

    procedure MoreColorsClickHandler(Sender: TObject);
    procedure SelectAutoColorHandler(Sender: TObject);
    procedure SelectColorHandler(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
    procedure SetAutoMenuItemColor(AValue: TColor);
    procedure SetColor(AValue: TColor);
    procedure SetCustomColors(AValue: TStrings);
    procedure SetShowItemBorders(AValue: Boolean);
  protected
    procedure AddColorItem(AGalleryGroup: TdxRibbonGalleryGroup; AColor: TColor);
    procedure AddColorRow(AGalleryGroup: TdxRibbonGalleryGroup; AColorMap: TdxRibbonColorGalleryColorMap);
    procedure AddCustomColor(AColor: TdxAlphaColor);
    procedure AssignColorBitmap(ATarget: TdxSmartGlyph; AColor: TColor);
    procedure BuildAccentColorScheme(ASource: TdxRibbonColorGalleryColorMap; var AResult: TdxRibbonColorGalleryAccentColorScheme); virtual;
    function CreateColorBitmap(AColor: TColor; AGlyphSize: Integer): TcxAlphaBitmap;
    procedure CreateSubControls; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopulateAccentColors; virtual;
    procedure PopulateCustomColors; virtual;
    procedure PopulateGallery; virtual;
    procedure PopulateStandardColors; virtual;
    procedure PopulateThemeColors; virtual;

    procedure ChangeScale(M, D: Integer); override;
    procedure ClearItemList; override;
    function GetActionLinkClass: TdxBarItemActionLinkClass; override;
    function InternalActuallyVisible: Boolean; override;
    procedure PopulateListedItemLinks(AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer); override;

    property AccentColorsGroup: TdxRibbonGalleryGroup read FAccentColorsGroup;
    property CustomColorsGroup: TdxRibbonGalleryGroup read FCustomColorsGroup;
    property StandardColorsGroup: TdxRibbonGalleryGroup read FStandardColorsGroup;
    property ThemeColorsGroup: TdxRibbonGalleryGroup read FThemeColorsGroup;

    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoClick; override;
  published
    property AutoMenuItemColor: TColor read FAutoMenuItemColor write SetAutoMenuItemColor default clDefault;
    property Color: TColor read FColor write SetColor default clDefault;
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
    property ShowItemBorders: Boolean read FShowItemBorders write SetShowItemBorders default True;
    property Theme: TdxRibbonColorGalleryTheme read FTheme write FTheme default rcptDefault;

    property OnColorChanged: TNotifyEvent read FOnColorChanged write FOnColorChanged;
  end;

  { TdxRibbonColorGalleryItemActionLink }

  TdxRibbonColorGalleryItemActionLink = class(TdxBarItemActionLink,
    IUnknown,
    IdxActionValueClient)
  strict private
    FUpdateValueLockCount: Integer;

    // IdxActionValueClient
    procedure ActionValueChanged(const AValue: Variant);

    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure UpdateValue(const AValue: Variant);
  end;

  { TdxRibbonColorGalleryItemControl }

  TdxRibbonColorGalleryItemControl = class(TdxBarContainerItemControl)
  protected
    function GetSubMenuControlClass: TdxBarContainerItemSubMenuControlClass; override;
  end;

  { TdxRibbonColorGalleryItemSubMenuControl }

  TdxRibbonColorGalleryItemSubMenuControl = class(TdxBarContainerItemSubMenuControl)
  protected
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
  end;

implementation

uses
  dxBarStrs, SysUtils, dxColorPicker, dxColorDialog, dxDPIAwareUtils;

type
  TAccent = (aLight80, aLight60, aLight50, aLight40, aLight35, aLight25, aLight15, aLight5,
    aDark10, aDark25, aDark50, aDark75, aDark90);

const
  StandardColorMap: TdxRibbonColorGalleryColorMap = (
    $0000C0, $0000FF, $00C0FF, $00FFFF, $50D092, $50B000, $F0B000, $C07000, $602000, $A03070
  );

  ThemeColorMaps: array [TdxRibbonColorGalleryTheme] of TdxRibbonColorGalleryColorMap =(
    ($FFFFFF, 0, $D2B48C, $00008B, $0000FF, $FF0000, $556B2F, $800080, $FFFF00, $FFA500),
    ($FFFFFF, 0, $7D491F, $E1ECEE, $BD814F, $4D50C0, $59BB9B, $A26480, $C6AC4B, $4696F7),
    ($FFFFFF, 0, $6D6769, $D1C2C9, $66B9CE, $84B09C, $C9B16B, $CF8565, $C96B7E, $BB79A3),
    ($FFFFFF, 0, $323232, $D1DEE3, $097FF0, $36299F, $7C581B, $42854E, $784860, $5998C1),
    ($FFFFFF, 0, $866B64, $D7D1C5, $4963D1, $00B4CC, $AEAD8C, $707B8C, $8CB08F, $4990D1),
    ($FFFFFF, 0, $464646, $FAF5DE, $BFA22D, $281FDA, $1B64EB, $9D6339, $784B47, $4A3C7D)
  );

{ TdxRibbonColorGalleryItem }

constructor TdxRibbonColorGalleryItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoMenuItemColor := clDefault;
  FColor := clDefault;
  FScaleFactor := TdxScaleFactor.Create;
  FCustomColors := TStringList.Create;
  FInternalItems := TComponentList.Create;
  FShowItemBorders := True;
end;

destructor TdxRibbonColorGalleryItem.Destroy;
begin
  ClearItemList;
  FreeAndNil(FInternalItems);
  FreeAndNil(FCustomColors);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TdxRibbonColorGalleryItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxRibbonColorGalleryItem then
  begin
    AutoMenuItemColor := TdxRibbonColorGalleryItem(Source).AutoMenuItemColor;
    Color := TdxRibbonColorGalleryItem(Source).Color;
    CustomColors := TdxRibbonColorGalleryItem(Source).CustomColors;
    ShowItemBorders := TdxRibbonColorGalleryItem(Source).ShowItemBorders;
  end;
end;

procedure TdxRibbonColorGalleryItem.DoClick;
begin
  if Supports(Action, IdxActionValue) then
    CallNotify(OnClick, Self)
  else
    inherited DoClick;
end;

procedure TdxRibbonColorGalleryItem.AddColorItem(AGalleryGroup: TdxRibbonGalleryGroup; AColor: TColor);
var
  AColorName: string;
  AItem: TdxRibbonGalleryGroupItem;
begin
  AItem := AGalleryGroup.Items.Add;
  AItem.Tag := AColor;
  AssignColorBitmap(AItem.Glyph, AColor);

  if cxNameByColor(AColor, AColorName) then
    AItem.Caption := AColorName
  else
    AItem.Caption := '$' + IntToHex(AColor, 6);
end;

procedure TdxRibbonColorGalleryItem.AddColorRow(
  AGalleryGroup: TdxRibbonGalleryGroup; AColorMap: TdxRibbonColorGalleryColorMap);
var
  I: Integer;
begin
  for I := Low(AColorMap) to High(AColorMap) do
    AddColorItem(AGalleryGroup, AColorMap[I]);
end;

procedure TdxRibbonColorGalleryItem.AddCustomColor(AColor: TdxAlphaColor);
var
  AColors: TdxColorDialogCustomColors;
  AIndexToInsert: Integer;
  I: Integer;
begin
  TdxColorDialogHelper.LoadCustomColors(CustomColors, AColors);
  for I := Low(TdxColorDialogCustomColors) to High(TdxColorDialogCustomColors) do
  begin
    if AColors[I] = AColor then
      Exit;
  end;

  AIndexToInsert := High(TdxColorDialogCustomColors);
  for I := Low(TdxColorDialogCustomColors) to High(TdxColorDialogCustomColors) do
    if AColors[I] = 0 then
    begin
      AIndexToInsert := I;
      Break;
    end;

  AColors[AIndexToInsert] := AColor;
  TdxColorDialogHelper.SaveCustomColors(CustomColors, AColors, False);
end;

procedure TdxRibbonColorGalleryItem.AssignColorBitmap(ATarget: TdxSmartGlyph; AColor: TColor);
var
  ASource: TcxAlphaBitmap;
begin
  ASource := CreateColorBitmap(AColor, ScaleFactor.Apply(GlyphSize));
  try
    ATarget.Assign(ASource);
    ATarget.SourceDPI := ScaleFactor.Apply(dxDefaultDPI);
  finally
    ASource.Free;
  end;
end;

procedure TdxRibbonColorGalleryItem.BuildAccentColorScheme(
  ASource: TdxRibbonColorGalleryColorMap; var AResult: TdxRibbonColorGalleryAccentColorScheme);

  function GetBrightness(ARGBColor: DWORD): Integer;
  begin
    Result := (GetBValue(ARGBColor) + GetGValue(ARGBColor) + GetRValue(ARGBColor)) div 3;
  end;

  procedure CreateAccent(AAccents: array of TAccent; AMapIndex: Integer);
  const
    AccentMap: array [TAccent] of Integer = (80, 60, 50, 40, 35, 25, 15, 5, -10, -25, -50, -75, -90);
  var
    I: Integer;
  begin
    for I := Low(AAccents) to High(AAccents) do
      AResult[I][AMapIndex] := dxGetColorTint(ASource[AMapIndex], AccentMap[AAccents[I]]);
  end;

var
  ABrightness: Integer;
  AMapIndex: Integer;
begin
  for AMapIndex := Low(ASource) to High(ASource) do
  begin
    ABrightness := GetBrightness(ColorToRGB(ASource[AMapIndex]));
    if ABrightness < 20 then
      CreateAccent([aLight50, aLight35, aLight25, aLight15, aLight5], AMapIndex)
    else
      if ABrightness < 230 then
        CreateAccent([aLight80, aLight60, aLight35, aDark25, aDark50], AMapIndex)
      else
        CreateAccent([aDark10, aDark25, aDark50, aDark75, aDark90], AMapIndex);
  end;
end;

function TdxRibbonColorGalleryItem.CreateColorBitmap(AColor: TColor; AGlyphSize: Integer): TcxAlphaBitmap;
begin
  Result := TcxAlphaBitmap.CreateSize(AGlyphSize, AGlyphSize);
  FillRectByColor(Result.Canvas.Handle, Result.ClientRect, AColor);
  if ShowItemBorders then
    FrameRectByColor(Result.Canvas.Handle, Result.ClientRect, clGray);
  if AColor = clNone then
    Result.RecoverAlphaChannel(0)
  else
    Result.TransformBitmap(btmSetOpaque);
end;

procedure TdxRibbonColorGalleryItem.CreateSubControls;
begin
  FAutoColorButton := BarManager.AddButton;
  FAutoColorButton.Caption := cxGetResourceString(@sdxRibbonColorGalleryAutoColor);
  FAutoColorButton.ButtonStyle := bsChecked;
  FAutoColorButton.OnClick := SelectAutoColorHandler;
  FAutoColorButton.Tag := clDefault;
  FAutoColorButton.FreeNotification(Self);
  AssignColorBitmap(FAutoColorButton.Glyph, AutoMenuItemColor);
  BarDesignController.AddInternalItem(FAutoColorButton, FInternalItems);

  FColorGallery := TdxRibbonGalleryItem(BarManager.AddItem(TdxRibbonGalleryItem));
  FColorGallery.GalleryOptions.EqualItemSizeInAllGroups := False;
  FColorGallery.GalleryInMenuOptions.CollapsedInSubmenu := False;
  FColorGallery.GalleryOptions.ColumnCount := dxRibbonColorGalleryColorMapSize;
  FColorGallery.GalleryOptions.SpaceBetweenGroups := 4;
  FColorGallery.GalleryOptions.ItemTextKind := itkNone;
  FColorGallery.OnGroupItemClick := SelectColorHandler;
  FColorGallery.FreeNotification(Self);
  BarDesignController.AddInternalItem(FColorGallery, FInternalItems);

  FThemeColorsGroup := FColorGallery.GalleryGroups.Add;
  FThemeColorsGroup.Header.Visible := True;
  FThemeColorsGroup.Header.Caption := cxGetResourceString(@sdxRibbonColorGalleryGroupThemeColors);
  FAccentColorsGroup := FColorGallery.GalleryGroups.Add;
  FAccentColorsGroup.Options.SpaceBetweenItemsVertically := -1;
  FStandardColorsGroup := FColorGallery.GalleryGroups.Add;
  FStandardColorsGroup.Header.Visible := True;
  FStandardColorsGroup.Header.Caption := cxGetResourceString(@sdxRibbonColorGalleryGroupStandardColors);
  FCustomColorsGroup := FColorGallery.GalleryGroups.Add;
  FCustomColorsGroup.Header.Caption := cxGetResourceString(@sdxRibbonColorGalleryGroupCustomColors);

  FMoreColorsButton := BarManager.AddButton;
  FMoreColorsButton.OnClick := MoreColorsClickHandler;
  FMoreColorsButton.Caption := cxGetResourceString(@sdxRibbonColorGalleryMoreColors);
  FMoreColorsButton.FreeNotification(Self);
  BarDesignController.AddInternalItem(FMoreColorsButton, FInternalItems)
end;

procedure TdxRibbonColorGalleryItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if FAutoColorButton = AComponent then
      FAutoColorButton := nil;
    if FColorGallery = AComponent then
      FColorGallery := nil;
    if FMoreColorsButton = AComponent then
      FMoreColorsButton := nil;
  end;
end;

procedure TdxRibbonColorGalleryItem.PopulateGallery;
begin
  PopulateThemeColors;
  PopulateAccentColors;
  PopulateStandardColors;
  PopulateCustomColors;
end;

procedure TdxRibbonColorGalleryItem.PopulateAccentColors;
var
  AAccentColorScheme: TdxRibbonColorGalleryAccentColorScheme;
  I: Integer;
begin
  AccentColorsGroup.Items.Clear;
  BuildAccentColorScheme(ThemeColorMaps[Theme], AAccentColorScheme);
  for I := Low(AAccentColorScheme) to High(AAccentColorScheme) do
    AddColorRow(AccentColorsGroup, AAccentColorScheme[I]);
end;

procedure TdxRibbonColorGalleryItem.PopulateCustomColors;
var
  AColors: TdxColorDialogCustomColors;
  I: Integer;
begin
  CustomColorsGroup.Items.Clear;
  TdxColorDialogHelper.LoadCustomColors(CustomColors, AColors);
  for I := Low(TdxColorDialogCustomColors) to High(TdxColorDialogCustomColors) do
  begin
    if AColors[I] <> 0 then
      AddColorItem(CustomColorsGroup, dxAlphaColorToColor(AColors[I]));
  end;
  CustomColorsGroup.Header.Visible := CustomColorsGroup.ItemCount > 0;
end;

procedure TdxRibbonColorGalleryItem.PopulateStandardColors;
begin
  StandardColorsGroup.Items.Clear;
  AddColorRow(StandardColorsGroup, StandardColorMap);
end;

procedure TdxRibbonColorGalleryItem.PopulateThemeColors;
begin
  ThemeColorsGroup.Items.Clear;
  AddColorRow(ThemeColorsGroup, ThemeColorMaps[Theme]);
end;

procedure TdxRibbonColorGalleryItem.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  ScaleFactor.Change(M, D);
  ClearItemList;
end;

procedure TdxRibbonColorGalleryItem.ClearItemList;
begin
  inherited ClearItemList;

  FreeAndNil(FAutoColorButton);
  FreeAndNil(FMoreColorsButton);
  FreeAndNil(FColorGallery);
end;

function TdxRibbonColorGalleryItem.InternalActuallyVisible: Boolean;
begin
  Result := True;
end;

function TdxRibbonColorGalleryItem.GetActionLinkClass: TdxBarItemActionLinkClass;
begin
  Result := TdxRibbonColorGalleryItemActionLink;
end;

procedure TdxRibbonColorGalleryItem.PopulateListedItemLinks(
  AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer);
var
  AGroup: TdxRibbonGalleryGroup;
  I, J: Integer;
begin
  if IsDesigning then
    Exit;

  BarManager.BeginUpdate;
  try
    CreateSubControls;
    PopulateGallery;

    FAutoColorButton.Down := FAutoColorButton.Tag = Color;
    for I := 0 to FColorGallery.GalleryGroups.Count - 1 do
    begin
      AGroup := FColorGallery.GalleryGroups[I];
      for J := 0 to AGroup.Items.Count - 1 do
        AGroup.Items[J].Selected := AGroup.Items[J].Tag = Color;
    end;

    AddListedItemLink(AItemLinks, ALinkData, AIndex, FMoreColorsButton);
    AddListedItemLink(AItemLinks, ALinkData, AIndex, FColorGallery);
    AddListedItemLink(AItemLinks, ALinkData, AIndex, FAutoColorButton);
  finally
    BarManager.EndUpdate;
  end;
end;

procedure TdxRibbonColorGalleryItem.SelectAutoColorHandler(Sender: TObject);
begin
  Color := (Sender as TComponent).Tag;
end;

procedure TdxRibbonColorGalleryItem.MoreColorsClickHandler(Sender: TObject);
var
  ADialog: TdxColorDialog;
begin
  ADialog := TdxColorDialog.Create(nil);
  try
    ADialog.CustomColors := CustomColors;
    ADialog.Options.ColorPicker.AllowEditAlpha := False;
    ADialog.Options.ColorPicker.DefaultVisible := True;
    ADialog.Color := dxColorToAlphaColor(Color);
    if ADialog.Execute then
    begin
      CustomColors := ADialog.CustomColors;
      AddCustomColor(ADialog.Color);
      Color := dxAlphaColorToColor(ADialog.Color);
    end;
  finally
    ADialog.Free;
  end;
end;

procedure TdxRibbonColorGalleryItem.SelectColorHandler(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
begin
  if FColorGallery.SelectedGroupItem <> nil then
    Color := FColorGallery.SelectedGroupItem.Tag;
end;

procedure TdxRibbonColorGalleryItem.SetAutoMenuItemColor(AValue: TColor);
begin
  if FAutoMenuItemColor <> AValue then
  begin
    FAutoMenuItemColor := AValue;
    Invalidate;
  end;
end;

procedure TdxRibbonColorGalleryItem.SetColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    if ActionLink <> nil then
      TdxRibbonColorGalleryItemActionLink(ActionLink).UpdateValue(Color);
    CallNotify(OnColorChanged, Self);
  end;
end;

procedure TdxRibbonColorGalleryItem.SetCustomColors(AValue: TStrings);
begin
  FCustomColors.Assign(AValue);
end;

procedure TdxRibbonColorGalleryItem.SetShowItemBorders(AValue: Boolean);
begin
  if FShowItemBorders <> AValue then
  begin
    FShowItemBorders := AValue;
    Invalidate;
  end;
end;

{ TdxRibbonColorGalleryItemActionLink }

procedure TdxRibbonColorGalleryItemActionLink.UpdateValue(const AValue: Variant);
var
  ABasicAction: TdxBasicAction;
  AEnabled: Boolean;
  AIntf: IdxActionValue;
begin
  if FUpdateValueLockCount = 0 then
  begin
    Inc(FUpdateValueLockCount);
    try
      if Supports(Action, IdxActionValue, AIntf) then
      begin
        ABasicAction := TdxBasicAction(Action);
        ABasicAction.BeginUpdate;
        try
          AEnabled := ABasicAction.Enabled;
          try
            ABasicAction.Enabled := True;
            AIntf.Value := AValue;
          finally
            ABasicAction.Enabled := AEnabled;
          end;
        finally
          ABasicAction.EndUpdate;
        end;
      end;
    finally
      Dec(FUpdateValueLockCount);
    end;
  end;
end;

procedure TdxRibbonColorGalleryItemActionLink.ActionValueChanged(const AValue: Variant);
begin
  if FUpdateValueLockCount = 0 then
  begin
    Inc(FUpdateValueLockCount);
    try
      TdxRibbonColorGalleryItem(FClient).Color := AValue;
    finally
      Dec(FUpdateValueLockCount);
    end;
  end;
end;

function TdxRibbonColorGalleryItemActionLink.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxRibbonColorGalleryItemActionLink._AddRef: Integer;
begin
  Result := -1;
end;

function TdxRibbonColorGalleryItemActionLink._Release: Integer;
begin
  Result := -1;
end;

{ TdxRibbonColorGalleryItemControl }

function TdxRibbonColorGalleryItemControl.GetSubMenuControlClass: TdxBarContainerItemSubMenuControlClass;
begin
  Result := TdxRibbonColorGalleryItemSubMenuControl;
end;

{ TdxRibbonColorGalleryItemSubMenuControl }

function TdxRibbonColorGalleryItemSubMenuControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := inherited GetBehaviorOptions - [bboCanShowPopupMenuOnMouseClick];
end;

initialization
  dxBarRegisterItem(TdxRibbonColorGalleryItem, TdxRibbonColorGalleryItemControl, True);

finalization
  dxBarUnregisterItem(TdxRibbonColorGalleryItem);
end.
