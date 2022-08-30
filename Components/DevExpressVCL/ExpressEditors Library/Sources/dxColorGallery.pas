{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxColorGallery;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics,
  dxCore, dxCoreGraphics, cxVariants, cxClasses, cxControls, cxGraphics, dxGalleryControl,
  cxLookAndFeels, dxGallery, cxLookAndFeelPainters;

const
  dxcOfficeSchemeColorCount = 10;
  dxcColorAccentCount = 5;
  dxcOfficeSchemeItemSize = 14;
  dxcExtendedSchemeItemSize = 12;
  dxcStandardSchemeItemSize = 24;

type
  TdxOfficeColorSet = array [0..dxcOfficeSchemeColorCount-1] of TColor;
  TdxColorPalette = (cpOffice, cpExtended, cpStandard);
  TdxColorSet = (csDefault, csTheme1, csTheme2, csTheme3, csTheme4, csTheme5, csCustom);
  TdxColorAccent = (caLight80, caLight60, caLight50, caLight40, caLight35, caLight25, caLight15, caLight5, caDark5, caDark10, caDark15, caDark25, caDark35, caDark50, caDark75, caDark90);
  TdxColorAccentArray = array [0..dxcColorAccentCount-1] of TdxColorAccent;
  TdxColorGetCustomColorSetEvent = procedure (Sender: TObject; var ASet: TColors) of object;

  { TdxColorGalleryPainter }

  TdxColorGalleryPainter = class(TdxGalleryControlPainter)
  protected
    function DrawItemSelectionFirst: Boolean; override;
  public
    procedure DrawItemSelection(ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo); override;
  end;

  { TdxCustomColorGallery }

  TdxCustomColorGallery = class(TdxCustomGalleryControl)
  private
    FEditValue: Variant;
    FColorPalette: TdxColorPalette;
    FColorSet: TdxColorSet;
    FItemSize: Integer;
    FShowItemBorders: Boolean;

    FOnChange: TNotifyEvent;
    FOnGetCustomColorSet: TdxColorGetCustomColorSetEvent;

    procedure InternalSetEditValue(AValue: Variant; ANeedSynchronizeSelectedItems: Boolean);
    function GetColorValue: TColor;
    procedure SetColorValue(AValue: TColor);
    procedure SetEditValue(AValue: Variant);
    procedure SetItemSize(AValue: Integer);
    procedure SetShowItemBorders(AValue: Boolean);
    procedure SynchronizeSelectedItems;

    procedure SetColorPalette(AValue: TdxColorPalette);
    procedure SetColorSet(AValue: TdxColorSet);

    function AddColorGroup(const ACaption: string): TdxGalleryGroup;
    procedure AddColorItem(AGroup: TdxGalleryGroup; AColor: TColor; ASize: Integer; ABorders: TcxBorders = cxBordersAll);
    procedure PopulateWithExtendedColors;
    procedure PopulateWithStandardColors;
    procedure PopulateWithOfficeColors;
    procedure BuildColorPalette;
  protected
    // TWinControl
    procedure Loaded; override;
    // TcxCustomControl
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;

    procedure DoClickItem(AItem: TdxGalleryItem); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    // TdxCustomGalleryControl
    function CreatePainter: TdxGalleryControlPainter; override;
    procedure SetColorParams(AColorPalette: TdxColorPalette; AColorSet: TdxColorSet; AItemSize: Integer);

    property ColorPalette: TdxColorPalette read FColorPalette write SetColorPalette;
    property ColorSet: TdxColorSet read FColorSet write SetColorSet;
    property ColorValue: TColor read GetColorValue write SetColorValue;
    property EditValue: Variant read FEditValue write SetEditValue;
    property ItemSize: Integer read FItemSize write SetItemSize;
    property ShowItemBorders: Boolean read FShowItemBorders write SetShowItemBorders;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetCustomColorSet: TdxColorGetCustomColorSetEvent read FOnGetCustomColorSet write FOnGetCustomColorSet;
  public
    constructor Create(AOwner: TComponent); override;
    // IdxLocalizerListener
    procedure TranslationChanged; override;
  end;

  { TdxColorGallery }

  TdxColorGallery = class(TdxCustomColorGallery)
  public
    property ColumnCount;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property PopupMenu;
    property Visible;

    property LookAndFeel;

    property AutoSizeMode default asAutoSize;
    property BorderStyle default cxcbsDefault;
    property ColorPalette default cpOffice;
    property ColorSet default csDefault;
    property ColorValue default clNone;
    property ItemShowHint default False;
    property ItemSize default 0;
    property ShowItemBorders default True;
    property TabOrder;
    property TabStop;
    property Transparent;

    // Events
    property OnItemClick;
    property OnChange;
    property OnGetCustomColorSet;
  end;

implementation

uses
  Controls, dxOffice11, Variants, cxEditConsts, Math, cxGeometry, dxDPIAwareUtils;

type
  TcxComponentCollectionAccess = class(TcxComponentCollection);
  TdxCustomGalleryAccess = class(TdxCustomGallery);

{ TdxColorGalleryPainter }

function TdxColorGalleryPainter.DrawItemSelectionFirst: Boolean;
begin
  Result := False;
end;

procedure TdxColorGalleryPainter.DrawItemSelection(
  ACanvas: TcxCanvas; AViewInfo: TdxGalleryItemViewInfo);
begin
  LookAndFeelPainter.DrawColorGalleryItemSelection(ACanvas, AViewInfo.GlyphRect, AViewInfo.State);
end;

{ TdxCustomColorGallery }

constructor TdxCustomColorGallery.Create(AOwner: TComponent);
begin
  inherited;

  AutoSizeMode := asAutoSize;
  Color := clWhite;

  ItemShowImageFrame := False;
  Gallery.ItemCheckMode := icmSingleRadio;
  FEditValue := clNone;
  FShowItemBorders := True;

  BuildColorPalette;
end;

procedure TdxCustomColorGallery.TranslationChanged;
begin
  BuildColorPalette;
end;

procedure TdxCustomColorGallery.Loaded;
begin
  inherited;
  BuildColorPalette;
end;

procedure TdxCustomColorGallery.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  ItemSize := MulDiv(ItemSize, M, D);
end;

procedure TdxCustomColorGallery.DoClickItem(AItem: TdxGalleryItem);
begin
  InternalSetEditValue(AItem.Tag, False);
  inherited;
end;

procedure TdxCustomColorGallery.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  BuildColorPalette;
end;

function TdxCustomColorGallery.CreatePainter: TdxGalleryControlPainter;
begin
  Result := TdxColorGalleryPainter.Create(Self);
end;

procedure TdxCustomColorGallery.SetColorParams(AColorPalette: TdxColorPalette; AColorSet: TdxColorSet; AItemSize: Integer);
begin
  if (FColorPalette <> AColorPalette) or (FColorSet <> AColorSet) or (FItemSize <> AItemSize) then
  begin
    FColorPalette := AColorPalette;
    FColorSet := AColorSet;
    FItemSize := AItemSize;
    BuildColorPalette;
  end;
end;

procedure TdxCustomColorGallery.InternalSetEditValue(AValue: Variant; ANeedSynchronizeSelectedItems: Boolean);
begin
  if not VarEquals(FEditValue, AValue) then
  begin
    FEditValue := AValue;
    if ANeedSynchronizeSelectedItems then
      SynchronizeSelectedItems;
    dxCallNotify(OnChange, Self);
  end;
end;

function TdxCustomColorGallery.GetColorValue: TColor;
begin
  if VarIsNull(FEditValue) then
    Result := clNone
  else
    Result := FEditValue;
end;

procedure TdxCustomColorGallery.SetColorValue(AValue: TColor);
begin
  EditValue := AValue;
end;

procedure TdxCustomColorGallery.SetItemSize(AValue: Integer);
begin
  if FItemSize <> AValue then
  begin
    FItemSize := AValue;
    BuildColorPalette;
  end;
end;

procedure TdxCustomColorGallery.SetShowItemBorders(AValue: Boolean);
begin
  if FShowItemBorders <> AValue then
  begin
    FShowItemBorders := AValue;
    BuildColorPalette;
  end;
end;

procedure TdxCustomColorGallery.SetEditValue(AValue: Variant);
begin
  InternalSetEditValue(AValue, True);
end;

procedure TdxCustomColorGallery.SynchronizeSelectedItems;
var
  AItem: TdxGalleryItem;
begin
  TdxCustomGalleryAccess(Gallery).CheckItem(Gallery.GetCheckedItem, False);

  AItem := Gallery.FindItemByTag(Integer(ColorValue));
  if AItem <> nil then
    TdxCustomGalleryAccess(Gallery).CheckItem(AItem, True);
end;

procedure TdxCustomColorGallery.SetColorPalette(AValue: TdxColorPalette);
begin
  if FColorPalette <> AValue then
  begin
    FColorPalette := AValue;
    BuildColorPalette;
  end;
end;

procedure TdxCustomColorGallery.SetColorSet(AValue: TdxColorSet);
begin
  if FColorSet <> AValue then
  begin
    FColorSet := AValue;
    if ColorPalette = cpOffice then
      BuildColorPalette;
  end;
end;

function TdxCustomColorGallery.AddColorGroup(const ACaption: string): TdxGalleryGroup;
begin
  Result := TdxGalleryGroup(TcxComponentCollectionAccess(Gallery.Groups).AddInternalItem);
  Result.Caption := ACaption;
end;

procedure TdxCustomColorGallery.AddColorItem(AGroup: TdxGalleryGroup; AColor: TColor; ASize: Integer; ABorders: TcxBorders);
var
  AItem: TdxGalleryItem;
  ABitmap: TcxBitmap32;
begin
  AItem := TdxGalleryItem(TcxComponentCollectionAccess(AGroup.Items).AddInternalItem);
  AItem.Tag := Integer(AColor);
  AItem.Hint := ColorToString(AColor);

  ABitmap := TcxBitmap32.CreateSize(ASize, ASize);
  try
    ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, AColor);
    if ShowItemBorders then
      ABitmap.cxCanvas.FrameRect(ABitmap.ClientRect, LookAndFeelPainter.GetGalleryItemImageFrameColor, 1, ABorders);
    ABitmap.SetAlphaChannel($FF);
    AItem.Glyph.Assign(ABitmap);
    AItem.Glyph.SourceDPI := ScaleFactor.Apply(dxDefaultDPI);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxCustomColorGallery.PopulateWithExtendedColors;
const
  ExtendedColorMap: array[0..39] of TColor =
    ($000000, $003399, $003333, $003300, $336600, $800000, $993333, $333333,
     $000080, $0066FF, $008080, $008000, $808000, $FF0000, $996666, $808080,
     $0000FF, $0099FF, $00CC99, $669933, $CCCC33, $FF6633, $800080, $999999,
     $FF00FF, $00CCFF, $00FFFF, $00FF00, $FFFF00, $FFCC00, $663399, $CCCCCC,
     $CC99FF, $99CCFF, $99FFFF, $CCFFCC, $FFFFCC, $FFCCCC, $FF99CC, $FFFFFF);
var
  I: Integer;
  AGroup: TdxGalleryGroup;
  AColors: TColors;
  AItemSize: Integer;
begin
  ColumnCount := 8;
  AGroup := AddColorGroup('');
  AItemSize := IfThen(ItemSize > 0, ItemSize, ScaleFactor.Apply(dxcExtendedSchemeItemSize));

  SetLength(AColors, Length(ExtendedColorMap));
  for I := 0 to Length(ExtendedColorMap) - 1 do
    AColors[I] := ExtendedColorMap[I];

  if (ColorSet = csCustom) and Assigned(OnGetCustomColorSet) then
    OnGetCustomColorSet(Self, AColors);

  for I := Low(AColors) to High(AColors) do
    AddColorItem(AGroup, AColors[I], AItemSize);
end;

procedure TdxCustomColorGallery.PopulateWithStandardColors;
const
  StandardColorMap: array[0..14] of TColor =
    ($0000FF, $00FF00, $FF0000,
     $FFFF00, $FF00FF, $00FFFF,
     $000080, $008000, $800000,
     $808000, $800080, $008080,
     $FFFFFF, $808080, $000000);
var
  I: Integer;
  AGroup: TdxGalleryGroup;
  AColors: TColors;
  AItemSize: Integer;
begin
  ColumnCount := 5;
  AGroup := AddColorGroup('');
  AItemSize := IfThen(ItemSize > 0, ItemSize, ScaleFactor.Apply(dxcStandardSchemeItemSize));

  SetLength(AColors, Length(StandardColorMap));
  for I := 0 to Length(StandardColorMap) - 1 do
    AColors[I] := StandardColorMap[I];

  if (ColorSet = csCustom) and Assigned(OnGetCustomColorSet) then
    OnGetCustomColorSet(Self, AColors);

  for I := Low(AColors) to High(AColors) do
    AddColorItem(AGroup, AColors[I], AItemSize);
end;

procedure TdxCustomColorGallery.PopulateWithOfficeColors;
const
  AnAccentMap: array [TdxColorAccent] of Integer = (80, 60, 50, 40, 35, 25, 15, 5, -5, -10, -15, -25, -35, -50, -75, -90);

  function GetAccent(ABaseColor: TColor; AnAccent: TdxColorAccent): TColor;
  begin
    Result := dxGetColorTint(ABaseColor, AnAccentMap[AnAccent]);
  end;

  procedure PopulateWithAccents(ABaseColors: TColors; AItemSize: Integer);

    function GetItemBorders(AIsFirst, AIsLast: Boolean): TcxBorders;
    begin
      Result := [bLeft, bRight];
      if AIsFirst then
        Include(Result, bTop);
      if AIsLast then
        Include(Result, bBottom);
    end;

    procedure InternalPopulateWithAccents(AIndex: Integer; var AColorMatrix: array of TdxOfficeColorSet);
    const
      ColorAccentMap1: TdxColorAccentArray = (caLight50, caLight35, caLight25, caLight15, caLight5);
      ColorAccentMap2: TdxColorAccentArray = (caLight80, caLight60, caLight40, caDark25, caDark50);
      ColorAccentMap3: TdxColorAccentArray = (caDark10, caDark25, caDark50, caDark75, caDark90);
      ColorAccentMap4: TdxColorAccentArray = (caDark5, caDark15, caDark25, caDark35, caDark50);
    var
      I: Integer;
      AnAccents: TdxColorAccentArray;
      AHSV: TdxHSV;
    begin
      AHSV := dxColorToHSV(ABaseColors[AIndex]);
      if AHSV.V < 0.08 then
        AnAccents := ColorAccentMap1
      else
        if (AHSV.V > 0.8) and (AHSV.S < 0.15) then
        begin
          if AHSV.S > 0.05 then
            AnAccents := ColorAccentMap3
          else
            AnAccents := ColorAccentMap4;
        end
        else
          AnAccents := ColorAccentMap2;

      for I := Low(AnAccents) to High(AnAccents) do
        AColorMatrix[I][AIndex] := GetAccent(ABaseColors[AIndex], AnAccents[I]);
    end;

  const
    RowsCount = 5;
  var
    AColorMatrix: array [0..RowsCount - 1] of TdxOfficeColorSet;
    AGroup: TdxGalleryGroup;
    I, J: Integer;
  begin
    AGroup := AddColorGroup('');
    for I := Low(TdxOfficeColorSet) to High(TdxOfficeColorSet) do
      InternalPopulateWithAccents(I, AColorMatrix);

    for J := 0 to RowsCount - 1 do
    begin
      for I := 0 to dxcOfficeSchemeColorCount - 1 do
        AddColorItem(AGroup, AColorMatrix[J][I], AItemSize, GetItemBorders(J = 0, J = RowsCount - 1));
    end;
  end;

const
  OfficeStandardColorMap: TdxOfficeColorSet =
    ($0000C0, $0000FF, $00C0FF, $00FFFF, $50D092, $50B000, $F0B000, $C07000, $602000, $A03070);
  OfficeColorMaps: array [TdxColorSet] of TdxOfficeColorSet =(
    ($FFFFFF, $000000, $D2B48C, $00008B, $0000FF, $FF0000, $556B2F, $800080, $FFFF00, $FFA500),
    ($FFFFFF, $000000, $E1ECEE, $7D491F, $BD814F, $4D50C0, $59BB9B, $A26480, $C6AC4B, $4696F7),
    ($FFFFFF, $000000, $6D6769, $D1C2C9, $66B9CE, $84B09C, $C9B16B, $CF8565, $C96B7E, $BB79A3),
    ($FFFFFF, $000000, $323232, $D1DEE3, $097FF0, $36299F, $7C581B, $42854E, $784860, $5998C1),
    ($FFFFFF, $000000, $866B64, $D7D1C5, $4963D1, $00B4CC, $AEAD8C, $707B8C, $8CB08F, $4990D1),
    ($FFFFFF, $000000, $464646, $FAF5DE, $BFA22D, $281FDA, $1B64EB, $9D6339, $784B47, $4A3C7D),
    ($FFFFFF, $000000, $D2B48C, $00008B, $0000FF, $FF0000, $556B2F, $800080, $FFFF00, $FFA500)
  );
var
  AColors: TColors;
  AGroup: TdxGalleryGroup;
  AItemSize: Integer;
  I: Integer;
begin
  ColumnCount := 10;
  AGroup := AddColorGroup(cxGetResourceString(@sdxColorGalleryThemeColors));
  AItemSize := IfThen(ItemSize > 0, ItemSize, ScaleFactor.Apply(dxcOfficeSchemeItemSize));

  SetLength(AColors, dxcOfficeSchemeColorCount * 2);
  for I := 0 to dxcOfficeSchemeColorCount - 1 do
    AColors[I] := OfficeColorMaps[ColorSet][I];
  for I := dxcOfficeSchemeColorCount to Length(AColors) - 1 do
    AColors[I] := OfficeStandardColorMap[I-dxcOfficeSchemeColorCount];
  if (ColorSet = csCustom) and Assigned(OnGetCustomColorSet) then
    OnGetCustomColorSet(Self, AColors);

  for I := 0 to dxcOfficeSchemeColorCount - 1 do
    AddColorItem(AGroup, AColors[I], AItemSize);
  PopulateWithAccents(AColors, AItemSize);

  AGroup := AddColorGroup(cxGetResourceString(@sdxColorGalleryStandardColors));
  for I := dxcOfficeSchemeColorCount to Length(AColors) - 1 do
    AddColorItem(AGroup, AColors[I], AItemSize);
end;

procedure TdxCustomColorGallery.BuildColorPalette;
begin
  if IsLoading then
    Exit;
  BeginUpdate;
  try
    Gallery.Groups.Clear;
    case ColorPalette of
      cpOffice:
        begin
          ContentOffsetItems.Margin := Rect(2, 0, 2, 0);
          ContentOffsetGroups.Margin := Rect(0, 2, 0, 2);
          PopulateWithOfficeColors;
        end;

      cpExtended:
        begin
          ContentOffsetItems.Margin := Rect(2, 2, 2, 2);
          ContentOffsetGroups.Margin := cxEmptyRect;
          PopulateWithExtendedColors;
        end;

      cpStandard:
        begin
          ContentOffsetItems.Margin := Rect(2, 2, 2, 2);
          ContentOffsetGroups.Margin := cxEmptyRect;
          PopulateWithStandardColors;
        end;
    end;
    SynchronizeSelectedItems;
  finally
    EndUpdate;
  end;
end;

end.
