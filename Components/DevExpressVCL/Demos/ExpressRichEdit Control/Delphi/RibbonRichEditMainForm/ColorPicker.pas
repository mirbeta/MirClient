unit ColorPicker;

interface

{$I cxVer.Inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Forms, Graphics, dxBar, dxRibbonGallery, cxGraphics, dxRibbon, dxColorDialog;

const
  SchemeColorCount = 10;

type
  TColorMap = array [0..SchemeColorCount-1] of TColor;

  { TColorPickerController }

  TColorPickerController = class
  private
    FColor: TColor;
    FColorGlyphSize: Integer;
    FColorDialog: TdxColorDialog;
    FColorItem: TdxRibbonGalleryItem;
    FColorMapItem: TdxRibbonGalleryItem;
    FContainerItemLinks: TdxBarItemLinks;
    FDefaultColor: TColor;
    FRibbon: TdxCustomRibbon;

    FThemeColorsGroup: TdxRibbonGalleryGroup;
    FAccentColorsGroup: TdxRibbonGalleryGroup;
    FStandardColorsGroup: TdxRibbonGalleryGroup;
    FCustomColorsGroup: TdxRibbonGalleryGroup;

    FMoreColorsButton: TdxBarButton;
    FNoColorButton: TdxBarButton;
    FColorDialogSetup: TdxBarButton;
    FOnColorChanged: TNotifyEvent;

    function GetBarManager: TdxBarManager;
    procedure SetColor(Value: TColor);

    procedure ColorItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
    procedure ColorMapItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
    procedure NoColorButtonClick(Sender: TObject);
    procedure MoreColorsClick(Sender: TObject);
    procedure ColorDialogSetupButtonClick(Sender: TObject);

    property BarManager: TdxBarManager read GetBarManager;
  protected
    function AddColorItem(AGalleryGroup: TdxRibbonGalleryGroup; AColor: TColor): TdxRibbonGalleryGroupItem;
    function CreateColorBitmap(AColor: TColor; AGlyphSize: Integer = 0): TcxAlphaBitmap;
    procedure CreateColorRow(AGalleryGroup: TdxRibbonGalleryGroup; AColorMap: TColorMap);
    procedure BuildThemeColorGallery;
    procedure BuildStandardColorGallery;
    procedure BuildColorSchemeGallery;

    procedure ColorChanged;
    procedure ColorMapChanged;

    function GetContainerItemLinks(AContainerItem: TObject): TdxBarItemLinks; virtual; abstract;
  public
    constructor Create(AContainerItem: TObject; AColorItem, AColorMapItem: TdxRibbonGalleryItem;
      ARibbon: TdxCustomRibbon; ADefaultColor: TColor; ADefaultColorCaption: string); virtual;
    destructor Destroy; override;

    property Color: TColor read FColor;
    property DefaultColor: TColor read FDefaultColor;
    property OnColorChanged: TNotifyEvent read FOnColorChanged write FOnColorChanged;
  end;

  { TPopupColorPickerController }

  TPopupColorPickerController = class(TColorPickerController)
  protected
    function GetContainerItemLinks(AContainerItem: TObject): TdxBarItemLinks; override;
  public
    constructor Create(APopupMenu: TdxRibbonPopupMenu; AColorItem, AColorMapItem: TdxRibbonGalleryItem;
      ARibbon: TdxCustomRibbon; ADefaultColor: TColor; ADefaultColorCaption: string); reintroduce; virtual;
  end;

  { TSubItemColorPickerController }

  TSubItemColorPickerController = class(TColorPickerController)
  protected
    function GetContainerItemLinks(AContainerItem: TObject): TdxBarItemLinks; override;
  public
    constructor Create(ASubItem: TdxBarSubItem; AColorItem, AColorMapItem: TdxRibbonGalleryItem;
      ARibbon: TdxCustomRibbon; ADefaultColor: TColor; ADefaultColorCaption: string); reintroduce; virtual;
  end;

implementation

uses
  Types, SysUtils, dxCoreGraphics, cxGeometry, RibbonRichEditDemoGallerySetup;

type
  TAccent = (aLight80, aLight60, aLight50, aLight40, aLight35, aLight25, aLight15, aLight5, aDark10, aDark25, aDark50, aDark75, aDark90);

  TColorMapInfo = record
    Name: string;
    Map: TColorMap;
  end;

const
  StandardColorMap: TColorMap =
    ($0000C0, $0000FF, $00C0FF, $00FFFF, $50D092, $50B000, $F0B000, $C07000, $602000, $A03070);
  ColorMaps: array [0..5] of TColorMapInfo =(
    (Name: 'Default'; Map: (clWindow, clWindowText, $D2B48C, $00008B, $0000FF, $FF0000, $556B2F, $800080, clAqua, $FFA500)),
    (Name: 'Theme1'; Map: (clWindow, clWindowText, $7D491F, $E1ECEE, $BD814F, $4D50C0, $59BB9B, $A26480, $C6AC4B, $4696F7)),
    (Name: 'Theme2'; Map: (clWindow, clWindowText, $6D6769, $D1C2C9, $66B9CE, $84B09C, $C9B16B, $CF8565, $C96B7E, $BB79A3)),
    (Name: 'Theme3'; Map: (clWindow, clWindowText, $323232, $D1DEE3, $097FF0, $36299F, $7C581B, $42854E, $784860, $5998C1)),
    (Name: 'Theme4'; Map: (clWindow, clWindowText, $866B64, $D7D1C5, $4963D1, $00B4CC, $AEAD8C, $707B8C, $8CB08F, $4990D1)),
    (Name: 'Theme5'; Map: (clWindow, clWindowText, $464646, $FAF5DE, $BFA22D, $281FDA, $1B64EB, $9D6339, $784B47, $4A3C7D))
  );

function DPIRatio: Single;
begin
  Result := Screen.PixelsPerInch / 96;
end;

{ TColorPickerController }

constructor TColorPickerController.Create(AContainerItem: TObject; AColorItem, AColorMapItem: TdxRibbonGalleryItem;
  ARibbon: TdxCustomRibbon; ADefaultColor: TColor; ADefaultColorCaption: string);

  procedure InitColorItem;
  begin
    FColorItem.GalleryOptions.EqualItemSizeInAllGroups := False;
    FColorItem.GalleryOptions.ColumnCount := SchemeColorCount;
    FColorItem.GalleryOptions.SpaceBetweenGroups := 4;
    FColorItem.GalleryOptions.ItemTextKind := itkNone;
    FColorItem.OnGroupItemClick := ColorItemClick;

    FThemeColorsGroup := FColorItem.GalleryGroups.Add;
    FThemeColorsGroup.Header.Caption := 'Theme Colors';
    FThemeColorsGroup.Header.Visible := True;
    FAccentColorsGroup := FColorItem.GalleryGroups.Add;
    FStandardColorsGroup := FColorItem.GalleryGroups.Add;
    FStandardColorsGroup.Header.Caption := 'Standard Colors';
    FStandardColorsGroup.Header.Visible := True;
    FCustomColorsGroup := FColorItem.GalleryGroups.Add;
    FCustomColorsGroup.Header.Caption := 'Custom Colors';
    AColorItem.GalleryGroups[1].Options.SpaceBetweenItemsVertically := -1;
  end;

  procedure InitColorMapItem;
  begin
    FColorMapItem.GalleryOptions.ColumnCount := 1;
    FColorMapItem.GalleryOptions.SpaceBetweenItemsAndBorder := 0;
    FColorMapItem.GalleryOptions.ItemTextKind := itkCaption;
    FColorMapItem.GalleryGroups.Add;
    FColorMapItem.OnGroupItemClick := ColorMapItemClick;
  end;

  procedure InitDropDownGallery;
  var
    ABitmap: TcxAlphaBitmap;
  begin
    FNoColorButton := TdxBarButton(FContainerItemLinks.AddButton.Item);
    FNoColorButton.ButtonStyle := bsChecked;
    FNoColorButton.Caption := ADefaultColorCaption;
    FNoColorButton.OnClick := NoColorButtonClick;
    ABitmap := CreateColorBitmap(ADefaultColor, 16);
    try
      FNoColorButton.Glyph.Assign(ABitmap);
    finally
      FreeAndNil(ABitmap);
    end;
    FContainerItemLinks.Add(AColorItem);
    FMoreColorsButton := TdxBarButton(FContainerItemLinks.AddButton.Item);
    FMoreColorsButton.Caption := '&More Colors...';
    FMoreColorsButton.OnClick := MoreColorsClick;
    FColorDialogSetup := TdxBarButton(FContainerItemLinks.AddButton.Item);
    FColorDialogSetup.Caption := '&Setup...';
    FColorDialogSetup.OnClick := ColorDialogSetupButtonClick;
  end;

  procedure PopulateGalleries;
  begin
    BuildColorSchemeGallery;
    BuildStandardColorGallery;
  end;

  procedure SelectDefaultColor;
  begin
    FNoColorButton.Click;
  end;

begin
  inherited Create;
  FRibbon := ARibbon;
  FColorItem := AColorItem;
  FColorMapItem := AColorMapItem;
  FColorGlyphSize := cxTextHeight(FRibbon.Fonts.Group);
  FColorDialog := TdxColorDialog.Create(nil);
  FColorDialog.Options.ColorPicker.AllowEditAlpha := False;
  FColorDialog.Options.ColorPicker.DefaultVisible := True;
  FDefaultColor := ADefaultColor;
  FContainerItemLinks := GetContainerItemLinks(AContainerItem);

  InitColorMapItem;
  InitColorItem;
  InitDropDownGallery;
  PopulateGalleries;
  SelectDefaultColor;
end;

destructor TColorPickerController.Destroy;
begin
  FreeAndNil(FColorDialog);
  inherited;
end;

function TColorPickerController.AddColorItem(AGalleryGroup: TdxRibbonGalleryGroup; AColor: TColor): TdxRibbonGalleryGroupItem;
var
  ABitmap: TcxAlphaBitmap;
  AColorName: string;
begin
  Result := AGalleryGroup.Items.Add;

  ABitmap := CreateColorBitmap(AColor);
  try
    Result.Glyph.Assign(ABitmap);
    if cxNameByColor(AColor, AColorName) then
      Result.Caption := AColorName
    else
      Result.Caption := '$' + IntToHex(AColor, 6);
    Result.Tag := AColor;
  finally
    ABitmap.Free;
  end;
end;

function TColorPickerController.CreateColorBitmap(AColor: TColor; AGlyphSize: Integer): TcxAlphaBitmap;
begin
  if AGlyphSize = 0 then
    AGlyphSize := FColorGlyphSize;
  Result := TcxAlphaBitmap.CreateSize(AGlyphSize, AGlyphSize);
  FillRectByColor(Result.Canvas.Handle, Result.ClientRect, AColor);
  FrameRectByColor(Result.Canvas.Handle, Result.ClientRect, clGray);
  if AColor = clNone then
    Result.RecoverAlphaChannel(0)
  else
    Result.TransformBitmap(btmSetOpaque);
end;

procedure TColorPickerController.CreateColorRow(AGalleryGroup: TdxRibbonGalleryGroup; AColorMap: TColorMap);
var
  I: Integer;
begin
  for I := Low(AColorMap) to High(AColorMap) do
    AddColorItem(AGalleryGroup, AColorMap[I]);
end;

procedure TColorPickerController.BuildThemeColorGallery;

const
  AnAccentCount = 5;

  function GetBrightness(ARGBColor: DWORD): Integer;
  begin
    Result := (GetBValue(ARGBColor) + GetGValue(ARGBColor) + GetRValue(ARGBColor)) div 3;
  end;

  procedure GetAccentColorScheme(AColorMap: TColorMap; var AnAccentColorScheme: array of TColorMap);

    procedure CreateAccent(AnAccents: array of TAccent; AMapIndex: Integer);
    var
      I: Integer;
      AColor: TColor;
    begin
      for I := Low(AnAccents) to High(AnAccents) do
      begin
        case AnAccents[I] of
          aLight80:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 80);
          aLight60:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 60);
          aLight50:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 50);
          aLight40:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 40);
          aLight35:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 35);
          aLight25:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 25);
          aLight15:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 15);
          aLight5:
            AColor := dxGetLighterColor(AColorMap[AMapIndex], 5);
          aDark10:
            AColor := dxGetDarkerColor(AColorMap[AMapIndex], 90);
          aDark25:
            AColor := dxGetDarkerColor(AColorMap[AMapIndex], 75);
          aDark50:
            AColor := dxGetDarkerColor(AColorMap[AMapIndex], 50);
          aDark75:
            AColor := dxGetDarkerColor(AColorMap[AMapIndex], 25);
        else {aDark90}
          AColor := dxGetDarkerColor(AColorMap[I], 10);
        end;
        AnAccentColorScheme[I][AMapIndex] := AColor;
      end;
    end;

  var
    I: Integer;
  begin
    for I := Low(AColorMap) to High(AColorMap) do
      if GetBrightness(ColorToRGB(AColorMap[I])) < 20 then
        CreateAccent([aLight50, aLight35, aLight25, aLight15, aLight5], I)
      else
        if GetBrightness(ColorToRGB(AColorMap[I])) < 230 then
          CreateAccent([aLight80, aLight60, aLight60, aDark25, aDark50], I)
        else
          CreateAccent([aDark10, aDark25, aDark50, aDark75, aDark90], I)
  end;

var
  I: Integer;
  AColorMap: TColorMap;
  AnAccentColorScheme: array [0..AnAccentCount-1] of TColorMap;
begin
  BarManager.BeginUpdate;
  try
    FThemeColorsGroup.Items.Clear;
    AColorMap := ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map;
    CreateColorRow(FThemeColorsGroup, AColorMap);

    FAccentColorsGroup.Items.Clear;
    GetAccentColorScheme(AColorMap, AnAccentColorScheme);
    for I := Low(AnAccentColorScheme) to High(AnAccentColorScheme) do
      CreateColorRow(FAccentColorsGroup, AnAccentColorScheme[I]);
  finally
    BarManager.EndUpdate;
  end;
end;

procedure TColorPickerController.BuildStandardColorGallery;
begin
  BarManager.BeginUpdate;
  try
    FStandardColorsGroup.Items.Clear;
    CreateColorRow(FStandardColorsGroup, StandardColorMap);
  finally
    BarManager.EndUpdate;
  end;
end;

procedure TColorPickerController.BuildColorSchemeGallery;
const
  ASystemColorCount = 2;
  AGlyphOffset = 1;
var
  I, J: Integer;
  ABitmap, AColorBitmap: TcxAlphaBitmap;
  ARect: TRect;
  AGroupItem: TdxRibbonGalleryGroupItem;
  AThemeColorCount: Integer;
begin
  BarManager.BeginUpdate;
  try
    AThemeColorCount := SchemeColorCount - ASystemColorCount;
    ABitmap := TcxAlphaBitmap.CreateSize(FColorGlyphSize * AThemeColorCount + (AThemeColorCount - 1) * AGlyphOffset, FColorGlyphSize);
    try
      for I := High(ColorMaps) downto Low(ColorMaps) do
      begin
        AGroupItem := FColorMapItem.GalleryGroups[0].Items.Insert(0);
        for J := Low(ColorMaps[I].Map) + ASystemColorCount to High(ColorMaps[I].Map) do
        begin
          AColorBitmap := CreateColorBitmap(ColorMaps[I].Map[J]);
          try
            ARect := cxRectOffset(AColorBitmap.ClientRect, (AColorBitmap.Width + AGlyphOffset) * (J - ASystemColorCount), 0);
            ABitmap.CopyBitmap(AColorBitmap, ARect, cxNullPoint);
          finally
            AColorBitmap.Free;
          end;
        end;
        AGroupItem.Glyph.Assign(ABitmap);
        AGroupItem.Caption := ColorMaps[I].Name;
      end;
      AGroupItem.Selected := True;
    finally
      ABitmap.Free;
    end;
  finally
    BarManager.EndUpdate;
  end;
end;

procedure TColorPickerController.ColorChanged;
var
  AGlyph: TcxAlphaBitmap;
begin
  AGlyph := CreateColorBitmap(Color, Round(16 * DPIRatio));
  try
    FColorItem.Glyph.Assign(AGlyph);
  finally
    AGlyph.Free;
  end;

  if Assigned(OnColorChanged) then
    OnColorChanged(Self);
end;

procedure TColorPickerController.ColorMapChanged;

  procedure FillGlyph(AGlyph: TcxAlphaBitmap);
  var
    ARect: TRect;
    ADC: HDC;
  begin
    ARect := Rect(0, 0, AGlyph.Width div 2, AGlyph.Height div 2);
    ADC := AGlyph.Canvas.Handle;
    FillRectByColor(ADC, ARect, ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[2]);
    FillRectByColor(ADC, cxRectOffset(ARect, cxRectWidth(ARect), 0), ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[3]);
    FillRectByColor(ADC, cxRectOffset(ARect, 0, cxRectHeight(ARect)), ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[4]);
    FillRectByColor(ADC, cxRectOffset(ARect, cxRectWidth(ARect), cxRectHeight(ARect)),
      ColorMaps[FColorMapItem.SelectedGroupItem.Index].Map[5]);
    FrameRectByColor(ADC, AGlyph.ClientRect, clGray);
    AGlyph.TransformBitmap(btmSetOpaque);
  end;

var
  AGlyph: TcxAlphaBitmap;
begin
  BarManager.BeginUpdate;
  try
    AGlyph := TcxAlphaBitmap.CreateSize(16, 16);
    FillGlyph(AGlyph);
    FColorMapItem.Glyph.Assign(AGlyph);
    AGlyph.SetSize(32, 32);
    FillGlyph(AGlyph);
    FColorMapItem.LargeGlyph.Assign(AGlyph);
    AGlyph.Free;
  finally
    BarManager.EndUpdate(False);
  end
end;

function TColorPickerController.GetBarManager: TdxBarManager;
begin
  Result := FColorItem.BarManager;
end;

procedure TColorPickerController.SetColor(Value: TColor);
begin
  FColor := Value;
  ColorChanged;
end;

procedure TColorPickerController.ColorItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
begin
  FNoColorButton.Down := False;
  if FColorItem.SelectedGroupItem <> nil then
    SetColor(FColorItem.SelectedGroupItem.Tag);
end;

procedure TColorPickerController.ColorMapItemClick(Sender: TdxRibbonGalleryItem; AItem: TdxRibbonGalleryGroupItem);
begin
  BuildThemeColorGallery;
  ColorMapChanged;
end;

procedure TColorPickerController.NoColorButtonClick(Sender: TObject);
begin
  if FColorItem.SelectedGroupItem <> nil then
    FColorItem.SelectedGroupItem.Selected := False;
  SetColor(FDefaultColor);
end;

procedure TColorPickerController.MoreColorsClick(Sender: TObject);
begin
  FColorDialog.Color := dxColorToAlphaColor(Color);
  if FColorDialog.Execute then
  begin
    FCustomColorsGroup.Header.Visible := True;
    AddColorItem(FCustomColorsGroup, dxAlphaColorToColor(FColorDialog.Color)).Selected := True;
  end;
end;

procedure TColorPickerController.ColorDialogSetupButtonClick(Sender: TObject);
var
  RemoveHorizontalPadding, RemoveVerticalPadding: Boolean;
begin
  RemoveHorizontalPadding := FColorItem.GalleryOptions.SpaceBetweenItemsHorizontally = -1;
  RemoveVerticalPadding := FColorItem.GalleryGroups[1].Options.SpaceBetweenItemsVertically = -1;
  if ColorDialogSetupForm.GetSettings(RemoveHorizontalPadding, RemoveVerticalPadding) then
  begin
    FColorItem.GalleryOptions.SpaceBetweenItemsHorizontally := -Ord(RemoveHorizontalPadding);
    FColorItem.GalleryGroups[1].Options.SpaceBetweenItemsVertically := -Ord(RemoveVerticalPadding);
  end;
end;

{ TPopupColorPickerController }

constructor TPopupColorPickerController.Create(APopupMenu: TdxRibbonPopupMenu;
  AColorItem, AColorMapItem: TdxRibbonGalleryItem; ARibbon: TdxCustomRibbon;
  ADefaultColor: TColor; ADefaultColorCaption: string);
begin
  inherited Create(APopupMenu, AColorItem, AColorMapItem,
    ARibbon, ADefaultColor, ADefaultColorCaption);
end;

function TPopupColorPickerController.GetContainerItemLinks(AContainerItem: TObject): TdxBarItemLinks;
begin
  Result := TdxRibbonPopupMenu(AContainerItem).ItemLinks;
end;

{ TSubItemColorPickerController }

constructor TSubItemColorPickerController.Create(ASubItem: TdxBarSubItem;
  AColorItem, AColorMapItem: TdxRibbonGalleryItem; ARibbon: TdxCustomRibbon;
  ADefaultColor: TColor; ADefaultColorCaption: string);
begin
  inherited Create(ASubItem, AColorItem, AColorMapItem,
    ARibbon, ADefaultColor, ADefaultColorCaption);
end;

function TSubItemColorPickerController.GetContainerItemLinks(AContainerItem: TObject): TdxBarItemLinks;
begin
  Result := TdxBarSubItem(AContainerItem).ItemLinks;
end;

end.
