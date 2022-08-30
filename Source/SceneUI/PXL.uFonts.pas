unit PXL.uFonts;

interface

uses
    Windows, Types, Classes, SysUtils,PXL.Devices, uCommon,
    PXL.SwapChains,
    PXL.Canvas,
    Math,
    PXL.Textures,
    PXL.Providers,
    Graphics;
const
  DefaultFontName = 'ËÎÌו';
  DefaultFontSize = 9;
  DefaultFontStyle = [];
type
  TCustomFontTexture = class
  private
    FTexture: TCustomLockableTexture;
//    FDevice: TCustomDevice;
    FText: String;
    FOutTimeTick: LongWord;
    FOutTimeTime: LongWord;
  public
    constructor Create();
    destructor Destroy; override;
    property Texture: TCustomLockableTexture read FTexture write FTexture;
    property Text: String read FText write FText;
    property OutTimeTick: LongWord read FOutTimeTick write FOutTimeTick;
    property OutTimeTime: LongWord read FOutTimeTime write FOutTimeTime;
//    property Device: TCustomDevice read FDevice;
  end;

  TCustomFontTextures = class
  private
    FList: TList;
    FOutTimeTick: LongWord;
    FOutTimeTime: LongWord;
    function GetTexture(Index: Integer): TCustomFontTexture;
    function GetTextureCount(): Integer;
  public
    constructor Create();
    destructor Destroy; override;
    procedure FreeIdleMemory;
    procedure Clear;
    procedure Add(Texture: TCustomFontTexture);
    property Textures[Index: Integer]: TCustomFontTexture read GetTexture;
    property TextureCount: Integer read GetTextureCount;
    property OutTimeTick: LongWord read FOutTimeTick write FOutTimeTick;
    property OutTimeTime: LongWord read FOutTimeTime write FOutTimeTime;
  end;

  TFontManager = class;

  TCustomTextureFont = class(TFont)
  private
    FManager: TFontManager;
    FFontTextures: TCustomFontTextures;
    FDevice: TCustomDevice;
    FName: String;
    FAlias: String;
    procedure NewBitmapFile(const AWidth, AHeight, ABitCount: Integer; var FileData: Pointer; var FileSize: Integer);
    function GetFontTexture(const Text: String): TCustomLockableTexture;
  public
    constructor Create(AOwner: TFontManager; ADevice:TCustomDevice);
    destructor Destroy; override;
    procedure FreeIdleMemory;
    procedure Initialize;
    procedure Finalize;
    function GetTextTexture(const Text: String): TCustomLockableTexture;
    function TextHeight(const Text: String): Integer;
    function TextWidth(const Text: String): Integer;
    function TextExtent(const Text: String): TSize;
    function TextOut(const Text: String): TCustomLockableTexture; inline;
    property _Name: String read FName write FName;
    property Alias: String read FAlias write FAlias;
    property Device: TCustomDevice read FDevice;
  end;

  TOnGetTextExtentSize = procedure(const Text: String; Font: TFont; var Value: TSize) of Object;
  TFontManager = class
  private
    Fonts: TList;
    FActiveFont,
    FDefault: TCustomTextureFont;
    FGetTextExtent: TOnGetTextExtentSize;
    FDevice: TCustomDevice;
    procedure DoGetTextHeight(const Text: String; Font: TFont; var Value: Integer);
    procedure DoGetTextWidth(const Text: String; Font: TFont; var Value: Integer);
    procedure DoGetTextExtent(const Text: String; Font: TFont; var Value: TSize);
  public
    constructor Create(ADevice:TCustomDevice);
    destructor Destroy; override;
    procedure FreeIdleMemory;
    procedure Initialize;
    procedure Finalize;
    procedure RemoveAll();
    function Add(const AName: String; const FontName: TFontName; FontSize: Integer; FontStyles: TFontStyles; ADefault: Boolean): TCustomTextureFont;
    function FindByAlias(const AAlias: String): TCustomTextureFont;
    function GetFont(const AFontName: String; FontSize: Integer; FontStyles: TFontStyles): TCustomTextureFont;
    property Default: TCustomTextureFont read FDefault;
    property OnGetTextExtent: TOnGetTextExtentSize read FGetTextExtent write FGetTextExtent;
    property Device: TCustomDevice read FDevice;
  end;

var
  FontManager: TFontManager = nil;
implementation
  uses PXL.Types;

function GetFontStylesValue(AFontStyles: TFontStyles): Integer;
var
  I: TFontStyle;
begin
  Result := 0;
  for I in AFontStyles do
    Include(TIntegerSet(Result), Ord(I));
end;

{TCustomFontTexture}

constructor TCustomFontTexture.Create();
begin
  inherited Create;
  FTexture := nil;
  FText := '';
  FOutTimeTick := GetTickCount;
  FOutTimeTime := 1000 * 60 * 1;
end;

destructor TCustomFontTexture.Destroy;
begin
  if FTexture <> nil then
    FreeAndNil(FTexture);
  inherited;
end;

constructor TCustomFontTextures.Create();
begin
  inherited;
  FList := TList.Create;
  FOutTimeTick := GetTickCount;
  FOutTimeTime := 1000 * 20;
end;

destructor TCustomFontTextures.Destroy;
var
  I: Integer;
  Texture: TCustomFontTexture;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Texture := FList.Items[I];
    FreeAndNil(Texture);
  end;
  FList.Free;
  inherited;
end;

procedure TCustomFontTextures.Add(Texture: TCustomFontTexture);
begin
  FList.Add(Texture);
end;

procedure TCustomFontTextures.Clear;
var
  I: Integer;
  Texture: TCustomFontTexture;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Texture := FList.Items[I];
    FreeAndNil(Texture);
  end;
  FList.Clear;
end;

procedure TCustomFontTextures.FreeIdleMemory;
var
  I: Integer;
  Texture: TCustomFontTexture;
begin
  if GetTickCount - FOutTimeTick > FOutTimeTime then
  begin
    FOutTimeTick := GetTickCount;
    for I := FList.Count - 1 downto 0 do
    begin
      Texture := FList.Items[I];
      if GetTickCount - Texture.OutTimeTick > Texture.OutTimeTime then
      begin
        FreeAndNil(Texture);
        FList.Delete(I);
      end;
    end;
  end;
end;

function TCustomFontTextures.GetTexture(Index: Integer): TCustomFontTexture;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := FList.Items[Index]
  else
    Result := nil;
end;

function TCustomFontTextures.GetTextureCount(): Integer;
begin
  Result := FList.Count;
end;

{TCustomTextureFont}

constructor TCustomTextureFont.Create(AOwner: TFontManager; ADevice:TCustomDevice);
begin
  inherited Create;
  FDevice:= ADevice;
  FManager := AOwner;
  Name := 'ËÎÌו';
  Size := 9;
  Charset := GB2312_CHARSET;
  Style := [];
  FFontTextures := TCustomFontTextures.Create;
end;

destructor TCustomTextureFont.Destroy;
begin
  FFontTextures.Free;
  inherited;
end;

function TCustomTextureFont.GetTextTexture(const Text: String): TCustomLockableTexture;
begin
  Result := GetFontTexture(Text);
end;

procedure TCustomTextureFont.NewBitmapFile(const AWidth, AHeight, ABitCount: Integer; var FileData: Pointer; var FileSize: Integer);
var
  FileHeader: PBitmapFileHeader;
  InfoHeader: PBitmapInfoHeader;
  Buffer: Pointer;
  APicth: Integer;
begin
  WidthW(AWidth, ABitCount, APicth);
  FileSize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + APicth * AHeight;

  GetMem(FileData, FileSize);

  Buffer := FileData;
  FileHeader := PBitmapFileHeader(Buffer);
  FileHeader.bfType := 19778;
  FileHeader.bfSize := FileSize;
  FileHeader.bfOffBits := FileHeader.bfSize - AWidth * AHeight * (ABitCount div 8);
  FileHeader.bfReserved1 := 0;
  FileHeader.bfReserved2 := 0;

  Buffer := Pointer(Integer(Buffer) + SizeOf(TBitmapFileHeader));

  InfoHeader := PBitmapInfoHeader(Buffer);
  InfoHeader.biSize := SizeOf(TBitmapInfoHeader);
  InfoHeader.biWidth := AWidth;
  InfoHeader.biHeight := -AHeight;
  InfoHeader.biPlanes := 1;
  InfoHeader.biBitCount := ABitCount;

  InfoHeader.biCompression := BI_RGB;
  InfoHeader.biSizeImage := APicth * AHeight;
  InfoHeader.biXPelsPerMeter := 0;
  InfoHeader.biYPelsPerMeter := 0;
  InfoHeader.biClrUsed := 0;
  InfoHeader.biClrImportant := 0;
end;

function TCustomTextureFont.GetFontTexture(const Text: String): TCustomLockableTexture;
var
  I, X, Y: Integer;
  PBitmapBits: PIntegerArray;
  BitmapInfo: TBitmapInfo;
  HHBitmap: HBitmap;
  HHDC: HDC;
  FontTexture: TCustomFontTexture;
  Bits: Pointer;
  Pitch: Integer;
  DesP: Pointer;
  Pix: Word;
  W: PWord;
  FileData: Pointer;
  FileSize: Integer;
  ASize: TSize;
begin
  Result := nil;
  if Text = '' then
    Exit;
  try
    for I := 0 to FFontTextures.TextureCount - 1 do
    begin
      FontTexture := FFontTextures.Textures[I];
      if CompareStr(FontTexture.Text, Text) = 0 then
      begin
        FontTexture.OutTimeTick := GetTickCount;
        Result := FontTexture.Texture;
        Exit;
      end;
    end;

    ASize := TextExtent(Text);
    FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
    with BitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(BitmapInfo.bmiHeader);
      biWidth := ASize.cx;
      biHeight := -ASize.cy;
      biPlanes := 1;
      biBitCount := 16;
      biCompression := BI_RGB;
      biSizeImage := 0;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0;
    end;

    HHDC := CreateCompatibleDC(0);
    HHBitmap := CreateDIBSection(HHDC, BitmapInfo, DIB_RGB_COLORS, Pointer(PBitmapBits), 0, 0);

    SelectObject(HHDC, Handle);
    SelectObject(HHDC, HHBitmap);

    SetTextColor(HHDC, RGB(255, 255, 255));
    SetBkColor(HHDC, RGB(0, 0, 0));
    Windows.TextOut(HHDC, 0, 0, PChar(Text), Length(Text));

    NewBitmapFile(ASize.cx, ASize.cy, 16, FileData, FileSize);

    Bits := Pointer(Integer(FileData) + SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader));
    WidthW(ASize.cx, 16, Pitch);

    for Y := 0 to ASize.cy - 1 do
    begin
      W := Pointer(Integer(Pointer(PBitmapBits)) + Y * Pitch);
      DesP := PCardinal(Integer(Bits) + Y * Pitch);
      for X := 0 to ASize.cx - 1 do
      begin
        Pix := W^;
        if Pix > 0 then
          Pix := $FFFF;
        PWord(DesP)^ := Pix;
        Inc(W);
        Inc(PWord(DesP));
      end;
    end;

    FontTexture := TCustomFontTexture.Create;
    FontTexture.Text := Text;
    FontTexture.FTexture := PXL.Providers.TGraphicsDeviceProvider(FDevice.Provider).CreateLockableTexture(FDevice, False);

     if FontTexture.FTexture = nil then Exit;
    if FontTexture.FTexture <> nil then
    begin
      FontTexture.FTexture.Mipmapping := False;
      FontTexture.FTexture.PixelFormat := TPixelFormat.A1R5G5B5;
      FontTexture.FTexture.SetSize(ASize.cx, ASize.cy, False);
      if not FontTexture.FTexture.LoadFromFontData(Bits, FileSize - 54, ASize.cx, ASize.cy{SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader)}) then
        FreeAndNil(FontTexture)
      else
      begin
        FFontTextures.Add(FontTexture);
        Result := FontTexture.FTexture;
      end;
    end
    else
      FreeAndNil(FontTexture);
    FreeMem(FileData, FileSize);

    DeleteObject(HHBitmap);
    DeleteDC(HHDC);
  except
  end;
end;

procedure TCustomTextureFont.FreeIdleMemory;
begin
  FFontTextures.FreeIdleMemory;
end;

procedure TCustomTextureFont.Initialize;
begin
end;

procedure TCustomTextureFont.Finalize;
begin
  FFontTextures.Clear;
end;

function TCustomTextureFont.TextExtent(const Text: String): TSize;
begin
  FManager.DoGetTextExtent(Text, Self, Result);
end;

function TCustomTextureFont.TextHeight(const Text: String): Integer;
var
  ASize: TSize;
begin
  ASize := TextExtent(Text);
  Result := ASize.cy;
end;

function TCustomTextureFont.TextWidth(const Text: String): Integer;
var
  ASize: TSize;
begin
  ASize := TextExtent(Text);
  Result := ASize.cx;
end;

function TCustomTextureFont.TextOut(const Text: String): TCustomLockableTexture;
begin
  Result := GetFontTexture(Text);
end;

constructor TFontManager.Create(ADevice:TCustomDevice);
begin
  inherited Create;
  FDevice := ADevice;
  Fonts := TList.Create;
  FActiveFont := nil;
  Add('Default', 'ËÎÌו', 9, [], True);
end;

destructor TFontManager.Destroy();
begin
  RemoveAll();
  FreeAndNil(Fonts);
  inherited;
end;

procedure TFontManager.DoGetTextExtent(const Text: String; Font: TFont; var Value: TSize);
begin
  Value.cx := 0;
  Value.cy := 0;
  if Assigned(FGetTextExtent) then
    FGetTextExtent(Text, Font, Value);
end;

procedure TFontManager.DoGetTextHeight(const Text: String; Font: TFont; var Value: Integer);
var
  ASize: TSize;
begin
  DoGetTextExtent(Text, Font, ASize);
  Value := ASize.cy;
end;

procedure TFontManager.DoGetTextWidth(const Text: String; Font: TFont; var Value: Integer);
var
  ASize: TSize;
begin
  DoGetTextExtent(Text, Font, ASize);
  Value := ASize.cx;
end;

procedure TFontManager.RemoveAll();
var
  I: Integer;
begin
  for I := 0 to Fonts.Count - 1 do
    if (Fonts[I] <> nil) then
      TCustomTextureFont(Fonts[I]).Free;
  FDefault := nil;
  FActiveFont := nil;
  Fonts.Clear;
end;

function TFontManager.Add(const AName: String; const FontName: TFontName; FontSize: Integer; FontStyles: TFontStyles; ADefault: Boolean): TCustomTextureFont;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Fonts.Count - 1 do
  begin
    if SameText(TCustomTextureFont(Fonts[I]).FName, AName) then
    begin
      Result := TCustomTextureFont(Fonts[I]);
      Exit;
    end;
  end;
  Result := TCustomTextureFont.Create(Self,FDevice);
  Fonts.Add(Result);
  Result.Name := FontName;
  Result.FAlias := Format('%s_%d_%d', [FontName, FontSize, GetFontStylesValue(FontStyles)]);
  Result.Size := FontSize;
  Result.Style := FontStyles;
  Result.Initialize;
  if ADefault then
    FDefault := Result;
  if FActiveFont = nil then
    FActiveFont := Result;
end;

function TFontManager.FindByAlias(const AAlias: String): TCustomTextureFont;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Fonts.Count - 1 do
  begin
    if SameText(TCustomTextureFont(Fonts[I]).FAlias, AAlias) then
    begin
      Result := TCustomTextureFont(Fonts[I]);
      Exit;
    end;
  end;
end;

function TFontManager.GetFont(const AFontName: String; FontSize: Integer; FontStyles: TFontStyles): TCustomTextureFont;
var
  AAlias: String;
begin
  AAlias := AFontName + '_' + IntToStr(FontSize) + '_' + IntToStr(GetFontStylesValue(FontStyles));
  Result := FindByAlias(AAlias);
  if Result = nil then
    Result := Add(AAlias, AFontName, FontSize, FontStyles, False);
end;

procedure TFontManager.FreeIdleMemory;
var
  I: Integer;
begin
  for I := 0 to Fonts.Count - 1 do
    TCustomTextureFont(Fonts[I]).FreeIdleMemory;
end;

procedure TFontManager.Initialize;
var
  I: Integer;
begin
  for I := 0 to Fonts.Count - 1 do
    TCustomTextureFont(Fonts[I]).Initialize;
end;

procedure TFontManager.Finalize;
var
  I: Integer;
begin
  for I := 0 to Fonts.Count - 1 do
    TCustomTextureFont(Fonts[I]).Finalize;
end;

initialization
// FontManager := TFontManager.Create(EngineDevice);

finalization
//  FreeAndNil(FontManager);
end.





