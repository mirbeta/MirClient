{***************************************************************************}
{ TAdvPicture                                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   copyright © 2001 - 2015                                                 }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvPicture;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, ActiveX, SysUtils, ExtDlgs, Dialogs;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.3.1.0 : New: added property PopupMenu to TAdvPicture
  // v1.3.1.1 : Improved: stretched painting
  // v1.3.1.2 : Fixed: issue with animated GIFs in Delphi 2009
  // v1.3.1.3 : Fixed: issue with transparent animated GIFs
  // v1.3.2.0 : New : added support to assign from TPicture
  // v1.4.0.0 : New : added support for OLE storage in TDBAdvPicture
  // v1.4.1.0 : Improved : handling of loading new picture while animation is on
  // v1.4.1.1 : Fixed : issue for animated GIFS using frame disposal = 4
  // v1.4.2.0 : New : public property DefaultDisposal added
  // v1.4.2.1 : Fixed : Implemented workaround for incorrect frametimes in animated gifs
  // v1.4.3.0 : New : Delphi XE5 & C++Builder XE5 support
  // v1.5.0.0 : New : EmptyText property added
  //          : New : AllowSelectOnClick property added
  //          : New : BorderStyle, BorderColor, BorderPenStyle properties added

type
  TPicturePosition = (bpTopLeft,bpTopRight,bpBottomLeft,bpBottomRight,bpCenter,
                      bpTiled,bpStretched,bpStretchedWithAspect);

  TStretchMode = (smNever,smShrink);

  TImageFileType = (itJPEG,itBMP,itPNG,itGIF,itEMF,itAll,itICO);
  TImageFileTypes = set of TImageFileType;

  TBorderStyle = (bDesign, bAlways, bEmpty, bNever);

  TPictureSelect = procedure(Sender: TObject; FileName: string; var Allow: boolean) of object;
  TPictureSelected = procedure(Sender: TObject; FileName: string) of object;

  TIPicture = class;

  TIPicture = class(TGraphic)
  private
    { Private declarations }
    gpPicture:IPicture;
    FDatastream:TMemoryStream;
    FIsEmpty:Boolean;
    FStretched:Boolean;
    FLogPixX,FLogPixY:Integer;
    FID:string;
    FFrame: Integer;
    FFrameCount: Integer;
    FOnFrameChange: TNotifyEvent;
    FFrameXPos: Word;
    FFrameYPos: Word;
    FFrameXSize: Word;
    FFrameYSize: Word;
    FFrameTransp: Boolean;
    FFrameDisposal: Word;
    FAnimMaxX,FAnimMaxY: Word;
    FDoubleBuffered: Boolean;
    FBackgroundColor: TColor;
    FOnClear: TNotifyEvent;
    FOnBeforeLoad: TNotifyEvent;
    FOnAfterLoad: TNotifyEvent;
    procedure LoadPicture;
    function GetFrameCount: Integer;
    function IsGIFFile: Boolean;
    function GetFrameTime(i: Integer): Integer;
  protected
    { Protected declarations }
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure SetFrame(const Value:Integer);
    procedure BeforeLoad;
    procedure AfterLoad;
  public
    { Public declarations }
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure LoadFromURL(url:string);
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property ID:string read fID write fID;
    property IsGIF: Boolean read IsGIFFile;
    property FrameCount:Integer read GetFrameCount;
    property FrameTime[i:Integer]:Integer read GetFrameTime;
    function GetMaxHeight: Integer;
    function GetMaxWidth: Integer;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
  published
    { Published declarations }
    property Stretch:Boolean read FStretched write FStretched;
    property Frame:Integer read FFrame write SetFrame;
    property OnFrameChange: TNotifyEvent read FOnFrameChange write FOnFrameChange;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnBeforeLoad: TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnAfterLoad: TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
  end;

  THelperWnd = class(TWinControl)
  private
    FOnTimer: TNotifyEvent;
    procedure WMTimer(var Msg:TMessage); message WM_TIMER;
    procedure WMDestroy(var Msg:TMessage); message WM_DESTROY;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
  published
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPicture = class(TGraphicControl)
  private
    { Private declarations }
    FAnimate: Boolean;
    FAutoSize: Boolean;
    FIPicture: TIPicture;
    FPicturePosition: TPicturePosition;
    FHelperWnd: THelperWnd;
    FTimerCount: Integer;
    FNextCount: Integer;
    FAnimatedGif: Boolean;
    FOnFrameChange: TNotifyEvent;
    FStretchMode: TStretchMode;
    FDoubleBuffered: Boolean;
    FBackgroundColor: TColor;
    FRefresh: Boolean;
    FOldAnimate: Boolean;
    FDefaultDisposal: integer;
    FImageTypes: TImageFileTypes;
    FAllowSelectOnClick: boolean;
    FAllowDropFiles: boolean;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FBorderPenStyle: TPenStyle;
    FEmptyText: string;
    FOnPictureSelected: TPictureSelected;
    FOnPictureSelect: TPictureSelect;
    procedure SetAutoSizeP(const Value: Boolean);
    procedure SetIPicture(const Value: TIPicture);
    procedure PictureChanged(Sender:TObject);
    procedure PictureCleared(Sender:TObject);
    procedure FrameChanged(Sender:TObject);
    procedure BeforeLoadPicture(Sender: TObject);
    procedure AfterLoadPicture(Sender: TObject);
    procedure Timer(Sender:TObject);
    procedure SetPicturePosition(const Value: TPicturePosition);
    procedure SetAnimate(const Value: Boolean);
    procedure SetStretchMode(const Value: TStretchMode);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDoubleBuffered(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    //procedure WMDropFiles(var Message: TMessage); message WM_DROPFILES;
    //procedure SetAllowDropFiles(const Value: boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderPenStyle(const Value: TPenStyle);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetEmptyText(const Value: string);
  protected
    { Protected declarations }
    procedure Click; override;
    procedure Paint; override;
    procedure PictureChange; virtual;
    procedure DoPictureSelect(FileName: string; var Allow: boolean); virtual;
    procedure DoPictureSelected(FileName: string); virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    //procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Loaded; override;
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property DefaultDisposal: integer read FDefaultDisposal write FDefaultDisposal;
  published
    { Published declarations }
    property AllowSelectOnClick: boolean read FAllowSelectOnClick write FAllowSelectOnClick default false;
    //property AllowDropFiles: boolean read FAllowDropFiles write SetAllowDropFiles default false;
    property Animate: Boolean read FAnimate write SetAnimate;
    property AutoSize: Boolean read FAutoSize write SetAutoSizeP default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bDesign;
    property BorderPenStyle: TPenStyle read FBorderPenStyle write SetBorderPenStyle default psDashDot;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property ImageTypes: TImageFileTypes read FImageTypes write FImageTypes;
    property Picture: TIPicture read FIPicture write SetIPicture;
    property PicturePosition: TPicturePosition read FPicturePosition write SetPicturePosition;
    { inherited published properties}
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Font;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StretchMode: TStretchMode read FStretchMode write SetStretchMode default smNever;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnStartDrag;
    property OnFrameChange: TNotifyEvent read FOnFrameChange write FOnFrameChange;
    property OnPictureSelect: TPictureSelect read FOnPictureSelect write FOnPictureSelect;
    property OnPictureSelected: TPictureSelected read FOnPictureSelected write FOnPictureSelected;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

uses
  Clipbrd, ShellApi
{$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
{$ENDIF}
  ;

const
  HIMETRIC_INCH = 2540;
  TIMER_ID = 500;

{ TIPicture }

procedure TIPicture.AfterLoad;
begin
  if Assigned(OnAfterLoad) then
    OnAfterLoad(Self);
end;

procedure TIPicture.Assign(Source: TPersistent);
var
  ms: TMemoryStream;
  clp: TClipboard;
  Data: THandle;
  DataPtr: pointer;

begin
  FIsEmpty := True;
  FFrameCount := -1;
  gpPicture := nil;
  if (Source = nil) then
  begin
    FDataStream.Clear;
    FIsEmpty := true;
    if Assigned(OnChange) then
      OnChange(Self);
    if Assigned(OnClear) then
      OnClear(self);
  end
  else
  begin
    if (Source is TIPicture) then
    begin
      FDataStream.LoadFromStream(TIPicture(Source).FDataStream);
      FIsEmpty := False;
      LoadPicture;
      if Assigned(OnChange) then
        OnChange(self);
    end;

    if (Source is TBitmap) then
    begin
      ms := TMemoryStream.Create;
      (Source as TBitmap).SaveToStream(ms);
      ms.Position := 0;
      FDataStream.LoadFromStream(ms);
      ms.Free;
      FIsEmpty := false;
      if Assigned(OnChange) then
        OnChange(self);
    end;

    if (Source is TClipboard) then
    begin
      clp := (Source as TClipboard);
      clp.Open;

      Data := GetClipboardData(CF_DIB);

      if Data = 0 then
      begin
        clp.Close;
        Exit;
      end;

      DataPtr := GlobalLock(Data);

      if DataPtr = nil then
      begin
        clp.Close;
        Exit;
      end;

      ms := TMemoryStream.Create;
      try
        ms.WriteBuffer(DataPtr^, GlobalSize(Data));

        ms.Position := 0;

        FDataStream.LoadFromStream(ms);

        FIsEmpty := false;
        if Assigned(OnChange) then
          OnChange(self);
      finally
        GlobalUnlock(Data);
        ms.Free;
        clp.Close;
      end;
    end;

    if (Source is TGraphic) then
    begin
      ms := TMemoryStream.Create;
      (Source as TGraphic).SaveToStream(ms);
      ms.Position := 0;
      FDataStream.LoadFromStream(ms);
      ms.Free;
      FIsEmpty := false;
      if Assigned(OnChange) then
        OnChange(self);
    end;

    if Source is TPicture then
    begin
      ms := TMemoryStream.Create;
      (Source as TPicture).Graphic.SaveToStream(ms);
      ms.Position := 0;
      FDataStream.LoadFromStream(ms);
      if (ms.Size > 0) then
      begin
        FIsEmpty := False;
        LoadPicture;
      end;
      if Assigned(OnChange) then
        OnChange(self);
      ms.Free;
    end;


  end;
end;

procedure TIPicture.BeforeLoad;
begin
  if Assigned(OnBeforeLoad) then
    OnBeforeLoad(Self);
end;

constructor TIPicture.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FIsEmpty := True;
  gpPicture := nil;
  FLogPixX := 96;
  FLogPixY := 96;
  FFrame := 0;
  FFrameCount := -1;

end;

destructor TIPicture.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure TIPicture.SetFrame(const Value:Integer);
begin
  FFrame := Value;
  if (FDataStream.Size > 0) then
  begin
    LoadPicture;
    if Assigned(OnFrameChange) then
      OnFrameChange(self);
  end;
end;

procedure TIPicture.LoadPicture;
const
  IID_IPicture: TGUID = (
  D1:$7BF80980;D2:$BF32;D3:$101A;D4:($8B,$BB,$00,$AA,$00,$30,$0C,$AB));
var
  hGlobal: THandle;
  pvData: pointer;
  pstm: IStream;
  hr: HResult;
  gifstream: TMemoryStream;
  i: Integer;
  b,c,d,e: byte;
  skipimg: Boolean;
  imgidx: Integer;

begin
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, FDataStream.Size);
  if (hGlobal = 0) then
    raise Exception.Create('Could not allocate memory for image');

  pvData := GlobalLock(hGlobal);
  FDataStream.Position := 0;

  FFrameXPos := 0;
  FFrameYPos := 0;
  FAnimMaxX := 0;
  FAnimMaxY := 0;

  //skip first image ctrl
  if IsGIF and (FrameCount>0) then
  begin
    //manipulate the stream here for animated GIF ?
    Gifstream := TMemoryStream.Create;
    imgidx := 1;
    skipimg := false;

    FDataStream.Position := 6;
    FDataStream.Read(FAnimMaxX,2);
    FDataStream.Read(FAnimMaxY,2);

    for i := 1 to FDataStream.Size do
    begin
      FDataStream.Position:=i-1;
      FDataStream.Read(b,1);

      if (b = $21) and (i+8 < FDataStream.Size) then
      begin
        FDataStream.Read(c,1);
        FDataStream.Read(d,1);
        FDataStream.Position := FDataStream.Position + 5;

        FDataStream.Read(e,1);
        if (c = $F9) and (d = $4) and (e = $2C) then
        begin
          if imgidx = fFrame then
          begin
            FDataStream.Read(FFrameXPos,2);
            FDataStream.Read(FFrameYPos,2);
            FDataStream.Read(FFrameXSize,2);
            FDataStream.Read(FFrameYSize,2);
          end;

          inc(imgidx);
          if imgidx <= fFrame then
            skipimg := true
          else
            skipimg := false;
        end;
      end;
      if not skipimg then
        GifStream.write(b,1);
    end;
    GifStream.Position := 0;
    GifStream.ReadBuffer(pvData^,GifStream.Size);
    GifStream.Free;
  end
  else
    FDataStream.ReadBuffer(pvData^,fDataStream.Size);

  GlobalUnlock(hGlobal);

  pstm := nil;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if not (hr = S_OK) then
  begin
    GlobalFree(hGlobal);
    raise Exception.Create('Could not create image stream')
  end
  else
    if (pstm = nil) then
      raise Exception.Create('Empty image stream created');

  // Create IPicture from image file
  hr := OleLoadPicture(pstm,
                       FDataStream.Size,
                       FALSE,
                       IID_IPicture,
                       gpPicture);

  pstm := nil;

  if not (hr = S_OK) then
    raise Exception.Create('Could not load image. Invalid format')
  else
    if (gpPicture = nil) then
      raise Exception.Create('Could not load image');
end;

procedure TIPicture.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  hmWidth: Integer;
  hmHeight: Integer;
  DrwRect: TRect;
  bmp: TBitmap;

begin
  if Empty then
    Exit;

  if gpPicture = nil then
    Exit;

  hmWidth  := 0;
  hmHeight := 0;
  gpPicture.get_Width(hmWidth);
  gpPicture.get_Height(hmHeight);

  DrwRect := Rect;

  OffsetRect(DrwRect,FFrameXPos,FFrameYPos);

  if FDoubleBuffered then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Width := self.Width;
      bmp.Height := self.Height;
      bmp.Canvas.Brush.Color := FBackgroundColor;
      bmp.Canvas.Pen.Color := FBackgroundColor;
      bmp.Canvas.Rectangle(0,0,self.Width,self.Height);
      gpPicture.Render(bmp.Canvas.Handle,DrwRect.Left,DrwRect.Bottom,DrwRect.Right-DrwRect.Left,
                      -(DrwRect.Bottom-DrwRect.Top),0,0, hmWidth,hmHeight, DrwRect);

      ACanvas.Draw(0,0,bmp);
    finally
      bmp.Free;
    end;
  end
  else
  begin
    gpPicture.Render(ACanvas.Handle,DrwRect.Left,DrwRect.Bottom,DrwRect.Right-DrwRect.Left,
                     -(DrwRect.Bottom-DrwRect.Top),0,0, hmWidth,hmHeight, DrwRect);
  end;
end;

function TIPicture.GetEmpty: Boolean;
begin
  Result := FIsEmpty;
end;

function TIPicture.GetHeight: Integer;
var
  hmHeight: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    gpPicture.get_Height(hmHeight);
    Result := MulDiv(hmHeight, FLogPixY, HIMETRIC_INCH);
  end;
end;

function TIPicture.GetWidth: Integer;
var
  hmWidth: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    gpPicture.get_Width(hmWidth);
    Result := MulDiv(hmWidth, fLogPixX, HIMETRIC_INCH);
  end;
end;

procedure TIPicture.LoadFromFile(const FileName: string);
begin
  BeforeLoad;

  try
    FDataStream.LoadFromFile(Filename);
    FIsEmpty := False;
    FFrame := 1;
    FAnimMaxX := 0;
    FAnimMaxY := 0;
    LoadPicture;

    if Assigned(OnClear) then
      OnClear(self);

    if Assigned(OnChange) then
      OnChange(self);
  except
    FIsEmpty := true;
  end;

  AfterLoad;
end;

procedure TIPicture.LoadFromStream(Stream: TStream);
begin
  BeforeLoad;

  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
    FFrame := 1;
    FAnimMaxX := 0;
    FAnimMaxY := 0;
    LoadPicture;
    if Assigned(OnChange) then
      OnChange(self);
  end;
  AfterLoad;
end;

procedure TIPicture.ReadData(Stream: TStream);
begin
  if assigned(Stream) then
  begin
    FDataStream.LoadFromStream(stream);
    FIsEmpty := False;
    LoadPicture;
  end;
end;

procedure TIPicture.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then
    FDataStream.SaveToStream(Stream);
end;

procedure TIPicture.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  if FindResource(Instance,PChar(ResName),RT_RCDATA) <> 0 then
  begin
    Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TIPicture.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


procedure TIPicture.SetHeight(Value: Integer);
begin

end;

procedure TIPicture.SetWidth(Value: Integer);
begin

end;

procedure TIPicture.WriteData(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.SaveToStream(stream);
  end;
end;

procedure TIPicture.LoadFromURL(url: string);
begin
  if (pos('RES://',UpperCase(url))=1) then
  begin
    ID := url;
    Delete(url,1,6);
    if (url<>'') then
      LoadFromResourceName(hinstance,url);
    Exit;
  end;

  if (pos('FILE://',uppercase(url))=1) then
  begin
    ID:=url;
    Delete(url,1,7);
    if (url<>'')
      then LoadFromFile(url);
  end;
end;

procedure TIPicture.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
end;

procedure TIPicture.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
end;

function TIPicture.GetFrameCount: Integer;
var
  i: Integer;
  b,c,d,e: Byte;
  Res: Integer;
begin
  Result := -1;

  if FFrameCount <> -1 then
    Result := FFrameCount
  else
    if IsGIFFile then
    begin
      Res := 0;
      for i := 1 to FDataStream.Size do
      begin
        FDataStream.Position := i - 1;
        FDataStream.Read(b,1);
        if (b = $21) and (i + 8 < FDataStream.Size) then
        begin
          FDataStream.Read(c,1);
          FDataStream.Read(d,1);
          FDataStream.Position:=fDataStream.Position+5;
          FDataStream.Read(e,1);
          if (c = $F9) and (d = $4) and (e = $2C) then Inc(res);
        end;
      end;
      FFrameCount := Res;  // cached FrameCount value
      Result := Res;
      FDataStream.Position := 0;
    end;
end;

function TIPicture.IsGIFFile: Boolean;
var
  buf: array[0..4] of ansichar;
begin
  Result := False;
  if FDataStream.Size>4 then
  begin
    FDataStream.Position := 0;
    FDataStream.Read(buf,4);
    buf[4] := #0;
    {$IFDEF DELPHIXE4_LVL}
    Result := AnsiStrings.Strpas(buf) = 'GIF8';
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    Result := Strpas(buf) = 'GIF8';
    {$ENDIF}
    FDataStream.Position := 0;
  end;
end;

function TIPicture.GetFrameTime(i: Integer): Integer;
var
 j: Integer;
 b,c,d,e: Byte;
 res: Integer;
 ft: Word;

begin
  Result := -1;

  if IsGIFFile then
  begin
    Res := 0;
    for j := 1 to FDataStream.Size do
    begin
      fDataStream.Position := j-1;
      fDataStream.Read(b,1);
      if (b = $21) and (i + 8 < FDataStream.Size) then
      begin
        FDataStream.Read(c,1);
        FDataStream.Read(d,1);
        FDataStream.Read(b,1);
        {transp. flag here}

        FDataStream.Read(ft,2);
        FDataStream.Position:=fDataStream.Position+2;

        FDataStream.Read(e,1);
        if (c = $F9) and (d = $4) and (e = $2C) then
        begin
          Inc(res);
          if res = i then
          begin
            if ft = 0 then
              ft := 5;
            Result := ft;
            fFrameTransp := b and $01=$01;
            fFrameDisposal := (b shr 3) and $7;
          end;
        end;
      end;
    end;
  end;
  FDataStream.Position := 0;
end;

function TIPicture.GetMaxHeight: Integer;
var
  hmHeight: Integer;
begin
  if gpPicture = nil then Result := 0
  else
  begin
    if FAnimMaxY > 0 then
      Result := FAnimMaxY
    else
    begin
      gpPicture.get_Height(hmHeight);
      Result := MulDiv(hmHeight, fLogPixY, HIMETRIC_INCH);
    end;
  end;

end;

function TIPicture.GetMaxWidth: Integer;
var
  hmWidth: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    if FAnimMaxX > 0 then
      Result := FAnimMaxX
    else
    begin
      gpPicture.get_Width(hmWidth);
      Result := MulDiv(hmWidth, fLogPixX, HIMETRIC_INCH);
    end;
  end;
end;

{ TAdvPicture }

procedure TAdvPicture.AfterLoadPicture(Sender: TObject);
begin
  Animate := FOldAnimate;
end;

procedure TAdvPicture.BeforeLoadPicture(Sender: TObject);
begin
  FOldAnimate := Animate;
  Animate := false;
end;

procedure TAdvPicture.Click;
var
  pd: TOpenPictureDialog;
  fltr: string;
  ext: string;
  Allow: boolean;

  function AppendStr(res, s, split: string): string;
  begin
    if res = '' then
      Result := s
    else
      Result := res + split + s;
  end;

begin
  inherited;
  if AllowSelectOnClick then
  begin

    pd := TOpenPictureDialog.Create(Self);

    fltr := '';
    ext := '';

    if itJPEG in ImageTypes then
    begin
      fltr := AppendStr(fltr,'JPG Images (*.jpg)|*.jpg|JPEG Images (*.jpeg)|*.jpeg','|');
      ext := AppendStr(ext,'*.jpg;*.jpeg',';');
    end;

    if itGIF in ImageTypes then
    begin
      fltr := AppendStr(fltr,'GIF Images (*.gif)|*.gif','|');
      ext := AppendStr(ext,'*.gif',';');
    end;

    if itBMP in ImageTypes then
    begin
      fltr := AppendStr(fltr,'Bitmap images (*.bmp)|*.bmp','|');
      ext := AppendStr(ext,'*.bmp',';');
    end;

    if itPNG in ImageTypes then
    begin
      fltr := AppendStr(fltr,'PNG images (*.png)|*.png','|');
      ext := AppendStr(ext,'*.png',';');
    end;

    if itICO in ImageTypes then
    begin
      fltr := AppendStr(fltr,'Icons (*.ico)|*.ico','|');
      ext := AppendStr(ext,'*.ico',';');
    end;

    if itEMF in ImageTypes then
    begin
      fltr := AppendStr(fltr,'Enhanced metafiles (*.emf)|*.emf','|');
      ext := AppendStr(ext,'*.emf',';');
    end;

    if (itAll in ImageTypes) and (fltr <> '') then
    begin
      fltr := 'All (' + ext + ')|'+ext+'|'+fltr;
    end;

    pd.Filter := fltr;
    if pd.Execute then
    begin
      Allow := true;
      DoPictureSelect(pd.FileName, Allow);
      if Allow then
      begin
        Picture.LoadFromFile(pd.FileName);
        DoPictureSelected(pd.FileName);
      end;
    end;
    pd.Free;
  end;
end;

constructor TAdvPicture.Create(aOwner: TComponent);
begin
  inherited;
  FIPicture := TIPicture.Create;
  FIPicture.OnChange := PictureChanged;
  FIPicture.OnFrameChange := FrameChanged;
  FIPicture.OnClear := PictureCleared;
  FIPicture.OnBeforeLoad := BeforeLoadPicture;
  FIPicture.OnAfterLoad := AfterLoadPicture;
  Width := 100;
  Height := 100;
  FDefaultDisposal := -1;
  FAnimatedGIF := False;
  FAllowDropFiles := False;
  FAllowSelectOnClick := False;
  FImageTypes := [];
  FBorderColor := clBlack;
  FBorderStyle := bDesign;
  FBorderPenStyle := psDashDot;
end;

{
procedure TAdvPicture.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle,FAllowDropFiles);
end;
}

destructor TAdvPicture.Destroy;
begin
  FIPicture.Free;
  FIPicture := nil;
  inherited;
end;

procedure TAdvPicture.DoPictureSelect(FileName: string; var Allow: boolean);
begin
  if Assigned(OnPictureSelect) then
    OnPictureSelect(Self, FileName, Allow);
end;

procedure TAdvPicture.DoPictureSelected(FileName: string);
begin
  if Assigned(OnPictureSelected) then
    OnPictureSelected(Self, FileName);
end;

procedure TAdvPicture.Loaded;
begin
  inherited;
  FIPicture.fLogPixX := GetDeviceCaps(canvas.handle,LOGPIXELSX);
  FIPicture.fLogPixY := GetDeviceCaps(canvas.handle,LOGPIXELSY);
  if not FIPicture.Empty then
  begin
    FAnimatedGIF := FIPicture.IsGIF and (FIPicture.FrameCount > 1);
  end;
end;

procedure TAdvPicture.Paint;
var
  xo,yo: Integer;
  rx,ry: Double;
  NewWidth,NewHeight: Integer;
  delta,h: integer;
  r: TRect;

  function Max(a,b:Integer): Integer;
  begin
     if a > b then Result := a else Result := b;
  end;

begin
  inherited;

  if FRefresh then
    Exit;

  if not Visible then
    Exit;

  if not Assigned(FIPicture) then
    Exit;

  if not FIPicture.Empty then
  begin
    NewWidth := FIPicture.GetMaxWidth;
    NewHeight := FIPicture.GetMaxHeight;

    if (Width > 0) and (Height > 0) then
    begin
      rx := FIPicture.GetMaxWidth/Width;
      ry := FIPicture.GetMaxHeight/Height;

      if (rx > 1) or (ry > 1) then
      begin
        if rx > ry then
        begin
          NewHeight := Trunc(FIPicture.GetMaxHeight/rx);
          NewWidth := Width;
        end
        else
        begin
          NewWidth := Trunc(FIPicture.GetMaxWidth/ry);
          NewHeight := Height;
        end;
      end;
    end
    else
      Exit;

    case FPicturePosition of
    bpTopLeft:
    begin
      if FStretchMode = smNever then
        Canvas.Draw(0,0,FIPicture)
      else
        Canvas.StretchDraw(Rect(0,0,NewWidth,NewHeight),FIPicture)
    end;

    bpTopRight:
    begin
      if FStretchMode = smNever then
        Canvas.Draw(Max(0,Width - FIPicture.GetMaxWidth),0,FIPicture)
      else
        Canvas.StretchDraw(Rect(Max(0,Width - NewWidth),0,Max(0,Width - NewWidth)+NewWidth,NewHeight),FIPicture);
    end;
    bpBottomLeft:
    begin
      if FStretchMode = smNever then
        Canvas.Draw(0,Max(0,Height - FIPicture.GetMaxHeight),FIPicture)
      else
        Canvas.StretchDraw(Rect(0,Max(0,Height - NewHeight),NewWidth, Height),FIPicture);
    end;
    bpBottomRight:
    begin
      if FStretchMode = smNever then
        Canvas.Draw(Max(0,Width - FIPicture.GetMaxWidth),Max(0,Height - FIPicture.GetMaxHeight),FIPicture)
      else
        Canvas.StretchDraw(Rect(Max(0,Width - NewWidth),Max(0,Height - NewHeight),Width,Height),FIPicture);
    end;
    bpCenter:
    begin
      if FStretchMode = smNever then
        Canvas.Draw(Max(0,Width - FIPicture.GetMaxWidth) shr 1,Max(0,Height - FIPicture.GetMaxHeight) shr 1,FIPicture)
      else
        Canvas.StretchDraw(Rect(Max(0,Width - NewWidth) shr 1,Max(0,Height - NewHeight) shr 1,
                                (Max(0,Width - NewWidth) shr 1) + NewWidth,
                                (Max(0,Height - NewHeight) shr 1) + NewHeight),FIPicture);
    end;
    bpTiled:
    begin
      yo := 0;
      while yo < Height do
      begin
        xo := 0;
        while xo < Width do
        begin
          Canvas.Draw(xo,yo,FIPicture);
          xo := xo + FIPicture.GetMaxWidth;
        end;
        yo := yo + FIPicture.GetMaxHeight;
      end;
    end;

    bpStretched:
    begin
      delta := round((Height / FIPicture.GetMaxHeight)) + 1;

      Canvas.StretchDraw(Rect(0,-1,Width,Height + delta),FIPicture)
    end;

    bpStretchedWithAspect:
    begin
      rx := FIPicture.GetMaxWidth/Width;
      ry := FIPicture.GetMaxHeight/Height;

      if rx > ry then
        Canvas.StretchDraw(Rect(0,0,Width,Trunc(FIPicture.GetMaxHeight/rx)),FIPicture)
      else
        Canvas.StretchDraw(Rect(0,0,Trunc(FIPicture.GetMaxWidth/ry),Height),FIPicture);
    end;
    end;
  end;

  if ((csDesigning in ComponentState) and (BorderStyle <> bNever)) or (BorderStyle <> bDesign) then
  begin
    if (BorderStyle = bAlways) or ((BorderStyle = bEmpty) and Picture.Empty) then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := BorderPenStyle;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := BorderColor;
      Canvas.Rectangle(ClientRect);
    end;
  end;

  if (EmptyText <> '') and (Picture.Empty) then
  begin
    r := ClientRect;
    InflateRect(r, -4, -4);
    Canvas.Font.Assign(Font);

    h := DrawText(Canvas.Handle, PChar(EmptyText), Length(EmptyText),r,DT_CENTER or DT_WORDBREAK or DT_CALCRECT);

    r := ClientRect;
    r.Top := r.Top + (Height - h) div 2;
    InflateRect(r, -4, -4);

    DrawText(Canvas.Handle, PChar(EmptyText), Length(EmptyText),r,DT_CENTER or DT_WORDBREAK);
  end;

end;

procedure TAdvPicture.PictureChange;
begin

end;

procedure TAdvPicture.PictureChanged(sender: TObject);
begin
  if FAutoSize and not FIPicture.Empty then
    SetAutoSizeP(FAutoSize);

  if not FIPicture.Empty then
  begin
    FAnimatedGIF := FIPicture.IsGIF and (FIPicture.FrameCount>1);
    FIPicture.Frame := 0;
  end;
  PictureChange;
end;

procedure TAdvPicture.FrameChanged(sender: TObject);
var
  R: TRect;
  disp: integer;
begin
  disp := FIPicture.FFrameDisposal;

  if (disp = 0) and (FDefaultDisposal <> -1) then
    disp := FDefaultDisposal;

  case disp of
  0:RePaint;
  1:Invalidate;
  2:begin
      with FIPicture do
      begin
        R := Rect(FFrameXPos,FFrameXPos,FFrameXPos+FFrameXSize,FFrameYPos+FFrameYSize);
        if Parent.HandleAllocated then
          InvalidateRect(Parent.Handle,@R,true);
      end;
    end;
  3,4:Paint;
  end;
  if Assigned(FOnFrameChange) then
    FOnFrameChange(Self);
end;

procedure TAdvPicture.Timer(sender:TObject);
begin
  if not Assigned(FIPicture) then
    Exit;

  if FAnimatedGIF and not FIPicture.Empty then
  begin

    if FTimerCount = FNextCount then
    begin
      if FIPicture.Frame < FIPicture.FrameCount then
        FIPicture.Frame := FIPicture.Frame + 1
      else
        FIPicture.Frame := 1;

      FNextCount := FNextCount + FIPicture.FrameTime[FIPicture.Frame];
    end;
    Inc(FTimerCount);
  end;
end;

{
procedure TAdvPicture.WMDropFiles(var Message: TMessage);
var
  Files: TStringList;
  FileCount,Len,i: Integer;
  FileName: array[0..255] of Char;
  DefaultHandler: Boolean;
begin
  Files := TStringList.Create;
  try
    FileCount := DragQueryFile(Message.wParam, UINT(-1), nil, 0);
    for i := 0 to FileCount - 1 do
    begin
      Len := DragQueryFile(Message.wParam, I, FileName, 255);
      if Len > 0 then
      begin
        Files.Add(StrPas(FileName));

        DefaultHandler := True;
//        if Assigned(FOnFileDrop) then
//          FOnFileDrop(Self,StrPas(FileName),DefaultHandler);
        if DefaultHandler then
        begin
          Picture.LoadFromFile(FileName);
        end;
      end;
    end;
  finally
    Files.Free;
  end;
end;
}

procedure TAdvPicture.SetAutoSizeP(const Value: Boolean);
begin
  FAutoSize := Value;
  if FAutoSize and not FIPicture.Empty then
  begin
    Self.Width := FIPicture.Width;
    Self.Height := FIPicture.Height;
  end;
end;

procedure TAdvPicture.SetIPicture(const Value: TIPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

procedure TAdvPicture.SetPicturePosition(const Value: TPicturePosition);
begin
  if FPicturePosition <> Value then
  begin
    FPicturePosition := Value;
    Invalidate;
  end;
end;

{
procedure TAdvPicture.SetAllowDropFiles(const Value: boolean);
begin
  FAllowDropFiles := Value;
  if HandleAllocated then
    DragAcceptFiles(Handle, Value);
end;
}

procedure TAdvPicture.SetAnimate(const Value: Boolean);
begin
  if FAnimate <> Value then
  begin
    FAnimate := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FAnimate then
      begin
        FHelperWnd := THelperWnd.Create(nil);
        FHelperWnd.Parent := Self.Parent;
        FTimerCount := 0;
        FNextCount := 0;
        FHelperWnd.OnTimer := Timer;
      end
      else
      begin
        FHelperWnd.Free;
      end;
    end;
  end;
end;

procedure TAdvPicture.SetStretchMode(const Value: TStretchMode);
begin
  if FStretchMode <> Value then
  begin
    FStretchMode := Value;
    Invalidate;
  end;
end;

procedure TAdvPicture.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  FIPicture.BackgroundColor := Value;
end;

procedure TAdvPicture.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvPicture.SetBorderPenStyle(const Value: TPenStyle);
begin
  if (FBorderPenStyle <> Value) then
  begin
    FBorderPenStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvPicture.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvPicture.SetDoubleBuffered(const Value: Boolean);
begin
  FDoubleBuffered := Value;
  FIPicture.DoubleBuffered := Value;
end;

procedure TAdvPicture.SetEmptyText(const Value: string);
begin
  if (FEmptyText <> Value) then
  begin
    FEmptyText := Value;
    Invalidate;
  end;
end;

procedure TAdvPicture.PictureCleared(Sender: TObject);
begin
  FRefresh := True;
  Repaint;
  FRefresh := False;
end;

function TAdvPicture.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvPicture.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvPicture.SetVersion(const Value: string);
begin

end;

{ THelperWnd }

constructor THelperWnd.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure THelperWnd.CreateWnd;
begin
  inherited;
  SetTimer(Self.Handle,TIMER_ID,10,nil);
end;

destructor THelperWnd.Destroy;
begin
  inherited;
end;

procedure THelperWnd.WMDestroy(var Msg: TMessage);
begin
  KillTimer(Self.Handle,TIMER_ID);
  inherited;
end;

procedure THelperWnd.WMTimer(var Msg: TMessage);
begin
  if Assigned(FOnTimer) then
    FOnTimer(self);
end;

end.
