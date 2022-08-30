{*************************************************************************}
{ TWebImage                                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{   TMS Software                                                          }
{   copyright © 2000 - 2014                                               }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit WebImage;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComObj,
  Activex, WinInet;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // Version history
  // 1.1.0.1 : Fixed issue with vertical stretch draw
  //         : Fixed issue with Visible property
  // 1.1.0.2 : Fixed issue with tiling
  // 1.2.0.0 : New : PicturePosition bpStretchedWithAspectRatio added
  // 1.2.0.1 : Fixed : Rare thread issue
  // 1.2.0.2 : Fixed : Issue with OnDownloadComplete

type
  TPicturePosition = (bpTopLeft,bpTopRight,bpBottomLeft,bpBottomRight,bpCenter,bpTiled,bpStretched, bpStretchedWithAspectRatio);

  TWebPicture = class;

  PInternetContent = ^TInternetContent;
  TInternetContent = record
    HResource: hinternet;
    Complete: Boolean;
    WebPicture: TWebPicture;
  end;

  TDownloadErrorEvent = procedure(Sender:TObject;err:string) of object;
  TDownloadCompleteEvent = procedure(Sender:TObject) of object;
  TDownloadCancelEvent = procedure(Sender:TObject;var Cancel:boolean) of object;
  TDownloadProgressEvent = procedure(Sender:TObject;dwSize,dwTotSize:Cardinal) of object;

  TDownLoadThread = class(TThread)
  private
    Webpicture:TWebPicture;
  protected
    procedure Execute; override;
    procedure UpdatePicture;
  public
    constructor Create(AWebPicture: TWebPicture);
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
  

  TWebPicture = class(TGraphic)
  private
    { Private declarations }
    gpPicture: IPicture;
    FDatastream: TMemoryStream;
    FIsEmpty: Boolean;
    FStretched: Boolean;
    FLogPixX,FLogPixY: Integer;
    FID: string;
    FURL: string;
    FFrame:Integer;
    FOnFrameChange: TNotifyEvent;
    FFrameXPos: Word;
    FFrameYPos: Word;
    FFrameXSize: Word;
    FFrameYSize: Word;
    FFrameTransp: Boolean;
    FFrameDisposal: Word;
    FAnimMaxX,FAnimMaxY: Word;
    FCheckContentLength: Boolean;
    FAsynch:Boolean;
    FThreadBusy:boolean;
    FMemStr: TMemoryStream;
    FOnDownLoadProgress: TDownLoadProgressEvent;
    FOnDownLoadCancel: TDownLoadCancelEvent;
    FOnDownLoadComplete: TDownLoadCompleteEvent;
    FOnDownLoadError: TDownLoadErrorEvent;
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
    procedure DownLoadError(err:string);
    procedure DownLoadComplete;
    procedure DownLoadCancel(var cancel:boolean);
    procedure DownLoadProgress(dwSize,dwTotSize:Cardinal);
    procedure DownLoad;
    procedure Update;
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
    property Busy: Boolean read FThreadBusy;
    property Asynch: Boolean read FAsynch write FAsynch;
    property CheckContentLength: Boolean read FCheckContentLength write FCheckContentLength;    
  published
    { Published declarations }
    property Stretch: Boolean read FStretched write FStretched;
    property Frame: Integer read FFrame write SetFrame;
    property OnFrameChange: TNotifyEvent read FOnFrameChange write FOnFrameChange;
    property OnDownLoadError: TDownLoadErrorEvent read FOnDownLoadError write FOnDownLoadError;
    property OnDownLoadComplete: TDownLoadCompleteEvent read FOnDownLoadComplete write FOnDownLoadComplete;
    property OnDownLoadCancel: TDownLoadCancelEvent read FOnDownLoadCancel write FOnDownLoadCancel;
    property OnDownLoadProgress: TDownLoadProgressEvent read FOnDownLoadProgress write FOnDownLoadProgress;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebImage = class(TGraphicControl)
  private
    { Private declarations }
    FAutoSize: Boolean;
    FWebPicture:TWebPicture;
    FPicturePosition:TPicturePosition;
    FOnDownLoadCancel: TDownLoadCancelEvent;
    FOnDownLoadComplete: TDownLoadCompleteEvent;
    FOnDownLoadError: TDownLoadErrorEvent;
    FOnDownLoadProgress: TDownLoadProgressEvent;
    FHelperWnd: THelperWnd;
    FTimerCount: Integer;
    FNextCount: Integer;
    FAnimatedGif: Boolean;
    FAnimate: Boolean;
    FOnFrameChange: TNotifyEvent;
    FURL: string;
    FAsynch: Boolean;
    FCheckContentLength: Boolean;
    procedure SetWebPicture(const Value: TWebPicture);
    procedure PictureChanged(sender: TObject);
    procedure SetPicturePosition(const Value: TPicturePosition);
    procedure DownLoadError(Sender: TObject;err: string);
    procedure DownLoadComplete(Sender: TObject);
    procedure DownLoadCancel(Sender: TObject;var cancel: Boolean);
    procedure DownLoadProgress(Sender: TObject;dwSize,dwTotSize:Cardinal);
    procedure SetAnimate(const Value: Boolean);
    procedure SetURL(const Value: string);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
     procedure Paint; override;
    procedure Timer(Sender:TObject);
    procedure FrameChanged(Sender:TObject);
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SaveToFile(Filename: string);
  published
    { Published declarations }
    property Animate: Boolean read FAnimate write SetAnimate;
    property Async: Boolean read FAsynch write FAsynch;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property CheckContentLength: Boolean read FCheckContentLength write FCheckContentLength;
    property WebPicture: TWebPicture read FWebPicture write SetWebPicture;
    property PicturePosition: TPicturePosition read FPicturePosition write SetPicturePosition;
    { inherited published properties}
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property ShowHint;
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
    property OnDownLoadError: TDownLoadErrorEvent read FOnDownLoadError write FOnDownLoadError;
    property OnDownLoadComplete: TDownLoadCompleteEvent read FOnDownLoadComplete write FOnDownLoadComplete;
    property OnDownLoadCancel: TDownLoadCancelEvent read FOnDownLoadCancel write FOnDownLoadCancel;
    property OnDownLoadProgress: TDownLoadProgressEvent read FOnDownLoadProgress write FOnDownLoadProgress;
    property OnFrameChange: TNotifyEvent read FOnFrameChange write FOnFrameChange;
    property URL: string read FURL write SetURL;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

const
  HIMETRIC_INCH = 2540;
  TIMER_ID = 500;  




{ TWebPicture }

procedure TWebPicture.Assign(Source: TPersistent);
begin
  FIsEmpty := true;
  gpPicture := nil;
  if (Source = nil) then
  begin
    FDataStream.Clear;
    if Assigned(OnChange) then OnChange(self);
  end
  else
  begin
    if (Source is TWebPicture) then
    begin
      FDataStream.LoadFromStream(TWebPicture(Source).FDataStream);
      FIsEmpty := False;
      LoadPicture;
      if Assigned(OnChange) then OnChange(self);
    end;
  end;
end;

constructor TWebPicture.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FIsEmpty := True;
  gpPicture := nil;
  FLogPixX := 96;
  FLogPixY := 96;
  FThreadBusy := False;
  FAsynch := True;
  FMemStr := nil;
end;

destructor TWebPicture.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure TWebPicture.LoadPicture;
const
  IID_IPicture: TGUID = (
  D1:$7BF80980;D2:$BF32;D3:$101A;D4:($8B,$BB,$00,$AA,$00,$30,$0C,$AB));

var
  hGlobal: THandle;
  pvData: Pointer;
  pstm: IStream;
  hr: HResult;
  gifstream: TMemoryStream;
  i: Integer;
  b,c,d,e: byte;
  skipimg: Boolean;
  imgidx: Integer;
begin
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, fDataStream.Size);
  if (hGlobal = 0) then raise Exception.Create('Could not allocate memory for image');

  pvData := GlobalLock(hGlobal);
  FDataStream.Position := 0;

  FFrameXPos := 0;
  FFrameYPos := 0;
  FAnimMaxX := 0;
  FAnimMaxY := 0;

  {skip first image ctrl}

  if IsGIF and (FrameCount>0) then
   begin
    //manipulate the stream here for animated GIF ?
    Gifstream := TMemoryStream.Create;
    imgidx := 1;
    skipimg := False;

    FDataStream.Position := 6;
    FDataStream.Read(FAnimMaxX,2);
    FDataStream.Read(FAnimMaxY,2);

    for i := 1 to FDataStream.Size do
    begin
      FDataStream.Position := i - 1;
      FDataStream.Read(b,1);


      if (b = $21) and (i + 8 < FDataStream.Size) then
      begin
        FDataStream.Read(c,1);
        FDataStream.Read(d,1);
        FDataStream.Position := FDataStream.Position + 5;
        FDataStream.Read(e,1);
        if (c=$F9) and (d=$4) and (e=$2C) then
        begin
          if imgidx = FFrame then
          begin
            FDataStream.Read(FFrameXPos,2);
            FDataStream.Read(FFrameYPos,2);
            FDataStream.Read(FFrameXSize,2);
            FDataStream.Read(FFrameYSize,2);
          end;

          inc(imgidx);
          if imgidx <= FFrame then
            skipimg := True
          else
            skipimg := False;
        end;
      end;
      if not skipimg then GifStream.write(b,1);
    end;
    GifStream.Position:=0;
    GifStream.ReadBuffer(pvData^,GifStream.Size);
    GifStream.Free;
  end
  else
    FDataStream.ReadBuffer(pvData^,FDataStream.Size);

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
                       fDataStream.Size,
                       FALSE,
                       IID_IPicture,
                       gpPicture);
  if not (hr = S_OK) then
    raise Exception.Create('Could not load image. Invalid format')
  else
    if (gpPicture = nil) then
      raise Exception.Create('Could not load image');
end;

procedure TWebPicture.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  hmWidth: Integer;
  hmHeight: Integer;
  DrwRect: TRect;

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
  gpPicture.Render(ACanvas.Handle,DrwRect.Left,DrwRect.Bottom,DrwRect.Right - DrwRect.Left,
                   -(DrwRect.Bottom - DrwRect.Top + 1),0,0, hmWidth,hmHeight, DrwRect);
end;

function TWebPicture.GetEmpty: Boolean;
begin
  Result := FIsEmpty;
end;

function TWebPicture.GetHeight: Integer;
var
  hmHeight:Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    gpPicture.get_Height(hmHeight);
    Result := MulDiv(hmHeight, FLogPixY, HIMETRIC_INCH);
  end;
end;

function TWebPicture.GetWidth: Integer;
var
  hmWidth: Integer;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    gpPicture.get_Width(hmWidth);
    Result := MulDiv(hmWidth, FLogPixX, HIMETRIC_INCH);
  end;
end;

procedure TWebPicture.LoadFromFile(const FileName: string);
begin
  try
    FDataStream.LoadFromFile(Filename);
    FIsEmpty := False;
    LoadPicture;
    if Assigned(OnChange) then
      OnChange(Self);
  except
    FIsEmpty := True;
  end;
end;

procedure TWebPicture.LoadFromStream(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
    LoadPicture;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TWebPicture.ReadData(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
    LoadPicture;
  end;
end;

procedure TWebPicture.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then
    FDataStream.SaveToStream(Stream);
end;

procedure TWebPicture.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  if FindResource(Instance,pchar(ResName),RT_BITMAP)<>0 then
  begin
    Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TWebPicture.LoadFromResourceID(Instance: THandle; ResID: Integer);
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


procedure TWebPicture.SetHeight(Value: integer);
begin

end;

procedure TWebPicture.SetWidth(Value: integer);
begin

end;

procedure TWebPicture.WriteData(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.Savetostream(Stream);
  end;
end;

procedure TWebPicture.LoadFromURL(url: string);
var
  uurl: string;
begin
  FFrame := 0;
  uurl := Uppercase(url);
  if Pos('RES://',uurl) = 1 then
  begin
   ID := url;
   Delete(url,1,6);
   if URL <> '' then
     LoadFromResourceName(hinstance,url);
   Exit;
  end;

  if Pos('FILE://',uurl) = 1 then
  begin
    ID := url;
    Delete(url,1,7);
    if url <> '' then
      LoadFromFile(url);
    Exit;
  end;

  if FAsynch then
  begin
    if FThreadBusy then
      Exit;
    FURL := url;
    FThreadBusy := True;
    TDownLoadThread.Create(self);
  end
  else
  begin
    FURL := url;
    ID := url;
    DownLoad;
    Update;
  end;
end;

procedure TWebPicture.DownLoad;
var
  RBSIZE:dword;
  httpstatus,httpsize,err:integer;
  dwIdx:dword;
  dwBufSize:dword;
  len:dword;
  cbuf:array[0..255] of char;
  rb:array[0..4095] of byte;
  fISession:hinternet;
  fIHttp:hinternet;
  cancel:boolean;

begin
  FISession := InternetOpen('WebImage',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if (FISession = nil) then
  begin
    DownLoadError('Cannot open internet session');
    FThreadBusy := False;
    Exit;
  end;

  FIHttp := InternetOpenURL(fISession,pchar(furl),nil,0,
    INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD,0);

  if (FIHttp = nil) then
  begin
    InternetCloseHandle(FISession);
    DownLoadError('Cannot open http connection');
    FThreadBusy := False;
    Exit;
  end;

  dwBufSize := SizeOf(cbuf);
  dwidx:=0;
  HttpQueryInfo(fIHttp,HTTP_QUERY_STATUS_CODE,@cbuf,dwBufSize,dwIdx);

  val(cbuf,httpstatus,err);
  if (httpstatus <> 200) or (err <> 0) then
  begin
    InternetCloseHandle(fISession);
    InternetCloseHandle(fIHttp);
    DownLoadError('Cannot open URL '+furl);
    FThreadBusy := False;
    Exit;
  end;

  dwBufSize:=sizeof(cbuf);
  dwidx:=0;
  HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_TYPE,@cbuf,dwBufSize,dwIdx);

  if (pos('IMAGE',uppercase(strpas(cbuf)))=0) then
  begin
    InternetCloseHandle(fISession);
    InternetCloseHandle(fIHttp);
    DownLoadError('Resource is not of image type : '+furl);
    FThreadBusy:=false;
    Exit;
  end;

  dwBufSize := SizeOf(cbuf);
  dwidx := 0;
  HttpQueryInfo(fIHttp,HTTP_QUERY_CONTENT_LENGTH,@cbuf,dwBufSize,dwIdx);

  Val(cbuf,httpsize,err);

  if ((httpsize = 0) or (err <> 0)) and CheckContentLength then
  begin
    InternetCloseHandle(fISession);
    InternetCloseHandle(fIHttp);
    DownLoadError('Image size is 0');
    FThreadBusy := False;
    Exit;
  end;

  DownLoadProgress(0,httpsize);

  len := 4096;
  RBSIZE := 4096;

  if Assigned(FMemStr) then
    FMemStr.Free;

  FMemStr := TMemoryStream.Create;

  Cancel := False;

  while (len > 0) and not Cancel do
  begin
    InternetReadFile(fIHttp,@rb,RBSIZE,len);
    if len > 0 then
      FMemStr.WriteBuffer(rb,len);
    DownLoadProgress(FMemStr.Size,httpsize);
    DownLoadCancel(cancel);
  end;

  if cancel then
    FMemStr.Clear;

  {
  if not cancel then
  begin
    ms.Position := 0;
    ms.LoadFromStream(ms);
    Synchronize();
  end;
  FMemStr.Free;
  }

  InternetCloseHandle(fIHttp);
  InternetCloseHandle(fISession);
  FThreadBusy := false;

  DownloadComplete;
end;

procedure TWebPicture.Update;
begin
  if Assigned(FMemStr) and (FMemStr.Size > 0) then
  begin
    FMemStr.Position := 0;
    LoadFromStream(FMemStr);

    FMemStr.Free;
    FMemStr := nil;
  end;
end;

procedure TWebPicture.DownLoadCancel(var cancel: boolean);
begin
  if Assigned(FOnDownLoadCancel) then FOnDownLoadCancel(Self,cancel);
end;

procedure TWebPicture.DownLoadComplete;
begin
  if Assigned(FOnDownLoadComplete) then FOnDownLoadComplete(Self);
end;

procedure TWebPicture.DownLoadError(err: string);
begin
  if Assigned(FOnDownloadError) then FOnDownLoadError(Self,err);
end;

procedure TWebPicture.DownLoadProgress(dwSize, dwTotSize: Cardinal);
begin
  if Assigned(FOnDownLoadProgress) then FOnDownLoadProgress(self,dwSize,dwTotSize);
end;

procedure TWebPicture.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
end;

procedure TWebPicture.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin

end;

function TWebPicture.GetFrameCount: Integer;
var
  i: Integer;
  b,c,d,e: Byte;
  Res: Integer;
begin
  Result := -1;

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
    Result := Res;
    FDataStream.Position := 0;
  end;
end;

function TWebPicture.IsGIFFile: Boolean;
var
  buf: array[0..4] of char;
begin
  Result := False;
  if FDataStream.Size>4 then
  begin
    FDataStream.Position := 0;
    FDataStream.Read(buf,4);
    buf[4] := #0;
    Result := Strpas(buf) = 'GIF8';
    FDataStream.Position := 0;
  end;
end;

function TWebPicture.GetFrameTime(i: Integer): Integer;
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
            Result := ft;
            FFrameTransp := b and $01=$01;
            FFrameDisposal := (b shr 3) and $7;
          end;
        end;
      end;
    end;
  end;
  FDataStream.Position := 0;
end;

function TWebPicture.GetMaxHeight: Integer;
var
  hmHeight: Integer;
begin
  if gpPicture=nil then Result:=0 else
  begin
    if FAnimMaxY>0 then Result:=FAnimMaxY
    else
    begin
      gpPicture.get_Height(hmHeight);
      Result := MulDiv(hmHeight, fLogPixY, HIMETRIC_INCH);
    end;
  end;

end;

function TWebPicture.GetMaxWidth: Integer;
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


procedure TWebPicture.SetFrame(const Value: Integer);
begin
  FFrame := Value;

  if (FDataStream.Size > 0) then
  begin
    LoadPicture;
    if Assigned(OnFrameChange) then
      OnFrameChange(self);
  end;
end;

{ TWebImage }

constructor TWebImage.Create(aOwner: TComponent);
begin
  inherited;
  FWebPicture := TWebPicture.Create;
  FWebPicture.OnChange := PictureChanged;
  Width := 100;
  Height := 100;
  FWebPicture.OnFrameChange := FrameChanged;  
  FWebPicture.OnDownLoadError := DownLoadError;
  FWebPicture.OnDownLoadCancel := DownLoadCancel;
  FWebPicture.OnDownLoadProgress := DownLoadProgress;
  FWebPicture.OnDownLoadComplete := DownLoadComplete;
  FAnimatedGIF := False;
end;

destructor TWebImage.Destroy;
begin
  FWebPicture.Free;
  inherited;
end;

procedure TWebImage.Loaded;
begin
  inherited;
  FWebPicture.FLogPixX := GetDeviceCaps(canvas.handle,LOGPIXELSX);
  FWebPicture.FLogPixY := GetDeviceCaps(canvas.handle,LOGPIXELSY);
  if not FWebPicture.Empty then
  begin
    FAnimatedGIF := FWebPicture.IsGIF and (FWebPicture.FrameCount > 1);
  end;

end;

procedure TWebImage.Paint;
var
  xo,yo:integer;
  rx,ry: Double;

  function Max(a,b:Integer): Integer;
  begin
    if (a > b) then Result := a else Result := b;
  end;

begin
  inherited;
  if Assigned(FWebPicture) then
  begin
    if not FWebPicture.Empty then
    case FPicturePosition of
    bpTopLeft:Canvas.Draw(0,0,FWebPicture);
    bpTopRight:Canvas.Draw(Max(0,Width - FWebPicture.Width),0,FWebPicture);
    bpBottomLeft:Canvas.Draw(0,Max(0,Height - FWebPicture.Height),FWebPicture);
    bpBottomRight:Canvas.Draw(Max(0,Width - FWebPicture.Width),Max(0,Height - FWebPicture.Height),FWebPicture);
    bpCenter:Canvas.Draw(Max(0,Width - FWebPicture.Width) shr 1,Max(0,Height - FWebPicture.Height) shr 1,FWebPicture);
    bpTiled:
      begin
        yo := 0;
        while (yo < Height) do
        begin
          xo := 0;
          while (xo < Width) do
          begin
            Canvas.Draw(xo,yo,FWebPicture);
            xo := xo + FWebPicture.Width;
          end;
          yo := yo + FWebPicture.Height - 1;
        end;
      end;
    bpStretched:Canvas.StretchDraw(Rect(0,0,width,height),FWebPicture);
    bpStretchedWithAspectRatio:
      begin
        rx := FWebPicture.Width/Width;
        ry := FWebPicture.Height/Height;
        if rx > ry then
          Canvas.StretchDraw(Rect(0,0,Width,Trunc(FWebPicture.Height/rx)),FWebPicture)
        else
         Canvas.StretchDraw(Rect(0,0,Trunc(FWebPicture.Width/ry),Height),FWebPicture);
      end;
    end;
  end;
end;

procedure TWebImage.PictureChanged(sender: TObject);
begin
  Invalidate;
  if FAutoSize and not FWebPicture.Empty then
  begin
    self.Width := FWebPicture.Width;
    self.Height := FWebPicture.Height;
  end;

  if not FWebPicture.Empty then
  begin
    FAnimatedGIF := FWebPicture.IsGIF and (FWebPicture.FrameCount > 1);
    FWebPicture.Frame:=0;
  end;
end;

procedure TWebImage.SetWebPicture(const Value: TWebPicture);
begin
  FWebPicture.Assign(Value);
  Invalidate;
end;

procedure TWebImage.SetPicturePosition(const Value: TPicturePosition);
begin
  if FPicturePosition <> Value then
  begin
    FPicturePosition := Value;
    Invalidate;
  end;
end;

procedure TWebImage.DownLoadCancel(Sender: TObject; var cancel: boolean);
begin
  if Assigned(FOnDownLoadCancel) then
    FOnDownLoadCancel(self,cancel);
end;

procedure TWebImage.DownLoadComplete(Sender: TObject);
begin
  if Assigned(FOnDownLoadComplete) then
    FOnDownLoadComplete(self);
end;

procedure TWebImage.DownLoadError(Sender: TObject; err: string);
begin
  if Assigned(FOnDownloadError) then
    FOnDownLoadError(self,err);
end;

procedure TWebImage.DownLoadProgress(Sender: TObject; dwSize,
  dwTotSize: Cardinal);
begin
  if Assigned(FOnDownLoadProgress) then
    FOnDownLoadProgress(self,dwSize,dwTotSize);
end;

procedure TWebImage.SetAnimate(const Value: Boolean);
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

procedure TWebImage.Timer(Sender: TObject);
begin
 if not Assigned(FWebPicture) then
   Exit;

  if FAnimatedGIF and not FWebPicture.Empty then
  begin
    if FTimerCount = FNextCount then
    begin
      if FWebPicture.Frame < FWebPicture.FrameCount then
        FWebPicture.Frame := FWebPicture.Frame + 1
      else
        FWebPicture.Frame := 1;

      FNextCount := FNextCount + FWebPicture.FrameTime[FWebPicture.Frame];
    end;
    Inc(FTimerCount);
  end;
end;

procedure TWebImage.FrameChanged(Sender: TObject);
var
  R: TRect;
begin
  case FWebPicture.FFrameDisposal of
  0:if Visible then Paint;
  1:Invalidate;
  2:begin
      with FWebPicture do
      begin
        R := Rect(FFrameXPos,FFrameXPos,FFrameXPos+FFrameXSize,FFrameYPos+FFrameYSize);
        if Parent.HandleAllocated then
          InvalidateRect(Parent.Handle,@R,true);
      end;
    end;
  3:if Visible then Paint;
  end;
  if Assigned(FOnFrameChange) then
    FOnFrameChange(Self);
end;

procedure TWebImage.SetURL(const Value: string);
begin
  FWebPicture.Asynch := FAsynch;
  FWebPicture.CheckContentLength := CheckContentLength;
  FURL := Value;
  if FURL <> '' then
    FWebPicture.LoadFromURL(FURL);
end;

procedure TWebImage.SaveToFile(Filename: string);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  FWebPicture.SaveToStream(st);
  st.SaveToFile(Filename);
  st.Free;
end;

function TWebImage.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TWebImage.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TWebImage.SetVersion(const Value: string);
begin

end;

{ TDownLoadThread }

constructor TDownLoadThread.Create(awebpicture: TWebPicture);
begin
  inherited Create(False);
  WebPicture := AWebPicture;
  FreeOnTerminate := True;
end;

procedure TDownLoadThread.Execute;
begin
  WebPicture.DownLoad;
  Synchronize(WebPicture.Update);
end;

procedure TDownLoadThread.UpdatePicture;
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
