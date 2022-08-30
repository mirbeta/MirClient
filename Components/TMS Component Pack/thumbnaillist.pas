{***************************************************************************}
{ TThumbnailList component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2011                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit ThumbnailList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls, ComObj, ActiveX;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // 1.1.0.2 : Fixed : Issue with LoadFromResourceName
  // 1.1.0.3 : Fixed : Issue with scrolling in vertical organised list
  

type
  TScrollStyle = (ssNormal,ssFlat,ssEncarta);
  TOnGetImageStreamEvent = procedure(Sender: Tobject; var Idx: integer; var S: TMemoryStream) of object;  

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
    FFrame:Integer;
    FOnFrameChange: TNotifyEvent;
    FFrameXPos: Word;
    FFrameYPos: Word;
    FFrameXSize: Word;
    FFrameYSize: Word;
    FFrameTransp: Boolean;
    FFrameDisposal: Word;
    FAnimMaxX,FAnimMaxY: Word;
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
  published
    { Published declarations }
    property Stretch:Boolean read FStretched write FStretched;
    property Frame:Integer read FFrame write SetFrame;
    property OnFrameChange: TNotifyEvent read FOnFrameChange write FOnFrameChange;
  end;

  TThumbnails = class;

  TThumbnailSource = (tsPicture,tsFile);

  TThumbnail = class(TCollectionItem)
  private
    FPicture: TIPicture;
    FFilename: string;
    FSource: TThumbnailSource;
    FCaption: string;
    FTag: Integer;
    procedure SetPicture(const Value: TIPicture);
    procedure SetFileName(const Value: string);
    procedure SetCaption(const Value: string);
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
  protected
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Picture: TIPicture read FPicture write SetPicture;
    property Source: TThumbnailSource read FSource write FSource;
    property Filename: string read FFilename write SetFileName;
    property Caption: string read FCaption write SetCaption;
    property Tag: Integer read FTag write FTag;
  end;

  TThumbnailList = class;

  TThumbnails = class(TCollection)
  private
    FOwner: TThumbnailList;
    FThumbnailCount: Integer;
    function GetItem(Index: Integer): TThumbnail;
    procedure SetItem(Index: Integer; const Value: TThumbnail);
    procedure Changed(Sender:TObject);
  protected
    function GetOwner: TPersistent; override;
    procedure AddThumb;
    procedure RemoveThumb;
  public
    constructor Create(AOwner:TThumbnailList);
    function Add: TThumbnail;
    function Insert(index:integer): TThumbnail;
    property Items[Index: Integer]: TThumbnail read GetItem write SetItem; default;
  end;

  TThumbnailOrientation = (toVertical, toHorizontal);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TThumbnailList = class(TCustomListBox)
  private
    FThumbnails: TThumbnails;
    FUpdateCount: Integer;
    FShowCaption: Boolean;
    FOrientation: TThumbnailOrientation;
    FThumbnailSize: Integer;
    FBuffered: Boolean;
    FShowSelection: Boolean;
    FInSizing: Boolean;
    FScrollStyle: TScrollStyle;
    FScrollColor: TColor;
    FScrollWidth: Integer;
    FOnGetImage: TOnGetImageStreamEvent;
    { Private declarations }
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetThumbnails(const Value: TThumbnails);
    procedure BuildItems;
    procedure SetShowCaption(const Value: Boolean);
    procedure SetOrientation(const Value: TThumbnailOrientation);
    procedure SetThumbnailSize(const Value: Integer);
    procedure SetShowSelection(const Value: Boolean);
    procedure WMVScroll(var WMScroll:TWMScroll ); message WM_VSCROLL;
    procedure WMHScroll(var WMScroll:TWMScroll ); message WM_HSCROLL;
    procedure FlatSetScrollPos(code,pos:integer;fRedraw:bool);
    procedure FlatSetScrollProp(index,newValue:integer;fRedraw:bool);
    procedure FlatSetScrollInfo(code:integer;var scrollinfo:TScrollInfo;fRedraw:bool);
    procedure FlatShowScrollBar(code:integer;show:bool);
    procedure SetScrollStyle(const Value: TScrollStyle);
    procedure SetScrollColor(const Value: TColor);
    procedure SetScrollWidth(const Value: integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
     function GetVersionNr: Integer; virtual;
    { Protected declarations }
    procedure WndProc(var Message: TMessage); override;
    procedure DrawItem(Index: Integer; ARect: TRect;AState: TOwnerDrawState); override;
    procedure CreateParams(var Params:TCreateParams); override;
    procedure UpdateHorzScroll;
    procedure UpdateVScrollBar;
    procedure UpdateHScrollBar;
    procedure UpdateStyle;
    procedure UpdateColor;
    procedure UpdateWidth;
    property Items;
    function VisibleItems: Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;    
    procedure ShowFolder(FolderName: string);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property BorderStyle;
    property Buffered: Boolean read FBuffered write FBuffered default True;
    property Color;
    property Columns;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MultiSelect;
    property Orientation: TThumbnailOrientation read FOrientation
      write SetOrientation default toVertical;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollStyle: TScrollStyle read FScrollStyle write SetScrollStyle;
    property ScrollColor: TColor read FScrollColor write SetScrollColor;
    property ScrollWidth: Integer read FScrollWidth write SetScrollWidth;
    property ShowCaption: Boolean read FShowCaption
      write SetShowCaption default True;
    property ShowHint;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection;

    property TabOrder;
    property TabStop;
    property Thumbnails: TThumbnails read FThumbnails write SetThumbnails;
    property ThumbnailSize: Integer read FThumbnailSize write SetThumbnailSize;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnGetImage: TOnGetImageStreamEvent read FOnGetImage write FOnGetImage;     
    property Version: string read GetVersion write SetVersion;
  end;



implementation

const
  HIMETRIC_INCH = 2540;
  TIMER_ID = 500;

  commctrl = 'comctl32.dll';
  MAX_TABS=20;

  WSB_PROP_CYVSCROLL  = $0000001;
  WSB_PROP_CXHSCROLL  = $0000002;
  WSB_PROP_CYHSCROLL  = $0000004;
  WSB_PROP_CXVSCROLL  = $0000008;
  WSB_PROP_CXHTHUMB   = $0000010;
  WSB_PROP_CYVTHUMB   = $0000020;
  WSB_PROP_VBKGCOLOR  = $0000040;
  WSB_PROP_HBKGCOLOR  = $0000080;
  WSB_PROP_VSTYLE     = $0000100;
  WSB_PROP_HSTYLE     = $0000200;
  WSB_PROP_WINSTYLE   = $0000400;
  WSB_PROP_PALETTE    = $0000800;
  WSB_PROP_MASK       = $0000FFF;

  FSB_FLAT_MODE       =    2;
  FSB_ENCARTA_MODE    =    1;
  FSB_REGULAR_MODE    =    0;


procedure TThumbnailList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
  end;
end;


constructor TThumbnailList.Create(AOwner: TComponent);
begin
  inherited;
  FThumbnails := TThumbnails.Create(Self);
  Style := lbOwnerDrawFixed;
  FThumbnailSize := 64;
  FBuffered := True;
  FShowCaption := True;
  FScrollWidth := 16;
  FScrollStyle := ssNormal;
end;

destructor TThumbnailList.Destroy;
begin
  FThumbnails.Free;
  inherited;
end;

procedure TThumbnailList.DrawItem(Index: Integer; ARect: TRect;
  AState: TOwnerDrawState);
var
  Rx, Ry: Double;
  Width,Height, TH: Integer;
  DrawRect: TRect;
  TempPic, Pic: TIPicture;
  ImageStream: TMemoryStream;
  I: integer;
begin

  if (odSelected in AState) and FShowSelection then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Pen.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
    Canvas.Pen.Width := 2;
    Canvas.Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Font.Color := Font.Color;
    Canvas.Pen.Width := 2;
    Canvas.Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
  end;

  //Make sure the focus rect. is redrawn
  Canvas.MoveTo(ARect.Left,ARect.Top);
  Canvas.LineTo(ARect.Right,ARect.Top);
  Canvas.LineTo(ARect.Right,ARect.Bottom);
  Canvas.LineTo(ARect.Left,ARect.Bottom);
  Canvas.LineTo(ARect.Left,ARect.Top);

  TH := 0;

  InflateRect(ARect,-2,-2);

  if (Index >= 0) and (Index < FThumbnails.Count) then
  begin
    with FThumbnails.Items[Index] do
    begin
      TempPic := nil;
      Pic := Picture;

	  if Assigned(FOnGetImage) and Picture.Empty then      
      begin
        ImageStream := TMemoryStream.create;
        I := Index;
        try
          FOnGetImage(Self, I, ImageStream);
          ImageStream.Position := 0;
          Picture.LoadFromStream(ImageStream);
        finally  
          ImageStream.Free;
        end;
      end; 

      if FBuffered then
      begin
        if Picture.Empty and (Filename <> '') and (Source = tsFile) then
        begin
          TempPic := TIPicture.Create;
          TempPic.LoadFromFile(Filename);
          Pic := TempPic;
          Picture.LoadFromFile(Filename);
        end;
      end;

      if FShowCaption then
      begin
        DrawTextEx(Canvas.Handle,pchar(Caption),Length(Caption),ARect,DT_CENTER or DT_BOTTOM or DT_SINGLELINE or DT_END_ELLIPSIS,nil);
        TH := Canvas.TextHeight('gh');
        ARect.Bottom := ARect.Bottom - TH;
      end;

      Width := ARect.Right - ARect.Left;
      Height := ARect.Bottom - ARect.Top;

      if (Width > 0) and (Height > 0) then
      begin
        rx := Pic.GetMaxWidth/Width;
        ry := Pic.GetMaxHeight/Height;

        if (rx > 1) or (ry > 1) then
        begin
          if rx > ry then
            DrawRect := Rect(0,0,Width,Trunc(Pic.GetMaxHeight/rx))
          else
            DrawRect := Rect(0,0,Trunc(Pic.GetMaxWidth/ry),Height);
        end
        else
          DrawRect := Rect(0,0,Pic.GetMaxWidth,Pic.GetMaxHeight);

        {Center the drawing rectangle}
        OffsetRect(DrawRect,ARect.Left + (Width - DrawRect.Right + DrawRect.Left) shr 1,
                            ARect.Top + (Height - DrawRect.Bottom + DrawRect.Top) shr 1);

        Canvas.StretchDraw(DrawRect,Pic);
      end;

      if Assigned(TempPic) then
        TempPic.Free;

    end;
  end;

  InflateRect(ARect,2,2);
  ARect.Bottom := ARect.Bottom + TH;
  
  if odFocused in AState then
    DrawFocusRect(Canvas.Handle,ARect);
end;

procedure TThumbnailList.SetOrientation(const Value: TThumbnailOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    SetThumbnailSize(FThumbnailSize);
    Invalidate;
  end;
end;


procedure TThumbnailList.BuildItems;
begin
  if csLoading in ComponentState then
    Exit;

  if csDestroying in ComponentState then
    Exit;

  if FUpdateCount > 0 then
    Exit;

  FUpdateCount := FUpdateCount + 1;

  while Items.Count > FThumbnails.FThumbnailCount do
    Items.Delete(Items.Count - 1);

  while Items.Count < FThumbnails.FThumbnailCount do
    Items.Add('_');

  UpdateHorzScroll;

  SetThumbnailSize(FThumbnailSize);

  UpdateStyle;
  UpdateColor;
  UpdateWidth;
  UpdateHScrollBar;
  UpdateVScrollBar;


  FUpdateCount := FUpdateCount - 1;
  Invalidate;
end;

procedure TThumbnailList.SetThumbnails(const Value: TThumbnails);
begin
  FThumbnails.Assign(Value);
end;


{ TIPicture }

procedure TIPicture.Assign(Source: TPersistent);
begin
  FIsEmpty := True;
  gpPicture := nil;
  if Source = nil then
  begin
    FDataStream.Clear;
    if Assigned(OnChange) then
      OnChange(Self);
  end
  else
  begin
    if (Source is TIPicture) then
    begin
      FDataStream.LoadFromStream(TIPicture(Source).FDataStream);
      FIsEmpty := False;
      LoadPicture;
      if assigned(OnChange) then OnChange(self);
    end;
  end;
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
end;

destructor TIPicture.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure TIPicture.SetFrame(const Value:Integer);
begin
 fFrame:=Value;
 if (fDataStream.Size>0) then
   begin
    LoadPicture;
    if assigned(OnFrameChange) then OnFrameChange(self);
   end;
end;

procedure TIPicture.LoadPicture;
const
  IID_IPicture: TGUID = (D1:$7BF80980;D2:$BF32;D3:$101A;D4:($8B,$BB,$00,$AA,$00,$30,$0C,$AB));

var
  hGlobal:thandle;
  pvData:pointer;
  pstm:IStream;
  hr:hResult;
  gifstream:tmemorystream;
  i:Integer;
  b,c,d,e:byte;
  skipimg:Boolean;
  imgidx:Integer;
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

  {skip first image ctrl}
  if IsGIF and (FrameCount > 0) then
   begin
    //manipulate the stream here for animated GIF ?
    Gifstream := TMemoryStream.Create;
    imgidx := 1;
    skipimg := False;

    fDataStream.Position := 6;
    fDataStream.Read(FAnimMaxX,2);
    fDataStream.Read(FAnimMaxY,2);

    for i := 1 to fDataStream.Size do
     begin
       fDataStream.Position := i - 1;
       fDataStream.Read(b,1);

       if (b=$21) and (i+8 < FDataStream.Size) then
        begin
         FDataStream.Read(c,1);
         FDataStream.Read(d,1);
         FDataStream.Position := FDataStream.Position+5;

         FDataStream.Read(e,1);
         if (c=$F9) and (d=$4) and (e=$2C) then
           begin
             if imgidx=fFrame then
              begin
               fDataStream.Read(FFrameXPos,2);
               fDataStream.Read(FFrameYPos,2);
               fDataStream.Read(FFrameXSize,2);
               fDataStream.Read(FFrameYSize,2);
              end;

             inc(imgidx);

             if imgidx <= fFrame then
               skipimg := true
             else
               skipimg := false;
           end;
        end;
       if not skipimg then
        gifstream.write(b,1);
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

begin
  if Empty then
    Exit;
  if gpPicture=nil then
    Exit;

  hmWidth  := 0;
  hmHeight := 0;
  gpPicture.get_Width(hmWidth);
  gpPicture.get_Height(hmHeight);

  DrwRect := Rect;

  OffsetRect(DrwRect,FFrameXPos,FFrameYPos);

  gpPicture.Render(ACanvas.Handle,DrwRect.Left,DrwRect.Bottom,DrwRect.Right-DrwRect.Left,
                   -(DrwRect.Bottom-DrwRect.Top),0,0, hmWidth,hmHeight, DrwRect);
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
  try
    FDataStream.LoadFromFile(Filename);
    FIsEmpty := False;
    FFrame := 1;
    FAnimMaxX := 0;
    FAnimMaxY := 0;

    LoadPicture;
    
    if Assigned(OnChange) then
      OnChange(self);

  except
    FIsEmpty := true;
  end;
end;

procedure TIPicture.LoadFromStream(Stream: TStream);
begin
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
  if FindResource(Instance,PChar(ResName),RT_RCDATA)<>0 then
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

function TIPicture.IsGIFFile: Boolean;
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
  if gpPicture = nil then
    Result := 0
  else
  begin
    if FAnimMaxY > 0 then
      Result := FAnimMaxY
    else
    begin
      gpPicture.get_Height(hmHeight);
      Result := MulDiv(hmHeight, FLogPixY, HIMETRIC_INCH);
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



{ TThumbnail }

procedure TThumbnail.Assign(Source: TPersistent);
begin
  if (Source is TThumbNail) then
  begin
    FPicture.Assign((Source as TThumbnail).Picture);
    FSource := (Source as TThumbnail).Source;
    FFileName := (Source as TThumbnail).FileName;
    FCaption := (Source as TThumbnail).Caption;
    FTag := (Source as TThumbnail).Tag;
  end;
end;

procedure TThumbnail.Changed;
begin
  (Collection as TThumbnails).Changed(Self);
end;

constructor TThumbnail.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TIPicture.Create;
  FPicture.OnChange := PictureChanged;
  (Collection as TThumbnails).AddThumb;
end;

destructor TThumbnail.Destroy;
begin
  (Collection as TThumbnails).RemoveThumb;
  FPicture.Free;
  inherited;
end;

procedure TThumbnail.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TThumbnail.SetCaption(const Value: string);
begin
  FCaption := Value;
  Changed;
end;

procedure TThumbnail.SetFileName(const Value: string);
begin
  FFilename := Value;
  Changed;
end;

procedure TThumbnail.SetPicture(const Value: TIPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

{ TThumbnails }

function TThumbnails.Add: TThumbnail;
begin
  Result := TThumbnail(inherited Add);
end;

procedure TThumbnails.AddThumb;
begin
  FThumbnailCount :=  FThumbnailCount + 1;
  FOwner.BuildItems;
end;

procedure TThumbnails.Changed(Sender: TObject);
begin
  FOwner.BuildItems;
end;

constructor TThumbnails.Create(AOwner: TThumbnailList);
begin
  inherited Create(TThumbnail);
  FOwner := AOwner;
  FThumbnailCount := 0;
end;

function TThumbnails.GetItem(Index: Integer): TThumbnail;
begin
  Result := TThumbnail(inherited Items[Index]);
end;

function TThumbnails.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TThumbnails.Insert(Index: Integer): TThumbnail;
begin
  Result := TThumbnail(inherited Insert(Index));
end;

procedure TThumbnails.RemoveThumb;
begin
  if FThumbnailCount >0 then
    FThumbnailCount :=  FThumbnailCount - 1;
  FOwner.BuildItems;
end;

procedure TThumbnails.SetItem(Index: Integer; const Value: TThumbnail);
begin
  inherited SetItem(Index, Value);
end;

procedure TThumbnailList.Loaded;
var
  NewStyle: TScrollStyle;
begin
  inherited;
  BuildItems;

  NewStyle := ScrollStyle;

  if NewStyle <> ssNormal then
  begin
    ScrollStyle := ssNormal;
    ScrollStyle := NewStyle;
  end;

  {seems to be required on older COMCTRL versions to force
   scroll height calculation}
  self.Height := self.Height+1;
  self.Height := self.Height-1;
end;

procedure TThumbnailList.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TThumbnailList.UpdateHorzScroll;
begin
  if Orientation <> toVertical then
    Columns := FThumbnails.Count;
end;

procedure TThumbnailList.CreateParams(var Params:TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or WS_HSCROLL;

  if Orientation <> toVertical then
    Params.Style := Params.Style or LBS_MULTICOLUMN
end;


procedure TThumbnailList.SetThumbnailSize(const Value: Integer);
begin
  FThumbnailSize := Value;

  if Orientation = toVertical then
  begin
    ItemHeight := Value;
    if Columns > 0 then
      SendMessage(Handle,LB_SETCOLUMNWIDTH,Value,0);
  end
  else
  begin
    ItemHeight := Height - GetSystemMetrics(SM_CYHSCROLL) - 4 * GetSystemMetrics(SM_CYBORDER);
    Columns := FThumbnails.Count;
    SendMessage(Handle,LB_SETCOLUMNWIDTH,Value,0);
  end;
end;

procedure TThumbnailList.WMSize(var Message: TWMSize);
var
  NewItemHeight: Integer;
begin
  inherited;

  if Orientation = toHorizontal then
  begin
    if (FThumbnailSize > 0) then
    begin
      FInSizing := True;
      SendMessage(Handle,LB_SETCOLUMNWIDTH,FThumbnailSize,0);
      FInSizing := False;
    end;

    if not (csLoading in ComponentState) then
    begin
      NewItemHeight := Height - GetSystemMetrics(SM_CYHSCROLL) - 4 * GetSystemMetrics(SM_CYBORDER);
      if NewItemHeight > 0 then
        ItemHeight := NewItemHeight;
    end;
  end
  else
  begin
    Invalidate;
  end;
end;

procedure TThumbnailList.ShowFolder(FolderName: string);
var
  SR: TSearchRec;

  procedure AddToList(Name:string);
  var
    Ext: string;
  begin
    Ext := Uppercase(ExtractFileExt(Name));
    if (Ext = '.JPG') or (Ext = '.JPEG') or (Ext = '.GIF') or
       (Ext = '.BMP') or (Ext = '.ICO') or (Ext = '.WMF') or (Ext = '.EMF') then
      with FThumbnails.Add do
      begin
        FileName := ExtractFilePath(FolderName) + Name;
        Source := tsFile;
        Caption := Name;
        if not FBuffered then
        begin
          Picture.LoadFromFile(Filename);
        end;
      end;
  end;

begin
  {$IFDEF DELPHI6_LVL}
  if not DirectoryExists(ExtractFilePath(foldername)) then
    Exit;
  {$ENDIF}  
  BeginUpdate;
  FThumbnails.Clear;

  if FindFirst(FolderName,faAnyFile,SR) = 0 then
  begin
    AddToList(SR.Name);
    while FindNext(SR) = 0 do
      AddToList(SR.Name);
  end;
  FindClose(SR);

  EndUpdate;
end;

procedure TThumbnailList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  NewC: Integer;
begin
  inherited;
  if (Columns > 0) and (Orientation = toVertical) then
  begin
    NewC := AWidth div FThumbnailSize;

    if (NewC > 0) and (NewC <> Columns) then
      Columns := NewC;
  end;
end;

procedure TThumbnailList.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TThumbnailList.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TThumbnailList.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
      BuildItems;
  end;
end;

procedure TThumbnailList.WMHScroll(var WMScroll: TWMScroll);
begin
  inherited;
  WMScroll.Pos := GetScrollPos(self.Handle,SB_HORZ);
  FlatSetScrollPos(SB_HORZ,wmScroll.Pos,True);
end;

procedure TThumbnailList.WMVScroll(var WMScroll: TWMScroll);
begin
  inherited;
  {
  SetScrollPos(Handle ,SB_VERT,wmScroll.Pos,False);
  SetScrollPos(wmScroll.ScrollBar ,SB_VERT,100,False);
  }
  WMScroll.Pos := GetScrollPos(self.Handle,SB_VERT);
  FlatSetScrollPos(SB_VERT,wmScroll.Pos,True);
  wmscroll.Result := 1;
end;

procedure TThumbnailList.UpdateStyle;
begin
  case FScrollStyle of
  ssNormal:FlatSetScrollProp(WSB_PROP_VSTYLE,FSB_REGULAR_MODE,true);
  ssFlat:FlatSetScrollProp(WSB_PROP_VSTYLE,FSB_FLAT_MODE,true);
  ssEncarta:FlatSetScrollProp(WSB_PROP_VSTYLE,FSB_ENCARTA_MODE,true);
  end;
  case FScrollStyle of
  ssNormal:FlatSetScrollProp(WSB_PROP_HSTYLE,FSB_REGULAR_MODE,true);
  ssFlat:FlatSetScrollProp(WSB_PROP_HSTYLE,FSB_FLAT_MODE,true);
  ssEncarta:FlatSetScrollProp(WSB_PROP_HSTYLE,FSB_ENCARTA_MODE,true);
  end;
end;

procedure TThumbnailList.UpdateColor;
begin
  FlatSetScrollPROP(WSB_PROP_VBKGCOLOR,longint(FScrollColor),True);
  FlatSetScrollPROP(WSB_PROP_HBKGCOLOR,longint(FScrollColor),True);
end;

procedure TThumbnailList.UpdateWidth;
begin
  FlatSetScrollPROP(WSB_PROP_CXVSCROLL,FScrollWidth,True);
  FlatSetScrollPROP(WSB_PROP_CYHSCROLL,FScrollWidth,True);
end;

procedure TThumbnailList.UpdateVScrollBar;
var
  scrollinfo: TScrollinfo;
begin
  scrollinfo.FMask := SIF_ALL;
  scrollinfo.cbSize := sizeof(scrollinfo);
  GetScrollInfo(self.Handle,SB_VERT,scrollinfo);
  scrollinfo.FMask := SIF_ALL;
  scrollinfo.cbSize:=sizeof(scrollinfo);
  FlatSetScrollInfo(SB_VERT,scrollinfo,true);
end;

procedure TThumbnailList.UpdateHScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  GetScrollInfo(Handle,SB_HORZ,ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  FlatSetScrollInfo(SB_HORZ,scrollinfo,true);
end;

procedure TThumbnailList.FlatSetScrollPos(code, pos: integer; fRedraw: bool);
var
 ComCtl32DLL: THandle;
 _FlatSB_SetScrollPos:function(wnd:hwnd;code,pos:integer;fRedraw:bool):integer; stdcall;

begin
 ComCtl32DLL:=GetModuleHandle(commctrl);
 if (ComCtl32DLL>0) then
  begin
   @_FlatSB_SetScrollPos:=GetProcAddress(ComCtl32DLL,'FlatSB_SetScrollPos');
   if assigned(_FlatSB_SetScrollPos) then
     _FlatSB_SetScrollPos(self.handle,code,pos,fRedraw);
  end;
end;

procedure TThumbnailList.FlatSetScrollInfo(code: integer;var scrollinfo:tscrollinfo;fRedraw: bool);
var
 ComCtl32DLL: THandle;
 _FlatSB_SetScrollInfo:function(wnd:hwnd;code:integer;var scrollinfo:tscrollinfo;fRedraw:bool):integer; stdcall;

begin
 ComCtl32DLL:=GetModuleHandle(commctrl);
 if (ComCtl32DLL>0) then
  begin
   @_FlatSB_SetScrollInfo:=GetProcAddress(ComCtl32DLL,'FlatSB_SetScrollInfo');
   if assigned(_FlatSB_SetScrollInfo) then
     begin
      _FlatSB_SetScrollInfo(self.handle,code,scrollinfo,fRedraw);
     end;
  end;
end;

procedure TThumbnailList.FlatSetScrollProp(index, newValue: integer; FRedraw: bool);
var
  ComCtl32DLL: THandle;
  _FlatSB_SetScrollProp:function(wnd:hwnd;Index,newValue:integer;fredraw:bool):bool stdcall;

begin
  ComCtl32DLL:=GetModuleHandle(commctrl);
  if (ComCtl32DLL>0) then
  begin
    @_FlatSB_SetScrollProp:=GetProcAddress(ComCtl32DLL,'FlatSB_SetScrollProp');
    if Assigned(_FlatSB_SetScrollProp) then
     _FlatSB_SetScrollProp(self.handle,index,newValue,fRedraw);
  end;
end;

procedure TThumbnailList.FlatShowScrollBar(code:integer;show:bool);
var
 ComCtl32DLL: THandle;
 _FlatSB_ShowScrollBar:function(wnd:hwnd;code:integer;show:bool):integer; stdcall;

begin
  ComCtl32DLL := GetModuleHandle(commctrl);
  if (ComCtl32DLL>0)  then
  begin
    @_FlatSB_ShowScrollBar:=GetProcAddress(ComCtl32DLL,'FlatSB_ShowScrollBar');
    if Assigned(_FlatSB_ShowScrollBar) then
      _FlatSB_ShowScrollBar(self.handle,code,show);
  end;
end;

procedure TThumbnailList.SetScrollStyle(const Value: TScrollStyle);
var
  ComCtl32DLL: THandle;
  _InitializeFlatSB: function(wnd:hwnd):Bool stdcall;
  _UnInitializeFlatSB: function(wnd:hwnd):Bool stdcall;

begin
  if (Value in [ssEncarta,ssFlat]) and
     (FScrollStyle = ssNormal) then
   begin
     ComCtl32DLL := GetModuleHandle(commctrl);
     if (ComCtl32DLL > 0) then
     begin
       @_InitializeFlatSB := GetProcAddress(ComCtl32DLL,'InitializeFlatSB');
       if Assigned(_InitializeFlatSB) then
       begin
        _InitializeFlatSB(self.Handle);
       end;
    end;
  end;

  if (Value = ssNormal) and
     (FScrollStyle in [ssEncarta,ssFlat]) then
  begin
    ComCtl32DLL := GetModuleHandle(commctrl);
    if (ComCtl32DLL>0) then
     begin
      @_UnInitializeFlatSB:=GetProcAddress(ComCtl32DLL,'UnInitializeFlatSB');
      if assigned(_UnInitializeFlatSB) then
       begin
        _UnInitializeFlatSB(self.handle);
       end;
    end;
   end;

  FScrollStyle := Value;
  UpdateStyle;
end;


procedure TThumbnailList.SetScrollColor(const Value: TColor);
begin
  FScrollColor := Value;
  UpdateColor;
end;

procedure TThumbnailList.SetScrollWidth(const Value: integer);
begin
  FScrollWidth := Value;
  UpdateWidth;
end;

procedure TThumbnailList.WndProc(var Message: TMessage);
begin
  inherited;

  if (message.msg=LB_ADDSTRING) or
     (message.msg=WM_SIZE) or
     (message.msg=LB_INSERTSTRING) or
     (message.msg=LB_DELETESTRING) or
     (message.msg=LB_RESETCONTENT) then
  begin
    {$IFDEF TMSDEBUG}
    case message.msg of
    LB_ADDSTRING:outputdebugstr('add '+inttostr(message.result));
    LB_INSERTSTRING:outputdebugstr('insert '+inttostr(message.wparam));
    LB_DELETESTRING:outputdebugstr('delete '+inttostr(message.wparam));
    LB_RESETCONTENT:outputdebugstr('reset content');
    end;
    {$ENDIF}

    if (FUpdateCount = 0) then
    begin
      if (VisibleItems < self.Items.Count) then
        FlatShowScrollbar(SB_VERT,True);

      UpdateHScrollBar;
      UpdateVScrollBar;
      UpdateWidth;
      UpdateStyle;
      UpdateColor;
    end;

    if (message.msg = LB_RESETCONTENT) then
      FlatShowScrollbar(SB_VERT,False);
  end;
end;

function TThumbnailList.VisibleItems: integer;
var
  hght: Integer;
begin
  hght := SendMessage(self.Handle,lb_getitemheight,0,0);
  if hght <> 0 then
    VisibleItems := self.Height div hght
  else
    VisibleItems := 0;
end;

procedure TThumbnailList.CreateWnd;
begin
  inherited;
end;

function TThumbnailList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TThumbnailList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TThumbnailList.SetVersion(const Value: string);
begin

end;

end.
