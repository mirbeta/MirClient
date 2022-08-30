{***************************************************************************}
{ TGDIPPicture class                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2015                                        }
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

unit GDIPicture;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Graphics, Controls , SysUtils, AdvGDIP, ComObj, ActiveX
  {$IFDEF DELPHIXE_LVL}
  , PngImage
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
  {$ENDIF}
  ;

type
  TPictureFormat = (pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone);

  TGDIPPicture = class(TGraphic)
  private
    FFileName: String;
    { Private declarations }
    FDatastream: TMemoryStream;
    FIsEmpty: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FDoubleBuffered: Boolean;
    FBackgroundColor: TColor;
    FTransparentBitmap: Boolean;
    FOnClear: TNotifyEvent;
    FAngle: integer;
  protected
    { Protected declarations }
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    function GetPictureFormat: TPictureFormat;
    procedure DoChange;
  public
    { Public declarations }
    function GetFileName: String;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawImage(Graphics: TGPGraphics; X, Y: Integer);
    procedure DrawImageRect(Graphics: TGPGraphics; X, Y, W, H: Integer);
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure LoadFromURL(URL: string);
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Angle: integer read FAngle write FAngle;
    function GetImageSizes: Boolean;
    property TransparentBitmap: boolean read FTransparentBitmap write FTransparentBitmap;
    property PictureFormat: TPictureFormat read GetPictureFormat;
  published
    { Published declarations }
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

resourcestring
  GDIPERRMSG = 'Could not allocate memory for image';

implementation

uses
  Math;

const
  MinGraphicSize = 44;

function FindPictureFormat(const Buffer; const BufferSize: Int64): TPictureFormat;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  Result := pfNone;
  if BufferSize < MinGraphicSize then
    Exit;
  case Words[0] of
    $4D42: Result := pfBMP;
    $D8FF: Result := pfJPG;
    $4949: if Words[1] = $002A then Result := pfTiff; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then Result := pfTiff; //i.e., TIFF
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      Result := pfPNG
    else if LongWords[0] = $9AC6CDD7 then
      Result := pfMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      Result := pfMetafile
    {$IFDEF DELPHIXE4_LVL}
    else if AnsiStrings.StrLComp(PAnsiChar(@Buffer), AnsiString('GIF'), 3) = 0 then
    {$ENDIF}
    {$IFNDEF DELPHIXE4_LVL}
    else if StrLComp(PAnsiChar(@Buffer), AnsiString('GIF'), 3) = 0 then
    {$ENDIF}
      Result := pfGIF
    else if Words[1] = 1 then
      Result := pfIco;
  end;
end;

{ TGDIPPicture }

function TGDIPPicture.GetPictureFormat: TPictureFormat;
begin
  Result := FindPictureFormat(FDatastream.Memory^, FDataStream.Size);
end;

procedure TGDIPPicture.Assign(Source: TPersistent);
var
  st: TMemoryStream;
begin
  FIsEmpty := True;
  if (Source = nil) then
  begin
    FDataStream.Clear;
    FIsEmpty := True;
    DoChange;

    if Assigned(OnClear) then
      OnClear(self);
  end
  else
  begin
    if Source is TGDIPPicture then
    begin
      FDataStream.LoadFromStream(TGDIPPicture(Source).FDataStream);
      FIsEmpty := (FDataStream.Size = 0);
      DoChange;
    end
    else
    begin
      if Source is TBitmap then
      begin
        st := TMemoryStream.Create;
        (Source as TBitmap).SaveToStream(st);
        st.Position := 0;
        FDataStream.LoadFromStream(st);
        st.Free;
        FIsEmpty := (FDataStream.Size = 0);
        DoChange;
      end
      else
        if (Source is TPicture) then
        begin
          st := TMemoryStream.Create;
          (Source as TPicture).Graphic.SaveToStream(st);
          st.Position := 0;
          FDataStream.LoadFromStream(st);
          st.Free;
          FIsEmpty := False;
          DoChange;
        end
        else
        if (Source is TIcon) then
        begin
          st := TMemoryStream.Create;
          (Source as TIcon).SaveToStream(st);
          st.Position := 0;
          FDataStream.LoadFromStream(st);
          st.Free;
          FIsEmpty := False;
          DoChange;
        end
        {$IFDEF DELPHIXE_LVL}
        else
        if (Source is TPNGImage) then
        begin
          st := TMemoryStream.Create;
          (Source as TPNGImage).SaveToStream(st);
          st.Position := 0;
          FDataStream.LoadFromStream(st);
          st.Free;
          FIsEmpty := False;
          DoChange;
        end
        {$ENDIF}
        else
          if (Source is TGraphic) then
          begin
            st := TMemoryStream.Create;
            (Source as TGraphic).SaveToStream(st);
            st.Position := 0;
            FDataStream.LoadFromStream(st);
            st.Free;
            FIsEmpty := false;
            DoChange;
          end;
    end;

    GetImageSizes;
  end;
end;

constructor TGDIPPicture.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FIsEmpty := True;
  FAngle := 0;
  FTransparentBitmap := true;
end;

destructor TGDIPPicture.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure TGDIPPicture.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TGDIPPicture.DrawImageRect(Graphics: TGPGraphics; X,Y,W,H: integer);
var
  multi: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  hr: HResult;
begin
  if Empty or (FDataStream.Size = 0) then
    Exit;

  hGlobal := GlobalAlloc(GMEM_MOVEABLE, FDataStream.Size);

  if (hGlobal = 0) then
    raise Exception.Create(GDIPERRMSG);

  pstm := nil;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);
  if hr = S_OK then
  begin
    pcbWrite := 0;
    pstm.Write(FDataStream.Memory, FDataStream.Size, @pcbWrite);

    if (pcbWrite = FDataStream.Size) then
    begin
      multi := TGPImage.Create(pstm);
      graphics.DrawImageRect(multi,x,y,w,h);
      multi.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);
end;


procedure TGDIPPicture.DrawImage(Graphics: TGPGraphics; X,Y: integer);
var
  multi: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  hr: HResult;
begin
  if Empty or (FDataStream.Size = 0) then
    Exit;

  hGlobal := GlobalAlloc(GMEM_MOVEABLE, FDataStream.Size);
  if (hGlobal = 0) then
    raise Exception.Create(GDIPERRMSG);

  pstm := nil;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);
  if hr = S_OK then
  begin
    pcbWrite := 0;
    pstm.Write(FDataStream.Memory, FDataStream.Size,@pcbWrite);

    if (pcbWrite = FDataStream.Size) then
    begin
      multi := TGPImage.Create(pstm);
      graphics.DrawImage(multi,x,y);
      multi.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);
end;

procedure TGDIPPicture.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  dc: HDC;
  multi: TGPImage;
  graphic: TGPGraphics;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  bmp: TBitmap;
  hr: HResult;
  mr: TGPMatrix;
  p: TGPPointF;

begin
  if Empty or (FDataStream.Size = 0) then
    Exit;

  hGlobal := GlobalAlloc(GMEM_MOVEABLE, FDataStream.Size);

  if (hGlobal = 0) then
    raise Exception.Create(GDIPERRMSG);

  pstm := nil;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if (hr = S_OK) then
  begin
    pcbWrite := 0;
    pstm.Write(FDataStream.Memory, FDataStream.Size, @pcbWrite);

    if (FDataStream.Size = pcbWrite) then
    begin
      dc := ACanvas.Handle;

      graphic:= TGPgraphics.Create(dc);
      multi := TGPImage.Create(pstm);

      if (multi.GetFormat = ifBMP) and TransparentBitmap then
      begin // use this alternative for easy bitmap auto transparent drawing
        bmp := TBitmap.Create;
        try
          FDataStream.Position := 0;
          bmp.LoadFromStream(FDataStream);
          bmp.TransparentMode := tmAuto;
          bmp.Transparent := true;
          ACanvas.StretchDraw(Rect,bmp);
        finally
          bmp.Free;
        end;
      end
      else
      begin
        FWidth := multi.GetWidth;
        FHeight := multi.GetHeight;

        mr := nil;

        if Angle <> 0 then
        begin
          mr := TGPMatrix.Create;

          p.x := Rect.Left + FWidth / 2;
          p.y := Rect.Top + FHeight / 2;

          mr.RotateAt(Angle, p);

          graphic.SetTransform(mr);
        end;

        graphic.DrawImageRect(multi, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);

        if Assigned(mr) then
        begin
          graphic.ResetTransform;
          mr.Free;
        end;

      end;

      multi.Free;
      graphic.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);
end;

function TGDIPPicture.GetImageSizes: boolean;
var
  multi: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  hr: HResult;
begin
  Result := false;

  if Empty or (FDataStream.Size = 0) then
    Exit;

  hGlobal := GlobalAlloc(GMEM_MOVEABLE, FDataStream.Size);
  if (hGlobal = 0) then
    raise Exception.Create(GDIPERRMSG);

  pstm := nil;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if (hr = S_OK) then
  begin
    pcbWrite := 0;
    pstm.Write(FDataStream.Memory, FDataStream.Size, @pcbWrite);

    if (pcbWrite = FDataStream.Size) then
    begin
      multi := TGPImage.Create(pstm);
      try
        FWidth := multi.GetWidth;
        FHeight := multi.GetHeight;
        Result := true;
      finally
        FreeAndNil(multi);
      end;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);
end;

function TGDIPPicture.GetEmpty: Boolean;
begin
  Result := FIsEmpty;
end;

function TGDIPPicture.GetFileName: String;
begin
  Result := FFileName;
end;

function TGDIPPicture.GetHeight: Integer;
begin  
  Result := FHeight;
end;

function TGDIPPicture.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TGDIPPicture.LoadFromFile(const FileName: string);
begin
  try
    FDataStream.LoadFromFile(Filename);

    FIsEmpty := False;
    FFileName := FileName;

    if Assigned(OnClear) then
      OnClear(self);

    GetImageSizes;

    if Assigned(OnChange) then
      OnChange(Self);

  except
    FIsEmpty := True;
  end;
end;

procedure TGDIPPicture.LoadFromStream(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;

    GetImageSizes;
        
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGDIPPicture.ReadData(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
  end;
end;

procedure TGDIPPicture.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then
    FDataStream.SaveToStream(Stream);
end;


procedure TGDIPPicture.SetHeight(Value: Integer);
begin
  inherited;
end;

procedure TGDIPPicture.SetWidth(Value: Integer);
begin
  inherited;
end;

procedure TGDIPPicture.LoadFromResourceName(Instance: THandle; const ResName: string);
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

procedure TGDIPPicture.LoadFromResourceID(Instance: THandle; ResID: Integer);
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

procedure TGDIPPicture.WriteData(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.SaveToStream(stream);
  end;
end;

procedure TGDIPPicture.LoadFromURL(URL: string);
begin
  if (Pos('RES://', UpperCase(URL)) = 1) then
  begin
    Delete(URL, 1, 6);
    if (URL <> '') then
      LoadFromResourceName(hinstance, URL);
    Exit;
  end;

  if (Pos('FILE://', UpperCase(URL)) = 1) then
  begin
    Delete(URL, 1, 7);
    if (URL <> '') then
      LoadFromFile(URL);
  end;
end;

procedure TGDIPPicture.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
end;

procedure TGDIPPicture.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
end;


end.
