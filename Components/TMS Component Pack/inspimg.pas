{********************************************************************}
{ TInspectorImage component                                          }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2000 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit InspImg;

{$I TMSDEFS.INC}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComObj,
  ActiveX;

type
  TInspImage = class(TGraphic)
  private
    { Private declarations }
    FDatastream: TMemoryStream;
    FIsEmpty: Boolean;
    gpPicture: IPicture;
    FLogPixX,FLogPixY: Integer;
    procedure LoadPicture;
  protected
    { Protected declarations }
    function GetEmpty: Boolean; override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetHeight(Value: integer); override;
    procedure SetWidth(Value: integer); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  public
    { Public declarations }
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
  published
    { Published declarations }
  end;

implementation
const
  HIMETRIC_INCH = 2540;


{ TInspImage }

procedure TInspImage.Assign(Source: TPersistent);
begin
  FIsEmpty := True;
  gpPicture := nil;
  if Source = nil then
  begin
    FDataStream.Clear;
    if assigned(OnChange) then OnChange(self);
  end
  else
  begin
    if (Source is TInspImage) then
    begin
      FDataStream.LoadFromStream(TInspImage(Source).fDataStream);
      FIsEmpty := False;
      LoadPicture;
      if Assigned(OnChange) then OnChange(self);
    end;
  end;
end;

constructor TInspImage.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FIsEmpty := True;
  gpPicture := nil;
  FLogPixX := 96;
  FLogPixY := 96;
end;

destructor TInspImage.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure TInspImage.LoadPicture;
const
  IID_IPicture: TGUID = (
    D1:$7BF80980;D2:$BF32;D3:$101A;D4:($8B,$BB,$00,$AA,$00,$30,$0C,$AB));

var
  hGlobal: THandle;
  pvData: Pointer;
  pstm: IStream;
  hr: HResult;

begin
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, fDataStream.Size);
  if (hGlobal = 0) then
    raise Exception.Create('Could not allocate memory for image');

  pvData := GlobalLock(hGlobal);

  FDataStream.Position := 0;
  FDataStream.ReadBuffer(pvData^,fDataStream.Size);

  GlobalUnlock(hGlobal);

  pstm := nil;

  // create IStream* from global memory
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

    else if (gpPicture = nil) then
      raise Exception.Create('Could not load image');
end;

procedure TInspImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  hmWidth: Integer;
  hmHeight: Integer;
begin
  if Empty then Exit;
  if (gpPicture = nil) then Exit;

  hmWidth  := 0;
  hmHeight := 0;
  gpPicture.get_Width(hmWidth);
  gpPicture.get_Height(hmHeight);

  gpPicture.Render(ACanvas.Handle,rect.Left,rect.Top,rect.Right - rect.Left,(rect.Bottom - rect.Top),0,hmHeight,
    hmWidth,-hmHeight,rect);
end;

function TInspImage.GetEmpty: Boolean;
begin
  Result := FIsEmpty;
end;

function TInspImage.GetHeight: integer;
var
  hmHeight: Integer;
  hScreenDC: HDC;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    hScreenDC := GetDC(0);
    gpPicture.get_Height(hmHeight);
    Result := MulDiv(hmHeight, GetDeviceCaps(hScreenDC, LOGPIXELSY), HIMETRIC_INCH);
    ReleaseDC(0, hScreenDC);
  end;
end;

function TInspImage.GetWidth: Integer;
var
  hmWidth: Integer;
  hScreenDC: HDC;
begin
  if gpPicture = nil then
    Result := 0
  else
  begin
    hScreenDC := GetDC(0);
    gpPicture.get_Width(hmWidth);
    Result := MulDiv(hmWidth, GetDeviceCaps(hScreenDC, LOGPIXELSX), HIMETRIC_INCH);
    ReleaseDC(0, hScreenDC);
  end;
end;

procedure TInspImage.LoadFromFile(const FileName: string);
begin
  try
    FDataStream.LoadFromFile(Filename);
    FIsEmpty := False;
    LoadPicture;
    if Assigned(OnChange) then OnChange(self);
  except
    FIsEmpty := True;
  end;
end;

procedure TInspImage.LoadFromStream(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
    LoadPicture;
    if Assigned(OnChange) then OnChange(self);
  end;
end;

procedure TInspImage.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TInspImage.LoadFromResourceID(Instance: THandle; ResID: Integer);
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

procedure TInspImage.ReadData(Stream: TStream);
begin
  if Assigned(Stream) then
  begin
    FDataStream.LoadFromStream(Stream);
    FIsEmpty := False;
    LoadPicture;
  end;
end;

procedure TInspImage.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then FDataStream.SaveToStream(Stream);
end;

procedure TInspImage.SetHeight(Value: integer);
begin

end;

procedure TInspImage.SetWidth(Value: integer);
begin

end;

procedure TInspImage.WriteData(Stream: TStream);
begin
  if assigned(Stream) then
  begin
    FDataStream.SaveToStream(stream);
  end;
end;


procedure TInspImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin

end;

procedure TInspImage.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin

end;




end.
