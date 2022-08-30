{********************************************************************}
{ TAdvImage component                                                }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2000 - 2011                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvImage;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Comobj, Activex;

type
  TAdvImage = class(TGraphic)
  private
    { Private declarations }
    FDatastream:TMemoryStream;
    FIsEmpty:boolean;
    gpPicture:IPicture;
    FLogPixX,fLogPixY:integer;
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

uses
  ExtCtrls;

const
 HIMETRIC_INCH = 2540;


{ TAdvImage }

procedure TAdvImage.Assign(Source: TPersistent);
var
  st: TMemoryStream;
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
    if (Source is TAdvImage) then
    begin
      if not (TAdvImage(Source).Empty) then
      begin
        FDataStream.LoadFromStream(TAdvImage(Source).fDataStream);
        FIsEmpty := false;
        LoadPicture;
        if Assigned(OnChange) then
          OnChange(self);
      end;
    end;

    if (Source is TImage) then
    begin
      st := TMemoryStream.Create;
      (Source as TImage).Picture.Graphic.SaveToStream(st);
      st.Position := 0;
      FDataStream.LoadFromStream(st);
      st.Free;
      FIsEmpty := false;
      if Assigned(OnChange) then
        OnChange(self);
    end;

    if Source is TBitmap then
    begin
      st := TMemoryStream.Create;
      (Source as TBitmap).SaveToStream(st);
      st.Position := 0;
      FDataStream.LoadFromStream(st);
      st.Free;
      FIsEmpty := false;
      if Assigned(OnChange) then
        OnChange(self);
    end;

    if (Source is TGraphic) then
    begin
      st := TMemoryStream.Create;
      (Source as TGraphic).SaveToStream(st);
      st.Position := 0;
      FDataStream.LoadFromStream(st);
      st.Free;
      FIsEmpty := false;
      if Assigned(OnChange) then
        OnChange(self);
    end;

    if (Source is TPicture) then
    begin
      st := TMemoryStream.Create;
      (Source as TPicture).Graphic.SaveToStream(st);
      st.Position := 0;
      FDataStream.LoadFromStream(st);
      st.Free;
      FIsEmpty := false;
      if Assigned(OnChange) then
        OnChange(self);
    end;
  end;
end;

constructor TAdvImage.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FIsEmpty := True;
  gpPicture := nil;
  FLogPixX := 96;
  FLogPixY := 96;
end;

destructor TAdvImage.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

procedure TAdvImage.LoadPicture;
const
  IID_IPicture: TGUID = (
    D1:$7BF80980;D2:$BF32;D3:$101A;D4:($8B,$BB,$00,$AA,$00,$30,$0C,$AB));

var
 hGlobal:thandle;
 pvData:pointer;
 pstm:IStream;
 hr:hresult;

begin
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, fDataStream.Size);
  if (hGlobal = 0) then
    raise Exception.Create('Could not allocate memory for image');

  pvData := GlobalLock(hGlobal);

  fDataStream.Position:=0;
  fDataStream.ReadBuffer(pvData^,fDataStream.Size);

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

  if (not (hr = S_OK)) then
    raise Exception.Create('Could not load image. Invalid format')
  else
    if (gpPicture = nil) then
      raise Exception.Create('Could not load image');
end;

procedure TAdvImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  hmWidth:integer;
  hmHeight:integer;
begin
  if (Empty) then Exit;
  if (gpPicture = nil) then Exit;

  hmWidth  := 0;
  hmHeight := 0;
  gpPicture.get_Width(hmWidth);
  gpPicture.get_Height(hmHeight);

  gpPicture.Render(ACanvas.Handle,Rect.Left,Rect.Bottom,Rect.Right - Rect.Left,-(Rect.Bottom-Rect.Top),0,0,
                   hmWidth,hmHeight, rect);
end;

function TAdvImage.GetEmpty: Boolean;
begin
  Result := fIsEmpty;
end;

function TAdvImage.GetHeight: integer;
var
 hmHeight:integer;
begin
  if gpPicture=nil then result:=0 else
   begin
    gpPicture.get_Height(hmHeight);
    result := MulDiv(hmHeight, fLogPixY, HIMETRIC_INCH);
   end;
end;

function TAdvImage.GetWidth: integer;
var
 hmWidth:integer;
begin
  if gpPicture=nil then result:=0 else
   begin
    gpPicture.get_Width(hmWidth);
    result := MulDiv(hmWidth, fLogPixX, HIMETRIC_INCH);
   end;

end;

procedure TAdvImage.LoadFromFile(const FileName: string);
begin
  try
   fDataStream.LoadFromFile(Filename);
   fIsEmpty:=false;
   LoadPicture;
   if assigned(OnChange) then OnChange(self);
  except
   fIsEmpty:=true;
  end;
end;

procedure TAdvImage.LoadFromStream(Stream: TStream);
begin
  if Assigned(Stream) then
   begin
     fDataStream.LoadFromStream(Stream);
     fIsEmpty:=false;
     LoadPicture;
     if assigned(OnChange) then OnChange(self);
   end;
end;

procedure TAdvImage.LoadFromResourceName(Instance: THandle; const ResName: string);
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

procedure TAdvImage.LoadFromResourceID(Instance: THandle; ResID: Integer);
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


procedure TAdvImage.ReadData(Stream: TStream);
begin

 if assigned(Stream) then
   begin
     fDataStream.LoadFromStream(stream);
     fIsEmpty:=false;
     LoadPicture;
   end;
end;

procedure TAdvImage.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then fDataStream.SaveToStream(Stream);
end;

procedure TAdvImage.SetHeight(Value: integer);
begin

end;

procedure TAdvImage.SetWidth(Value: integer);
begin

end;

procedure TAdvImage.WriteData(Stream: TStream);
begin
  if assigned(Stream) then
   begin
     fDataStream.savetostream(stream);
   end;
end;


procedure TAdvImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin

end;

procedure TAdvImage.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin


end;

end.
 