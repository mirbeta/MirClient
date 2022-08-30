{***************************************************************************}
{ TAdvReflectionImage component                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2010                                        }
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

unit AdvReflectionImage;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, GDIPicture, AdvHintInfo, AdvGDIP;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with OnClick event
  // v1.5.0.0 : New : property ReflectionSize added
  //          : New : method SaveToFile added
  //          : New : properties to control start & end reflection opacity
  // v1.5.0.1 : Fixed : issue with design time dotted border painted at runtime
  // v1.5.0.2 : Fixed : issue with SaveToFile()


type

  TImageType = (itPNG, itBMP, itJPEG, itTIFF, itGIF);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvReflectionImage = class(TGraphicControl)
  private
    FAutoSize: Boolean;
    FMouseInControl: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOfficeHint: TAdvHintInfo;
    FIPicture: TGDIPPicture;
    FReflectionPic: TGPBitmap;
    FReflectionOpacityStart: Integer;
    FReflectionOpacityEnd: integer;
    FReflectionSize: integer;
    FReflectionAxis: Integer;
    procedure OnPictureChanged(Sender: TObject);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetIPicture(const Value: TGDIPPicture);
    procedure SetReflectionOpacityStart(const Value: Integer);
    procedure SetReflectionOpacityEnd(const Value: Integer);
    procedure SetReflectionSize(const Value: integer);
    procedure SetReflectionAxis(const Value: Integer);
    procedure SetAutoSizeEx(const Value: Boolean);
  protected
    procedure DrawImage(ACanvas: TCanvas); virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure UpdateReflection;
    procedure UpdateSize;
    property MouseInControl: Boolean read FMouseInControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; virtual;
    procedure SaveToFile(FileName: string; ImageType: TImageType = itPng);
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FAutoSize write SetAutoSizeEx;
    property BiDiMode;
    property Constraints;
    property Picture: TGDIPPicture read FIPicture write SetIPicture;
    property PopupMenu;
    property ShowHint;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property ReflectionOpacityStart: Integer read FReflectionOpacityStart write SetReflectionOpacityStart default 255;
    property ReflectionOpacityEnd: Integer read FReflectionOpacityEnd write SetReflectionOpacityEnd default 0;
    property ReflectionSize: integer read FReflectionSize write SetReflectionSize default 100;
    property ReflectionAxis: Integer read FReflectionAxis write SetReflectionAxis default 1;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;


implementation

uses
  ActiveX;
//------------------------------------------------------------------------------

{ TAdvReflectionImage }

constructor TAdvReflectionImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := OnPictureChanged;

  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];

  FOfficeHint := TAdvHintInfo.Create;

  ShowHint := False;
  Width := 32;
  Height := 32;
  FReflectionPic := nil;
  FReflectionOpacityStart := 255;
  FReflectionOpacityEnd := 0;
  FReflectionSize := 100;
  FReflectionAxis := 1;
end;

//------------------------------------------------------------------------------

destructor TAdvReflectionImage.Destroy;
begin
  FIPicture.Free;
  FOfficeHint.Free;
  if Assigned(FReflectionPic) then
    FReflectionPic.Free;
  inherited;
end;

(*
procedure TAdvReflectionImage.SaveToImage(Filename: String; ImageWidth, ImageHeight: integer; ImageType: TChartImageType = itBMP; ImageQualityPercentage: integer = 100);
var
  img: graphics.TBitmap;
  gpimg: TGPImage;
  g: TGPGraphics;
  enc: TEncoderParameters;
begin
  img := nil;
  gpimg := nil;
  g := nil;
  try
    img := graphics.TBitmap.Create;
    img.Width := ImageWidth;
    img.Height := ImageHeight;

    SaveChart(img.Canvas, rect(0, 0, ImageWidth, ImageHeight));

    gpimg := TGPImage.Create(CreateStream(img));

    enc := GetEncoderQualityParameters(ImageQualityPercentage);

    gpimg.Save(filename, GetCLSID(ImageType), @enc);

  finally
    gpimg.Free;
    g.Free;
    img.Free;
  end;
end;
*)

function GetCLSID(ImageType: TImageType): TCLSID;
var
  I: integer;
  num, numi, size: Cardinal;
  clsId: TCLSID;
  pinfo: PImageCodecInfo;
  infoarr: array[0..100] of TImageCodecInfo;
  str: String;
begin
  GdipGetImageEncodersSize(num, size);

  pinfo := AllocMem(size);

  numi := num;

  GdipGetImageEncoders(num, size, pinfo);

  move(pinfo^, infoarr[0], size);

  case ImageType of
    itPNG: str := 'image/png';
    itBMP: str := 'image/bmp';
    itJPEG: str := 'image/jpeg';
    itTIFF: str := 'image/tiff';
    itGIF: str := 'image/gif';
  end;

  for I := 0 to numi - 1 do
  begin
    if infoarr[i].MimeType = str then
    begin
      clsid := infoarr[i].Clsid;
      break;
    end;
  end;

  FreeMem(pinfo);

  Result := clsid;
end;


//------------------------------------------------------------------------------

procedure TAdvReflectionImage.SaveToFile(Filename: string; ImageType: TImageType = itPng);
var
  gpbmp: TGPBitmap;
  g: TGPGraphics;
  enc: TEncoderParameters;
begin
  //
  if Assigned(FReflectionPic) then
  begin
    gpbmp := TGPBitmap.Create(Picture.Width, Picture.Height + FReflectionSize + FReflectionAxis);

    g := TGPGraphics.Create(gpbmp);

    Picture.DrawImageRect(g, 0, 0,picture.Width,picture.Height);

    g.DrawImage(FReflectionPic, 0, Picture.Height);

//    enc := GetEncoderQualityParameters(100);
    enc.Count := 1;
    enc.Parameter[0].Guid := EncoderQuality;
    enc.Parameter[0].NumberOfValues := 1;
    enc.Parameter[0].Type_ := EncoderParameterValueTypeLong;
    enc.Parameter[0].Value := Pointer(100);

    gpbmp.Save(filename, GetCLSID(ImageType), @enc);

    g.Free;
    gpbmp.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := true;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := false;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.Paint;
begin
  inherited;
  DrawImage(Canvas);
end;

//------------------------------------------------------------------------------


procedure TAdvReflectionImage.DrawImage(ACanvas: TCanvas);
var
  Pic: TGDIPPicture;
  x, y: Integer;
  graphics : TGPGraphics;
begin
  Pic := Picture;

  if Assigned(Pic) and not Pic.Empty then
  begin
    Pic.GetImageSizes;

    x := 0;
    y := Picture.Height;
    graphics := TGPgraphics.Create(Canvas.Handle);

    if Assigned(FReflectionPic) then
//      graphics.DrawImageRect(FReflectionPic, x, y + FReflectionAxis, Pic.Width, Pic.Height);
      graphics.DrawImage(FReflectionPic, x, y + FReflectionAxis, Pic.Width, Pic.Height);

    pic.DrawImageRect(graphics,0,0,pic.Width,pic.Height);

    graphics.Free;
  end
  else
  begin
    if (csDesigning in ComponentState) then
    begin
      ACanvas.Pen.Style := psDot;
      ACanvas.Pen.Color := clBlue;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Rectangle(ClientRect);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.UpdateSize;
begin
  if Assigned(FIPicture) then
    if not FIPicture.Empty then
    begin
      FIPicture.GetImageSizes;
      Width := FIPicture.Width;
      Height := FIPicture.Height + ReflectionSize;
    end;

end;
//------------------------------------------------------------------------------

procedure TAdvReflectionImage.UpdateReflection;
var
  gpbmp: TGPBitmap;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  w, h, x, y, op, alph: integer;
  clr, clrTemp: TGPColor;
  a: byte;
  hr: HResult;
begin
  if Picture.Empty or (csLoading in ComponentState) then
    Exit;

  Picture.GetImageSizes;

  w := Picture.Width;
  h := Picture.Height;

  ms := TMemoryStream.Create;
  Picture.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for reflection image');
  end;

  pstm := nil;
  pcbWrite := 0;


  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if (hr = S_OK) then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      gpbmp := TGPBitmap.Create(pstm);
      gpbmp.RotateFlip(RotateNoneFlipY);

      if Assigned(FReflectionPic) then
      begin
        FReflectionPic.Free;
        FReflectionPic := nil;
      end;

      FReflectionPic := TGPBitmap.Create(w, h{, PixelFormat32bppARGB});

      for y := 0 to h do
      begin
        if (y < FReflectionSize) then
        begin
          op := Round( ((FReflectionSize - y)/FReflectionSize * FReflectionOpacityStart) +
                         y/ReflectionSize * FReflectionOpacityEnd);
        end
        else
          op := 0;

  //      op := Round((255.0 / h) * (h - y)) - FReflection;

        if (op < 0) then
          op := 0;
        if (op > 255) then
          op := 255;

        for x := 0 to w do
        begin
          gpbmp.GetPixel(x, y, clr);
          a := GetAlpha(clr);
          if (a = 0) then
            continue;

          alph := Round((op / 255) * a);
          clrTemp := MakeColor(alph, GetRed(clr), GetGreen(clr), GetBlue(clr));
          FRefLectionPic.SetPixel(x, y, clrTemp);
        end;
      end;
      gpbmp.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);
  ms.Free;
end;

//------------------------------------------------------------------------------

function TAdvReflectionImage.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvReflectionImage.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.SetAutoSizeEx(const Value: Boolean);
begin
  if (Value <> FAutoSize) then
  begin
    FAutoSize := Value;
    if Value then
      UpdateSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.SetIPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.OnPictureChanged(Sender: TObject);
begin
  UpdateReflection;
  if FAutoSize then
    UpdateSize;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.Loaded;
begin
  inherited;
  UpdateReflection;
end;


//------------------------------------------------------------------------------

procedure TAdvReflectionImage.SetReflectionAxis(const Value: Integer);
begin
  if (FReflectionAxis <> Value) then
  begin
    FReflectionAxis := Value;
    Invalidate;
  end;
end;

procedure TAdvReflectionImage.SetReflectionOpacityEnd(const Value: Integer);
begin
  if (Value < 256) and (Value >= 0) then
  begin
    if (Value <> FReflectionOpacityEnd) then
    begin
      FReflectionOpacityEnd := Value;
      UpdateReflection;
      Invalidate;
    end;
  end;
end;

procedure TAdvReflectionImage.SetReflectionOpacityStart(const Value: Integer);
begin
  if (Value < 256) and (Value >= 0) then
  begin
    if (Value <> FReflectionOpacityStart) then
    begin
      FReflectionOpacityStart := Value;
      UpdateReflection;
      Invalidate;
    end;
  end;
end;

procedure TAdvReflectionImage.SetReflectionSize(const Value: integer);
begin
  if (FReflectionSize <> Value) and (FReflectionSize >= 0) then
  begin
    FReflectionSize := Value;
    UpdateReflection;
    if FAutoSize then
      UpdateSize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionImage.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

end.
