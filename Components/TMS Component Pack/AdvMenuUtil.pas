{***************************************************************************}
{ TAdvMenu & TAdvPopupMenu component                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2012                                        }
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

unit AdvMenuUtil;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Graphics, SysUtils, Menus
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

resourcestring
  SInvalidPropertyIndexAt = 'Invalid property index %d at %s.';
  SInvalidPropertyIndex = 'Invalid property index %d.';
  SRequireOwner = 'Tried to create %s with no owner.';

type
  EInvalidPropertyIndex = class(Exception);
  ERequireOwner = class(Exception);

  TColorQuad = record
    Red, Green, Blue, Alpha: Byte;
  end;

  TSmallColorQuad = record
    Red, Green, Blue: Byte;
  end;

  TLargeColorQuad = record
    Red, Green, Blue, Alpha: Longint;
  end;

  T24bitScanLineElement = record
    Blue, Green, Red: Byte;
  end;

  T32bitScanLineElement = record
    Blue, Green, Red, Alpha: Byte;
  end;

  P24bitQuadScanLine = ^T24bitQuadScanLine;
  T24bitQuadScanLine = array[0..High(Word) div 3] of T24bitScanLineElement;

  P32bitQuadScanLine = ^T32bitQuadScanLine;
  T32bitQuadScanLine = array[0..High(Word) div 3] of T32bitScanLineElement;

  TOpacity = 0..255;

  TSetLayeredWindowAttributes = function(hWnd: THandle; crKey: TColorRef; bAlpha: Byte;
    dwFlags: Cardinal): BOOL; stdcall;
  TUpdateLayeredWindow = function(hWnd: THandle; hdcDst: HDC; pptDst: PPoint;
    psize: PSize; hdcSrc: HDC; pptSrc: PPoint; crKey: TColorRef;
    pblend: PBlendFunction; dwFlags: Cardinal): BOOL; stdcall;

const
  LWA_ALPHA     = $00000002;
  LWA_COLORKEY  = $00000001;
  ULW_COLORKEY  = $00000001;
  ULW_ALPHA     = $00000002;
  ULW_OPAQUE    = $00000004;
  AC_SRC_ALPHA  = $01;
  WS_EX_LAYERED = $00080000;

function SupportsLayeredWindows: Boolean;
function SetWindowLayered(Handle: THandle; Value: Boolean): Boolean;

function ColorTo24bitScanLineElement(Color: TColor): T24bitScanLineElement;
function ColorTo32bitScanLineElement(Color: TColor): T32bitScanLineElement;
procedure ColorToRGBVal(AColor: TColor; var R,G,B: Integer);
function RGB(Red, Green, Blue: Byte; Alpha: Byte = $00): TColor;
function Min(Value1, Value2: Integer): Integer;
function Max(Value1, Value2: Integer): Integer;

function CreateRotatedFont(F: TFont; const Angle: Integer): HFont;

function RectWidth(const ARect: TRect): Integer;
function RectHeight(const ARect: TRect): Integer;
function RectInRect(const Source, Target: TRect): Boolean;
function CenterPoint(const Rect: TRect): TPoint;
procedure CenterRect(var Rect: TRect; const Width, Height: Integer);

function BitmapRect(const ABitmap: TBitmap): TRect;

var
  NilRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  UpdateLayeredWindow: TUpdateLayeredWindow = nil;
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

implementation

var
  User32Dll: HMODULE;
  FSupportsLayeredWindows: Boolean;

{ ============================================================================
  CreateRotatedFont
  Description: Creates rotated font, returns handle to it
  ---------------------------------------------------------------------------- }
function CreateRotatedFont(F: TFont; const Angle: Integer): HFont;
var
  LF : TLogFont;
begin
  FillChar(LF, SizeOf(LF), #0);
  with LF do
  begin
    lfHeight := F.Height;
    lfWidth := 0;
    lfEscapement := Angle * 10;
    lfOrientation := 0;
    if fsBold in F.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in F.Style);
    lfUnderline := Byte(fsUnderline in F.Style);
    lfStrikeOut := Byte(fsStrikeOut in F.Style);
    lfCharSet := DEFAULT_CHARSET;

    StrPCopy(lfFaceName, F.Name);

    lfQuality := DEFAULT_QUALITY;

    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case F.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LF);
end;


function RectInRect(const Source, Target: TRect): Boolean;
begin
  Result := (PtInRect(Target, Source.TopLeft) and PtInRect(Target, Source.BottomRight));
end;

function RectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function RectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  with Rect do
  begin
    Result.X := (Right - Left) div 2 + Left;
    Result.Y := (Bottom - Top) div 2 + Top;
  end;
end;

procedure CenterRect(var Rect: TRect; const Width, Height: Integer);
begin
  Rect.Right := (Rect.Left + Rect.Right + Width) div 2;
  Rect.Bottom := (Rect.Top + Rect.Bottom + Height) div 2;
  Rect.Left := Rect.Right - Width;
  Rect.Top := Rect.Bottom - Height;
end;

function BitmapRect(const ABitmap: TBitmap): TRect;
begin
  if Assigned(ABitmap) then
    Result := Rect(0, 0, ABitmap.Width, ABitmap.Height)
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure ColorToRGBVal(AColor: TColor; var R,G,B: Integer);
var
  rgb: Integer;
begin
  rgb := ColorToRGB(AColor);

  R := TColorQuad(rgb).Red;
  G := TColorQuad(rgb).Green;
  B := TColorQuad(rgb).Blue;

end;

function ColorTo24bitScanLineElement(Color: TColor): T24bitScanLineElement;
begin
  Result.Red := TColorQuad(Color).Red;
  Result.Green := TColorQuad(Color).Green;
  Result.Blue := TColorQuad(Color).Blue;

end;

function ColorTo32bitScanLineElement(Color: TColor): T32bitScanLineElement;
begin
  Result.Red := TColorQuad(Color).Red;
  Result.Green := TColorQuad(Color).Green;
  Result.Blue := TColorQuad(Color).Blue;
  Result.Alpha := TColorQuad(Color).Alpha;
end;

function RGB(Red, Green, Blue: Byte; Alpha: Byte = $00): TColor;
begin
  Result := (Alpha shl 24) or (Blue shl 16) or (Green shl 8) or Red;
end;

function Min(Value1, Value2: Integer): Integer;
begin
  if Value1 > Value2 then
    Result := Value2
  else
    Result := Value1;
end;

function Max(Value1, Value2: Integer): Integer;
begin
  if Value1 < Value2 then
    Result := Value2
  else
    Result := Value1;
end;



function SupportsLayeredWindows: Boolean;
begin
  Result := FSupportsLayeredWindows;
end;

function SetWindowLayered(Handle: THandle; Value: Boolean): Boolean;
var Flags: Integer;
begin
  Result := True;
  Flags := GetWindowLong(Handle, GWL_EXSTYLE);
  if Value then
  begin
    if ((Flags and WS_EX_LAYERED) = 0) then
      SetWindowLong(Handle, GWL_EXSTYLE, Flags or WS_EX_LAYERED)
  end else if (Flags and WS_EX_LAYERED) <> 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, Flags and not WS_EX_LAYERED);
end;

initialization
  User32Dll := LoadLibrary(user32);
  FSupportsLayeredWindows := False;
  if User32Dll <> 0 then
  begin
    UpdateLayeredWindow := GetProcAddress(User32Dll, 'UpdateLayeredWindow');
    SetLayeredWindowAttributes := GetProcAddress(User32Dll, 'SetLayeredWindowAttributes');
    FreeLibrary(User32Dll);

    FSupportsLayeredWindows := Assigned(SetLayeredWindowAttributes);
  end;

end.

