{***************************************************************************}
{ TAdvFocusHelper component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2008 - 2015                                        }
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

unit AdvFocusHelper;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Controls, Forms, Messages, SysUtils, Dialogs, Graphics,
  StdCtrls, ComCtrls, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // 1.0.0.0   : first release
  // 1.0.0.1   : Fixed : issue with incorrect TWinControl casting
  // 1.0.0.2   : Fixed : issue with partially visible controls on form
  // 1.0.0.3   : Fixed : issue with combobox DropDown vs DropDownList style for include controls
  // 1.0.0.4   : Fixed : issue for forms with option SPI_SETDRAGFULLWINDOWS turned off
  // 1.0.1.0   : New : Active property added
  //           : New : Method UpdateHelper added
  // 1.0.1.1   : Fixed : issue with InterSectRect calculation when parent windows are not visible
  // 1.0.2.0   : Improved : dynamic linking of UpdateLayeredWindow
  // 1.1.0.0   : Improved : faster blur implementation
  // 1.1.0.1   : Fixed : Issue with focus rectangle calculation
  // 1.1.0.2   : Fixed : Issue with form closing
  // 1.1.0.3   : Fixed : Issue with focus indicator on TDateTimePicker
  // 1.1.0.4   : Fixed : Issue with anchored controls during maximize
  // 1.1.0.5   : Fixed : Regression with handling maximize/restore

type
  TFocusWindow = class(TCustomControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
  end;

  TIncludeControl = (icEdit, icMemo, icListBox, icComboBox, icDateTimePicker, icTagValue);
  TExcludeControl = (ecButton, ecCheckBox, ecRadioButton, ecTagValue);

  TIncludeControls = set of TIncludeControl;
  TExcludeControls = set of TExcludeControl;

  TGlyphPosition = (gpAfterControl, gpBeforeControl);

  TOnShowFocusEvent = procedure(Sender: TObject; Control: TWinControl; var ShowFocus: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvFocusHelper = class(TComponent)
  private
    FHasHook: boolean;
    FW: TFocusWindow;
    FocusControl: THandle;
    FocusHeight: integer;
    FocusWidth: integer;
    FColor: TColor;
    FSize: integer;
    FGlyph: TBitmap;
    FBlur: integer;
    FOpacity: byte;
    FRounding: byte;
    FGlyphPosition: TGlyphPosition;
    FOnShowFocus: TOnShowFocusEvent;
    FExcludeControls: TExcludeControls;
    FIncludeControls: TIncludeControls;
    FControlTag: integer;
    FIsMaximized: boolean;
    FActive: boolean;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetBlur(const Value: integer);
    procedure SetSize(const Value: integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetActive(const Value: boolean);
  protected
    function AcceptControl(wc,wcp: TWinControl): boolean; virtual;
    procedure InitHook; virtual;
    procedure Unhook; virtual;
    function GetVersionNr: Integer; virtual;
    property IsMaximized: boolean read FIsMaximized write FIsMaximized;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHelper;
    procedure HideHelper;
    procedure MoveHelper(R: TRect);
    procedure ShowHelperOnControl(R: TRect); virtual;
    procedure UpdateHelper;
  published
    property Active: boolean read FActive write SetActive default true;
    property Blur: integer read FBlur write SetBlur default 3;
    property Color: TColor read FColor write FColor default $FDD397;
    property ControlTag: integer read FControlTag write FControlTag default 0;
    property ExcludeControls: TExcludeControls read FExcludeControls write FExcludeControls;
    property IncludeControls: TIncludeControls read FIncludeControls write FIncludeControls;
    property Size: integer read FSize write SetSize default 4;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphPosition: TGlyphPosition read FGlyphPosition write FGlyphPosition default gpAfterControl;
    property Opacity: byte read FOpacity write FOpacity default 180;
    property Rounding: byte read FRounding write FRounding default 5;
    property Version: string read GetVersion write SetVersion;    
    property OnShowFocus: TOnShowFocusEvent read FOnShowFocus write FOnShowFocus;
  end;

procedure DrawRoundRect(Bitmap: TBitmap; W, H, BMPW: Integer; BMPPos: TGlyphPosition; Thickness, Radius: Integer; Blur: Double);


implementation

const
  MaxKernelSize = 10;
  
var
  MsgHook: HHOOK;
  FocusHelper: TAdvFocusHelper;

type
  PRGBA = ^TRGBA;
  TRGBA = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Alpha: Byte;
  end;

  PRGB = ^TRGB;
  TRGB = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;

  PRGBTriplet = ^TRGBTriplet;
  TRGBTriplet = packed record
    b: byte;
    g: byte;
    r: byte;
  end;

  TKernelSize = 1..MaxKernelSize;
  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of single;
  end;
  PKernel = ^TKernel;

  PBlendFunction = ^TBlendFunction;
 _BLENDFUNCTION = packed record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
 end;
 TBlendFunction = _BLENDFUNCTION;


  PRow = ^TRow;
  TRow = array[0..10000] of TRGBTriplet;
  PPRows = ^TPRows;
  TPRows = array[0..10000] of PRow;

{ TAlphaMask }

type
  TAlphaMask = class(TBitmap)
  private
    FOpacity: Byte;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure MergeMask(Mask, Bitmap: TBitmap; Color: TColor);
    property Opacity: Byte read FOpacity write FOpacity;
  end;

function DynaLink_SetLayeredWindowAttributes(HWND: thandle; crKey: DWORD; bAlpha: byte; dwFlags: DWORD): boolean;
var
  UserDLL: THandle;
  user_SetLayeredWindowAttributes: function(HWND: thandle; crKey: DWORD; bAlpha: byte; dwFlags: DWORD): DWORD; stdcall;

begin
  Result := TRUE;
  UserDLL := GetModuleHandle('USER32.DLL');
  if (UserDLL > 0) then
  begin
    @user_SetLayeredWindowAttributes := GetProcAddress(UserDLL, 'SetLayeredWindowAttributes');
    if Assigned(user_SetLayeredWindowAttributes) then
    begin
      Result := user_SetLayeredWindowAttributes(hwnd, crKey, bAlpha, dwFlags) <> 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

function DynaLink_UpdateLayeredWindow(hwnd, hdcDst: thandle;
  pptDst, size: ppoint; hdcSrc: thandle;
  pptSrc: ppoint;
  crKey: dword;
  var pblend: _BLENDFUNCTION;
  dwFlags: DWORD): boolean;

var
  UserDLL: THandle;
  user_UpdateLayeredWindow: function(hwnd, hdcDst: thandle;
    pptDst, size: ppoint; hdcSrc: thandle;
    pptSrc: ppoint;
    crKey: dword;
    var pblend: _BLENDFUNCTION;
    dwFlags: DWORD): DWORD; stdcall;

begin
  Result := TRUE;
  UserDLL := GetModuleHandle('USER32.DLL');
  if (UserDLL > 0) then
  begin
    @user_UpdateLayeredWindow := GetProcAddress(UserDLL, 'UpdateLayeredWindow');
    if Assigned(user_UpdateLayeredWindow) then
    begin
      Result := user_UpdateLayeredWindow(hwnd, hdcDst, pptDst, size, hdcSrc, pptSrc, crKey, pblend, dwFlags) <> 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

constructor TAlphaMask.Create;
begin
  inherited Create;
  PixelFormat := pf32bit;
  FOpacity := $FF;
end;

procedure TAlphaMask.Assign(Source: TPersistent);
var
  AlphaMap: TAlphaMask absolute Source;
  I: Integer;
begin
  if Source is TAlphaMask then
  begin
    Opacity := AlphaMap.Opacity;
    for I := 0 to Height - 1 do
      Move(AlphaMap.ScanLine[I]^, ScanLine[I]^, Width * SizeOf(TRGBA));
  end
  else
    inherited Assign(Source);
  PixelFormat := pf32bit;
end;


procedure TAlphaMask.MergeMask(Mask, Bitmap: TBitmap; Color: TColor);
var
  Fill: TRGBA;
  Swap: Byte;
  A: PRGBA;
  B: PRGB;
  C: PRGB;
  X, Y: Integer;
begin
  if Bitmap.PixelFormat <> pf24Bit then
    Exit;

  Width := Bitmap.Width;
  Height := Bitmap.Height;

  Fill := TRGBA(ColorToRGB(Color));
  Swap := Fill.Blue;
  Fill.Blue := Fill.Red;
  Fill.Red := Swap;

  for Y := 0 to Height - 1 do
  begin
    A := ScanLine[Y];
    B := Bitmap.Scanline[Y];
    C := Mask.ScanLine[Y];

    for X := 0 to Width - 1 do
    begin
      A.Alpha := C.Red;
      A.Red := Round(B.Red * (C.Red / $FF));
      A.Green := Round(B.Green * (C.Red / $FF));
      A.Blue := Round(B.Blue * (C.Red / $FF));

      Inc(A);
      Inc(B);
      Inc(C);
    end;
  end;
end;


procedure UpdateAlphaWindow(Wnd: HWND; AlphaMap: TAlphaMask);
var
  Blend: _BlendFunction;
  Rect: TRect;
  P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
  if AlphaMap.Height = 0 then
    Exit;

  SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
  GetWindowRect(Wnd, Rect);
  P1.X := Rect.Left;
  P1.Y := Rect.Top;
  SetWindowPos(Wnd, 0, 0, 0, AlphaMap.Width, AlphaMap.Height, SWP_NOACTIVATE or SWP_NOMOVE);

  with Blend do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := AlphaMap.Opacity;
    AlphaFormat := AC_SRC_ALPHA;
  end;

  DC := GetDC(0);
  P2 := Point(0, 0);
  S.cx := AlphaMap.Width;
  S.cy := AlphaMap.Height;

  Dynalink_UpdateLayeredWindow(Wnd, DC, @P1, @S, AlphaMap.Canvas.Handle, @P2, 0, Blend, ULW_ALPHA);

  ReleaseDC(0, DC);
end;


procedure MakeGaussianKernel(var K: TKernel; radius: double; MaxData, DataGranularity: double);
var
  j: integer; temp, delta: double;
  KernelSize: TKernelSize;

begin
  for j := Low(K.Weights) to High(K.Weights) do
  begin
    temp := j/radius;
    K.Weights[j] := exp(- temp*temp/2);
  end;

  temp := 0;

  for j := Low(K.Weights) to High(K.Weights) do
    temp := temp + K.Weights[j];

  for j := Low(K.Weights) to High(K.Weights) do
    K.Weights[j] := K.Weights[j] / temp;

  KernelSize := MaxKernelSize;
  delta := DataGranularity / (2*MaxData);
  temp := 0;

  while (temp < delta) and (KernelSize > 1) do
  begin
    temp := temp + 2 * K.Weights[KernelSize];
    dec(KernelSize);
  end;

  K.Size := KernelSize;

  temp := 0;

  for j := -K.Size to K.Size do
    temp := temp + K.Weights[j];

  for j := -K.Size to K.Size do
    K.Weights[j] := K.Weights[j] / temp;
end;


function TrimInt(Lower, Upper, theInteger: integer): integer;
begin
  if (theInteger <= Upper) and (theInteger >= Lower) then
    Result := theInteger
  else
    if theInteger > Upper then
      Result := Upper
    else
      Result := Lower;
end;

function TrimReal(Lower, Upper: integer; x: double): integer;
begin
  if (x < upper) and (x >= lower) then
    Result := trunc(x)
  else
    if x > Upper then
      Result := Upper
    else
      Result := Lower;
end;

procedure BlurRow(var theRow: array of TRGBTriplet; K: PKernel; P: PRow);
var
  j, n: integer; tr, tg, tb: double; //tempRed, etc
  w: double;

begin
  for j:= 0 to High(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;

    for n := -K.Size to K.Size do
    begin
      w := K.Weights[n];

      with theRow[TrimInt(0, High(theRow), j - n)] do
      begin
        tb := tb + w * b;
        tg := tg + w * g;
        tr := tr + w * r;
      end;
    end;

    with P[j] do
    begin
      b := TrimReal(0, 255, tb);
      g := TrimReal(0, 255, tg);
      r := TrimReal(0, 255, tr);
    end;
  end;

  Move(P[0], theRow[0], (High(theRow) + 1) * Sizeof(TRGBTriplet));
end;

procedure GBlur(theBitmap: TBitmap; radius: double);
var
  i,
  Row, Col: integer;
  theRows: PPRows;
  K: TKernel;
  ACol: PRow; P:PRow;

begin
  if (theBitmap.HandleType <> bmDIB) or (theBitmap.PixelFormat <> pf24Bit) then
    raise exception.Create('Blur only works for 24-bit bitmaps');

  MakeGaussianKernel(K, radius, 255, 1);

  GetMem(theRows, theBitmap.Height * SizeOf(PRow));
  GetMem(ACol, theBitmap.Height * SizeOf(TRGBTriplet));

  for Row := 0 to theBitmap.Height - 1 do
    theRows[Row] := theBitmap.Scanline[Row];

  P := AllocMem(theBitmap.Width*SizeOf(TRGBTriplet));
  for Row := 0 to theBitmap.Height - 1 do
  begin
    //Optimization: rows between 20 and Height - 20 are the same...
    if (Row > 20) and (Row < theBitmap.Height - 20) then
      //...so copy a previous row
      for i := 0 to theBitmap.Width do
        theRows[Row][i] := theRows[15][i]
    else
      BlurRow(Slice(theRows[Row]^, theBitmap.Width), @K, P);
  end;  

  ReAllocMem(P, theBitmap.Height*SizeOf(TRGBTriplet));

  for Col := 0 to theBitmap.Width - 1 do
  begin
    //Optimization: rows between 20 and With - 20 are the same...
    if (Col > 20) and (Col < theBitmap.Width - 20) then
    begin
      //...so copy a previous col
      for Row := 0 to theBitmap.Height - 1 do
        theRows[Row][Col]:= theRows[Row][15];
      Continue;
    end;

    for Row := 0 to theBitmap.Height - 1 do
      ACol[Row] := theRows[Row][Col];

    BlurRow(Slice(ACol^, theBitmap.Height), @K, P);

    for Row := 0 to theBitmap.Height - 1 do
     theRows[Row][Col]:= ACol[Row];
  end;

  FreeMem(theRows);
  FreeMem(ACol);
  ReAllocMem(P, 0);
end;

procedure DrawRoundRect(Bitmap: TBitmap; W, H, BMPW: Integer; BMPPOS: TGlyphPosition; Thickness, Radius: Integer; Blur: Double);
var
  DC: HDC;
  P: HPEN;
  B: HBRUSH;
  I: Integer;
begin
  Bitmap.PixelFormat := pf24bit;

  Bitmap.Width := W;
  Bitmap.Height := H;

  DC := Bitmap.Canvas.Handle;

  // outer black rectangle
  FillRect(DC, Rect(0, 0, Bitmap.Width, Bitmap.Height), GetStockObject(BLACK_BRUSH));

  if Thickness > 0 then
  begin
    P := SelectObject(DC, CreatePen(PS_SOLID, Thickness, $FFFFFF));
    B := SelectObject(DC, GetStockObject(BLACK_BRUSH));
  end
  else
  begin
    P := SelectObject(DC, GetStockObject(WHITE_PEN));
    B := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  end;

  I := 2;
  if BMPPOS = gpAfterControl then
    RoundRect(DC, I + Thickness shr 1, I + Thickness shr 1, W - BMPW - I - Thickness shr 1, H - I - Thickness shr 1, Radius, Radius)
  else
    RoundRect(DC, I + BMPW + Thickness shr 1, I + Thickness shr 1, W - I - Thickness shr 1, H - I - Thickness shr 1, Radius, Radius);

  if Thickness > 0 then
  begin
    SelectObject(DC, B);
    DeleteObject(SelectObject(DC, P));
  end
  else
  begin
    SelectObject(DC, B);
    SelectObject(DC, P);
  end;

  if (Blur > 0) then
    GBlur(Bitmap, Blur);
end;

//------------------------------------------------------------------------------

procedure WindowBlend(hwnd,hdc: THandle;Colorkey: TColor;Alpha:byte; r:trect);
var
  dw: dword;
  blnd: _BLENDFUNCTION;
  dskdc: THandle;
  size,src: TPoint;
  //si : TSize;
begin
  dw := GetWindowLong(hwnd, GWL_EXSTYLE);
  SetWindowLong(hwnd, GWL_EXSTYLE,dw or WS_EX_LAYERED);

  Dynalink_SetLayeredWindowAttributes(hwnd,DWORD(colorkey),Alpha,2);

  blnd.BlendOp := AC_SRC_OVER;
  blnd.BlendFlags := 0;
  blnd.SourceConstantAlpha := 0;
  blnd.AlphaFormat := 0;

  dskdc := getdc(0);
  size := point(r.right-r.left,r.bottom-r.top);
  src := point(r.left,r.top);
  Dynalink_UpdateLayeredWindow(hwnd, dskdc, nil, @size, hdc, @src, dword(colorkey), blnd, ULW_ALPHA);
  ReleaseDC(hwnd,dskdc);
end;

//------------------------------------------------------------------------------

function IsProcessWindow(Wnd: HWND): Boolean;
var
  Process: THandle;
begin
  Result := IsWindow(Wnd);
  if Result then
  begin
    GetWindowThreadProcessId(Wnd, @Process);
    Result := (Process = GetCurrentProcessID);
  end;
end;

//------------------------------------------------------------------------------


function MsgHookProc(Code: Integer; CurrentProcess: Cardinal; HookStruct: PCWPStruct): integer; stdcall;
var
  r: TRect;
  wc: TWinControl;
  wcp: TWinControl;
  frm: TWinControl;
  AllowShowFocus: boolean;
  i: integer;
  ph: THandle;
  pr,ir: TRect;
  winclass: array[0..255] of char;

begin
  case HookStruct.Message of
  WM_SETFOCUS:

     if IsProcessWindow(HookStruct.hwnd) then
     begin
       if Assigned(FocusHelper) then
       begin
         wc := FindControl(HookStruct.hwnd);
         wcp := FindControl(GetParent(HookStruct.hwnd));

         AllowShowFocus := FocusHelper.AcceptControl(wc,wcp);

         if FocusHelper.Active then
         begin
           if Assigned(FocusHelper.OnShowFocus) then
           begin
             FocusHelper.OnShowFocus(FocusHelper, wc, AllowShowFocus);
           end;
         end
         else
           AllowShowFocus := false;

         if (wc is TCustomForm) then
           AllowShowFocus := false;

         GetClassName(hookStruct.hwnd, winclass, sizeof(winclass));

         // Do not display focus indicator on dropdown of TDateTimePicker
         if StrComp(winclass,'SysMonthCal32') = 0 then
         begin
           AllowShowFocus := false;
         end;

         if AllowShowFocus then
         begin
           FocusHelper.FocusControl := HookStruct.hwnd;
           GetWindowRect(HookStruct.hwnd, r);

           if (wc is TCustomComboBox) or (wcp is TCustomComboBox) then
           begin
             if (wcp is TCustomComboBox) then
             begin
               GetWindowrect(wcp.Handle, r);
             end;
           end;

           ph := HookStruct.hwnd;
           repeat
             ph := GetParent(ph);

             if (ph <> 0) then
             begin
               GetWindowrect(ph, pr);
               if (pr.Right - pr.Left > 0) and (pr.Bottom - pr.Top > 0) then
               begin
                 IntersectRect(ir, pr, r);
                 r := ir;
               end;
             end;

             frm := Controls.FindControl(ph);
             if Assigned(frm) and (frm is TForm) then
               ph := 0;

           until (ph = 0);

           FocusHelper.FocusHeight := r.Bottom - r.Top;
           FocusHelper.FocusWidth := r.Right - r.Left;

           FocusHelper.ShowHelperOnControl(r);
         end;
       end;
     end;

  WM_CLOSE:
     begin
       FocusHelper.FocusControl := 0;
       FocusHelper.HideHelper;
     end;

  WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED:
     begin
       if IsChild(HookStruct.hwnd, FocusHelper.FocusControl) then
       begin
         GetWindowRect(FocusHelper.FocusControl, r);

         wc := FindControl(FocusHelper.FocusControl);
         wcp := FindControl(GetParent(FocusHelper.FocusControl));


         if Assigned(wcp) then
         begin
           if not wcp.Visible then
           begin
             FocusHelper.FocusControl := 0;
             FocusHelper.HideHelper;
           end;
         end;

         if (wc is TCustomComboBox) or (wcp is TCustomComboBox) then
         begin
           if (wcp is TCustomComboBox) then
           begin
             GetWindowrect(wcp.Handle, r);
           end;
         end;

         ph := FocusHelper.FocusControl;
         repeat
            ph := GetParent(ph);
            if (ph <> 0) then
            begin
              GetWindowrect(ph, pr);

              if (pr.Right - pr.Left > 0) and (pr.Bottom - pr.Top > 0) then
              begin
                IntersectRect(ir, pr, r);
                r := ir;
              end;
            end;
         until ph = 0;

         if (r.Bottom - r.Top = FocusHelper.FocusHeight) and
            (r.Right - r.Left = FocusHelper.FocusWidth) then
           FocusHelper.MoveHelper(r)
         else
         begin
           FocusHelper.FocusHeight := r.Bottom - r.Top;
           FocusHelper.FocusWidth := r.Right - r.Left;
           FocusHelper.ShowHelperOnControl(r);
         end;
       end;
     end;
  WM_KILLFOCUS:
     begin
       if FocusHelper.FocusControl <> 0 then
       begin
         FocusHelper.FocusControl := 0;
         FocusHelper.HideHelper;
       end;

       if IsProcessWindow(HookStruct.hwnd) then
       begin
         InvalidateRect(hookstruct.hwnd, nil, true);
       end;
     end;
  WM_SIZE:
     begin
       if HookStruct.wParam in [SIZE_MAXIMIZED, SIZE_RESTORED] then
       begin
         if (HookStruct.wParam = SIZE_MAXIMIZED) and not FocusHelper.IsMaximized then
         begin
           FocusHelper.IsMaximized := true;
           if FocusHelper.FocusControl <> 0 then
             FocusHelper.UpdateHelper;
         end;

         if (HookStruct.wParam = SIZE_RESTORED) and FocusHelper.IsMaximized then
         begin
           FocusHelper.IsMaximized := false;
           if FocusHelper.FocusControl <> 0 then
             FocusHelper.UpdateHelper;
         end;

       end;
     end;
  WM_ACTIVATE:
     begin
       wc := FindControl(HookStruct.hwnd);
       if Assigned(wc) then
       begin
         if (wc is TCustomForm) then
         begin
           if (hookstruct.wParam = WA_ACTIVE) or (hookstruct.wParam = WA_CLICKACTIVE) then
           begin
             for i := 0 to wc.ComponentCount - 1 do
             begin
               if (wc.Components[i] is TAdvFocusHelper) then
               begin
                 FocusHelper := (wc.Components[i] as TAdvFocusHelper);
               end;
             end;
           end
           else
           begin
             if FocusHelper.FocusControl <> 0 then
             begin
               FocusHelper.FocusControl := 0;
               FocusHelper.HideHelper;
             end;
           end;
         end;
       end;
     end;
  end;

  Result := CallNextHookEx(MsgHook, Code, CurrentProcess, LParam(HookStruct))
end;

//------------------------------------------------------------------------------

{ TAdvFocusHelper }

function TAdvFocusHelper.AcceptControl(wc,wcp: TWinControl): boolean;
begin
  Result := true;

  if (ExcludeControls <> []) then
  begin
    Result := true;
    if (ecButton in ExcludeControls) and (wc is TButton) then
      Result := false;
    if (ecCheckBox in ExcludeControls) and (wc is TCustomCheckBox) then
      Result := false;
    if (ecRadioButton in ExcludeControls) and (wc is TRadioButton) then
      Result := false;
    if (wc is TWinControl) then
      if (ecTagValue in ExcludeControls) and ((wc as TWinControl).Tag = ControlTag) then
        Result := false;
  end;

  if (IncludeControls <> []) then
  begin
    Result := false;

    if (icEdit in IncludeControls) and (wc is TCustomEdit) and not (wc is TCustomMemo) then
      Result := true;
    if (icMemo in IncludeControls) and (wc is TCustomMemo) then
      Result := true;
    if (icListBox in IncludeControls) and (wc is TCustomListBox) then
      Result := true;
    if (icComboBox in IncludeControls) and ((wcp is TCustomComboBox) or (wc is TCustomComboBox)) then
      Result := true;
    if (icDateTimePicker in IncludeControls) and (wc is TDateTimePicker) then
      Result := true;
    if (wc is TWinControl) then
      if (icTagValue in IncludeControls) and ((wc as TWinControl).Tag = ControlTag) then
        Result := true;
  end;
end;

//------------------------------------------------------------------------------

constructor TAdvFocusHelper.Create(AOwner: TComponent);
var
  Instances, I: integer;
begin
  inherited Create(AOwner);

  // global variable
  FHasHook := false;

  if not (Owner is TForm) then
    raise Exception.Create('Control parent must be a form!');

  Instances := 0;

  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is TAdvFocusHelper) then
      Inc(Instances);

  if (Instances > 1) then
    raise Exception.Create('The form already contains a TAdvFocusHelper component');

  FActive := true;
  FSize := 4;
  FBlur := 3;
  FColor :=  $FDD397;
  FRounding := 5;
  FOpacity := 230;
  FRounding := 5;
  FGlyph := TBitmap.Create;
  FGlyphPosition := gpAfterControl;
  FW := TFocusWindow.Create(self);

  if not (csDesigning in ComponentState) and (MsgHook = 0) then
  begin
    // assign the focus helper
    FocusHelper := self;
    // do the hooking here ....
    InitHook;
    FHasHook := true;
  end;
end;

//------------------------------------------------------------------------------

destructor TAdvFocusHelper.Destroy;
begin
  FGlyph.Free;
  FW.Free;

  if not (csDesigning in ComponentState) and FHasHook then
  begin
    Unhook;
    FocusHelper := nil;
    FHasHook := false;
  end;
  
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TAdvFocusHelper.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvFocusHelper.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.InitHook;
begin
  MsgHook := SetWindowsHookEx(WH_CALLWNDPROC, @MsgHookProc, 0, GetCurrentThreadID);
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.MoveHelper(R: TRect);
var
  BMPW: integer;
begin
  BMPW := 0;

  if (GlyphPosition = gpBeforeControl) and Assigned(Glyph) and not Glyph.Empty then
  begin
    BMPW := Glyph.Width + 2 * 2;
  end;

  MoveWindow(FocusHelper.FW.Handle,
    r.Left - FocusHelper.Size - BMPW,
    r.Top - FocusHelper.Size,
    r.Right - r.Left + 2 * FocusHelper.Size,
    r.Bottom - r.Top + 2 * FocusHelper.Size, true);
end;

//------------------------------------------------------------------------------


procedure TAdvFocusHelper.SetActive(const Value: boolean);
begin
  FActive := Value;

  if not (csDesigning in ComponentState) then
  begin
    if not FActive then
    begin
      if (Owner is TCustomForm) then
        SendMessage((Owner as TCustomForm).Handle, WM_KILLFOCUS, 0,0);
    end
    else
    begin
      if (Owner is TCustomForm) then
      begin
        SendMessage(Windows.GetFocus(), WM_SETFOCUS, 0 , 0);
      end;
    end;
  end;
end;

procedure TAdvFocusHelper.SetBlur(const Value: integer);
begin
  if (Value >= 0) and (Value < 10) then
    FBlur := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.SetSize(const Value: integer);
begin
  if (Value >= 0) and (Value < 15) then
    FSize := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.SetVersion(const Value: string);
begin
  // readonly
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.ShowHelper;
begin
  if Assigned(FocusHelper) then
    ShowWindow(FocusHelper.FW.Handle, SW_SHOWNOACTIVATE);
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.HideHelper;
begin
  if Assigned(FocusHelper) then
    ShowWindow(FocusHelper.FW.Handle, SW_HIDE);
end;

//------------------------------------------------------------------------------

procedure GlyphToMask(Glyph, Bitmap: TBitmap);
var
  x,y: integer;
  A: PRGB;
begin
  Bitmap.Width := Glyph.Width;
  Bitmap.Height := Glyph.Height;
  Bitmap.PixelFormat := pf24bit;

  Bitmap.Canvas.Brush.Color := clBlack;
  Bitmap.Canvas.Brush.Style := bsSolid;
  Bitmap.Canvas.FillRect(Rect(0,0,Bitmap.Width, Bitmap.Height));

  Glyph.TransparentMode := tmAuto;
  Glyph.Transparent := true;
  Bitmap.Canvas.Draw(0,0,Glyph);

  for y := 0 to Bitmap.Height  - 1 do
  begin
    A := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if (A^.Red <> 0) or (A^.Blue <> 0) or (A^.Green <> 0) then
      begin
        A^.Blue := $FF;
        A^.Green := $FF;
        A^.Blue := $FF;
      end;
      inc(A);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.ShowHelperOnControl(R: TRect);
var
  A,B,C: THandle;
  AlphaMask: TAlphaMask;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap;
  GlyphBitmap: TBitmap;
  W,H: integer;
  BMPW: integer;
  BMPO: integer;
begin
  W := r.Right - r.Left + (FocusHelper.Size * 2) + 1;
  H := r.Bottom - r.Top + (FocusHelper.Size * 2) + 1;

  BMPW := 0;
  BMPO := 0;

  if Assigned(FocusHelper.Glyph) and not FocusHelper.Glyph.Empty then
  begin
    BMPW := 2 * 2 + FocusHelper.Glyph.Width;
    W := W + BMPW;

    if FocusHelper.Glyph.Width < H then
      BMPO := (H - FocusHelper.Glyph.Width) div 2;
  end;

  FocusHelper.FW.Visible := false;
  FocusHelper.FW.Width := W;
  FocusHelper.FW.Height := H;

  AlphaMask := TAlphaMask.Create;
  try
    MaskBitmap := TBitmap.Create;
    MaskBitmap.PixelFormat := pf24bit;
    MaskBitmap.Width := W;
    MaskBitmap.Height := H;

    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := W;
    Bitmap.Height := H;

    try
      MaskBitmap.Canvas.Brush.Color := clBlack;
      MaskBitmap.Canvas.Brush.Style := bsSolid;
      MaskBitmap.Canvas.Pen.Color := clBlack;
      MaskBitmap.Canvas.Pen.Style := psSolid;
      MaskBitmap.Canvas.Rectangle(0,0,W,H);

      DrawRoundRect(MaskBitmap, W, H, BMPW, FocusHelper.GlyphPosition, FocusHelper.Size, FocusHelper.Rounding, FocusHelper.Blur);

      if Assigned(FocusHelper.Glyph) and not FocusHelper.Glyph.Empty then
      begin
        GlyphBitmap := TBitmap.Create;
        try
          GlyphToMask(FocusHelper.Glyph, GlyphBitmap);
          if FocusHelper.GlyphPosition = gpAfterControl then
            MaskBitmap.Canvas.Draw(W - BMPW + 2, BMPO, GlyphBitmap)
          else
            MaskBitmap.Canvas.Draw(2, BMPO, GlyphBitmap);
        finally
          GlyphBitmap.Free;
        end;
      end;

      Bitmap.Canvas.Pen.Color := FocusHelper.Color;
      Bitmap.Canvas.Pen.Width := 1;
      Bitmap.Canvas.Brush.Color := FocusHelper.Color;
      Bitmap.Canvas.Brush.Style := bsSolid;
      Bitmap.Canvas.Rectangle(0,0,bitmap.width,bitmap.height);

      if Assigned(FocusHelper.Glyph) and not FocusHelper.Glyph.Empty then
      begin
        FocusHelper.Glyph.TransparentMode := tmAuto;
        FocusHelper.Glyph.Transparent := true;
        if FocusHelper.GlyphPosition = gpAfterControl then
          Bitmap.Canvas.Draw(W - BMPW + 2, BMPO, FocusHelper.Glyph)
        else
          Bitmap.Canvas.Draw(2, BMPO, FocusHelper.Glyph)
      end;

      AlphaMask.MergeMask(MaskBitmap, Bitmap, clYellow);
    finally
      Bitmap.Free;
      MaskBitmap.Free;
    end;

    AlphaMask.Opacity := FocusHelper.Opacity;
    UpdateAlphaWindow(FocusHelper.FW.Handle, AlphaMask);
  finally
    AlphaMask.Free;
  end;

  if not ( (GlyphPosition = gpBeforeControl) and Assigned(Glyph) and not Glyph.Empty) then
  begin
    BMPW := 0;
  end;

  A := CreateRectRgn(0, 0, W, H);
  B := CreateRectRgn(0, 0, R.Right - R.Left, R.Bottom - R.Top);

  OffsetRgn(B, FocusHelper.Size + BMPW, FocusHelper.Size);
  C := CreateRectRgn(0, 0, 1, 1);
  CombineRgn(C, A, B, RGN_XOR);
  SetWindowRgn(FocusHelper.FW.Handle, C, False);
  DeleteObject(A);
  DeleteObject(B);

  MoveWindow(FocusHelper.FW.Handle, r.Left - FocusHelper.Size - BMPW, r.Top - FocusHelper.Size, W, H, true);
  ShowWindow(FocusHelper.FW.Handle, SW_SHOWNOACTIVATE);
end;

//------------------------------------------------------------------------------

procedure TAdvFocusHelper.Unhook;
begin
  if MsgHook <> 0 then
     UnhookWindowsHookEx(MsgHook);
  MsgHook := 0;
end;

procedure TAdvFocusHelper.UpdateHelper;
begin
  SendMessage(Windows.GetFocus(), WM_SETFOCUS, 0, 0);
end;

//------------------------------------------------------------------------------

{ TFocusWindow }

constructor TFocusWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 0;
  Height := 0;
  Color := clRed;
end;

//------------------------------------------------------------------------------

procedure TFocusWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := WS_POPUP or WS_DISABLED;
  Params.ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW; /// or WS_EX_TRANSPARENT;
end;

//------------------------------------------------------------------------------

procedure TFocusWindow.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TFocusWindow.Paint;
begin
  inherited;
end;

//------------------------------------------------------------------------------

{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}

initialization
{$IFDEF FREEWARE}
  if (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) or
     (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
  begin
    MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
  end;
{$ENDIF}

  MsgHook := 0;
  FocusHelper := nil;
end.
