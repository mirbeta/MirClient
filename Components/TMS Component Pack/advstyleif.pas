{***************************************************************************}
{ TAdvStyleIF interface                                                     }
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

unit AdvStyleIF;

interface

{$I TMSDEFS.INC}

uses
  Classes, Registry, SysUtils, Windows, Messages, Controls, Forms, Graphics;

const
  //TMSMETROFONT = 'Segoe UI';

  WM_OFFICETHEMECHANGED  = WM_USER + 1969;

  WM_THEMECHANGED = $031A;
  {$IFDEF DELPHI2006_LVL}
  {$EXTERNALSYM WM_THEMECHANGED}
  {$ENDIF}

//Need to redeclare the API function - instead of BOOL is uses DWORD.
function RegNotifyChangeKeyValue(hKey: HKEY; bWatchSubtree: DWORD; dwNotifyFilter: DWORD; hEvent: THandle; fAsynchronus: DWORD): Longint; stdcall;
  external 'advapi32.dll' name 'RegNotifyChangeKeyValue';

type
  TMetroStyle = (msLight, msDark);

  TTMSStyle = (tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsOffice2003Classic,
    tsOffice2007Luna, tsOffice2007Obsidian, tsWindowsXP, tsWhidbey, tsCustom, tsOffice2007Silver, tsWindowsVista,
    tsWindows7, tsTerminal, tsOffice2010Blue, tsOffice2010Silver, tsOffice2010Black, tsWindows8,
    tsOffice2013White, tsOffice2013LightGray, tsOffice2013Gray,
    tsWindows10,
    tsOffice2016White, tsOffice2016Gray, tsOffice2016Black);

  TColorTone = record
    BrushColor: TColor;
    BorderColor: TColor;
    TextColor: TColor;
  end;

  TColorTones = record
    Background: TColorTone;
    Foreground: TColorTone;
    Selected: TColorTone; // =Down
    Hover: TColorTone;
    Disabled: TColorTone;
  end;

  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

  TOfficeVersion = (ov2003, ov2007, ov2010, ov2013);

  TOfficeTheme = (ot2003Blue,ot2003Silver,ot2003Olive,ot2003Classic,ot2007Blue,ot2007Silver,ot2007Black,ot2010Blue,ot2010Silver,ot2010Black,ot2013White,ot2013Silver,ot2013Gray,ot2016White,ot2016Gray,ot2016Black,otUnknown);

  ITMSTones = interface
  ['{1F492643-6699-4F25-8B34-3233FA735036}']
     procedure SetColorTones(ATones: TColorTones);
  end;

  ITMSStyle = interface
  ['{11AC2DDC-C087-4298-AB6E-EA1B5017511B}']
    procedure SetComponentStyle(AStyle: TTMSStyle);
  end;

  ITMSStyleEx = interface(ITMSStyle)
  ['{037BA87F-7CBD-4FDD-854E-2B3F0BCC06AE}']
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
  end;

  ITMSMetro = interface
  ['{A7E8D091-0327-446D-83D6-7069760B3320}']
    function IsMetro: boolean;
  end;

  THandleList = class(TList)
  private
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer):Integer;
  public
    constructor Create;
    procedure DeleteValue(Value: Integer);
    function HasValue(Value: Integer): Boolean;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: Integer);
    procedure Insert(Index,Value: Integer);
    procedure Delete(Index: Integer);
  end;

  TRegMonitorThread = class(TThread)
  private
    FReg: TRegistry;
    FEvent: THandle;
    FKey: string;
    FRootKey: HKey;
    FWatchSub: boolean;
    FFilter: integer;
    FWnd: THandle;
    FWinList: THandleList;
    procedure InitThread;
    procedure SetFilter(const Value: integer);
    function GetFilter: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop;
    property Key: string read FKey write FKey;
    property RootKey: HKey read FRootKey write FRootKey;
    property WatchSub: boolean read FWatchSub write FWatchSub;
    property Filter: integer read GetFilter write SetFilter;
    property Wnd: THandle read FWnd write FWnd;
    property WinList: THandleList read FWinList;
  protected
    procedure Execute; override;
  end;

  TThemeNotifierWindow = class(TWinControl)
  private
    FOnOfficeThemeChange: TNotifyEvent;
  protected
    procedure WndProc(var Msg: TMessage); override;
  published
    property OnOfficeThemeChange: TNotifyEvent read FOnOfficeThemeChange write FOnOfficeThemeChange;
  end;

  TThemeNotifier = class(TComponent)
  private
    RegMonitorThread : TRegMonitorThread;
    FNotifier: TThemeNotifierWindow;
    FOnOfficeThemeChange: TNotifyEvent;
  protected
    procedure OfficeThemeChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterWindow(Hwnd: THandle);
    procedure UnRegisterWindow(Hwnd: THandle);
  published
    property OnOfficeThemeChange: TNotifyEvent read FOnOfficeThemeChange write FOnOfficeThemeChange;
  end;

function ThemeNotifier(AParent: TWinControl): TThemeNotifier;
function IsVista: boolean;
function IsWinXP: Boolean;
function GetOfficeVersion: TOfficeVersion;
function GetOfficeTheme: TOfficeTheme;
function IsThemedApp: boolean;

function GetMetroFont: string;
function DefaultMetroTones: TColorTones;
function CreateMetroTones(Light: boolean; Color, TextColor: TColor): TColorTones;
function ClearTones: TColorTones;
function IsClearTones(ATones: TColorTones): boolean;

function ChangeBrightness(Color: TColor; Perc: Integer): TColor;
function ChangeColor(Color: TColor; Perc: integer): TColor;


var
  ThemeNotifierInstance: TThemeNotifier;
  TMSDISABLEWITHCOLORSATURATION: boolean = true;

implementation

uses
  Dialogs, Math;

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;

//------------------------------------------------------------------------------

{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function IsThemedApp: Boolean;
var
  i: Integer;
begin
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

function GetOfficeVersion: TOfficeVersion;
var
  reg: TRegistry;
begin
  Result := ov2003;

  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;

  if reg.KeyExists('Software\Microsoft\Office\11.0\Common\General') then
    Result := ov2003;

  if reg.KeyExists('Software\Microsoft\Office\12.0\Common\General') then
    Result := ov2007;

  if reg.KeyExists('Software\Microsoft\Office\14.0\Common\General') then
    Result := ov2010;

  if reg.KeyExists('Software\Microsoft\Office\15.0\Common\General') then
    Result := ov2013;

  reg.Free;
end;

//------------------------------------------------------------------------------

function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

//------------------------------------------------------------------------------
function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;

  Result := xpNone;

  if not IsWinXP then
    Exit;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib,'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive and IsThemedApp then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib,'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);

            GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255);

            {$IFDEF DELPHI_UNICODE}
            if StrPos('NormalColor',PWideChar(ColorScheme)) <> nil then
              Result := xpBlue
            else if StrPos('HomeStead',PWideChar(ColorScheme)) <> nil then
              Result := xpGreen
            else if StrPos('Metallic',PWideChar(ColorScheme)) <> nil then
              Result := xpGray
            else
              Result := xpNone;

            {$ENDIF}
            {$IFNDEF DELPHI_UNICODE}
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
            {$ENDIF}
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;

//------------------------------------------------------------------------------

function GetOfficeThemeValue(reg: TRegistry; key: string): integer;
begin
  Result := -1;

  if reg.KeyExists(key) then
  begin
    reg.OpenKey(key, false);
    if reg.ValueExists('Theme') then
      Result := reg.ReadInteger('Theme');
    reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------

function GetOfficeUIThemeValue(reg: TRegistry; key: string): integer;
begin
  Result := -1;

  if reg.KeyExists(key) then
  begin
    reg.OpenKey(key, false);
    if reg.ValueExists('UI Theme') then
      Result := reg.ReadInteger('UI Theme');
    reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------

function GetOfficeTheme: TOfficeTheme;
var
  reg: TRegistry;
  i: integer;

begin
  Result := ot2003Blue;

  reg := TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;

  // Office 2016?
  i := GetOfficeUIThemeValue(reg,'Software\Microsoft\Office\16.0\Common');
  if (i <> -1) then
  begin
    case i of
    0,1: Result := ot2016White;
    2: Result := ot2016Gray;
    3: Result := ot2016Black;
    end;
  end
  else
  begin
    // Office 2013?
    i := GetOfficeUIThemeValue(reg,'Software\Microsoft\Office\15.0\Common');

    if (i <> -1) then
    begin
      case i of
      0: Result := ot2013White;
      1: Result := ot2013Silver;
      2: Result := ot2013Gray;
      end;
    end
    else
    begin
      // Office 2010?
      i := GetOfficeThemeValue(reg,'Software\Microsoft\Office\14.0\Common');

      if (i <> -1) then
      begin
        case i of
        1: Result := ot2010Blue;
        2: Result := ot2010Silver;
        3: Result := ot2010Black;
        end;
      end
      else
      begin
        // Office 2007?
        i := GetOfficeThemeValue(reg,'Software\Microsoft\Office\12.0\Common');

        if (i <> -1) then
        begin
          case i of
          1: Result := ot2007Blue;
          2: Result := ot2007Silver;
          3: Result := ot2007Black;
          end;
        end
        else
        begin
          // Office 2003?
          case CurrentXPTheme of
          xpNone: Result := ot2003Classic;
          xpBlue: Result := ot2003Blue;
          xpGreen: Result := ot2003Olive;
          xpGray: Result := ot2003Silver;
          end;
        end;
      end;
    end;
  end;

  reg.Free;
end;

//------------------------------------------------------------------------------


function ThemeNotifier(AParent: TWinControl): TThemeNotifier;
begin
  if not Assigned(ThemeNotifierInstance) then
  begin
    if Assigned(Application) and Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
    begin
      ThemeNotifierInstance := TThemeNotifier.Create(Application.MainForm)
    end
    else
    begin
      ThemeNotifierInstance := TThemeNotifier.Create(AParent);
    end;
  end;
  Result := ThemeNotifierInstance;
end;

//------------------------------------------------------------------------------

{ TRegMonitorThread }
constructor TRegMonitorThread.Create;
begin
  // Execute won’t be called until after Resume is called.
  inherited Create(True);
  FReg := TRegistry.Create;
  FWinList := THandleList.Create;
end;

destructor TRegMonitorThread.Destroy;
begin
  if Assigned(FWinList) then
    FreeAndNil(FWinList);
  if Assigned(FReg) then
    FreeAndNil(FReg);
  if FEvent <> 0 then
    CloseHandle(FEvent);

  inherited Destroy;
end;

procedure TRegMonitorThread.InitThread;
begin
  FReg.RootKey := RootKey;
  if not FReg.OpenKeyReadOnly(Key) then
  begin
    raise Exception.Create('Unable to open registry key ' + Key);
  end;
  FEvent := CreateEvent(nil, True, False, 'RegMonitorChange');
  RegNotifyChangeKeyValue(FReg.CurrentKey, 1, Filter, FEvent, 1);
end;

procedure TRegMonitorThread.Execute;
var
  i: integer;
begin
  InitThread;

  while not Terminated do
  begin
    if WaitForSingleObject(FEvent, INFINITE) = WAIT_OBJECT_0 then
    begin
      if Terminated then
        Exit;

      // Notify notifier Window
      if Wnd <> 0 then
        SendMessage(Wnd, WM_OFFICETHEMECHANGED, RootKey, LParam(PChar(Key)));

      for i := 0 to WinList.Count - 1 do
        SendMessage(WinList.Items[i], WM_OFFICETHEMECHANGED, RootKey, LParam(PChar(Key)));

      ResetEvent(FEvent);
      RegNotifyChangeKeyValue(FReg.CurrentKey, 1, Filter, FEvent, 1);
    end;
  end;
end;

procedure TRegMonitorThread.SetFilter(const Value: integer);
begin
  if fFilter <> Value then
    fFilter := Value;
end;

procedure TRegMonitorThread.Stop;
begin
  Terminate;
  Windows.SetEvent(FEvent);
end;

function TRegMonitorThread.GetFilter: integer;
begin
  if fFilter = 0 then
  begin
    fFilter := REG_NOTIFY_CHANGE_NAME or
      REG_NOTIFY_CHANGE_ATTRIBUTES or
      REG_NOTIFY_CHANGE_LAST_SET or
      REG_NOTIFY_CHANGE_SECURITY;
  end;
  Result := fFilter;
end;

//------------------------------------------------------------------------------

{ TNotifierWindow }

procedure TThemeNotifierWindow.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_OFFICETHEMECHANGED) then
  begin
    if Assigned(FOnOfficeThemeChange) then
    begin
      FOnOfficeThemeChange(Self);
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

{ TThemeNotifier }

constructor TThemeNotifier.Create(AOwner: TComponent);
var
  ov: TOfficeVersion;
begin
  inherited;
  RegMonitorThread := nil;

  if not (csDesigning in ComponentState) then
  begin
    ov := GetOfficeVersion;

    if (ov in [ov2007, ov2010, ov2013]) then
    begin
      FNotifier := TThemeNotifierWindow.Create(Self);
      FNotifier.Parent := (AOwner as TWinControl);
      FNotifier.Visible := false;
      FNotifier.OnOfficeThemeChange := OfficeThemeChanged;

      RegMonitorThread := TRegMonitorThread.Create;

      with RegMonitorThread do
      begin
        FreeOnTerminate := True;
        Wnd := FNotifier.Handle;
        Filter := REG_NOTIFY_CHANGE_LAST_SET;

        case ov of
          ov2007: Key := 'Software\Microsoft\Office\12.0\Common';
          ov2010: Key := 'Software\Microsoft\Office\14.0\Common';
          ov2013: Key := 'Software\Microsoft\Office\15.0\Common';
        end;

        RootKey := HKEY_CURRENT_USER;
        WatchSub := True;
        {$IFDEF DELPHI2010_LVL}
        Start;
        {$ENDIF}
        {$IFNDEF DELPHI2010_LVL}
        Resume;
        {$ENDIF}
      end;
    end;
  end;
end;

destructor TThemeNotifier.Destroy;
begin
  if Assigned(RegMonitorThread) then
    RegMonitorThread.Stop;
  ThemeNotifierInstance := nil;
  inherited;
end;

procedure TThemeNotifier.OfficeThemeChanged(Sender: TObject);
begin
  if Assigned(OnOfficeThemeChange) then
    OnOfficeThemeChange(Self);
end;

procedure TThemeNotifier.RegisterWindow(Hwnd: THandle);
begin
  if Assigned(RegMonitorThread) and not RegMonitorThread.WinList.HasValue(Hwnd) then
    RegMonitorThread.WinList.Add(Hwnd);
end;

procedure TThemeNotifier.UnRegisterWindow(Hwnd: THandle);
begin
  if Assigned(RegMonitorThread) then
  begin
    RegMonitorThread.WinList.DeleteValue(Hwnd);
  end;
end;

//------------------------------------------------------------------------------

{ THandleList }

constructor THandleList.Create;
begin
  inherited Create;
end;

procedure THandleList.SetInteger(Index:Integer;Value:Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

function THandleList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure THandleList.DeleteValue(Value: Integer);
var
  i: integer;
begin
  i := IndexOf(Pointer(Value));

  if i <> -1 then
    Delete(i);
end;

function THandleList.HasValue(Value: Integer): Boolean;
begin
  Result := IndexOf(Pointer(Value)) <> -1;
end;

procedure THandleList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));
end;

procedure THandleList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure THandleList.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Pointer(Value));
end;

function DarkenColor(Color: TColor; Perc: integer): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  r := ((l AND $FF0000) shr 16) and $FF;
  g := ((l AND $FF00) shr 8) and $FF;
  b := (l AND $FF);

  r := Min(255,Round(r * (100 - Perc)/100));
  g := Min(255,Round(g * (100 - Perc)/100));
  b := Min(255,Round(b * (100 - Perc)/100));

  Result := (r shl 16) or (g shl 8) or b;
end;

function ChangeColor(Color: TColor; Perc: integer): TColor;
begin
  Result := DarkenColor(Color, Perc);
end;

function CreateMetroTones(Light: boolean; Color, TextColor: TColor): TColorTones;
begin
  if Light then
  begin
    Result.Background.BrushColor := clWhite;
    Result.Background.TextColor := clBlack;
    Result.Background.BorderColor := Color;
  end
  else
  begin
    Result.Background.BrushColor := clBlack;
    Result.Background.TextColor := clWhite;
    Result.Background.BorderColor := Color;
  end;

  Result.Selected.BrushColor := Color;
  Result.Selected.TextColor := clWhite;
  Result.Selected.BorderColor := Color;

  Result.Disabled.BrushColor := clSilver;
  Result.Disabled.TextColor := clBlack;
  Result.Disabled.BorderColor := clGray;

  Result.Foreground.BrushColor := clSilver;
  Result.Foreground.TextColor := clBlack;
  Result.Foreground.BorderColor := clSilver;

  Result.Hover.BrushColor := DarkenColor(Color,20);
  Result.Hover.TextColor := clWhite;
  Result.Hover.BorderColor := Result.Hover.BrushColor;
end;

function DefaultMetroTones: TColorTones;
begin
  Result.Background.BrushColor := clWindow;
  Result.Background.TextColor := clWindowText;
  Result.Background.BorderColor := clHighLight;

  Result.Selected.BrushColor := clHighlight;
  Result.Selected.TextColor := clHighlightText;
  Result.Selected.BorderColor := clHighLight;

  Result.Foreground.BrushColor := clSilver;
  Result.Foreground.TextColor := clBlack;
  Result.Foreground.BorderColor := clSilver;

  Result.Hover.BrushColor := clGray;
  Result.Hover.TextColor := clWhite;
  Result.Hover.BorderColor := clGray;

  Result.Disabled.BrushColor := clWindow;
  Result.Disabled.TextColor := clSilver;
  Result.Disabled.BorderColor := clSilver;
end;

function ClearTones: TColorTones;
begin
  Result.Background.BrushColor := clNone;
  Result.Background.TextColor := clNone;
  Result.Background.BorderColor := clNone;

  Result.Selected.BrushColor := clNone;
  Result.Selected.TextColor := clNone;
  Result.Selected.BorderColor := clNone;

  Result.Foreground.BrushColor := clNone;
  Result.Foreground.TextColor := clNone;
  Result.Foreground.BorderColor := clNone;

  Result.Hover.BrushColor := clNone;
  Result.Hover.TextColor := clNone;
  Result.Hover.BorderColor := clNone;

  Result.Disabled.BrushColor := clNone;
  Result.Disabled.TextColor := clNone;
  Result.Disabled.BorderColor := clNone;
end;

function IsClearTones(ATones: TColorTones): boolean;
begin
  Result := (ATones.Background.BrushColor = clNone) and
            (ATones.Background.TextColor = clNone) and
            (ATones.Background.BorderColor = clNone);
end;

function GetMetroFont: string;
begin
  if IsVista then
    Result := 'Segoe UI'
  else
    Result := 'Tahoma';
end;

procedure RGBToHSV(const R,G,B: double; var H,S,V: double);
var
  Delta: double;
  Min : double;
begin
  Min := MinValue( [R, G, B] );
  V := MaxValue( [R, G, B] );

  Delta := V - Min;

  if V = 0.0 then
    S := 0
  else
    S := Delta / V;

  if S = 0.0 then
    H := NaN
  else
  begin
    if R = V then
      H := 60.0 * (G - B) / Delta
    else
      if G = V then
        H := 120.0 + 60.0 * (B - R) / Delta
      else
        if B = V then
          H := 240.0 + 60.0 * (R - G) / Delta;
    if H < 0.0 then
      H := H + 360.0
  end;
end;

procedure HSVtoRGB(const H,S,V: double; var R,G,B: double);
var
  f : double;
  i : INTEGER;
  hTemp,p,q,t: double;
begin
  if  S = 0.0 then
  begin
    if IsNaN(H) then
    begin
      R := V;
      G := V;
      B := V
    end
    else
      raise Exception.Create('HSVtoRGB: S = 0 and H has a value');
  end
  else
  begin
    if  H = 360.0 then
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60;
    i := Trunc(hTemp);
    f := hTemp - i;

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    case i of
      0: begin R := V; G := t;  B := p  end;
      1: begin R := q; G := V; B := p  end;
      2: begin R := p; G := V; B := t   end;
      3: begin R := p; G := q; B := V  end;
      4: begin R := t;  G := p; B := V  end;
      5: begin R := V; G := p; B := q  end;
    end;
  end;
end;

function ChangeBrightness(Color: TColor; Perc: Integer): TColor;
var
  r,g,b: double;
  rb,gb,bb: integer;
  l: longint;
  h,s,v: double;
begin
  l := ColorToRGB(Color);

  r := (l AND $FF0000) shr 16;
  g := (l AND $00FF00) shr 8;
  b := (l AND $0000FF);

  RGBToHSV(R,G,B,H,S,V);

  if V < 255 then
    V :=  Max(255, V + (V * Perc/100))
  else
    S :=  S + (S * Perc/100);

  HSVToRGB(H,S,V,R,G,B);

  rb := round(r);
  gb := round(g);
  bb := round(b);

  rb := ((rb and $FF) shl 16) and $FF0000;
  gb := ((gb and $FF) shl 8) and $FF00;
  bb := (bb and $FF);

  Result := rb or gb or bb;
end;

function BrightnessColor(Col: TColor; Brightness: integer): TColor;
var
  r1,g1,b1: Integer;
begin
  Col := ColorToRGB(Col);
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,Brightness)
  else
    r1 := Round( Min(100,(100 + Brightness))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,Brightness)
  else
    g1 := Round( Min(100,(100 + Brightness))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,Brightness)
  else
    b1 := Round( Min(100,(100 + Brightness))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;



initialization
  ThemeNotifierInstance  := nil;

finalization

end.

