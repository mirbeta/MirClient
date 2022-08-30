{***************************************************************************}
{ TAdvMenuStylers component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005 - 2015                                        }
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

unit AdvMenuStylers;

{$I TMSDEFS.INC}

{$R AdvMenuStylers.res}

interface

uses
  Classes, AdvMenus, Dialogs, Forms, Controls, Messages, Windows, AdvStyleIF;

type
  TOfficeStyle = (osOffice2003Blue, osOffice2003Silver, osOffice2003Olive, osOfficeXP, osOffice2007Luna, osOffice2007Obsidian, osCustom, osOffice2007Silver, osVista, osWhidbey, osWindows7, osTerminal,
    osOffice2010Blue, osOffice2010Silver, osOffice2010Black,
    osWindows8, osOffice2013White, osOffice2013LightGray, osOffice2013Gray,
    osWindows10, osOffice2016White, osOffice2016Gray, osOffice2016Black);

  TNotifierWindow = class(TWinControl)
  private
    FOnThemeChange: TNotifyEvent;
  protected
    procedure WndProc(var Msg: TMessage); override;
  published
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMenuOfficeStyler = class(TCustomAdvMenuStyler, ITMSStyle)
  private
    FNotifierWnd: TNotifierWindow;
    FStyle: TOfficeStyle;
    FAutoThemeAdapt: Boolean;
    procedure SetStyle(const Value: TOfficeStyle);
  protected
    procedure ThemeChanged(Sender: TObject);
    procedure SetAutoThemeAdapt(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
  published
    property AntiAlias;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write SetAutoThemeAdapt;
    property Style: TOfficeStyle read FStyle write SetStyle;
    property Background;
    property ButtonAppearance;
    property IconBar;
    property SelectedItem;
    property RootItem;
    property Glyphs;
    property SideBar;
    property Separator;
    property Font;
    property NotesFont;
    property UseSystemFont;
    property MenuBorderColor;
  end;

  TFantasyStyle = (fsArctic, fsAquaBlue, fsChocolate, fsMacOS, fsSilverFox,
    fsSoftSand, fsTerminalGreen, fsTextured, fsWindowsClassic, fsWhidbey, fsCustom);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMenuFantasyStyler = class(TCustomAdvMenuStyler)
  private
    FStyle: TFantasyStyle;
    procedure SetStyle(const Value: TFantasyStyle);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AntiAlias;
    property Style: TFantasyStyle read FStyle write SetStyle;
    property Background;
    property ButtonAppearance;
    property IconBar;
    property SelectedItem;
    property RootItem;
    property Glyphs;
    property SideBar;
    property Separator;
    property Font;
    property NotesFont;    
    property UseSystemFont;
    property MenuBorderColor;
  end;

implementation

uses
  ComObj, Graphics;

const
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;


function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

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
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib,'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255);
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;


{ TAdvMenuOfficeStyler }

constructor TAdvMenuOfficeStyler.Create(AOwner: TComponent);
var
  ctrl: TComponent;
begin
  inherited Create(AOwner);
  Style := osOffice2003Blue;
  FNotifierWnd := TNotifierWindow.Create(Self);

  // find first owning TWinControl owner
  ctrl := AOwner;
  while Assigned(ctrl) and not (ctrl is TWinControl) do
  begin
    ctrl := ctrl.Owner;
  end;

  if Assigned(ctrl) then
    if (ctrl is TWinControl) then
      FNotifierWnd.Parent := TWinControl(ctrl);

  FNotifierWnd.OnThemeChange := ThemeChanged;
end;

destructor TAdvMenuOfficeStyler.Destroy;
begin
  // becomes owned by owner and owner is responsible to free notifier window
  //  FNotifierWnd.Free;
  inherited;
end;

procedure TAdvMenuOfficeStyler.Loaded;
begin
  inherited;

  ThemeChanged(Self);
end;

procedure TAdvMenuOfficeStyler.ThemeChanged(Sender: TObject);
var
  eTheme: XPColorScheme;
begin
  if not AutoThemeAdapt then
    Exit;

  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: Style := osOffice2003Blue;
    xpGreen: Style := osOffice2003Olive;
    xpGray: Style := osOffice2003Silver;
  else
    Style := osOfficeXP;
  end;
end;

procedure TAdvMenuOfficeStyler.SetAutoThemeAdapt(const Value: boolean);
begin
  if (FAutoThemeAdapt <> Value) then
  begin
    FAutoThemeAdapt := Value;
    ThemeChanged(Self);
  end;
end;

procedure TAdvMenuOfficeStyler.SetComponentStyle(AStyle: TTMSStyle);
begin
  case AStyle of
    tsOffice2003Blue: Style := osOffice2003Blue;
    tsOffice2003Silver: Style := osOffice2003Silver;
    tsOffice2003Olive: Style := osOffice2003Olive;
    tsOffice2003Classic: Style := osOfficeXP;
    tsOffice2007Luna: Style := osOffice2007Luna;
    tsOffice2007Silver: Style := osOffice2007Silver;
    tsOffice2007Obsidian: Style := osOffice2007Obsidian;
    tsWindowsXP: Style := osOfficeXP;
    tsWhidbey: Style := osWhidbey;
    tsWindowsVista: Style := osVista;
    tsWindows7: Style := osWindows7;
    tsWindows8: Style := osWindows8;
    tsWindows10: Style := osWindows10;
    tsTerminal: Style := osTerminal;
    tsOffice2010Blue: Style := osOffice2010Blue;
    tsOffice2010Silver: Style := osOffice2010Silver;
    tsOffice2010Black: Style := osOffice2010Black;
    tsOffice2013White: Style := osOffice2013White;
    tsOffice2013LightGray: Style := osOffice2013LightGray;
    tsOffice2013Gray: Style := osOffice2013Gray;
    tsOffice2016White: Style := osOffice2016White;
    tsOffice2016Gray: Style := osOffice2016Gray;
    tsOffice2016Black: Style := osOffice2016Black;
  end;
end;

procedure TAdvMenuOfficeStyler.SetStyle(const Value: TOfficeStyle);
begin
  FStyle := Value;
  Font.Color := clWindowText;
  UseSystemFont := true;

  case FStyle of
  osOffice2003Blue:
    begin
      Background.Color := $F6F6F6;
      Background.ColorTo := $F6F6F6;
      MenuBorderColor := $962D00;

      RootItem.Color := $F5BE9E;       // #9EBEF5
      RootItem.ColorTo := $F7CDB2;     // #B2CDF7
      RootItem.SelectedColor := $FFEFE3; // #E3EFFF
      RootItem.SelectedColorTo := $E7B593;  // #93B5E7
      RootItem.SelectedBorderColor := $962D00;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;

      RootItem.HoverColor := $CCF4FF;  // #FFF4CC
      RootItem.HoverColorTo := $9AD6FF;  // #FFD69A
      RootItem.HoverBorderColor := $006F4B4B;
      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdHorizontal;
      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $C2EEFF;  // #FFEEC2
      SelectedItem.ColorTo := clNone;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $800000;

      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $800000;
      SelectedItem.RadioBorder := $800000;

      IconBar.Color := $FFEFE3;     // #E3EFFF
      IconBar.ColorTo := $E4AD87;   // #87ADE4
      //IconBar.Size := 24;
      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $800000;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $800000;

      Separator.Color := $CB8C6A;
      Separator.ColorTo := clNone;

      with ButtonAppearance do
      begin
        DownColor := $4E91FE;
        DownColorTo := $91D3FF;
        HoverColor := $CCF4FF;
        HoverColorTo := $91D0FF;
        DownBorderColor := $800000;
        HoverBorderColor := $800000;
      end;

    end;
  osOffice2003Olive:
    begin
      Background.Color := $F6F6F6;
      Background.ColorTo := $F6F6F6;
      MenuBorderColor := $5E8D75;

      RootItem.Color := $A8D9D9;      // #D9D9A8
      RootItem.ColorTo := $E4F0F2;    // #F2F0E4

      RootItem.SelectedColor := $D5F0EC;    // #ECF0D5
      RootItem.SelectedColorTo := $9FCEC2;  // #C2CE9F
      RootItem.SelectedBorderColor := $5E8D75;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;

      RootItem.HoverColor := $CCF4FF;  // #FFF4CC
      RootItem.HoverColorTo := $9AD6FF;  // #FFD69A
      RootItem.HoverBorderColor := $006F4B4B;
      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdHorizontal;
      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $C2EEFF;
      SelectedItem.ColorTo := clNone;
      SelectedItem.BorderColor := $385D3F;
      SelectedItem.CheckBorder := $385D3F;
      SelectedItem.RadioBorder := $385D3F;

      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;

      IconBar.Color := $EDFFFF;  // #FFFFED
      IconBar.ColorTo := $92C7B8; // #B8C792
      //IconBar.Size := 24;
      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $385D3F;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $385D3F;

      Separator.Color := $588060;
      Separator.ColorTo := clNone;
      with ButtonAppearance do
      begin
        DownColor := $4E91FE;
        DownColorTo := $91D3FF;
        HoverColor := $CCF4FF;
        HoverColorTo := $91D0FF;
        DownBorderColor := $800000;
        HoverBorderColor := $800000;
      end;

    end;
  osOffice2003Silver:
    begin
      Background.Color := $F6F6F6;
      Background.ColorTo := $F6F6F6;
      MenuBorderColor := $5E8D75;

      RootItem.Color := $E5D7D7;  // #D7D7E5
      RootItem.ColorTo := $F7F3F3;  // #F3F3F7

      RootItem.SelectedColor := $F1E9E8;   // #E8E9F1
      RootItem.SelectedColorTo := $CDB9BA; // #BAB9CD
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;

      RootItem.SelectedBorderColor := $947C7C;

      RootItem.HoverColor := $CCF4FF;  // #FFF4CC
      RootItem.HoverColorTo := $9AD6FF;  // #FFD69A
      RootItem.HoverBorderColor := $006F4B4B;
      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdHorizontal;
      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $C2EEFF;
      SelectedItem.ColorTo := clNone;
      SelectedItem.BorderColor := $80000;
      SelectedItem.CheckBorder := $6F4B4B;
      SelectedItem.RadioBorder := $6F4B4B;

      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;

      IconBar.Color := $FFF9F9;   // #F9F9FF
      IconBar.ColorTo := $B99D9F;  // #9F9DB9
      //IconBar.Size := 24;
      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $006F4B4B;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $006F4B4B;

      Separator.Color := $8F6D6E;
      Separator.ColorTo := clNone;
      
      with ButtonAppearance do
      begin
        DownColor := $4E91FE;
        DownColorTo := $91D3FF;
        HoverColor := $CCF4FF;
        HoverColorTo := $91D0FF;
        DownBorderColor := $800000;
        HoverBorderColor := $800000;
      end;

    end;
  osOfficeXP:
    begin
      Background.Color := clWindow;
      Background.ColorTo := clWindow;
      MenuBorderColor := clGray;

      RootItem.Color := clBtnFace;
      RootItem.ColorTo := clBtnFace;

      RootItem.SelectedColor := clBtnFace;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := $7A868A;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;

      RootItem.HoverColor := $EFD3C6;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := $C66931;
      RootItem.HoverTextColor := clWindowText;
      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $EFD3C6;
      SelectedItem.ColorTo := clNone;
      SelectedItem.BorderColor := $C66931;
      SelectedItem.CheckBorder := $EFD3C6;
      SelectedItem.CheckColor := $EFD3C6;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $EFD3C6;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.RadioBorder := $EFD3C6;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;

      IconBar.Color := clBtnFace;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;
      IconBar.CheckColor := clBtnFace;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clBtnFace;

      IconBar.RadioColor := clBtnFace;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clBtnFace;

      Separator.Color := $7A868A;
      Separator.ColorTo := clNone;

      with ButtonAppearance do
      begin
        DownColor := $B59284;
        DownColorTo := clNone;
        DownBorderColor := $C66931;
        HoverColor := $EFD3C6; //$D6BE85;
        HoverColorTo := clNone;
        HoverBorderColor := $C66931;
      end;
    end;
  osOffice2007Luna:
    begin
      Background.Color := $F6F6F6;
      Background.ColorTo := $F6F6F6;
      //MenuBorderColor := $962D00;
      MenuBorderColor := clSilver;

      RootItem.Color := $F2DAC2;
      RootItem.ColorTo := $F0D9C1;

      RootItem.SelectedColor := $76AFF1;
      RootItem.SelectedColorTo := $4190F3;
      RootItem.SelectedColorMirror := $0E72F1;
      RootItem.SelectedColorMirrorTo := $4C9FFD;
      RootItem.SelectedBorderColor := $45667B;

      RootItem.HoverColor := $EBFDFF;
      RootItem.HoverColorTo := $ABEBFF;
      RootItem.HoverColorMirror := $69D6FF;
      RootItem.HoverColorMirrorTo := $96E4FF;

      RootItem.HoverBorderColor := $99CEDB;

      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdvertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $EBFDFF;
      SelectedItem.ColorTo := $ABEBFF;
      SelectedItem.ColorMirror := $69D6FF;
      SelectedItem.ColorMirrorTo := $96E4FF;

      SelectedItem.BorderColor := $99CEDB;

      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $800000;
      SelectedItem.RadioBorder := $800000;

      IconBar.Color := $EEEEE9;     // #E3EFFF
      IconBar.ColorTo := $EEEEE9;   // #87ADE4
      //IconBar.Size := 24;
      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $800000;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $800000;
      IconBar.SeparatorColor := $C5C5C5;

      Separator.Color := $C5C5C5;
      Separator.ColorTo := clNone;

      ButtonAppearance.HoverColor := RGB(221, 222, 224);
      ButtonAppearance.HoverColorTo := RGB(241, 242, 244);;
      ButtonAppearance.HoverBorderColor := RGB(168, 175, 181);

      ButtonAppearance.DownColor := RGB(201, 202, 204);
      ButtonAppearance.DownColorTo := RGB(231, 232, 234);;
      ButtonAppearance.DownBorderColor := RGB(168, 175, 181);

    end;
  osOffice2007Obsidian:
    begin
      Background.Color := $F6F6F6;
      Background.ColorTo := $F6F6F6;
      //MenuBorderColor := $962D00;
      MenuBorderColor := clSilver;

      RootItem.Color := $B8B8B6;
      RootItem.ColorTo := $A0A09E;

      RootItem.SelectedColor := $76AFF1;
      RootItem.SelectedColorTo := $4190F3;
      RootItem.SelectedColorMirror := $0E72F1;
      RootItem.SelectedColorMirrorTo := $4C9FFD;
      RootItem.SelectedBorderColor := $45667B;

      RootItem.HoverColor := $EBFDFF;
      RootItem.HoverColorTo := $ABEBFF;
      RootItem.HoverColorMirror := $69D6FF;
      RootItem.HoverColorMirrorTo := $96E4FF;

      RootItem.HoverBorderColor := $99CEDB;

      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $EBFDFF;
      SelectedItem.ColorTo := $ABEBFF;
      SelectedItem.ColorMirror := $69D6FF;
      SelectedItem.ColorMirrorTo := $96E4FF;

      SelectedItem.BorderColor := $99CEDB;

      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $800000;
      SelectedItem.RadioBorder := $800000;

      IconBar.Color := $EEEEE9;     // #E3EFFF
      IconBar.ColorTo := $EEEEE9;   // #87ADE4
      //IconBar.Size := 24;
      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $800000;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $800000;
      IconBar.SeparatorColor := $C5C5C5;

      Separator.Color := $C5C5C5;
      Separator.ColorTo := clNone;

      ButtonAppearance.HoverColor := RGB(221, 222, 224);
      ButtonAppearance.HoverColorTo := RGB(241, 242, 244);;
      ButtonAppearance.HoverBorderColor := RGB(168, 175, 181);

      ButtonAppearance.DownColor := RGB(201, 202, 204);
      ButtonAppearance.DownColorTo := RGB(231, 232, 234);;
      ButtonAppearance.DownBorderColor := RGB(168, 175, 181);

    end;

  osOffice2007Silver:
    begin
      Background.Color := $FAFAFA;
      Background.ColorTo := $FAFAFA;
      //MenuBorderColor := $962D00;
      MenuBorderColor := $868686;

      RootItem.Color := $E5D7D7; //$F2DAC2;
      RootItem.ColorTo := $F7F3F3;  //$F0D9C1;

      RootItem.SelectedColor := $76AFF1;
      RootItem.SelectedColorTo := $4190F3;
      RootItem.SelectedColorMirror := $0E72F1;
      RootItem.SelectedColorMirrorTo := $4C9FFD;
      RootItem.SelectedBorderColor := $45667B;

      RootItem.HoverColor := $EBFDFF;
      RootItem.HoverColorTo := $ABEBFF;
      RootItem.HoverColorMirror := $69D6FF;
      RootItem.HoverColorMirrorTo := $96E4FF;

      RootItem.HoverBorderColor := $99CEDB;

      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdHorizontal;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $EBFDFF;
      SelectedItem.ColorTo := $ABEBFF;
      SelectedItem.ColorMirror := $69D6FF;
      SelectedItem.ColorMirrorTo := $96E4FF;

      SelectedItem.BorderColor := $99CEDB;

      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $800000;
      SelectedItem.RadioBorder := $800000;

      IconBar.Color := $EFEFEF;
      IconBar.ColorTo := $EFEFEF;
      //IconBar.Size := 24;
      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $800000;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $800000;
      IconBar.SeparatorColor := $C5C5C5;

      Separator.Color := $C5C5C5;
      Separator.ColorTo := clNone;

      ButtonAppearance.HoverColor := RGB(221, 222, 224);
      ButtonAppearance.HoverColorTo := RGB(241, 242, 244);;
      ButtonAppearance.HoverBorderColor := RGB(168, 175, 181);

      ButtonAppearance.DownColor := RGB(201, 202, 204);
      ButtonAppearance.DownColorTo := RGB(231, 232, 234);;
      ButtonAppearance.DownBorderColor := RGB(168, 175, 181);
    end;

  osWhidbey:
    begin
      Background.Color := $F9FCFC;
      Background.ColorTo := $F9FCFC;
      MenuBorderColor := $7A868A;

      RootItem.Color := $D9E6E7;
      RootItem.ColorTo := $D9E6E7;

      RootItem.SelectedColor := $F4F9F9;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := $7A868A;

      RootItem.HoverColor := $EFD3C6;
      RootItem.HoverColorTo := clNone;

      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;

      RootItem.HoverBorderColor := $C66931;
      RootItem.HoverTextColor := clWindowText;
      RootItem.SelectedTextColor := clBlack;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $EED2C1;
      SelectedItem.ColorTo := clNone;

      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;

      SelectedItem.BorderColor := $C56A31;
      SelectedItem.CheckBorder := clBlack;
      SelectedItem.CheckColor := $C56A31;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $C56A31;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.RadioBorder := clBlack;

      IconBar.Color := $FBFEFE;
      IconBar.ColorTo := $ACC3C4;
      //IconBar.Size := 24;
      IconBar.CheckColor := $E8E6E1;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $C56A31;


      IconBar.RadioColor := $E8E6E1;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $C56A31;

      Separator.Color := $B8C2C5;
      Separator.ColorTo := clNone;
    end;

  osVista:
    begin
                         // RRGGBB
      Background.Color := $EEEEEE;
      Background.ColorTo := clNone;
      //MenuBorderColor := $962D00;
      MenuBorderColor := $979797;

      RootItem.Color := clWhite; //$F2DAC2;
      RootItem.ColorTo := $EEDBD4;  //$F0D9C1;

      RootItem.SelectedColor := $BBB8B6;
      RootItem.SelectedColorTo := $E1D0C9;
      RootItem.SelectedColorMirror := $E1D0C9;
      RootItem.SelectedColorMirrorTo := $E9DAD5;
      RootItem.SelectedBorderColor := $534D4B;

      RootItem.HoverColor := clWhite;
      RootItem.HoverColorTo := $E7D9D3;
      RootItem.HoverColorMirror := $E7D9D3;
      RootItem.HoverColorMirrorTo := $F9F1EF;
      RootItem.HoverBorderColor := $A79A95;

      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $F4F1EB;
      SelectedItem.ColorTo := $F3EBDA;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $EBD8A8;

      SelectedItem.CheckColor := $F4EFE6;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $F4EFE6;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $E6D3CD;
      SelectedItem.RadioBorder := $E6D3CD;

      IconBar.Color := $EEEEEE;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;
      IconBar.CheckColor := $F4EFE6;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $E6D3CD;

      IconBar.RadioColor := $F4EFE6;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $E6D3CD;
      IconBar.SeparatorColor := $C5C5C5;

      Separator.Color := $E0E0E0;
      Separator.ColorTo := clWHite;

      ButtonAppearance.HoverColor := RGB(221, 222, 224);
      ButtonAppearance.HoverColorTo := RGB(241, 242, 244);;
      ButtonAppearance.HoverBorderColor := RGB(168, 175, 181);

      ButtonAppearance.DownColor := RGB(201, 202, 204);
      ButtonAppearance.DownColorTo := RGB(231, 232, 234);;
      ButtonAppearance.DownBorderColor := RGB(168, 175, 181);
    end;
  osWindows7:
    begin
                         // RRGGBB
      Background.Color := $EEEEEE;
      Background.ColorTo := clNone;
      //MenuBorderColor := $962D00;
      MenuBorderColor := $979797;

      RootItem.Color := clWhite; //$F2DAC2;
      RootItem.ColorTo := $EEDBD4;  //$F0D9C1;

      RootItem.SelectedColor := $FCEBDC;
      RootItem.SelectedColorTo := $FCDBC1;
      RootItem.SelectedColorMirror := $FCDBC1;
      RootItem.SelectedColorMirrorTo := $FCEBDC;
      RootItem.SelectedBorderColor := $CEA27D;

      RootItem.HoverColor := clWhite;
      RootItem.HoverColorTo := $E7D9D3;
      RootItem.HoverColorMirror := $E7D9D3;
      RootItem.HoverColorMirrorTo := $F9F1EF;
      RootItem.HoverBorderColor := $A79A95;

      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $FCEBDC;
      SelectedItem.ColorTo := $FCDBC1;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $CEA27D;

      SelectedItem.CheckColor := $FCEBDC;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $FCEBDC;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $E6D3CD;
      SelectedItem.RadioBorder := $CEA27D;

      IconBar.Color := $EEEEEE;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;
      IconBar.CheckColor := $F4EFE6;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $E6D3CD;

      IconBar.RadioColor := $F4EFE6;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $E6D3CD;
      IconBar.SeparatorColor := $C5C5C5;

      Separator.Color := $E0E0E0;
      Separator.ColorTo := clWHite;

      ButtonAppearance.HoverColor := RGB(221, 222, 224);
      ButtonAppearance.HoverColorTo := RGB(241, 242, 244);;
      ButtonAppearance.HoverBorderColor := RGB(168, 175, 181);

      ButtonAppearance.DownColor := RGB(201, 202, 204);
      ButtonAppearance.DownColorTo := RGB(231, 232, 234);;
      ButtonAppearance.DownBorderColor := RGB(168, 175, 181);
    end;
  osWindows8, osWindows10:
    begin
                         // RRGGBB
      Background.Color := $F7F6F5;
      Background.ColorTo := clNone;
      //MenuBorderColor := $962D00;
      MenuBorderColor := $979797;

      RootItem.Color := clWhite; //$F2DAC2;
      RootItem.ColorTo := $EEDBD4;  //$F0D9C1;

      RootItem.SelectedColor := $F7EFE8;
      RootItem.SelectedColorTo := $F7EFE8;
      RootItem.SelectedColorMirror := $F7EFE8;
      RootItem.SelectedColorMirrorTo := $F7EFE8;
      RootItem.SelectedBorderColor := $F9CEA4;

      RootItem.HoverColor := $F7EFE8;
      RootItem.HoverColorTo := $F7EFE8;
      RootItem.HoverColorMirror := $F7EFE8;
      RootItem.HoverColorMirrorTo := $F7EFE8;
      RootItem.HoverBorderColor := $F9CEA4;

      RootItem.HoverTextColor := clWindowText;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $F7EFE8;
      SelectedItem.ColorTo := $F7EFE8;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $F9CEA4;

      SelectedItem.CheckColor := $F7EFE8;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $F7EFE8;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $F9CEA4;
      SelectedItem.RadioBorder := $F9CEA4;

      IconBar.Color := $EEEEEE;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;
      IconBar.CheckColor := $F7EFE8;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $F9CEA4;

      IconBar.RadioColor := $F7EFE8;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $F9CEA4;
      IconBar.SeparatorColor := $C5C5C5;

      Separator.Color := $E0E0E0;
      Separator.ColorTo := clWHite;

      ButtonAppearance.HoverColor := $F7EFE8;
      ButtonAppearance.HoverColorTo := $F7EFE8;
      ButtonAppearance.HoverBorderColor := $F9CEA4;

      ButtonAppearance.DownColor := $F7EFE8;
      ButtonAppearance.DownColorTo := $F7EFE8;
      ButtonAppearance.DownBorderColor := $F9CEA4;
    end;
  osTerminal:
    begin

      Background.Color := clBtnFace;
      Background.ColorTo := clBtnFace;
      MenuBorderColor := clGray;

      RootItem.Color := clBtnFace;
      RootItem.ColorTo := clBtnFace;

      RootItem.SelectedColor := clHighLight;
      RootItem.SelectedColorTo := clHighLight;
      RootItem.SelectedColorMirror := clHighLight;
      RootItem.SelectedColorMirrorTo := clHighLight;
      RootItem.SelectedBorderColor := clGray;

      RootItem.HoverColor := clSilver;
      RootItem.HoverColorTo := clSilver;
      RootItem.HoverColorMirror := clSilver;
      RootItem.HoverColorMirrorTo := clSilver;
      RootItem.HoverBorderColor := clGray;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := clSilver;
      SelectedItem.ColorTo := clSilver;
      SelectedItem.ColorMirror := clSilver;
      SelectedItem.ColorMirrorTo := clSilver;
      SelectedItem.BorderColor := clGray;

      SelectedItem.CheckColor := clSilver;
      SelectedItem.CheckColorTo := clSilver;
      SelectedItem.RadioColor := clSilver;
      SelectedItem.RadioColorTo := clSilver;
      SelectedItem.CheckBorder := clGray;
      SelectedItem.RadioBorder := clGray;

      IconBar.Color := clBtnFace;
      IconBar.ColorTo := clBtnFace;
      //IconBar.Size := 24;
      IconBar.CheckColor := clSilver;
      IconBar.CheckColorTo := clSilver;
      IconBar.CheckBorder := clGray;

      IconBar.RadioColor := clSilver;
      IconBar.RadioColorTo := clSilver;
      IconBar.RadioBorder := clSilver;
      IconBar.SeparatorColor := clSilver;

      Separator.Color := clGray;
      Separator.ColorTo := clGray;

      ButtonAppearance.HoverColor := $EFD3C6;
      ButtonAppearance.HoverColorTo := clNone;
      ButtonAppearance.HoverBorderColor := $C66931;

      ButtonAppearance.DownColor := $B59284;
      ButtonAppearance.DownColorTo := clNone;
      ButtonAppearance.DownBorderColor := $C66931;
    end;
  osOffice2010Blue:
    begin

      Background.Color := $FFFFFF;
      Background.ColorTo := $FFFFFF;
      MenuBorderColor := $B0ABA7;

      RootItem.Color := $FDF6EF;
      RootItem.ColorTo := $F0DAC7;

      RootItem.SelectedColor := $7BEEFF;
      RootItem.SelectedColorTo := $6CD0FF;
      RootItem.SelectedColorMirror := $6CD0FF;
      RootItem.SelectedColorMirrorTo := $7BEEFF;
      RootItem.SelectedBorderColor := $308AC2;

      RootItem.HoverColor := $D9F9FD;
      RootItem.HoverColorTo := $8AE3FD;
      RootItem.HoverColorMirror := $8AE3FD;
      RootItem.HoverColorMirrorTo := $D9F9FD;
      RootItem.HoverBorderColor := $58CAF1;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $D9F9FD;
      SelectedItem.ColorTo := $8AE3FD;
      SelectedItem.ColorMirror := $8AE3FD;
      SelectedItem.ColorMirrorTo := $D9F9FD;
      SelectedItem.BorderColor := $58CAF1;

      SelectedItem.CheckColor := $C2F1FC;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $C2F1FC;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $3695F2;
      SelectedItem.RadioBorder := $3695F2;


      IconBar.Color := $FFFFFF;
      IconBar.ColorTo := $FFFFFF;
      //IconBar.Size := 24;
      IconBar.CheckColor := $C2F1FC;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $3695F2;

      IconBar.RadioColor := $C2F1FC;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $3695F2;
      IconBar.SeparatorColor := $E7E4E2;

      Separator.Color := $C9C8C7;
      Separator.ColorTo := $C9C8C7;

      ButtonAppearance.HoverColor := $8AE3FD;
      ButtonAppearance.HoverColorTo := $D9F9FD;
      ButtonAppearance.HoverBorderColor := $58CAF1;

      ButtonAppearance.DownColor := $6CD0FF;
      ButtonAppearance.DownColorTo := $7BEEFF;
      ButtonAppearance.DownBorderColor := $308AC2;
    end;
  osOffice2010Silver:
    begin
      Background.Color := $FFFFFF;
      Background.ColorTo := $FFFFFF;
      MenuBorderColor := $B0ABA7;

      RootItem.Color := $FFFFFF;
      RootItem.ColorTo := $EDE5E0;

      RootItem.SelectedColor := $7BEEFF;
      RootItem.SelectedColorTo := $6CD0FF;
      RootItem.SelectedColorMirror := $6CD0FF;
      RootItem.SelectedColorMirrorTo := $7BEEFF;
      RootItem.SelectedBorderColor := $308AC2;

      RootItem.HoverColor := $D9F9FD;
      RootItem.HoverColorTo := $8AE3FD;
      RootItem.HoverColorMirror := $8AE3FD;
      RootItem.HoverColorMirrorTo := $D9F9FD;
      RootItem.HoverBorderColor := $58CAF1;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $D9F9FD;
      SelectedItem.ColorTo := $8AE3FD;
      SelectedItem.ColorMirror := $8AE3FD;
      SelectedItem.ColorMirrorTo := $D9F9FD;
      SelectedItem.BorderColor := $58CAF1;

      SelectedItem.CheckColor := $C2F1FC;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $C2F1FC;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $3695F2;
      SelectedItem.RadioBorder := $3695F2;


      IconBar.Color := $FFFFFF;
      IconBar.ColorTo := $FFFFFF;
      //IconBar.Size := 24;
      IconBar.CheckColor := $C2F1FC;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $3695F2;

      IconBar.RadioColor := $C2F1FC;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $3695F2;
      IconBar.SeparatorColor := $E7E4E2;

      Separator.Color := $C9C8C7;
      Separator.ColorTo := $C9C8C7;

      ButtonAppearance.HoverColor := $8AE3FD;
      ButtonAppearance.HoverColorTo := $D9F9FD;
      ButtonAppearance.HoverBorderColor := $58CAF1;

      ButtonAppearance.DownColor := $6CD0FF;
      ButtonAppearance.DownColorTo := $7BEEFF;
      ButtonAppearance.DownBorderColor := $308AC2;
    end;
  osOffice2010Black:
    begin
      Background.Color := $FFFFFF;
      Background.ColorTo := $FFFFFF;
      MenuBorderColor := $B0ABA7;

      RootItem.Color := $BFBFBF;
      RootItem.ColorTo := $919191;

      RootItem.SelectedColor := $7BEEFF;
      RootItem.SelectedColorTo := $6CD0FF;
      RootItem.SelectedColorMirror := $6CD0FF;
      RootItem.SelectedColorMirrorTo := $7BEEFF;
      RootItem.SelectedBorderColor := $308AC2;

      RootItem.HoverColor := $D9F9FD;
      RootItem.HoverColorTo := $8AE3FD;
      RootItem.HoverColorMirror := $8AE3FD;
      RootItem.HoverColorMirrorTo := $D9F9FD;
      RootItem.HoverBorderColor := $58CAF1;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $D9F9FD;
      SelectedItem.ColorTo := $8AE3FD;
      SelectedItem.ColorMirror := $8AE3FD;
      SelectedItem.ColorMirrorTo := $D9F9FD;
      SelectedItem.BorderColor := $58CAF1;

      SelectedItem.CheckColor := $C2F1FC;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $C2F1FC;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $3695F2;
      SelectedItem.RadioBorder := $3695F2;


      IconBar.Color := $FFFFFF;
      IconBar.ColorTo := $FFFFFF;
      //IconBar.Size := 24;
      IconBar.CheckColor := $C2F1FC;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $3695F2;

      IconBar.RadioColor := $C2F1FC;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $3695F2;
      IconBar.SeparatorColor := $E7E4E2;

      Separator.Color := $C9C8C7;
      Separator.ColorTo := $C9C8C7;

      ButtonAppearance.HoverColor := $8AE3FD;
      ButtonAppearance.HoverColorTo := $D9F9FD;
      ButtonAppearance.HoverBorderColor := $58CAF1;

      ButtonAppearance.DownColor := $6CD0FF;
      ButtonAppearance.DownColorTo := $7BEEFF;
      ButtonAppearance.DownBorderColor := $308AC2;
    end;

  osOffice2013White:
    begin

      Background.Color := clWhite;
      Background.ColorTo := clWhite;
      MenuBorderColor := $B0ABA7;

      RootItem.Color := clWhite;
      RootItem.ColorTo := clWhite;

      RootItem.SelectedColor := $FCF0E4;
      RootItem.SelectedColorTo := $FCF0E4;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;
      RootItem.SelectedBorderColor := $EAB47E;

      RootItem.HoverColor := $00F7E6CD;
      RootItem.HoverColorTo := $00F7E6CD;
      RootItem.HoverColorMirror := $00F7E6CD;
      RootItem.HoverColorMirrorTo := $00F7E6CD;
      RootItem.HoverBorderColor := $00E7BE7A;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $FCF0E4;
      SelectedItem.ColorTo := $FCF0E4;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $EAB47E;

      SelectedItem.CheckColor := $FCF0E4;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $FCF0E4;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $3695F2;
      SelectedItem.RadioBorder := $3695F2;


      IconBar.Color := $FFFFFF;
      IconBar.ColorTo := $FFFFFF;
      //IconBar.Size := 24;
      IconBar.CheckColor := $FCF0E4;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $EAB47E;

      IconBar.RadioColor := $FCF0E4;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $EAB47E;
      IconBar.SeparatorColor := $E7E4E2;

      Separator.Color := $C9C8C7;
      Separator.ColorTo := $C9C8C7;

      ButtonAppearance.HoverColor := $FCF0E4;
      ButtonAppearance.HoverColorTo := $FCF0E4;
      ButtonAppearance.HoverBorderColor := $EAB47E;

      ButtonAppearance.DownColor := $FCF0E4;
      ButtonAppearance.DownColorTo := $FCF0E4;
      ButtonAppearance.DownBorderColor := $EAB47E;
    end;
  osOffice2013LightGray:
    begin

      Background.Color := $F6F6F6;
      Background.ColorTo := $F6F6F6;
      MenuBorderColor := $B0ABA7;

      RootItem.Color := $F6F6F6;
      RootItem.ColorTo := $F6F6F6;

      RootItem.SelectedColor := $FCF0E4;
      RootItem.SelectedColorTo := $FCF0E4;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;
      RootItem.SelectedBorderColor := $EAB47E;

      RootItem.HoverColor := $00F7E6CD;
      RootItem.HoverColorTo := $00F7E6CD;
      RootItem.HoverColorMirror := $00F7E6CD;
      RootItem.HoverColorMirrorTo := $00F7E6CD;
      RootItem.HoverBorderColor := $00E7BE7A;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $FCF0E4;
      SelectedItem.ColorTo := $FCF0E4;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $EAB47E;

      SelectedItem.CheckColor := $FCF0E4;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $FCF0E4;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $EAB47E;
      SelectedItem.RadioBorder := $EAB47E;


      IconBar.Color := $FFFFFF;
      IconBar.ColorTo := $FFFFFF;
      //IconBar.Size := 24;
      IconBar.CheckColor := $FCF0E4;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $EAB47E;

      IconBar.RadioColor := $FCF0E4;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $EAB47E;
      IconBar.SeparatorColor := $E7E4E2;

      Separator.Color := $C9C8C7;
      Separator.ColorTo := $C9C8C7;

      ButtonAppearance.HoverColor := $FCF0E4;
      ButtonAppearance.HoverColorTo := $FCF0E4;
      ButtonAppearance.HoverBorderColor := $EAB47E;

      ButtonAppearance.DownColor := $FCF0E4;
      ButtonAppearance.DownColorTo := $FCF0E4;
      ButtonAppearance.DownBorderColor := $EAB47E;
    end;
  osOffice2013Gray:
    begin

      Background.Color := $E5E5E5;
      Background.ColorTo := $E5E5E5;
      MenuBorderColor := $B0ABA7;

      RootItem.Color := $E5E5E5;
      RootItem.ColorTo := $E5E5E5;

      RootItem.SelectedColor := $FCF0E4;
      RootItem.SelectedColorTo := $FCF0E4;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;
      RootItem.SelectedBorderColor := $EAB47E;

      RootItem.HoverColor := $00F7E6CD;
      RootItem.HoverColorTo := $00F7E6CD;
      RootItem.HoverColorMirror := $00F7E6CD;
      RootItem.HoverColorMirrorTo := $00F7E6CD;
      RootItem.HoverBorderColor := $00E7BE7A;

      RootItem.HoverTextColor := clBlack;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;

      SelectedItem.Color := $FCF0E4;
      SelectedItem.ColorTo := $FCF0E4;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $EAB47E;

      SelectedItem.CheckColor := $FCF0E4;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $FCF0E4;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $EAB47E;
      SelectedItem.RadioBorder := $EAB47E;

      IconBar.Color := $F6F6F6;
      IconBar.ColorTo := $F6F6F6;
      //IconBar.Size := 24;
      IconBar.CheckColor := $FCF0E4;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $EAB47E;

      IconBar.RadioColor := $FCF0E4;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $EAB47E;
      IconBar.SeparatorColor := $A0A0A0;

      Separator.Color := $C9C8C7;
      Separator.ColorTo := $C9C8C7;

      ButtonAppearance.HoverColor := $FCF0E4;
      ButtonAppearance.HoverColorTo := $FCF0E4;
      ButtonAppearance.HoverBorderColor := $EAB47E;

      ButtonAppearance.DownColor := $FCF0E4;
      ButtonAppearance.DownColorTo := $FCF0E4;
      ButtonAppearance.DownBorderColor := $EAB47E;
    end;
  osOffice2016White:
    begin

      Background.Color := clWhite;
      Background.ColorTo := clWhite;
      MenuBorderColor := $D4D4D4;

      RootItem.Color := clWhite;
      RootItem.ColorTo := clWhite;

      RootItem.SelectedColor := $E3BDA3;
      RootItem.SelectedColorTo := $E3BDA3;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;
      RootItem.SelectedBorderColor := $E3BDA3;

      RootItem.HoverColor := $F2E1D5;
      RootItem.HoverColorTo := $F2E1D5;
      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.HoverBorderColor := $F2E1D5;

      RootItem.HoverTextColor := $505050;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := $505050;

      SelectedItem.Color := $E3BDA3;
      SelectedItem.ColorTo := $E3BDA3;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $E3BDA3;

      SelectedItem.CheckColor := $F2D5C2;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $F2D5C2;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $F2D5C2;
      SelectedItem.RadioBorder := $F2D5C2;


      IconBar.Color := clWhite;
      IconBar.ColorTo := clWhite;
      //IconBar.Size := 24;
      IconBar.CheckColor := $F2D5C2;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $F2D5C2;

      IconBar.RadioColor := $F2E1D5;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $F2E1D5;
      IconBar.SeparatorColor := $D4D4D4;

      Separator.Color := $D4D4D4;
      Separator.ColorTo := $D4D4D4;

      ButtonAppearance.HoverColor := $F2E1D5;
      ButtonAppearance.HoverColorTo := $F2E1D5;
      ButtonAppearance.HoverBorderColor := $F2E1D5;

      ButtonAppearance.DownColor := $E3BDA3;
      ButtonAppearance.DownColorTo := $E3BDA3;
      ButtonAppearance.DownBorderColor := $E3BDA3;
    end;
  osOffice2016Gray:
    begin
      Background.Color := $B2B2B2;
      Background.ColorTo := $B2B2B2;
      MenuBorderColor := $444444;

      RootItem.Color := $444444;
      RootItem.ColorTo := $444444;


      RootItem.SelectedColor := $E3BDA3;
      RootItem.SelectedColorTo := $E3BDA3;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;
      RootItem.SelectedBorderColor := $E3BDA3;

      RootItem.HoverColor := $F2E1D5;
      RootItem.HoverColorTo := $F2E1D5;
      RootItem.HoverColorMirror := clNone;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.HoverBorderColor := $F2E1D5;

      RootItem.HoverTextColor := $424242;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := $F0F0F0;
      RootItem.UseSystemFont := false;

      SelectedItem.Color := $E3BDA3;
      SelectedItem.ColorTo := $E3BDA3;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $E3BDA3;

      SelectedItem.CheckColor := $F2D5C2;
      SelectedItem.CheckColorTo := $F2D5C2;
      SelectedItem.RadioColor := $F2D5C2;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $F2D5C2;
      SelectedItem.RadioBorder := $F2D5C2;


      IconBar.Color := $B2B2B2;
      IconBar.ColorTo := $B2B2B2;
      //IconBar.Size := 24;
      IconBar.CheckColor := $F2D5C2;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $F2D5C2;

      IconBar.RadioColor := $F2E1D5;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $F2E1D5;
      IconBar.SeparatorColor := $444444;

      Separator.Color := $444444;
      Separator.ColorTo := $444444;

      ButtonAppearance.HoverColor := $F2E1D5;
      ButtonAppearance.HoverColorTo := $F2E1D5;
      ButtonAppearance.HoverBorderColor := $F2E1D5;

      ButtonAppearance.DownColor := $E3BDA3;
      ButtonAppearance.DownColorTo := $E3BDA3;
      ButtonAppearance.DownBorderColor := $E3BDA3;
    end;
  osOffice2016Black:
    begin
      Background.Color := $363636;
      Background.ColorTo := $363636;
      MenuBorderColor := $444444;

      Font.Color := $C6C6C6;

      RootItem.Color := $363636;
      RootItem.ColorTo := $363636;

      RootItem.SelectedColor := $444444;
      RootItem.SelectedColorTo := $444444;
      RootItem.SelectedColorMirror := clNone;
      RootItem.SelectedColorMirrorTo := clNone;
      RootItem.SelectedBorderColor := $444444;

      RootItem.HoverColor := $6A6A6A;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverColorMirror := $6A6A6A;
      RootItem.HoverColorMirrorTo := clNone;
      RootItem.HoverBorderColor := $6A6A6A;

      RootItem.HoverTextColor := $A6A6A6;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := $A6A6A6;
      RootItem.UseSystemFont := False;
      RootItem.SelectedTextColor := clWhite;

//      SelectedItem.Color := $444444;
//      SelectedItem.ColorTo := $444444;
      SelectedItem.Color := clWhite;
      SelectedItem.ColorTo := clWhite;
      SelectedItem.ColorMirror := clNone;
      SelectedItem.ColorMirrorTo := clNone;
      SelectedItem.BorderColor := $444444;

      SelectedItem.CheckColor := $6A6A6A;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $6A6A6A;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.CheckBorder := $6A6A6A;
      SelectedItem.RadioBorder := $6A6A6A;

      SelectedItem.Font.Color := clBlack;
      SelectedItem.UseSystemFont := false;

      IconBar.Color := $363636;
      IconBar.ColorTo := $363636;
      //IconBar.Size := 24;
      IconBar.CheckColor := $6A6A6A;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $6A6A6A;

      IconBar.RadioColor := $6A6A6A;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $6A6A6A;
      IconBar.SeparatorColor := $444444;

      Separator.Color := $444444;
      Separator.ColorTo := $444444;

      ButtonAppearance.HoverColor := $6A6A6A;
      ButtonAppearance.HoverColorTo := $6A6A6A;
      ButtonAppearance.HoverBorderColor := $6A6A6A;

      ButtonAppearance.DownColor := $444444;
      ButtonAppearance.DownColorTo := $444444;
      ButtonAppearance.DownBorderColor := $444444;

      UseSystemFont := false;
    end;
  end;

  if Assigned(Menu) and (Menu is TAdvMainMenu) then
    DrawMenuBar(Menu.WindowHandle);
end;

{ TAdvMenuFantasyStyler }

constructor TAdvMenuFantasyStyler.Create(AOwner: TComponent);
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    Style := fsCustom;
  end;
end;

procedure TAdvMenuFantasyStyler.SetStyle(const Value: TFantasyStyle);
begin
  FStyle := Value;

  RootItem.SelectedColorMirror := clNone;
  RootItem.SelectedColorMirrorTo := clNone;
  RootItem.HoverColorMirror := clNone;
  RootItem.HoverColorMirrorTo := clNone;
  SelectedItem.ColorMirror := clNone;
  SelectedItem.ColorMirrorTo := clNone;

  case FStyle of
  fsChocolate:
    begin
      Background.Color := clWindow;
      Background.ColorTo := clBtnFace;
      Background.GradientDirection := gdHorizontal;
      MenuBorderColor := clGray;
      Background.Image.Bitmap := nil;

      Font.Color := clMaroon;
      RootItem.Font.Style := [];
      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      RootItem.Color := clBtnFace;
      RootItem.ColorTo := clWhite;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clMaroon;
      RootItem.Font.Size := 8;

      RootItem.SelectedColor := clBtnFace;
      RootItem.SelectedColorTo := clWhite;
      RootItem.SelectedBorderColor := $7A868A;
      RootItem.SelectedTextColor := $7A868A;

      RootItem.HoverColor := $D9FFFF;
      RootItem.HoverColorTo := $C1FFFE;
      RootItem.HoverBorderColor := $94CCCB;
      RootItem.HoverTextColor := clMaroon;

      SelectedItem.Color := $C1FFFE;
      SelectedItem.ColorTo := $D9FFFF;
      SelectedItem.GradientDirection := gdHorizontal;
      SelectedItem.BorderColor := $94CCCB;
      SelectedItem.Font.Color := $7A868A;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;

      SelectedItem.CheckBorder := $006F4B4B;
      SelectedItem.CheckColor := $3E80FE;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioBorder := $006F4B4B;
      SelectedItem.RadioColor := $3E80FE;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := $D7EDED;
      IconBar.ColorTo := $8AC2C1;
      //IconBar.Size := 24;

      IconBar.RadioColor := $006FC0FF;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := $006F4B4B;

      IconBar.CheckColor := $006FC0FF;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $006F4B4B;


      Separator.Color := $7A868A;
      Separator.ColorTo := clNone;

      SideBar.Visible := false;
    end;
  fsWindowsClassic:
    begin
      Background.Color := clMenu;
      Background.ColorTo := clMenu;
      Background.GradientDirection := gdHorizontal;
      MenuBorderColor := clBlack;
      Background.Image.Bitmap := nil;

      Font.Color := clBlack;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      RootItem.Color := clMenu;
      RootItem.ColorTo := clNone;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;
      RootItem.Font.Size := 8;
      RootItem.Font.Style := [];            

      RootItem.SelectedColor := $00A1684A;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := clNone;
      RootItem.SelectedTextColor := clWhite;

      RootItem.HoverColor := $00A1684A;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := clNone;
      RootItem.HoverTextColor := clWhite;

      SelectedItem.Color := $00A1684A;
      SelectedItem.ColorTo := clNone;
      SelectedItem.GradientDirection := gdHorizontal;
      SelectedItem.BorderColor := clNone;
      SelectedItem.Font.Color := clWhite;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;

      SelectedItem.CheckBorder := clNone;
      SelectedItem.CheckColor := clNone;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioBorder := clNone;
      SelectedItem.RadioColor := clNone;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := clMenu;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;

      IconBar.RadioColor := clNone;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clNone;

      IconBar.CheckColor := clNone;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clNone;


      Separator.Color := clBlack;
      Separator.ColorTo := clNone;

      SideBar.Visible := true;
      SideBar.Caption := 'TMS Menu';
      SideBar.Image.Color := $00EDC6A9;
      SideBar.Image.ColorTo := $006A240A;
      SideBar.Background.GradientDirection := gdHorizontal;
      SideBar.Font.Color := clWhite;

    end;
  fsArctic:
    begin
      Background.Color := clWindow;
      Background.ColorTo := clNone;
      Background.GradientDirection := gdHorizontal;
      MenuBorderColor := clSilver;
      Background.Image.Bitmap := nil;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      Font.Color := clSilver;
      RootItem.Font.Style := [];

      RootItem.Color := clWhite;
      RootItem.ColorTo := clInactiveCaptionText;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clSilver;
      RootItem.Font.Size := 8;      

      RootItem.SelectedColor := clInactiveCaptionText;
      RootItem.SelectedColorTo := clWhite;
      RootItem.SelectedBorderColor := clSilver;
      RootItem.SelectedTextColor := clGray;

      RootItem.HoverColor := clNone;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := clNone;
      RootItem.HoverTextColor := clGray;

      SelectedItem.Color := clWhite;
      SelectedItem.ColorTo := clInactiveCaptionText;
      SelectedItem.GradientDirection := gdVertical;
      SelectedItem.BorderColor := clSilver;
      SelectedItem.Font.Color := clGray;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;

      SelectedItem.CheckBorder := clSilver;
      SelectedItem.CheckColor := clSilver;
      SelectedItem.CheckColorTo := clWhite;
      SelectedItem.RadioBorder := clSilver;
      SelectedItem.RadioColor := clSilver;
      SelectedItem.RadioColorTo := clWhite;

      IconBar.Color := $EFEFEF;
      IconBar.ColorTo := clWhite;
      //IconBar.Size := 24;

      IconBar.RadioColor := clNone;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clSilver;

      IconBar.CheckColor := clNone;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clSilver;


      Separator.Color := clSilver;
      Separator.ColorTo := clWhite;
      Separator.GradientDirection := gdHorizontal;

      SideBar.Visible := false;

      end;
  fsTerminalGreen:
    begin
      Background.Color := clWindow;
      Background.ColorTo := clNone;
      Background.GradientDirection := gdHorizontal;
      MenuBorderColor := $0000C000;
      Background.Image.Bitmap := nil;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      Font.Color := clGreen;
      RootItem.Font.Style := [];

      RootItem.Color := $00F7FFF7;
      RootItem.ColorTo := clNone;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;
      RootItem.Font.Size := 8;

      RootItem.SelectedColor := $0000C000;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := $0000C000;
      RootItem.SelectedTextColor := clBlack;

      RootItem.HoverColor := $00F7FFF7;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := clNone;
      RootItem.HoverTextColor := $0000C000;

      SelectedItem.Color := $00F7FFF7;
      SelectedItem.ColorTo := clNone;
      SelectedItem.GradientDirection := gdVertical;
      SelectedItem.BorderColor := $0000C000;
      SelectedItem.Font.Color := clGreen;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;

      SelectedItem.CheckBorder := clNone;
      SelectedItem.CheckColor := clNone;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioBorder := clNone;
      SelectedItem.RadioColor := clNone;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := $00A000;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;

      IconBar.RadioColor := $0000C000;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clNone;

      IconBar.CheckColor := $0000C000;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clNone;

      Separator.Color := $0000C000;
      Separator.ColorTo := clNone;

      SideBar.Visible := false;
    end;
  fsSoftSand:
    begin
      Background.Color := $00E7F7FF;
      Background.ColorTo := clNone;
      Background.GradientDirection := gdHorizontal;
      MenuBorderColor := clBlack;
      Background.Image.Bitmap := nil;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      RootItem.Color := $00E7F7FF;
      RootItem.ColorTo := clWhite;
      RootItem.GradientDirection := gdHorizontal;
      RootItem.Font.Color := clGray;
      RootItem.Font.Size := 8;

      RootItem.SelectedColor := $00E7F7FF;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := clBlack;
      RootItem.SelectedTextColor := $000884BD;

      RootItem.HoverColor := $00E7F7FF;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := clNone;
      RootItem.HoverTextColor := $000884BD;

      Font.Color := clGray;
      RootItem.Font.Style := [];

      SelectedItem.Color := $00B5DEF7;
      SelectedItem.ColorTo := $00E7F7FF;
      SelectedItem.GradientDirection := gdHorizontal;
      SelectedItem.BorderColor := clNone;
      SelectedItem.Font.Color := $000884BD;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;

      SelectedItem.CheckBorder := clNone;
      SelectedItem.CheckColor := clNone;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioBorder := clNone;
      SelectedItem.RadioColor := clNone;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := $00B5DEF7;
      IconBar.ColorTo := clNone;
      //IconBar.Size := 24;

      IconBar.RadioColor := clNone;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clNone;

      IconBar.CheckColor := clNone;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clNone;


      Separator.Color := $000884BD;
      Separator.ColorTo := $000884BD;

      SideBar.Visible := false;
    end;
  fsAquaBlue:
    begin
      Background.Color := clwhite;
      Background.ColorTo := clWhite;
      Background.GradientDirection := gdHorizontal;
      Background.Image.Bitmap := nil;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      MenuBorderColor := clNone;

      RootItem.Color := $00D08130;
      RootItem.ColorTo := $00FEF17E;
      RootItem.GradientDirection := gdHorizontal;
      RootItem.Font.Color := clWhite;
      RootItem.Font.Size := 8;      

      RootItem.SelectedColor := $00E6E0B0;
      RootItem.SelectedColorTo := $00F9FFBF;
      RootItem.SelectedBorderColor := clNone;
      RootItem.SelectedTextColor := $000080FF;

      RootItem.HoverColor := $00E6E0B0;
      RootItem.HoverColorTo := $00F9FFBF;
      RootItem.HoverBorderColor := clNone;
      RootItem.HoverTextColor := $00D08130;

      SelectedItem.Color := $00FEF17E;
      SelectedItem.ColorTo := $00D08130;
      SelectedItem.GradientDirection := gdVertical;
      SelectedItem.BorderColor := $00D08130;
      SelectedItem.Font.Color := clWhite;
      SelectedItem.Font.Style := [fsBold];      
      SelectedItem.UseSystemFont := false;

      Font.Color := $0026B0FB;
      RootItem.Font.Style := [];

      SelectedItem.CheckBorder := $00D08130;
      SelectedItem.CheckColor := clNone;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.CheckGradientDirection := gdHorizontal;
      SelectedItem.RadioBorder := $00D08130;
      SelectedItem.RadioColor := clNone;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := $00E6E0B0;
      IconBar.ColorTo := clWhite;
      //IconBar.Size := 24;

      IconBar.RadioColor := $00A7EDFF;
      IconBar.RadioColorTo := clWhite;
      IconBar.RadioBorder := clNone;
      IconBar.RadioGradientDirection := gdHorizontal;

      IconBar.CheckColor := $00A7EDFF;
      IconBar.CheckColorTo := clWhite;
      IconBar.CheckBorder := clNone;
      IconBar.CheckGradientDirection := gdHorizontal;


      Separator.Color := $00E6E0B0;
      Separator.ColorTo := $00E6E0B0;
      Separator.GradientDirection := gdHorizontal;

      SideBar.Visible := false;

    end;
  fsSilverFox:
    begin
      Background.Color := $00D1C9C8;
      Background.ColorTo := $00D1C9C8;
      Background.GradientDirection := gdVertical;
      MenuBorderColor := $00666666;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      Background.Image.Bitmap := nil;
      Font.Color := clBlack;
      RootItem.Font.Style := [];      

      RootItem.Color := clWhite;
      RootItem.ColorTo := $00C5BAB7;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;
      RootItem.Font.Size := 8;      

      RootItem.SelectedColor := $00F1F3F4;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := $00666666;
      RootItem.SelectedTextColor := clBlack;

      RootItem.HoverColor := $00D2BDB5;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := $006A240A;
      RootItem.HoverTextColor := $000000B0;

      SelectedItem.Color := $00D2BDB5;
      SelectedItem.ColorTo := clNone;
      SelectedItem.GradientDirection := gdVertical;
      SelectedItem.BorderColor := $006A240A;
      SelectedItem.Font.Color := $000000B0;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;

      SelectedItem.CheckBorder := clNone;
      SelectedItem.CheckColor := clNone;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioBorder := clNone;
      SelectedItem.RadioColor := clNone;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := clWhite;
      IconBar.ColorTo := $00E4DCDA;
      //IconBar.Size := 24;

      IconBar.RadioColor := clNone;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clNone;

      IconBar.CheckColor := clNone;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clNone;


      Separator.Color := $00A6A6A6;
      Separator.ColorTo := clNone;
      Separator.GradientDirection := gdHorizontal;

      SideBar.Visible := false;

      end;
 fsTextured:
    begin
      Background.Color := clNone;
      Background.ColorTo := clNone;
      Background.GradientDirection := gdVertical;
      MenuBorderColor := $000000B0;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      Background.Image.Bitmap.LoadFromResourceName(HInstance, 'MENUTEXTURE');
      Background.Position := bpTiled;

      Font.Color := clBlack;
      Font.Style := [fsBold];
    

      RootItem.Color := $00CFD5F3;
      RootItem.ColorTo := $00CFD5F3;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;
      RootItem.Font.Size := 10;

      RootItem.Font.Style := [fsBold,fsItalic];

      RootItem.SelectedColor := clNone;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := $000000B0;
      RootItem.SelectedTextColor := $000000B0;

      RootItem.HoverColor := clNone;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := $000000B0;
      RootItem.HoverTextColor := clBlack;

      SelectedItem.Color := clNone;
      SelectedItem.ColorTo := clNone;
      SelectedItem.GradientDirection := gdVertical;
      SelectedItem.UseSystemFont := False;
      SelectedItem.BorderColor := clRed;
      SelectedItem.Font.Color := clRed;
      SelectedItem.Font.Style := [fsBold];

      SelectedItem.CheckBorder := clNone;
      SelectedItem.CheckColor := clNone;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioBorder := clNone;
      SelectedItem.RadioColor := clNone;
      SelectedItem.RadioColorTo := clNone;

      IconBar.Color := $00CFD5F3;
      IconBar.ColorTo := $00CFD5F3;
      //IconBar.Size := 24;

      IconBar.RadioColor := clNone;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clNone;

      IconBar.CheckColor := clNone;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clNone;


      Separator.Color := clWhite;
      Separator.ColorTo := clNone;
      Separator.GradientDirection := gdHorizontal;

      SideBar.Visible := false;
    end;
  fsMacOS:
    begin
      Background.Color := clWhite;
      Background.ColorTo := $00F3F3F3;
      Background.GradientDirection := gdVertical;
      MenuBorderColor := clGray;
      Background.Image.Bitmap := nil;

      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      Font.Color := clBlack;
      RootItem.Font.Style := [];      

      RootItem.Color := clWhite;
      RootItem.ColorTo := $00F3F3F3;
      RootItem.GradientDirection := gdVertical;
      RootItem.Font.Color := clBlack;
      RootItem.Font.Size := 8;

      RootItem.SelectedColor := $00F19546;
      RootItem.SelectedColorTo := $00C34907;
      RootItem.SelectedBorderColor := clNone;
      RootItem.SelectedTextColor := clWhite;

      RootItem.HoverColor := clSilver;
      RootItem.HoverColorTo := $00BBBBBB;
      RootItem.HoverBorderColor := clNone;
      RootItem.HoverTextColor := clBlack;

      SelectedItem.Color := $00F19546;
      SelectedItem.ColorTo := $00C34907;
      SelectedItem.GradientDirection := gdVertical;
      SelectedItem.BorderColor := clNone;
      SelectedItem.Font.Color := clWhite;
      SelectedItem.Font.Style := [];
      SelectedItem.UseSystemFont := false;      

      SelectedItem.CheckBorder := clNone;
      SelectedItem.CheckColor := $00F3F3F3;
      SelectedItem.CheckColorTo := $00BBBBBB;
      SelectedItem.CheckGradientDirection := gdVertical;
      SelectedItem.RadioBorder := clNone;
      SelectedItem.RadioColor := $00F3F3F3;
      SelectedItem.RadioColorTo := $00BBBBBB;
      SelectedItem.RadioGradientDirection := gdVertical;

      IconBar.Color := $00F3F3F3;
      IconBar.ColorTo := clWhite;
      //IconBar.Size := 24;

      IconBar.RadioColor := clNone;
      IconBar.RadioColorTo := clNone;
      IconBar.RadioBorder := clNone;

      IconBar.CheckColor := clNone;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := clNone;

      Separator.Color := clWhite;
      Separator.ColorTo := clWhite;

      SideBar.Visible := false;
    end;
  fsWhidbey:
    begin
      Background.Color := $F9FCFC;
      Background.ColorTo := $F9FCFC;
      MenuBorderColor := $7A868A;
      UseSystemFont := false;
      RootItem.UseSystemFont := false;

      RootItem.Color := $D9E6E7;
      RootItem.ColorTo := $D9E6E7;

      RootItem.SelectedColor := $F4F9F9;
      RootItem.SelectedColorTo := clNone;
      RootItem.SelectedBorderColor := $7A868A;

      RootItem.HoverColor := $EFD3C6;
      RootItem.HoverColorTo := clNone;
      RootItem.HoverBorderColor := $C66931;
      RootItem.HoverTextColor := clWindowText;
      RootItem.SelectedTextColor := clBlack;

      SelectedItem.Color := $EED2C1;
      SelectedItem.ColorTo := clNone;
      SelectedItem.BorderColor := $C56A31;
      SelectedItem.CheckBorder := clBlack;
      SelectedItem.CheckColor := $C56A31;
      SelectedItem.CheckColorTo := clNone;
      SelectedItem.RadioColor := $C56A31;
      SelectedItem.RadioColorTo := clNone;
      SelectedItem.RadioBorder := clBlack;
      SelectedItem.UseSystemFont := false;

      IconBar.Color := $FBFEFE;
      IconBar.ColorTo := $ACC3C4;
      //IconBar.Size := 24;
      IconBar.CheckColor := $E8E6E1;
      IconBar.CheckColorTo := clNone;
      IconBar.CheckBorder := $C56A31;

      Separator.Color := $B8C2C5;
      Separator.ColorTo := clNone;
    end;
  end;
  if Assigned(Menu) and (Menu is TAdvMainMenu) then
    DrawMenuBar(Menu.WindowHandle);
end;

{ TNotifierWindow }

procedure TNotifierWindow.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_THEMECHANGED  then
  begin
    if Assigned(FOnThemeChange) then
      FOnThemeChange(Self);
  end;
  inherited;
end;

end.
