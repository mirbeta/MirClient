{*************************************************************************}
{ TAdvPreviewMenu component                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2005 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvPreviewMenuStylers;

interface

uses
  Graphics, Windows, Forms, Messages, Controls, Classes, SysUtils, AdvPreviewMenu,
  AdvStyleIF, AdvGlowButton, AdvGDIP;

type
  TPreviewMenuStyle = (psOffice2003Blue, psOffice2003Silver, psOffice2003Olive, psOffice2003Classic, psOffice2007Luna, psOffice2007Obsidian, psWindowsXP, psWhidbeyStyle, psCustom, psOfficeXP, psOffice2007Silver, psWindowsVista, psWindows7, psTerminal,
    psOffice2010Blue, psOffice2010Silver, psOffice2010Black,
    psWindows8, psOffice2013White, psOffice2013Lightgray, psOffice2013Gray,
    psWindows10, psOffice2016White, psOffice2016gray, psOffice2016Black);

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
  TAdvPreviewMenuOfficeStyler = class(TAdvCustomPreviewMenuStyler, ITMSStyle, IWinStyle)
  private
    FNotifierWnd: TNotifierWindow;
    FStyle: TPreviewMenuStyle;
    procedure SetStyle(const Value: TPreviewMenuStyle);
  protected
    procedure ThemeChanged(Sender: TObject);
    procedure ChangeStyle(AWin7: Boolean; AStyle: Integer);
    procedure ChangeMenu(AColor: TColor);
    procedure UpdateMenu;
    procedure HideMenu;
    procedure ShowMenuShortCuts;
    procedure HideMenuShortCuts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
  published
    //property AutoThemeAdapt;
    property FrameAppearance;
    property LeftFrameColor;
    property RightFrameColor;
    property MenuItemAppearance;
    property ButtonAppearance;
    property Style: TPreviewMenuStyle read FStyle write SetStyle;
  end;

  TAdvPreviewMenuFantasyStyler = class(TAdvCustomPreviewMenuStyler)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FrameAppearance;
    property LeftFrameColor;
    property RightFrameColor;
    property MenuItemAppearance;
    property ButtonAppearance;
  end;



implementation


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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function HTMLToRgb(color: tcolor): tcolor;
var
  r,g,b: integer;
begin
  r := (Color and $0000FF);
  g := (Color and $00FF00);
  b := (Color and $FF0000) shr 16;
  Result := b or g or (r shl 16);
end;

//------------------------------------------------------------------------------

procedure TAdvPreviewMenuOfficeStyler.ChangeMenu(AColor: TColor);
begin
//
end;

//------------------------------------------------------------------------------

procedure TAdvPreviewMenuOfficeStyler.ChangeStyle(AWin7: Boolean; AStyle: integer);
begin
  if (csLoading in ComponentState) then
    Exit;

  if AWin7 then
    Style := psWindows7
  else
    Style := psOffice2007Luna;
end;

//------------------------------------------------------------------------------

constructor TAdvPreviewMenuOfficeStyler.Create(AOwner: TComponent);
var
  ctrl: TComponent;
begin
  inherited;
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

destructor TAdvPreviewMenuOfficeStyler.Destroy;
begin
  inherited;
end;

procedure TAdvPreviewMenuOfficeStyler.HideMenu;
begin
//
end;

procedure TAdvPreviewMenuOfficeStyler.HideMenuShortCuts;
begin
//
end;

procedure TAdvPreviewMenuOfficeStyler.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    ThemeChanged(Self);
end;

procedure TAdvPreviewMenuOfficeStyler.ThemeChanged(Sender: TObject);
//var
  //eTheme: XPColorScheme;
begin
  //if not AutoThemeAdapt then
    //Exit;
end;

procedure TAdvPreviewMenuOfficeStyler.UpdateMenu;
begin
//
end;

procedure TAdvPreviewMenuOfficeStyler.SetComponentStyle(AStyle: TTMSStyle);
begin
//  Style := TPreviewMenuStyle(AStyle);

  case AStyle of
    tsOffice2003Blue: Style := psOffice2003Blue;
    tsOffice2003Silver: Style := psOffice2003Silver;
    tsOffice2003Olive: Style := psOffice2003Olive;
    tsOffice2003Classic: Style := psOffice2003Classic;
    tsOffice2007Luna: Style := psOffice2007Luna;
    tsOffice2007Obsidian: Style := psOffice2007Obsidian;
    tsWindowsXP: Style := psWindowsXP;
    tsWhidbey: Style := psWhidbeyStyle;
    tsOffice2007Silver: Style := psOffice2007Silver;
    tsWindowsVista: Style := psWindowsVista;
    tsWindows7: Style := psWindows7;
    tsWindows8: Style := psWindows8;
    tsWindows10: Style := psWindows10;
    tsTerminal: Style := psTerminal;
    tsOffice2010Blue: Style := psOffice2010Blue;
    tsOffice2010Silver: Style := psOffice2010Silver;
    tsOffice2010Black: Style := psOffice2010Black;
    tsOffice2013Gray: Style := psOffice2013Gray;
    tsOffice2013LightGray: Style := psOffice2013LightGray;
    tsOffice2013White: Style := psOffice2013White;
    tsOffice2016White: Style := psOffice2016White;
    tsOffice2016Gray: Style := psOffice2016Gray;
    tsOffice2016Black: Style := psOffice2016Black;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvPreviewMenuFantasyStyler }

constructor TAdvPreviewMenuFantasyStyler.Create(AOwner: TComponent);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvPreviewMenuOfficeStyler.SetStyle(
  const Value: TPreviewMenuStyle);
begin
  FStyle := Value;

  RoundedButtons := true;

  if not (FStyle in [psCustom]) then
  begin
        MenuItemAppearance.Color := clNone;
        MenuItemAppearance.ColorTo := clNone;
        MenuItemAppearance.ColorMirror := clNone;
        MenuItemAppearance.ColorMirrorTo := clNone;
        MenuItemAppearance.BorderColor := clNone;
        MenuItemAppearance.Gradient := ggVertical;
        MenuItemAppearance.GradientMirror := ggVertical;

        MenuItemAppearance.ColorDisabled := clNone;
        MenuItemAppearance.ColorDisabledTo := clNone;
        MenuItemAppearance.ColorMirrorDisabled := clNone;
        MenuItemAppearance.ColorMirrorDisabledTo := clNone;
        MenuItemAppearance.BorderColorDisabled := clNone;
        MenuItemAppearance.GradientDisabled := ggVertical;
        MenuItemAppearance.GradientMirrorDisabled := ggVertical;

        LeftFrameColor := clWhite;
        RightFrameColor := $00EEEAE9;

        FrameAppearance.Gradient := ggVertical;
        FrameAppearance.GradientMirror := ggVertical;
  end;

      if (FStyle in [psOffice2003Blue, psOffice2003Olive, psOffice2003Silver, psWhidbeyStyle]) then
    begin

      ButtonAppearance.ColorHot := $EBFDFF;
      ButtonAppearance.ColorHotTo := $ACECFF;
      ButtonAppearance.ColorMirrorHot := $59DAFF;
      ButtonAppearance.ColorMirrorHotTo := $A4E9FF;
      ButtonAppearance.BorderColorHot := $99CEDB;
      ButtonAppearance.GradientHot := ggVertical;
      ButtonAppearance.GradientMirrorHot := ggVertical;

      ButtonAppearance.ColorDown := $76AFF1;
      ButtonAppearance.ColorDownTo := $4190F3;
      ButtonAppearance.ColorMirrorDown := $0E72F1;
      ButtonAppearance.ColorMirrorDownTo := $4C9FFD;
      ButtonAppearance.BorderColorDown := $45667B;
      ButtonAppearance.GradientDown := ggVertical;
      ButtonAppearance.GradientMirrorDown := ggVertical;

      ButtonAppearance.ColorChecked := $B5DBFB;
      ButtonAppearance.ColorCheckedTo := $78C7FE;
      ButtonAppearance.ColorMirrorChecked := $9FEBFD;
      ButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
      ButtonAppearance.GradientChecked := ggVertical;
      ButtonAppearance.GradientMirrorChecked := ggVertical;

      MenuItemAppearance.ColorHot := $C2EEFF;
      MenuItemAppearance.ColorHotTo := $C2EEFF;
      MenuItemAppearance.ColorMirrorHot := $C2EEFF;
      MenuItemAppearance.ColorMirrorHotTo := $C2EEFF;
      MenuItemAppearance.BorderColorHot := $385D3F;
      MenuItemAppearance.GradientHot := ggVertical;
      MenuItemAppearance.GradientMirrorHot := ggVertical;

      MenuItemAppearance.ColorDown := $3E80FE;
      MenuItemAppearance.ColorDownTo := $3E80FE;
      MenuItemAppearance.ColorMirrorDown := $3E80FE;
      MenuItemAppearance.ColorMirrorDownTo := $3E80FE;
      MenuItemAppearance.BorderColorDown := $385D3F;
      MenuItemAppearance.GradientDown := ggVertical;
      MenuItemAppearance.GradientMirrorDown := ggVertical;

      MenuItemAppearance.ColorChecked := $6FC0FF;
      MenuItemAppearance.ColorCheckedTo := $6FC0FF;
      MenuItemAppearance.ColorMirrorChecked := $6FC0FF;
      MenuItemAppearance.ColorMirrorCheckedTo := $6FC0FF;
      MenuItemAppearance.BorderColorChecked := $385D3F;
      MenuItemAppearance.GradientChecked := ggVertical;
      MenuItemAppearance.GradientMirrorChecked := ggVertical;

      MenuItemAppearance.TextColor := clBlack;
      MenuItemAppearance.TextColorHot := clBlack;
      MenuItemAppearance.TextColorDown := clBlack;

      MenuItemAppearance.SubItemTitleFont.Style := [fsBold];
    end;

     if (FStyle in [psOffice2007Luna, psOffice2007Obsidian, psOffice2007Silver]) then
    begin
        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.ColorMirrorHot := $59DAFF;
        ButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        ButtonAppearance.BorderColorHot := $99CEDB;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $76AFF1;
        ButtonAppearance.ColorDownTo := $4190F3;
        ButtonAppearance.ColorMirrorDown := $0E72F1;
        ButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        ButtonAppearance.BorderColorDown := $45667B;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $B5DBFB;
        ButtonAppearance.ColorCheckedTo := $78C7FE;
        ButtonAppearance.ColorMirrorChecked := $9FEBFD;
        ButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        ButtonAppearance.BorderColorChecked := $45667B;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $EBFDFF;
        MenuItemAppearance.ColorHotTo := $ACECFF;
        MenuItemAppearance.ColorMirrorHot := $59DAFF;
        MenuItemAppearance.ColorMirrorHotTo := $A4E9FF;
        MenuItemAppearance.BorderColorHot := $99CEDB;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $76AFF1;
        MenuItemAppearance.ColorDownTo := $4190F3;
        MenuItemAppearance.ColorMirrorDown := $0E72F1;
        MenuItemAppearance.ColorMirrorDownTo := $4C9FFD;
        MenuItemAppearance.BorderColorDown := $45667B;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $B5DBFB;
        MenuItemAppearance.ColorCheckedTo := $78C7FE;
        MenuItemAppearance.ColorMirrorChecked := $9FEBFD;
        MenuItemAppearance.ColorMirrorCheckedTo := $56B4FE;
        MenuItemAppearance.BorderColorChecked := $45667B;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;
        MenuItemAppearance.SubItemTitleFont.Style := [fsBold];
    end;

  case FStyle of
  psOffice2003Blue:
    begin

        ButtonAppearance.Color := $EEDBC8;
        ButtonAppearance.ColorTo := $F6DDC9;
        ButtonAppearance.ColorMirror := $EDD4C0;
        ButtonAppearance.ColorMirrorTo := $F7E1D0;
        ButtonAppearance.BorderColor := $E0B99B;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        FrameAppearance.BorderColor := $385D3F;
        FrameAppearance.Color := $E7B38F;
        FrameAppearance.ColorMirror := $E7B38F;
        FrameAppearance.ColorMirrorTo := $F6F6F6;
        FrameAppearance.ColorTo := $E7B38F;

        RightFrameColor := $FDE9DB;

    end;
  psOffice2003Olive:
    begin

        ButtonAppearance.Color := $CFF0EA;
        ButtonAppearance.ColorTo := $CFF0EA;
        ButtonAppearance.ColorMirror := $CFF0EA;
        ButtonAppearance.ColorMirrorTo := $8CC0B1;
        ButtonAppearance.BorderColor := $8CC0B1;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        FrameAppearance.BorderColor := $5E8D75;
        FrameAppearance.Color := $9ACCBE;
        FrameAppearance.ColorMirror := $9ACCBE;
        FrameAppearance.ColorMirrorTo := $EDFFFF;
        FrameAppearance.ColorTo := $9ACCBE;

        RightFrameColor := $D0EDE8;

    end;

  psOffice2007Luna:
    begin
        ButtonAppearance.Color := $EEDBC8;
        ButtonAppearance.ColorTo := $F6DDC9;
        ButtonAppearance.ColorMirror := $EDD4C0;
        ButtonAppearance.ColorMirrorTo := $F7E1D0;
        ButtonAppearance.BorderColor := $E0B99B;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        MenuItemAppearance.TextColor := $6E1500;
        MenuItemAppearance.TextColorHot := $6E1500;
        MenuItemAppearance.TextColorDown := $6E1500;

        FrameAppearance.BorderColor := $00C9AF9C;
        FrameAppearance.Color := $00F5E1D1;
        FrameAppearance.ColorMirror := $00F5E1D1;
        FrameAppearance.ColorMirrorTo := $00E7CDB8;
        FrameAppearance.ColorTo := $00E7CDB8;

    end;

  psOffice2007Obsidian:
    begin
        ButtonAppearance.Color := HTMLToRgb($D6DEDF);
        ButtonAppearance.ColorTo := HTMLToRgb($DBE2E4);
        ButtonAppearance.ColorMirror := HTMLToRgb($CED5D7);
        ButtonAppearance.ColorMirrorTo := HTMLToRgb($E0E5E7);
        ButtonAppearance.BorderColor := HTMLToRgb($B2BCC0);
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        MenuItemAppearance.TextColor := $464646;
        MenuItemAppearance.TextColorHot := $464646;
        MenuItemAppearance.TextColorDown := $464646;

        FrameAppearance.BorderColor := $676768;
        FrameAppearance.Color := $524643;
        FrameAppearance.ColorMirror := $3D3D3D;
        FrameAppearance.ColorMirrorTo := $2F2F2F;
        FrameAppearance.ColorTo := $4A403C;
    end;

  psOffice2007Silver:
    begin
        ButtonAppearance.Color := $F5F0EB;
        ButtonAppearance.ColorTo := $F5F0EC;
        ButtonAppearance.ColorMirror := $F9F4F0;
        ButtonAppearance.ColorMirrorTo := $DBD2CB;
        ButtonAppearance.BorderColor := $9D948D;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        MenuItemAppearance.TextColor := $464646;
        MenuItemAppearance.TextColorHot := $464646;
        MenuItemAppearance.TextColorDown := $464646;

        FrameAppearance.BorderColor := $B4AEA9;
        FrameAppearance.Color := $D7D0CA;
        FrameAppearance.ColorMirror := $E6E2D9;
        FrameAppearance.ColorMirrorTo := $D0C9C3;
        FrameAppearance.ColorTo := $D8D1CC;
    end;

  psOffice2003Silver:
    begin
        ButtonAppearance.Color := $EDD4C0;
        ButtonAppearance.ColorTo := $00E6D8D8;
        ButtonAppearance.ColorMirror := $EDD4C0;
        ButtonAppearance.ColorMirrorTo := $C8B2B3;
        ButtonAppearance.BorderColor := $927476;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        FrameAppearance.BorderColor := $947C7C;
        FrameAppearance.Color := $C2A9AB;
        FrameAppearance.ColorMirror := $C2A9AB;
        FrameAppearance.ColorMirrorTo := $FFF9F9;
        FrameAppearance.ColorTo := $C2A9AB;

        RightFrameColor := $EEE4E4;

    end;

  psOfficeXP:
    begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := $C9D1D5;
        ButtonAppearance.ColorMirror := clWhite;
        ButtonAppearance.ColorMirrorTo := $C9D1D5;
        ButtonAppearance.BorderColor := clBlack;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.ColorMirrorHot := $59DAFF;
        ButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        ButtonAppearance.BorderColorHot := $99CEDB;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $76AFF1;
        ButtonAppearance.ColorDownTo := $4190F3;
        ButtonAppearance.ColorMirrorDown := $0E72F1;
        ButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        ButtonAppearance.BorderColorDown := $45667B;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $B5DBFB;
        ButtonAppearance.ColorCheckedTo := $78C7FE;
        ButtonAppearance.ColorMirrorChecked := $9FEBFD;
        ButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $EFD3C6;
        MenuItemAppearance.ColorHotTo := $EFD3C6;
        MenuItemAppearance.ColorMirrorHot := $EFD3C6;
        MenuItemAppearance.ColorMirrorHotTo := $EFD3C6;
        MenuItemAppearance.BorderColorHot := clHighlight;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $EFD3C6;
        MenuItemAppearance.ColorDownTo := $EFD3C6;
        MenuItemAppearance.ColorMirrorDown := $EFD3C6;
        MenuItemAppearance.ColorMirrorDownTo := $EFD3C6;
        MenuItemAppearance.BorderColorDown := clHighlight;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := clHighlight;
        MenuItemAppearance.ColorCheckedTo := clHighlight;
        MenuItemAppearance.ColorMirrorChecked := clHighlight;
        MenuItemAppearance.ColorMirrorCheckedTo := clHighlight;
        MenuItemAppearance.BorderColorChecked := clHighlight;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $7A868A;
        FrameAppearance.Color := clBtnFace;
        FrameAppearance.ColorMirror := clBtnFace;
        FrameAppearance.ColorMirrorTo := clBtnFace;
        FrameAppearance.ColorTo := clBtnFace;

        RightFrameColor := clBtnFace;        

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;

    end;

  psWindowsXP, psOffice2003Classic:
    begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := HTMLToRgb($DCD8B9);
        ButtonAppearance.ColorMirror := HTMLToRgb($DCD8B9);
        ButtonAppearance.ColorMirrorTo := HTMLToRgb($DCD8B9);
        ButtonAppearance.BorderColor := HTMLToRgb($DCD8B9);
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $EFD3C6;
        ButtonAppearance.ColorHotTo := $EFD3C6;
        ButtonAppearance.ColorMirrorHot := $EFD3C6;
        ButtonAppearance.ColorMirrorHotTo := $EFD3C6;
        ButtonAppearance.BorderColorHot := clHighlight;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $B59284;
        ButtonAppearance.ColorDownTo := $B59284;
        ButtonAppearance.ColorMirrorDown := $B59284;
        ButtonAppearance.ColorMirrorDownTo := $B59284;
        ButtonAppearance.BorderColorDown := clHighlight;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := HTMLToRgb($DCD8B9);
        ButtonAppearance.ColorCheckedTo := HTMLToRgb($DCD8B9);
        ButtonAppearance.ColorMirrorChecked := HTMLToRgb($DCD8B9);
        ButtonAppearance.ColorMirrorCheckedTo := HTMLToRgb($DCD8B9);
        ButtonAppearance.BorderColorChecked := clBlack;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $C56A31;
        MenuItemAppearance.ColorHotTo := $C56A31;
        MenuItemAppearance.ColorMirrorHot := $C56A31;
        MenuItemAppearance.ColorMirrorHotTo := $C56A31;
        MenuItemAppearance.BorderColorHot := clNone;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $C56A31;
        MenuItemAppearance.ColorDownTo := $C56A31;
        MenuItemAppearance.ColorMirrorDown := $C56A31;
        MenuItemAppearance.ColorMirrorDownTo := $C56A31;
        MenuItemAppearance.BorderColorDown := clNone;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := clNone;
        MenuItemAppearance.ColorCheckedTo := clNone;
        MenuItemAppearance.ColorMirrorChecked := clNone;
        MenuItemAppearance.ColorMirrorCheckedTo := clNone;
        MenuItemAppearance.BorderColorChecked := clNone;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $C55C22;
        FrameAppearance.Color := $EC9247;
        FrameAppearance.ColorMirror := $CE6616;
        FrameAppearance.ColorMirrorTo := $CE6616;
        FrameAppearance.ColorTo := $CE6616;

        RightFrameColor := $FAE5D3;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clWhite;
        MenuItemAppearance.TextColorDown := clWhite;

    end;

  psWhidbeyStyle:
    begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := $DFEDF0;
        ButtonAppearance.ColorMirror := $DFEDF0;
        ButtonAppearance.ColorMirrorTo := $DFEDF0;
        ButtonAppearance.BorderColor := $99A8AC;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        FrameAppearance.BorderColor := $99A8AC;
        FrameAppearance.Color := $A8C0C1;
        FrameAppearance.ColorMirror := $A8C0C1;
        FrameAppearance.ColorMirrorTo := $DFEDF0;//$FBFEFE;
        FrameAppearance.ColorTo := $A8C0C1;

        RightFrameColor := $DFEDF0;

    end;
    psWindowsVista:
    begin
        ButtonAppearance.Color := $FDF8F1;
        ButtonAppearance.ColorTo := $FDF8F1;
        ButtonAppearance.ColorMirror := $FCEFD5;
        ButtonAppearance.ColorMirrorTo := $FDF8F1;
        ButtonAppearance.BorderColor := $FDDE99;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := clNone;
        ButtonAppearance.ColorHotTo := $FFFAF0;
        ButtonAppearance.ColorMirrorHot := $FFFAF0;
        ButtonAppearance.ColorMirrorHotTo := $FFFAF0;
        ButtonAppearance.BorderColorHot := $FCF2DA;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $FCEFD5;
        ButtonAppearance.ColorDownTo := $FCEFD5;
        ButtonAppearance.ColorMirrorDown := $FDF4E3;
        ButtonAppearance.ColorMirrorDownTo := $FDF4E3;
        ButtonAppearance.BorderColorDown := $FEDF9A;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $FCEFD5;
        ButtonAppearance.ColorCheckedTo := $FAE9C6;
        ButtonAppearance.ColorMirrorChecked := $F7DAA2;
        ButtonAppearance.ColorMirrorCheckedTo := $FBEDD3;
        ButtonAppearance.BorderColorChecked := $FEDF9A;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $FFFDF9;
        MenuItemAppearance.ColorHotTo := $FFFDF9;
        MenuItemAppearance.ColorMirrorHot := $FFFAF0;
        MenuItemAppearance.ColorMirrorHotTo := $FFFAF0;
        MenuItemAppearance.BorderColorHot := $FCF2DA;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $FEF9F0;
        MenuItemAppearance.ColorDownTo := $FEF9F0;
        MenuItemAppearance.ColorMirrorDown := $FDF0D7;
        MenuItemAppearance.ColorMirrorDownTo := $FDF0D7;
        MenuItemAppearance.BorderColorDown := $FEDF9A;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $FBEDD3;
        MenuItemAppearance.ColorCheckedTo := $FBEDD3;
        MenuItemAppearance.ColorMirrorChecked := $FAE9C6;
        MenuItemAppearance.ColorMirrorCheckedTo := $FAE9C6;
        MenuItemAppearance.BorderColorChecked := clNone;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $FDDE99;
        FrameAppearance.Color := $FCEFD5;
        FrameAppearance.ColorMirror := $FDF4E3;
        FrameAppearance.ColorMirrorTo := $FDF4E3;
        FrameAppearance.ColorTo := $FCEFD5;

        RightFrameColor := $00EEEAE9;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;

    end;
    psWindows7:
    begin
      if IsWin7 then
      begin
        ButtonAppearance.Color := RGB(246, 248, 251);
        ButtonAppearance.ColorTo := RGB(230, 238, 245);
        ButtonAppearance.ColorMirror := RGB(223, 232, 241);
        ButtonAppearance.ColorMirrorTo := RGB(225, 234, 244);
        ButtonAppearance.BorderColor := RGB(176, 190, 210);
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := RGB(253, 240, 225);
        ButtonAppearance.ColorHotTo := RGB(255, 206, 105);
        ButtonAppearance.ColorMirrorHot := RGB(255, 255, 203);
        ButtonAppearance.ColorMirrorHotTo := RGB(255, 206, 105);
        ButtonAppearance.BorderColorHot := RGB(255, 183, 0);
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := RGB(238, 201, 139);
        ButtonAppearance.ColorDownTo := RGB(245, 199, 122);
        ButtonAppearance.ColorMirrorDown := RGB(243, 235, 147);
        ButtonAppearance.ColorMirrorDownTo := RGB(245, 187, 86);
        ButtonAppearance.BorderColorDown := RGB(194, 155, 41);
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := RGB(246, 205, 152);
        ButtonAppearance.ColorCheckedTo := RGB(252, 208, 143);
        ButtonAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        ButtonAppearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
        ButtonAppearance.BorderColorChecked:= RGB(194, 146, 59);
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := clNone;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := clNone;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled:= clgray;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := RGB(253, 240, 225);
        MenuItemAppearance.ColorHotTo := RGB(255, 206, 105);
        MenuItemAppearance.ColorMirrorHot := RGB(255, 206, 105);
        MenuItemAppearance.ColorMirrorHotTo := RGB(255, 255, 203);
        MenuItemAppearance.BorderColorHot := RGB(255, 183, 0);
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := RGB(246, 205, 152);
        MenuItemAppearance.ColorCheckedTo := RGB(252, 208, 143);
        MenuItemAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        MenuItemAppearance.ColorMirrorCheckedTo := RGB(194, 146, 59);
        MenuItemAppearance.BorderColorChecked:= RGB(255, 215, 131);
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := RGB(238, 201, 139);
        MenuItemAppearance.ColorDownTo := RGB(245, 199, 122);
        MenuItemAppearance.ColorMirrorDown := RGB(245, 187, 86);
        MenuItemAppearance.ColorMirrorDownTo := RGB(243, 235, 147);
        MenuItemAppearance.BorderColorDown := RGB(194, 155, 41);
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;
        MenuItemAppearance.Rounded := true;

        FrameAppearance.BorderColor := RGB(132, 146, 166);
        FrameAppearance.Color := RGB(252, 254, 255);
        FrameAppearance.ColorTo := RGB(232, 241, 251);
        FrameAppearance.ColorMirror := RGB(220, 230, 244);
        FrameAppearance.ColorMirrorTo := RGB(221, 233, 247);

        RightFrameColor := RGB(239, 245, 250);

        MenuItemAppearance.TextColor := RGB(37, 66, 100);
        MenuItemAppearance.TextColorHot := RGB(37, 66, 100);
        MenuItemAppearance.TextColorDown := RGB(37, 66, 100);
      end
      else
      begin
        ButtonAppearance.Color := $FCEBDC;
        ButtonAppearance.ColorTo := $FCDBC1;
        ButtonAppearance.ColorMirror := $FCDBC1;
        ButtonAppearance.ColorMirrorTo := $FCDBC1;
        ButtonAppearance.BorderColor := $CEA27D;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $FDFBFA;
        ButtonAppearance.ColorHotTo := $FDF3EB;
        ButtonAppearance.ColorMirrorHot := $FDF3EB;
        ButtonAppearance.ColorMirrorHotTo := $FDFBFA;
        ButtonAppearance.BorderColorHot := $FBD6B8;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $FCEBDC;
        ButtonAppearance.ColorDownTo := $FCDBC1;
        ButtonAppearance.ColorMirrorDown := $FCDBC1;
        ButtonAppearance.ColorMirrorDownTo := $FCEBDC;
        ButtonAppearance.BorderColorDown := $CEA27D;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $FDFBFA;
        ButtonAppearance.ColorCheckedTo := $FDF3EB;
        ButtonAppearance.ColorMirrorChecked := $FCEBDC;
        ButtonAppearance.ColorMirrorCheckedTo := $FCEBDC;
        ButtonAppearance.BorderColorChecked := clHighlight;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $FDFBFA;
        MenuItemAppearance.ColorHotTo := $FDFBFA;
        MenuItemAppearance.ColorMirrorHot := $FDF3EB;
        MenuItemAppearance.ColorMirrorHotTo := $FDF3EB;
        MenuItemAppearance.BorderColorHot := $FBD6B8;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $FCEBDC;
        MenuItemAppearance.ColorDownTo := $FCEBDC;
        MenuItemAppearance.ColorMirrorDown := $FCDBC1;
        MenuItemAppearance.ColorMirrorDownTo := $FCDBC1;
        MenuItemAppearance.BorderColorDown := $CEA27D;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $FDFBFA;
        MenuItemAppearance.ColorCheckedTo := $FDFBFA;
        MenuItemAppearance.ColorMirrorChecked := $FDF3EB;
        MenuItemAppearance.ColorMirrorCheckedTo := $FDF3EB;
        MenuItemAppearance.BorderColorChecked := clNone;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $CEA27D;
        FrameAppearance.Color := $FCEBDC;
        FrameAppearance.ColorMirror := $FCDBC1;
        FrameAppearance.ColorMirrorTo := $FCEBDC;
        FrameAppearance.ColorTo := $FCDBC1;

        RightFrameColor := $00EEEAE9;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;
      end;
    end;
    psWindows8, psWindows10:
    begin
      if IsWin7 then
      begin
        ButtonAppearance.Color := $F8F7F6; // RGB(246, 248, 251);
        ButtonAppearance.ColorTo := $F8F7F6;
        ButtonAppearance.ColorMirror := $F8F7F6;
        ButtonAppearance.ColorMirrorTo := $F8F7F6;
        ButtonAppearance.BorderColor := $F8F7F6;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $00FCF4ED;
        ButtonAppearance.ColorHotTo := $00FCF4ED;
        ButtonAppearance.ColorMirrorHot := $00FCF4ED;
        ButtonAppearance.ColorMirrorHotTo := $00FCF4ED;
        ButtonAppearance.BorderColorHot := $FDD2A8;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $00F7E0C9;
        ButtonAppearance.ColorDownTo := $00F7E0C9;
        ButtonAppearance.ColorMirrorDown := $00F7E0C9;
        ButtonAppearance.ColorMirrorDownTo := $00F7E0C9;
        ButtonAppearance.BorderColorDown := $00E4A262;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := RGB(246, 205, 152);
        ButtonAppearance.ColorCheckedTo := RGB(252, 208, 143);
        ButtonAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        ButtonAppearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
        ButtonAppearance.BorderColorChecked:= RGB(194, 146, 59);
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := clNone;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := clNone;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled:= clGray;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $00FCF4ED;
        MenuItemAppearance.ColorHotTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHot := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHotTo := $00FCF4ED;
        MenuItemAppearance.BorderColorHot := $FDD2A8;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := RGB(246, 205, 152);
        MenuItemAppearance.ColorCheckedTo := RGB(252, 208, 143);
        MenuItemAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        MenuItemAppearance.ColorMirrorCheckedTo := RGB(194, 146, 59);
        MenuItemAppearance.BorderColorChecked:= RGB(255, 215, 131);
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $00FCF4ED;
        MenuItemAppearance.ColorDownTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDown := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDownTo := $00FCF4ED;
        MenuItemAppearance.BorderColorDown := $FDD2A8;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;
        MenuItemAppearance.Rounded := false;
        FrameAppearance.BorderColor := clBlack;
        FrameAppearance.Color := $F8F7F6;
        FrameAppearance.ColorTo := $F8F7F6;
        FrameAppearance.ColorMirror := $F8F7F6;
        FrameAppearance.ColorMirrorTo := $F8F7F6;

        RightFrameColor := clWhite;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := RGB(37, 66, 100);
        MenuItemAppearance.TextColorHot := RGB(37, 66, 100);
        MenuItemAppearance.TextColorDown := RGB(37, 66, 100);
      end
      else
      begin
        ButtonAppearance.Color := $FCEBDC;
        ButtonAppearance.ColorTo := $FCDBC1;
        ButtonAppearance.ColorMirror := $FCDBC1;
        ButtonAppearance.ColorMirrorTo := $FCDBC1;
        ButtonAppearance.BorderColor := $CEA27D;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $FDFBFA;
        ButtonAppearance.ColorHotTo := $FDF3EB;
        ButtonAppearance.ColorMirrorHot := $FDF3EB;
        ButtonAppearance.ColorMirrorHotTo := $FDFBFA;
        ButtonAppearance.BorderColorHot := $FBD6B8;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $FCEBDC;
        ButtonAppearance.ColorDownTo := $FCDBC1;
        ButtonAppearance.ColorMirrorDown := $FCDBC1;
        ButtonAppearance.ColorMirrorDownTo := $FCEBDC;
        ButtonAppearance.BorderColorDown := $CEA27D;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $FDFBFA;
        ButtonAppearance.ColorCheckedTo := $FDF3EB;
        ButtonAppearance.ColorMirrorChecked := $FCEBDC;
        ButtonAppearance.ColorMirrorCheckedTo := $FCEBDC;
        ButtonAppearance.BorderColorChecked := clHighlight;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $FDFBFA;
        MenuItemAppearance.ColorHotTo := $FDFBFA;
        MenuItemAppearance.ColorMirrorHot := $FDF3EB;
        MenuItemAppearance.ColorMirrorHotTo := $FDF3EB;
        MenuItemAppearance.BorderColorHot := $FBD6B8;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $FCEBDC;
        MenuItemAppearance.ColorDownTo := $FCEBDC;
        MenuItemAppearance.ColorMirrorDown := $FCDBC1;
        MenuItemAppearance.ColorMirrorDownTo := $FCDBC1;
        MenuItemAppearance.BorderColorDown := $CEA27D;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $FDFBFA;
        MenuItemAppearance.ColorCheckedTo := $FDFBFA;
        MenuItemAppearance.ColorMirrorChecked := $FDF3EB;
        MenuItemAppearance.ColorMirrorCheckedTo := $FDF3EB;
        MenuItemAppearance.BorderColorChecked := clNone;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $CEA27D;
        FrameAppearance.Color := $FCEBDC;
        FrameAppearance.ColorMirror := $FCDBC1;
        FrameAppearance.ColorMirrorTo := $FCEBDC;
        FrameAppearance.ColorTo := $FCDBC1;

        RightFrameColor := $00EEEAE9;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;
      end;
    end;
    psOffice2013White:
    begin
        ButtonAppearance.Color := clWhite; // RGB(246, 248, 251);
        ButtonAppearance.ColorTo := clWhite;
        ButtonAppearance.ColorMirror := clWhite;
        ButtonAppearance.ColorMirrorTo := clWhite;
        ButtonAppearance.BorderColor := clWhite;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $00FCF4ED;
        ButtonAppearance.ColorHotTo := $00FCF4ED;
        ButtonAppearance.ColorMirrorHot := $00FCF4ED;
        ButtonAppearance.ColorMirrorHotTo := $00FCF4ED;
        ButtonAppearance.BorderColorHot := $00FCF4ED;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $00F7E0C9;
        ButtonAppearance.ColorDownTo := $00F7E0C9;
        ButtonAppearance.ColorMirrorDown := $00F7E0C9;
        ButtonAppearance.ColorMirrorDownTo := $00F7E0C9;
        ButtonAppearance.BorderColorDown := $00F7E0C9;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := RGB(246, 205, 152);
        ButtonAppearance.ColorCheckedTo := RGB(252, 208, 143);
        ButtonAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        ButtonAppearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
        ButtonAppearance.BorderColorChecked:= RGB(194, 146, 59);
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := clNone;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := clNone;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled:= clGray;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $00FCF4ED;
        MenuItemAppearance.ColorHotTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHot := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHotTo := $00FCF4ED;
        MenuItemAppearance.BorderColorHot := $00FCF4ED;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := RGB(246, 205, 152);
        MenuItemAppearance.ColorCheckedTo := RGB(252, 208, 143);
        MenuItemAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        MenuItemAppearance.ColorMirrorCheckedTo := RGB(194, 146, 59);
        MenuItemAppearance.BorderColorChecked:= RGB(255, 215, 131);
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $00FCF4ED;
        MenuItemAppearance.ColorDownTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDown := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDownTo := $00FCF4ED;
        MenuItemAppearance.BorderColorDown := $FDD2A8;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;
        MenuItemAppearance.Rounded := false;
        FrameAppearance.BorderColor := clBlack;
        FrameAppearance.Color := clWhite;
        FrameAppearance.ColorTo := clWhite;
        FrameAppearance.ColorMirror := clWhite;
        FrameAppearance.ColorMirrorTo := clWhite;

        RightFrameColor := clWhite;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := RGB(37, 66, 100);
        MenuItemAppearance.TextColorHot := RGB(37, 66, 100);
        MenuItemAppearance.TextColorDown := RGB(37, 66, 100);
    end;
    psOffice2013Lightgray:
    begin
        ButtonAppearance.Color := clWhite; // RGB(246, 248, 251);
        ButtonAppearance.ColorTo := clWhite;
        ButtonAppearance.ColorMirror := clWhite;
        ButtonAppearance.ColorMirrorTo := clWhite;
        ButtonAppearance.BorderColor := clWhite;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $00FCF4ED;
        ButtonAppearance.ColorHotTo := $00FCF4ED;
        ButtonAppearance.ColorMirrorHot := $00FCF4ED;
        ButtonAppearance.ColorMirrorHotTo := $00FCF4ED;
        ButtonAppearance.BorderColorHot := $00FCF4ED;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $00F7E0C9;
        ButtonAppearance.ColorDownTo := $00F7E0C9;
        ButtonAppearance.ColorMirrorDown := $00F7E0C9;
        ButtonAppearance.ColorMirrorDownTo := $00F7E0C9;
        ButtonAppearance.BorderColorDown := $00F7E0C9;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := RGB(246, 205, 152);
        ButtonAppearance.ColorCheckedTo := RGB(252, 208, 143);
        ButtonAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        ButtonAppearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
        ButtonAppearance.BorderColorChecked:= RGB(194, 146, 59);
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := clNone;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := clNone;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled:= clGray;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $00FCF4ED;
        MenuItemAppearance.ColorHotTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHot := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHotTo := $00FCF4ED;
        MenuItemAppearance.BorderColorHot := $00FCF4ED;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := RGB(246, 205, 152);
        MenuItemAppearance.ColorCheckedTo := RGB(252, 208, 143);
        MenuItemAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        MenuItemAppearance.ColorMirrorCheckedTo := RGB(194, 146, 59);
        MenuItemAppearance.BorderColorChecked:= RGB(255, 215, 131);
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $00FCF4ED;
        MenuItemAppearance.ColorDownTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDown := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDownTo := $00FCF4ED;
        MenuItemAppearance.BorderColorDown := $FDD2A8;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;
        MenuItemAppearance.Rounded := false;
        FrameAppearance.BorderColor := clBlack;
        FrameAppearance.Color := $DEDEDE;
        FrameAppearance.ColorTo := $DEDEDE;
        FrameAppearance.ColorMirror := $DEDEDE;
        FrameAppearance.ColorMirrorTo := $DEDEDE;

        RightFrameColor := $DEDEDE;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := RGB(37, 66, 100);
        MenuItemAppearance.TextColorHot := RGB(37, 66, 100);
        MenuItemAppearance.TextColorDown := RGB(37, 66, 100);
    end;
    psOffice2013Gray:
    begin
        ButtonAppearance.Color := clWhite; // RGB(246, 248, 251);
        ButtonAppearance.ColorTo := clWhite;
        ButtonAppearance.ColorMirror := clWhite;
        ButtonAppearance.ColorMirrorTo := clWhite;
        ButtonAppearance.BorderColor := clWhite;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $00FCF4ED;
        ButtonAppearance.ColorHotTo := $00FCF4ED;
        ButtonAppearance.ColorMirrorHot := $00FCF4ED;
        ButtonAppearance.ColorMirrorHotTo := $00FCF4ED;
        ButtonAppearance.BorderColorHot := $00FCF4ED;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $00F7E0C9;
        ButtonAppearance.ColorDownTo := $00F7E0C9;
        ButtonAppearance.ColorMirrorDown := $00F7E0C9;
        ButtonAppearance.ColorMirrorDownTo := $00F7E0C9;
        ButtonAppearance.BorderColorDown := $00F7E0C9;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := RGB(246, 205, 152);
        ButtonAppearance.ColorCheckedTo := RGB(252, 208, 143);
        ButtonAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        ButtonAppearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
        ButtonAppearance.BorderColorChecked:= RGB(194, 146, 59);
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := clNone;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := clNone;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled:= clGray;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $00FCF4ED;
        MenuItemAppearance.ColorHotTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHot := $00FCF4ED;
        MenuItemAppearance.ColorMirrorHotTo := $00FCF4ED;
        MenuItemAppearance.BorderColorHot := $00FCF4ED;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := RGB(246, 205, 152);
        MenuItemAppearance.ColorCheckedTo := RGB(252, 208, 143);
        MenuItemAppearance.ColorMirrorChecked := RGB(255, 196, 94);
        MenuItemAppearance.ColorMirrorCheckedTo := RGB(194, 146, 59);
        MenuItemAppearance.BorderColorChecked:= RGB(255, 215, 131);
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $00FCF4ED;
        MenuItemAppearance.ColorDownTo := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDown := $00FCF4ED;
        MenuItemAppearance.ColorMirrorDownTo := $00FCF4ED;
        MenuItemAppearance.BorderColorDown := $FDD2A8;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;
        MenuItemAppearance.Rounded := false;
        FrameAppearance.BorderColor := clBlack;
        FrameAppearance.Color := $F1F1F1;
        FrameAppearance.ColorTo := $F1F1F1;
        FrameAppearance.ColorMirror := $F1F1F1;
        FrameAppearance.ColorMirrorTo := $F1F1F1;

        RightFrameColor := $F1F1F1;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := RGB(37, 66, 100);
        MenuItemAppearance.TextColorHot := RGB(37, 66, 100);
        MenuItemAppearance.TextColorDown := RGB(37, 66, 100);
    end;
    psOffice2016White:
    begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.ColorMirror := clWhite;
        ButtonAppearance.ColorMirrorTo := clNone;
        ButtonAppearance.BorderColor := $D4D4D4;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $F2E1D5;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.ColorMirrorHot := $F2E1D5;
        ButtonAppearance.ColorMirrorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $F2E1D5;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $E3BDA3;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.ColorMirrorDown := $E3BDA3;
        ButtonAppearance.ColorMirrorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E3BDA3;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $F2D5C2;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.ColorMirrorChecked := $F2D5C2;
        ButtonAppearance.ColorMirrorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $F2D5C2;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := clWhite;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := clWhite;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $D4D4D4;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $F2E1D5;
        MenuItemAppearance.ColorHotTo := $F2E1D5;
        MenuItemAppearance.ColorMirrorHot := $F2E1D5;
        MenuItemAppearance.ColorMirrorHotTo := $F2E1D5;
        MenuItemAppearance.BorderColorHot := $F2E1D5;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := $F2D5C2;
        MenuItemAppearance.ColorCheckedTo := $F2D5C2;
        MenuItemAppearance.ColorMirrorChecked := $F2D5C2;
        MenuItemAppearance.ColorMirrorCheckedTo := $F2D5C2;
        MenuItemAppearance.BorderColorChecked:= $F2D5C2;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $E3BDA3;
        MenuItemAppearance.ColorDownTo := $E3BDA3;
        MenuItemAppearance.ColorMirrorDown := $E3BDA3;
        MenuItemAppearance.ColorMirrorDownTo := $E3BDA3;
        MenuItemAppearance.BorderColorDown := $E3BDA3;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;
        MenuItemAppearance.Rounded := false;

        FrameAppearance.BorderColor := $D4D4D4;
        FrameAppearance.Color := clWhite;
        FrameAppearance.ColorTo := clWhite;
        FrameAppearance.ColorMirror := clWhite;
        FrameAppearance.ColorMirrorTo := clWhite;

        RightFrameColor := clWhite;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := $505050;
        MenuItemAppearance.TextColorHot := $505050;
        MenuItemAppearance.TextColorDown := $505050;
    end;
    psOffice2016Gray:
    begin
        ButtonAppearance.Color := $B2B2B2;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.ColorMirror := $B2B2B2;
        ButtonAppearance.ColorMirrorTo := clNone;
        ButtonAppearance.BorderColor := $444444;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $F2E1D5;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.ColorMirrorHot := $F2E1D5;
        ButtonAppearance.ColorMirrorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $F2E1D5;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $E3BDA3;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.ColorMirrorDown := $E3BDA3;
        ButtonAppearance.ColorMirrorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E3BDA3;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $F2D5C2;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.ColorMirrorChecked := $F2D5C2;
        ButtonAppearance.ColorMirrorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $F2D5C2;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := $B2B2B2;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := $B2B2B2;
        ButtonAppearance.ColorMirrorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $444444;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $F2E1D5;
        MenuItemAppearance.ColorHotTo := $F2E1D5;
        MenuItemAppearance.ColorMirrorHot := $F2E1D5;
        MenuItemAppearance.ColorMirrorHotTo := $F2E1D5;
        MenuItemAppearance.BorderColorHot := $F2E1D5;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := $F2D5C2;
        MenuItemAppearance.ColorCheckedTo := $F2D5C2;
        MenuItemAppearance.ColorMirrorChecked := $F2D5C2;
        MenuItemAppearance.ColorMirrorCheckedTo := $F2D5C2;
        MenuItemAppearance.BorderColorChecked:= $F2D5C2;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $E3BDA3;
        MenuItemAppearance.ColorDownTo := $E3BDA3;
        MenuItemAppearance.ColorMirrorDown := $E3BDA3;
        MenuItemAppearance.ColorMirrorDownTo := $E3BDA3;
        MenuItemAppearance.BorderColorDown := $E3BDA3;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.Rounded := false;
        FrameAppearance.BorderColor := $444444;
        FrameAppearance.Color := $B2B2B2;
        FrameAppearance.ColorTo := $B2B2B2;
        FrameAppearance.ColorMirror := $B2B2B2;
        FrameAppearance.ColorMirrorTo := $B2B2B2;

        RightFrameColor := $B2B2B2;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := $424242;
        MenuItemAppearance.TextColorHot := $424242;
        MenuItemAppearance.TextColorDown := $424242;
    end;
    psOffice2016Black:
    begin
        ButtonAppearance.Color := $363636;
        ButtonAppearance.ColorTo := $363636;
        ButtonAppearance.ColorMirror := $363636;
        ButtonAppearance.ColorMirrorTo := $363636;
        ButtonAppearance.BorderColor := $444444;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $6A6A6A;
        ButtonAppearance.ColorHotTo := $6A6A6A;
        ButtonAppearance.ColorMirrorHot := $6A6A6A;
        ButtonAppearance.ColorMirrorHotTo := $6A6A6A;
        ButtonAppearance.BorderColorHot := $6A6A6A;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $444444;
        ButtonAppearance.ColorDownTo := $444444;
        ButtonAppearance.ColorMirrorDown := $444444;
        ButtonAppearance.ColorMirrorDownTo := $444444;
        ButtonAppearance.BorderColorDown := $444444;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $575757;
        ButtonAppearance.ColorCheckedTo := $575757;
        ButtonAppearance.ColorMirrorChecked := $575757;
        ButtonAppearance.ColorMirrorCheckedTo := $575757;
        ButtonAppearance.BorderColorChecked := $575757;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        ButtonAppearance.ColorDisabled := $363636;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.ColorMirrorDisabled := $363636;
        ButtonAppearance.ColorMirrorDownTo := clNone;
        ButtonAppearance.BorderColorDisabled := $444444;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $6A6A6A;
        MenuItemAppearance.ColorHotTo := $6A6A6A;
        MenuItemAppearance.ColorMirrorHot := $6A6A6A;
        MenuItemAppearance.ColorMirrorHotTo := $6A6A6A;
        MenuItemAppearance.BorderColorHot := $6A6A6A;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorChecked := $575757;
        MenuItemAppearance.ColorCheckedTo := $575757;
        MenuItemAppearance.ColorMirrorChecked := $575757;
        MenuItemAppearance.ColorMirrorCheckedTo := $575757;
        MenuItemAppearance.BorderColorChecked:= $575757;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorDown := $444444;
        MenuItemAppearance.ColorDownTo := $444444;
        MenuItemAppearance.ColorMirrorDown := $444444;
        MenuItemAppearance.ColorMirrorDownTo := $444444;
        MenuItemAppearance.BorderColorDown := $444444;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.Rounded := false;
        FrameAppearance.BorderColor := $4E4E4E;
        FrameAppearance.Color := $363636;
        FrameAppearance.ColorTo := $363636;
        FrameAppearance.ColorMirror := $363636;
        FrameAppearance.ColorMirrorTo := $363636;

        RightFrameColor := $363636;

        RoundedButtons := false;

        MenuItemAppearance.TextColor := $A6A6A6;
        MenuItemAppearance.TextColorHot := $A6A6A6;
        MenuItemAppearance.TextColorDown := $A6A6A6;
    end;
    psTerminal:
    begin
        ButtonAppearance.Color := clbtnFace;
        ButtonAppearance.ColorTo := clbtnFace;
        ButtonAppearance.ColorMirror := clbtnFace;
        ButtonAppearance.ColorMirrorTo := clbtnFace;
        ButtonAppearance.BorderColor := clGray;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := clSilver;
        ButtonAppearance.ColorHotTo := clSilver;
        ButtonAppearance.ColorMirrorHot := clSilver;
        ButtonAppearance.ColorMirrorHotTo := clSilver;
        ButtonAppearance.BorderColorHot := clGray;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := clHighlight;
        ButtonAppearance.ColorDownTo := clHighlight;
        ButtonAppearance.ColorMirrorDown := clHighlight;
        ButtonAppearance.ColorMirrorDownTo := clHighlight;
        ButtonAppearance.BorderColorDown := clHighlight;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := clGray;
        ButtonAppearance.ColorCheckedTo := clGray;
        ButtonAppearance.ColorMirrorChecked := clGray;
        ButtonAppearance.ColorMirrorCheckedTo := clGray;
        ButtonAppearance.BorderColorChecked := clGray;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := clSilver;
        MenuItemAppearance.ColorHotTo := clSilver;
        MenuItemAppearance.ColorMirrorHot := clSilver;
        MenuItemAppearance.ColorMirrorHotTo := clSilver;
        MenuItemAppearance.BorderColorHot := clGray;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := clHighLight;
        MenuItemAppearance.ColorDownTo := clHighLight;
        MenuItemAppearance.ColorMirrorDown := clHighLight;
        MenuItemAppearance.ColorMirrorDownTo := clHighLight;
        MenuItemAppearance.BorderColorDown := clGray;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := clNone;
        MenuItemAppearance.ColorCheckedTo := clNone;
        MenuItemAppearance.ColorMirrorChecked := clNone;
        MenuItemAppearance.ColorMirrorCheckedTo := clNone;
        MenuItemAppearance.BorderColorChecked := clGray;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := clGray;
        FrameAppearance.Color := clBtnFace;
        FrameAppearance.ColorMirror := clBtnFace;
        FrameAppearance.ColorMirrorTo := clBtnFace;
        FrameAppearance.ColorTo := clBtnFace;

        RightFrameColor := clBtnFace;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clWhite;
    end;
    psOffice2010Blue:
    begin
        ButtonAppearance.Color := $FDF6EF;
        ButtonAppearance.ColorTo := $F0DAC7;
        ButtonAppearance.ColorMirror := $F0DAC7;
        ButtonAppearance.ColorMirrorTo := $F0DAC7;
        ButtonAppearance.BorderColor := $C7B29F;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $8AE3FD;
        ButtonAppearance.ColorHotTo := $D9F9FD;
        ButtonAppearance.ColorMirrorHot := $D9F9FD;
        ButtonAppearance.ColorMirrorHotTo := $8AE3FD;
        ButtonAppearance.BorderColorHot := $58CAF1;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $6CD0FF;
        ButtonAppearance.ColorDownTo := $7BEEFF;
        ButtonAppearance.ColorMirrorDown := $7BEEFF;
        ButtonAppearance.ColorMirrorDownTo := $6CD0FF;
        ButtonAppearance.BorderColorDown := $308AC2;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $D9F9FD;
        ButtonAppearance.ColorCheckedTo := $6CD0FF;
        ButtonAppearance.ColorMirrorChecked := $6CD0FF;
        ButtonAppearance.ColorMirrorCheckedTo := $6CD0FF;
        ButtonAppearance.BorderColorChecked := $308AC2;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $D9F9FD;
        MenuItemAppearance.ColorHotTo := $8AE3FD;
        MenuItemAppearance.ColorMirrorHot := $8AE3FD;
        MenuItemAppearance.ColorMirrorHotTo := $D9F9FD;
        MenuItemAppearance.BorderColorHot := $58CAF1;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $7BEEFF;
        MenuItemAppearance.ColorDownTo := $6CD0FF;
        MenuItemAppearance.ColorMirrorDown := $6CD0FF;
        MenuItemAppearance.ColorMirrorDownTo := $7BEEFF;
        MenuItemAppearance.BorderColorDown := $308AC2;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $D9F9FD;
        MenuItemAppearance.ColorCheckedTo := $8AE3FD;
        MenuItemAppearance.ColorMirrorChecked := $8AE3FD;
        MenuItemAppearance.ColorMirrorCheckedTo := $D9F9FD;
        MenuItemAppearance.BorderColorChecked := $58CAF1;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $C7B29F;
        FrameAppearance.Color := $FDF6EF;
        FrameAppearance.ColorMirror := $F0DAC7;
        FrameAppearance.ColorMirrorTo := $F0DAC7;
        FrameAppearance.ColorTo := $F0DAC7;

        RightFrameColor := $EAD3BF;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;

    end;
    psOffice2010Silver:
    begin
        ButtonAppearance.Color := $FFFFFF;
        ButtonAppearance.ColorTo := $EDE5E0;
        ButtonAppearance.ColorMirror := $EDE5E0;
        ButtonAppearance.ColorMirrorTo := $EDE5E0;
        ButtonAppearance.BorderColor := $D2CDC8;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $8AE3FD;
        ButtonAppearance.ColorHotTo := $D9F9FD;
        ButtonAppearance.ColorMirrorHot := $D9F9FD;
        ButtonAppearance.ColorMirrorHotTo := $8AE3FD;
        ButtonAppearance.BorderColorHot := $58CAF1;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $6CD0FF;
        ButtonAppearance.ColorDownTo := $7BEEFF;
        ButtonAppearance.ColorMirrorDown := $7BEEFF;
        ButtonAppearance.ColorMirrorDownTo := $6CD0FF;
        ButtonAppearance.BorderColorDown := $308AC2;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $D9F9FD;
        ButtonAppearance.ColorCheckedTo := $6CD0FF;
        ButtonAppearance.ColorMirrorChecked := $6CD0FF;
        ButtonAppearance.ColorMirrorCheckedTo := $6CD0FF;
        ButtonAppearance.BorderColorChecked := $308AC2;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $D9F9FD;
        MenuItemAppearance.ColorHotTo := $8AE3FD;
        MenuItemAppearance.ColorMirrorHot := $8AE3FD;
        MenuItemAppearance.ColorMirrorHotTo := $D9F9FD;
        MenuItemAppearance.BorderColorHot := $58CAF1;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $7BEEFF;
        MenuItemAppearance.ColorDownTo := $6CD0FF;
        MenuItemAppearance.ColorMirrorDown := $6CD0FF;
        MenuItemAppearance.ColorMirrorDownTo := $7BEEFF;
        MenuItemAppearance.BorderColorDown := $308AC2;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $D9F9FD;
        MenuItemAppearance.ColorCheckedTo := $8AE3FD;
        MenuItemAppearance.ColorMirrorChecked := $8AE3FD;
        MenuItemAppearance.ColorMirrorCheckedTo := $D9F9FD;
        MenuItemAppearance.BorderColorChecked := $58CAF1;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $D2CDC8;
        FrameAppearance.Color := $FFFFFF;
        FrameAppearance.ColorMirror := $EDE5E0;
        FrameAppearance.ColorMirrorTo := $EDE5E0;
        FrameAppearance.ColorTo := $EDE5E0;

        RightFrameColor := $D4CFCB;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;

    end;
    psOffice2010Black:
    begin
        ButtonAppearance.Color := $BFBFBF;
        ButtonAppearance.ColorTo := $919191;
        ButtonAppearance.ColorMirror := $919191;
        ButtonAppearance.ColorMirrorTo := $919191;
        ButtonAppearance.BorderColor := $6D6D6D;
        ButtonAppearance.Gradient := ggVertical;
        ButtonAppearance.GradientMirror := ggVertical;

        ButtonAppearance.ColorHot := $8AE3FD;
        ButtonAppearance.ColorHotTo := $D9F9FD;
        ButtonAppearance.ColorMirrorHot := $D9F9FD;
        ButtonAppearance.ColorMirrorHotTo := $8AE3FD;
        ButtonAppearance.BorderColorHot := $58CAF1;
        ButtonAppearance.GradientHot := ggVertical;
        ButtonAppearance.GradientMirrorHot := ggVertical;

        ButtonAppearance.ColorDown := $6CD0FF;
        ButtonAppearance.ColorDownTo := $7BEEFF;
        ButtonAppearance.ColorMirrorDown := $7BEEFF;
        ButtonAppearance.ColorMirrorDownTo := $6CD0FF;
        ButtonAppearance.BorderColorDown := $308AC2;
        ButtonAppearance.GradientDown := ggVertical;
        ButtonAppearance.GradientMirrorDown := ggVertical;

        ButtonAppearance.ColorChecked := $D9F9FD;
        ButtonAppearance.ColorCheckedTo := $6CD0FF;
        ButtonAppearance.ColorMirrorChecked := $6CD0FF;
        ButtonAppearance.ColorMirrorCheckedTo := $6CD0FF;
        ButtonAppearance.BorderColorChecked := $308AC2;
        ButtonAppearance.GradientChecked := ggVertical;
        ButtonAppearance.GradientMirrorChecked := ggVertical;

        MenuItemAppearance.ColorHot := $D9F9FD;
        MenuItemAppearance.ColorHotTo := $8AE3FD;
        MenuItemAppearance.ColorMirrorHot := $8AE3FD;
        MenuItemAppearance.ColorMirrorHotTo := $D9F9FD;
        MenuItemAppearance.BorderColorHot := $58CAF1;
        MenuItemAppearance.GradientHot := ggVertical;
        MenuItemAppearance.GradientMirrorHot := ggVertical;

        MenuItemAppearance.ColorDown := $7BEEFF;
        MenuItemAppearance.ColorDownTo := $6CD0FF;
        MenuItemAppearance.ColorMirrorDown := $6CD0FF;
        MenuItemAppearance.ColorMirrorDownTo := $7BEEFF;
        MenuItemAppearance.BorderColorDown := $308AC2;
        MenuItemAppearance.GradientDown := ggVertical;
        MenuItemAppearance.GradientMirrorDown := ggVertical;

        MenuItemAppearance.ColorChecked := $D9F9FD;
        MenuItemAppearance.ColorCheckedTo := $8AE3FD;
        MenuItemAppearance.ColorMirrorChecked := $8AE3FD;
        MenuItemAppearance.ColorMirrorCheckedTo := $D9F9FD;
        MenuItemAppearance.BorderColorChecked := $58CAF1;
        MenuItemAppearance.GradientChecked := ggVertical;
        MenuItemAppearance.GradientMirrorChecked := ggVertical;

        FrameAppearance.BorderColor := $6D6D6D;
        FrameAppearance.Color := $BFBFBF;
        FrameAppearance.ColorMirror := $919191;
        FrameAppearance.ColorMirrorTo := $919191;
        FrameAppearance.ColorTo := $919191;

        RightFrameColor := $00BCBCBC; //$656565;

        MenuItemAppearance.TextColor := clBlack;
        MenuItemAppearance.TextColorHot := clBlack;
        MenuItemAppearance.TextColorDown := clBlack;
    end;
  end;
end;

procedure TAdvPreviewMenuOfficeStyler.ShowMenuShortCuts;
begin
  //
end;

end.
