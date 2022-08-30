{***************************************************************************}
{ TAdvOfficeTabSetStyler component                                          }
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

unit AdvOfficeTabSetStylers;

interface

uses
  AdvOfficeTabSet, Graphics, Windows, Forms, Messages, Controls, Classes, SysUtils,
  AdvStyleIF, AdvGlowButton;

type
  TOfficeTabSetFantasyStyle = (tsArctic, tsAquaBlue, tsChocolate, tsMacOS, tsSilverFox,
    tsSoftSand, tsTerminalGreen, tsTextured, tsWindowsClassic, tsUser);

  TOfficeTabSetStyle = (tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsOffice2003Classic,
    tsOffice2007Luna, tsOffice2007Obsidian, tsWindowsXP, tsWhidbey, tsCustom,tsOffice2007Silver, tsWindowsVista,
    tsWindows7, tsTerminal,
    tsOffice2010Blue, tsOffice2010Silver, tsOffice2010Black,
    tsWindows8,
    tsOffice2013White, tsOffice2013LightGray, tsOffice2013Gray,
    tsWindows10,
    tsOffice2016White, tsOffice2016Gray, tsOffice2016Black);

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
  TAdvOfficeTabSetOfficeStyler = class(TCustomAdvOfficeTabSetStyler, ITMSStyleEx)
  private
    FTones: TColorTones;
    FOldOfficeTheme: TOfficeTheme;
    FNotifierWnd: TNotifierWindow;
    FOfficeTabSetStyle: TOfficeTabSetStyle;
    FMetroStyle: TMetroStyle;
    FMetro: boolean;
    FMetroTextColor: TColor;
    FMetroColor: TColor;
    procedure SetMetro(const Value: boolean);
    procedure SetMetroColor(const Value: TColor);
    procedure SetMetroStyle(const Value: TMetroStyle);
    procedure SetMetroTextColor(const Value: TColor);
  protected
    procedure SetOfficeTabSetStyle(const Value: TOfficeTabSetStyle);
    procedure SetOfficeTabSetStyleAndAppColor(const Value: TOfficeTabSetStyle; AppColor: TColor);
    procedure ThemeChanged(Sender: TObject);
    procedure SetAutoThemeAdapt(const Value: boolean); override;
    procedure InitColorTones; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure ChangeTones; virtual;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
  published
    property AutoThemeAdapt;
    property ButtonBorderColor;
    property GlowButtonAppearance;
    property Metro: boolean read FMetro write SetMetro default false;
    property MetroColor: TColor read FMetroColor write SetMetroColor default clHighlight;
    property MetroTextColor: TColor read FMetroTextColor write SetMetroTextColor default clBlack;
    property MetroStyle: TMetroStyle read FMetroStyle write SetMetroStyle default msLight;
    property TabRounding;
    property Style: TOfficeTabSetStyle read FOfficeTabSetStyle write SetOfficeTabSetStyle default tsOffice2007Luna;
    property TabAppearance;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeTabSetFantasyStyler = class(TCustomAdvOfficeTabSetStyler)
  private
    FOfficeTabSetStyle: TOfficeTabSetFantasyStyle;
  protected
    procedure SetOfficeTabSetStyle(const Value: TOfficeTabSetFantasyStyle);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ButtonBorderColor;
    property GlowButtonAppearance;
    property Style: TOfficeTabSetFantasyStyle read FOfficeTabSetStyle write SetOfficeTabSetStyle default tsChocolate;
    property TabAppearance;
  end;



implementation

//------------------------------------------------------------------------------

{ TNotifierWindow }

procedure TNotifierWindow.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_THEMECHANGED) or (Msg.Msg = WM_OFFICETHEMECHANGED) then
  begin
    if Assigned(FOnThemeChange) then
      FOnThemeChange(Self);
  end;
  inherited;
end;


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

procedure TAdvOfficeTabSetOfficeStyler.ChangeTones;
begin
  FTones := CreateMetroTones(MetroStyle = msLight, FMetroColor, FMetroTextColor);
  if (FOfficeTabSetStyle = tsCustom) and FMetro then
  begin
    SetColorTones(FTones);
  end;
end;

constructor TAdvOfficeTabSetOfficeStyler.Create(AOwner: TComponent);
var
  ctrl: TComponent;
begin
  inherited;

  FOldOfficeTheme := otUnknown;

  FTones := DefaultMetroTones;

  FMetroColor := FTones.Selected.BrushColor;
  FMetroTextColor := clBlack;
  FMetroStyle := msLight;

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

  Style := tsWindowsXP;
  Style := tsOffice2007Luna;
end;

destructor TAdvOfficeTabSetOfficeStyler.Destroy;
begin
  //FNotifierWnd.Free;
  inherited;
end;

procedure TAdvOfficeTabSetOfficeStyler.InitColorTones;
begin
  ChangeTones;
end;

procedure TAdvOfficeTabSetOfficeStyler.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    ThemeChanged(Self);
    if AutoThemeAdapt then
    begin
      ThemeNotifier(FNotifierWnd).RegisterWindow(FNotifierWnd.Handle);
    end;
  end;

  if Metro then
    SetColorTones(FTones);
end;

procedure TAdvOfficeTabSetOfficeStyler.ThemeChanged(Sender: TObject);
var
  ot: TOfficeTheme;
begin

  if not AutoThemeAdapt then
    Exit;

  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  ot := GetOfficeTheme;

  if (ot = FOldOfficeTheme) then
    Exit;

  FOfficeTabSetStyle := tsCustom;

  case ot of
  ot2003Classic: Style := tsOffice2003Classic;
  ot2003Blue: Style := tsOffice2003Blue;
  ot2003Olive: Style := tsOffice2003Olive;
  ot2003Silver: Style := tsOffice2003Silver;
  ot2007Blue: Style := tsOffice2007Luna;
  ot2007Silver: Style := tsOffice2007Silver;
  ot2007Black: Style := tsOffice2007Obsidian;
  ot2010Blue: Style := tsOffice2010Blue;
  ot2010Silver: Style := tsOffice2010Silver;
  ot2010Black: Style := tsOffice2010Black;
  ot2013Silver: Style := tsOffice2013LightGray;
  ot2013Gray: Style := tsOffice2013Gray;
  ot2016White: Style := tsOffice2016White;
  ot2016Gray: Style := tsOffice2016Gray;
  ot2016Black: Style := tsOffice2016Black;

  end;

  FOldOfficeTheme := ot;
end;

procedure TAdvOfficeTabSetOfficeStyler.SetAutoThemeAdapt(const Value: boolean);
begin
  if (Value <> AutoThemeAdapt) then
  begin
    inherited;

    if AutoThemeAdapt then
    begin
      FOldOfficeTheme := otUnknown;
      ThemeChanged(Self);
    end;

    if not (csDesigning in ComponentState) and
       FNotifierWnd.HandleAllocated and not (csLoading in ComponentState) then
    begin
      if AutoThemeAdapt then
        ThemeNotifier(FNotifierWnd).RegisterWindow(FNotifierWnd.Handle)
      else
        ThemeNotifier(FNotifierWnd).UnRegisterWindow(FNotifierWnd.Handle)
    end;
  end;
end;

procedure TAdvOfficeTabSetOfficeStyler.SetComponentStyle(AStyle: TTMSStyle);
begin
  SetComponentStyleAndAppColor(AStyle, clBlack);
end;

procedure TAdvOfficeTabSetOfficeStyler.SetComponentStyleAndAppColor(
  AStyle: TTMSStyle; AppColor: TColor);
begin
  Style := TOfficeTabSetStyle(AStyle);
end;

procedure TAdvOfficeTabSetOfficeStyler.SetMetro(const Value: boolean);
begin
  if (FMetro <> Value) then
  begin
    FMetro := Value;
    if (FMetro) and (csDesigning in ComponentState) then
      FOfficeTabSetStyle := tsCustom;
    if FMetro then
      ChangeTones;
  end;
end;

procedure TAdvOfficeTabSetOfficeStyler.SetMetroColor(const Value: TColor);
begin
  if (FMetroColor <> Value) then
  begin
    FMetroColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvOfficeTabSetOfficeStyler.SetMetroStyle(const Value: TMetroStyle);
begin
  if (FMetroStyle <> Value) then
  begin
    FMetroStyle := Value;
    ChangeTones;
  end;
end;

procedure TAdvOfficeTabSetOfficeStyler.SetMetroTextColor(const Value: TColor);
begin
  if (FMetroTextColor <> Value) then
  begin
    FMetroTextColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvOfficeTabSetOfficeStyler.SetOfficeTabSetStyle(
  const Value: TOfficeTabSetStyle);
begin
  SetOfficeTabSetStyleAndAppColor(Value, clBlack);
end;

procedure TAdvOfficeTabSetOfficeStyler.SetOfficeTabSetStyleAndAppColor(
  const Value: TOfficeTabSetStyle; AppColor: TColor);
begin
  if FOfficeTabSetStyle <> Value then
  begin
    FOfficeTabSetStyle := Value;
    BlendFactor := 50;

    RoundEdges := not (Value in [tsWindowsXP, tsWindows8, tsWindows10,
      tsOffice2013White, tsOffice2013LightGray, tsOffice2013Gray,
      tsOffice2016White, tsOffice2016Gray, tsOffice2016Black
      ]);

    if Value <> tsCustom then
    begin
      if RoundEdges then
        TabRounding := 1
      else
        TabRounding := 0;
    end;

    case FOfficeTabSetStyle of
    tsWindowsXP:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := clBtnFace;
        TabAppearance.BackGround.ColorTo := clBtnFace;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clHighlight;
        TabAppearance.BorderColorSelected := clBlack;
        TabAppearance.BorderColorSelectedHot := clHighlight;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $EFD3C6;
        TabAppearance.ColorSelectedTo := clBtnFace;
        TabAppearance.ColorMirrorSelected := clBtnFace;
        TabAppearance.ColorMirrorSelectedTo := clBtnFace;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clBtnFace;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clBtnFace;

        TabAppearance.ColorHot := $EFD3C6;
        TabAppearance.ColorHotTo := $EFD3C6;
        TabAppearance.ColorMirrorHot := $EFD3C6;
        TabAppearance.ColorMirrorHotTo := $EFD3C6;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := RGB(158, 169, 191);
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { GlowButtonAppearance}
        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.ColorMirror := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.ColorMirrorTo := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.BorderColor := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EFD3C6;
        GlowButtonAppearance.ColorHotTo := $EFD3C6;
        GlowButtonAppearance.ColorMirrorHot := $EFD3C6;
        GlowButtonAppearance.ColorMirrorHotTo := $EFD3C6;
        GlowButtonAppearance.BorderColorHot := clHighlight;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $B59284;
        GlowButtonAppearance.ColorDownTo := $B59284;
        GlowButtonAppearance.ColorMirrorDown := $B59284;
        GlowButtonAppearance.ColorMirrorDownTo := $B59284;
        GlowButtonAppearance.BorderColorDown := clHighlight;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;


        GlowButtonAppearance.ColorChecked := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.ColorCheckedTo := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.ColorMirrorChecked := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.ColorMirrorCheckedTo := HTMLToRgb($DCD8B9);
        GlowButtonAppearance.BorderColorChecked := clBlack;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;
      end;
    tsOffice2003Blue:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := $E4AE88;
        TabAppearance.BackGround.ColorTo := HTMLToRgb($C4DAFA);
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clHighlight;
        TabAppearance.BorderColorSelected := $E3B28D;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FEF6F0;
        TabAppearance.ColorSelectedTo := $FAF1E9;
        TabAppearance.ColorMirrorSelected := $FAF1E9;
        TabAppearance.ColorMirrorSelectedTo := $F6EAE0;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $CCF4FF;
        TabAppearance.ColorHotTo := $CCF4FF;
        TabAppearance.ColorMirrorHot := $CCF4FF;
        TabAppearance.ColorMirrorHotTo := $91D0FF;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := RGB(164, 191, 235);
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $EEDBC8;
        GlowButtonAppearance.ColorTo := $F6DDC9;
        GlowButtonAppearance.ColorMirror := $EDD4C0;
        GlowButtonAppearance.ColorMirrorTo := $F7E1D0;
        GlowButtonAppearance.BorderColor := $E0B99B;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EBFDFF;
        GlowButtonAppearance.ColorHotTo := $ACECFF;
        GlowButtonAppearance.ColorMirrorHot := $59DAFF;
        GlowButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $76AFF1;
        GlowButtonAppearance.ColorDownTo := $4190F3;
        GlowButtonAppearance.ColorMirrorDown := $0E72F1;
        GlowButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $B5DBFB;
        GlowButtonAppearance.ColorCheckedTo := $78C7FE;
        GlowButtonAppearance.ColorMirrorChecked := $9FEBFD;
        GlowButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2003Olive:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := $8CC0B1;
        TabAppearance.BackGround.ColorTo := $CFF0EA;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clBlack;
        TabAppearance.BorderColorSelected := clBlack;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $8CC0B1;
        TabAppearance.ColorSelectedTo := $CFF0EA;
        TabAppearance.ColorMirrorSelected := $CFF0EA;
        TabAppearance.ColorMirrorSelectedTo := $CFF0EA;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $CCF4FF;
        TabAppearance.ColorHotTo := $CCF4FF;
        TabAppearance.ColorMirrorHot := $CCF4FF;
        TabAppearance.ColorMirrorHotTo := $91D0FF;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := RGB(130, 140, 100);
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $CFF0EA;
        GlowButtonAppearance.ColorTo := $CFF0EA;
        GlowButtonAppearance.ColorMirror := $CFF0EA;
        GlowButtonAppearance.ColorMirrorTo := $8CC0B1;
        GlowButtonAppearance.BorderColor := $8CC0B1;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $CCF4FF;
        GlowButtonAppearance.ColorHotTo := $CCF4FF;
        GlowButtonAppearance.ColorMirrorHot := $CCF4FF;
        GlowButtonAppearance.ColorMirrorHotTo := $91D0FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $4E91FE;
        GlowButtonAppearance.ColorDownTo := $4E91FE;
        GlowButtonAppearance.ColorMirrorDown := $4E91FE;
        GlowButtonAppearance.ColorMirrorDownTo := $91D3FF;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $4E91FE;
        GlowButtonAppearance.ColorCheckedTo := $4E91FE;
        GlowButtonAppearance.ColorMirrorChecked := $4E91FE;
        GlowButtonAppearance.ColorMirrorCheckedTo := $91D3FF;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2003Silver:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := $00E6D8D8;
        TabAppearance.BackGround.ColorTo := $00E6D8D8;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $927476;
        TabAppearance.BorderColorSelected := $927476;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $927476;
        TabAppearance.ColorSelectedTo := $00F7F3F3;
        TabAppearance.ColorMirrorSelected := $00E6D8D8;
        TabAppearance.ColorMirrorSelectedTo := $00F7F3F3;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $CCF4FF;
        TabAppearance.ColorHotTo := $CCF4FF;
        TabAppearance.ColorMirrorHot := $CCF4FF;
        TabAppearance.ColorMirrorHotTo := $91D0FF;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := RGB(167, 173, 188);
        TabAppearance.HighLightColorSelected := RGB(234, 234, 240);
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $EDD4C0;
        GlowButtonAppearance.ColorTo := $00E6D8D8;
        GlowButtonAppearance.ColorMirror := $EDD4C0;
        GlowButtonAppearance.ColorMirrorTo := $C8B2B3;
        GlowButtonAppearance.BorderColor := $927476;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $CCF4FF;
        GlowButtonAppearance.ColorHotTo := $CCF4FF;
        GlowButtonAppearance.ColorMirrorHot := $CCF4FF;
        GlowButtonAppearance.ColorMirrorHotTo := $91D0FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $4E91FE;
        GlowButtonAppearance.ColorDownTo := $4E91FE;
        GlowButtonAppearance.ColorMirrorDown := $4E91FE;
        GlowButtonAppearance.ColorMirrorDownTo := $91D3FF;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;


        GlowButtonAppearance.ColorChecked := $4E91FE;
        GlowButtonAppearance.ColorCheckedTo := $4E91FE;
        GlowButtonAppearance.ColorMirrorChecked := $4E91FE;
        GlowButtonAppearance.ColorMirrorCheckedTo := $91D3FF;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2007Luna:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := $FFDBBF;
        TabAppearance.BackGround.ColorTo := clNone;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $EABC99;
        TabAppearance.BorderColorSelected := $E3B28D;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := $8B4215;
        TabAppearance.TextColorHot := $8B4215;
        TabAppearance.TextColorSelected := $8B4215;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FEF6F0;
        TabAppearance.ColorSelectedTo := $FAF1E9;
        TabAppearance.ColorMirrorSelected := $FAF1E9;
        TabAppearance.ColorMirrorSelectedTo := $F6EAE0;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $DDE5E4;
        TabAppearance.ColorHotTo := $FFDEC5;
        TabAppearance.ColorMirrorHot := $D5DFDD;
        TabAppearance.ColorMirrorHotTo := $A3D3E1;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggRadial;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $00E8C7AE;
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $EEDBC8;
        GlowButtonAppearance.ColorTo := $F6DDC9;
        GlowButtonAppearance.ColorMirror := $EDD4C0;
        GlowButtonAppearance.ColorMirrorTo := $F7E1D0;
        GlowButtonAppearance.BorderColor := $E0B99B;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EBFDFF;
        GlowButtonAppearance.ColorHotTo := $ACECFF;
        GlowButtonAppearance.ColorMirrorHot := $59DAFF;
        GlowButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $76AFF1;
        GlowButtonAppearance.ColorDownTo := $4190F3;
        GlowButtonAppearance.ColorMirrorDown := $0E72F1;
        GlowButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $B5DBFB;
        GlowButtonAppearance.ColorCheckedTo := $78C7FE;
        GlowButtonAppearance.ColorMirrorChecked := $9FEBFD;
        GlowButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        GlowButtonAppearance.BorderColorChecked := $45667B;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;

    tsOffice2007Obsidian:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := HTMLToRgb($535353);
        TabAppearance.BackGround.ColorTo := HTMLToRgb($3A3A3A);
        TabAppearance.BackGround.Direction := gdVertical;

        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := HTMLToRgb($9E9C96);
        TabAppearance.BorderColorSelected := $E3B28D;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clWhite;
        TabAppearance.TextColorHot := clWhite;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := HTMLToRgb($EDEEEF);
        TabAppearance.ColorSelectedTo := HTMLToRgb($EDEEEF);
        TabAppearance.ColorMirrorSelected := HTMLToRgb($CED2D2);
        TabAppearance.ColorMirrorSelectedTo := HTMLToRgb($CED2D2);

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := HTMLToRgb($78705B);
        TabAppearance.ColorHotTo := HTMLToRgb($979695);
        TabAppearance.ColorMirrorHot := HTMLToRgb($5F5743);
        TabAppearance.ColorMirrorHotTo := HTMLToRgb($EDAE18);

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggRadial;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggRadial;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clBlack;
        TabAppearance.HighLightColorSelected := $00959899; //$00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00959899;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := HTMLToRgb($D6DEDF);
        GlowButtonAppearance.ColorTo := HTMLToRgb($DBE2E4);
        GlowButtonAppearance.ColorMirror := HTMLToRgb($CED5D7);
        GlowButtonAppearance.ColorMirrorTo := HTMLToRgb($E0E5E7);
        GlowButtonAppearance.BorderColor := HTMLToRgb($B2BCC0);
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EBFDFF;
        GlowButtonAppearance.ColorHotTo := $ACECFF;
        GlowButtonAppearance.ColorMirrorHot := $59DAFF;
        GlowButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $76AFF1;
        GlowButtonAppearance.ColorDownTo := $4190F3;
        GlowButtonAppearance.ColorMirrorDown := $0E72F1;
        GlowButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $B5DBFB;
        GlowButtonAppearance.ColorCheckedTo := $78C7FE;
        GlowButtonAppearance.ColorMirrorChecked := $9FEBFD;
        GlowButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        GlowButtonAppearance.BorderColorChecked := $45667B;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2007Silver:
      begin
        TabAppearance.BackGround.Color := $DDD4D0;
        TabAppearance.BackGround.ColorTo := clNone;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $C1BEBD;
        TabAppearance.BorderColorSelected := $FFFAC1;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := $5C534C;
        TabAppearance.TextColorHot := $5C534C;
        TabAppearance.TextColorSelected := $5C534C;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $F6F6F6;
        TabAppearance.ColorSelectedTo := $F6F6F6;
        TabAppearance.ColorMirrorSelected := $F6F6F6;
        TabAppearance.ColorMirrorSelectedTo := $EFE6E1;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $DDE5E4;
        TabAppearance.ColorHotTo := $FFDEC5;
        TabAppearance.ColorMirrorHot := $D5DFDD;
        TabAppearance.ColorMirrorHotTo := $A3D3E1;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggRadial;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $00D6CDC9;
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;

        GlowButtonAppearance.Color := $F3F3F1;
        GlowButtonAppearance.ColorTo := $F2F2F0;
        GlowButtonAppearance.ColorMirror := $F8F7F6;
        GlowButtonAppearance.ColorMirrorTo := $EEEAE7;
        GlowButtonAppearance.BorderColor := $CAC7C6;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EBFDFF;
        GlowButtonAppearance.ColorHotTo := $ACECFF;
        GlowButtonAppearance.ColorMirrorHot := $59DAFF;
        GlowButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $76AFF1;
        GlowButtonAppearance.ColorDownTo := $4190F3;
        GlowButtonAppearance.ColorMirrorDown := $0E72F1;
        GlowButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $B5DBFB;
        GlowButtonAppearance.ColorCheckedTo := $78C7FE;
        GlowButtonAppearance.ColorMirrorChecked := $9FEBFD;
        GlowButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        GlowButtonAppearance.BorderColorChecked := $45667B;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;
      end;

    tsWhidbey:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := $859D9D;
        TabAppearance.BackGround.ColorTo := $ADC4C4;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $E3B28D;
        TabAppearance.BorderColorSelected := clBlack;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $ADC4C4;
        TabAppearance.ColorSelectedTo := clWhite;
        TabAppearance.ColorMirrorSelected := clWhite;
        TabAppearance.ColorMirrorSelectedTo := clWhite;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $CCF4FF;
        TabAppearance.ColorHotTo := $CCF4FF;
        TabAppearance.ColorMirrorHot := $CCF4FF;
        TabAppearance.ColorMirrorHotTo := $91D0FF;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := RGB(130, 140, 100);
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := $DFEDF0;
        GlowButtonAppearance.ColorMirror := $DFEDF0;
        GlowButtonAppearance.ColorMirrorTo := $DFEDF0;
        GlowButtonAppearance.BorderColor := $99A8AC;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EBFDFF;
        GlowButtonAppearance.ColorHotTo := $ACECFF;
        GlowButtonAppearance.ColorMirrorHot := $59DAFF;
        GlowButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $76AFF1;
        GlowButtonAppearance.ColorDownTo := $4190F3;
        GlowButtonAppearance.ColorMirrorDown := $0E72F1;
        GlowButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $B5DBFB;
        GlowButtonAppearance.ColorCheckedTo := $78C7FE;
        GlowButtonAppearance.ColorMirrorChecked := $9FEBFD;
        GlowButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;
        
      end;

   tsOffice2003Classic:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color := $C9D1D5;
        TabAppearance.BackGround.ColorTo := clWhite;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $E3B28D;
        TabAppearance.BorderColorSelected := clBlack;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $C9D1D5;
        TabAppearance.ColorSelectedTo := clWhite;
        TabAppearance.ColorMirrorSelected := clWhite;
        TabAppearance.ColorMirrorSelectedTo := clWhite;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $CCF4FF;
        TabAppearance.ColorHotTo := $CCF4FF;
        TabAppearance.ColorMirrorHot := $CCF4FF;
        TabAppearance.ColorMirrorHotTo := $91D0FF;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := RGB(170, 167, 161);
        TabAppearance.HighLightColorSelected := $00FFFABF;
        //TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := $C9D1D5;
        GlowButtonAppearance.ColorMirror := clWhite;
        GlowButtonAppearance.ColorMirrorTo := $C9D1D5;
        GlowButtonAppearance.BorderColor := clBlack;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $EBFDFF;
        GlowButtonAppearance.ColorHotTo := $ACECFF;
        GlowButtonAppearance.ColorMirrorHot := $59DAFF;
        GlowButtonAppearance.ColorMirrorHotTo := $A4E9FF;
        GlowButtonAppearance.BorderColorHot := $99CEDB;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $76AFF1;
        GlowButtonAppearance.ColorDownTo := $4190F3;
        GlowButtonAppearance.ColorMirrorDown := $0E72F1;
        GlowButtonAppearance.ColorMirrorDownTo := $4C9FFD;
        GlowButtonAppearance.BorderColorDown := $45667B;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $B5DBFB;
        GlowButtonAppearance.ColorCheckedTo := $78C7FE;
        GlowButtonAppearance.ColorMirrorChecked := $9FEBFD;
        GlowButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
      tsWindowsVista:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color := $FFFDF9;
        TabAppearance.BackGround.ColorTo := $FFFAF0;
        TabAppearance.BorderColor := $FCF2DA;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $FEDF9A;
        TabAppearance.BorderColorSelected := $FEDF9A;
        TabAppearance.BorderColorSelectedHot := clHighlight;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FEF9F0;
        TabAppearance.ColorSelectedTo := $FDF0D7;
        TabAppearance.ColorMirrorSelected := $FDF0D7;
        TabAppearance.ColorMirrorSelectedTo := $FEF9F0;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $FFFDF9;
        TabAppearance.ColorHotTo := $FFFAF0;
        TabAppearance.ColorMirrorHot := $FFFAF0;
        TabAppearance.ColorMirrorHotTo := $FFFDF9;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $FBEDD3;
        TabAppearance.HighLightColorSelected := $FAE9C6;
        TabAppearance.HighLightColorSelectedHot :=  $FCF2DA;
        TabAppearance.HighLightColorDown := $FEF8EC;
        TabAppearance.HighLightColorHot := $FEDF9A;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $FDF8F1;
        GlowButtonAppearance.ColorTo := $FDF8F1;
        GlowButtonAppearance.ColorMirror := $FCEFD5;
        GlowButtonAppearance.ColorMirrorTo := $FDF8F1;
        GlowButtonAppearance.BorderColor := $FDDE99;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := clNone;
        GlowButtonAppearance.ColorHotTo := $FFFAF0;
        GlowButtonAppearance.ColorMirrorHot := $FFFAF0;
        GlowButtonAppearance.ColorMirrorHotTo := $FFFAF0;
        GlowButtonAppearance.BorderColorHot := $FCF2DA;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $FCEFD5;
        GlowButtonAppearance.ColorDownTo := $FCEFD5;
        GlowButtonAppearance.ColorMirrorDown := $FDF4E3;
        GlowButtonAppearance.ColorMirrorDownTo := $FDF4E3;
        GlowButtonAppearance.BorderColorDown := $FEDF9A;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $FCEFD5;
        GlowButtonAppearance.ColorCheckedTo := $FAE9C6;
        GlowButtonAppearance.ColorMirrorChecked := $F7DAA2;
        GlowButtonAppearance.ColorMirrorCheckedTo := $FBEDD3;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
      tsWindows7:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color := $FCEBDC;
        TabAppearance.BackGround.ColorTo := $FCDBC1;
        TabAppearance.BorderColor := $CEA27D;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $E3B28D;
        TabAppearance.BorderColorSelected := $CEA27D;
        TabAppearance.BorderColorSelectedHot := clHighlight;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FCEBDC;
        TabAppearance.ColorSelectedTo := $FCDBC1;
        TabAppearance.ColorMirrorSelected := $FCDBC1;
        TabAppearance.ColorMirrorSelectedTo := $FCEBDC;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHot := $FDFBFA;
        TabAppearance.ColorHotTo := $FDF3EB;
        TabAppearance.ColorMirrorHot := $FDF3EB;
        TabAppearance.ColorMirrorHotTo := $FDFBFA;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $FAE9C6;
        TabAppearance.HighLightColorSelected := $FDF3EB;
        TabAppearance.HighLightColorSelectedHot := $FBD6B8;
        TabAppearance.HighLightColorDown := $FDEFE3;
        TabAppearance.HighLightColorHot := $CEA27D;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $FCEBDC;
        GlowButtonAppearance.ColorTo := $FCDBC1;
        GlowButtonAppearance.ColorMirror := $FCDBC1;
        GlowButtonAppearance.ColorMirrorTo := $FCDBC1;
        GlowButtonAppearance.BorderColor := $CEA27D;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $FDFBFA;
        GlowButtonAppearance.ColorHotTo := $FDF3EB;
        GlowButtonAppearance.ColorMirrorHot := $FDF3EB;
        GlowButtonAppearance.ColorMirrorHotTo := $FDFBFA;
        GlowButtonAppearance.BorderColorHot := $FBD6B8;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $FCEBDC;
        GlowButtonAppearance.ColorDownTo := $FCDBC1;
        GlowButtonAppearance.ColorMirrorDown := $FCDBC1;
        GlowButtonAppearance.ColorMirrorDownTo := $FCEBDC;
        GlowButtonAppearance.BorderColorDown := $CEA27D;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $FDFBFA;
        GlowButtonAppearance.ColorCheckedTo := $FDF3EB;
        GlowButtonAppearance.ColorMirrorChecked := $FCEBDC;
        GlowButtonAppearance.ColorMirrorCheckedTo := $FCEBDC;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
      tsTerminal:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color := clBtnFace;
        TabAppearance.BackGround.ColorTo := clBtnFace;
        TabAppearance.BorderColor := clGray;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clGray;
        TabAppearance.BorderColorSelected := clHighlight;
        TabAppearance.BorderColorSelectedHot := clGray;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clWhite;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := clHighlight;
        TabAppearance.ColorSelectedTo := clHighlight;
        TabAppearance.ColorMirrorSelected := clHighlight;
        TabAppearance.ColorMirrorSelectedTo := clHighlight;

        TabAppearance.ColorDisabled := clGray;
        TabAppearance.ColorDisabledTo := clGray;
        TabAppearance.ColorMirrorDisabled := clGray;
        TabAppearance.ColorMirrorDisabledTo := clGray;

        TabAppearance.ColorHot := clSilver;
        TabAppearance.ColorHotTo := clSilver;
        TabAppearance.ColorMirrorHot := clSilver;
        TabAppearance.ColorMirrorHotTo := clSilver;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clGray;
        TabAppearance.HighLightColorSelected := clBtnFace;
        TabAppearance.HighLightColorSelectedHot := clHighLight;
        TabAppearance.HighLightColorDown := clHighLight;
        TabAppearance.HighLightColorHot := clGray;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := clbtnFace;
        GlowButtonAppearance.ColorTo := clbtnFace;
        GlowButtonAppearance.ColorMirror := clbtnFace;
        GlowButtonAppearance.ColorMirrorTo := clbtnFace;
        GlowButtonAppearance.BorderColor := clGray;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := clSilver;
        GlowButtonAppearance.ColorHotTo := clSilver;
        GlowButtonAppearance.ColorMirrorHot := clSilver;
        GlowButtonAppearance.ColorMirrorHotTo := clSilver;
        GlowButtonAppearance.BorderColorHot := clGray;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := clHighlight;
        GlowButtonAppearance.ColorDownTo := clHighlight;
        GlowButtonAppearance.ColorMirrorDown := clHighlight;
        GlowButtonAppearance.ColorMirrorDownTo := clHighlight;
        GlowButtonAppearance.BorderColorDown := clHighlight;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := clGray;
        GlowButtonAppearance.ColorCheckedTo := clGray;
        GlowButtonAppearance.ColorMirrorChecked := clGray;
        GlowButtonAppearance.ColorMirrorCheckedTo := clGray;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2010Blue:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color :=  $F0DAC7;
        TabAppearance.BackGround.ColorTo := $FDF6EF;
        TabAppearance.BorderColor := $C7B29F;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $BAB5B1;
        TabAppearance.BorderColorSelected := $C7B29F;
        TabAppearance.BorderColorSelectedHot := $C7B29F;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $F0DAC7;
        TabAppearance.ColorSelectedTo := $F0DAC7;
        TabAppearance.ColorMirrorSelected := $F0DAC7;
        TabAppearance.ColorMirrorSelectedTo := $F0DAC7;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $F2EBE4;
        TabAppearance.ColorHotTo := $F2EBE4;
        TabAppearance.ColorMirrorHot := $F2EBE4;
        TabAppearance.ColorMirrorHotTo := $F2EBE4;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $C7B29F;
        TabAppearance.HighLightColorSelected := $C7B29F;
        TabAppearance.HighLightColorSelectedHot := $F0DAC7;
        TabAppearance.HighLightColorDown := $F6E7D9;
        TabAppearance.HighLightColorHot := $FDF6EF;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $FDF6EF;
        GlowButtonAppearance.ColorTo := $F0DAC7;
        GlowButtonAppearance.ColorMirror := $F0DAC7;
        GlowButtonAppearance.ColorMirrorTo := $F0DAC7;
        GlowButtonAppearance.BorderColor := $C7B29F;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $8AE3FD;
        GlowButtonAppearance.ColorHotTo := $D9F9FD;
        GlowButtonAppearance.ColorMirrorHot := $D9F9FD;
        GlowButtonAppearance.ColorMirrorHotTo := $8AE3FD;
        GlowButtonAppearance.BorderColorHot := $58CAF1;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $6CD0FF;
        GlowButtonAppearance.ColorDownTo := $7BEEFF;
        GlowButtonAppearance.ColorMirrorDown := $7BEEFF;
        GlowButtonAppearance.ColorMirrorDownTo := $6CD0FF;
        GlowButtonAppearance.BorderColorDown := $308AC2;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $D9F9FD;
        GlowButtonAppearance.ColorCheckedTo := $6CD0FF;
        GlowButtonAppearance.ColorMirrorChecked := $6CD0FF;
        GlowButtonAppearance.ColorMirrorCheckedTo := $6CD0FF;
        GlowButtonAppearance.BorderColorChecked := $308AC2;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;



      end;
    tsOffice2010Silver:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color := $EDE5E0;
        TabAppearance.BackGround.ColorTo := $FFFFFF;
        TabAppearance.BorderColor := $D2CDC8;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $F8F5F3;
        TabAppearance.BorderColorSelected := $D2CDC8;
        TabAppearance.BorderColorSelectedHot := $D0D0D0;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FFFFFF;
        TabAppearance.ColorSelectedTo := $FFFFFF;
        TabAppearance.ColorMirrorSelected := $FFFFFF;
        TabAppearance.ColorMirrorSelectedTo := $FFFFFF;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $FDFBFA;
        TabAppearance.ColorHotTo := $FDF3EB;
        TabAppearance.ColorMirrorHot := $FDF3EB;
        TabAppearance.ColorMirrorHotTo := $FDFBFA;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $BFBAB6;
        TabAppearance.HighLightColorSelected := $D2CDC8;
        TabAppearance.HighLightColorSelectedHot := $EDE5E0;
        TabAppearance.HighLightColorDown := $F5F4F2;
        TabAppearance.HighLightColorHot := $FFFFFF;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $FFFFFF;
        GlowButtonAppearance.ColorTo := $EDE5E0;
        GlowButtonAppearance.ColorMirror := $EDE5E0;
        GlowButtonAppearance.ColorMirrorTo := $EDE5E0;
        GlowButtonAppearance.BorderColor := $D2CDC8;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $8AE3FD;
        GlowButtonAppearance.ColorHotTo := $D9F9FD;
        GlowButtonAppearance.ColorMirrorHot := $D9F9FD;
        GlowButtonAppearance.ColorMirrorHotTo := $8AE3FD;
        GlowButtonAppearance.BorderColorHot := $58CAF1;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $6CD0FF;
        GlowButtonAppearance.ColorDownTo := $7BEEFF;
        GlowButtonAppearance.ColorMirrorDown := $7BEEFF;
        GlowButtonAppearance.ColorMirrorDownTo := $6CD0FF;
        GlowButtonAppearance.BorderColorDown := $308AC2;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $D9F9FD;
        GlowButtonAppearance.ColorCheckedTo := $6CD0FF;
        GlowButtonAppearance.ColorMirrorChecked := $6CD0FF;
        GlowButtonAppearance.ColorMirrorCheckedTo := $6CD0FF;
        GlowButtonAppearance.BorderColorChecked := $308AC2;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;
      end;
    tsOffice2010Black:
      begin
        BlendFactor := 80;
        { TabAppearance }

        TabAppearance.BackGround.Color := $919191;
        TabAppearance.BackGround.ColorTo := $BFBFBF;
        TabAppearance.BorderColor := $6D6D6D;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $BAB5B1;
        TabAppearance.BorderColorSelected := $6D6D6D;
        TabAppearance.BorderColorSelectedHot := $D7D7D6;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $C9C9C9;
        TabAppearance.ColorSelectedTo := $C9C9C9;
        TabAppearance.ColorMirrorSelected := $C9C9C9;
        TabAppearance.ColorMirrorSelectedTo := $C9C9C9;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $949494;
        TabAppearance.ColorHotTo := $949494;
        TabAppearance.ColorMirrorHot := $949494;
        TabAppearance.ColorMirrorHotTo := $949494;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $BEBEBE;
        TabAppearance.HighLightColorSelected := $6D6D6D;
        TabAppearance.HighLightColorSelectedHot := $919191;
        TabAppearance.HighLightColorDown := $A5A5A5;
        TabAppearance.HighLightColorHot := $BFBFBF;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $BFBFBF;
        GlowButtonAppearance.ColorTo := $919191;
        GlowButtonAppearance.ColorMirror := $919191;
        GlowButtonAppearance.ColorMirrorTo := $919191;
        GlowButtonAppearance.BorderColor := $6D6D6D;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $8AE3FD;
        GlowButtonAppearance.ColorHotTo := $D9F9FD;
        GlowButtonAppearance.ColorMirrorHot := $D9F9FD;
        GlowButtonAppearance.ColorMirrorHotTo := $8AE3FD;
        GlowButtonAppearance.BorderColorHot := $58CAF1;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $6CD0FF;
        GlowButtonAppearance.ColorDownTo := $7BEEFF;
        GlowButtonAppearance.ColorMirrorDown := $7BEEFF;
        GlowButtonAppearance.ColorMirrorDownTo := $6CD0FF;
        GlowButtonAppearance.BorderColorDown := $308AC2;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $D9F9FD;
        GlowButtonAppearance.ColorCheckedTo := $6CD0FF;
        GlowButtonAppearance.ColorMirrorChecked := $6CD0FF;
        GlowButtonAppearance.ColorMirrorCheckedTo := $6CD0FF;
        GlowButtonAppearance.BorderColorChecked := $308AC2;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;

      tsWindows8, tsWindows10:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := clWhite;
        TabAppearance.BackGround.ColorTo := clNone;

        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $ECECEB;
        TabAppearance.BorderColorSelected := $DCDBDA;
        TabAppearance.BorderColorSelectedHot := $DCDBDA;
        TabAppearance.BorderColorDown := $DCDBDA;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $F7F6F5;
        TabAppearance.ColorSelectedTo := clNone;
        TabAppearance.ColorMirrorSelected := $F7F6F5;
        TabAppearance.ColorMirrorSelectedTo := clNone;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $FFFDFD;
        TabAppearance.ColorHotTo := clNone;
        TabAppearance.ColorMirrorHot := $FFFDFD;
        TabAppearance.ColorMirrorHotTo := clNone;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;


        { GlowButtonAppearance }

        GlowButtonAppearance.Color := $F7F6F5;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := $F7F6F5;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $E4E3E2;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $F7EFE8;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := $F7EFE8;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $F9CEA4;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $F7E0C9;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := $F7E0C9;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E4A262;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $F6E8CB;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $F6E8CB;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $DAA026;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := $F7F7F7;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := $F7F7F7;
        GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $DEDEDE;

      end;

      tsOffice2013White:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := clWhite;
        TabAppearance.BackGround.ColorTo := clNone;

        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clNone;
        TabAppearance.BorderColorSelected := $D4D4D4;
        TabAppearance.BorderColorSelectedHot := $D4D4D4;
        TabAppearance.BorderColorDown := $D4D4D4;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := clWhite;
        TabAppearance.ColorSelectedTo := clNone;
        TabAppearance.ColorMirrorSelected := clWhite;
        TabAppearance.ColorMirrorSelectedTo := clNone;

        TabAppearance.ColorDisabled := $EEEEEE;
        TabAppearance.ColorDisabledTo := clNone;
        TabAppearance.ColorMirrorDisabled := $EEEEEE;
        TabAppearance.ColorMirrorDisabledTo := clNone;

        TabAppearance.ColorHot := clWhite;
        TabAppearance.ColorHotTo := clNone;
        TabAppearance.ColorMirrorHot := clWhite;
        TabAppearance.ColorMirrorHotTo := clNone;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;


 { GlowButtonAppearance}

        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := clWhite;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $D4D4D4;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $FCF0E4;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := $FCF0E4;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $EAB47E;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $FCE2C8;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := $FCE2C8;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E59D56;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $FF9933;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $FF9933;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $FF9933;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := $EEEEEE;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := $EEEEEE;
        GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $ACACAC;


      end;

      tsOffice2013LightGray:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := $F6F6F6;
        TabAppearance.BackGround.ColorTo := clNone;

        TabAppearance.BorderColor := $C6C6C6;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clNone;
        TabAppearance.BorderColorSelected := $C6C6C6;
        TabAppearance.BorderColorSelectedHot := $C6C6C6;
        TabAppearance.BorderColorDown := $C6C6C6;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FAFAFA;
        TabAppearance.ColorSelectedTo := clNone;
        TabAppearance.ColorMirrorSelected := $FAFAFA;
        TabAppearance.ColorMirrorSelectedTo := clNone;

        TabAppearance.ColorDisabled := $EEEEEE;
        TabAppearance.ColorDisabledTo := clNone;
        TabAppearance.ColorMirrorDisabled := $EEEEEE;
        TabAppearance.ColorMirrorDisabledTo := clNone;

        TabAppearance.ColorHot := $F6F6F6;
        TabAppearance.ColorHotTo := clNone;
        TabAppearance.ColorMirrorHot := $F6F6F6;
        TabAppearance.ColorMirrorHotTo := clNone;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $F6F6F6;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := $F6F6F6;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $C6C6C6;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $FCF0E4;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := $FCF0E4;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $EAB47E;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $FCE2C8;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := $FCE2C8;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E59D56;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $FF9933;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $FF9933;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $FF9933;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := $EEEEEE;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := $EEEEEE;
        GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $ACACAC;
      end;

      tsOffice2013Gray:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := $E5E5E5;
        TabAppearance.BackGround.ColorTo := clNone;

        TabAppearance.BorderColor := $ABABAB;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clNone;
        TabAppearance.BorderColorSelected := $ABABAB;
        TabAppearance.BorderColorSelectedHot := $ABABAB;
        TabAppearance.BorderColorDown := $ABABAB;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $F3F3F3;
        TabAppearance.ColorSelectedTo := clNone;
        TabAppearance.ColorMirrorSelected := $F3F3F3;
        TabAppearance.ColorMirrorSelectedTo := clNone;

        TabAppearance.ColorDisabled := $EEEEEE;
        TabAppearance.ColorDisabledTo := clNone;
        TabAppearance.ColorMirrorDisabled := $EEEEEE;
        TabAppearance.ColorMirrorDisabledTo := clNone;

        TabAppearance.ColorHot := $F3F3F3; //$E5E5E5;
        TabAppearance.ColorHotTo := clNone;
        TabAppearance.ColorMirrorHot := $F3F3F3; //$E5E5E5;
        TabAppearance.ColorMirrorHotTo := clNone;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;


        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $E5E5E5;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := $E5E5E5;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $ABABAB;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $FCF0E4;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := $FCF0E4;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $EAB47E;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $FCE2C8;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := $FCE2C8;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E59D56;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $FF9933;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $FF9933;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $FF9933;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := $EEEEEE;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := $EEEEEE;
        GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $ACACAC;

      end;

      tsOffice2016White:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := clWhite;
        TabAppearance.BackGround.ColorTo := clNone;

        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clNone;
        TabAppearance.BorderColorSelected := $D4D4D4;
        TabAppearance.BorderColorSelectedHot := $D4D4D4;
        TabAppearance.BorderColorDown := $D4D4D4;

        TabAppearance.TextColor := $444444;
        TabAppearance.TextColorHot := AppColor;
        TabAppearance.TextColorSelected := AppColor;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := clWhite;
        TabAppearance.ColorSelectedTo := clNone;
        TabAppearance.ColorMirrorSelected := clWhite;
        TabAppearance.ColorMirrorSelectedTo := clNone;

        TabAppearance.ColorDisabled := $EEEEEE;
        TabAppearance.ColorDisabledTo := clNone;
        TabAppearance.ColorMirrorDisabled := $EEEEEE;
        TabAppearance.ColorMirrorDisabledTo := clNone;

        TabAppearance.ColorHot := clWhite;
        TabAppearance.ColorHotTo := clNone;
        TabAppearance.ColorMirrorHot := clWhite;
        TabAppearance.ColorMirrorHotTo := clNone;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;

 { GlowButtonAppearance}

        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := clWhite;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $D4D4D4;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $F2E1D5;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := $F2E1D5;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $F2E1D5;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $E3BDA3;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := $E3BDA3;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E3BDA3;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $F2D5C2;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $F2D5C2;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $F2D5C2;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := clWhite;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := clWhite;
        GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $D4D4D4;


      end;

      tsOffice2016Gray:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := $444444;
        TabAppearance.BackGround.ColorTo := $444444;

        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $454545;
        TabAppearance.BorderColorSelected := $B2B2B2;
        TabAppearance.BorderColorSelectedHot := $B2B2B2;
        TabAppearance.BorderColorDown := $B2B2B2;

        TabAppearance.TextColor := $F0F0F0;
        TabAppearance.TextColorHot := $FFFFFF;
        TabAppearance.TextColorSelected := $262626;
        TabAppearance.TextColorDisabled := $646D5B;

        TabAppearance.ColorSelected := $B2B2B2;
        TabAppearance.ColorSelectedTo := $B2B2B2;
        TabAppearance.ColorMirrorSelected := $B2B2B2;
        TabAppearance.ColorMirrorSelectedTo := $B2B2B2;

        TabAppearance.ColorDisabled := $EEEEEE;
        TabAppearance.ColorDisabledTo := clNone;
        TabAppearance.ColorMirrorDisabled := $EEEEEE;
        TabAppearance.ColorMirrorDisabledTo := clNone;

        TabAppearance.ColorHot := $454545;
        TabAppearance.ColorHotTo := $454545;
        TabAppearance.ColorMirrorHot := $454545;
        TabAppearance.ColorMirrorHotTo := $454545;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;


        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $B2B2B2;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := $B2B2B2;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $444444;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $F2E1D5;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := $F2E1D5;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $F2E1D5;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $E3BDA3;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := $E3BDA3;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E3BDA3;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $F2D5C2;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $F2D5C2;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $F2D5C2;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := $B2B2B2;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := $B2B2B2;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $444444;
      end;

      tsOffice2016Black:
      begin
        BlendFactor := 80;
        { TabAppearance }
        TabAppearance.BackGround.Color := $252525;
        TabAppearance.BackGround.ColorTo := clNone;

        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $262626;
        TabAppearance.BorderColorSelected := $4D4D4D;
        TabAppearance.BorderColorSelectedHot := $4D4D4D;
        TabAppearance.BorderColorDown := $4D4D4D;

        TabAppearance.TextColor := $DADADA;
        TabAppearance.TextColorHot := $FFFFFF;
        TabAppearance.TextColorSelected := $FFFFFF;
        TabAppearance.TextColorDisabled := $6D6D6D;

        TabAppearance.ColorSelected := $363636;
        TabAppearance.ColorSelectedTo := $363636;
        TabAppearance.ColorMirrorSelected := $363636;
        TabAppearance.ColorMirrorSelectedTo := $363636;

        TabAppearance.ColorDisabled := $262626;
        TabAppearance.ColorDisabledTo := clNone;
        TabAppearance.ColorMirrorDisabled := $262626;
        TabAppearance.ColorMirrorDisabledTo := clNone;

        TabAppearance.ColorHot := $262626;
        TabAppearance.ColorHotTo := $262626;
        TabAppearance.ColorMirrorHot := $262626;
        TabAppearance.ColorMirrorHotTo := $262626;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := clNone;
        TabAppearance.HighLightColorSelected := clNone;
        TabAppearance.HighLightColorSelectedHot := clNone;
        TabAppearance.HighLightColorDown := clNone;
        TabAppearance.HighLightColorHot := clNone;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $363636;
        GlowButtonAppearance.ColorTo := $363636;
        GlowButtonAppearance.ColorMirror := $363636;
        GlowButtonAppearance.ColorMirrorTo := $363636;
        GlowButtonAppearance.BorderColor := $444444;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $6A6A6A;
        GlowButtonAppearance.ColorHotTo := $6A6A6A;
        GlowButtonAppearance.ColorMirrorHot := $6A6A6A;
        GlowButtonAppearance.ColorMirrorHotTo := $6A6A6A;
        GlowButtonAppearance.BorderColorHot := $6A6A6A;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $444444;
        GlowButtonAppearance.ColorDownTo := $444444;
        GlowButtonAppearance.ColorMirrorDown := $444444;
        GlowButtonAppearance.ColorMirrorDownTo := $444444;
        GlowButtonAppearance.BorderColorDown := $444444;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $575757;
        GlowButtonAppearance.ColorCheckedTo := $575757;
        GlowButtonAppearance.ColorMirrorChecked := $575757;
        GlowButtonAppearance.ColorMirrorCheckedTo := $575757;
        GlowButtonAppearance.BorderColorChecked := $575757;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := $363636;
        GlowButtonAppearance.ColorDisabledTo := clNone;
        GlowButtonAppearance.ColorMirrorDisabled := $363636;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $444444;

      end;

    end;
    Change(2);
  end;
end;

{ TAdvOfficeTabSetFantasyStyler }

constructor TAdvOfficeTabSetFantasyStyler.Create(AOwner: TComponent);
begin
  inherited;
  Style := tsChocolate;
end;

procedure TAdvOfficeTabSetFantasyStyler.SetOfficeTabSetStyle(
  const Value: TOfficeTabSetFantasyStyle);
begin
  FOfficeTabSetStyle := Value;
  BlendFactor := 50;

  case FOfficeTabSetStyle of
  tsChocolate:
    begin
      RoundEdges:= true;
    end;

  tsArctic :
    begin
      RoundEdges:= true;
    end;

  tsWindowsClassic :
    begin
      RoundEdges:= false;
    end;

  tsTerminalGreen :
    begin
      RoundEdges:= false;
  end;

  tsAquaBlue :
    begin
      RoundEdges:= true;
    end;

  tsSilverFox :
    begin
      RoundEdges:= true;
    end;

  tsTextured :
    begin
      RoundEdges:= false;
    end;

  tsMacOS :
    begin
      RoundEdges:= true;
    end;

  tsSoftSand :
    begin
      RoundEdges:= true;
    end;
 { tsWhidbey:
    begin
      RoundEdges := true;
    end; }
  end;

end;

end.
