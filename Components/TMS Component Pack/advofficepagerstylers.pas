{***************************************************************************}
{ TAdvOfficePagerStyler component                                           }
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

unit AdvOfficePagerStylers;

interface

{$I TMSDEFS.INC}

uses
  AdvOfficePager, Graphics, Windows, Forms, Messages, Controls, Classes, SysUtils,
  AdvStyleIF, AdvGlowButton;

type
  TOfficePagerFantasyStyle = (psArctic, psAquaBlue, psChocolate, psMacOS, psSilverFox,
    psSoftSand, psTerminalGreen, psTextured, psWindowsClassic, psUser);

  TOfficePagerStyle = (psOffice2003Blue, psOffice2003Silver, psOffice2003Olive, psOffice2003Classic,
    psOffice2007Luna, psOffice2007Obsidian, psWindowsXP, psWhidbey, psCustom, psOffice2007Silver,
    psWindowsVista, psWindows7, psTerminal,
    psOffice2010Blue, psOffice2010Silver, psOffice2010Black,
    psWindows8,
    psOffice2013White, psOffice2013LightGray, psOffice2013Gray,
    psWindows10,
    psOffice2016White, psOffice2016Gray, psOffice2016Black);

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
  TAdvOfficePagerOfficeStyler = class(TCustomAdvOfficePagerStyler, ITMSStyleEx)
  private
    FTones: TColorTones;
    FOldOfficeTheme: TOfficeTheme;
    FNotifierWnd: TNotifierWindow;
    FOfficePagerStyle: TOfficePagerStyle;
    FMetroStyle: TMetroStyle;
    FMetro: boolean;
    FMetroTextColor: TColor;
    FMetroColor: TColor;
    procedure SetMetro(const Value: boolean);
    procedure SetMetroColor(const Value: TColor);
    procedure SetMetroStyle(const Value: TMetroStyle);
    procedure SetMetroTextColor(const Value: TColor);
  protected
    procedure SetOfficePagerStyle(const Value: TOfficePagerStyle);
    procedure SetOfficePagerStyleAndAppColor(const Value: TOfficePagerStyle; AppColor: TColor);
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
    property Style: TOfficePagerStyle read FOfficePagerStyle write SetOfficePagerStyle default psOffice2007Luna;
    property AutoThemeAdapt;
    property GlowButtonAppearance;
    property Metro: boolean read FMetro write SetMetro default false;
    property MetroColor: TColor read FMetroColor write SetMetroColor default clHighlight;
    property MetroTextColor: TColor read FMetroTextColor write SetMetroTextColor default clBlack;
    property MetroStyle: TMetroStyle read FMetroStyle write SetMetroStyle default msLight;
    property PageAppearance;
    property Show3D;
    property ShowShadow;
    property TabAppearance;
    property TabRounding;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficePagerFantasyStyler = class(TCustomAdvOfficePagerStyler)
  private
    FOfficePagerStyle: TOfficePagerFantasyStyle;
  protected
    procedure SetOfficePagerStyle(const Value: TOfficePagerFantasyStyle);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Style: TOfficePagerFantasyStyle read FOfficePagerStyle write SetOfficePagerStyle default psChocolate;
    property PageAppearance;
    property TabAppearance;
    property GlowButtonAppearance;
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

procedure TAdvOfficePagerOfficeStyler.ChangeTones;
begin
  FTones := CreateMetroTones(MetroStyle = msLight, FMetroColor, FMetroTextColor);
  if (FOfficePagerStyle = psCustom) and FMetro then
  begin
    SetColorTones(FTones);
  end;
end;

constructor TAdvOfficePagerOfficeStyler.Create(AOwner: TComponent);
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

  Style := psWindowsXP;
  Style := psOffice2007Luna;
end;

destructor TAdvOfficePagerOfficeStyler.Destroy;
begin
  //FNotifierWnd.Free;
  inherited;
end;

procedure TAdvOfficePagerOfficeStyler.InitColorTones;
begin
  ChangeTones;
end;

procedure TAdvOfficePagerOfficeStyler.Loaded;
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

procedure TAdvOfficePagerOfficeStyler.ThemeChanged(Sender: TObject);
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

  FOfficePagerStyle := psCustom;

  case ot of
  ot2003Classic: Style := psOffice2003Classic;
  ot2003Blue: Style := psOffice2003Blue;
  ot2003Olive: Style := psOffice2003Olive;
  ot2003Silver: Style := psOffice2003Silver;
  ot2007Blue: Style := psOffice2007Luna;
  ot2007Silver: Style := psOffice2007Silver;
  ot2007Black: Style := psOffice2007Obsidian;
  ot2010Blue: Style := psOffice2010Blue;
  ot2010Silver: Style := psOffice2010Silver;
  ot2010Black: Style := psOffice2010Black;
  end;

  FOldOfficeTheme := ot;
end;

procedure TAdvOfficePagerOfficeStyler.SetAutoThemeAdapt(const Value: boolean);
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

procedure TAdvOfficePagerOfficeStyler.SetComponentStyle(AStyle: TTMSStyle);
begin
  SetOfficePagerStyleAndAppColor(TOfficePagerStyle(AStyle), clBlack);
end;

procedure TAdvOfficePagerOfficeStyler.SetComponentStyleAndAppColor(
  AStyle: TTMSStyle; AppColor: TColor);
begin
  SetOfficePagerStyleAndAppColor(TOfficePagerStyle(AStyle), AppColor);
end;

procedure TAdvOfficePagerOfficeStyler.SetMetro(const Value: boolean);
begin
  if (FMetro <> Value) then
  begin
    FMetro := Value;
    if (FMetro) and (csDesigning in ComponentState) then
      FOfficePagerStyle := psCustom;
    if FMetro then
      ChangeTones;
  end;
end;

procedure TAdvOfficePagerOfficeStyler.SetMetroColor(const Value: TColor);
begin
  if (FMetroColor <> Value) then
  begin
    FMetroColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvOfficePagerOfficeStyler.SetMetroStyle(const Value: TMetroStyle);
begin
  if (FMetroStyle <> Value) then
  begin
    FMetroStyle := Value;
    ChangeTones;
  end;
end;

procedure TAdvOfficePagerOfficeStyler.SetMetroTextColor(const Value: TColor);
begin
  if (FMetroTextColor <> Value) then
  begin
    FMetroTextColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvOfficePagerOfficeStyler.SetOfficePagerStyle(
  const Value: TOfficePagerStyle);
begin
  SetOfficePagerStyleAndAppColor(Value, clBlack);
end;

procedure TAdvOfficePagerOfficeStyler.SetOfficePagerStyleAndAppColor(
  const Value: TOfficePagerStyle; AppColor: TColor);
begin
  if FOfficePagerStyle <> Value then
  begin
    FOfficePagerStyle := Value;
    BlendFactor := 50;
    Show3D := True;
    ShowShadow := True;

    GlowButtonAppearance.BorderColorDisabled := $00D3D3D3;

    RoundEdges := not (Value in [psWindows8, psOffice2013White, psOffice2013LightGray, psOffice2013Gray,
    psWindows10, psOffice2016White, psOffice2016Gray, psOffice2016Black]);

    if RoundEdges then
      TabRounding := 1
    else
      TabRounding := 0;

    case FOfficePagerStyle of
    {
    bsCustom:
      begin
        RightHandleColorHotTo := clNone;
        RightHandleColorDownTo := clNone;
        with ButtonAppearance do
        begin
          ColorTo := clNone;
          ColorDownTo := clNone;
          ColorHotTo := clNone;
          ColorCheckedTo := clNone;
        end;
        RoundEdges := false;
      end;
    bsFlat:
      begin
        RoundEdges := false;
        Color.Color := clBtnFace;
        Color.ColorTo := clNone;
        Color.Direction := gdVertical;
        Color.Steps := 16;
        DockColor.ColorTo := clNone;
        DockColor.Color := clBtnFace;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := clBtnFace;
        RightHandleColorTo := clNone;
        RightHandleColorHot := $D6BE85;
        RightHandleColorHotTo := clNone;
        RightHandleColorDown := $B59284;
        RightHandleColorDownTo := clNone;
        CaptionColor := clHighLight;
        CaptionColorTo := clNone;
        CaptionBorderColor := clHighLight;
        with ButtonAppearance do
        begin
          Color := clBtnFace;
          ColorTo := clNone;
          //ColorChecked := RGB(255, 191, 113); //$94E6FB;
          //ColorCheckedTo := clNone; //$1595EE;

          ColorDown := $B59284;
          ColorDownTo := clNone;

          ColorHot := $D6BE85;
          ColorHotTo := clNone;

          ColorChecked := clBtnFace;
          ColorCheckedTo := clNone;
          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;
        RoundEdges:= false;
        DragGripStyle := dsSingleLine;
        Bevel:= bvRaised;
        UseBevel := False;
      end;
    }
    psWindowsXP:
      begin
        RoundEdges := false;

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
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { PageAppearance }
        PageAppearance.BorderColor := clBlack;
        PageAppearance.Color := clBtnFace;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clBtnFace;
        PageAppearance.ColorMirrorTo := clBtnFace;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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
    psOffice2003Blue:
      begin
        RoundEdges:= true;
        { TabAppearance }

        TabAppearance.BackGround.Color := $E4AE88;
        TabAppearance.BackGround.ColorTo := HTMLToRgb($C4DAFA);
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := clHighlight;
        TabAppearance.BorderColorSelected := clBlack; //$E3B28D;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := clBlack;
        TabAppearance.TextColorSelected := clBlack;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FCDCC4; //$FEF6F0;
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

        TabAppearance.ShadowColor := $A3673F; //RGB(164, 191, 235);
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { PageAppearance }
        PageAppearance.BorderColor := $E3B28D;
        PageAppearance.Color := $FDEADA;
        PageAppearance.ColorTo := HTMLToRgb($C4DAFA);
        PageAppearance.ColorMirror := HTMLToRgb($C4DAFA);
        PageAppearance.ColorMirrorTo := HTMLToRgb($C4DAFA);
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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
    psOffice2003Olive:
      begin
        RoundEdges:= true;
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
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { PageAppearance }
        PageAppearance.BorderColor := $8CC0B1;
        PageAppearance.Color := $CFF0EA;
        PageAppearance.ColorTo := $CFF0EA;
        PageAppearance.ColorMirror := $CFF0EA;
        PageAppearance.ColorMirrorTo := $CFF0EA;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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
    psOffice2003Silver:
      begin
        RoundEdges:= true;
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
        TabAppearance.HighLightColor := RGB(234, 234, 240); //$00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;

        { PageAppearance }
        PageAppearance.BorderColor := $927476;
        PageAppearance.Color := $00F7F3F3;
        PageAppearance.ColorTo := $00E6D8D8;
        PageAppearance.ColorMirror := $00E6D8D8;
        PageAppearance.ColorMirrorTo := $00E6D8D8;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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
    psOffice2007Luna:
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
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        RoundEdges := True;
        { PageAppearance }
        PageAppearance.BorderColor := $E3B28D;
        PageAppearance.Color := $FAF1E9;
        PageAppearance.ColorTo := $EDD8C7;
        PageAppearance.ColorMirror := $EDD8C7;
        PageAppearance.ColorMirrorTo := $FFF2E7;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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

    psOffice2007Obsidian:
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
        TabAppearance.HighLightColor := $00959899; //$00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00959899;

        RoundEdges := True;

        { PageAppearance }
        PageAppearance.Color := HtmlToRgb($C1C6CF);
        PageAppearance.ColorTo := HtmlToRgb($B4BBC5);
        PageAppearance.ColorMirror := HtmlToRgb($B4BBC5);
        PageAppearance.ColorMirrorTo := HtmlToRgb($E5ECEC);
        PageAppearance.BorderColor := HtmlToRgb($AEB0B4);        
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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
    psOffice2007Silver:
      begin
        { TabAppearance }

        TabAppearance.BackGround.Color := $DDD4D0;
        TabAppearance.BackGround.ColorTo := clNone;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $C1BEBD;
        TabAppearance.BorderColorSelected := $C1BFBD; //$FFFAC6;
        TabAppearance.BorderColorSelectedHot := $5FCAFA;

        TabAppearance.TextColor := $5C534C;
        TabAppearance.TextColorHot := $5C534C;
        TabAppearance.TextColorSelected := $5C534C;
        TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $F8F8F4;
        TabAppearance.ColorSelectedTo := $EFE7E2;
        TabAppearance.ColorMirrorSelected := $EFE7E2;
        TabAppearance.ColorMirrorSelectedTo := $EFE7E2;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

        TabAppearance.ColorHotTo := $DFD7D4;
        TabAppearance.ColorHot := $CEE1E9;
        TabAppearance.ColorMirrorHot := $C5D8DF;
        TabAppearance.ColorMirrorHotTo := $8ECEE9;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggRadial;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $00D6CDC9;
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;

        RoundEdges := True;
        { PageAppearance }
        PageAppearance.BorderColor := $BEBEBE;
        PageAppearance.Color := $F9F5F3;
        PageAppearance.ColorTo := $E7DCD5;
        PageAppearance.ColorMirrorTo := $FBFAF0;
        PageAppearance.ColorMirror := $E7DCD5;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance }
        GlowButtonAppearance.Color := $F3F3F1;
        GlowButtonAppearance.ColorTo := $F5F5F3;
        GlowButtonAppearance.ColorMirror := $EEEAE7;
        GlowButtonAppearance.ColorMirrorTo := $F8F7F6;
        GlowButtonAppearance.BorderColor := $CCCAC9;
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
    psWhidbey:
      begin
        RoundEdges := true;

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
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;
        
        { PageAppearance }
        PageAppearance.BorderColor := clBlack;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := $D9E9EC;
        PageAppearance.ColorMirror := $D9E9EC;
        PageAppearance.ColorMirrorTo := clWhite;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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

   psOffice2003Classic:
      begin
        BlendFactor := 80;
        RoundEdges := true;
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
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;
        
        { PageAppearance }
        PageAppearance.BorderColor := clBlack;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := $C9D1D5;
        PageAppearance.ColorMirror := $C9D1D5;
        PageAppearance.ColorMirrorTo := clWhite;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

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
        psWindowsVista:
      begin
        RoundEdges := false;

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

        { PageAppearance }
        PageAppearance.BorderColor := $FCF2DA;
        PageAppearance.Color := $FFFDF9;
        PageAppearance.ColorTo := $FDF0D7;
        PageAppearance.ColorMirror := $FDF0D7;
        PageAppearance.ColorMirrorTo := $FEF9F0;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;


        { GlowButtonAppearance}
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
        GlowButtonAppearance.BorderColorChecked := $FEDF9A;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;
      end;

      psWindows7:
      begin
        RoundEdges := false;

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

        { PageAppearance }
        PageAppearance.BorderColor := $CEA27D;
        PageAppearance.Color := $FCEBDC;
        PageAppearance.ColorTo := $FCDBC1;
        PageAppearance.ColorMirror := $FCDBC1;
        PageAppearance.ColorMirrorTo := $FCEBDC;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}
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
        GlowButtonAppearance.BorderColorChecked := clHighLight;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

      end;
      psTerminal:
      begin
        RoundEdges := false;

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


        { PageAppearance }
        PageAppearance.BorderColor := clGray;
        PageAppearance.Color := clBtnFace;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clBtnFace;
        PageAppearance.ColorMirrorTo := clBtnFace;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}
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
     psOffice2010Blue:
      begin
        RoundEdges := false;

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


        { PageAppearance }
        PageAppearance.BorderColor := $C7B29F;
        PageAppearance.Color := $FDF6EF;
        PageAppearance.ColorTo := $F0DAC7;
        PageAppearance.ColorMirror := $F0DAC7;
        PageAppearance.ColorMirrorTo := $F0DAC7;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}
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
     psOffice2010Silver:
      begin
        RoundEdges := false;

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



        { PageAppearance }
        PageAppearance.BorderColor := $D2CDC8;
        PageAppearance.Color := $FFFFFF;
        PageAppearance.ColorTo := $EDE5E0;
        PageAppearance.ColorMirror := $EDE5E0;
        PageAppearance.ColorMirrorTo := $EDE5E0;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}
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
    psOffice2010Black:
      begin
        RoundEdges := false;

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


        { PageAppearance }
        PageAppearance.BorderColor := $6D6D6D;
        PageAppearance.Color := $BFBFBF;
        PageAppearance.ColorTo := $919191;
        PageAppearance.ColorMirror := $919191;
        PageAppearance.ColorMirrorTo := $919191;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}
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

      psWindows8, psWindows10:
      begin
        RoundEdges := false;

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
        TabAppearance.ColorSelectedTo := $F7F6F5;
        TabAppearance.ColorMirrorSelected := $F7F6F5;
        TabAppearance.ColorMirrorSelectedTo := $F7F6F5;

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
        Tabappearance.HighLightColor := clNone;


        { PageAppearance }
        PageAppearance.BorderColor := $DCDBDA;
        PageAppearance.Color := $F7F6F5;
        PageAppearance.ColorTo := $F7F6F5;
        PageAppearance.ColorMirror := $F7F6F5;
        PageAppearance.ColorMirrorTo := $F7F6F5;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;


        { GlowButtonAppearance}
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

      psOffice2013White:
      begin
        RoundEdges := false;

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
        Tabappearance.HighLightColor := clNone;


        { PageAppearance }
        PageAppearance.BorderColor := $D4D4D4;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clWhite;
        PageAppearance.ColorMirror := clWhite;
        PageAppearance.ColorMirrorTo := clWhite;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;


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

        GlowButtonAppearance.ColorChecked := clWhite;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := clWhite;
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

      psOffice2013LightGray:
      begin
        RoundEdges := false;

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

        TabAppearance.ColorHot := $FAFAFA; //$F6F6F6;
        TabAppearance.ColorHotTo := clNone;
        TabAppearance.ColorMirrorHot := $FAFAFA; // $F6F6F6;
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
        Tabappearance.HighLightColor := clNone;

        { PageAppearance }
        PageAppearance.BorderColor := $C6C6C6;
        PageAppearance.Color := $FAFAFA;
        PageAppearance.ColorTo := $FAFAFA;
        PageAppearance.ColorMirror := $FAFAFA;
        PageAppearance.ColorMirrorTo := $FAFAFA;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}

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

        GlowButtonAppearance.ColorChecked := $F6F6F6;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $F6F6F6;
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

      psOffice2013Gray:
      begin
        RoundEdges := false;

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
        Tabappearance.HighLightColor := clNone;

        { PageAppearance }
        PageAppearance.BorderColor := $ABABAB;
        PageAppearance.Color := $F3F3F3;
        PageAppearance.ColorTo := $F3F3F3;
        PageAppearance.ColorMirror := $F3F3F3;
        PageAppearance.ColorMirrorTo := $F3F3F3;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;


        { GlowButtonAppearance}

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

        GlowButtonAppearance.ColorChecked := $E5E5E5;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := $E5E5E5;
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

     psOffice2016White:
      begin
        RoundEdges := false;

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
        Tabappearance.HighLightColor := clNone;


        { PageAppearance }
        PageAppearance.BorderColor := $D4D4D4;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clWhite;
        PageAppearance.ColorMirror := clWhite;
        PageAppearance.ColorMirrorTo := clWhite;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;


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

      psOffice2016Gray:
      begin
        RoundEdges := false;

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
        Tabappearance.HighLightColor := clNone;

        { PageAppearance }
        PageAppearance.BorderColor := $444444;
        PageAppearance.Color := $B2B2B2;
        PageAppearance.ColorTo := $B2B2B2;
        PageAppearance.ColorMirror := $B2B2B2;
        PageAppearance.ColorMirrorTo := $B2B2B2;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { GlowButtonAppearance}

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
        GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
        GlowButtonAppearance.BorderColorDisabled := $444444;


      end;

      psOffice2016Black:
      begin
        RoundEdges := false;

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
        Tabappearance.HighLightColor := clNone;

        { PageAppearance }
        PageAppearance.BorderColor := $4E4E4E;
        PageAppearance.Color := $363636;
        PageAppearance.ColorTo := $363636;
        PageAppearance.ColorMirror := $363636;
        PageAppearance.ColorMirrorTo := $363636;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;


        { GlowButtonAppearance}

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



(*
procedure TAdvOfficePagerOfficeStyler.LoadFromFile(FileName: String);
var
  ss: string;
  sl: TStringList;
  f: TextFile;
begin
  AssignFile(f, FileName);
  Reset(f);
  if IOResult <> 0 then
    raise Exception.Create('Cannot open file ' + FileName);

  sl:= TStringList.Create;

  Readln(f,ss);
  if UpperCase(ss) = UpperCase('bsOffice2003Blue')then
    Style := bsOffice2003Blue
  else if UpperCase(ss) = UpperCase('bsOffice2003Olive') then
    Style := bsOffice2003Olive
  else if UpperCase(ss) = UpperCase('bsOffice2003Silver') then
    Style := bsOffice2003Silver
  else if UpperCase(ss) = UpperCase('bsOfficeXP') then
    Style := bsOfficeXP
  else if UpperCase(ss) = UpperCase('bsCustom') then
    Style := bsCustom
  else if UpperCase(ss) = UpperCase('bsOffice2007Luna') then
    Style := bsOffice2007Luna
  else if UpperCase(ss) = UpperCase('bsOffice2007Obsidian') then
    Style := bsOffice2007Obsidian;

  LoadPropFromFile(F);

  sl.Free;
  CloseFile(f);
end;

procedure TAdvOfficePagerOfficeStyler.SaveToFile(FileName: String);
var
  ss: string;
  f: TextFile;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  if IOResult <> 0 then
    raise Exception.Create('Cannot Create ' + FileName);

  ss := 'bsOffice2003Blue';
  case Style of
    bsOffice2003Blue:     ss := 'bsOffice2003Blue';
    bsOffice2003Olive:    ss := 'bsOffice2003Olive';
    bsOffice2003Silver:   ss := 'bsOffice2003Silver';
    bsOfficeXP:           ss := 'bsOfficeXP';
    bsCustom:             ss := 'bsCustom';
    bsOffice2007Luna:     ss := 'bsOffice2007Luna';
    bsOffice2007Obsidian: ss := 'bsOffice2007Obsidian';
  end;

  Writeln(f,ss);
  SavePropToFile(f);
  CloseFile(f);
end;
*)

{ TAdvOfficePagerFantasyStyler }

constructor TAdvOfficePagerFantasyStyler.Create(AOwner: TComponent);
begin
  inherited;
  Style := psChocolate;
end;

(*
procedure TAdvOfficePagerFantasyStyler.LoadFromFile(FileName: String);
var
  ss: string;
  sl: TStringList;
  f: TextFile;
begin
  AssignFile(f, FileName);
  Reset(f);
  if IOResult <> 0 then
    raise Exception.Create('Cannot open file ' + FileName);

  sl:= TStringList.Create;

  Readln(f,ss);
  if UpperCase(ss) = UpperCase('bsArctic')then
    Style := bsArctic
  else if UpperCase(ss) = UpperCase('bsAquaBlue') then
    Style := bsAquaBlue
  else if UpperCase(ss) = UpperCase('bsChocolate') then
    Style := bsChocolate
  else if UpperCase(ss) = UpperCase('bsMacOS') then
    Style := bsMacOS
  else if UpperCase(ss) = UpperCase('bsSilverFox') then
    Style := bsSilverFox
  else if UpperCase(ss) = UpperCase('bsSoftSand') then
    Style := bsSoftSand
  else if UpperCase(ss) = UpperCase('bsTerminalGreen') then
    Style := bsTerminalGreen
  else if UpperCase(ss) = UpperCase('bsTextured') then
    Style := bsTextured
  else if UpperCase(ss) = UpperCase('bsWindowsClassic') then
    Style := bsWindowsClassic;

  LoadPropFromFile(F);

  sl.Free;
  CloseFile(f);
end;

procedure TAdvOfficePagerFantasyStyler.SaveToFile(FileName: String);
var
  ss: string;
  f: TextFile;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  if IOResult <> 0 then
    raise Exception.Create('Cannot Create ' + FileName);

  ss := 'bsArctic';
  case Style of
    bsArctic:             ss := 'bsArctic';
    bsAquaBlue:           ss := 'bsAquaBlue';
    bsChocolate:          ss := 'bsChocolate';
    bsMacOS:              ss := 'bsMacOS';
    bsSilverFox:          ss := 'bsSilverFox';
    bsSoftSand:           ss := 'bsSoftSand';
    bsTerminalGreen:      ss := 'bsTerminalGreen';
    bsTextured:           ss := 'bsTextured';
    bsWindowsClassic:     ss := 'bsWindowsClassic';
  end;

  Writeln(f,ss);
  SavePropToFile(f);
  CloseFile(f);
end;
*)

procedure TAdvOfficePagerFantasyStyler.SetOfficePagerStyle(
  const Value: TOfficePagerFantasyStyle);
begin
  FOfficePagerStyle := Value;
  BlendFactor := 50;

  case FOfficePagerStyle of
  psChocolate:
    begin
      RoundEdges:= true;
    end;

  psArctic :
    begin
      RoundEdges:= true;
    end;

  psWindowsClassic :
    begin
      RoundEdges:= false;
    end;

  psTerminalGreen :
    begin
      RoundEdges:= false;
  end;

  psAquaBlue :
    begin
      RoundEdges:= true;
    end;

  psSilverFox :
    begin
      RoundEdges:= true;
    end;

  psTextured :
    begin
      RoundEdges:= false;
    end;

  psMacOS :
    begin
      RoundEdges:= true;
    end;

  psSoftSand :
    begin
      RoundEdges:= true;
    end;
 { psWhidbey:
    begin
      RoundEdges := true;
    end; }
  end;

end;

end.
