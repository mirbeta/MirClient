{*************************************************************************}
{ TMS ToolBars component                                                  }
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

unit AdvToolBarStylers;

interface

uses
  AdvToolBar, Windows, Messages, Controls, Classes, SysUtils, Graphics, AdvStyleIF;

type
  TToolBarFantasyStyle = (bsArctic, bsAquaBlue, bsChocolate, bsMacOS, bsSilverFox,
    bsSoftSand, bsTerminalGreen, bsTextured, bsWindowsClassic, bsUser, bsWhidbey);

  TToolBarStyle = (bsOffice2003Blue, bsOffice2003Silver, bsOffice2003Olive, bsOffice2003Classic,
    bsOffice2007Luna, bsOffice2007Obsidian,
    bsWindowsXP, bsWhidbeyStyle, bsCustom, bsOffice2007Silver, bsOfficeXP, bsWindowsVista,
    bsWindows7, bsTerminal,
    bsOffice2010Blue, bsOffice2010Silver, bsOffice2010Black,
    bsWindows8,
    bsOffice2013White, bsOffice2013LightGray, bsOffice2013Gray,
    bsWindows10,
    bsOffice2016White, bsOffice2016Gray, bsOffice2016Black);

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
  TAdvToolBarOfficeStyler = class(TCustomAdvToolBarStyler, ITMSStyleEx)
  private
    FOldOfficeTheme: TOfficeTheme;
    FDesignTime: boolean;
    FNotifierWnd: TNotifierWindow;
    FToolBarStyle: TToolBarStyle;
  protected
    procedure SetToolBarStyle(const Value: TToolBarStyle); virtual;
    procedure ThemeChanged(Sender: TObject);
    procedure SetAutoThemeAdapt(const Value: boolean); override;
    procedure InitStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AAppColor: TColor);
    property UseBevel;
    property FormBorderColor;
    procedure UpdateAppColors;
    property RoundEdges;
  published
    property AppColor;
    property Style: TToolBarStyle read FToolBarStyle write SetToolBarStyle default bsOffice2003Blue;
    property AdvMenuStyler;
    property AutoThemeAdapt;
    { Common properties for AdvToolBar and DockPanel -PropID: 1- }
    property BackGround;
    property BackGroundTransparent;
    property BackGroundDisplay;
    property BorderColor;
    property BorderColorHot;
    property ButtonAppearance;
    property CaptionAppearance;
    property CaptionFont;
    property ContainerAppearance;
    property Color;
    property ColorHot;
    property CompactGlowButtonAppearance;
    property DockColor;
    property DragGripStyle;
    property DragGripImage;
    property FloatingWindowBorderColor;
    property FloatingWindowBorderWidth;
    property Font;
    property GlowButtonAppearance;
    property GroupAppearance;
    property PageAppearance;
    property PagerCaption;
    property QATAppearance;
    property RightHandleImage;
    property RightHandleColor;
    property RightHandleColorTo;
    property RightHandleColorHot;
    property RightHandleColorHotTo;
    property RightHandleColorDown;
    property RightHandleColorDownTo;
    property TabAppearance;
    property SystemFont;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvToolBarFantasyStyler = class(TCustomAdvToolBarStyler)
  private
    FToolBarStyle: TToolBarFantasyStyle;
  protected
    procedure SetToolBarStyle(const Value: TToolBarFantasyStyle);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    property UseBevel;
  published
    property Style: TToolBarFantasyStyle read FToolBarStyle write SetToolBarStyle default bsChocolate;
    property AdvMenuStyler;
    property BackGround;
    property BackGroundTransparent;
    property BackGroundDisplay;
    property BorderColor;
    property BorderColorHot;
    property ButtonAppearance;
    property CaptionAppearance;
    {property CaptionColor;
    property CaptionColorTo;
    property CaptionColorHot;
    property CaptionColorHotTo;
    property CaptionTextColorHot;
    property CaptionBorderColorHot; }
    property CaptionFont;
    {property CaptionTextColor;
    property CaptionBorderColor;}
    property ContainerAppearance;
    property Color;
    property ColorHot;
    property CompactGlowButtonAppearance;
    property DockColor;
    property DragGripStyle;
    property DragGripImage;
    property FloatingWindowBorderColor;
    property FloatingWindowBorderWidth;
    property Font;
    property GlowButtonAppearance;
    property GroupAppearance;
    property PageAppearance;
    property PagerCaption;
    property QATAppearance;
    property RightHandleImage;
    property RightHandleColor;
    property RightHandleColorTo;
    property RightHandleColorHot;
    property RightHandleColorHotTo;
    property RightHandleColorDown;
    property RightHandleColorDownTo;
    property TabAppearance;
  end;


implementation

uses
  AdvGDIP, AdvGlowButton;

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

constructor TAdvToolBarOfficeStyler.Create(AOwner: TComponent);
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

  FToolBarStyle := bsCustom;
  FOldOfficeTheme := otUnknown;

  if Assigned(ctrl) then
    if (ctrl is TWinControl) then
      FNotifierWnd.Parent := TWinControl(ctrl);
  
  FNotifierWnd.OnThemeChange := ThemeChanged;

  // do this only when in designtime!
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    Style := bsWindowsXP;
    Style := bsOffice2003Blue;
  end;
end;

destructor TAdvToolBarOfficeStyler.Destroy;
begin
  inherited;
end;

procedure TAdvToolBarOfficeStyler.Loaded;
var
  st: TToolBarStyle;
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

  if (Style in [bsOffice2013White, bsOffice2013Gray, bsOffice2013LightGray, bsWindows8]) then
  begin
    st := Style;
    FToolBarStyle := bsCustom;
    Style := st;
  end;

end;

procedure TAdvToolBarOfficeStyler.ThemeChanged(Sender: TObject);
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

  FToolBarStyle := bsCustom;

  case ot of
  ot2003Classic: Style := bsOffice2003Classic;
  ot2003Blue: Style := bsOffice2003Blue;
  ot2003Olive: Style := bsOffice2003Olive;
  ot2003Silver: Style := bsOffice2003Silver;
  ot2007Blue: Style := bsOffice2007Luna;
  ot2007Silver: Style := bsOffice2007Silver;
  ot2007Black: Style := bsOffice2007Obsidian;
  ot2010Blue: Style := bsOffice2010Blue;
  ot2010Silver: Style := bsOffice2010Silver;
  ot2010Black: Style := bsOffice2010Black;
  ot2013White: Style := bsOffice2013White;
  ot2013Silver: Style := bsOffice2013LightGray;
  ot2013Gray: Style := bsOffice2013Gray;
  ot2016White: Style := bsOffice2016White;
  ot2016Gray: Style := bsOffice2016Gray;
  ot2016Black: Style := bsOffice2016Black;
  end;

  FOldOfficeTheme := ot;
end;

procedure TAdvToolBarOfficeStyler.UpdateAppColors;
var
  FOldStyle: TToolBarStyle;
begin
  FOldStyle := FToolBarStyle;
  FToolBarStyle := bsCustom;
  Style := FOldStyle;
end;

procedure TAdvToolBarOfficeStyler.InitStyle;
begin
  inherited;
  SetToolBarStyle(Style);
end;

procedure TAdvToolBarOfficeStyler.SetAutoThemeAdapt(const Value: boolean);
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

procedure TAdvToolBarOfficeStyler.SetComponentStyle(AStyle: TTMSStyle);
begin
  SetComponentStyleAndAppColor(AStyle, clNone);
end;

procedure TAdvToolBarOfficeStyler.SetComponentStyleAndAppColor(AStyle: TTMSStyle; AAppColor: TColor);
begin
  if AAppColor <> clNone then
  begin
    AppColor.AppButtonColor := AAppColor;
    AppColor.AppButtonHoverColor := ChangeBrightness(AAppColor, 10);
  end;

  case AStyle of
    tsOffice2003Blue: Style := bsOffice2003Blue;
    tsOffice2003Silver: Style := bsOffice2003Silver;
    tsOffice2003Olive: Style := bsOffice2003Olive;
    tsOffice2003Classic: Style := bsOffice2003Classic;
    tsOffice2007Luna: Style := bsOffice2007Luna;
    tsOffice2007Obsidian: Style := bsOffice2007Obsidian;
    tsWindowsXP: Style := bsWindowsXP;
    tsWhidbey: Style := bsWhidbeyStyle;
    tsOffice2007Silver: Style := bsOffice2007Silver;
    tsWindowsVista: Style := bsWindowsVista;
    tsWindows7: Style := bsWindows7;
    tsWindows8: Style := bsWindows8;
    tsWindows10: Style := bsWindows10;
    tsTerminal: Style := bsTerminal;
    tsOffice2010Blue: Style := bsOffice2010blue;
    tsOffice2010Silver: Style := bsOffice2010Silver;
    tsOffice2010Black: Style := bsOffice2010Black;
    tsOffice2013White: Style := bsOffice2013White;
    tsOffice2013LightGray: Style := bsOffice2013LightGray;
    tsOffice2013Gray: Style := bsOffice2013Gray;
    tsOffice2016White: Style := bsOffice2016White;
    tsOffice2016Gray: Style := bsOffice2016Gray;
    tsOffice2016Black: Style := bsOffice2016Black;
  end;
end;

procedure TAdvToolBarOfficeStyler.SetToolBarStyle(const Value: TToolBarStyle);
var
  ATones: TColorTones;
  FTones: TOfficeTones;
  APager: TAdvToolBarPager;

  procedure InitFont(Font: TFont; SysFont: boolean);
  begin
    if SysFont then
    begin
      if IsVista then
        Font.Name := 'Segoe UI'
      else
        Font.Name := 'Tahoma';
    end;
  end;

begin
//  if FToolBarStyle <> Value then
  begin
    FToolBarStyle := Value;

    case FToolBarStyle of
      bsOffice2003Blue: ApplyStyle(tsOffice2003Blue);
      bsOffice2003Silver: ApplyStyle(tsOffice2003Silver);
      bsOffice2003Olive: ApplyStyle(tsOffice2003Olive);
      bsOffice2003Classic: ApplyStyle(tsOffice2003Classic);
      bsOffice2007Luna: ApplyStyle(tsOffice2007Luna);
      bsOffice2007Obsidian: ApplyStyle(tsOffice2007Obsidian);
      bsOfficeXP, bsWindowsXP: ApplyStyle(tsWindowsXP);
      bsWhidbeyStyle: ApplyStyle(tsWhidbey);
      bsCustom: ApplyStyle(tsCustom);
      bsOffice2007Silver: ApplyStyle(tsOffice2007Silver);
      bsWindowsVista: ApplyStyle(tsWindowsVista);
      bsWindows7: ApplyStyle(tsWindows7);
      bsWindows8: ApplyStyle(tsWindows8);
      bsWindows10: ApplyStyle(tsWindows10);
      bsTerminal: ApplyStyle(tsTerminal);
      bsOffice2010Blue: ApplyStyle(tsOffice2010Blue);
      bsOffice2010Silver: ApplyStyle(tsOffice2010Silver);
      bsOffice2010Black: ApplyStyle(tsOffice2010Black);
      bsOffice2013White: ApplyStyle(tsOffice2013White);
      bsOffice2013LightGray: ApplyStyle(tsOffice2013LightGray);
      bsOffice2013Gray: ApplyStyle(tsOffice2013Gray);
      bsOffice2016White: ApplyStyle(tsOffice2016White);
      bsOffice2016Gray: ApplyStyle(tsOffice2016Gray);
      bsOffice2016Black: ApplyStyle(tsOffice2016Black);
    end;

    TMSStyle := NewTMSStyle;

    if FToolBarStyle = bsCustom then
      Exit;

    SysButtonColor := clBlack;
    PagerCaption.TextColor := clGray;
    PagerCaption.TextColorExtended := clBlue;
    PagerCaption.Font.Size := 10;

    QATSharpCurve := False;

    InitFont(TabAppearance.Font, TabAppearance.SystemFont);
    InitFont(PagerCaption.Font, PagerCaption.SystemFont);
    InitFont(GroupAppearance.Font, GroupAppearance.SystemFont);
    InitFont(ButtonAppearance.CaptionFont, ButtonAppearance.SystemFont);
    InitFont(Font, SystemFont);
    InitFont(CaptionFont, SystemFont);

    GlowButtonAppearance.TranspHover := (FToolBarStyle in [bsOffice2010Blue, bsOffice2010Silver, bsOffice2010Black, bsOffice2013White, bsOffice2013LightGray, bsOffice2013Gray, bsOffice2016White, bsOffice2016Gray, bsOffice2016Black]);
    GlowButtonAppearance.NoBorderDefault := (FToolBarStyle in [bsOffice2013White, bsOffice2013LightGray, bsOffice2013Gray, bsOffice2016White, bsOffice2016Gray, bsOffice2016Black]);
    GlowButtonAppearance.TextColor := clNone;
    GlowButtonAppearance.SystemFont := true;
    CompactGlowButtonAppearance.SystemFont := true;

    APager := GetToolBarPager;
    if Assigned(APager) then
      APager.ClearOfficeTones;

    if (FToolBarStyle in [bsOffice2003Blue, bsOffice2003Olive, bsOffice2003Silver, bsWhidbeyStyle]) then
    begin
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

      CompactGlowButtonAppearance.Assign(GlowButtonAppearance);
    end;

    case FToolBarStyle of
    bsWindowsXP, bsOfficeXP:
      begin
        TMSStyle := tsWindowsXP;
        Color.Color := clBtnFace;
        Color.ColorTo := clBtnFace;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := clBtnFace;
        DockColor.Color := clBtnFace;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := clBtnFace;
        RightHandleColorTo := clNone;
        RightHandleColorHot := $EFD3C6; //$D6BE85;
        RightHandleColorHotTo := clNone;
        RightHandleColorDown := $B59284;
        RightHandleColorDownTo := clNone;

        CaptionAppearance.CaptionColor := clHighLight;
        CaptionAppearance.CaptionColorTo := clHighLight;
        CaptionAppearance.CaptionBorderColor := clHighLight;

        with ButtonAppearance do
        begin
          Color := clBtnFace;
          ColorTo := clBtnFace;
          //ColorChecked := RGB(255, 191, 113); //$94E6FB;
          //ColorCheckedTo := clNone; //$1595EE;

          ColorDown := $B59284;
          ColorDownTo := clNone;

          ColorHot := $EFD3C6; //$D6BE85;
          ColorHotTo := clNone;

          ColorChecked := clBtnFace;
          ColorCheckedTo := clNone;
          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;

        FloatingWindowBorderColor := clHighlight;
        FloatingWindowBorderWidth := 2;

        RoundEdges := false;
        DragGripStyle := dsSingleLine;
        Bevel := bvRaised;
        UseBevel := True;

        {AdvToolBarPager}
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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);


        CaptionAppearance.CaptionColorHot := clHighLight;
        CaptionAppearance.CaptionColorHotTo := clHighLight;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clWhite;

        { GroupAppearance }

        GroupAppearance.TextColor := clBlack;
        GroupAppearance.Color := $EFD3C6;
        GroupAppearance.ColorTo := $EFD3C6;
        GroupAppearance.ColorMirror := $EFD3C6;
        GroupAppearance.ColorMirrorTo := $EFD3C6;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := clHighlight;

        GroupAppearance.TabAppearance.ColorHot := $EFD3C6;
        GroupAppearance.TabAppearance.ColorHotTo := $EFD3C6;
        GroupAppearance.TabAppearance.ColorMirrorHot := $EFD3C6;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $EFD3C6;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $EFD3C6;
        GroupAppearance.TabAppearance.ColorSelectedTo := $EFD3C6;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $EFD3C6;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $EFD3C6;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := clBlack;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := clHighlight;
        GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
        GroupAppearance.TabAppearance.BorderColor := clHighlight;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.TabAppearance.ShadowColor := RGB(191, 188, 164);
        GroupAppearance.TabAppearance.HighLightColor := $00FFFABF;
        GroupAppearance.TabAppearance.HighLightColorSelected := $0063CCF8;
        GroupAppearance.TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        GroupAppearance.TabAppearance.HighLightColorDown := $00FFFBD0;
        GroupAppearance.TabAppearance.HighLightColorHot := $00FDF4ED;

        GroupAppearance.PageAppearance.Color := $EFD3C6;
        GroupAppearance.PageAppearance.ColorTo := clBtnFace;
        GroupAppearance.PageAppearance.BorderColor := clBlack;
        GroupAppearance.PageAppearance.ColorMirror := clBtnFace;
        GroupAppearance.PageAppearance.ColorMirrorTo := clBtnFace;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := clBtnFace;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := clbtnFace;
        GroupAppearance.ToolBarAppearance.BorderColor := clBlack;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EFD3C6;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $EFD3C6;
        GroupAppearance.ToolBarAppearance.BorderColorHot := clHighlight;

        GroupAppearance.CaptionAppearance.CaptionColor := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

        TabAppearance.ColorSelected := clWhite; //$EFD3C6;
        TabAppearance.ColorSelectedTo := clBtnFace;
        TabAppearance.ColorMirrorSelected := clBtnFace;
        TabAppearance.ColorMirrorSelectedTo := clBtnFace;

        TabAppearance.ColorDisabled := clWhite;
        TabAppearance.ColorDisabledTo := clSilver;
        TabAppearance.ColorMirrorDisabled := clWhite;
        TabAppearance.ColorMirrorDisabledTo := clSilver;

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

        TabAppearance.ShadowColor := RGB(191, 188, 164);
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { ToolBar color & color hot }
        Color.Mirror.Color := clBtnFace;
        Color.Mirror.ColorTo := clBtnFace;
        Color.Mirror.ColorMirror := clBtnFace;
        Color.Mirror.ColorMirrorTo := clBtnFace;

        ColorHot.Color := $EFD3C6;
        ColorHot.ColorTo := $EFD3C6;
        ColorHot.Mirror.Color := RGB(244, 242, 232);
        ColorHot.Mirror.ColorTo := RGB(244, 242, 232);
        ColorHot.Mirror.ColorMirror := RGB(244, 242, 232);
        ColorHot.Mirror.ColorMirrorTo := RGB(244, 242, 232);
        ColorHot.Direction := gdVertical;
        BorderColorHot := clHighlight;
        BorderColor := clGray;

        { PageAppearance }
        PageAppearance.BorderColor := clBlack;
        PageAppearance.Color := clBtnFace;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clBtnFace;
        PageAppearance.ColorMirrorTo := clBtnFace;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { PagerCaption }
        PagerCaption.Color := clBtnFace;
        PagerCaption.ColorTo := clBtnFace;
        PagerCaption.ColorMirror := clBtnFace;
        PagerCaption.ColorMirrorTo := clBtnFace;
        PagerCaption.BorderColor := clBlack;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := RGB(172, 168, 153);
        PageAppearance.HighLightColor := clBlack; //$00FDF9CE;

        QATAppearance.Color := clBtnFace;
        QATAppearance.ColorTo := clBtnFace;
        QATAppearance.BorderColor := clGray;

        QATAppearance.FullSizeColor := clBtnFace;
        QATAppearance.FullSizeColorTo := clBtnFace;
        QATAppearance.FullSizeBorderColor := clGray;

      end;
    bsOffice2003Blue:
      begin
        TMSStyle := tsOffice2003Blue;
        Color.Color := $FDEADA;
        Color.ColorTo := $E4AE88;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := $00FADAC4;
        DockColor.Color := $00F5BFA0;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;
        RightHandleColor := $F1A675;
        RightHandleColorTo := $913500;
        RightHandleColorHot := $D3F8FF;
        RightHandleColorHotTo := $76C1FF;
        RightHandleColorDown := $087FE8;
        RightHandleColorDownTo := $7CDAF7;

        CaptionAppearance.CaptionColor := clHighLight;
        CaptionAppearance.CaptionColorTo := clHighLight;
        CaptionAppearance.CaptionBorderColor := clHighLight;

        with ButtonAppearance do
        begin
          Color := $FDEADA;
          ColorTo := $E4AE88;
          {
          ColorDown := $087FE8;
          ColorDownTo := $7CDAF7;
          ColorHot := $DCFFFF;
          ColorHotTo := $5BC0F7;
          ColorChecked := $3E80FE;
          ColorCheckedTo := clNone;
          }
          ColorDown := $4E91FE;
          ColorDownTo := $91D3FF;
          ColorHot := $CCF4FF;
          ColorHotTo := $91D0FF;
          ColorChecked := $8CD5FF;
          ColorCheckedTo := $58AFFF;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderDownColor := $800000;
          BorderHotColor := $800000;
          BorderCheckedColor := $800000;
        end;

        FloatingWindowBorderColor := $913500;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

        GlowButtonAppearance.Color := $EEDBC8;
        GlowButtonAppearance.ColorTo := $F6DDC9;
        GlowButtonAppearance.ColorMirror := $EDD4C0;
        GlowButtonAppearance.ColorMirrorTo := $F7E1D0;
        GlowButtonAppearance.BorderColor := $E0B99B;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;
        {
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
        }
        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        {
        CaptionAppearance.CaptionColorHot := $FDEADA;
        CaptionAppearance.CaptionColorHotTo := clHighLight;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clWhite;
        }

        CaptionAppearance.CaptionColorHot := $CCF4FF;
        CaptionAppearance.CaptionColorHotTo := $91D0FF;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clBlack;

        { GroupAppearance }

        GroupAppearance.TextColor := clBlack;
        GroupAppearance.Color := $FDEADA;
        GroupAppearance.ColorTo := $F1DECC;
        GroupAppearance.ColorMirror := $F1DECC;
        GroupAppearance.ColorMirrorTo := $FDEADA;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := $C2C2C2;

        GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
        GroupAppearance.TabAppearance.BorderColor := clHighLight;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $F0D9C1;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $FFE0C8;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $FFEDD6;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

        TabAppearance.ShadowColor := $A3673F; //RGB(118, 161, 224);
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;

        { ToolBar color & color hot }
        Color.Mirror.Color := RGB(196, 218, 250);
        Color.Mirror.ColorTo := RGB(196, 218, 250);
        Color.Mirror.ColorMirror := RGB(185, 210, 249);
        Color.Mirror.ColorMirrorTo := RGB(206, 225, 252);
        BorderColor := RGB(171, 199, 235);

        ColorHot.Color := $FFF1E6;
        ColorHot.ColorTo := $FAEADE;
        ColorHot.Mirror.Color := RGB(210, 226, 251);
        ColorHot.Mirror.ColorTo := RGB(210, 226, 251);
        ColorHot.Mirror.ColorMirror := RGB(200, 220, 251);
        ColorHot.Mirror.ColorMirrorTo := RGB(215, 231, 253);
        ColorHot.Direction := gdVertical;
        BorderColorHot := $E0C7AD;
        BorderColor := $E0C7AD;

        { PageAppearance }
        PageAppearance.BorderColor := $E3B28D;
        PageAppearance.Color := RGB(196, 218, 250);//$FDEADA;   //ch: Khan
        PageAppearance.ColorTo := RGB(196, 218, 250); //HTMLToRgb($C4DAFA);
        PageAppearance.ColorMirror := RGB(185, 210, 249); //HTMLToRgb($C4DAFA);
        PageAppearance.ColorMirrorTo := RGB(215, 232, 253); //HTMLToRgb($C4DAFA);
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := $CD9D7A; //  RGB(172, 168, 153);
        PageAppearance.HighLightColor := $00FDF9CE;
       

        { PagerCaption }
        PagerCaption.Color := $E4AE88;
        PagerCaption.ColorTo := $E4AE88;
        PagerCaption.ColorMirror := $E4AE88;
        PagerCaption.ColorMirrorTo := $E4AE88;
        PagerCaption.BorderColor := $F0CAAE;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := $00FDEADA;
        QATAppearance.ColorTo := $00E4AE88;
        QATAppearance.BorderColor := $00913500;

        QATAppearance.FullSizeColor := $00E4AE88;
        QATAppearance.FullSizeColorTo := $00E4AE88;
        QATAppearance.FullSizeBorderColor := $00E4AE88;

      end;
    bsOffice2003Olive:
      begin
        TMSStyle := tsOffice2003Olive;
        Color.Color := $CFF0EA;
        Color.ColorTo := $8CC0B1;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := $00E4F1F2;
        DockColor.Color := $00AADADA;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $8CC2B0;
        RightHandleColorTo := $6B7760;
        RightHandleColorHot := $D3F8FF;
        RightHandleColorHotTo := $76C1FF;

        CaptionAppearance.CaptionColor := clHighLight;
        CaptionAppearance.CaptionColorTo := clHighLight;
//        CaptionAppearance.CaptionColor := HTMLToRgb($657C6D);
//        CaptionAppearance.CaptionColorTo := $CFF0EA;
        CaptionAppearance.CaptionBorderColor := clWhite;

        with ButtonAppearance do
        begin
          Color := $CFF0EA;
          ColorTo := $8CC0B1;
          {
          ColorDown := $087FE8;
          ColorDownTo := $7CDAF7;
          ColorHot := $DCFFFF;
          ColorHotTo := $5BC0F7;
          ColorChecked := $3E80FE;
          ColorCheckedTo := clNone;
          }

          ColorDown := $4E91FE;
          ColorDownTo := $91D3FF;
          ColorHot := $CCF4FF;
          ColorHotTo := $91D0FF;
          ColorChecked := $8CD5FF;
          ColorCheckedTo := $58AFFF;

          BorderDownColor := $385D3F;
          BorderHotColor := $385D3F;
          BorderCheckedColor := $385D3F;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;

        FloatingWindowBorderColor := $6B7760;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

        GlowButtonAppearance.Color := $CFF0EA;
        GlowButtonAppearance.ColorTo := $CFF0EA;
        GlowButtonAppearance.ColorMirror := $CFF0EA;
        GlowButtonAppearance.ColorMirrorTo := $8CC0B1;
        GlowButtonAppearance.BorderColor := $8CC0B1;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;
        {
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
        }
        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CaptionAppearance.CaptionColorHot := $CCF4FF;
        CaptionAppearance.CaptionColorHotTo := $91D0FF;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clBlack;

        { GroupAppearance }

        GroupAppearance.TextColor := clBlack;
        GroupAppearance.Color := $CFF0EA;
        GroupAppearance.ColorTo := $CFF0EA;
        GroupAppearance.ColorMirror := $CFF0EA;
        GroupAppearance.ColorMirrorTo := $CFF0EA;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := $C2C2C2;

        GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := clBlack;
        GroupAppearance.TabAppearance.BorderColor := clNone;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := HTMLToRgb($F2F1E4);
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := HTMLToRgb($F2F1E4);
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $F0D9C1;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $CCF4FF;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $91D0FF;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

        { ToolBar color & color hot }
        Color.Mirror.Color := RGB(225, 234, 185);
        Color.Mirror.ColorTo := RGB(225, 234, 185);
        Color.Mirror.ColorMirror := RGB(217, 227, 166);
        Color.Mirror.ColorMirrorTo := RGB(229, 236, 195);
        //BorderColor := $DFD2C5;

        ColorHot.Color := $CFF0EA;
        ColorHot.ColorTo := $CFF0EA;
        ColorHot.Mirror.Color := RGB(234, 240, 206);
        ColorHot.Mirror.ColorTo := RGB(234, 240, 206);
        ColorHot.Mirror.ColorMirror := RGB(228, 236, 196);
        ColorHot.Mirror.ColorMirrorTo := RGB(236, 241, 211);
        ColorHot.Direction := gdVertical;
        BorderColorHot := $8CC0B1;
        BorderColor := $8CC0B1;

        { PageAppearance }
        PageAppearance.BorderColor := $8CC0B1;
        PageAppearance.Color := RGB(225, 234, 185); //$CFF0EA;
        PageAppearance.ColorTo := RGB(225, 234, 185); //$CFF0EA;
        PageAppearance.ColorMirror := RGB(217, 227, 166); //$CFF0EA;
        PageAppearance.ColorMirrorTo := RGB(234, 240, 207); //$CFF0EA;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { PagerCaption }
        PagerCaption.Color := $8CC0B1;
        PagerCaption.ColorTo := $8CC0B1;
        PagerCaption.ColorMirror := $8CC0B1;
        PagerCaption.ColorMirrorTo := $8CC0B1;
        PagerCaption.BorderColor := $8CC0B1;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := RGB(171, 188, 133);
        PageAppearance.HighLightColor := RGB(225, 234, 185);

        QATAppearance.Color := $00CFF0EA;
        QATAppearance.ColorTo := $008CC0B1;
        QATAppearance.BorderColor := $006B7760;

        QATAppearance.FullSizeColor := $008CC0B1;
        QATAppearance.FullSizeColorTo := $008CC0B1;
        QATAppearance.FullSizeBorderColor := $008CC0B1;

      end;
    bsOffice2003Silver:
      begin
        TMSStyle := tsOffice2003Silver;
        Color.Color := $ECE2E1;
        Color.ColorTo := $B39698;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := $00F7F3F3;
        DockColor.Color := $00E6D8D8;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $C8B2B3;
        RightHandleColorTo := $927476;
        RightHandleColorHot := $D3F8FF;
        RightHandleColorHotTo := $76C1FF;

        CaptionAppearance.CaptionColor := clHighLight;
        CaptionAppearance.CaptionColorTo := clHighLight;
        CaptionAppearance.CaptionBorderColor := clHighLight;
        with ButtonAppearance do
        begin
          Color := $ECE2E1;
          ColorTo := $B39698;

          {
          ColorDown := $087FE8;
          ColorDownTo := $7CDAF7;
          ColorHot := $DCFFFF;
          ColorHotTo := $5BC0F7;
          ColorChecked := $3E80FE;
          ColorCheckedTo := clNone;
          }

          ColorDown := $4E91FE;
          ColorDownTo := $91D3FF;
          ColorHot := $CCF4FF;
          ColorHotTo := $91D0FF;
          ColorChecked := $8CD5FF;
          ColorCheckedTo := $58AFFF;

          BorderDownColor := $6F4B4B;
          BorderHotColor := $6F4B4B;
          BorderCheckedColor := $6F4B4B;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;

        FloatingWindowBorderColor := $927476;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

        GlowButtonAppearance.Color := $E6E9E2; //$EDD4C0;
        GlowButtonAppearance.ColorTo := $00E6D8D8;
        GlowButtonAppearance.ColorMirror := $E6E9E2; //$EDD4C0;
        GlowButtonAppearance.ColorMirrorTo := $C8B2B3;
        GlowButtonAppearance.BorderColor := $927476;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;
        {
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
        }
        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CaptionAppearance.CaptionColorHot := $CCF4FF;
        CaptionAppearance.CaptionColorHotTo := $91D0FF;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clBlack;

        { GroupAppearance }

        GroupAppearance.TextColor := clWhite;
        GroupAppearance.Color := $C8B2B3;
        GroupAppearance.ColorTo := $927476;
        GroupAppearance.ColorMirror := $927476;
        GroupAppearance.ColorMirrorTo := $927476;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := $C2C2C2;

        GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := $927476;
        GroupAppearance.TabAppearance.BorderColor := $927476;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $00F7F3F3;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $00E6D8D8;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $00E6D8D8;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $00E6D8D8;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $CCF4FF;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $91D0FF;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

        { ToolBar color & color hot }
        Color.Mirror.Color := RGB(222, 222, 235);
        Color.Mirror.ColorTo := RGB(222, 222, 235);
        Color.Mirror.ColorMirror := RGB(210, 210, 227);
        Color.Mirror.ColorMirrorTo := RGB(230, 230, 238);

        ColorHot.Color := $00E6D8D8;
        ColorHot.ColorTo := $00E6D8D8;
        ColorHot.Mirror.Color := RGB(232, 232, 240);
        ColorHot.Mirror.ColorTo := RGB(232, 232, 240);
        ColorHot.Mirror.ColorMirror := RGB(222, 222, 235);
        ColorHot.Mirror.ColorMirrorTo := RGB(237, 237, 243);
        ColorHot.Direction := gdVertical;
        BorderColorHot := $00F7F3F3;
        BorderColor := $00F7F3F3;

        { PageAppearance }
        PageAppearance.BorderColor := $927476;
        PageAppearance.Color := RGB(222, 222, 235); //$00F7F3F3;  //ch: Khan
        PageAppearance.ColorTo := RGB(222, 222, 235); //$00E6D8D8; 
        PageAppearance.ColorMirror := RGB(210, 210, 227); //$00E6D8D8;
        PageAppearance.ColorMirrorTo := RGB(239, 239, 244); //$00E6D8D8;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        { PagerCaption }
        PagerCaption.Color := $00E6D8D8;
        PagerCaption.ColorTo := $00E6D8D8;
        PagerCaption.ColorMirror := $00E6D8D8;
        PagerCaption.ColorMirrorTo := $00E6D8D8;
        PagerCaption.BorderColor := $00E6D8D8;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := RGB(162, 159, 181);
        PageAppearance.HighLightColor := RGB(207, 220, 239); //$00FDF9CE;

        QATAppearance.Color := $00ECE2E1;
        QATAppearance.ColorTo := $00B39698;
        QATAppearance.BorderColor := $00927476;

        QATAppearance.FullSizeColor := $00E6D8D8;
        QATAppearance.FullSizeColorTo := $00E6D8D8;
        QATAppearance.FullSizeBorderColor := $00E6D8D8;

      end;
    bsOffice2007Luna:
      begin
        TMSStyle := tsOffice2007Luna;
        DockColor.ColorTo := $FAF1E9;
        DockColor.Color := $EDD8C7;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $00DFD2C5;
        RightHandleColorTo := $00E0C7AD;
        RightHandleColorHot := $00D3F8FF;
        RightHandleColorHotTo := $0076C1FF;
        RightHandleColorDown := $00087FE8;
        RightHandleColorDownTo := $007CDAF7;

        FloatingWindowBorderColor := $00E0C7AD;
        FloatingWindowBorderWidth := 2;

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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CompactGlowButtonAppearance.ColorHot := $F7F7EF;
        CompactGlowButtonAppearance.ColorHotTo := $F7E7D6;
        CompactGlowButtonAppearance.ColorMirrorHot := $F7EAC3;
        CompactGlowButtonAppearance.ColorMirrorHotTo := $F7E7D6;
        CompactGlowButtonAppearance.BorderColorHot := $E7D6B5;

        CompactGlowButtonAppearance.GradientHot := ggVertical;
        CompactGlowButtonAppearance.GradientMirrorHot := ggVertical;

        CompactGlowButtonAppearance.ColorDown := $DEC6AD;
        CompactGlowButtonAppearance.ColorDownTo := $CEAD8C;
        CompactGlowButtonAppearance.ColorMirrorDown := $FFD6B5;
        CompactGlowButtonAppearance.ColorMirrorDownTo := $C69473;
        CompactGlowButtonAppearance.BorderColorDown := $B7A38E;
        CompactGlowButtonAppearance.GradientDown := ggVertical;
        CompactGlowButtonAppearance.GradientMirrorDown := ggVertical;

        CompactGlowButtonAppearance.ColorChecked := $DEC6AD;
        CompactGlowButtonAppearance.ColorCheckedTo := $CEAD8C;
        CompactGlowButtonAppearance.ColorMirrorChecked := $FFD6B5;
        CompactGlowButtonAppearance.ColorMirrorCheckedTo := $C69473;
        CompactGlowButtonAppearance.BorderColorChecked := $B7A38E;
        CompactGlowButtonAppearance.GradientChecked := ggVertical;
        CompactGlowButtonAppearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.CaptionColor := $F2DAC2;
        CaptionAppearance.CaptionColorTo := $F0D9C1;
        CaptionAppearance.CaptionColorHot := $FFE0C8;
        CaptionAppearance.CaptionColorHotTo := $FFEDD6;
        CaptionAppearance.CaptionTextColor := $AA6A3E;
        CaptionAppearance.CaptionTextColorHot := $AA6A3E;

        { GroupAppearance }

        GroupAppearance.TextColor := $8B4215;
        GroupAppearance.Color := $F1DECC;
        GroupAppearance.ColorTo := $D8E2E1;
        GroupAppearance.ColorMirror := $D8E2E1;
        GroupAppearance.ColorMirrorTo := $A5E1E9;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := $C2C2C2;

        GroupAppearance.TabAppearance.ColorHot := $DDE5E4;
        GroupAppearance.TabAppearance.ColorHotTo := $FFDEC5;
        GroupAppearance.TabAppearance.ColorMirrorHot := $D5DFDD;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $A3D3E1;
        GroupAppearance.TabAppearance.Gradient := ggRadial;
        GroupAppearance.TabAppearance.GradientMirror := ggRadial;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := $8B4215;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColor := $A0BFCC;
        GroupAppearance.TabAppearance.TextColor := $8B4215;
        GroupAppearance.TabAppearance.TextColorHot := $8B4215;

        GroupAppearance.TabAppearance.ShadowColor := $00E8C7AE;
        GroupAppearance.TabAppearance.HighLightColor := $00FFFABF;
        GroupAppearance.TabAppearance.HighLightColorSelected := $0063CCF8;
        GroupAppearance.TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        GroupAppearance.TabAppearance.HighLightColorDown := $00FFFBD0;
        GroupAppearance.TabAppearance.HighLightColorHot := $00FDF4ED;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;
        GroupAppearance.PageAppearance.ShadowColor := $00C4AA96;
        GroupAppearance.PageAppearance.HighLightColor := $00FDF9CE;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $F0D9C1;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $FFE0C8;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $FFEDD6;
        GroupAppearance.CaptionAppearance.CaptionTextColor := $AA6A3E;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := $AA6A3E;

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

        DragGripStyle := dsNone;
        RoundEdges := True;
        Bevel := bvNone;
        UseBevel := False;

        { ToolBar color & color hot }
        Color.Color := $EDD8C7;
        Color.ColorTo := $F6E9D9;
        Color.Mirror.Color := $00F3E5DA;
        Color.Mirror.ColorTo := $00F0DED0;
        Color.Mirror.ColorMirror := $00EDD9C8;
        Color.Mirror.ColorMirrorTo := $00FBEDDC;
        Color.Direction := gdVertical;
        BorderColor :=  $00D7C8B7; //$DFD2C5; 

        ColorHot.Color := $FFF1E6;
        ColorHot.ColorTo := $FAEADE;
        ColorHot.Mirror.Color := $00FDEFE5;
        ColorHot.Mirror.ColorTo := $00FCF0E7;
        ColorHot.Mirror.ColorMirror := $00FBE9DB;
        ColorHot.Mirror.ColorMirrorTo := $00FBEDDC;
        ColorHot.Direction := gdVertical;
        BorderColorHot := $E0C7AD;

        FloatingWindowBorderColor := $E3B28D;
        FloatingWindowBorderWidth := 1;

        { PageAppearance }
        PageAppearance.BorderColor := $E3B28D;
        {PageAppearance.Color := $FAF1E9;   // ch: Khan
        PageAppearance.ColorTo := $EDD8C7;
        PageAppearance.ColorMirror := $EDD8C7;
        PageAppearance.ColorMirrorTo := $FFF2E7;}
        PageAppearance.Color := $00F3E5DA;
        PageAppearance.ColorTo := $00F0DED0;
        PageAppearance.ColorMirror := $00EDD9C8;
        PageAppearance.ColorMirrorTo := $00FFF4E3;

        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := $00C4AA96;
        PageAppearance.HighLightColor := $00FDF9CE;

        { PagerCaption }
        PagerCaption.Color := $ECE7E2;
        PagerCaption.ColorTo := $EDE5DE;
        PagerCaption.ColorMirror := $ECE1D8;
        PagerCaption.ColorMirrorTo := $EFEBDF;
        PagerCaption.BorderColor := $F0CAAE;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;
        PagerCaption.TextColor := $00AA6A3E;
        PagerCaption.TextColorExtended := $00797069;
        PagerCaption.Font.Size := 10;
        SysButtonColor := $B99A82;

        QATAppearance.Color := $00F4E7DE; //$F8EADC;  // ch: Khan
        QATAppearance.ColorTo := $00EED8C9; //$F8EADC;
        QATAppearance.BorderColor := $00D5B39A; //$B2A69F;

        QATAppearance.FullSizeColor := $EDCDB2;
        QATAppearance.FullSizeColorTo := $EAC5AA;
        QATAppearance.FullSizeBorderColor := $CDA17E;

        with ButtonAppearance do
        begin
          Color := $CFF0EA;
          ColorTo := $8CC0B1;
          {
          ColorDown := $087FE8;
          ColorDownTo := $7CDAF7;
          ColorHot := $DCFFFF;
          ColorHotTo := $5BC0F7;
          ColorChecked := $3E80FE;
          ColorCheckedTo := clNone;
          }

          ColorDown := $4E91FE;
          ColorDownTo := $91D3FF;
          ColorHot := $CCF4FF;
          ColorHotTo := $91D0FF;
          ColorChecked := $8CD5FF;
          ColorCheckedTo := $58AFFF;

          BorderDownColor := $385D3F;
          BorderHotColor := $385D3F;
          BorderCheckedColor := $385D3F;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;

      end;

    bsOffice2007Obsidian:
      begin
        TMSStyle := tsOffice2007Obsidian;
        DockColor.Color := HtmlToRgb($C1C6CF);
        DockColor.ColorTo := HtmlToRgb($B4BBC5);
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $00CAC7C6;
        RightHandleColorTo := $00B4B0AE;
        RightHandleColorHot := $00D3F8FF;
        RightHandleColorHotTo := $0076C1FF;
        RightHandleColorDown := $00087FE8;
        RightHandleColorDownTo := $007CDAF7;

        FloatingWindowBorderColor := $00B4B0AE;
        FloatingWindowBorderWidth := 2;


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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        { CaptionAppearance}

        CaptionAppearance.CaptionColor := HTMLToRgb($B6B8B8);
        CaptionAppearance.CaptionColorTo := HTMLToRgb($9EA0A0);
        CaptionAppearance.CaptionColorHot := HTMLToRgb($A9AAAA);
        CaptionAppearance.CaptionColorHotTo := HTMLToRgb($6D6E6E);
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clWhite;

        { GroupAppearance }

        GroupAppearance.TextColor := clWhite;
        GroupAppearance.Color := HTMLToRgb($5F5743);
        GroupAppearance.ColorTo := HTMLToRgb($403E37);
        GroupAppearance.ColorMirror := HTMLToRgb($5F5743);
        GroupAppearance.ColorMirrorTo := HTMLToRgb($EDAE18);
        GroupAppearance.Gradient := ggRadial;
        GroupAppearance.GradientMirror := ggRadial;
        GroupAppearance.BorderColor := HTMLToRgb($737B87);

        GroupAppearance.TabAppearance.ColorHot := HTMLToRgb($78705B);;
        GroupAppearance.TabAppearance.ColorHotTo := HTMLToRgb($979695);
        GroupAppearance.TabAppearance.ColorMirrorHot := HTMLToRgb($5F5743);
        GroupAppearance.TabAppearance.ColorMirrorHotTo := HTMLToRgb($EDAE18);
        GroupAppearance.TabAppearance.Gradient := ggRadial;
        GroupAppearance.TabAppearance.GradientMirror := ggRadial;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := $8B4215;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := HTMLToRgb($9E9C96);
        GroupAppearance.TabAppearance.BorderColor := $A0BFCC;
        GroupAppearance.TabAppearance.TextColor := clWhite;
        GroupAppearance.TabAppearance.TextColorHot := clWhite;

        GroupAppearance.TabAppearance.ShadowColor := clBlack;
        GroupAppearance.TabAppearance.HighLightColor := $00959899;
        GroupAppearance.TabAppearance.HighLightColorSelected := $0063CCF8;
        GroupAppearance.TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        GroupAppearance.TabAppearance.HighLightColorDown := $00FFFBD0;
        GroupAppearance.TabAppearance.HighLightColorHot := $00959899;

        GroupAppearance.PageAppearance.Color := HTMLToRgb($FDF8EC);
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.PageAppearance.ShadowColor := clBlack;
        GroupAppearance.PageAppearance.HighLightColor := $00ECEBE7;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := HTMLToRgb($B6B8B8);
        GroupAppearance.CaptionAppearance.CaptionColorTo := HTMLToRgb($9EA0A0);
        GroupAppearance.CaptionAppearance.CaptionColorHot := HTMLToRgb($A9AAAA);
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := HTMLToRgb($6D6E6E);
        GroupAppearance.CaptionAppearance.CaptionTextColor := clWhite;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clWhite;

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

        DragGripStyle := dsNone;
        RoundEdges := True;
        Bevel := bvNone;
        UseBevel := False;

        { ToolBar color & color hot }
        Color.Color := HtmlToRgb($B4BBC5);
        Color.ColorTo := HtmlToRgb($E5ECEC);
        Color.Mirror.Color := RGB(215, 219, 224);
        Color.Mirror.ColorTo := RGB(194, 199, 207);
        Color.Mirror.ColorMirror := RGB(181, 188, 198);
        Color.Mirror.ColorMirrorTo := RGB(217, 226, 228);
        Color.Direction := gdVertical;
        BorderColor := HtmlToRgb($AEB0B4);

        FloatingWindowBorderColor := HtmlToRgb($AEB0B4);
        FloatingWindowBorderWidth := 1;

        ColorHot.Color := HtmlToRgb($EEF0F2);
        ColorHot.ColorTo := HtmlToRgb($F6F8F8);
        ColorHot.Mirror.Color := RGB(236, 238, 240);
        ColorHot.Mirror.ColorTo := RGB(232, 234, 237);
        ColorHot.Mirror.ColorMirror := RGB(223, 225, 230);
        ColorHot.Mirror.ColorMirrorTo := RGB(243, 246, 246);
        ColorHot.Direction := gdVertical;
        BorderColorHot := HtmlToRgb($AEB0B4);

        { PageAppearance }
        PageAppearance.Color := RGB(215, 219, 224); //HtmlToRgb($C1C6CF);
        PageAppearance.ColorTo := RGB(194, 199, 207); //HtmlToRgb($B4BBC5);
        PageAppearance.ColorMirror := RGB(181, 188, 198); //HtmlToRgb($B4BBC5);
        PageAppearance.ColorMirrorTo := RGB(231, 240, 240); //HtmlToRgb($E5ECEC);
        PageAppearance.BorderColor := HtmlToRgb($AEB0B4);
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := RGB(80, 80, 80);
        PageAppearance.HighLightColor := $00ECEBE7;

        { PagerCaption }
        PagerCaption.Color := HtmlToRgb($434752);
        PagerCaption.ColorTo := HtmlToRgb($3C404A);
        PagerCaption.ColorMirror := HtmlToRgb($2F3030);
        PagerCaption.ColorMirrorTo := HtmlToRgb($3E3E3E);
        PagerCaption.BorderColor := clBlack;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        PagerCaption.TextColor := $00AA6A3E;
        PagerCaption.TextColorExtended := clWhite;
        PagerCaption.Font.Size := 10;
        SysButtonColor := $B3A69C;

        //QATAppearance.Color := $00C5BBB4;
        //QATAppearance.ColorTo := $00ECECE5;
        //QATAppearance.BorderColor := $B2A69F;

        QATAppearance.Color := RGB(123, 126, 132);
        QATAppearance.ColorTo := RGB(71, 71, 71);
        QATAppearance.BorderColor := RGB(41, 41, 41);
        QATAppearance.FullSizeColor := $8F8D8A;
        QATAppearance.FullSizeColorTo := $8F8D8A;
        QATAppearance.FullSizeBorderColor := $CECCCB;
        QATSharpCurve := True;

        with ButtonAppearance do
        begin
          Color := $CFF0EA;
          ColorTo := $8CC0B1;
          {
          ColorDown := $087FE8;
          ColorDownTo := $7CDAF7;
          ColorHot := $DCFFFF;
          ColorHotTo := $5BC0F7;
          ColorChecked := $3E80FE;
          ColorCheckedTo := clNone;
          }

          ColorDown := $4E91FE;
          ColorDownTo := $91D3FF;
          ColorHot := $CCF4FF;
          ColorHotTo := $91D0FF;
          ColorChecked := $8CD5FF;
          ColorCheckedTo := $58AFFF;

          BorderDownColor := $385D3F;
          BorderHotColor := $385D3F;
          BorderCheckedColor := $385D3F;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;

      end;
    bsOffice2007Silver:
      begin
        TMSStyle := tsOffice2007Silver;
        DockColor.ColorTo :=  $F7F3F3; //$FAF1E9;
        DockColor.Color := $E5D7D7; //$EDD8C7;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $C6B0B1; //$00DFD2C5;
        RightHandleColorTo := $947779; //$00E0C7AD;
        RightHandleColorHot := $00D3F8FF;
        RightHandleColorHotTo := $0076C1FF;
        RightHandleColorDown := $00087FE8;
        RightHandleColorDownTo := $007CDAF7;

        FloatingWindowBorderColor := $C6B0B1; //$00E0C7AD;
        FloatingWindowBorderWidth := 2;

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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CompactGlowButtonAppearance.ColorHot := $F7F4F2;
        CompactGlowButtonAppearance.ColorHotTo := $EBE9E2;
        CompactGlowButtonAppearance.ColorMirrorHot := $EBEBEA;
        CompactGlowButtonAppearance.ColorMirrorHotTo := $EBE9E2;
        CompactGlowButtonAppearance.BorderColorHot := $C1B8A0;

        CompactGlowButtonAppearance.GradientHot := ggVertical;
        CompactGlowButtonAppearance.GradientMirrorHot := ggVertical;

        CompactGlowButtonAppearance.ColorDown := $CCC3BF;
        CompactGlowButtonAppearance.ColorDownTo := $D6D6D4;
        CompactGlowButtonAppearance.ColorMirrorDown := $DEE4E4;
        CompactGlowButtonAppearance.ColorMirrorDownTo := $D6D6D4;
        CompactGlowButtonAppearance.BorderColorDown := $8A8A8A;
        CompactGlowButtonAppearance.GradientDown := ggVertical;
        CompactGlowButtonAppearance.GradientMirrorDown := ggVertical;

        CompactGlowButtonAppearance.ColorChecked := $CCC3BF;
        CompactGlowButtonAppearance.ColorCheckedTo := $D6D6D4;
        CompactGlowButtonAppearance.ColorMirrorChecked := $DEE4E4;
        CompactGlowButtonAppearance.ColorMirrorCheckedTo := $D6D6D4;
        CompactGlowButtonAppearance.BorderColorChecked := $8A8A8A;
        CompactGlowButtonAppearance.GradientChecked := ggVertical;
        CompactGlowButtonAppearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.CaptionColor := $EFE3DF;
        CaptionAppearance.CaptionColorTo := $D1C7C3;
        CaptionAppearance.CaptionColorHot := $EEE2DE;
        CaptionAppearance.CaptionColorHotTo := $C7B9B3;
        CaptionAppearance.CaptionTextColor := $595453;
        CaptionAppearance.CaptionTextColorHot := $595453;
        CaptionAppearance.CaptionBorderColor := $C6B0B1;

        { GroupAppearance }

        GroupAppearance.TextColor := $5C534C;
        GroupAppearance.Color := $D1CDCA;//$E3E2E1;
        GroupAppearance.ColorTo := $C3DEE0;//$E3E2E1;
        GroupAppearance.ColorMirror := $E3E2E1;
        GroupAppearance.ColorMirrorTo := $1BE5FE;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggRadial;
        GroupAppearance.BorderColor := $B7AFAC;

        GroupAppearance.TabAppearance.ColorHot := $DCE1E0;
        GroupAppearance.TabAppearance.ColorHotTo := $9CDADF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $9CDADF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $63E2ED;
        GroupAppearance.TabAppearance.Gradient := ggRadial;
        GroupAppearance.TabAppearance.GradientMirror := ggRadial;

        GroupAppearance.TabAppearance.ColorSelected := $9FF5FF;
        GroupAppearance.TabAppearance.ColorSelectedTo := $B8F7FF;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $B8F7FF;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D0F9FE;
        GroupAppearance.TabAppearance.TextColorSelected := $5C534C;

        GroupAppearance.TabAppearance.BorderColorSelected := $AEC6C9;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := $C1BEBD;
        GroupAppearance.TabAppearance.BorderColor := $A0BFCC;
        GroupAppearance.TabAppearance.TextColor := $5C534C;
        GroupAppearance.TabAppearance.TextColorHot := $5C534C;

        GroupAppearance.PageAppearance.Color := $C8F8FF;
        GroupAppearance.PageAppearance.ColorTo := clWhite;
        GroupAppearance.PageAppearance.ColorMirror := clWhite;
        GroupAppearance.PageAppearance.ColorMirrorTo := clWhite;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $F2FCFD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := clWhite;
        GroupAppearance.ToolBarAppearance.BorderColor := $B3C2C4;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $F7FDFE;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FEFEFE;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $B3C2C4;

        GroupAppearance.CaptionAppearance.CaptionColor := $EFE3DF;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $D1C7C3;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $EEE2DE;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $C7B9B3;
        GroupAppearance.CaptionAppearance.CaptionTextColor := $595453;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := $595453;

        { TabAppearance }

        TabAppearance.BackGround.Color := $DDD4D0;
        TabAppearance.BackGround.ColorTo := clNone;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $C1BEBD;
        TabAppearance.BorderColorSelected :=  $C1BFBD; //$FFFAC1;  //ch: Khan
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
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00F7F4F3;

        DragGripStyle := dsNone;
        RoundEdges := True;
        Bevel := bvNone;
        UseBevel := False;

        { ToolBar color & color hot }
        Color.Color := $E7DCD5;
        Color.ColorTo := $F4F4EE;
        Color.Mirror.Color := RGB(241, 244, 248);
        Color.Mirror.ColorTo := RGB(227, 232, 240);
        Color.Mirror.ColorMirror := RGB(213, 220, 231);
        Color.Mirror.ColorMirrorTo := RGB(232, 239, 242);
        Color.Direction := gdVertical;
        BorderColor := $939291;

        ColorHot.Color := $F4F0EE;
        ColorHot.ColorTo := $F9F9F7;
        ColorHot.Mirror.Color := RGB(247, 248, 250);
        ColorHot.Mirror.ColorTo := RGB(243, 245, 248);
        ColorHot.Mirror.ColorMirror := RGB(236, 238, 242);
        ColorHot.Mirror.ColorMirrorTo := RGB(247, 249, 249);
        ColorHot.Direction := gdVertical;
        BorderColorHot := $91908F;

        FloatingWindowBorderColor := $C6B0B1; //$E3B28D;
        FloatingWindowBorderWidth := 1;

        { PageAppearance }
        PageAppearance.BorderColor := $C1BFBD;
        {PageAppearance.Color := $F6F1EE;
        PageAppearance.ColorTo := $E7DCD5;//$EEE6E1;
        PageAppearance.ColorMirror := $E7DCD5;
        PageAppearance.ColorMirrorTo := $F4F4EE;}
        PageAppearance.Color := RGB(241, 244, 248);
        PageAppearance.ColorTo := RGB(227, 232, 240);
        PageAppearance.ColorMirror := $00E8DED9; //RGB(213, 220, 231);
        PageAppearance.ColorMirrorTo := RGB(239, 246, 247);

        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := $00ABA49E;
        PageAppearance.HighLightColor := $00ECEBE7;

        { PagerCaption }
        PagerCaption.Color := $EBE8E7;
        PagerCaption.ColorTo := $D1CDCA;
        PagerCaption.ColorMirror := $CAC1BA;
        PagerCaption.ColorMirrorTo := $F7EEE9;
        PagerCaption.BorderColor := $989898;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;
        PagerCaption.TextColor := $00AA6A3E;
        PagerCaption.TextColorExtended := $00797069;
        PagerCaption.Font.Size := 10;
        SysButtonColor := $A59180;

        QATAppearance.Color := $00E7DCD5;
        QATAppearance.ColorTo := $00F4F4EE;
        QATAppearance.BorderColor := $00939291;

        QATAppearance.FullSizeColor := $E6DED9;
        QATAppearance.FullSizeColorTo := $E6DED9;
        QATAppearance.FullSizeBorderColor := $F3EFED;

        with ButtonAppearance do
        begin
          Color := $CFF0EA;
          ColorTo := $8CC0B1;
          {
          ColorDown := $087FE8;
          ColorDownTo := $7CDAF7;
          ColorHot := $DCFFFF;
          ColorHotTo := $5BC0F7;
          ColorChecked := $3E80FE;
          ColorCheckedTo := clNone;
          }

          ColorDown := $4E91FE;
          ColorDownTo := $91D3FF;
          ColorHot := $CCF4FF;
          ColorHotTo := $91D0FF;
          ColorChecked := $8CD5FF;
          ColorCheckedTo := $58AFFF;

          BorderDownColor := $385D3F;
          BorderHotColor := $385D3F;
          BorderCheckedColor := $385D3F;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;
        end;

      end;

    bsWhidbeyStyle:
      begin
        TMSStyle := tsWhidbey;
        Color.Color := $F5F9FA;
        Color.ColorTo := $A8C0C0;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        DockColor.Color := $D7E5E5;
        DockColor.ColorTo := $E7F2F3;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $EBEEEF;
        RightHandleColorTo := $7E9898;
        RightHandleColorHot := $EED2C1;
        RightHandleColorHotTo := clNone;
        RightHandleColorDown := $E8E6E1;
        RightHandleColorDownTo := clNone;

        CaptionAppearance.CaptionColor := $99A8AC;
        CaptionAppearance.CaptionColorTo := $99A8AC;
        CaptionAppearance.CaptionBorderColor := $828F92;

        with ButtonAppearance do
        begin
          Color := clBtnFace;
          ColorTo := clNone;

          ColorDown := $E2B598;
          ColorDownTo := clNone;

          ColorHot := $EED2C1;
          ColorHotTo := clNone;

          ColorChecked := clBtnFace;
          ColorCheckedTo := clNone;
          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderHotColor := $C56A31;
          BorderDownColor := $6F4B4B;
        end;

        FloatingWindowBorderColor := $828F92;
        FloatingWindowBorderWidth := 2;

        RoundEdges := true;
        DragGripStyle := dsDots;
        Bevel := bvNone;
        UseBevel := False;


        {AdvToolBarPager}
        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := $DFEDF0;
        GlowButtonAppearance.ColorMirror := $DFEDF0;
        GlowButtonAppearance.ColorMirrorTo := $DFEDF0;
        GlowButtonAppearance.BorderColor := $99A8AC;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;
        {
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
        }
        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CaptionAppearance.CaptionColorHot := $ADC4C4;
        CaptionAppearance.CaptionColorHotTo := $ADC4C4;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clWhite;

        { GroupAppearance }

        GroupAppearance.TextColor := clBlack;
        GroupAppearance.Color := clWhite;
        GroupAppearance.ColorTo := clWhite;
        GroupAppearance.ColorMirror := clWhite;
        GroupAppearance.ColorMirrorTo := $FFDDBB;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := clWhite;

        GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
        GroupAppearance.TabAppearance.BorderColor := clHighLight;
        GroupAppearance.TabAppearance.TextColor := clWhite;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $D9E9EC;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $D9E9EC;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $DFEDF0;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $DFEDF0;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

        { TabAppearance }

        TabAppearance.BackGround.Color := $859D9D;
        TabAppearance.BackGround.ColorTo := $ADC4C4;
        TabAppearance.BorderColor := clNone;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $E3B28D;
        TabAppearance.BorderColorSelected := clBlack;
        TabAppearance.BorderColorSelectedHot := $60CCF9;

        TabAppearance.TextColor := clWhite;
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

        {
        TabAppearance.HighLightColor := $00FFFABF;
        TabAppearance.HighLightColorSelected := $0063CCF8;
        TabAppearance.HighLightColorSelectedHot := $00BDFFFF;
        TabAppearance.HighLightColorDown := $00FFFBD0;
        TabAppearance.HighLightColorHot := $00FDF4ED;
        }
        TabAppearance.HighLightColor := clSilver;
        TabAppearance.HighLightColorSelected := clSilver;
        TabAppearance.HighLightColorSelectedHot := clSilver;
        TabAppearance.HighLightColorDown := clGray;
        TabAppearance.HighLightColorHot := clSilver;
        TabAppearance.BorderColorHot := clSilver;

        { ToolBar color & color hot }
        Color.Mirror.Color := RGB(232, 230, 210);
        Color.Mirror.ColorTo := RGB(232, 230, 210);
        Color.Mirror.ColorMirror := RGB(224, 221, 197);
        Color.Mirror.ColorMirrorTo := RGB(244, 243, 228);

        ColorHot.Color := $F5F9FA;
        ColorHot.ColorTo := $F5F9FA;
        ColorHot.Mirror.Color := RGB(241, 240, 228);
        ColorHot.Mirror.ColorTo := RGB(241, 240, 228);
        ColorHot.Mirror.ColorMirror := RGB(237, 235, 220);
        ColorHot.Mirror.ColorMirrorTo := RGB(251, 251, 247);
        ColorHot.Direction := gdVertical;
        BorderColorHot := $E0C7AD;
        BorderColor := clSilver;

        { PageAppearance }
        PageAppearance.BorderColor := clBlack;
        PageAppearance.Color := RGB(232, 230, 210); //clWhite;
        PageAppearance.ColorTo := RGB(232, 230, 210); //$D9E9EC;
        PageAppearance.ColorMirror := RGB(224, 221, 197); //$D9E9EC;
        PageAppearance.ColorMirrorTo := RGB(253, 253, 251); //clWhite;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := RGB(145, 145, 107);
        PageAppearance.HighLightColor := RGB(146, 138, 118); //$00FDF9CE;

        { PagerCaption }
        PagerCaption.Color := $859D9D;
        PagerCaption.ColorTo := $859D9D;
        PagerCaption.ColorMirror := $859D9D;
        PagerCaption.ColorMirrorTo := $859D9D;
        PagerCaption.BorderColor := $F0CAAE;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := $00F5F9FA;
        QATAppearance.ColorTo := $00A8C0C0;
        QATAppearance.BorderColor := $00828F92;

        QATAppearance.FullSizeColor := $00859D9D;
        QATAppearance.FullSizeColorTo := $00859D9D;
        QATAppearance.FullSizeBorderColor := $00828F92;

      end;

   bsOffice2003Classic:
      begin

        Color.Color := clWhite;
        Color.ColorTo := $C9D1D5;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        DockColor.Color := $D2BDB6;
        DockColor.ColorTo := $D2BDB6;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $808080;
        RightHandleColorTo := $808080;
        RightHandleColorHot := $D2BDB6;
        RightHandleColorHotTo := clNone;
        RightHandleColorDown := $B59285;
        RightHandleColorDownTo := clNone;

        CaptionAppearance.CaptionColor := $808080;
        CaptionAppearance.CaptionColorTo := $808080;
        CaptionAppearance.CaptionBorderColor := clBlack;

        with ButtonAppearance do
        begin
          Color := clBtnFace;
          ColorTo := clNone;

          ColorDown := $E2B598;
          ColorDownTo := clNone;

          ColorHot := $D2BDB6;
          ColorHotTo := clNone;

          ColorChecked := $D8D5D4;
          ColorCheckedTo := clNone;
          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderHotColor := clBlack;
          BorderDownColor := clBlack;
        end;

        FloatingWindowBorderColor := $828F92;
        FloatingWindowBorderWidth := 2;

        RoundEdges := true;
        DragGripStyle := dsDots;
        Bevel := bvNone;
        UseBevel := False;


        {AdvToolBarPager}
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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CaptionAppearance.CaptionColorHot := $D2BDB6;
        CaptionAppearance.CaptionColorHotTo := $D2BDB6;
        CaptionAppearance.CaptionTextColor := clWhite;
        CaptionAppearance.CaptionTextColorHot := clWhite;

        { GroupAppearance }

        GroupAppearance.TextColor := clWhite;
        GroupAppearance.Color := $B59285;
        GroupAppearance.ColorTo := $B59285;
        GroupAppearance.ColorMirror := $B59285;
        GroupAppearance.ColorMirrorTo := $B59285;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := clWhite;

        GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
        GroupAppearance.TabAppearance.BorderColor := clHighLight;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $D9E9EC;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $D9E9EC;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $C9D1D5;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $C9D1D5;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

        { ToolBar color & color hot }
        Color.Mirror.Color := RGB(230, 227, 223);
        Color.Mirror.ColorTo := RGB(230, 227, 223);
        Color.Mirror.ColorMirror := RGB(214, 210, 202);
        Color.Mirror.ColorMirrorTo := RGB(239, 237, 234);

        ColorHot.Color := $F5F9FA;
        ColorHot.ColorTo := $F5F9FA;
        ColorHot.Mirror.Color := RGB(246, 239, 237);
        ColorHot.Mirror.ColorTo := RGB(246, 239, 237);
        ColorHot.Mirror.ColorMirror := RGB(230, 227, 223);
        ColorHot.Mirror.ColorMirrorTo := RGB(247, 245, 244);
        ColorHot.Direction := gdVertical;
        BorderColorHot := $D8D5D4;

        { PageAppearance }
        PageAppearance.BorderColor := clBlack;
        PageAppearance.Color := RGB(230, 227, 223); //clWhite;
        PageAppearance.ColorTo := RGB(230, 227, 223); //$C9D1D5;
        PageAppearance.ColorMirror := RGB(214, 210, 202); //$C9D1D5;
        PageAppearance.ColorMirrorTo := RGB(251, 250, 250); //clWhite;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := RGB(191, 189, 185);
        PageAppearance.HighLightColor := RGB(146, 138, 118);

        { PagerCaption }
        PagerCaption.Color := $808080;
        PagerCaption.ColorTo := $859D9D;
        PagerCaption.ColorMirror := $859D9D;
        PagerCaption.ColorMirrorTo := $C9D1D5;
        PagerCaption.BorderColor := $F0CAAE;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;
        
        QATAppearance.Color := clWhite;
        QATAppearance.ColorTo := $00C9D1D5;
        QATAppearance.BorderColor := $00828F92;

        QATAppearance.FullSizeColor := clGray;
        QATAppearance.FullSizeColorTo := $00859D9D;
        QATAppearance.FullSizeBorderColor := $00828F92;

      end;

      bsWindowsVista:
      begin
        TMSStyle := tsWindowsVista;
        Color.Color := $FDF8F1;
        Color.ColorTo := $FCEFD5;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := $FCEFD5;
        DockColor.Color := $FCEFD5;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;
        RightHandleColor := $F7DAA2;
        RightHandleColorTo := $F7DAA2;
        RightHandleColorHot := $FFFDF9;
        RightHandleColorHotTo := $FFFAF0;
        RightHandleColorDown := $FEF9F0;
        RightHandleColorDownTo := $FDF0D7;

        CaptionAppearance.CaptionColor := $FBEDD3;
        CaptionAppearance.CaptionColorTo := $FBEDD3;
        CaptionAppearance.CaptionBorderColor := clHighLight;

        with ButtonAppearance do
        begin
          Color := $FDF8F1;
          ColorTo := $FCEFD5;

          ColorDown := $FEF9F0;
          ColorDownTo := $FDF0D7;
          ColorHot := $FFFDF9;
          ColorHotTo := $FFFAF0;
          ColorChecked := $FBEDD3;
          ColorCheckedTo := $FAE9C6;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderDownColor := $FEDF9A;
          BorderHotColor := $FCF2DA;
          BorderCheckedColor := $FCF2DA;
        end;

        FloatingWindowBorderColor := $FCF2DA;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

        GlowButtonAppearance.Color := $FDF8F1;
        GlowButtonAppearance.ColorTo := $FDF8F1;
        GlowButtonAppearance.ColorMirror := $FCEFD5;
        GlowButtonAppearance.ColorMirrorTo := $FCEFD5;
        GlowButtonAppearance.BorderColor := $FDDE99;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $FFFDF9;
        GlowButtonAppearance.ColorHotTo := $FFFDF9;
        GlowButtonAppearance.ColorMirrorHot := $FFFAF0;
        GlowButtonAppearance.ColorMirrorHotTo := $FFFAF0;
        GlowButtonAppearance.BorderColorHot := $FCF2DA;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $FEF9F0;
        GlowButtonAppearance.ColorDownTo := $FEF9F0;
        GlowButtonAppearance.ColorMirrorDown := $FDF0D7;
        GlowButtonAppearance.ColorMirrorDownTo := $FDF0D7;
        GlowButtonAppearance.BorderColorDown := $FEDF9A;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $FBEDD3;
        GlowButtonAppearance.ColorCheckedTo := $FBEDD3;
        GlowButtonAppearance.ColorMirrorChecked := $FAE9C6;
        GlowButtonAppearance.ColorMirrorCheckedTo := $FAE9C6;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CaptionAppearance.CaptionColorHot := $FDF0D7;
        CaptionAppearance.CaptionColorHotTo := $FEF9F0;
        CaptionAppearance.CaptionTextColor := clBlack;
        CaptionAppearance.CaptionTextColorHot := clBlack;



        { GroupAppearance }

        GroupAppearance.TextColor := clBlack;
        GroupAppearance.Color := $F7DAA2;
        GroupAppearance.ColorTo := $F7DAA2;
        GroupAppearance.ColorMirror := $F7DAA2;
        GroupAppearance.ColorMirrorTo := $F7DAA2;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := $F7DAA2;

        GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
        GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
        GroupAppearance.TabAppearance.TextColorSelected := clBlack;

        GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
        GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
        GroupAppearance.TabAppearance.BorderColor := clHighLight;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := $D6F2FE;
        GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
        GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

        GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
        GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
        GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
        GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

        GroupAppearance.CaptionAppearance.CaptionColor := $F2DAC2;
        GroupAppearance.CaptionAppearance.CaptionColorTo := $F0D9C1;
        GroupAppearance.CaptionAppearance.CaptionColorHot := $FFE0C8;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := $FFEDD6;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

        { ToolBar color & color hot }

        Color.Mirror.Color := $FEF9F0;
        Color.Mirror.ColorTo := $FEF9F0;
        Color.Mirror.ColorMirror := $FDF0D7;
        Color.Mirror.ColorMirrorTo := $FEF9F0;
        BorderColor := $FCF2DA;

        ColorHot.Color := $FDF4E6;
        ColorHot.ColorTo := $FDF4E6;
        ColorHot.Mirror.Color := $FDF0D7;
        ColorHot.Mirror.ColorTo := $FEF9F0;
        ColorHot.Mirror.ColorMirror :=  $FDF4E6;
        ColorHot.Mirror.ColorMirrorTo := $FDF4E6;
        ColorHot.Direction := gdVertical;
        BorderColorHot := $FEDF9A;


        { PageAppearance }
        PageAppearance.BorderColor := $FCF2DA;
        PageAppearance.Color := $FEF9F0;
        PageAppearance.ColorTo := $FEF9F0;
        PageAppearance.ColorMirror := $FDF0D7;
        PageAppearance.ColorMirrorTo := $FEF9F0;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := $00CCCCCC;
        PageAppearance.HighLightColor := $FCF2DA;


        { PagerCaption }
        PagerCaption.Color := $FEF9F0;
        PagerCaption.ColorTo := $FEF9F0;
        PagerCaption.ColorMirror := $FEF9F0;
        PagerCaption.ColorMirrorTo := $FEF9F0;
        PagerCaption.BorderColor := $FCF2DA;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := $FDF8F1;
        QATAppearance.ColorTo := $FCEFD5;
        QATAppearance.BorderColor := $FEDF9A;

        QATAppearance.FullSizeColor := $FCEFD5;
        QATAppearance.FullSizeColorTo := $FCEFD5;
        QATAppearance.FullSizeBorderColor := $FCEFD5;

      end;

      bsWindows7:
      begin
        TMSStyle := tsWindows7;
        if IsWin7 or not Assigned(APager) then
        begin
          Color.Color := RGB(186, 201, 219);
          Color.ColorTo := RGB(186, 201, 219);
          Color.Direction := gdVertical;
          Color.Steps := 64;
          DockColor.ColorTo := RGB(186, 201, 219);
          DockColor.Color := RGB(186, 201, 219);
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;
          RightHandleColor := $ECB58C;
          RightHandleColorTo := $ECB58C;
          RightHandleColorHot := $FDFBFA;
          RightHandleColorHotTo := $FDF3EB;
          RightHandleColorDown := $FCEBDC;
          RightHandleColorDownTo := $FCDBC1;

          CaptionAppearance.CaptionColor := RGB(220, 231, 245);
          CaptionAppearance.CaptionColorTo := RGB(220, 231, 245);
          CaptionAppearance.CaptionBorderColor := clNone;

          with ButtonAppearance do
          begin
            Color := $FCEBDC;
            ColorTo := $FCDBC1;

            ColorDown := $FCEBDC;
            ColorDownTo := $FCDBC1;
            ColorHot := $FDFBFA;
            ColorHotTo := $FDF3EB;
            ColorChecked := $FDFBFA;
            ColorCheckedTo := $FDF3EB;

            CaptionTextColor := clBlack;
            CaptionTextColorChecked := clBlack;
            CaptionTextColorDown := clBlack;
            CaptionTextColorHot := clBlack;

            BorderDownColor := $CEA27D;
            BorderHotColor := $FBD6B8;
            BorderCheckedColor := $FBD6B8;
          end;

          FloatingWindowBorderColor := $CEA27D;
          FloatingWindowBorderWidth := 2;

          RoundEdges:= true;
          DragGripStyle := dsDots;
          Bevel:= bvNone;
          UseBevel := False;

          {AdvToolBarPager}

          GlowButtonAppearance.Color := RGB(246, 248, 251);
          GlowButtonAppearance.ColorTo := RGB(230, 238, 245);
          GlowButtonAppearance.ColorMirror := RGB(223, 232, 241);
          GlowButtonAppearance.ColorMirrorTo := RGB(225, 234, 244);
          GlowButtonAppearance.BorderColor := RGB(176, 190, 210);
          GlowButtonAppearance.Gradient := ggVertical;
          GlowButtonAppearance.GradientMirror := ggVertical;

          GlowButtonAppearance.ColorHot := RGB(253, 240, 225);
          GlowButtonAppearance.ColorHotTo := RGB(255, 206, 105);
          GlowButtonAppearance.ColorMirrorHot := RGB(255, 255, 203);
          GlowButtonAppearance.ColorMirrorHotTo := RGB(255, 206, 105);
          GlowButtonAppearance.BorderColorHot := RGB(255, 183, 0);
          GlowButtonAppearance.GradientHot := ggVertical;
          GlowButtonAppearance.GradientMirrorHot := ggVertical;

          GlowButtonAppearance.ColorDown := RGB(238, 201, 139);
          GlowButtonAppearance.ColorDownTo := RGB(245, 199, 122);
          GlowButtonAppearance.ColorMirrorDown := RGB(243, 235, 147);
          GlowButtonAppearance.ColorMirrorDownTo := RGB(245, 187, 86);
          GlowButtonAppearance.BorderColorDown := RGB(194, 155, 41);
          GlowButtonAppearance.GradientDown := ggVertical;
          GlowButtonAppearance.GradientMirrorDown := ggVertical;

          GlowButtonAppearance.ColorChecked := RGB(246, 205, 152);
          GlowButtonAppearance.ColorCheckedTo := RGB(252, 208, 143);
          GlowButtonAppearance.ColorMirrorChecked := RGB(255, 196, 94);
          GlowButtonAppearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
          GlowButtonAppearance.BorderColorChecked:= RGB(194, 146, 59);
          GlowButtonAppearance.GradientChecked := ggVertical;
          GlowButtonAppearance.GradientMirrorChecked := ggVertical;

          GlowButtonAppearance.ColorDisabled := clNone;
          GlowButtonAppearance.ColorDisabledTo := clNone;
          GlowButtonAppearance.ColorMirrorDisabled := clNone;
          GlowButtonAppearance.ColorMirrorDisabledTo := clNone;
          GlowButtonAppearance.BorderColorDisabled:= clgray;
          GlowButtonAppearance.GradientChecked := ggVertical;
          GlowButtonAppearance.GradientMirrorChecked := ggVertical;

          CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

          CaptionAppearance.CaptionColorHot := RGB(235, 245, 251);
          CaptionAppearance.CaptionColorHotTo := RGB(235, 245, 251);
          CaptionAppearance.CaptionTextColor := RGB(115, 131, 153);
          CaptionAppearance.CaptionTextColorHot := RGB(115, 131, 153);

          { GroupAppearance }

          GroupAppearance.TextColor := RGB(30, 57, 91);
          GroupAppearance.Color := RGB(255, 183, 0);
          GroupAppearance.ColorTo := RGB(255, 183, 0);
          GroupAppearance.ColorMirror := RGB(255, 183, 0);
          GroupAppearance.ColorMirrorTo := RGB(255, 183, 0);
          GroupAppearance.Gradient := ggVertical;
          GroupAppearance.GradientMirror := ggVertical;
          GroupAppearance.BorderColor := RGB(255, 183, 0);

          GroupAppearance.TabAppearance.BorderColor := RGB(186, 201, 219);
          GroupAppearance.TabAppearance.BorderColorDisabled := clDkGray;
          GroupAppearance.TabAppearance.BorderColorHot := RGB(255, 183, 0);
          GroupAppearance.TabAppearance.BorderColorSelected := RGB(255, 183, 0);
          GroupAppearance.TabAppearance.BorderColorSelectedHot := RGB(255, 183, 0);
          GroupAppearance.TabAppearance.BorderColorDown := RGB(186, 201, 219);

          GroupAppearance.TabAppearance.TextColor := RGB(30, 57, 91);
          GroupAppearance.TabAppearance.TextColorHot := RGB(30, 57, 91);
          GroupAppearance.TabAppearance.TextColorSelected := RGB(21, 66, 139);
          GroupAppearance.TabAppearance.TextColorDisabled := clGray;

          GroupAppearance.TabAppearance.ColorSelected := RGB(251, 253, 255);
          GroupAppearance.TabAppearance.ColorSelectedTo := RGB(251, 253, 255);
          GroupAppearance.TabAppearance.ColorMirrorSelected := RGB(251, 253, 255);
          GroupAppearance.TabAppearance.ColorMirrorSelectedTo := RGB(251, 253, 255);

          GroupAppearance.TabAppearance.ColorDisabled := RGB(245, 245, 245);
          GroupAppearance.TabAppearance.ColorDisabledTo := RGB(245, 245, 245);
          GroupAppearance.TabAppearance.ColorMirrorDisabled := RGB(245, 245, 245);
          GroupAppearance.TabAppearance.ColorMirrorDisabledTo := RGB(245, 245, 245);

          GroupAppearance.TabAppearance.ColorHot := RGB(234, 240, 249);
          GroupAppearance.TabAppearance.ColorHotTo := RGB(234, 240, 249);
          GroupAppearance.TabAppearance.ColorMirrorHot := RGB(234, 240, 249);
          GroupAppearance.TabAppearance.ColorMirrorHotTo := RGB(234, 240, 249);

          GroupAppearance.TabAppearance.Gradient := ggVertical;
          GroupAppearance.TabAppearance.GradientDisabled := ggVertical;
          GroupAppearance.TabAppearance.GradientHot := ggVertical;
          GroupAppearance.TabAppearance.GradientMirrorDisabled := ggVertical;
          GroupAppearance.TabAppearance.GradientMirrorHot := ggVertical;
          GroupAppearance.TabAppearance.GradientMirrorSelected := ggVertical;
          GroupAppearance.TabAppearance.GradientSelected := ggVertical;

          GroupAppearance.TabAppearance.ShadowColor := RGB(220, 231, 245);
          GroupAppearance.TabAppearance.HighLightColor := clNone;
          GroupAppearance.TabAppearance.HighLightColorSelected := clNone;
          GroupAppearance.TabAppearance.HighLightColorSelectedHot := clNone;
          GroupAppearance.TabAppearance.HighLightColorDown := clNone;
          GroupAppearance.TabAppearance.HighLightColorHot := clNone;

          GroupAppearance.TabAppearance.Color := clWhite;
          GroupAppearance.TabAppearance.ColorTo := clWhite;
          GroupAppearance.TabAppearance.ColorMirror := clWhite;
          GroupAppearance.TabAppearance.ColorMirrorTo := clWhite;


          { PageAppearance }
          GroupAppearance.PageAppearance.BorderColor := RGB(186, 201, 219);
          GroupAppearance.PageAppearance.Color := RGB(251, 253, 255);
          GroupAppearance.PageAppearance.ColorTo := RGB(229, 239, 249);
          GroupAppearance.PageAppearance.ColorMirror := RGB(229, 239, 249);
          GroupAppearance.PageAppearance.ColorMirrorTo := RGB(220, 231, 245);
          GroupAppearance.PageAppearance.Gradient := ggVertical;
          GroupAppearance.PageAppearance.GradientMirror := ggVertical;

          GroupAppearance.PageAppearance.ShadowColor := clSilver;
          GroupAppearance.PageAppearance.HighLightColor := clNone;


          GroupAppearance.ToolBarAppearance.Color.Color := RGB(251, 253, 255);
          GroupAppearance.ToolBarAppearance.Color.ColorTo := RGB(220, 231, 245);
          GroupAppearance.ToolBarAppearance.BorderColor := RGB(176, 190, 210);

          GroupAppearance.ToolBarAppearance.ColorHot.Color := RGB(251, 253, 255);
          GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := RGB(235, 245, 251);
          GroupAppearance.ToolBarAppearance.BorderColorHot := RGB(176, 190, 210);

          GroupAppearance.CaptionAppearance.CaptionColor := RGB(220, 231, 245);
          GroupAppearance.CaptionAppearance.CaptionColorTo := RGB(220, 231, 245);
          GroupAppearance.CaptionAppearance.CaptionBorderColor := clNone;
          GroupAppearance.CaptionAppearance.CaptionBorderColorHot := clNone;
          GroupAppearance.CaptionAppearance.CaptionColorHot := RGB(235, 245, 251);
          GroupAppearance.CaptionAppearance.CaptionColorHotTo := RGB(235, 245, 251);
          GroupAppearance.CaptionAppearance.CaptionTextColor := RGB(115, 131, 153);
          GroupAppearance.CaptionAppearance.CaptionTextColorHot := RGB(115, 131, 153);

          { TabAppearance }

          TabAppearance.BackGround.Color := RGB(223, 233, 245);
          TabAppearance.BackGround.ColorTo := RGB(223, 233, 245);
          TabAppearance.BorderColor := RGB(186, 201, 219);
          TabAppearance.BorderColorDisabled := clDkGray;
          TabAppearance.BorderColorHot := RGB(255, 183, 0);
          TabAppearance.BorderColorSelected := RGB(186, 201, 219);
          TabAppearance.BorderColorSelectedHot := RGB(255, 183, 0);
          TabAppearance.BorderColorDown := RGB(186, 201, 219);

          TabAppearance.TextColor := RGB(30, 57, 91);
          TabAppearance.TextColorHot := RGB(30, 57, 91);
          TabAppearance.TextColorSelected := RGB(21, 66, 139);
          TabAppearance.TextColorDisabled := clGray;

          TabAppearance.ColorSelected := RGB(251, 253, 255);
          TabAppearance.ColorSelectedTo := RGB(251, 253, 255);
          TabAppearance.ColorMirrorSelected := RGB(251, 253, 255);
          TabAppearance.ColorMirrorSelectedTo := RGB(251, 253, 255);

          TabAppearance.ColorDisabled := RGB(245, 245, 245);
          TabAppearance.ColorDisabledTo := RGB(245, 245, 245);
          TabAppearance.ColorMirrorDisabled := RGB(245, 245, 245);
          TabAppearance.ColorMirrorDisabledTo := RGB(245, 245, 245);

          TabAppearance.ColorHot := RGB(234, 240, 249);
          TabAppearance.ColorHotTo := RGB(234, 240, 249);
          TabAppearance.ColorMirrorHot := RGB(234, 240, 249);
          TabAppearance.ColorMirrorHotTo := RGB(234, 240, 249);

          TabAppearance.Color := clWhite;
          TabAppearance.ColorTo := clWhite;
          TabAppearance.ColorMirror := clWhite;
          TabAppearance.ColorMirrorTo := clWhite;

          TabAppearance.Gradient := ggVertical;
          TabAppearance.GradientDisabled := ggVertical;
          TabAppearance.GradientHot := ggVertical;
          TabAppearance.GradientMirrorDisabled := ggVertical;
          TabAppearance.GradientMirrorHot := ggVertical;
          TabAppearance.GradientMirrorSelected := ggVertical;
          TabAppearance.GradientSelected := ggVertical;

          TabAppearance.ShadowColor := RGB(220, 231, 245);
          TabAppearance.HighLightColor := clNone;
          TabAppearance.HighLightColorSelected := clNone;
          TabAppearance.HighLightColorSelectedHot := clNone;
          TabAppearance.HighLightColorDown := clNone;
          TabAppearance.HighLightColorHot := clNone;

          { ToolBar color & color hot }
          Color.Mirror.Color := RGB(251, 253, 255);
          Color.Mirror.ColorTo := RGB(229, 239, 249);
          Color.Mirror.ColorMirror := RGB(229, 239, 249);
          Color.Mirror.ColorMirrorTo := RGB(220, 231, 245);
          BorderColor := RGB(186, 201, 219);

          ColorHot.Color := RGB(251, 253, 255);
          ColorHot.ColorTo := RGB(251, 253, 255);
          ColorHot.Mirror.Color := RGB(253, 255, 255);
          ColorHot.Mirror.ColorTo := RGB(235, 245, 251);
          ColorHot.Mirror.ColorMirror := RGB(235, 245, 251);
          ColorHot.Mirror.ColorMirrorTo := RGB(235, 245, 251);
          ColorHot.Direction := gdVertical;
          BorderColorHot := RGB(186, 201, 219);

          { PageAppearance }
          PageAppearance.BorderColor := RGB(186, 201, 219);
          PageAppearance.Color := RGB(251, 253, 255);
          PageAppearance.ColorTo := RGB(229, 239, 249);
          PageAppearance.ColorMirror := RGB(229, 239, 249);
          PageAppearance.ColorMirrorTo := RGB(220, 231, 245);
          PageAppearance.Gradient := ggVertical;
          PageAppearance.GradientMirror := ggVertical;

          PageAppearance.ShadowColor := clSilver;
          PageAppearance.HighLightColor := clNone;

          { PagerCaption }
          PagerCaption.Color := RGB(223, 233, 245);
          PagerCaption.ColorTo := RGB(223, 233, 245);
          PagerCaption.ColorMirror := RGB(223, 233, 245);
          PagerCaption.ColorMirrorTo := RGB(223, 233, 245);
          PagerCaption.BorderColor := RGB(186, 201, 219);
          PagerCaption.Gradient := ggVertical;
          PagerCaption.GradientMirror := ggVertical;

          QATAppearance.Color := RGB(223, 233, 245);
          QATAppearance.ColorTo := RGB(223, 233, 245);
          QATAppearance.BorderColor := RGB(178, 191, 210);

          QATAppearance.FullSizeColor := RGB(223, 233, 245);
          QATAppearance.FullSizeColorTo := RGB(223, 233, 245);
          QATAppearance.FullSizeBorderColor := RGB(178, 191, 210);
        end
        else
        begin
          Color.Color := $FCEBDC;
          Color.ColorTo := $FCDBC1;
          Color.Direction := gdVertical;
          Color.Steps := 64;
          DockColor.ColorTo := $FCDBC1;
          DockColor.Color := $FCDBC1;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;
          RightHandleColor := $ECB58C;
          RightHandleColorTo := $ECB58C;
          RightHandleColorHot := $FDFBFA;
          RightHandleColorHotTo := $FDF3EB;
          RightHandleColorDown := $FCEBDC;
          RightHandleColorDownTo := $FCDBC1;

          CaptionAppearance.CaptionColor := $FCDBC1;
          CaptionAppearance.CaptionColorTo := $FCDBC1;
          CaptionAppearance.CaptionBorderColor := clHighLight;

          with ButtonAppearance do
          begin
            Color := $FCEBDC;
            ColorTo := $FCDBC1;

            ColorDown := $FCEBDC;
            ColorDownTo := $FCDBC1;
            ColorHot := $FDFBFA;
            ColorHotTo := $FDF3EB;
            ColorChecked := $FDFBFA;
            ColorCheckedTo := $FDF3EB;

            CaptionTextColor := clBlack;
            CaptionTextColorChecked := clBlack;
            CaptionTextColorDown := clBlack;
            CaptionTextColorHot := clBlack;

            BorderDownColor := $CEA27D;
            BorderHotColor := $FBD6B8;
            BorderCheckedColor := $FBD6B8;
          end;

          FloatingWindowBorderColor := $CEA27D;
          FloatingWindowBorderWidth := 2;

          RoundEdges:= true;
          DragGripStyle := dsDots;
          Bevel:= bvNone;
          UseBevel := False;

          {AdvToolBarPager}

          GlowButtonAppearance.Color := clWhite;
          GlowButtonAppearance.ColorTo := clWhite;
          GlowButtonAppearance.ColorMirror := clSilver;
          GlowButtonAppearance.ColorMirrorTo := clWhite;
          GlowButtonAppearance.BorderColor := $E0B99B;
          GlowButtonAppearance.Gradient := ggVertical;
          GlowButtonAppearance.GradientMirror := ggVertical;

          GlowButtonAppearance.ColorHot := $FDFBFA;
          GlowButtonAppearance.ColorHotTo := $FDF3EB;
          GlowButtonAppearance.ColorMirrorHot := clNone;;
          GlowButtonAppearance.ColorMirrorHotTo := clNone;;
          GlowButtonAppearance.BorderColorHot := $FBD6B8;
          GlowButtonAppearance.GradientHot := ggVertical;
          GlowButtonAppearance.GradientMirrorHot := ggVertical;

          GlowButtonAppearance.ColorDown := $FCEBDC;
          GlowButtonAppearance.ColorDownTo := $FCDBC1;
          GlowButtonAppearance.ColorMirrorDown := clNone;
          GlowButtonAppearance.ColorMirrorDownTo := clNone;
          GlowButtonAppearance.BorderColorDown := $CEA27D;
          GlowButtonAppearance.GradientDown := ggVertical;
          GlowButtonAppearance.GradientMirrorDown := ggVertical;

          GlowButtonAppearance.ColorChecked := $FDFBFA;
          GlowButtonAppearance.ColorCheckedTo := $FDF3EB;
          GlowButtonAppearance.ColorMirrorChecked := clNone;
          GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
          GlowButtonAppearance.BorderColorChecked:= clHighLight;
          GlowButtonAppearance.GradientChecked := ggVertical;
          GlowButtonAppearance.GradientMirrorChecked := ggVertical;

          CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

          CaptionAppearance.CaptionColorHot := $FCEBDC;
          CaptionAppearance.CaptionColorHotTo := $FCDBC1;
          CaptionAppearance.CaptionTextColor := clBlack;
          CaptionAppearance.CaptionTextColorHot := clBlack;


          { GroupAppearance }

          GroupAppearance.TextColor := clBlack;
          GroupAppearance.Color := $ECB58C;
          GroupAppearance.ColorTo := $ECB58C;
          GroupAppearance.ColorMirror := $ECB58C;
          GroupAppearance.ColorMirrorTo := $ECB58C;
          GroupAppearance.Gradient := ggVertical;
          GroupAppearance.GradientMirror := ggVertical;
          GroupAppearance.BorderColor := $ECB58C;

          GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
          GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
          GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
          GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
          GroupAppearance.TabAppearance.Gradient := ggVertical;
          GroupAppearance.TabAppearance.GradientMirror := ggVertical;

          GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
          GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
          GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
          GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
          GroupAppearance.TabAppearance.TextColorSelected := clBlack;

          GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
          GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
          GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
          GroupAppearance.TabAppearance.BorderColor := clHighLight;
          GroupAppearance.TabAppearance.TextColor := clBlack;
          GroupAppearance.TabAppearance.TextColorHot := clBlack;

          GroupAppearance.PageAppearance.Color := $D6F2FE;
          GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
          GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

          GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
          GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
          GroupAppearance.PageAppearance.Gradient := ggVertical;
          GroupAppearance.PageAppearance.GradientMirror := ggVertical;

          GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
          GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
          GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

          GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
          GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
          GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

          GroupAppearance.CaptionAppearance.CaptionColor := $F2DAC2;
          GroupAppearance.CaptionAppearance.CaptionColorTo := $F0D9C1;
          GroupAppearance.CaptionAppearance.CaptionColorHot := $FFE0C8;
          GroupAppearance.CaptionAppearance.CaptionColorHotTo := $FFEDD6;
          GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
          GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

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

          { ToolBar color & color hot }
          Color.Mirror.Color := $FCEBDC;
          Color.Mirror.ColorTo := $FCEBDC;
          Color.Mirror.ColorMirror := $FCDBC1;
          Color.Mirror.ColorMirrorTo := $FCEBDC;
          BorderColor := $CEA27D;

          ColorHot.Color := $FDFBFA;
          ColorHot.ColorTo := $FDF3EB;
          ColorHot.Mirror.Color := $FCEBDC;
          ColorHot.Mirror.ColorTo := $FCDBC1;
          ColorHot.Mirror.ColorMirror := $FCEBDC;
          ColorHot.Mirror.ColorMirrorTo := $FCEBDC;
          ColorHot.Direction := gdVertical;
          BorderColorHot := $E3B28D;


          { PageAppearance }
          PageAppearance.BorderColor := $CEA27D;
          PageAppearance.Color := $FCEBDC;
          PageAppearance.ColorTo := $FCEBDC;
          PageAppearance.ColorMirror := $FCDBC1;
          PageAppearance.ColorMirrorTo := $FCEBDC;
          PageAppearance.Gradient := ggVertical;
          PageAppearance.GradientMirror := ggVertical;

          PageAppearance.ShadowColor := $00AAAAAA;
          PageAppearance.HighLightColor := $CEA27D;


          { PagerCaption }
          PagerCaption.Color := $FCEBDC;
          PagerCaption.ColorTo := $FCEBDC;
          PagerCaption.ColorMirror := $FCEBDC;
          PagerCaption.ColorMirrorTo := $FCEBDC;
          PagerCaption.BorderColor := $CEA27D;
          PagerCaption.Gradient := ggVertical;
          PagerCaption.GradientMirror := ggVertical;

          QATAppearance.Color := $FCEBDC;
          QATAppearance.ColorTo := $FCDBC1;
          QATAppearance.BorderColor := $CEA27D;

          QATAppearance.FullSizeColor := $FCDBC1;
          QATAppearance.FullSizeColorTo := $FCDBC1;
          QATAppearance.FullSizeBorderColor := $FCDBC1;
        end;
      end;
      bsTerminal:
      begin
        TMSStyle := tsTerminal;
        Color.Color := clBtnFace;
        Color.ColorTo := clBtnFace;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := clBtnFace;
        DockColor.Color := clBtnFace;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;
        RightHandleColor := clGray;
        RightHandleColorTo := clGray;
        RightHandleColorHot := clSilver;
        RightHandleColorHotTo := clSilver;
        RightHandleColorDown := clHighLight;
        RightHandleColorDownTo := clHighLight;

        CaptionAppearance.CaptionColor := clSilver;
        CaptionAppearance.CaptionColorTo := clSilver;
        CaptionAppearance.CaptionBorderColor := clHighLight;

        with ButtonAppearance do
        begin
          Color := clBtnFace;
          ColorTo := clBtnFace;

          ColorDown := clHighLight;
          ColorDownTo := clHighLight;
          ColorHot := clSilver;
          ColorHotTo := clSilver;
          ColorChecked := clWhite;
          ColorCheckedTo := clWhite;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderDownColor := clGray;
          BorderHotColor := clGray;
          BorderCheckedColor := clGray;
        end;

        FloatingWindowBorderColor := clGray;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

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
        GlowButtonAppearance.BorderColorChecked:= clGray;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        CaptionAppearance.CaptionColorHot := clHighLight;
        CaptionAppearance.CaptionColorHotTo := clHighLight;
        CaptionAppearance.CaptionTextColor := clBlack;
        CaptionAppearance.CaptionTextColorHot := clWhite;


        { GroupAppearance }

        GroupAppearance.TextColor := clBlack;
        GroupAppearance.Color := clSilver;
        GroupAppearance.ColorTo := clSilver;
        GroupAppearance.ColorMirror := clSilver;
        GroupAppearance.ColorMirrorTo := clSilver;
        GroupAppearance.Gradient := ggVertical;
        GroupAppearance.GradientMirror := ggVertical;
        GroupAppearance.BorderColor := clGray;

        GroupAppearance.TabAppearance.ColorHot := clSilver;
        GroupAppearance.TabAppearance.ColorHotTo := clSilver;
        GroupAppearance.TabAppearance.ColorMirrorHot := clSilver;
        GroupAppearance.TabAppearance.ColorMirrorHotTo := clSilver;
        GroupAppearance.TabAppearance.Gradient := ggVertical;
        GroupAppearance.TabAppearance.GradientMirror := ggVertical;

        GroupAppearance.TabAppearance.ColorSelected := clHighlight;
        GroupAppearance.TabAppearance.ColorSelectedTo := clHighlight;
        GroupAppearance.TabAppearance.ColorMirrorSelected := clHighlight;
        GroupAppearance.TabAppearance.ColorMirrorSelectedTo := clHighlight;
        GroupAppearance.TabAppearance.TextColorSelected := clWhite;

        GroupAppearance.TabAppearance.BorderColorSelected := clGray;
        GroupAppearance.TabAppearance.BorderColorSelectedHot := clGray;
        GroupAppearance.TabAppearance.BorderColorHot := clGray;
        GroupAppearance.TabAppearance.BorderColor := clGray;
        GroupAppearance.TabAppearance.TextColor := clBlack;
        GroupAppearance.TabAppearance.TextColorHot := clBlack;

        GroupAppearance.PageAppearance.Color := clSilver;
        GroupAppearance.PageAppearance.ColorTo := clSilver;
        GroupAppearance.PageAppearance.BorderColor := clGray;

        GroupAppearance.PageAppearance.ColorMirror := clSilver;
        GroupAppearance.PageAppearance.ColorMirrorTo := clSilver;
        GroupAppearance.PageAppearance.Gradient := ggVertical;
        GroupAppearance.PageAppearance.GradientMirror := ggVertical;

        GroupAppearance.ToolBarAppearance.Color.Color := clBtnface;
        GroupAppearance.ToolBarAppearance.Color.ColorTo := clBtnface;
        GroupAppearance.ToolBarAppearance.BorderColor := clGray;

        GroupAppearance.ToolBarAppearance.ColorHot.Color := clBtnface;
        GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := clBtnface;
        GroupAppearance.ToolBarAppearance.BorderColorHot := clGray;

        GroupAppearance.CaptionAppearance.CaptionColor := clSilver;
        GroupAppearance.CaptionAppearance.CaptionColorTo := clSilver;
        GroupAppearance.CaptionAppearance.CaptionColorHot := clHighLight;
        GroupAppearance.CaptionAppearance.CaptionColorHotTo := clHighLight;
        GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
        GroupAppearance.CaptionAppearance.CaptionTextColorHot := clWhite;

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


        { ToolBar color & color hot }
        Color.Mirror.Color := clBtnface;
        Color.Mirror.ColorTo := clBtnface;
        Color.Mirror.ColorMirror := clBtnface;;
        Color.Mirror.ColorMirrorTo := clBtnface;
        BorderColor := clGray;

        ColorHot.Color := clBtnface;
        ColorHot.ColorTo := clBtnface;
        ColorHot.Mirror.Color := clBtnface;
        ColorHot.Mirror.ColorTo := clBtnface;
        ColorHot.Mirror.ColorMirror := clBtnface;
        ColorHot.Mirror.ColorMirrorTo := clBtnface;
        ColorHot.Direction := gdVertical;
        BorderColorHot := clGray;


        { PageAppearance }
        PageAppearance.BorderColor := clGray;
        PageAppearance.Color := clBtnFace;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clBtnFace;
        PageAppearance.ColorMirrorTo := clBtnFace;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := clGray;
        PageAppearance.HighLightColor := clSilver;


        { PagerCaption }
        PagerCaption.Color := clBtnFace;
        PagerCaption.ColorTo := clBtnFace;
        PagerCaption.ColorMirror := clBtnFace;
        PagerCaption.ColorMirrorTo := clBtnFace;
        PagerCaption.BorderColor := clGray;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := clBtnFace;
        QATAppearance.ColorTo := clBtnFace;
        QATAppearance.BorderColor := clGray;

        QATAppearance.FullSizeColor := clBtnFace;
        QATAppearance.FullSizeColorTo := clBtnFace;
        QATAppearance.FullSizeBorderColor := clBtnFace;

      end;

    bsOffice2010Blue:
      begin
        TMSStyle := tsOffice2010Blue;
        Color.Color := $FDF6EF;
        Color.ColorTo := $F0DAC7;
        Color.Direction := gdVertical;
        Color.Steps := 64;
        DockColor.ColorTo := $FDF6EF;
        DockColor.Color := $F0DAC7;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $FDF6EF;
        RightHandleColorTo := $F0DAC7;
        RightHandleColorHot := $D9F9FD;
        RightHandleColorHotTo := $8AE3FD;
        RightHandleColorDown := $7BEEFF;
        RightHandleColorDownTo := $6CD0FF;

        CaptionAppearance.CaptionColor := $F6E9DC;
        CaptionAppearance.CaptionColorTo := $F6E9DC;
        CaptionAppearance.CaptionBorderColor := $F6E9DC;
        CaptionAppearance.CaptionColorHot := $F5EFE9;
        CaptionAppearance.CaptionColorHotTo := $F5EFE9;
        CaptionAppearance.CaptionBorderColorHot := $F5EFE9;
        CaptionAppearance.CaptionTextColor := $5B391E;
        CaptionAppearance.CaptionTextColorHot := $5B391E;

        with ButtonAppearance do
        begin
          Color := $FDF6EF;
          ColorTo := $F0DAC7;

          ColorDown := $6CD0FF;
          ColorDownTo := $7BEEFF;
          ColorHot := $8AE3FD;
          ColorHotTo := $D9F9FD;
          ColorChecked := $6CD0FF;
          ColorCheckedTo := $6CD0FF;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderDownColor := $308AC2;
          BorderHotColor := $58CAF1;
          BorderCheckedColor := $308AC2;
        end;

        FloatingWindowBorderColor := $D6C0B1;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        { TabAppearance }

        TabAppearance.BackGround.Color := $FAF4ED;
        TabAppearance.BackGround.ColorTo := $FEFCFA;

        TabAppearance.BorderColor := $C7B29F;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $BAB5B1;
        TabAppearance.BorderColorSelected := $DBC9B8;
        TabAppearance.BorderColorSelectedHot := $DBC9B8;

        TabAppearance.TextColor := $5B391E;
        TabAppearance.TextColorHot := $5B391E;
        TabAppearance.TextColorSelected := $5B391E;

        if IsWin8 then
          TabAppearance.TextColorDisabled := clSilver
        else
          TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FFFAF5;
        TabAppearance.ColorSelectedTo := $FFFAF5;
        TabAppearance.ColorMirrorSelected := $FFFAF5;
        TabAppearance.ColorMirrorSelectedTo := $FFFAF5;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $FAF4ED;
        TabAppearance.ColorHotTo := $FEFCF7;
        TabAppearance.ColorMirrorHot := $FEFCFA;
        TabAppearance.ColorMirrorHotTo := $FEFCFA;

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

        { ToolBar color & color hot }
        Color.Mirror.Color := $FFFAF5;
        Color.Mirror.ColorTo := $FAEFE5;
        Color.Mirror.ColorMirror := $FAEFE5;
        Color.Mirror.ColorMirrorTo := $F6E9DC;
        BorderColor := $C7B29F;

        ColorHot.Color := $F7EDE3;
        ColorHot.ColorTo := $FDF6EF;
        ColorHot.Mirror.Color := $FFFAF5;
        ColorHot.Mirror.ColorTo := $FAEFE5;
        ColorHot.Mirror.ColorMirror := $FAEFE5;
        ColorHot.Mirror.ColorMirrorTo := $F5EFE9;
        ColorHot.Direction := gdVertical;
        BorderColorHot := $D6C0B1;

        { PageAppearance }
        PageAppearance.Color := $FFFAF5;
        PageAppearance.ColorTo := $FAEFE5;
        PageAppearance.ColorMirror := $FAEFE5;
        PageAppearance.ColorMirrorTo := $F6E9DC;
        PageAppearance.BorderColor := $C7B29F;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := $E6D1BF;
        PageAppearance.HighLightColor := $CCB9A9;


        { PagerCaption }
        PagerCaption.Color := $FFFAF5;
        PagerCaption.ColorTo := $EAD3BF;
        PagerCaption.ColorMirror := $EAD3BF;
        PagerCaption.ColorMirrorTo := $EAD3BF;
        PagerCaption.BorderColor := $C7B29F;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := $FDF6EF;
        QATAppearance.ColorTo := $F0DAC7;
        QATAppearance.BorderColor := $C7B29F;

        QATAppearance.FullSizeColor := $FDF6EF;
        QATAppearance.FullSizeColorTo := $F0DAC7;
        QATAppearance.FullSizeBorderColor := $C7B29F;
      end;
    bsOffice2010Silver:
      begin
        TMSStyle := tsOffice2010Silver;
        Color.Color := $FFFFFF;
        Color.ColorTo := $EDE5E0;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        DockColor.ColorTo := $FFFFFF;
        DockColor.Color := $EDE5E0;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $FFFFFF;
        RightHandleColorTo := $EDE5E0;
        RightHandleColorHot := $D9F9FD;
        RightHandleColorHotTo := $8AE3FD;
        RightHandleColorDown := $7BEEFF;
        RightHandleColorDownTo := $6CD0FF;

        CaptionAppearance.CaptionColor := $F2EFED;
        CaptionAppearance.CaptionColorTo := $F2EFED;
        CaptionAppearance.CaptionBorderColor := $F2EFED;
        CaptionAppearance.CaptionBorderColorHot := $F2EFED;
        CaptionAppearance.CaptionColorHot := $F7F5F3;
        CaptionAppearance.CaptionColorHotTo := $F7F5F3;
        CaptionAppearance.CaptionTextColor := $675F58;
        CaptionAppearance.CaptionTextColorHot := $675F58;

        with ButtonAppearance do
        begin
          Color := $FFFFFF;
          ColorTo := $EDE5E0;

          ColorDown := $6CD0FF;
          ColorDownTo := $7BEEFF;
          ColorHot := $8AE3FD;
          ColorHotTo := $D9F9FD;
          ColorChecked := $6CD0FF;
          ColorCheckedTo := $6CD0FF;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderDownColor := $308AC2;
          BorderHotColor := $58CAF1;
          BorderCheckedColor := $308AC2;
        end;

        FloatingWindowBorderColor := $CDCAC6;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

       { TabAppearance }
        TabAppearance.BackGround.Color := $FAF4ED;
        TabAppearance.BackGround.ColorTo := $FEFCFA;

        TabAppearance.BorderColor := $D2CDC8;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $BAB5B1;
        TabAppearance.BorderColorSelected := $BFBAB6;
        TabAppearance.BorderColorSelectedHot := $BAB5B1;

        TabAppearance.TextColor := $615F5E;
        TabAppearance.TextColorHot := $675F58;
        TabAppearance.TextColorSelected := $675F58;

        if IsWin8 then
          TabAppearance.TextColorDisabled := clSilver
        else
          TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $FFFFFE;
        TabAppearance.ColorSelectedTo := $FFFFFE;
        TabAppearance.ColorMirrorSelected := $FFFFFE;
        TabAppearance.ColorMirrorSelectedTo := $FFFFFE;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $FFFFFE;
        TabAppearance.ColorHotTo := $FFFFFE;
        TabAppearance.ColorMirrorHot := $FFFFFE;
        TabAppearance.ColorMirrorHotTo := $FFFFFE;

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
        TabAppearance.HighLightColorHot := $FFFFFE;

        { ToolBar color & color hot }
        Color.Mirror.Color :=$FFFFFE;
        Color.Mirror.ColorTo := $FFFFFE;
        Color.Mirror.ColorMirror := $FFFFFE;
        Color.Mirror.ColorMirrorTo := $F2EFED;

        ColorHot.Color := $FFFFFE;
        ColorHot.ColorTo := $FFFFFE;
        ColorHot.Mirror.Color := $FFFFFE;
        ColorHot.Mirror.ColorTo := $FFFFFE;
        ColorHot.Mirror.ColorMirror := $FFFFFE;
        ColorHot.Mirror.ColorMirrorTo := $F7F5F3;
        ColorHot.Direction := gdVertical;
        BorderColorHot := $EDE5E0;


        { PageAppearance }
        PageAppearance.Color := $FFFFFE;
        PageAppearance.ColorTo := $FFFFFE;
        PageAppearance.ColorMirror := $FFFFFE;
        PageAppearance.ColorMirrorTo := $F2EFED;
        PageAppearance.BorderColor := $F2EFED;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := clGray;
        PageAppearance.HighLightColor := clSilver;


        { PagerCaption }
        PagerCaption.Color := $D4CFCB;
        PagerCaption.ColorTo := $D4CFCB;
        PagerCaption.ColorMirror := $D4CFCB;
        PagerCaption.ColorMirrorTo := $D4CFCB;
        PagerCaption.BorderColor := $D2CDC8;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := $FFFFFF;
        QATAppearance.ColorTo := $EDE5E0;
        QATAppearance.BorderColor := $D2CDC8;

        QATAppearance.FullSizeColor := $FFFFFF;
        QATAppearance.FullSizeColorTo := $EDE5E0;
        QATAppearance.FullSizeBorderColor := $D2CDC8;

      end;
    bsOffice2010Black:
      begin
        TMSStyle := tsOffice2010Black;
        Color.Color := $BFBFBF;
        Color.ColorTo := $919191;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        DockColor.ColorTo := $BFBFBF;
        DockColor.Color := $919191;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        RightHandleColor := $BFBFBF;
        RightHandleColorTo := $919191;
        RightHandleColorHot := $D9F9FD;
        RightHandleColorHotTo := $8AE3FD;
        RightHandleColorDown := $7BEEFF;
        RightHandleColorDownTo := $6CD0FF;

        CaptionAppearance.CaptionColor := $A7A7A7;
        CaptionAppearance.CaptionColorTo := $A7A7A7;
        CaptionAppearance.CaptionBorderColor := $A7A7A7;
        CaptionAppearance.CaptionBorderColorHot := $B7B7B7;
        CaptionAppearance.CaptionColorHot := $B7B7B7;
        CaptionAppearance.CaptionColorHotTo := $B7B7B7;
        CaptionAppearance.CaptionTextColor := $242424;
        CaptionAppearance.CaptionTextColorHot := $242424;

        with ButtonAppearance do
        begin
          Color := $BFBFBF;
          ColorTo := $919191;

          ColorDown := $6CD0FF;
          ColorDownTo := $7BEEFF;
          ColorHot := $8AE3FD;
          ColorHotTo := $D9F9FD;
          ColorChecked := $6CD0FF;
          ColorCheckedTo := $6CD0FF;

          CaptionTextColor := clBlack;
          CaptionTextColorChecked := clBlack;
          CaptionTextColorDown := clBlack;
          CaptionTextColorHot := clBlack;

          BorderDownColor := $308AC2;
          BorderHotColor := $58CAF1;
          BorderCheckedColor := $308AC2;
        end;

        FloatingWindowBorderColor := $848484;
        FloatingWindowBorderWidth := 2;

        RoundEdges:= true;
        DragGripStyle := dsDots;
        Bevel:= bvNone;
        UseBevel := False;

        {AdvToolBarPager}

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

        CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

        { TabAppearance }

        TabAppearance.BackGround.Color := $CFCFCF;
        TabAppearance.BackGround.ColorTo := $CFCFCF;
        TabAppearance.BorderColor := $5E5E5E;
        TabAppearance.BorderColorDisabled := clNone;
        TabAppearance.BorderColorHot := $5E5E5E;
        TabAppearance.BorderColorSelected := $5E5E5E;
        TabAppearance.BorderColorSelectedHot := $5E5E5E;

        TabAppearance.TextColor := clBlack;
        TabAppearance.TextColorHot := $00F4F4F4; //clWhite;
        TabAppearance.TextColorSelected := clBlack;

        if IsWin8 then
          TabAppearance.TextColorDisabled := clSilver
        else
          TabAppearance.TextColorDisabled := clGray;

        TabAppearance.ColorSelected := $CFCFCF;
        TabAppearance.ColorSelectedTo := $CFCFCF;
        TabAppearance.ColorMirrorSelected := $CFCFCF;
        TabAppearance.ColorMirrorSelectedTo := $CFCFCF;

        TabAppearance.ColorDisabled := $00F2F2F2;
        TabAppearance.ColorDisabledTo := $00F2F2F2;
        TabAppearance.ColorMirrorDisabled := $00F2F2F2;
        TabAppearance.ColorMirrorDisabledTo := $00F2F2F2;

        TabAppearance.ColorHot := $999086;
        TabAppearance.ColorHotTo := $999086;
        TabAppearance.ColorMirrorHot := $999086;
        TabAppearance.ColorMirrorHotTo := $585B5D;

        TabAppearance.Gradient := ggVertical;
        TabAppearance.GradientDisabled := ggVertical;
        TabAppearance.GradientHot := ggVertical;
        TabAppearance.GradientMirrorDisabled := ggVertical;
        TabAppearance.GradientMirrorHot := ggVertical;
        TabAppearance.GradientMirrorSelected := ggVertical;
        TabAppearance.GradientSelected := ggVertical;

        TabAppearance.ShadowColor := $BEBEBE;
        TabAppearance.HighLightColorSelected := clWhite;
        TabAppearance.HighLightColorSelectedHot := clWhite;
        TabAppearance.HighLightColorDown := clWhite;
        TabAppearance.HighLightColorHot := $999086;

        { ToolBar color & color hot }
        Color.Mirror.Color := $CFCFCF;
        Color.Mirror.ColorTo := $CFCFCF;
        Color.Mirror.ColorMirror := $CFCFCF;
        Color.Mirror.ColorMirrorTo := $A7A7A7;
        BorderColor := $6D6D6D;

        ColorHot.Color := $CFCFCF;
        ColorHot.ColorTo := $CFCFCF;
        ColorHot.Mirror.Color := $CFCFCF;
        ColorHot.Mirror.ColorTo := $CFCFCF;
        ColorHot.Mirror.ColorMirror := $CFCFCF;
        ColorHot.Mirror.ColorMirrorTo := $B7B7B7;
        ColorHot.Direction := gdVertical;
        BorderColorHot := $6D6D6D;


        { PageAppearance }
        PageAppearance.Color := $CFCFCF;
        PageAppearance.ColorTo := $CFCFCF;
        PageAppearance.ColorMirror := $CFCFCF;
        PageAppearance.ColorMirrorTo := $A7A7A7;
        PageAppearance.BorderColor := $6D6D6D;
        PageAppearance.Gradient := ggVertical;
        PageAppearance.GradientMirror := ggVertical;

        PageAppearance.ShadowColor := clGray;
        PageAppearance.HighLightColor := clSilver;

        { PagerCaption }
        PagerCaption.Color := $656565;
        PagerCaption.ColorTo := $656565;
        PagerCaption.ColorMirror := $656565;
        PagerCaption.ColorMirrorTo := $656565;
        PagerCaption.BorderColor := $6D6D6D;
        PagerCaption.Gradient := ggVertical;
        PagerCaption.GradientMirror := ggVertical;

        QATAppearance.Color := $BFBFBF;
        QATAppearance.ColorTo := $919191;
        QATAppearance.BorderColor := $6D6D6D;

        QATAppearance.FullSizeColor := $BFBFBF;
        QATAppearance.FullSizeColorTo := $919191;
        QATAppearance.FullSizeBorderColor := $6D6D6D;
      end;

    bsOffice2013White:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := clWhite;
        DockColor.Color := clWhite;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := clWhite;
        Color.ColorTo := clWhite;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        RightHandleColor := clSilver;
        RightHandleColorTo := clSilver;
        RightHandleColorHot := clSilver;
        RightHandleColorHotTo := clSilver;
        RightHandleColorDown := $A0A0A0;
        RightHandleColorDownTo := $A0A0A0;

        ButtonAppearance.BorderColor := clNone;
        ButtonAppearance.BorderHotColor := clNone;
        ButtonAppearance.BorderDownColor := clNone;

        RoundEdges := false;

        GlowButtonAppearance.Color := clWhite;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := clNone;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := clWhite;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := AppColor.HoverColor;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := clNone;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := AppColor.HoverBorderColor;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := AppColor.SelectedColor;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := clNone;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := AppColor.SelectedBorderColor;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := AppColor.SelectedColor;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := clNone;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := AppColor.SelectedBorderColor;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := $FFFFFF;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := clWhite;
          FTones.Tab.Hover.BorderColor := clWhite;
          FTones.Tab.Hover.TextColor := clBlack;

          FTones.Tab.Selected.BrushColor := clSilver;
          FTones.Tab.Selected.BorderColor := clSilver;
          FTones.Tab.Selected.TextColor := clBlue;

          FTones.Tab.Background.BrushColor := clWhite;
          FTones.Tab.Background.BorderColor := clSilver;
          FTones.Tab.Background.TextColor := $666666;

          FTones.App := AppColor;
          FTones.App.HoverBorderColor := FTones.App.HoverColor;
          FTones.App.SelectedBorderColor := FTones.App.SelectedColor;
          APager.SetOfficeTones(FTones, gsOffice2013);
        end
        else
        begin
          DockColor.ColorTo := clWhite;
          DockColor.Color := clWhite;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;

          Color.Color := clWhite;
          Color.ColorTo := clWhite;
          Color.Direction := gdVertical;
          Color.Steps := 64;

          RightHandleColor := clSilver;
          RightHandleColorTo := clSilver;
          RightHandleColorHot := clGray;
          RightHandleColorHotTo := clGray;
          RightHandleColorDown := clGray;
          RightHandleColorDownTo := clGray;

          BorderColor := clSilver;
        end;
        TMSStyle := tsOffice2013White;
        Exit;
      end;
    bsOffice2013Gray:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := $F3F3F3;
        DockColor.Color := $F3F3F3;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := $F3F3F3;
        Color.ColorTo := $F3F3F3;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        RightHandleColor := clSilver;
        RightHandleColorTo := clSilver;
        RightHandleColorHot := clSilver;
        RightHandleColorHotTo := clSilver;
        RightHandleColorDown := $A0A0A0;
        RightHandleColorDownTo := $A0A0A0;

        ButtonAppearance.BorderColor := clNone;
        ButtonAppearance.BorderHotColor := clNone;
        ButtonAppearance.BorderDownColor := clNone;


        RoundEdges := false;

        GlowButtonAppearance.Color := $F3F3F3;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := clNone;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $F3F3F3;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := AppColor.HoverColor;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := clNone;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := AppColor.HoverBorderColor;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := AppColor.SelectedColor;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := clNone;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := AppColor.SelectedBorderColor;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := AppColor.SelectedColor;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := clNone;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := AppColor.SelectedBorderColor;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := $DEDEDE;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := $F3F3F3;
          FTones.Tab.Hover.BorderColor := $F3F3F3;
          FTones.Tab.Hover.TextColor := clBlue;

          FTones.Tab.Selected.BrushColor := clSilver;
          FTones.Tab.Selected.BorderColor := clSilver;
          FTones.Tab.Selected.TextColor := clBlue;

          FTones.Tab.Background.BrushColor := $F3F3F3;
          FTones.Tab.Background.BorderColor := clSilver;

          FTones.App := AppColor;
          FTones.App.HoverBorderColor := FTones.App.HoverColor;
          FTones.App.SelectedBorderColor := FTones.App.SelectedColor;

          APager.SetOfficeTones(FTones, gsOffice2013);
        end
        else
        begin
          DockColor.ColorTo := clSilver;
          DockColor.Color := clSilver;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;

          Color.Color := clSilver;
          Color.ColorTo := clSilver;
          Color.Direction := gdVertical;
          Color.Steps := 64;

          RightHandleColor := clGray;
          RightHandleColorTo := clGray;
          RightHandleColorHot := $707070;
          RightHandleColorHotTo := $707070;
          RightHandleColorDown := clGray;
          RightHandleColorDownTo := clGray;

          BorderColor := clGray;
        end;

        TMSStyle := tsOffice2013Gray;
        Exit;
      end;
    bsOffice2013LightGray:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := $FAFAFA;
        DockColor.Color := $FAFAFA;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := $FAFAFA;
        Color.ColorTo := $FAFAFA;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        RightHandleColor := clSilver;
        RightHandleColorTo := clSilver;
        RightHandleColorHot := clSilver;
        RightHandleColorHotTo := clSilver;
        RightHandleColorDown := $A0A0A0;
        RightHandleColorDownTo := $A0A0A0;

        ButtonAppearance.BorderColor := clNone;
        ButtonAppearance.BorderHotColor := clNone;
        ButtonAppearance.BorderDownColor := clNone;


        RoundEdges := false;

        GlowButtonAppearance.Color := $FAFAFA;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := clNone;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $FAFAFA;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := AppColor.HoverColor;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := clNone;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := AppColor.HoverBorderColor;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := AppColor.SelectedColor;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := clNone;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := AppColor.SelectedBorderColor;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := AppColor.SelectedColor;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := clNone;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := AppColor.SelectedBorderColor;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;


        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := $F1F1F1;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := $FAFAFA;
          FTones.Tab.Hover.BorderColor := $FAFAFA;
          FTones.Tab.Hover.TextColor := clBlue;

          FTones.Tab.Selected.BrushColor := clSilver;
          FTones.Tab.Selected.BorderColor := clSilver;
          FTones.Tab.Selected.TextColor := clBlue;

          FTones.Tab.Background.BrushColor := $FAFAFA;
          FTones.Tab.Background.BorderColor := clSilver;

          FTones.App := AppColor;
          FTones.App.HoverBorderColor := FTones.App.HoverColor;
          FTones.App.SelectedBorderColor := FTones.App.SelectedColor;

          APager.SetOfficeTones(FTones, gsOffice2013);
        end
        else
        begin
          DockColor.ColorTo := $FAFAFA;
          DockColor.Color := $FAFAFA;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;

          Color.Color := $FAFAFA;
          Color.ColorTo := $FAFAFA;
          Color.Direction := gdVertical;
          Color.Steps := 64;

          RightHandleColor := clGray;
          RightHandleColorTo := clGray;
          RightHandleColorHot := $707070;
          RightHandleColorHotTo := $707070;
          RightHandleColorDown := clGray;
          RightHandleColorDownTo := clGray;

          BorderColor := clGray;
        end;

        TMSStyle := tsOffice2013LightGray;
        Exit;

      end;
    bsWindows8, bsWindows10:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := $F7F6F5;
        DockColor.Color := $F7F6F5;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := $F7F6F5;
        Color.ColorTo := $F7F6F5;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        RightHandleColor := $ECECEB;
        RightHandleColorTo := $ECECEB;
        RightHandleColorHot := $F7EFE8;
        RightHandleColorHotTo := $F7EFE8;
        RightHandleColorDown := $F7E0C9;
        RightHandleColorDownTo := $F7E0C9;

        RoundEdges := false;

        GlowButtonAppearance.Color := $F7F6F5;
        GlowButtonAppearance.ColorTo := clNone;
        GlowButtonAppearance.ColorMirror := clNone;
        GlowButtonAppearance.ColorMirrorTo := clNone;
        GlowButtonAppearance.BorderColor := $F7F6F5;
        GlowButtonAppearance.Gradient := ggVertical;
        GlowButtonAppearance.GradientMirror := ggVertical;

        GlowButtonAppearance.ColorHot := $F7EFE8;
        GlowButtonAppearance.ColorHotTo := clNone;
        GlowButtonAppearance.ColorMirrorHot := clNone;
        GlowButtonAppearance.ColorMirrorHotTo := clNone;
        GlowButtonAppearance.BorderColorHot := $F9CEA4;
        GlowButtonAppearance.GradientHot := ggVertical;
        GlowButtonAppearance.GradientMirrorHot := ggVertical;

        GlowButtonAppearance.ColorDown := $F7E0C9;
        GlowButtonAppearance.ColorDownTo := clNone;
        GlowButtonAppearance.ColorMirrorDown := clNone;
        GlowButtonAppearance.ColorMirrorDownTo := clNone;
        GlowButtonAppearance.BorderColorDown := $E4A262;
        GlowButtonAppearance.GradientDown := ggVertical;
        GlowButtonAppearance.GradientMirrorDown := ggVertical;

        GlowButtonAppearance.ColorChecked := $F7E0C9;
        GlowButtonAppearance.ColorCheckedTo := clNone;
        GlowButtonAppearance.ColorMirrorChecked := clNone;
        GlowButtonAppearance.ColorMirrorCheckedTo := clNone;
        GlowButtonAppearance.BorderColorChecked := $E4A262;
        GlowButtonAppearance.GradientChecked := ggVertical;
        GlowButtonAppearance.GradientMirrorChecked := ggVertical;

        GlowButtonAppearance.ColorDisabled := GlowButtonAppearance.Color;
        GlowButtonAppearance.ColorDisabledTo := GlowButtonAppearance.ColorTo;
        GlowButtonAppearance.ColorMirrorDisabled := GlowButtonAppearance.ColorMirror;
        GlowButtonAppearance.ColorMirrorDisabledTo := GlowButtonAppearance.ColorMirrorTo;
        GlowButtonAppearance.BorderColorDisabled := GlowButtonAppearance.BorderColor;

        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := $FFFFFF;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := $FFFDFD;
          FTones.Tab.Hover.BorderColor := $ECECEB;
          FTones.Tab.Hover.TextColor := clBlack;

          FTones.Tab.Selected.BrushColor := clSilver;
          FTones.Tab.Selected.BorderColor := clSilver;
          FTones.Tab.Selected.TextColor := clBlack;

          FTones.Tab.Background.BrushColor := $F7F6F5;
          FTones.Tab.Background.BorderColor := $DCDBDA;

          FTones.App := AppColor;
          FTones.App.HoverColor := $F7EFE8;
          FTones.App.HoverBorderColor := $F9CEA4;

          FTones.App.SelectedColor := $F7E0C9;
          FTones.App.SelectedBorderColor := $E4A262;

          APager.SetOfficeTones(FTones, gsWindows8);
        end;

        TMSStyle := tsWindows8;
        Exit;
      end;

    bsOffice2016White:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := clWhite;
        DockColor.Color := clWhite;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := clWhite;
        Color.ColorTo := clWhite;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        RightHandleColor := clWhite;
        RightHandleColorTo := clWhite;
        RightHandleColorHot := $F2E1D5;
        RightHandleColorHotTo := $F2E1D5;
        RightHandleColorDown := $E3BDA3;
        RightHandleColorDownTo := $E3BDA3;

        ButtonAppearance.BorderColor := clNone;
        ButtonAppearance.BorderHotColor := clNone;
        ButtonAppearance.BorderDownColor := clNone;


        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := AppColor.AppButtonColor;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := clWhite;
          FTones.Tab.Hover.BorderColor := clNone;
          FTones.Tab.Hover.TextColor := clBlack;

          FTones.Tab.Selected.BrushColor := clWhite;
          FTones.Tab.Selected.BorderColor := $D4D4D4;
          FTones.Tab.Selected.TextColor := AppColor.AppButtonColor;

          FTones.Tab.Background.BrushColor := clWhite;
          FTones.Tab.Background.BorderColor := $D4D4D4;
          FTones.Tab.Background.TextColor := clWhite; //$444444;

          FTones.App := AppColor;
          FTones.App.HoverBorderColor := FTones.App.HoverColor;
          FTones.App.SelectedBorderColor := FTones.App.SelectedColor;
          APager.SetOfficeTones(FTones, gsOffice2013);
        end
        else
        begin
          DockColor.ColorTo := clWhite;
          DockColor.Color := clWhite;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;

          Color.Color := clWhite;
          Color.ColorTo := clWhite;
          Color.Direction := gdVertical;
          Color.Steps := 64;

          RightHandleColor := clWhite;
          RightHandleColorTo := clWhite;
          RightHandleColorHot := $F2E1D5;
          RightHandleColorHotTo := $F2E1D5;
          RightHandleColorDown := $E3BDA3;
          RightHandleColorDownTo := $E3BDA3;

          BorderColor := $D4D4D4;
        end;

        RoundEdges := false;

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

        TMSStyle := tsOffice2016White;
        Exit;
      end;
    bsOffice2016Gray:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := $B2B2B2;
        DockColor.Color := $B2B2B2;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := $B2B2B2;
        Color.ColorTo := $B2B2B2;
        Color.Direction := gdVertical;
        Color.Steps := 64;

          RightHandleColor := $B2B2B2;
          RightHandleColorTo := $B2B2B2;
          RightHandleColorHot := $F2E1D5;
          RightHandleColorHotTo := $F2E1D5;
          RightHandleColorDown := $E3BDA3;
          RightHandleColorDownTo := $E3BDA3;

        ButtonAppearance.BorderColor := clNone;
        ButtonAppearance.BorderHotColor := clNone;
        ButtonAppearance.BorderDownColor := clNone;

        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := $444444;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := $454545;
          FTones.Tab.Hover.BorderColor := $454545;
          FTones.Tab.Hover.TextColor := $FFFFFF;

          FTones.Tab.Selected.BrushColor := $B2B2B2;
          FTones.Tab.Selected.BorderColor := $B2B2B2;
          FTones.Tab.Selected.TextColor := $262626;

          FTones.Tab.Background.BrushColor := $B2B2B2;
          FTones.Tab.Background.BorderColor := $444444;
          FTones.Tab.Background.TextColor := $F0F0F0;

          FTones.App := AppColor;
          FTones.App.HoverBorderColor := FTones.App.HoverColor;
          FTones.App.SelectedBorderColor := FTones.App.SelectedColor;

          APager.SetOfficeTones(FTones, gsOffice2013);
        end
        else
        begin
          DockColor.ColorTo := $B2B2B2;
          DockColor.Color := $B2B2B2;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;

          Color.Color := $B2B2B2;
          Color.ColorTo := $B2B2B2;
          Color.Direction := gdVertical;
          Color.Steps := 64;

          RightHandleColor := $B2B2B2;
          RightHandleColorTo := $B2B2B2;
          RightHandleColorHot := $F2E1D5;
          RightHandleColorHotTo := $F2E1D5;
          RightHandleColorDown := $E3BDA3;
          RightHandleColorDownTo := $E3BDA3;

          BorderColor := $444444;
        end;

        RoundEdges := false;

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

        TMSStyle := tsOffice2016Gray;
        Exit;
      end;
    bsOffice2016Black:
      begin
        APager := GetToolBarPager;

        DockColor.ColorTo := $363636;
        DockColor.Color := $363636;
        DockColor.Direction := gdHorizontal;
        DockColor.Steps := 128;

        Color.Color := $363636;
        Color.ColorTo := $363636;
        Color.Direction := gdVertical;
        Color.Steps := 64;

        RightHandleColor := $363636;
        RightHandleColorTo := $363636;
        RightHandleColorHot := $6A6A6A;
        RightHandleColorHotTo := $6A6A6A;
        RightHandleColorDown := $444444;
        RightHandleColorDownTo := $444444;

        ButtonAppearance.BorderColor := clNone;
        ButtonAppearance.BorderHotColor := clNone;
        ButtonAppearance.BorderDownColor := clNone;

        if Assigned(APager) then
        begin
          ATones := CreateMetroTones(True,clWhite,clBlue);
          FTones.Buttons := ATones;
          FTones.Pager := ATones;

          FTones.Pager.Background.BrushColor := $252525;

          FTones.Tab := ATones;
          FTones.Tab.Hover.BrushColor := $262626;
          FTones.Tab.Hover.BorderColor := $262626;
          FTones.Tab.Hover.TextColor := $FFFFFF;

          FTones.Tab.Selected.BrushColor := $363636;
          FTones.Tab.Selected.BorderColor := $4D4D4D;
          FTones.Tab.Selected.TextColor := $FFFFFF;

          FTones.Tab.Background.BrushColor := $363636;
          FTones.Tab.Background.BorderColor := $4E4E4E;
          FTones.Tab.Background.TextColor := $DADADA;

          FTones.App := AppColor;
          FTones.App.HoverBorderColor := FTones.App.HoverColor;
          FTones.App.SelectedBorderColor := FTones.App.SelectedColor;

          APager.SetOfficeTones(FTones, gsOffice2013);
        end
        else
        begin
          DockColor.ColorTo := $363636;
          DockColor.Color := $363636;
          DockColor.Direction := gdHorizontal;
          DockColor.Steps := 128;

          Color.Color := $363636;
          Color.ColorTo := $363636;
          Color.Direction := gdVertical;
          Color.Steps := 64;

          RightHandleColor := $363636;
          RightHandleColorTo := $363636;
          RightHandleColorHot := $6A6A6A;
          RightHandleColorHotTo := $6A6A6A;
          RightHandleColorDown := $444444;
          RightHandleColorDownTo := $444444;

          BorderColor := $4E4E4E;
        end;

        RoundEdges := false;

        TabAppearance.TextColorHot := clred;
        TabAppearance.TextColorSelected := clYellow;

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

        GlowButtonAppearance.TextColor := $A6A6A6;
        GlowButtonAppearance.SystemFont := false;
        CompactGlowButtonAppearance.TextColor := $A6A6A6;
        CompactGlowButtonAppearance.SystemFont := false;

        TMSStyle := tsOffice2016Black;
        Exit;
      end;
    end;

    case FToolBarStyle of
     bsOffice2010Blue, bsOffice2010Silver, bsOffice2010Black:
     begin
      GroupAppearance.TextColor := clWhite;
      GroupAppearance.Color := $46C0FF;
      GroupAppearance.ColorTo := $46C0FF;
      GroupAppearance.ColorMirror := $46C0FF;
      GroupAppearance.ColorMirrorTo := $46C0FF;
      GroupAppearance.Gradient := ggVertical;
      GroupAppearance.GradientMirror := ggVertical;
      GroupAppearance.BorderColor := $1475FF;

      GroupAppearance.TabAppearance.Assign(TabAppearance);
      GroupAppearance.PageAppearance.Assign(PageAppearance);
      GroupAppearance.CaptionAppearance.Assign(CaptionAppearance);

      GroupAppearance.ToolBarAppearance.Color.Color := PageAppearance.Color;
      GroupAppearance.ToolBarAppearance.Color.ColorTo := PageAppearance.ColorMirrorTo;
      GroupAppearance.ToolBarAppearance.BorderColor := PageAppearance.BorderColor;

      GroupAppearance.ToolBarAppearance.ColorHot.Color := PageAppearance.Color;
      GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := ColorHot.Mirror.ColorMirrorTo;

      GroupAppearance.TabAppearance.BorderColorHot := $1475FF;
      GroupAppearance.TabAppearance.BorderColorSelected := $1475FF;
      GroupAppearance.TabAppearance.BorderColorSelectedHot := $1475FF;
     end;
    end;

    Change(2);
  end;
end;



procedure TAdvToolBarOfficeStyler.LoadFromFile(FileName: String);
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
  else if UpperCase(ss) = UpperCase('bsWindowsXP') then
    Style := bsWindowsXP
  else if UpperCase(ss) = UpperCase('bsOfficeXP') then
    Style := bsOfficeXP
  else if UpperCase(ss) = UpperCase('bsCustom') then
    Style := bsCustom
  else if UpperCase(ss) = UpperCase('bsOffice2007Luna') then
    Style := bsOffice2007Luna
  else if UpperCase(ss) = UpperCase('bsOffice2007Obsidian') then
    Style := bsOffice2007Obsidian
  else if UpperCase(ss) = UpperCase('bsOffice2003Classic') then
    Style := bsOffice2003Classic
  else if UpperCase(ss) = UpperCase('bsWhidbey') then
    Style := bsWhidbeyStyle
  else if UpperCase(ss) = UpperCase('bsWindowsVista') then
    Style := bsWindowsVista
  else if UpperCase(ss) = UpperCase('bsWindows7') then
    Style := bsWindows7
  else if UpperCase(ss) = UpperCase('bsTerminal') then
    Style := bsTerminal
  else if UpperCase(ss) = UpperCase('bsOffice2010Blue') then
    Style := bsOffice2010Blue
  else if UpperCase(ss) = UpperCase('bsOffice2010Silver') then
    Style := bsOffice2010Silver
  else if UpperCase(ss) = UpperCase('bsOffice2010Black') then
    Style := bsOffice2010Black
  else if UpperCase(ss) = UpperCase('bsOffice2013White') then
    Style := bsOffice2013White
  else if UpperCase(ss) = UpperCase('bsOffice2013LightGray') then
    Style := bsOffice2013LightGray
  else if UpperCase(ss) = UpperCase('bsOffice2013Gray') then
    Style := bsOffice2013Gray
      else if UpperCase(ss) = UpperCase('bsOffice2016White') then
    Style := bsOffice2016White
  else if UpperCase(ss) = UpperCase('bsOffice2016Gray') then
    Style := bsOffice2016Gray
  else if UpperCase(ss) = UpperCase('bsOffice2016Black') then
    Style := bsOffice2016Black;


  LoadPropFromFile(F);

  sl.Free;
  CloseFile(f);
end;

procedure TAdvToolBarOfficeStyler.SaveToFile(FileName: String);
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
    bsOffice2003Classic:  ss := 'bsOffice2003Classic';
    bsWindowsXP:          ss := 'bsWindowsXP';
    bsOfficeXP:           ss := 'bsOfficeXP';
    bsCustom:             ss := 'bsCustom';
    bsOffice2007Luna:     ss := 'bsOffice2007Luna';
    bsOffice2007Obsidian: ss := 'bsOffice2007Obsidian';
    bsWhidbeyStyle:       ss := 'bsWhidbey';
    bsWindowsVista:       ss := 'bsWindowsVista';
    bsWindows7:           ss := 'bsWindows7';
    bsTerminal:           ss := 'bsTerminal';
    bsOffice2010Blue:     ss := 'bsOffice2010Blue';
    bsOffice2010Silver:   ss := 'bsOffice2010Silver';
    bsOffice2010Black:    ss := 'bsOffice2010Black';
    bsOffice2013White:     ss := 'bsOffice2013White';
    bsOffice2013LightGray: ss := 'bsOffice2013LightGray';
    bsOffice2013Gray:      ss := 'bsOffice2013Gray';
    bsOffice2016White:     ss := 'bsOffice2016White';
    bsOffice2016Gray:      ss := 'bsOffice2016Gray';
    bsOffice2016Black:      ss := 'bsOffice2016Black';

  end;

  Writeln(f,ss);
  SavePropToFile(f);
  CloseFile(f);
end;

{ TAdvToolBarFantasyStyler }

constructor TAdvToolBarFantasyStyler.Create(AOwner: TComponent);
begin
  inherited;
  Style := bsChocolate;
end;

procedure TAdvToolBarFantasyStyler.LoadFromFile(FileName: String);
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
    Style := bsWindowsClassic
  else if UpperCase(ss) = UpperCase('bsUser') then
    Style := bsUser
  else if UpperCase(ss) = UpperCase('bsWhidbey') then
    Style := bsWhidbey;

  LoadPropFromFile(F);

  sl.Free;
  CloseFile(f);
end;

procedure TAdvToolBarFantasyStyler.SaveToFile(FileName: String);
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
    bsUser:               ss := 'bsUser';
    bsWhidbey:            ss := 'bsWhidbey';
  end;

  Writeln(f,ss);
  SavePropToFile(f);
  CloseFile(f);
end;

procedure TAdvToolBarFantasyStyler.SetToolBarStyle(
  const Value: TToolBarFantasyStyle);
begin
  FToolbarStyle := Value;

  case FToolbarStyle of
  bsChocolate:
    begin
      Color.Color := $D7EDED;
      Color.ColorTo := $8AC2C1;
      Color.Direction := gdVertical;
      Color.Steps := 64;
      DockColor.ColorTo := $C1FFFE;
      DockColor.Color := $D9FFFF;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clMaroon;

      RightHandleColor := $D7EDED;
      RightHandleColorTo := $8AC2C1;
      RightHandleColorDown := $D9FFFF;
      RightHandleColorDownTo := $C1FFFE;
      RightHandleColorHot := $D9FFFF;
      RightHandleColorHotTo := $8AC2C1;
      CaptionAppearance.CaptionColor := clMaroon;
      CaptionAppearance.CaptionColorTo := clMaroon;
      CaptionAppearance.CaptionBorderColor := clMaroon;
      CaptionAppearance.CaptionTextColor := $C1FFFE;
      with ButtonAppearance do
      begin
        Color := clBtnFace;
        ColorTo := clWhite;
        ColorDown := clBtnFace;
        ColorDownTo := clWhite;
        ColorHot := $D9FFFF;
        ColorHotTo := $C1FFFE;
        BorderHotColor := $94CCCB;
        BorderDownColor := $94CCCB;
        BorderCheckedColor := $7A868A;

        ColorChecked := $D9FFFF;
        ColorCheckedTo := $C1FFFE;
        CaptionTextColor := clMaroon;
        CaptionTextColorChecked := $7A868A;
        CaptionTextColorDown := $7A868A;
        CaptionTextColorHot := clMaroon;
      end;
      RoundEdges:= true;
      DragGripStyle := dsDots;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsArctic :
    begin
     Color.Color := clWhite;
      Color.ColorTo := clNone;
      Color.Direction := gdHorizontal;
      Color.Steps := 64;
      DockColor.ColorTo := $00F8E4D8;
      DockColor.Color := clWhite;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clBlack;

      RightHandleColor := clWhite;
      RightHandleColorTo := $00F8E4D8;
      RightHandleColorHot := $00F8E4D8;
      RightHandleColorHotTo := clSilver;
      RightHandleColorDown := clWhite;
      RightHandleColorDownTo := clSilver;
      CaptionAppearance.CaptionColor := $00F8E4D8;
      CaptionAppearance.CaptionColorTo := $00F8E4D8;
      CaptionAppearance.CaptionBorderColor := $00F8E4D8;
      CaptionAppearance.CaptionTextColor := clBlack;
      with ButtonAppearance do
      begin
        Color := clWhite;
        ColorTo := $00F8E4D8;
        ColorDown := $00F8E4D8;
        ColorDownTo := clWhite;
        ColorHot := clNone;
        ColorHotTo := clNone;
        BorderHotColor := clNone;
        BorderDownColor := clSilver;
        BorderCheckedColor := clSilver;

        ColorChecked := clWhite;
        ColorCheckedTo := $00F8E4D8;
        CaptionTextColor := clSilver;
        CaptionTextColorChecked := $00F8E4D8;
        CaptionTextColorDown := clBlack;
        CaptionTextColorHot := clSilver;
      end;
      RoundEdges:= true;
      DragGripStyle := dsFlatDots;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsWindowsClassic :
    begin
      Color.Color := clMenu;
      Color.ColorTo := clMenu;
      Color.Direction := gdHorizontal;
      Color.Steps := 64;
      DockColor.ColorTo := clMenu;
      DockColor.Color := clMenu;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clBlack;

      RightHandleColor := clMenu;
      RightHandleColorTo := clMenu;
      RightHandleColorHot := $00A1684A;
      RightHandleColorHotTo := $00A1684A;
      RightHandleColorDown := clMenu;
      RightHandleColorDownTo := $00A1684A;
      CaptionAppearance.CaptionColor := $00A1684A;
      CaptionAppearance.CaptionColorTo := clNone;
      CaptionAppearance.CaptionBorderColor := clHighLight;
      CaptionAppearance.CaptionTextColor := clWhite;
      with ButtonAppearance do
      begin
        Color := clMenu;
        ColorTo := clNone;
        ColorDown := $00A1684A;
        ColorDownTo := clNone;
        ColorHot := $00A1684A;
        ColorHotTo := clNone;
        BorderColor := clNone;
        BorderHotColor := clNone;
        BorderDownColor := clNone;
        BorderCheckedColor := clNone;

        ColorChecked := $00A1684A;
        ColorCheckedTo := $00A1684A;
        CaptionTextColor := clBlack;
        CaptionTextColorChecked := clWhite;
        CaptionTextColorDown := clWhite;
        CaptionTextColorHot := clWhite;
      end;
      RoundEdges:= false;
      DragGripStyle := dsSingleLine;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsTerminalGreen :
    begin
      Color.Color := clWindow;
      Color.ColorTo := clNone;
      Color.Direction := gdHorizontal;
      Color.Steps := 64;
      DockColor.ColorTo := clMenu;
      DockColor.Color := clMenu;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clBlack;

      RightHandleColor := $0000C000;
      RightHandleColorTo := $00F7FFF7;
      RightHandleColorHot := $0000C000;
      RightHandleColorHotTo := clGreen;
      RightHandleColorDown := $0000C000;
      RightHandleColorDownTo := $0000C000;
      CaptionAppearance.CaptionColor := $0000C000;
      CaptionAppearance.CaptionColorTo := clNone;
      CaptionAppearance.CaptionBorderColor := clNone;
      CaptionAppearance.CaptionTextColor := clBlack;
      with ButtonAppearance do
      begin
        Color := clMenu;
        ColorTo := clNone;
        ColorDown := $0000C000;
        ColorDownTo := clNone;
        ColorHot := $00F7FFF7;
        ColorHotTo := clNone;
        BorderColor := clNone;
        BorderDownColor := clNone;
        BorderHotColor := clNone;
        BorderCheckedColor := clNone;

        ColorChecked := $0000C000;
        ColorCheckedTo := $0000C000;
        CaptionTextColor := clBlack;
        CaptionTextColorChecked := clWhite;
        CaptionTextColorDown := clBlack;
        CaptionTextColorHot := $0000C000;
      end;
      RoundEdges:= false;
      DragGripStyle := dsDoubleLine;
      Bevel:= bvNone;
      UseBevel := False;
  end;

  bsAquaBlue :
    begin
      Color.Color := clMenu;
      Color.ColorTo := clMenu;
      Color.Direction := gdHorizontal;
      Color.Steps := 64;
      DockColor.ColorTo := clWhite;
      DockColor.Color := clWhite;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := $0026B0FB;

      RightHandleColor := $00D08130;
      RightHandleColorTo := $00FEF17E;
      RightHandleColorDown := $00E6E0B0;
      RightHandleColorDownTo := $00FEF17E;
      RightHandleColorHot := $00E6E0B0;
      RightHandleColorHotTo := $00F9FFBF;
      CaptionAppearance.CaptionColor := $00E6E0B0;
      CaptionAppearance.CaptionColorTo := $00E6E0B0;
      CaptionAppearance.CaptionBorderColor := $00E6E0B0;
      CaptionAppearance.CaptionTextColor := $00D08130;
      with ButtonAppearance do
      begin
        Color := $00D08130;
        ColorTo := $00FEF17E;
        ColorDown := $00E6E0B0;
        ColorDownTo := $00F9FFBF;
        ColorHot := $00E6E0B0;
        ColorHotTo := $00F9FFBF;
        BorderColor := clNone;
        BorderHotColor := clNone;
        BorderDownColor := clNone;        
        BorderCheckedColor := clNone;

        ColorChecked := $00FEF17E;
        ColorCheckedTo := $00D08130;
        CaptionTextColor := clBlack;
        CaptionTextColorChecked := $00D08130;
        CaptionTextColorDown := $000080FF;
        CaptionTextColorHot := $00D08130;
      end;
      RoundEdges:= true;
      DragGripStyle := dsSingleLine;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsSilverFox :
    begin
      Color.Color := clWhite;
      Color.ColorTo := $00C5BAB7;
      Color.Direction := gdVertical;
      Color.Steps := 64;
      DockColor.ColorTo := $00D1C9C8;
      DockColor.Color := $00D1C9C8;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clBlack;

      RightHandleColor := clWhite;
      RightHandleColorTo := $00C5BAB7;
      RightHandleColorDown := $00F1F3F4;
      RightHandleColorDownTo := clNone;
      RightHandleColorHot := $00D2BDB5;
      RightHandleColorHotTo := clNone;
      CaptionAppearance.CaptionColor := $00E4DCDA;
      CaptionAppearance.CaptionColorTo := clNone;
      CaptionAppearance.CaptionBorderColor := clSilver;
      CaptionAppearance.CaptionTextColor := clBlack;
      with ButtonAppearance do
      begin
        Color := clWhite;
        ColorTo := $00C5BAB7;
        ColorDown := $00F1F3F4;
        ColorDownTo := clNone;
        ColorHot := $00D2BDB5;
        ColorHotTo := clNone;
        BorderColor := clNone;
        BorderHotColor := $006A240A;
        BorderDownColor := $00666666;        
        BorderCheckedColor := clNone;

        ColorChecked := clWhite;
        ColorCheckedTo := clDkGray;
        CaptionTextColor := clBlack;
        CaptionTextColorChecked := clBlack;
        CaptionTextColorDown := clBlack;
        CaptionTextColorHot := $000000B0;
      end;
      RoundEdges:= true;
      DragGripStyle := dsDots;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsTextured :
    begin
      Color.Color := $00CFD5F3;
      Color.ColorTo := $00CFD5F3;
      Color.Direction := gdVertical;
      Color.Steps := 64;
      DockColor.ColorTo := clNone;
      DockColor.Color := $00CFD5F3;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clBlack;

      RightHandleColor := $00CFD5F3;
      RightHandleColorTo := $00CFD5F3;
      RightHandleColorDown := $000000B0;
      RightHandleColorDownTo := $00CFD5F3;
      RightHandleColorHot := $00CFD5F3;
      RightHandleColorHotTo := $00CFD5F3;
      CaptionAppearance.CaptionColor := $00CFD5F3;
      CaptionAppearance.CaptionColorTo := $00CFD5F3;
      CaptionAppearance.CaptionBorderColor := $00CFD5F3;
      CaptionAppearance.CaptionTextColor := $000000B0;
      with ButtonAppearance do
      begin
        Color := $00CFD5F3;
        ColorTo := $00CFD5F3;
        ColorDown := clNone;
        ColorDownTo := clNone;
        ColorHot := clNone;
        ColorHotTo := clNone;
        BorderColor := clNone;
        BorderHotColor := $000000B0;
        BorderDownColor := $000000B0;
        BorderCheckedColor := $000000B0;

        ColorChecked := $00CFD5F3;
        ColorCheckedTo := $00CFD5F3;
        CaptionTextColor := $000000B0;
        CaptionTextColorChecked := $000000B0;
        CaptionTextColorDown := $000000B0;
        CaptionTextColorHot := clBlack;
      end;
      RoundEdges:= false;
      DragGripStyle := dsFlatDots;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsMacOS :
    begin
      Color.Color := clWhite;
      Color.ColorTo := $00F3F3F3;
      Color.Direction := gdVertical;
      Color.Steps := 64;
      DockColor.ColorTo := clWhite;
      DockColor.Color := $00F3F3F3;
      DockColor.Direction := gdVertical;
      DockColor.Steps := 128;

      Font.Color := clBlack;

      RightHandleColor := clWhite;
      RightHandleColorTo := $00F3F3F3;
      RightHandleColorDown := $00F19546;
      RightHandleColorDownTo := $00C34907;
      RightHandleColorHot := clSilver;
      RightHandleColorHotTo := $00BBBBBB;
      CaptionAppearance.CaptionColor := $00F3F3F3;
      CaptionAppearance.CaptionColorTo := clWhite;
      CaptionAppearance.CaptionBorderColor := $00F3F3F3;
      CaptionAppearance.CaptionTextColor := clGray;
      with ButtonAppearance do
      begin
        Color := clWhite;
        ColorTo := $00F3F3F3;
        ColorDown := $00F19546;
        ColorDownTo := $00C34907;
        ColorHot := clSilver;
        ColorHotTo := $00BBBBBB;
        BorderColor := clGray;
        BorderHotColor := clNone;
        BorderDownColor := clNone;
        BorderCheckedColor := clNone;

        ColorChecked := $00F19546;
        ColorCheckedTo := clWhite;
        CaptionTextColor := $000000B0;
        CaptionTextColorChecked := clWhite;
        CaptionTextColorDown := clWhite;
        CaptionTextColorHot := clBlack;
      end;
      RoundEdges:= true;
      DragGripStyle := dsDoubleLine;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsSoftSand :                           
    begin
      Color.Color := clWhite;
      Color.ColorTo := $00E7F7FF;
      Color.Direction := gdVertical;
      Color.Steps := 64;
      DockColor.ColorTo := clWhite;
      DockColor.Color := $00E7F7FF;
      DockColor.Direction := gdHorizontal;
      DockColor.Steps := 128;

      Font.Color := clGray;

      RightHandleColor := $00E7F7FF;
      RightHandleColorTo := clNone;
      RightHandleColorDown := $00E7F7FF;
      RightHandleColorDownTo := $00B5DEF7;
      RightHandleColorHot := $00B5DEF7;
      RightHandleColorHotTo := clNone;
      CaptionAppearance.CaptionColor := $00E7F7FF;
      CaptionAppearance.CaptionColorTo := $00E7F7FF;
      CaptionAppearance.CaptionBorderColor := clNone;
      CaptionAppearance.CaptionTextColor := $000884BD;
      with ButtonAppearance do
      begin
        Color := $00E7F7FF;
        ColorTo := clNone;
        ColorDown := $00E7F7FF;
        ColorDownTo := clNone;
        ColorHot := $00E7F7FF;
        ColorHotTo := clNone;
        BorderColor := clGray;
        BorderHotColor := clNone;
        BorderDownColor := clBlack;
        BorderCheckedColor := clNone;

        ColorChecked := $00B5DEF7;
        ColorCheckedTo := $00E7F7FF;
        CaptionTextColor := $000000B0;
        CaptionTextColorChecked := $000884BD;
        CaptionTextColorDown := $000884BD;
        CaptionTextColorHot := $000884BD;
      end;
      RoundEdges:= true;
      DragGripStyle := dsFlatDots;
      Bevel:= bvNone;
      UseBevel := False;
    end;

  bsWhidbey:
    begin

      Color.Color := $F5F9FA;
      Color.ColorTo := $A8C0C0;
      Color.Direction := gdVertical;
      Color.Steps := 64;

      DockColor.Color := $D7E5E5;
      DockColor.ColorTo := $E7F2F3;
      DockColor.Direction := gdHorizontal;
      DockColor.Steps := 128;

      RightHandleColor := $EBEEEF;
      RightHandleColorTo := $7E9898;
      RightHandleColorHot := $EED2C1;
      RightHandleColorHotTo := clNone;
      RightHandleColorDown := $E8E6E1;
      RightHandleColorDownTo := clNone;

      CaptionAppearance.CaptionColor := $99A8AC;
      CaptionAppearance.CaptionColorTo := $99A8AC;
      CaptionAppearance.CaptionBorderColor := $828F92;

      with ButtonAppearance do
      begin
        Color := clBtnFace;
        ColorTo := clNone;

        ColorDown := $E2B598;
        ColorDownTo := clNone;

        ColorHot := $EED2C1;
        ColorHotTo := clNone;

        ColorChecked := clBtnFace;
        ColorCheckedTo := clNone;
        CaptionTextColor := clBlack;
        CaptionTextColorChecked := clBlack;
        CaptionTextColorDown := clBlack;
        CaptionTextColorHot := clBlack;

        BorderHotColor := $C56A31;
        BorderDownColor := $6F4B4B;
      end;

      FloatingWindowBorderColor := $828F92;
      FloatingWindowBorderWidth := 2;

      RoundEdges := true;
      DragGripStyle := dsDots;
      Bevel := bvNone;
      UseBevel := False;


      {AdvToolBarPager}
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

      CompactGlowButtonAppearance.Assign(GlowButtonAppearance);

      CaptionAppearance.CaptionColorHot := $ADC4C4;
      CaptionAppearance.CaptionColorHotTo := $ADC4C4;
      CaptionAppearance.CaptionTextColor := clWhite;
      CaptionAppearance.CaptionTextColorHot := clWhite;

      { GroupAppearance }

      GroupAppearance.TextColor := clBlack;
      GroupAppearance.Color := clWhite;
      GroupAppearance.ColorTo := clWhite;
      GroupAppearance.ColorMirror := clWhite;
      GroupAppearance.ColorMirrorTo := $FFDDBB;
      GroupAppearance.Gradient := ggVertical;
      GroupAppearance.GradientMirror := ggVertical;
      GroupAppearance.BorderColor := clWhite;

      GroupAppearance.TabAppearance.ColorHot := $CCF4FF;
      GroupAppearance.TabAppearance.ColorHotTo := $CCF4FF;
      GroupAppearance.TabAppearance.ColorMirrorHot := $CCF4FF;
      GroupAppearance.TabAppearance.ColorMirrorHotTo := $91D0FF;
      GroupAppearance.TabAppearance.Gradient := ggVertical;
      GroupAppearance.TabAppearance.GradientMirror := ggVertical;

      GroupAppearance.TabAppearance.ColorSelected := $9EDFFB;
      GroupAppearance.TabAppearance.ColorSelectedTo := $BAE8FC;
      GroupAppearance.TabAppearance.ColorMirrorSelected := $BAE8FC;
      GroupAppearance.TabAppearance.ColorMirrorSelectedTo := $D4F1FD;
      GroupAppearance.TabAppearance.TextColorSelected := clBlack;

      GroupAppearance.TabAppearance.BorderColorSelected := $A0BFCC;
      GroupAppearance.TabAppearance.BorderColorSelectedHot := $A0BFCC;
      GroupAppearance.TabAppearance.BorderColorHot := clHighLight;
      GroupAppearance.TabAppearance.BorderColor := clHighLight;
      GroupAppearance.TabAppearance.TextColor := clWhite;
      GroupAppearance.TabAppearance.TextColorHot := clBlack;

      GroupAppearance.PageAppearance.Color := $D6F2FE;
      GroupAppearance.PageAppearance.ColorTo := $F9F9F9;
      GroupAppearance.PageAppearance.BorderColor := $C2C2C2;

      GroupAppearance.PageAppearance.ColorMirror := $F9F9F9;
      GroupAppearance.PageAppearance.ColorMirrorTo := $F9F9F9;
      GroupAppearance.PageAppearance.Gradient := ggVertical;
      GroupAppearance.PageAppearance.GradientMirror := ggVertical;

      GroupAppearance.ToolBarAppearance.Color.Color := $ECF8FD;
      GroupAppearance.ToolBarAppearance.Color.ColorTo := $F9F9F9;
      GroupAppearance.ToolBarAppearance.BorderColor := $CCD1D3;

      GroupAppearance.ToolBarAppearance.ColorHot.Color := $EEF4F5;
      GroupAppearance.ToolBarAppearance.ColorHot.ColorTo := $FDFBFA;
      GroupAppearance.ToolBarAppearance.BorderColorHot := $C7C7C7;

      GroupAppearance.CaptionAppearance.CaptionColor := $D9E9EC;
      GroupAppearance.CaptionAppearance.CaptionColorTo := $D9E9EC;
      GroupAppearance.CaptionAppearance.CaptionColorHot := $DFEDF0;
      GroupAppearance.CaptionAppearance.CaptionColorHotTo := $DFEDF0;
      GroupAppearance.CaptionAppearance.CaptionTextColor := clBlack;
      GroupAppearance.CaptionAppearance.CaptionTextColorHot := clBlack;

      { TabAppearance }

      TabAppearance.BackGround.Color := $859D9D;
      TabAppearance.BackGround.ColorTo := $ADC4C4;
      TabAppearance.BorderColor := clNone;
      TabAppearance.BorderColorDisabled := clNone;
      TabAppearance.BorderColorHot := $E3B28D;
      TabAppearance.BorderColorSelected := clBlack;
      TabAppearance.BorderColorSelectedHot := $60CCF9;

      TabAppearance.TextColor := clWhite;
      TabAppearance.TextColorHot := clBlack;
      TabAppearance.TextColorSelected := clBlack;
      TabAppearance.TextColorDisabled := clSilver;

      TabAppearance.ColorSelected := $ADC4C4;
      TabAppearance.ColorSelectedTo := clWhite;
      TabAppearance.ColorMirrorSelected := clWhite;
      TabAppearance.ColorMirrorSelectedTo := clWhite;

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

      { ToolBar color & color hot }
      ColorHot.Color := $F5F9FA;
      ColorHot.ColorTo := $F5F9FA;
      ColorHot.Direction := gdVertical;
      BorderColorHot := $E0C7AD;


      { PageAppearance }
      PageAppearance.BorderColor := clBlack;
      PageAppearance.Color := clWhite;
      PageAppearance.ColorTo := $D9E9EC;
      PageAppearance.ColorMirror := $D9E9EC;
      PageAppearance.ColorMirrorTo := clWhite;
      PageAppearance.Gradient := ggVertical;
      PageAppearance.GradientMirror := ggVertical;

      { PagerCaption }
      PagerCaption.Color := $859D9D;
      PagerCaption.ColorTo := $859D9D;
      PagerCaption.ColorMirror := $859D9D;
      PagerCaption.ColorMirrorTo := $859D9D;
      PagerCaption.BorderColor := $F0CAAE;
      PagerCaption.Gradient := ggVertical;
      PagerCaption.GradientMirror := ggVertical;
    end;

  end;

end;

end.
