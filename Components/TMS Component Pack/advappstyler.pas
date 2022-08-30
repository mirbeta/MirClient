{*************************************************************************}
{ TMS TAdvFormStyler & TAdvAppStyler component                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2006 - 2015                                      }
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

unit AdvAppStyler;

interface

uses
  Classes, AdvStyleIF, Forms, Windows, SysUtils, Controls, Messages,
  Dialogs, StdCtrls, Graphics;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.1.0 : added OnChange event
  // 1.1.0.0 : Added AutoThemeAdapt capability
  // 1.1.1.0 : Added capability to handle frames in TAdvFormStyler
  // 1.1.2.0 : Added OnChange event for TAdvAppStyler
  // 1.1.3.0 : Added capability to adapt theme programmatically at runtime by setting AutoThemeAdapt
  // 1.1.4.0 : Added support to handle frames in frames
  // 1.1.4.1 : Fixed issue with AutoThemeAdapt
  // 1.2.0.0 : New : Added event OnApplyStyle in TAdvFormStyler to allow excluding components from being styled
  // 1.2.0.1 : Fixed : issue with form inheritance
  // 1.2.1.0 : Improved : handling of components that should not be styled at runtime & designtime
  // 1.2.2.0 : New : ComboBox property added to enable style selection form combobox
  //         : New : public function GetStyles: TStringList added
  // 1.2.3.0 : New : Can scan for and use style defined on form level too
  // 2.0.0.0 : New : Support for Metro style
  // 2.1.0.0 : New : OnAppliedStyle event added
  //         : New : OnAppliedColorTones event added
  // 2.1.0.1 : Fixed : Small issue with introducec Enabled property
  // 2.1.0.2 : Fixed : Issue with TAdvFormStyler in Metro style and frames
  // 2.1.1.0 : New : Extensions for Windows 8 & Office 2013 styles
  // 2.1.2.0 : Improved : Handling of Autothemeadapt
  // 2.2.0.0 : New : AutoThemeAdapt support added for Office 2013
  // 2.2.0.1 : Fixed : Issue with applying style to frame
  // 2.2.1.0 : New : AutoThemeAdapt support added for Office 2016

type
  TAdvFormStyler = class;

  TThemeNotifierWindow = class(TWinControl)
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
  TAdvAppStyler = class(TComponent)
  private
    FTones: TColorTones;
    FForms: TList;
    FStyle: TTMSStyle;
    FAutoThemeAdapt: boolean;
    FNotifier: TThemeNotifierWindow;
    FOnChange: TNotifyEvent;
    FMetroStyle: TMetroStyle;
    FMetro: boolean;
    FMetroTextColor: TColor;
    FMetroColor: TColor;
    FEnabled: boolean;
    FAppColor: TColor;
    procedure SetStyle(const Value: TTMSStyle);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure ThemeChanged(Sender: TObject);
    procedure SetAutoThemeAdapt(const Value: boolean);
    procedure SetMetro(const Value: boolean);
    procedure SetMetroColor(const Value: TColor);
    procedure SetMetroStyle(const Value: TMetroStyle);
    procedure SetMetroTextColor(const Value: TColor);
    procedure SetEnabled(const Value: boolean);
  protected
    procedure Loaded; override;
    procedure ChangeTones; virtual;
    procedure DoChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyStyle;
    procedure RegisterFormStyler(AFormStyler: TAdvFormStyler);
    procedure UnRegisterFormStyler(AFormStyler: TAdvFormStyler);
    function GetVersionNr: integer;
    procedure SetColorTones(ATones: TColorTones);
  published
    property AppColor: TColor read FAppColor write FAppColor default clNone;
    property AutoThemeAdapt: boolean read FAutoThemeAdapt write SetAutoThemeAdapt default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Metro: boolean read FMetro write SetMetro default false;
    property MetroColor: TColor read FMetroColor write SetMetroColor default clHighlight;
    property MetroTextColor: TColor read FMetroTextColor write SetMetroTextColor default clBlack;
    property MetroStyle: TMetroStyle read FMetroStyle write SetMetroStyle default msLight;
    property Style: TTMSStyle read FStyle write SetStyle default tsCustom;
    property Version: string read GetVersion write SetVersion stored false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TApplyStyleEvent = procedure(Sender: TObject; AComponent: TComponent; var Allow:boolean) of object;

  TAppliedStyleEvent = procedure(Sender: TObject; AComponent: TComponent) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvFormStyler = class(TComponent)
  private
    FStyle: TTMSStyle;
    FAppStyle: TAdvAppStyler;
    FOnChange: TNotifyEvent;
    FAutoThemeAdapt: boolean;
    FNotifier: TThemeNotifierWindow;
    FOnApplyStyle: TApplyStyleEvent;
    FOnApplyColorTones: TApplyStyleEvent;
    FMetroStyle: TMetroStyle;
    FMetro: boolean;
    FMetroTextColor: TColor;
    FMetroColor: TColor;
    FTones: TColorTones;
    FOnAppliedStyle: TAppliedStyleEvent;
    FOnAppliedColorTones: TAppliedStyleEvent;
    FEnabled: boolean;
    FComboBox: TComboBox;
    FAppColor: TColor;
    procedure SetStyle(const Value: TTMSStyle);
    procedure SetAppStyle(const Value: TAdvAppStyler);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure ThemeChanged(Sender: TObject);
    procedure SetAutoThemeAdapt(const Value: boolean);
    procedure SetMetro(const Value: boolean);
    procedure SetMetroColor(const Value: TColor);
    procedure SetMetroStyle(const Value: TMetroStyle);
    procedure SetMetroTextColor(const Value: TColor);
    procedure SetEnabled(const Value: boolean);
    procedure SetComboBox(const Value: TComboBox);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ChangeTones; virtual;
    procedure DoChange; virtual;
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyStyle;
    procedure ApplyStyleToForm(Form: TCustomForm; AStyle:TTMSStyle);
    procedure ApplyStyleToFrame(Frame: TCustomFrame; AStyle:TTMSStyle);
    procedure ApplyColorTonesToForm(Form: TCustomForm; ATones: TColorTones);
    procedure ApplyColorTonesToFrame(Frame: TCustomFrame; ATones: TColorTones);
    procedure SetColorTones(ATones: TColorTones);
    function GetVersionNr: integer;
    function GetStyles: TStringList;
    procedure HandleStyleSelect(Sender: TObject);
  published
    property AppColor: TColor read FAppColor write FAppColor default clBlack;
    property AutoThemeAdapt: boolean read FAutoThemeAdapt write SetAutoThemeAdapt default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Metro: boolean read FMetro write SetMetro default false;
    property MetroColor: TColor read FMetroColor write SetMetroColor default clHighlight;
    property MetroTextColor: TColor read FMetroTextColor write SetMetroTextColor default clBlack;
    property MetroStyle: TMetroStyle read FMetroStyle write SetMetroStyle default msLight;
    property Style: TTMSStyle read FStyle write SetStyle default tsCustom;
    property AppStyle: TAdvAppStyler read FAppStyle write SetAppStyle;
    property ComboBox: TComboBox read FComboBox write SetComboBox;
    property OnApplyStyle: TApplyStyleEvent read FOnApplyStyle write FOnApplyStyle;
    property OnAppliedStyle: TAppliedStyleEvent read FOnAppliedStyle write FOnAppliedStyle;
    property OnApplyColorTones: TApplyStyleEvent read FOnApplyColorTones write FOnApplyColorTones;
    property OnAppliedColorTones: TAppliedStyleEvent read FOnAppliedColorTones write FOnAppliedColorTones;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Version: string read GetVersion write SetVersion stored false;
  end;


procedure Register;


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

procedure TThemeNotifierWindow.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_THEMECHANGED  then
  begin
    if Assigned(FOnThemeChange) then
    begin
      FOnThemeChange(Self);
    end;
  end;
  inherited;
end;


{ TAdvFormStyler }

procedure TAdvFormStyler.ApplyStyleToForm(Form: TCustomForm; AStyle: TTMSStyle);
var
  i: integer;
  tmsif: ITMSStyle;
  tmsifex: ITMSStyleEx;
  Allow: boolean;

begin
  if Form.GetInterface(ITMSStyleEx, tmsifex) then
  begin
    Allow := true;
    if Assigned(FOnApplyStyle) then
      FOnApplyStyle(Self, Form, Allow);

    if Allow then
    begin
      tmsifex.SetComponentStyleAndAppColor(AStyle, AppColor);

      if Assigned(FOnAppliedStyle) then
        FOnAppliedStyle(Self, Form);
    end;
  end
  else
  if Form.GetInterface(ITMSStyle, tmsif) then
  begin
    Allow := true;
    if Assigned(FOnApplyStyle) then
      FOnApplyStyle(Self, Form, Allow);

    if Allow then
    begin
      tmsif.SetComponentStyle(AStyle);

      if Assigned(FOnAppliedStyle) then
        FOnAppliedStyle(Self, Form);
    end;
  end;

  for i := 0 to Form.ComponentCount - 1 do
  begin
    if (Form.Components[i] is TCustomFrame) then
      ApplyStyleToFrame(Form.Components[i] as TCustomFrame, AStyle);

    if Form.Components[i].GetInterface(ITMSStyleEx, tmsifex) then
    begin
      Allow := true;
      if Assigned(FOnApplyStyle) then
        FOnApplyStyle(Self, Form.Components[i], Allow);

      if Allow then
      begin
        tmsifex.SetComponentStyleAndAppColor(AStyle, AppColor);

        if Assigned(FOnAppliedStyle) then
          FOnAppliedStyle(Self, Form.Components[i]);
      end;
    end
    else
    if Form.Components[i].GetInterface(ITMSStyle, tmsif) then
    begin
      Allow := true;
      if Assigned(FOnApplyStyle) then
        FOnApplyStyle(Self, Form.Components[i], Allow);

      if Allow then
      begin
        tmsif.SetComponentStyle(AStyle);

        if Assigned(FOnAppliedStyle) then
          FOnAppliedStyle(Self, Form.Components[i]);
      end;
    end;

  end;
end;

procedure TAdvFormStyler.ApplyStyleToFrame(Frame: TCustomFrame; AStyle: TTMSStyle);
var
  i: integer;
  tmsif: ITMSStyle;
  tmsifex: ITMSStyleEx;
  allow: boolean;
begin
  for i := 0 to Frame.ComponentCount - 1 do
  begin
    if Frame.Components[i].GetInterface(ITMSStyleEx, tmsifex) then
    begin
      allow := true;
      if Assigned(FOnApplyStyle) then
        FOnApplyStyle(Self, Frame.Components[i],allow);

      if allow then
      begin
        tmsifex.SetComponentStyleAndAppColor(AStyle, AppColor);

        if Assigned(FOnAppliedStyle) then
          FOnAppliedStyle(Self, Frame.Components[i]);
      end;
    end
    else
    if Frame.Components[i].GetInterface(ITMSStyle, tmsif) then
    begin
      allow := true;
      if Assigned(FOnApplyStyle) then
        FOnApplyStyle(Self, Frame.Components[i],allow);

      if allow then
      begin
        tmsif.SetComponentStyle(AStyle);

        if Assigned(FOnAppliedStyle) then
          FOnAppliedStyle(Self, Frame.Components[i]);
      end;
    end;

    if Frame.Components[i] is TCustomFrame then
    begin
      ApplyStyleToFrame(TCustomFrame(Frame.Components[i]), AStyle);
    end;
  end;
end;

procedure TAdvFormStyler.Assign(Source: TPersistent);
begin
  if (Source is TAdvFormStyler) then
  begin
    AutoThemeAdapt := (Source as TAdvFormStyler).AutoThemeAdapt;
    Style := (Source as TAdvFormStyler).Style;
  end;
end;

procedure TAdvFormStyler.ChangeTones;
begin
  if not FEnabled then
    Exit;

  FTones := CreateMetroTones(MetroStyle = msLight, FMetroColor, FMetroTextColor);

  if (FStyle = tsCustom) and FMetro then
    SetColorTones(FTones);
end;

constructor TAdvFormStyler.Create(AOwner: TComponent);
var
  ctrl: TComponent;
begin
  inherited;

  FStyle := tsCustom;
  FTones := DefaultMetroTones;

  FMetroColor := FTones.Selected.BrushColor;
  FMetroTextColor := clBlack;
  FMetroStyle := msLight;
  FAppColor := clBlack;

  if not (csDesigning in ComponentState) then
  begin
    FNotifier := TThemeNotifierWindow.Create(self);

    // find first owning TWinControl owner
    ctrl := AOwner;
    while Assigned(ctrl) and not (ctrl is TWinControl) do
    begin
      ctrl := ctrl.Owner;
    end;

    if Assigned(ctrl) then
      if (ctrl is TWinControl) then
        FNotifier.Parent := TWinControl(ctrl);

    FNotifier.OnThemeChange := ThemeChanged;
  end;

  FEnabled := True;
end;

destructor TAdvFormStyler.Destroy;
begin
  if Assigned(FAppStyle) and not (csDesigning in ComponentState) then
    FAppStyle.UnRegisterFormStyler(Self);

  inherited;
end;

function TAdvFormStyler.GetStyles: TStringList;
begin
  Result := TStringList.Create;
  Result.AddObject('Office 2003 Blue', TObject(tsOffice2003Blue));
  Result.AddObject('Office 2003 Silver', TObject(tsOffice2003Silver));
  Result.AddObject('Office 2003 Olive', TObject(tsOffice2003Olive));
  Result.AddObject('Office 2003 Classic', TObject(tsOffice2003Classic));
  Result.AddObject('Office 2007 Blue', TObject(tsOffice2007Luna));
  Result.AddObject('Office 2007 Silver', TObject(tsOffice2007Silver));
  Result.AddObject('Office 2007 Black', TObject(tsOffice2007Obsidian));
  Result.AddObject('Windows XP', TObject(tsWindowsXP));
  Result.AddObject('Windows Vista', TObject(tsWindowsVista));
  Result.AddObject('Windows 7', TObject(tsWindows7));
  Result.AddObject('Windows 8', TObject(tsWindows8));
  Result.AddObject('Windows 10', TObject(tsWindows10));
  Result.AddObject('Terminal', TObject(tsTerminal));
  Result.AddObject('Office 2010 Blue', TObject(tsOffice2010Blue));
  Result.AddObject('Office 2010 Silver', TObject(tsOffice2010Silver));
  Result.AddObject('Office 2010 Black', TObject(tsOffice2010Black));
  Result.AddObject('Office 2013 White', TObject(tsOffice2013White));
  Result.AddObject('Office 2013 Light gray', TObject(tsOffice2013LightGray));
  Result.AddObject('Office 2013 Gray', TObject(tsOffice2013Gray));
  Result.AddObject('Office 2016 White', TObject(tsOffice2016White));
  Result.AddObject('Office 2016 Gray', TObject(tsOffice2016Gray));
  Result.AddObject('Office 2016 Black', TObject(tsOffice2016Black));
  Result.AddObject('Custom',TObject(tsCustom));
end;

function TAdvFormStyler.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvFormStyler.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvFormStyler.HandleStyleSelect(Sender: TObject);
begin
  with (Sender as TComboBox) do
  begin
    if ItemIndex >= 0 then
      self.Style := TTMSStyle(integer(Items.Objects[ItemIndex]));
  end;
end;

procedure TAdvFormStyler.Loaded;
var
  AStyle: TTMSStyle;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FAppStyle) then
    begin
      Style := FAppStyle.Style;

      FTones := FAppStyle.FTones;
      Metro := FAppStyle.Metro;
      MetroStyle := FAppStyle.MetroStyle;
      MetroTextColor := FAppStyle.MetroTextColor;
      MetroColor := FAppStyle.MetroColor;
    end
    else
    begin
      AStyle := Style;
      Style := tsCustom;
      Style := AStyle;
    end;
  end;

  if Metro and not (csDesigning in ComponentState) then
    SetColorTones(FTones);

  if not (csDesigning in ComponentState) then
    ThemeChanged(Self);
end;

procedure TAdvFormStyler.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (FAppStyle = AComponent) then
  begin
    FAppStyle := nil;
  end;

  if (AOperation = opInsert) then
  begin
  end;
end;

procedure TAdvFormStyler.SetAppStyle(const Value: TAdvAppStyler);
begin
  FAppStyle := Value;
  if not (csLoading in ComponentState) then
  begin
    if Assigned(FAppStyle) then
      Style := FAppStyle.Style;
  end;

  if Assigned(FAppStyle) and not (csDesigning in ComponentState) then
  begin
    FAppStyle.RegisterFormStyler(self);
  end;
end;

procedure TAdvFormStyler.SetAutoThemeAdapt(const Value: boolean);
begin
  FAutoThemeAdapt := value;
  ThemeChanged(self);
end;

procedure TAdvFormStyler.SetColorTones(ATones: TColorTones);
begin
  FMetro := true;
  FStyle := tsCustom;

  if (Owner is TForm) then
  begin
    ApplyColorTonesToForm((Owner as TForm), ATones);
  end;

  if (Owner is TFrame) then
  begin
    ApplyColorTonesToFrame((Owner as TFrame), ATones);
  end;

end;

procedure TAdvFormStyler.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    if FEnabled then
      ApplyStyle;
  end;
end;

procedure TAdvFormStyler.SetMetro(const Value: boolean);
begin
  if (FMetro <> Value) then
  begin
    FMetro := Value;
    if FMetro then
      ChangeTones;
  end;
end;

procedure TAdvFormStyler.SetMetroColor(const Value: TColor);
begin
  if (FMetroColor <> Value) then
  begin
    FMetroColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvFormStyler.SetMetroStyle(const Value: TMetroStyle);
begin
  if (FMetroStyle <> Value) then
  begin
    FMetroStyle := Value;
    ChangeTones;
  end;
end;

procedure TAdvFormStyler.SetMetroTextColor(const Value: TColor);
begin
  if (FMetroTextColor <> Value) then
  begin
    FMetroTextColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvFormStyler.SetComboBox(const Value: TComboBox);
var
  sl: TStringList;
  i: integer;
begin
  FComboBox := Value;
  if Assigned(FComboBox) then
  begin
    sl := GetStyles;
    FComboBox.Items.Assign(sl);
    sl.Free;
    FComboBox.OnSelect := HandleStyleSelect;
    FComboBox.Style := csDropDownList;

    for i := 0 to FComboBox.Items.Count - 1 do
    begin
      if FComboBox.Items.Objects[i] = TObject(self.Style) then
      begin
        FComboBox.ItemIndex := i;
        break;
      end;
    end;
  end;
end;

procedure TAdvFormStyler.ApplyColorTonesToForm(Form: TCustomForm; ATones: TColorTones);
var
  i: integer;
  tmsif: ITMSTones;
  Allow: boolean;

begin
  if Form.GetInterface(ITMSTones, tmsif) then
  begin
    Allow := true;
    if Assigned(FOnApplyColorTones) then
      FOnApplyColorTones(Self, Form, Allow);

    if Allow then
    begin
      tmsif.SetColorTones(ATones);
      if Assigned(FOnAppliedColorTones) then
        FOnAppliedColorTones(Self, Form);
    end;
  end;

  for i := 0 to Form.ComponentCount - 1 do
  begin
    if (Form.Components[i] is TCustomFrame) then
      ApplyColorTonesToFrame(Form.Components[i] as TCustomFrame, ATones);

    if Form.Components[i].GetInterface(ITMSTones, tmsif) then
    begin
      Allow := true;
      if Assigned(FOnApplyColorTones) then
        FOnApplyColorTones(Self, Form.Components[i], Allow);

      if Allow then
      begin
        tmsif.SetColorTones(ATones);
        if Assigned(FOnAppliedColorTones) then
          FOnAppliedColorTones(Self, Form.Components[i]);
      end;
    end;
  end;
end;

procedure TAdvFormStyler.ApplyColorTonesToFrame(Frame: TCustomFrame; ATones: TColorTones);
var
  i: integer;
  tmsif: ITMSTones;
  allow: boolean;
begin
  for i := 0 to Frame.ComponentCount - 1 do
  begin
    if Frame.Components[i].GetInterface(ITMSTones, tmsif) then
    begin
      allow := true;
      if Assigned(FOnApplyColorTones) then
        FOnApplyColorTones(Self, Frame.Components[i],allow);

      if allow then
      begin
        tmsif.SetColorTones(ATones);
        if Assigned(FOnAppliedColorTones) then
          FOnAppliedColorTones(Self, Frame.Components[i]);
      end;
    end;

    if Frame.Components[i] is TCustomFrame then
    begin
      ApplyColorTonesToFrame(TCustomFrame(Frame.Components[i]), ATones);
    end;
  end;
end;

procedure TAdvFormStyler.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvFormStyler.ApplyStyle;
var
  AForm: TCustomForm;
  AFrame: TCustomFrame;
begin
  if not FEnabled then
    Exit;

  // apply style to form
  if Owner is TCustomForm then
  begin
    AForm := Owner as TCustomForm;
    ApplyStyleToForm(AForm, FStyle);
  end;

  if Owner is TCustomFrame then
  begin
    AFrame := Owner as TCustomFrame;
    ApplyStyleToFrame(AFrame, FStyle);
  end;

  DoChange;
end;


procedure TAdvFormStyler.SetStyle(const Value: TTMSStyle);
begin

  if (FStyle <> Value) then
  begin
    FStyle := Value;

    if (Value <> tsCustom) and not (csLoading in ComponentState) then
    begin
      ApplyStyle;
    end;
  end;
end;

procedure TAdvFormStyler.SetVersion(const Value: string);
begin

end;

procedure TAdvFormStyler.ThemeChanged(Sender: TObject);
var
  ot: TOfficeTheme;
begin
  if not AutoThemeAdapt then
    Exit;

  ot := GetOfficeTheme;

  Style := tsCustom;

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
  ot2013White: Style := tsOffice2013White;
  ot2013Silver: Style := tsOffice2013LightGray;
  ot2013Gray: Style := tsOffice2013Gray;
  ot2016White: Style := tsOffice2016White;
  ot2016Gray: Style := tsOffice2016Gray;
  ot2016Black: Style := tsOffice2016Black;
  end;
end;


procedure Register;
begin
  RegisterComponents('TMS Menus',[TAdvFormStyler, TAdvAppStyler]);
end;


{ TAdvAppStyler }

procedure TAdvAppStyler.Assign(Source: TPersistent);
begin
  if (Source is TAdvAppStyler) then
  begin
    Style := (Source as TAdvAppStyler).Style;
    AutoThemeAdapt := (Source as TAdvAppStyler).AutoThemeAdapt;
  end;
end;

constructor TAdvAppStyler.Create(AOwner: TComponent);
begin
  inherited;
  FForms := TList.Create;
  FStyle := tsCustom;
  FMetroColor := DefaultMetroTones.Selected.BrushColor;
  FMetroTextColor := clBlack;
  FMetroStyle := msLight;
  FAppColor := clNone;

  if not (csDesigning in ComponentState) then
  begin
    FNotifier := TThemeNotifierWindow.Create(self);
    FNotifier.Parent := Application.MainForm;
    FNotifier.OnThemeChange := ThemeChanged;
  end;
  FEnabled := True;
end;

destructor TAdvAppStyler.Destroy;
var
  i: integer;
begin
  for i := 0 to FForms.Count - 1 do
  begin
    if Assigned(FForms[i]) then
    begin
      TAdvFormStyler(FForms[i]).AppStyle := nil; //Unregister from the form to avoid AV
    end;
  end;

  FForms.Free;
  inherited;
end;

function TAdvAppStyler.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvAppStyler.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvAppStyler.Loaded;
begin
  inherited;
  if FAutoThemeAdapt then
  begin
    ThemeChanged(Self);
  end;
end;

procedure TAdvAppStyler.RegisterFormStyler(AFormStyler: TAdvFormStyler);
begin
  if FForms.IndexOf(Pointer(AFormStyler)) = -1 then
    FForms.Add(Pointer(AFormStyler));
end;

procedure TAdvAppStyler.SetAutoThemeAdapt(const Value: boolean);
begin
  FAutoThemeAdapt := Value;
  ThemeChanged(Self);
end;

procedure TAdvAppStyler.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  FTones := ATones;

  FMetro := true;
  FStyle := tsCustom;
  FMetroColor := ATones.Selected.BrushColor;
  FMetroTextColor := ATones.Selected.BrushColor;

  if ATones.Background.BrushColor = clWhite then
    FMetroStyle := msLight;

  if ATones.Background.BrushColor = clBlack then
    FMetroStyle := msDark;


  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    for i := 0 to FForms.Count - 1 do
    begin
      TAdvFormStyler(FForms[i]).SetColorTones(ATones);
    end;
  end;
  DoChange;
end;

procedure TAdvAppStyler.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    if FEnabled then
      ApplyStyle;
  end;
end;

procedure TAdvAppStyler.SetMetro(const Value: boolean);
begin
  if (FMetro <> Value) then
  begin
    FMetro := Value;
    ChangeTones;
  end;
end;

procedure TAdvAppStyler.SetMetroColor(const Value: TColor);
begin
  if (FMetroColor <> Value) then
  begin
    FMetroColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvAppStyler.SetMetroStyle(const Value: TMetroStyle);
begin
  if (FMetroStyle <> Value) then
  begin
    FMetroStyle := Value;
    ChangeTones;
  end;
end;

procedure TAdvAppStyler.SetMetroTextColor(const Value: TColor);
begin
  if (FMetroTextColor <> Value) then
  begin
    FMetroTextColor := Value;
    ChangeTones;
  end;
end;

procedure TAdvAppStyler.ChangeTones;
var
  FTones: TColorTones;
  i: integer;
begin
  FTones := CreateMetroTones(MetroStyle = msLight, FMetroColor, FMetroTextColor);

  if (FStyle = tsCustom) and FMetro then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    begin
      for i := 0 to FForms.Count - 1 do
      begin
        TAdvFormStyler(FForms[i]).SetColorTones(FTones);
      end;
    end;
    DoChange;
  end;
end;

procedure TAdvAppStyler.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAdvAppStyler.ApplyStyle;
var
  i: integer;
begin
  if not FEnabled then
    Exit;

  for i := 0 to FForms.Count - 1 do
  begin
    TAdvFormStyler(FForms[i]).Style := FStyle;
  end;

  DoChange;
end;

procedure TAdvAppStyler.SetStyle(const Value: TTMSStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;

    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    begin
      ApplyStyle;
    end;
  end;
end;

procedure TAdvAppStyler.SetVersion(const Value: string);
begin
end;

procedure TAdvAppStyler.ThemeChanged(Sender: TObject);
var
  ot: TOfficeTheme;
begin
  if not AutoThemeAdapt then
    Exit;

  ot := GetOfficeTheme;

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
  ot2013White: Style := tsOffice2013White;
  ot2013Silver: Style := tsOffice2013LightGray;
  ot2013Gray: Style := tsOffice2013Gray;
  ot2016White: Style := tsOffice2016White;
  ot2016Gray: Style := tsOffice2016Gray;
  ot2016Black: Style := tsOffice2016Black;
  end;

end;

procedure TAdvAppStyler.UnRegisterFormStyler(AFormStyler: TAdvFormStyler);
var
  idx: integer;
begin
  idx := FForms.IndexOf(Pointer(AFormStyler));
  if idx <> -1 then
    FForms.Delete(idx);
end;

end.
