{**********************************************************************}
{ TAdvComboBox component                                               }
{ for Delphi & C++Builder                                              }
{                                                                      }
{ written by                                                           }
{  TMS Software                                                        }
{  copyright © 1996 - 2015                                             }
{  Email : info@tmssoftware.com                                        }
{  Web : http://www.tmssoftware.com                                    }
{                                                                      }
{ The source code is given as is. The author is not responsible        }
{ for any possible damage done due to the use of this code.            }
{ The component can be freely used in any application. The source      }
{ code remains property of the author and may not be distributed       }
{ freely as such.                                                      }
{**********************************************************************}

unit AdvCombo;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Forms, Controls, Graphics, StdCtrls,
  SysUtils, ACXPVS
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF};

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // 1.1.0.1 : fixed issue with changing visibility at runtime
  // 1.2.0.0 : New FocusColor, FocusBorderColor properties added
  //         : Improved DFM property storage
  // 1.2.1.0 : Exposed ComboLabel public property
  // 1.2.2.0 : Improved : behaviour with ParentFont = true for LabelFont
  // 1.2.3.0 : New : property DisabledBorder added
  // 1.2.4.0 : New : method SelectItem added
  // 1.2.4.1 : Fixed : issue with label margin
  // 1.2.5.0 : New : exposed Align property
  // 1.2.5.1 : Fixed : possible issue with label positioning for large label font
  // 1.2.5.2 : Improved : painting of dropdown button in flat mode
  // 1.2.5.3 : Fixed : focus border paint issue on Windows Vista
  // 1.2.6.0 : New : FocusLabel property added
  // 1.2.6.1 : Improved : painting in flat mode
  // 1.3.0.0 : New : label RightTop, RightCenter, RightBottom positions added
  // 1.3.0.1 : Fixed : issue with LabelFont and Form ScaleBy
  // 1.3.1.0 : New : Exposed CharCase property
  // 1.3.2.0 : New : Events OnMouseEnter / OnMouseLeave exposed
  // 1.3.2.1 : Fixed : Issue with ParentFont persistence
  // 1.3.2.2 : Fixed : Issue with painting in non themed apps
  // 1.3.2.3 : Fixed : Issue with label positioning with accelerators
  // 1.4.0.0 : New : BorderColor property added
  // 1.4.1.0 : Improved : OnChange event triggered when SelectItem is called
  // 1.4.2.0 : New : Property FocusFontColor added
  // 1.4.3.0 : Improved : Label font handling with ChangeScale()
  // 1.4.3.1 : Fixed : Issue with label position & reparenting
  // 1.5.0.0 : New : EmptyText, EmptyTextFocused, EmptyTextStyle added
  // 1.5.0.1 : Fixed : Issue with EmptyText and BackgroundColor <> clWindow
  // 1.5.1.0 : New : Exposed : OnCloseUp event
  // 1.5.1.1 : Fixed : Issue with alignment & use of labels
  // 1.6.0.0 : New : Property ReturnIsTab added
  // 1.6.0.1 : Improved : Behavior of EmptyText / EmptyTextWhenFocused
  // 1.6.1.0 : Improved : Tab key handling when dropdown is visible
  // 1.6.1.1 : Fixed : Issue with setting LabelFont color at design time
  // 1.6.1.2 : Fixed : Rare issue with use via RDP & Windows XP

type
  TWinCtrl = class(TWinControl);

  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
                    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter,
                    lpBottomCenter, lpRightTop, lpRightCenter, lpRighBottom);

  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


  TAdvCustomCombo = class(TCustomComboBox)
  private
    //FEditHandle: THandle;
    FAutoFocus: boolean;
    FFlat: Boolean;
    FEtched: Boolean;
    FOldColor: TColor;
    FLoadedColor: TColor;
    FOldParentColor: Boolean;
    FButtonWidth: Integer;
    FFocusBorder: Boolean;
    FMouseInControl: Boolean;
    FDropWidth: integer;
    FIsWinXP: Boolean;
    FIsThemed: Boolean;
    FButtonHover: Boolean;
    FLabelAlwaysEnabled: Boolean;
    FLabelTransparent: Boolean;
    FLabelMargin: Integer;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabel: TLabel;
    FFocusLabel: boolean;
    FFlatLineColor: TColor;
    FFlatParentColor: Boolean;
    FOnDropUp: TNotifyEvent;
    FFocusColor: TColor;
    FFocusBorderColor: TColor;
    FBorderColor: TColor;
    FDisabledBorder: boolean;
    FNormalColor: TColor;
    FHasFocus: Boolean;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColorDown: TColor;
    FFocusFontColor: TColor;
    FParentFnt: boolean;
    FFontColor: TColor;
    FWinProc: LInteger;
    FEmptyTextStyle: TFontStyles;
    FEmptyText: string;
    FEmptyTextFocused: boolean;
    FReturnIsTab: boolean;
    FOldWin: boolean;
    procedure SetEtched(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetButtonWidth(const Value: Integer);
    procedure DrawButtonBorder(DC:HDC);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawBorders;
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNCommand (var Message: TWMCommand); message CN_COMMAND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure SetDropWidth(const Value: integer);
    function GetLabelCaption: string;
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelFont(const Value: TFont);
    procedure SetLabelMargin(const Value: Integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelTransparent(const Value: Boolean);
    procedure UpdateLabel;
    procedure UpdateLabelPos;
    procedure LabelFontChange(Sender: TObject);
    procedure SetFlatLineColor(const Value: TColor);
    procedure SetFlatParentColor(const Value: Boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    function GetEnabledEx: Boolean;
    procedure SetEnabledEx(const Value: Boolean);
    function GetVersionEx: string;
    procedure SetVersion(const Value: string);
    function GetVisibleEx: boolean;
    procedure SetVisibleEx(const Value: boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetEmptyText(const Value: string);
    procedure SetEmptyTextStyle(const Value: TFontStyles);
  protected
    property WinProc: LInteger read FWinProc write FWinProc;
    function GetVersionNr: Integer; virtual;
    function DoVisualStyles: Boolean;
    function CreateLabel: TLabel;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 19;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FlatLineColor: TColor read FFlatLineColor write SetFlatLineColor default clBlack;
    property FlatParentColor: Boolean read FFlatParentColor write SetFlatParentColor default True;
    property Etched: Boolean read FEtched write SetEtched default False;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder default False;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default False;
    property DropWidth: integer read FDropWidth write SetDropWidth;
    property ReturnIsTab: boolean read FReturnIsTab write FReturnIsTab default False;
    procedure DoExit; override;
    {$IFDEF DELPHI_UNICODE}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Init;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ComboLabel: TLabel read FLabel;

    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;

    property EmptyText: string read FEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read FEmptyTextFocused write FEmptyTextFocused default false;
    property EmptyTextStyle: TFontStyles read FEmptyTextStyle write SetEmptyTextStyle;

    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor default clNone;
    property DisabledBorder: Boolean read FDisabledBorder write FDisabledBorder default true;
    property LabelCaption:string read GetLabelCaption write SetLabelCaption;
    property LabelPosition:TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    procedure SelectItem(AString: string); virtual;

  published
    property AutoComplete;
    property Color: TColor read GetColorEx write SetColorEx;
    property OnDropUp: TNotifyEvent read FOnDropUp write FOnDropUp;
    property Version: string read GetVersionEx write SetVersion;
    property Visible: boolean read GetVisibleEx write SetVisibleEx;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvComboBox = class(TAdvCustomCombo)
  published
    property Align;
    {$IFDEF DELPHIXE_LVL}
    property AutoCloseUp;
    property AutoDropDown;
    {$ENDIF}
    property AutoFocus;
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property BevelEdges;
    property BorderColor;
    property ButtonWidth;
    property DisabledBorder;
    property Style;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property EmptyText;
    property EmptyTextFocused;
    property EmptyTextStyle;
    property Etched;
    property FocusBorder;
    property FocusBorderColor;
    property FocusColor;
    property FocusFontColor;
    property FocusLabel;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property DropWidth;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemIndex;
    property ItemHeight;
    property Items;
    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReturnIsTab;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnDropUp;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    {$IFDEF DELPHI2007_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDrag;
    property OnSelect;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;


implementation

var
  wpOrigEditProc: Integer;


function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion = 5);
end;

function EditSubclassProc(hwnd: THandle; uMsg: Integer; wParam: word; lparam: longint): Integer; stdcall;
begin
  Result := CallWindowProc(tfnwndproc(wpOrigEditProc), hwnd, uMsg, wParam, lParam);
end;

{ TAdvCustomCombo }
constructor TAdvCustomCombo.Create(AOwner: TComponent);
var
  dwVersion:Dword;
  dwWindowsMajorVersion,dwWindowsMinorVersion:Dword;
  i: Integer;

begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
  FFlat := False;
  FMouseInControl := False;
  FDisabledBorder := True;
  FReturnIsTab := False;

  dwVersion := GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion :=  DWORD(HIBYTE(LOWORD(dwVersion)));

  FIsWinXP := (dwWindowsMajorVersion > 5) OR
    ((dwWindowsMajorVersion = 5) AND (dwWindowsMinorVersion >= 1));

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);

  FParentFnt := true;
  FButtonHover := False;
  FLabel := nil;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
  FLabelMargin := 4;
  FFlatLineColor := clBlack;
  FFlatParentColor := True;
  FLoadedColor := clWindow;
  FFocusColor := clNone;
  FFocusBorderColor := clNone;
  FBorderColor := clNone;

  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonBorderColor := clNone;
  FFocusFontColor := clNone;

  FOldWin := IsWinXP;
end;

procedure TAdvCustomCombo.SetButtonWidth(const Value: integer);
begin
  if (value<14) or (value>32) then
    Exit;

  FButtonWidth := Value;
  Invalidate;
end;

procedure TAdvCustomCombo.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Ctl3D := not Value;
    if FFlatParentColor and FFlat then
    begin
      if (Parent is TWinControl) then
        inherited Color := (Parent as TWinControl).Brush.Color;
    end
    else
      inherited Color := FLoadedColor;

    Invalidate;
  end;
end;

procedure TAdvCustomCombo.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomCombo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := true;
    DrawBorders;
    if (FFocusColor <> clNone) then
      Color := FFocusColor;

    if FFocusLabel and (FLabel <> nil) then
    begin
      FLabel.Font.Style := FLabel.Font.Style + [fsBold];
      UpdateLabelPos;
    end;
  end;
end;

procedure TAdvCustomCombo.CMExit(var Message: TCMExit);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := false;
    DrawBorders;
    if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
      Color := FNormalColor;

    if FFocusLabel and (FLabel <> nil) then
    begin
      FLabel.Font.Style := FLabel.Font.Style - [fsBold];
      UpdateLabelPos;
    end;

//    if not FIsThemed then
//    begin
//      Width := Width + 1;
//      Width := Width - 1;
//    end;
  end;
end;

procedure TAdvCustomCombo.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont then
    begin
      FLabel.Font.Assign(self.Font);
    end;

  inherited;
end;

procedure TAdvCustomCombo.CMMouseEnter(var Message: TMessage);
var
  pf: TCustomForm;
begin
  inherited;

  if not FMouseInControl and Enabled then
    begin
     FMouseInControl := True;
     DrawBorders;
    end;

  pf := GetParentForm(self);

  if FAutoFocus and not (csDesigning in ComponentState) then
  begin
    if Assigned(pf) then
    begin
      if (GetActiveWindow = pf.Handle) then
        SetFocus;
    end
    else
      SetFocus;
  end;

  if FIsWinXP then
    Invalidate;
end;

procedure TAdvCustomCombo.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if FMouseInControl and Enabled then
    begin
     FMouseInControl := False;
     DrawBorders;
    end;

  FButtonHover := False;

  if FIsWinXP then
    Invalidate;
end;

procedure TAdvCustomCombo.CMEnabledChanged(var Msg: TMessage);
begin

  if FFlat then
  begin
    if not (csLoading in ComponentState) then
    begin
      if Enabled then
      begin
        inherited ParentColor := FOldParentColor;
        inherited Color := FOldColor;
      end
      else
      begin
        FOldParentColor := inherited Parentcolor;
        FOldColor := inherited Color;
        inherited ParentColor := True;
      end;
    end;
  end;

  inherited;
end;

procedure TAdvCustomCombo.WMNCPaint(var Message: TMessage);
begin
  inherited;

  if FFlat or (not Enabled and DoVisualStyles and not FDisabledBorder) then
    DrawBorders;

  if ((FFocusBorderColor <> clNone) and (GetFocus = Handle)) or (FBorderColor <> clNone) then
    DrawBorders;
end;

function IsMouseButtonDown: Boolean;
{
  Returns a "True" if a Mouse button happens to be down.
}
begin
  {Note: Key state is read twice because the first time you read it,
   you learn if the bittpm has been pressed ever.
   The second time you read it you learn if
   the button is currently pressed.}
  Result := not(((GetAsyncKeyState(VK_RBUTTON)and $8000)=0) and
     ((GetAsyncKeyState(VK_LBUTTON)and $8000)=0));
     (*
  begin
    {Mouse buttons are up}
    Result := False;
  end
  else
  begin
    {Mouse buttons are up}
    Result:=True;
  end;
  *)
end;


procedure TAdvCustomCombo.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;

  procedure DrawButton;
  var
    ARect: TRect;
    htheme: THandle;
    pt: array of TPoint;

  begin
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    Inc(ARect.Left, ClientWidth - FButtonWidth + 2);
    InflateRect(ARect, -1, -1);

    if FButtonColor <> clNone then
    begin
      Canvas.Brush.Color := FButtonColor;

      if FButtonHover then
        Canvas.Brush.Color := FButtonColorHot;

      if IsMouseButtonDown and FButtonHover then
        Canvas.Brush.Color := FButtonColorDown;

      Canvas.Pen.Color := Canvas.Brush.Color;

      ARect.Left := ARect.Left - 1;
      ARect.Top := ARect.Top + 1;
      ARect.Bottom := ARect.Bottom - 1;
      ARect.Right := ARect.Right - 1;
      Canvas.FillRect(ARect);

      Canvas.Pen.Color := FButtonBorderColor;
      Canvas.MoveTo(ARect.Left, ARect.Top);
      Canvas.LineTo(ARect.Left, ARect.Bottom);

      Canvas.Brush.Color := FButtonTextColor;

      if IsMouseButtonDown then
       Canvas.Brush.Color := FButtonTextColorDown;

      Canvas.Pen.Color := Canvas.Brush.Color;

      SetLength(pt, 3);

      pt[0].X := ARect.Left + 4;
      pt[0].y := ARect.Top + 7;

      pt[1].X := ARect.Left + 10;
      pt[1].y := ARect.Top + 7;

      pt[2].X := ARect.Left + 7;
      pt[2].y := ARect.Top + 10;

      Canvas.Polygon(pt);
    end
    else
    begin
      if DoVisualStyles then
      begin
        htheme := OpenThemeData(Handle,'combobox');

        if not Enabled then
        begin
          DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
        end
        else
        begin
          if IsMouseButtonDown and DroppedDown then
          begin
            DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
          end
          else
          begin
            if not IsMouseButtonDown and FButtonHover and not DroppedDown then
              DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
            else
            DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
          end;
        end;

        CloseThemeData(htheme);
      end
      else
      begin
        if Enabled then
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT )
        else
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE )
      end;
    end;

    ExcludeClipRect(DC, ClientWidth - FButtonWidth -4 , 0, ClientWidth +2, ClientHeight);
  end;


begin
  inherited;

  if not (FFlat or (FButtonColor <> clNone) or (FBorderColor <> clNone) or ((FFocusBorderColor <> clNone) and FHasFocus)) and not (not Enabled and DoVisualStyles and not DisabledBorder) then
  begin
    Exit;
  end;

  if Message.DC = 0 then
    DC := BeginPaint(Handle, PS)
  else
    DC := Message.DC;
  try
    if (Style <> csSimple) then
    begin
      FillRect(DC, ClientRect, Brush.Handle);
      DrawButton;
    end;

    PaintWindow(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;

  DrawBorders;
end;

function TAdvCustomCombo.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := Result and FFocusBorder;
end;

function TAdvCustomCombo.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TAdvCustomCombo.DrawButtonBorder(DC: HDC);
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  ARect: TRect;
  BtnFaceBrush: HBRUSH;

begin
  ExcludeClipRect(DC, ClientWidth - FButtonWidth + 4, 4, ClientWidth - 4, ClientHeight - 4);

  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth - 2);
  InflateRect(ARect, -2, -2);

  if Is3DBorderButton then
    DrawEdge(DC, ARect, Edge[Etched], BF_RECT or Flags[DroppedDown])
  else
  begin
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    InflateRect(ARect, 0, -1);
    ARect.Right := ARect.Right - 1;
    FillRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
  end;

  ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TAdvCustomCombo.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
  OldPen: HPen;

begin
  if not Enabled and FIsThemed and not DisabledBorder then
  begin
    BtnFaceBrush := CreateSolidBrush(ColorToRGB($B99D7F));
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    FrameRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
    Exit;
  end;

  if (FBorderColor <> clNone) then
  begin
    if not FHasFocus or (FFocusBorderColor = clNone) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
      Exit;
    end;
  end;

  if (FFocusBorderColor <> clNone) then
  begin
    if FHasFocus then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if Is3DBorderControl then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((Parent as TWinControl).Brush.Color));

  WindowBrush := CreateSolidBrush(ColorToRGB(self.Color));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);

      ARect.Right := ARect.Right - ButtonWidth; //GetSystemMetrics(SM_CXVSCROLL);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
      ARect.Right := ARect.Right + ButtonWidth; //GetSystemMetrics(SM_CXVSCROLL);
    end;

    if FFlat and (FFlatLineColor <> clNone) then
    begin
      OldPen := SelectObject(DC,CreatePen( PS_SOLID,1,ColorToRGB(FFlatLineColor)));
      MovetoEx(DC,ARect.Left - 2,Height - 1,nil);
      LineTo(DC,ARect.Right - ButtonWidth - 1 ,Height - 1);
      DeleteObject(SelectObject(DC,OldPen));
    end;

  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

procedure TAdvCustomCombo.DrawBorders;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FBorderColor <> clNone) or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    if (Style <> csSimple) and not
      (FIsWinXP and DoVisualStyles) then
        DrawButtonBorder(DC);
  finally
    ReleaseDC(Handle,DC);
  end;
end;

procedure TAdvCustomCombo.CNCommand(var Message: TWMCommand);
var
  r:TRect;
begin
  inherited;

  if (Message.NotifyCode in [CBN_CLOSEUP,CBN_DROPDOWN]) then
  begin
    r := GetClientRect;
    r.Left := r.Right - Fbuttonwidth;
    InvalidateRect(Handle,@r,FALSE);
    if (Message.NotifyCode = CBN_CLOSEUP) and Assigned(FOnDropUp) then
      FOnDropUp(Self);
  end;
end;


procedure TAdvCustomCombo.SetDropWidth(const Value: integer);
begin
  FDropWidth := Value;
  if Value > 0 then
    SendMessage(self.Handle,CB_SETDROPPEDWIDTH,FDropWidth,0);
end;

function TAdvCustomCombo.DoVisualStyles: Boolean;
begin
  if FIsThemed then
    Result := IsThemeActive
  else
    Result := False;
end;

procedure TAdvCustomCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (X > Width - FButtonWidth) and (X < Width) then
  begin
    if not FButtonHover then
    begin
      FButtonHover := True;
      Invalidate;
    end;
  end
  else
  begin
    if FButtonHover then
    begin
      FButtonHover := False;
      Invalidate;
    end;
  end;

end;

function TAdvCustomCombo.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

procedure TAdvCustomCombo.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then UpdateLabel;  
end;

procedure TAdvCustomCombo.SetLabelCaption(const Value: string);
begin
  if FLabel = nil then
     FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelMargin(const Value: Integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomCombo.SetLabelTransparent(const Value: Boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

destructor TAdvCustomCombo.Destroy;
begin
  FlabelFont.Destroy;
  if FLabel <> nil then
    FLabel.Free;
  inherited;
end;

function TAdvCustomCombo.CreateLabel: TLabel;
begin
  Result := TLabel.Create(self);
  Result.Parent:= Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
  Result.ParentFont := ParentFont;
end;

function EditWindowProc(hWnd: hWnd; uMsg: Integer; WParam: WParam;
  lParam: lParam): LRESULT; stdcall;
var
  OldWndProc: LInteger;
  DC: THandle;
  ACanvas: TCanvas;
  CC: TAdvCustomCombo;
  i: integer;
begin
  {$IFDEF DELPHI_UNICODE}
  CC := TAdvCustomCombo(GetWindowLongPtr(hWnd, GWL_USERDATA));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  CC := TAdvCustomCombo(GetWindowLong(hWnd, GWL_USERDATA));
  {$ENDIF}


  if (uMsg = WM_LBUTTONDOWN) and (CC.Text = '') and (CC.EmptyText <> '') and CC.EmptyTextFocused then
  begin
    SetFocus(hwnd);
    InvalidateRect(Hwnd,nil,true);
    Result := 1;
    Exit;
  end;

  if (uMsg = WM_CHAR) then
  begin
    if (wParam = VK_RETURN) and CC.ReturnIsTab  then
    begin
      Result := 1;
      Exit;
    end;

    if (wParam = ord(#9)) and CC.DroppedDown then
    begin
      Result := 1;
      Exit;
    end;
  end;

  OldWndProc := CC.WinProc;
  Result := CallWindowProc(IntPtr(OldWndProc), hWnd, uMsg, WParam, lParam);

  if (uMsg = WM_KEYUP) and not CC.FOldWin and CC.DroppedDown then
  begin
    if wParam = VK_TAB then
    begin
      i := cc.ItemIndex;
      PostMessage(CC.Handle, CB_SHOWDROPDOWN, Longint(False), 0);
      PostMessage(CC.Handle, CB_SETCURSEL, i, 0);
      PostMessage(CC.Handle, WM_KEYDOWN, VK_TAB, 0);
      Result := 1;
      Exit;
    end;
  end;

  if (uMsg = WM_KEYDOWN) and CC.DroppedDown then
  begin
    if wParam = VK_TAB then
    begin
      Result := 1;
      Exit;
    end;
  end;

  if (uMsg = WM_KEYUP) and CC.ReturnIsTab then
  begin

    if wParam = VK_RETURN then
    begin
      PostMessage(CC.Handle, WM_KEYDOWN, VK_TAB, 0);
      Result := 1;
      Exit;
    end;
  end;

  if (uMsg = WM_PAINT) and (CC.Text = '') and (CC.EmptyText <> '') then
  begin
    if not CC.EmptyTextFocused and (GetFocus = hwnd) then
      Exit;

    DC := GetDC(hwnd);
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      ACanvas.Font.Assign(CC.Font);
      ACanvas.Font.Color := clGray;
      ACanvas.Font.Style := CC.EmptyTextStyle;
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextOut(3,1, CC.EmptyText);
    finally
      ACanvas.Free;
      ReleaseDC(hwnd,dc);
    end;
  end;
end;

procedure TAdvCustomCombo.CreateWnd;
var
  ci: TComboBoxInfo;
begin
  inherited;
  if Assigned(FLabel) then
    UpdateLabelPos;

  ci.cbSize := Sizeof(ci);
  GetComboBoxInfo(Handle, ci);

  {$IFDEF DELPHI_UNICODE}
  if (GetWindowLongPtr(ci.hwndItem, GWL_WNDPROC) <> LInteger(@EditWindowProc)) then
  begin
    WinProc := GetWindowLongPtr(ci.hwndItem, GWL_WNDPROC);
    SetWindowLongPtr(ci.hwndItem, GWL_USERDATA, LInteger(Self));
    SetWindowLongPtr(ci.hwndItem, GWL_WNDPROC, LInteger(@EditWindowProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  if (GetWindowLong(ci.hwndItem, GWL_WNDPROC) <> LInteger(@EditWindowProc)) then
  begin
    WinProc := GetWindowLong(ci.hwndItem, GWL_WNDPROC);
    SetWindowLong(ci.hwndItem, GWL_USERDATA, LInteger(Self));
    SetWindowLong(ci.hwndItem, GWL_WNDPROC, LInteger(@EditWindowProc));
  {$ENDIF}
  end;
end;

procedure TAdvCustomCombo.UpdateLabel;
begin
  FLabel.Transparent := FLabeltransparent;

  if not FParentFnt then
    FLabel.Font.Assign(FLabelFont)
  else
    FLabel.Font.Assign(Font);

  if FLabel.Parent.HandleAllocated then
    UpdateLabelPos;
end;

procedure TAdvCustomCombo.UpdateLabelPos;
var
  tw: integer;
  r: TRect;

begin
  r := Rect(0,0,1000,255);
  DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
  tw := r.Right;

  case FLabelPosition of
  lpLeftTop:
    begin
      FLabel.top := Top;
      FLabel.left := Left- tw - LabelMargin;
    end;
  lpLeftCenter:
    begin
      if self.Height < FLabel.Height then
        FLabel.Top := Top - ((FLabel.Height - Height) div 2)
      else
        FLabel.top := Top + ((Height - FLabel.Height) div 2);
      FLabel.left := Left - tw - FLabelMargin;
    end;
  lpLeftBottom:
    begin
      FLabel.top := Top + Height - FLabel.Height;
      FLabel.left := Left - tw - FLabelMargin;
    end;
  lpTopLeft:
    begin
      FLabel.top := Top - FLabel.Height - FLabelMargin;
      FLabel.left := Left;
    end;
  lpTopCenter:
    begin
      FLabel.Top := Top - FLabel.Height - FLabelMargin;
      if (self.Width > FLabel.width) then
        FLabeL.Left := Left + ((Width - FLabel.width) div 2)
      else
        FLabeL.Left := Left - ((FLabel.Width - Width) div 2)
    end;
  lpBottomLeft:
    begin
      FLabel.top := Top + Height + FLabelMargin;
      FLabel.left := Left;
    end;
  lpBottomCenter:
    begin
      FLabel.top := self.Top + self.Height + FLabelMargin;

      if (self.Width > FLabel.width) then
        FLabeL.Left := Left + ((Width-FLabel.Width) div 2)
      else
        FLabeL.Left := Left - ((FLabel.Width - Width) div 2)
    end;
  lpLeftTopLeft:
    begin
      FLabel.top := Top;
      FLabel.left := Left-FLabelMargin;
    end;
  lpLeftCenterLeft:
    begin
      if self.Height < FLabel.Height then
        FLabel.Top := Top - ((FLabel.Height - self.Height) div 2)
      else
        FLabel.top := Top + ((self.height-FLabel.height) div 2);

      FLabel.left := Left - FLabelMargin;
    end;
  lpLeftBottomLeft:
    begin
      FLabel.top := Top + Height - FLabel.height;
      FLabel.left := Left - FLabelMargin;
    end;
  lpRightTop:
    begin
      FLabel.Top := Top;
      FLabel.Left := Left + Width + FLabelMargin;
    end;
  lpRightCenter:
    begin
      if Self.Height > FLabel.Height then
        FLabel.Top := Top + ((self.Height - FLabel.Height) div 2)
      else
        FLabel.Top := Top - ((FLabel.Height - Height) div 2);

      FLabel.Left := Left + Width + FLabelMargin;
    end;
  lpRighBottom:
    begin
      FLabel.Top := Top + Height - FLabel.Height;
      FLabel.Left := Left + Width + FLabelMargin;
    end;
  end;

  FLabel.Visible := Visible;
end;

procedure TAdvCustomCombo.LabelFontChange(Sender: TObject);
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    ParentFont := false;
    FParentFnt := false;
  end;

  if Assigned(FLabel) then
    UpdateLabel;
end;

procedure TAdvCustomCombo.Loaded;
begin
  inherited;

  if FDropWidth > 0 then
    DropWidth := FDropWidth;

  FOldColor := FLoadedColor;

  if not FlatParentColor or Flat then
    Color := FLoadedColor;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
    FLabel.Enabled := Enabled;

  if Assigned(FLabel) then
  begin
    if ParentFont then
      FLabel.Font.Assign(Font)
    else
      FLabel.Font.Assign(FLabelFont);
  end;

  FParentFnt := ParentFont;

  if Assigned(FLabel) then
    UpdateLabel;

  Init;
end;

procedure TAdvCustomCombo.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;


procedure TAdvCustomCombo.Init;
begin
  FNormalColor := Color;
end;

procedure TAdvCustomCombo.SelectItem(AString: string);
var
  i: integer;
begin
  i := Items.IndexOf(Astring);
  if (i <> -1) then
  begin
    ItemIndex := i;
    Change;
  end;
end;

procedure TAdvCustomCombo.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0,0);
  end;
end;

procedure TAdvCustomCombo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Assigned(FLabel) then
  begin
    case LabelPosition of
      lpLeftTop, lpLeftCenter, lpLeftBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
          begin
            AWidth := AWidth - (FLabel.Width + LabelMargin);
            ALeft := ALeft + (FLabel.Width + LabelMargin);
          end;
        end;
      lpRightTop, lpRightCenter, lpRighBottom:
        begin
          if (Align in [alTop, alClient, alBottom]) then
            AWidth := AWidth - (FLabel.Width + LabelMargin);
        end;
      lpTopLeft, lpTopCenter:
        begin
          if (Align in [alTop, alClient, alRight, alLeft]) then
            ATop := ATop + FLabel.Height;
        end;
    end;
  end;

  inherited;

  if (csDestroying in ComponentState) then
    Exit;
  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvCustomCombo.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAdvCustomCombo.SetFlatParentColor(const Value: Boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

function TAdvCustomCombo.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvCustomCombo.SetColorEx(const Value: TColor);
begin
  if (csLoading in ComponentState) then
    FLoadedColor := Value;

  inherited Color := Value;
end;

function TAdvCustomCombo.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvCustomCombo.SetEmptyText(const Value: string);
begin
  if (FEmptyText <> Value) then
  begin
    FEmptyText := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomCombo.SetEmptyTextStyle(const Value: TFontStyles);
begin
  if (FEmptyTextStyle <> Value) then
  begin
    FEmptyTextStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomCombo.SetEnabledEx(const Value: Boolean);
var
  OldValue: Boolean;

begin
  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
     (csDesigning in ComponentState) then
    Exit;

  if OldValue <> Value then
  begin
    if Assigned(FLabel) then
      if not FLabelAlwaysEnabled then
      begin
        FLabel.Enabled := Value;
        UpdateLabel;
      end;
  end;
end;

function TAdvCustomCombo.GetVersionEx: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomCombo.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvCustomCombo.SetVersion(const Value: string);
begin

end;

function TAdvCustomCombo.GetVisibleEx: boolean;
begin
  Result := inherited Visible;
end;

procedure TAdvCustomCombo.SetVisibleEx(const Value: boolean);
begin
  inherited Visible := Value;
  if Assigned(FLabel) then
    FLabel.Visible := Value;
end;

{$IFDEF DELPHI_UNICODE}
procedure TAdvCustomCombo.ChangeScale(M, D: Integer);
begin
  inherited;
  if not (csLoading in ComponentState) then
    FLabelFont.Size := Font.Size;
end;
{$ENDIF}

procedure TAdvCustomCombo.DoExit;
begin
  inherited;
  if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
    Color := FNormalColor;

  if (FFocusFontColor <> clNone) then
  begin
    Font.Color := FFontColor;
    if Assigned(FLabel) then
      UpdateLabel;
  end;

  if FFocusLabel and Assigned(FLabel) then
  begin
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];
    UpdateLabelPos;
  end;

  Invalidate;
end;


procedure TAdvCustomCombo.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
end;

procedure TAdvCustomCombo.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  if (FFocusBorderColor <> clNone) then
   Invalidate;

  if (FFocusColor <> clNone) then
  begin
    inherited Color := FFocusColor;
  end;

  if (FFocusFontColor <> clNone) and (Font.Color <> FFocusFontColor) then
  begin
    FFontColor := Font.Color;
    Font.Color := FFocusFontColor;
    if Assigned(FLabel) then
      UpdateLabel;
  end;

  if FFocusLabel and Assigned(FLabel) then
  begin
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
    UpdateLabelPos;
  end;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.

