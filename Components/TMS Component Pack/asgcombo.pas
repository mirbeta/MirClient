{**********************************************************************}
{ TAsgComboBox component                                               }
{ for Delphi & C++Builder                                              }
{                                                                      }
{ written by                                                           }
{  TMS Software                                                        }
{  copyright © 1996-2012                                               }
{  Email : info@tmssoftware.com                                        }
{  Web : http://www.tmssoftware.com                                    }
{                                                                      }
{ The source code is given as is. The author is not responsible        }
{ for any possible damage done due to the use of this code.            }
{ The component can be freely used in any application. The source      }
{ code remains property of the author and may not be distributed       }
{ freely as such.                                                      }
{**********************************************************************}

unit AsgCombo;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Forms, Controls, Graphics, StdCtrls,
  SysUtils, ADVXPVS
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}
  ;

type
  TWinCtrl = class(TWinControl);

  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
                    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter,
                    lpBottomCenter, lpRightTop, lpRightCenter, lpRighBottom);

  TAsgCustomCombo = class(TCustomComboBox)
  private
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
    FFlatLineColor: TColor;
    FFlatParentColor: Boolean;
    FOnDropUp: TNotifyEvent;
    FFocusColor: TColor;
    FFocusBorderColor: TColor;
    FBorderColor: TColor;
    FDisabledBorder: boolean;
    FNormalColor: TColor;
    FHasFocus: Boolean;
    FFocusLabel: Boolean;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColorDown: TColor;
    FIsVista: Boolean;
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
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
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
    procedure LabelFontChange(Sender: TObject);
    procedure SetFlatLineColor(const Value: TColor);
    procedure SetFlatParentColor(const Value: Boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    function GetEnabledEx: Boolean;
    procedure SetEnabledEx(const Value: Boolean);
    function GetVisibleEx: boolean;
    procedure SetVisibleEx(const Value: boolean);
    procedure SetBorderColor(const Value: TColor);
  protected
    function DoVisualStyles: Boolean;
    function CreateLabel: TLabel;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 19;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FlatLineColor: TColor read FFlatLineColor write SetFlatLineColor default clNone;
    property FlatParentColor: Boolean read FFlatParentColor write SetFlatParentColor default True;
    property Etched: Boolean read FEtched write SetEtched default False;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder default False;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default False;
    property DropWidth: integer read FDropWidth write SetDropWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property DisabledBorder: Boolean read FDisabledBorder write FDisabledBorder default true;
    property IsWinXP: Boolean read FIsWinXP write FIsWinXP;
    property IsVista: Boolean read FIsVista write FIsVista;
    property LabelCaption:string read GetLabelCaption write SetLabelCaption;
    property LabelPosition:TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont:TFont read FLabelFont write SetLabelFont;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    procedure SelectItem(AString: string);
  published
    {$IFDEF DELPHI7_LVL}
    property AutoComplete;
    {$ENDIF}
    property Color: TColor read GetColorEx write SetColorEx;
    property OnDropUp: TNotifyEvent read FOnDropUp write FOnDropUp;
    property Visible: boolean read GetVisibleEx write SetVisibleEx;
  end;

  TAsgComboBox = class(TAsgCustomCombo)
  published
    property Align;
    property AutoFocus;
    property BorderColor;
    property ButtonWidth;
    property DisabledBorder;
    property Style;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property Etched;
    property FocusBorder;
    property FocusBorderColor;
    property FocusColor;
    property FocusLabel;
    {$IFDEF DELPHI7_LVL}
    property CharCase;
    {$ENDIF}
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
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
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
    {$IFDEF DELPHI6_LVL}
    property OnSelect;
    {$ENDIF}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$IFDEF DELPHI7_LVL}
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property BevelEdges;
    {$ENDIF}
  end;


implementation

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
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}


{ TAsgCustomCombo }
constructor TAsgCustomCombo.Create(AOwner: TComponent);
var
  i: Integer;

begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
//  FOldColor := inherited Color;
//  FOldParentColor := inherited ParentColor;
  FFlat := False;
  FMouseInControl := False;
  FDisabledBorder := True;


  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);

  FButtonHover := False;
  FLabel := nil;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
  FLabelMargin := 4;
  FFlatLineColor := clNone;
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
end;

procedure TAsgCustomCombo.SetButtonWidth(const Value: integer);
begin
  if (value<14) or (value>32) then
    Exit;

  FButtonWidth := Value;
  Invalidate;
end;

procedure TAsgCustomCombo.SetFlat(const Value: Boolean);
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

procedure TAsgCustomCombo.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := Value;
    Invalidate;
  end;
end;

procedure TAsgCustomCombo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := true;
    DrawBorders;
    if (FFocusColor <> clNone) then
      Color := FFocusColor;

    if FFocusLabel and (FLabel <> nil) then
      FLabel.Font.Style := FLabel.Font.Style + [fsBold];
  end;
end;

procedure TAsgCustomCombo.CMExit(var Message: TCMExit);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FHasFocus := false;
    DrawBorders;
    if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
      Color := FNormalColor;

    if FFocusLabel and (FLabel <> nil) then
      FLabel.Font.Style := FLabel.Font.Style - [fsBold];

//    if not FIsThemed then
//    begin
//      Width := Width + 1;
//      Width := Width - 1;
//    end;
  end;
end;

procedure TAsgCustomCombo.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont then
    begin
      FLabel.Font.Assign(self.Font);
    end;

  inherited;
end;

procedure TAsgCustomCombo.CMMouseEnter(var Message: TMessage);
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

procedure TAsgCustomCombo.CMMouseLeave(var Message: TMessage);
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

procedure TAsgCustomCombo.CMEnabledChanged(var Msg: TMessage);
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

procedure TAsgCustomCombo.WMNCPaint(var Message: TMessage);
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


procedure TAsgCustomCombo.WMPaint(var Message: TWMPaint);
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
        ARect.Left := ARect.Left - 4;
        ARect.Top := ARect.Top + 1;
        ARect.Bottom := ARect.Bottom - 1;

        if Enabled then
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT )
        else
          DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE )
      end;
    end;

    ExcludeClipRect(DC, ClientWidth - FButtonWidth -4 , 0, ClientWidth +2, ClientHeight);
  end;

begin
  if FIsVista and (FButtonColor = clNone) then
  begin
    inherited;
    Exit;
  end;

  if not (FFlat or (FButtonColor <> clNone) or (FBorderColor <> clNone) or ((FFocusBorderColor <> clNone) and FHasFocus)) and not (not Enabled and DoVisualStyles and not DisabledBorder) then
  begin
    inherited;
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
      if BidiMode = bdLeftToRight then
        DrawButton;
    end;

    PaintWindow(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;

  if BidiMode = bdLeftToRight then
    DrawBorders;
end;

function TAsgCustomCombo.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := Result and FFocusBorder;
end;

function TAsgCustomCombo.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

procedure TAsgCustomCombo.DrawButtonBorder(DC: HDC);
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
  begin
    //DrawEdge(DC, ARect, Edge[Etched], BF_RECT or Flags[DroppedDown]);
  end
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

procedure TAsgCustomCombo.DrawControlBorder(DC: HDC);
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

procedure TAsgCustomCombo.DrawBorders;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FBorderColor <> clNone) or (FFocusBorderColor <> clNone)) then
    Exit;

  if FIsVista and (FBorderColor = clNone) then
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

procedure TAsgCustomCombo.CNCommand(var Message: TWMCommand);
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


procedure TAsgCustomCombo.SetDropWidth(const Value: integer);
begin
  FDropWidth := Value;
  if Value > 0 then
    SendMessage(self.Handle,CB_SETDROPPEDWIDTH,FDropWidth,0);
end;

function TAsgCustomCombo.DoVisualStyles: Boolean;
begin
  if FIsThemed then
    Result := IsThemeActive
  else
    Result := False;
end;

procedure TAsgCustomCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
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

function TAsgCustomCombo.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

procedure TAsgCustomCombo.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then UpdateLabel;  
end;

procedure TAsgCustomCombo.SetLabelCaption(const Value: string);
begin
  if FLabel = nil then
     FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAsgCustomCombo.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAsgCustomCombo.SetLabelMargin(const Value: Integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAsgCustomCombo.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAsgCustomCombo.SetLabelTransparent(const Value: Boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

destructor TAsgCustomCombo.Destroy;
begin
  FlabelFont.Destroy;
  if FLabel <> nil then
    FLabel.Free;
  inherited;
end;

function TAsgCustomCombo.CreateLabel: TLabel;
begin
  Result := Tlabel.Create(self);
  Result.Parent:=self.parent;
  Result.FocusControl:=self;
  Result.Font.Assign(LabelFont);
  Result.ParentFont := self.ParentFont;  
end;


procedure TAsgCustomCombo.UpdateLabel;
var
  tw: integer;
  r: TRect;

begin
  FLabel.Transparent := FLabeltransparent;

  if not ParentFont then
    FLabel.Font.Assign(FLabelFont);

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
      FLabel.top := self.top+self.height+FLabelMargin;

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

procedure TAsgCustomCombo.LabelFontChange(Sender: TObject);
begin
  if FLabel <> nil then
  begin
    UpdateLabel;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      ParentFont := false;
  end;
end;

procedure TAsgCustomCombo.Loaded;
begin
  inherited;

  if FDropWidth > 0 then
    DropWidth := FDropWidth;

  FOldColor := FLoadedColor;

  if not FlatParentColor or Flat then
    Color := FLoadedColor;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
    FLabel.Enabled := Enabled;

  if ParentFont and Assigned(FLabel) then
  begin
    FLabel.Font.Assign(Font);
  end;

  if Assigned(FLabel) then
    UpdateLabel;

  Init;
end;

procedure TAsgCustomCombo.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;


procedure TAsgCustomCombo.Init;
begin
  FNormalColor := Color;
end;

procedure TAsgCustomCombo.SelectItem(AString: string);
var
  i: integer;
begin
  i := Items.IndexOf(Astring);
  if (i <> -1) then
    ItemIndex := i;
end;

procedure TAsgCustomCombo.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0,0);
  end;
end;

procedure TAsgCustomCombo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if (csDestroying in ComponentState) then
    Exit;
  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAsgCustomCombo.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAsgCustomCombo.SetFlatParentColor(const Value: Boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

function TAsgCustomCombo.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAsgCustomCombo.SetColorEx(const Value: TColor);
begin
  if (csLoading in ComponentState) then
    FLoadedColor := Value;

  inherited Color := Value;
end;

function TAsgCustomCombo.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAsgCustomCombo.SetEnabledEx(const Value: Boolean);
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

function TAsgCustomCombo.GetVisibleEx: boolean;
begin
  Result := inherited Visible;
end;

procedure TAsgCustomCombo.SetVisibleEx(const Value: boolean);
begin
  inherited Visible := Value;
  if Assigned(FLabel) then
    FLabel.Visible := Value;
end;

procedure TAsgCustomCombo.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  if (FFocusColor <> clNone) and (FNormalColor <> clNone) then
    Color := FNormalColor;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];

  Invalidate;
end;

procedure TAsgCustomCombo.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (FFocusBorderColor <> clNone) then
   Invalidate;

  if (FFocusColor <> clNone) then
  begin
    inherited Color := FFocusColor;
  end;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];
end;


end.

