{***********************************************************************}
{ TAdvIPEdit component                                                  }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2014 - 2015                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvIPEdit;

interface

uses
  Windows, Classes, StdCtrls, Controls, AdvEdit, Forms, Messages, SysUtils,
  MaskUtils, Dialogs;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : Added support to use right & left arrow keys to move between fields
  //          : Fixed : No longer allow minus sign to be entered
  //          : Improved : Allow space & backspace to be used to toggle between fields
  //          : Fixed : Issue with label font setting
  //          : Fixed : Issue with disabled color handling
  // v1.1.0.0 : New : Support added for mac address type
  // v1.1.0.1 : Improved : Disabled state
  // v1.1.0.2 : Fixed : Issue with OnKey* events for sub octets
  // v1.1.0.3 : Fixed : Issue with display when non default is used
  // v1.1.0.4 : Fixed : Issue with OnChange handling
  // v1.1.0.5 : Fixed : Issue with setting & getting IP address when control is not yet created
  // v1.1.0.6 : Fixed : Issue with FocusColor / FocusBorderColor
  // v1.1.0.7 : Fixed : Issue with setting IP address type at runtime before parent is set
  // v1.1.0.8 : Fixed : Issue with setting Enabled when handle is not allocated
  // v1.1.1.0 : Improved : Removal of unneeded leading zeroes

type
  TNumericInputMode = (niNumbers, niHex);

  TAdvNumCustomMaskEdit = class(TAdvCustomMaskEdit)
  private
    FMode: TNumericInputMode;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
  protected
    property Mode: TNumericInputMode read FMode write FMode;
  end;

  TIPAddressType = (ipv4, ipv6, mac);

  TAdvIPEdit = class(TAdvNumCustomMaskEdit)
  private
    FOctets: array[1..7] of TAdvNumCustomMaskEdit;
    FNumOctets: integer;
    FOctetLen: integer;
    FSeparators: array[1..7] of TStaticText;
    FIPAddress: string;
    FIPAddressType: TIPAddressType;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    function GetEditMask: TEditMask;
    procedure SetEditMask(const Value: TEditMask);
    procedure SetIPAddress(const Value: string);
    function GetIPAddress: string;
    procedure IPToOctets;
    procedure SetIPAddressType(const Value: TIPAddressType);
  protected
    procedure DestroySubControls;
    procedure CreateSubControls;
    procedure UpdateSubControls;
    procedure InitControls;
    procedure SetEditRect;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OctetKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OctetKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OctetKeyPress(Sender: TObject; var Key: Char);
    procedure OctetChange(Sender: TObject);
    procedure OctetExit(Sender: TObject);
    procedure Resize; override;
    procedure DoEnter; override;
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    // TCustomMaskEdit properties
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    {$IFDEF DELPHIXE_LVL}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Visible;
    // TAdvCustomMaskEdit properties
    property AutoFocus;
    property AutoTab;
    property CanUndo;
    property BorderColor;
    property DisabledBorder;
    property DisabledColor;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property ShowModified;
    property FocusBorderColor;
    property FocusColor;
    property FocusBorder;
    property FocusFontColor;
    property FocusLabel;
    property LabelCaption;
    property LabelAlwaysEnabled;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelFont;
    property ModifiedColor;
    property ReturnIsTab;
    property SoftBorder;
    property SelectFirstChar;
    property Version;

    property EditMask: TEditMask read GetEditMask write SetEditMask;
    property IPAddress: string read GetIPAddress write SetIPAddress;
    property IPAddressType: TIPAddressType read FIPAddressType write SetIPAddressType;

    // events
    property OnMaskComplete;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    {$IFDEF DELPHIXE_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

const
  SEPARATOR_WIDTH = 5;
  IPV4SEPARATOR = '.';
  IPV6SEPARATOR = ':';
  MACSEPARATOR  = '-';

const
  Ctrl_Codes = [vk_back, vk_tab, vk_return];
  Numeric_Codes = [ord('0')..ord('9')];
  Hex_Codes = [ord('A')..ord('F'), ord('a') .. ord('f')];

{ TAdvIPEdit }

procedure TAdvIPEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdateSubControls;
  if HandleAllocated then
    SetEditRect;
end;

procedure TAdvIPEdit.CMEnabledChanged(var Msg: TMessage);
var
  i: integer;
begin
  inherited;
  if HandleAllocated then
  begin
    for i := 1 to FNumOctets - 1 do
      FOctets[i].Enabled := Enabled;
  end;
end;

procedure TAdvIPEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateSubControls;
  if HandleAllocated then
    SetEditRect;
end;

constructor TAdvIPEdit.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;

  for i := 1 to 7 do
  begin
    FOctets[i] := nil;
    FSeparators[i] := nil;
  end;

  Alignment := taCenter;
  FIPAddressType := ipv4;
  FOctetLen := 3;
  FNumOctets := 4;
  MaxLength := FOctetLen;
  Width := 140;
  IPAddress := '0' + IPV4SEPARATOR + '0' + IPV4SEPARATOR + '0' + IPV4SEPARATOR + '0';
end;

procedure TAdvIPEdit.CreateSubControls;
var
  i: integer;
  m: TNumericInputMode;
begin
  if FIPAddressType = ipv4 then
    m := niNumbers
  else
    m := niHex;

  for i := 1 to FNumOctets - 1 do
  begin
    if not Assigned(FOctets[i]) then
    begin
      FOctets[i] := TAdvNumCustomMaskEdit.Create(Self);
      FOctets[i].Parent := Self;
      FOctets[i].BorderStyle := bsNone;
      FOctets[i].Top := 2;
      FOctets[i].MaxLength := FOctetLen;
      FOctets[i].OnKeyUp := OctetKeyUp;
      FOctets[i].OnKeyDown := OctetKeyDown;
      FOctets[i].OnKeyPress := OctetKeyPress;
      FOctets[i].OnChange := OctetChange;
      FOctets[i].OnExit := OctetExit;
      FOctets[i].Tag := i;
      FOctets[i].PopupMenu := PopupMenu;
      FOctets[i].Mode := m;
      FOctets[i].Alignment := taCenter;
    end;

    if not Assigned(FSeparators[i]) then
    begin
      FSeparators[i] := TStaticText.Create(Self);
      FSeparators[i].Parent := Self;
      case IPAddressType of
        ipv4: FSeparators[i].Caption := '·';
        ipv6: FSeparators[i].Caption := ':';
        mac: FSeparators[i].Caption := '-';
      end;
      FSeparators[i].Top := 2;
    end;
  end;
end;

procedure TAdvIPEdit.CreateWnd;
begin
  inherited;
  Width := Width - 1;
  Width := Width + 1;

  InitControls;
  SetEditRect;
end;

procedure TAdvIPEdit.DestroySubControls;
var
  i: integer;
begin
  if Assigned(FOctets[1]) then
  begin
    for i := 1 to FNumOctets - 1 do
    begin
      FOctets[i].Free;
      FOctets[i] := nil;
      FSeparators[i].Free;
      FSeparators[i] := nil;
    end;
  end;
end;

procedure TAdvIPEdit.DoEnter;
begin
  inherited;
  SetEditRect;
end;

function TAdvIPEdit.GetEditMask: TEditMask;
begin
  Result := inherited EditMask;
end;

function StripLeadingZeros(s: string): string;
var
  i: integer;
begin
  for i := 1 to Length(s) - 1 do
  begin
    if (s[i] = '0') then
      s[i] := ' '
    else
     break;
  end;

  Result := Trim(s);
end;

function TAdvIPEdit.GetIPAddress: string;
var
  i: integer;
begin
  if (FNumOctets > 0) and Assigned(FOctets[1]) then
  begin
    Result := StripLeadingZeros(Text);

    for i := 1 to FNumOctets - 1 do
    begin
      begin
        case IPAddressType of
        IPV4: Result := Result + IPV4SEPARATOR + StripLeadingZeros(FOctets[i].Text);
        IPV6: Result := Result + IPV6SEPARATOR + StripLeadingZeros(FOctets[i].Text);
        MAC:  Result := Result + MACSEPARATOR + StripLeadingZeros(FOctets[i].Text);
        end;
      end;
    end;
  end
  else
    Result := FIPAddress;
end;

function TAdvIPEdit.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvIPEdit.InitControls;
begin
  CreateSubControls;
  UpdateSubControls;
  SetEditRect;
  IPToOctets;
end;

procedure TAdvIPEdit.IPToOctets;
var
  s,pt: string;
  i: integer;
  SEP,EMPT: string;

  procedure Extend(var s: string);
  begin
    if IPAddressType = ipv6 then
    begin
      while length(s) < 4 do
        s := '0' + s;
    end;

    if IPAddressType = mac then
    begin
      while length(s) < 2 do
        s := '0' + s;
    end;
  end;

begin

  if Assigned(FOctets[1]) then
  begin
    s := FIPAddress;

    case IPAddressType of
      ipv4:
        begin
          SEP := IPV4SEPARATOR;
          EMPT := '0';
        end;
      ipv6:
        begin
          SEP := IPV6SEPARATOR;
          EMPT := '0000';
        end;
      mac:
        begin
          SEP := MACSEPARATOR;
          EMPT := '00';
        end;
    end;

    i := Pos(SEP,s);

    if i > 1 then
      pt := copy(s,1,i - 1)
    else
    begin
      pt := EMPT;
      s := '';
    end;

    Extend(pt);

    Text := pt;

    delete(s,1,Pos(SEP,s));

    i := 1;
    while (length(s) > 0) and (Pos(SEP,S) > 0) and (i < FNumOctets) do
    begin
      pt := copy(s,1,Pos(SEP,s) - 1);
      Extend(pt);
      FOctets[i].Text := pt;
      delete(s,1,Pos(SEP,s));
      inc(i);
    end;

    if (i < FNumOctets) and (s <> '') then
    begin
      Extend(s);
      FOctets[i].Text := s;
      inc(i);
    end;

    while (i < FNumOctets) do
    begin
      FOctets[i].Text := EMPT;
      inc(i);
    end;
  end;
end;

procedure TAdvIPEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  i,e: integer;
begin
  inherited;

  if (Length(Text) = FOctetLen) and (SelStart = FOctetLen) then
  begin
    if IPAddressType = ipv4 then
    begin
      val(Text,i,e);
      if (e <> 0) or (i > 255) then
        Text := '0';
    end;

    FOctets[1].SetFocus;
  end;

  if (Key = VK_RIGHT) or (Key = VK_SPACE) then
  begin
    FOctets[1].SetFocus;
  end;

end;

procedure TAdvIPEdit.Loaded;
begin
  inherited;
  SetEditRect;
  Width := Width + 1;
  Width := Width - 1;
end;

procedure TAdvIPEdit.OctetChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvIPEdit.OctetExit(Sender: TObject);
var
  s: string;
begin
  s := (Sender as TAdvNumCustomMaskEdit).Text;

  if IPAddressType = ipv6 then
  begin
    if length(s) < 4 then
    begin
      while (Length(s) < 4) do
        s := '0' + s;
    end;
  end
  else
    s := StripLeadingZeros(s);

  (Sender as TAdvNumCustomMaskEdit).Text := s;
end;

procedure TAdvIPEdit.OctetKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvIPEdit.OctetKeypress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;


procedure TAdvIPEdit.OctetKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  tg,i,e: integer;
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);

  if (Length((Sender as TAdvNumCustomMaskEdit).Text) = FOctetLen) and ((Sender as TAdvNumCustomMaskEdit).SelStart = FOctetLen)  then
  begin
    if IPAddressType = ipv4 then
    begin
      val((Sender as TAdvNumCustomMaskEdit).Text,i,e);
      if (e <> 0) or (i > 255) then
        (Sender as TAdvNumCustomMaskEdit).Text := '0';
    end;

    tg := (Sender as TAdvNumCustomMaskEdit).Tag;
    if tg < FNumOctets - 1 then
    begin
      FOctets[tg + 1].SetFocus;
    end;
  end;

  if ((Key = VK_RIGHT) and ((Sender as TAdvNumCustomMaskEdit).SelStart = 1 + Length((Sender as TAdvNumCustomMaskEdit).Text)))
   or (Key = VK_SPACE) then
  begin
    tg := (Sender as TAdvNumCustomMaskEdit).Tag;
    if tg < FNumOctets - 1 then
    begin
      FOctets[tg + 1].SetFocus;
    end;
  end;

  if (Key = VK_LEFT) or ((Key = VK_BACK) and ((Sender as TAdvNumCustomMaskEdit).SelStart = 0)) then
  begin
    if (Sender as TAdvNumCustomMaskEdit).Text = '' then
      (Sender as TAdvNumCustomMaskEdit).Text := '0';

    tg := (Sender as TAdvNumCustomMaskEdit).Tag;
    if tg > 1 then
    begin
      FOctets[tg - 1].SetFocus;
    end
    else
    begin
      SetFocus;
    end;
  end;
end;

procedure TAdvIPEdit.Resize;
begin
  inherited;
  UpdateSubControls;
  SetEditRect;
end;

procedure TAdvIPEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetEditRect;
end;

procedure TAdvIPEdit.SetEditMask(const Value: TEditMask);
var
  i: integer;
begin
  inherited EditMask := Value;

  for i := 1 to FNumOctets - 1 do
  begin
    if Assigned(FOctets[i]) then
      FOctets[i].EditMask := Value;
  end;
end;

procedure TAdvIPEdit.SetEditRect;
var
  Loc: TRect;
begin
  if (FNumOctets > 0) and HandleAllocated then
  begin
    SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
    Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
    Loc.Right := -3 + (ClientWidth - SEPARATOR_WIDTH * (FNumOctets - 1)) div FNumOctets;

    if (BorderStyle = bsNone) then
    begin
      Loc.Top := 4;
      if Flat then
        Loc.Left := 4
      else
        Loc.Left := 2;
    end
    else
    begin
      Loc.Top := 1;
      Loc.Left := 1;
    end;

    SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  end;
end;

procedure TAdvIPEdit.SetIPAddress(const Value: string);
begin
  FIPAddress := Value;
  IPToOctets;
end;

procedure TAdvIPEdit.SetIPAddressType(const Value: TIPAddressType);
begin
  if (FIPAddressType <> Value) then
  begin
    FIPAddressType := Value;
    case Value of
      ipv4:
        begin
          FNumOctets := 4;
          FOctetLen := 3;
          Mode := niNumbers;
        end;
      ipv6:
        begin
          FNumOctets := 8;
          FOctetLen := 4;
          Mode := niHex;
        end;
      mac:
        begin
          FNumOctets := 6;
          FOctetLen := 2;
          Mode := niHex;
        end;
    end;

    MaxLength := FOctetLen;
    DestroySubControls;
    InitControls;
  end;
end;

procedure TAdvIPEdit.UpdateSubControls;
var
  w,i: integer;

begin
  if Assigned(FOctets[1]) and HandleAllocated then
  begin
    for i := 1 to FNumOctets - 1 do
    begin
      w := (ClientWidth - SEPARATOR_WIDTH * (FNumOctets - 1)) div FNumOctets;
      FOctets[i].Width := w;
      FOctets[i].Color := Color;
      FOctets[i].Left := 2 + (w + SEPARATOR_WIDTH) * i;
      FSeparators[i].Left := -2 + (i * w + (i - 1) * SEPARATOR_WIDTH);
      FOctets[i].PopupMenu := PopupMenu;
      FOctets[i].Alignment := taCenter;
    end;
  end;
end;


procedure TAdvIPEdit.WMKillFocus(var Message: TMessage);
var
  s: string;
begin
  inherited;

  s := Text;

  if IPAddressType = ipv6 then
  begin
    if length(s) < 4 then
    begin
      while (Length(s) < 4) do
        s := '0' + s;
    end;
  end
  else
    s := StripLeadingZeros(s);

  Text := s;
end;

{ TAdvNumCustomMaskEdit }

procedure TAdvNumCustomMaskEdit.WMChar(var Msg: TWMKey);
var
  cond: boolean;
begin
  if Mode = niNumbers then
    cond := (Msg.CharCode in Numeric_Codes + Ctrl_Codes)
  else
    cond := (Msg.CharCode in Numeric_Codes + Ctrl_Codes + Hex_Codes);

  if cond then
    inherited
  else
    Msg.Result := 1;
end;


end.
