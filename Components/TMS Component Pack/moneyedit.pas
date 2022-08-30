{********************************************************************}
{ TMoneyEdit component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2013                                   }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit MoneyEdit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.1.0.1 : Fixed issue with dropdown display in Delphi 2005
  // 1.1.1.0 : Fixed button caption painting issue

type
  TMoneyEdit = class;

{TTabForm}
  TTabForm = class(TForm)
  private
   FButtonWidth:integer;
   FButtonHeight:integer;
   FButtonColor:TColor;
   FEx:array[1..4] of boolean;
   procedure SetEx(i:integer;b:boolean);
   function GetEx(i:integer):boolean;
   procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
   procedure WMClose(var Msg:TMessage); message WM_CLOSE;
  protected
   procedure Paint; override;
  public
   constructor Create(aOwner:TComponent); override;
   destructor Destroy; override;
   property Ex[i:integer]:boolean read GetEx write SetEx;
  published
   property ButtonWidth:integer read FButtonWidth write FButtonWidth;
   property ButtonHeight:integer read FButtonHeight write FButtonHeight;
   property ButtonColor:TColor read FButtonColor write FButtonColor;
  end;

{ TDropCalculatorButton }
  TDropCalculatorButton = class(TSpeedButton)
  private
    FFocusControl: TWinControl;
  protected
    procedure Paint; override;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property FocusControl:TWinControl read FFocusControl write FFocusControl;
  end;

  TCalculatorButton = class(TSpeedButton)
  private
    FCaption: string;
    procedure SetCaption(const Value: string);

  protected
    procedure Paint; override;

  published
    property Caption: string read FCaption write SetCaption;
  end;



  TCalculatorLook = class(TPersistent)
  private
    FButtonBkg: TBitmap;
    FButtonWidth: integer;
    FButtonHeight: integer;
    FButtonColor: TColor;
    FColor: TColor;
    FFont: TFont;
    FFlat: boolean;
    procedure SetFont(const Value: TFont);
    procedure SetButtonBkg(const Value: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ButtonWidth:integer read FButtonWidth write FButtonWidth;
    property ButtonHeight:integer read FButtonHeight write FButtonHeight;
    property ButtonColor:TColor read FButtonColor write FButtonColor;
    property Color:TColor read FColor write FColor;
    property Flat:boolean read FFlat write FFlat;
    property Font:TFont read FFont write SetFont;
    property ButtonBkg:TBitmap read FButtonBkg write SetButtonBkg;
  end;

{ TExtraButtons }

  TExtraButtons = class(TPersistent)
  private
    fExtra1: string;
    fExtra4: string;
    fExtra2: string;
    fExtra3: string;
  published
    property Extra1:string read fExtra1 write fExtra1;
    property Extra2:string read fExtra2 write fExtra2;
    property Extra3:string read fExtra3 write fExtra3;
    property Extra4:string read fExtra4 write fExtra4;
  end;

  TExtraButtonClickEvent = procedure(Sender:TObject;idx:integer;var value:extended) of object;

{ TMoneyEdit }

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMoneyEdit = class(TCustomEdit)
  private
    FButton: TDropCalculatorButton;
    FEditorEnabled: Boolean;
    FOnClickBtn:TNotifyEvent;
    FCalcForm:TTabForm;
    FCalcClosed:boolean;
    FCloseClick:boolean;
    sp:array[0..22] of TCalculatorButton;
    newval:boolean;
    prevval:extended;
    prevop:integer;
    FCalculatorLook: TCalculatorLook;
    FExtraButtons: TExtraButtons;
    FOnExtraButtonClick: TExtraButtonClickEvent;
    function GetMinHeight: Integer;
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure FormDeactivate(Sender: TObject);
    procedure NumButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowCalculator;
    procedure HideCalculator;
    procedure BuildCalculator(AForm: TForm);
    function GetButtonGlyph: TBitmap;
    procedure SetButtonGlyph(const Value: TBitmap);
    procedure docalc;
    procedure doplus;
    procedure domin;
    procedure domul;
    procedure dodiv;
    procedure doeq;
    procedure doperc;
    procedure SetEditorEnabled(const Value: Boolean);
    procedure SetCalculatorLook(const Value: TCalculatorLook);
    function GetValue: extended;
    procedure SetValue(const Value: extended);
    procedure BtnClick (Sender: TObject); virtual;
    procedure SetExtraButtons(const Value: TExtraButtons);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetParentForm(Control: TControl): TCustomForm; virtual;
    procedure KeyPress(var key:char); override;
    procedure CalcChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TDropCalculatorButton read FButton;
    property Text;
    property Value:extended read GetValue write SetValue;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CalculatorLook:TCalculatorLook read FCalculatorLook write SetCalculatorLook;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default True;
    property Enabled;
    property ExtraButtons: TExtraButtons read FExtraButtons write SetExtraButtons;
    property Font;
    property ButtonGlyph: TBitmap read GetButtonGlyph write SetButtonGlyph;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Height;
    property Version: string read GetVersion write SetVersion;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
    property OnClickBtn: TNotifyEvent read FOnClickBtn write FOnClickBtn;
    property OnExtraButtonClick: TExtraButtonClickEvent read fOnExtraButtonClick
                  write fOnExtraButtonClick;
  end;

implementation

{$I DELPHIXE.INC}

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;


{ TDropCalculatorButton }
constructor TDropCalculatorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor := crArrow;
end;

procedure TDropCalculatorButton.Paint;
begin

  inherited Paint;
end;

procedure TDropCalculatorButton.Click;
begin
  if (FFocusControl <> nil) and FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
     FFocusControl.SetFocus;
  inherited Click;
end;

{ TMoneyEdit }
constructor TMoneyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TDropCalculatorButton.Create (Self);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnClick := BtnClick;
  FButton.Margin:=0;
  FButton.Spacing:=0;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FCalcClosed:=false;
  Enabled:=true;
  FCalculatorLook:=TCalculatorLook.Create;
  FExtraButtons:=TExtraButtons.Create;
end;

destructor TMoneyEdit.Destroy;
begin
  FButton.Free;
  FCalculatorLook.Free;
  FExtraButtons.Free;
  inherited Destroy;
end;

function TMoneyEdit.GetParentForm(Control: TControl): TCustomForm;
begin
  Result := nil;
  if Assigned(Control) then
    if Control is TCustomForm then
    begin
      Result := Control as TCustomForm;
      Exit;
    end else
    begin
      if Assigned(Control.Parent) then
        Result := GetParentForm(Control.Parent);
    end;
end;

procedure TMoneyEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or ES_RIGHT;
end;

procedure TMoneyEdit.DestroyWnd;
begin
  inherited;
end;

procedure TMoneyEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TMoneyEdit.BuildCalculator(AForm:TForm);
var
 i,n:integer;
 FButtonWidth:integer;
 FButtonHeight:integer;
 ex:array[1..4] of string;

begin
  FButtonWidth := FCalculatorLook.ButtonWidth;
  FButtonHeight := FCalculatorLook.ButtonHeight;

  n := 5;
  with FExtraButtons do
   begin
    if (fExtra1<>'') or (fExtra2<>'') or (fExtra3<>'') or (fExtra4<>'') then n:=6;

    AForm.Width:=4+n*(FButtonWidth+2);
    AForm.Height:=4+4*(FButtonHeight+2);

    ex[1]:=fExtra1;
    ex[2]:=fExtra2;
    ex[3]:=fExtra3;
    ex[4]:=fExtra4;

    for i:=1 to 4 do
    if ex[i]<>'' then
     begin
      sp[18+i]:=TCalculatorButton.Create(AForm);
      if not FCalculatorLook.ButtonBkg.Empty then
        begin
         sp[18+i].Glyph:=FCalculatorLook.ButtonBkg;
         sp[18+i].Spacing:=-(FCalculatorLook.ButtonBkg.Width shr 1)-(FCalculatorLook.Font.Size shr 1);
         sp[18+i].Margin:=0;
        end;

      sp[18+i].Font.Assign(FCalculatorLook.Font);

      sp[18+i].left:=2+5*(FButtonWidth+2);
      sp[18+i].top:=2+(i-1)*(FButtonHeight+2);
      sp[18+i].width:=FButtonWidth;
      sp[18+i].height:=FButtonHeight;

      sp[18+i].tag:=18+i;
      sp[18+i].flat:=FCalculatorLook.Flat;
      sp[18+i].caption:=ex[i]+' ';
      sp[18+i].OnClick := NumButtonClick;
      sp[18+i].Parent:=AForm;
      sp[18+i].Visible:=true;

     end;
   end;

  for i:=0 to 18 do
  begin
    sp[i] := TCalculatorButton.Create(AForm);

    sp[i].Font.Assign(FCalculatorLook.Font);

    if not FCalculatorLook.ButtonBkg.Empty then
    begin
      sp[i].Glyph := FCalculatorLook.ButtonBkg;
      sp[i].Spacing := -(FCalculatorLook.ButtonBkg.Width shr 1) - (FCalculatorLook.Font.Size shr 1);
      sp[i].Margin := 0;
    end;

    case i of
    0,1,4,7:sp[i].left:=2;
    2,5,8,14:sp[i].left:=2+(FButtonWidth+2);
    3,6,9,15:sp[i].left:=2+2*(FButtonWidth+2);
    10,11,12,13:sp[i].left:=2+3*(FButtonWidth+2);
    16,17,18:sp[i].left:=2+4*(FButtonWidth+2);
    end;

    case i of
    7,8,9,10:sp[i].top:=2;
    4,5,6,11,18:sp[i].top:=2+(FButtonHeight+2);
    1,2,3,12,16:sp[i].top:=2+2*(FButtonHeight+2);
    0,13,14,15,17:sp[i].top:=2+3*(FButtonHeight+2);
    end;

    sp[i].width:=FButtonWidth;
    sp[i].height:=FButtonHeight;

    sp[i].tag:=i;
    sp[i].flat:=FCalculatorLook.Flat;

    case i of
    0..9:sp[i].caption:=inttostr(i)+' ';
    10:sp[i].caption:='+ ';
    11:sp[i].caption:='- ';
    12:sp[i].caption:='* ';
    13:sp[i].caption:='/ ';
    14:sp[i].caption:='+/- ';
    15:sp[i].caption:='. ';
    16:sp[i].caption:='C ';
    17:sp[i].caption:='= ';
    18:sp[i].caption:='% ';
    end;

    sp[i].OnClick := NumButtonClick;
    sp[i].Parent:=AForm;
    sp[i].Visible:=true;
   end;
end;

procedure TMoneyEdit.ShowCalculator;
var
  P: TPoint;
  fDropDirection:boolean;
  {$IFDEF DELPHI9_LVL}
  w,h : integer;
  {$ENDIF}
begin
  fCalcClosed:=false;

  P := Point(0, 0);
  P := Self.ClientToScreen(P);

  FCalcForm:=TTabForm.CreateNew(self,0);

  FCalcForm.BorderStyle:=bsNone;
  FCalcForm.Visible := False;
  FCalcForm.ButtonWidth := FCalculatorLook.ButtonWidth;
  FCalcForm.ButtonHeight := FCalculatorLook.ButtonHeight;

  if not FCalculatorLook.ButtonBkg.Empty then
   FCalcForm.ButtonColor := FCalculatorLook.Color
  else
   FCalcForm.ButtonColor := FCalculatorLook.ButtonColor;

  FCalcForm.FormStyle := fsStayOnTop;

  FCalcForm.Ex[1]:=FExtraButtons.Extra1<>'';
  FCalcForm.Ex[2]:=FExtraButtons.Extra2<>'';
  FCalcForm.Ex[3]:=FExtraButtons.Extra3<>'';
  FCalcForm.Ex[4]:=FExtraButtons.Extra4<>'';

  FCalcForm.Color:=FCalculatorLook.Color;

  FCalcForm.OnDeactivate := FormDeactivate;
  FCalcForm.OnKeypress:= FormKeyPress;
  FCalcForm.OnKeyDown:= FormKeyDown;

  P := Point(0, 0);
  P := ClientToScreen(P);
  FCalcForm.Left:=P.x;

  BuildCalculator(FCalcForm);

  FDropDirection:=false;

  if P.y + FCalcForm.Height >= GetSystemMetrics(SM_CYSCREEN) then
    fDropDirection := True;

  if P.y - FCalcForm.Height <= 0 then
    fDropDirection := False;

  if (FDropDirection=false) then
    FCalcForm.Top:=P.y + self.Height
  else
    FCalcForm.Top:=P.y - FCalcForm.Height;

  {$IFNDEF DELPHI9_LVL}
  FCalcForm.Show;
  {$ENDIF}
  {$IFDEF DELPHI9_LVL}
  w := FCalcForm.Width;
  h := FCalcForm.Height;

  FCalcForm.Width := 0;
  FCalcForm.height := 0;
  FCalcForm.Show;

  FCalcForm.Left:=P.x;
  FDropDirection:=false;

  if P.y + FCalcForm.Height >= GetSystemMetrics(SM_CYSCREEN) then
    fDropDirection := True;

  if P.y - FCalcForm.Height <= 0 then
    fDropDirection := False;

  if (FDropDirection=false) then
    FCalcForm.Top:=P.y + self.Height
  else
    FCalcForm.Top:=P.y - FCalcForm.Height;

  FCalcForm.width := w;
  FCalcForm.height := h;
  {$ENDIF}
end;

procedure TMoneyEdit.HideCalculator;
begin
 fCalcForm.Free;
 fCalcform:=nil;
end;

procedure TMoneyEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 3;
  if self.BorderStyle = bsNone then
   begin
    Loc.Top := 2;
    Loc.Left := 2;
    Loc.Right := Loc.Right - 2;
   end
  else
   begin
    Loc.Top := 1;
    Loc.Left := 1;
   end;
  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));  {debug}
end;

procedure TMoneyEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
  Dist:integer;
begin
  inherited;
  if BorderStyle=bsNone then Dist:=2 else Dist:=5;

  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }

  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - Dist, 0, FButton.Width, Height - Dist)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TMoneyEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  {Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 +2;}
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

procedure TMoneyEdit.BtnClick (Sender: TObject);
begin
 if not fCalcClosed then
   begin
    if not fCloseClick then ShowCalculator
   end
   else HideCalculator;
 fCloseClick:=false;
 if assigned(FOnClickBtn) then FOnClickBtn(Sender);
end;

procedure TMoneyEdit.WMPaste(var Message: TWMPaste);
begin
  if fEditorEnabled then inherited;
end;

procedure TMoneyEdit.WMCut(var Message: TWMPaste);
begin
  if fEditorEnabled then inherited;
end;

procedure TMoneyEdit.CMExit(var Message: TCMExit);
begin
  inherited;
end;

procedure TMoneyEdit.WMKeyDown(var Msg:TWMKeydown);
begin
 inherited;
 if (msg.CharCode=vk_F4) then
  begin
    ShowCalculator;
  end;

 if (msg.CharCode=vk_delete) then if Text='' then Text:='0';
end;

procedure TMoneyEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;


function TMoneyEdit.GetButtonGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TMoneyEdit.SetButtonGlyph(const Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

procedure TMoneyEdit.FormDeactivate(Sender: TObject);
var
  pt:TPoint;
  r:trect;
begin
  {check cursor here...}
  getcursorpos(pt);
  pt:=screentoclient(pt);
  r:=clientrect;
  r.left:=r.right-16;
  FCloseClick:=ptinrect(r,pt);

  PostMessage((Sender as TForm).Handle,WM_CLOSE,0,0);
end;

procedure TMoneyEdit.NumButtonClick(Sender: TObject);
var
  s:string;
  e,n:extended;
begin
  CalcChange;

  if ((sender as TCalculatorButton).tag<10) then
  begin
    if (Text='0') or (newval) then
    begin
      Text:=inttostr((sender as TCalculatorButton).tag);
      newval:=false;
    end
    else
      Text := Text + Inttostr((sender as TCalculatorButton).tag);
  end
  else
  begin
    case ((sender as TCalculatorButton).Tag) of
    10:begin doplus; prevop:=0; end;
    11:begin domin; prevop:=2; end;
    12:begin domul; prevop:=1; end;
    13:begin dodiv; prevop:=3; end;
    14:if pos('-',Text)=0 then
         Text:='-'+Text
       else
       begin
         s := Text;
         delete(s,1,1);
         Text := s;
        end;
   15:begin
        if (Text = '0') or (newval) then
        begin
          Text := '0' + DecimalSeparator;
          newval := false;
        end
        else
          if pos(DecimalSeparator,Text) = 0 then
            Text := Text + DecimalSeparator;

      end;  
   16:begin
        Text := '0';
        prevval := 0;
        prevop:=-1;
      end;
   17:doeq;
   18:begin doperc; end;
   19..22:begin
           if Text='' then Text:='0';
           e:=strtofloat(Text);
           n:=e;
           if Assigned(FOnExtraButtonClick) then
                      FOnExtraButtonClick(sender,(sender as TCalculatorButton).tag-18,e);
           if (e<>n) then Text:=Format('%g',[e]);
          end;
    end;
  end;
end;

procedure TMoneyEdit.FormKeyPress(Sender: TObject; var Key: Char);
begin
 CalcChange;
 if Key = DecimalSeparator then
   if (pos(DecimalSeparator,Text)=0) then Text:=Text+DecimalSeparator;

 case key of
 '0'..'9':if (Text='0') or (newval) then
            begin
             Text:=key;
             newval:=false;
            end else Text:=Text+key;
 'c','C':begin
          Text:='0';
          prevval:=0;
          newval:=true;
          prevop:=-1;
         end;
 '+':doplus;
 '/':dodiv;
 '-':domin;
 '*':domul;
 '=':doeq;
 '%':doperc;
 #13:begin
      doeq;
      postmessage((Sender as TForm).Handle,WM_CLOSE,0,0);
     end;
 #27:postmessage((Sender as TForm).Handle,WM_CLOSE,0,0);
 end;

end;

procedure TMoneyEdit.docalc;
var
 e:extended;
begin
 if Text='' then Text:='0';
 e:=strtofloat(Text);
 try
   case prevop of
   0:prevval:=prevval+e;
   1:prevval:=prevval*e;
   2:prevval:=prevval-e;
   3:if (e<>0) then prevval:=prevval/e else prevval:=0;
   else
    prevval:=strtofloat(Text);
   end;

 except
   prevval:=0;
 end;

 Text:=format('%g',[prevval]);
 newval:=true;
end;

procedure TMoneyEdit.dodiv;
begin
 docalc;
 prevop:=3;
end;

procedure TMoneyEdit.doeq;
begin
 docalc;
 if Text='' then Text:='0';
 prevval:=strtofloat(Text);
 prevop:=-1;
end;

procedure TMoneyEdit.doperc;
var
 e:extended;
begin
 if Text='' then Text:='0';
 e:=strtofloat(Text);

 e:=prevval*e/100;
 Text:=format('%g',[e]);
end;


procedure TMoneyEdit.domin;
begin
 docalc;
 prevop:=2;
end;

procedure TMoneyEdit.domul;
begin
 docalc;
 prevop:=1;
end;

procedure TMoneyEdit.doplus;
begin
 docalc;
 prevop := 0;
end;


procedure TMoneyEdit.SetEditorEnabled(const Value: Boolean);
begin
  FEditorEnabled := Value;
  ReadOnly := not fEditorEnabled;
end;

procedure TMoneyEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 s:string;
begin
  if key in [vk_F4,vk_tab] then postmessage((Sender as TForm).Handle,WM_CLOSE,0,0);

  if (key=vk_back) then
  begin
    s:=Text;
    delete(s,length(Text),1);
    if s='' then s:='0';
    Text:=s;
  end;
end;

procedure TMoneyEdit.KeyPress(var Key: Char);
begin
  case key of
  'c','C':begin
            Text:='0';
            newval:=true;
            prevop:=-1;
           end;
  '+':doplus;
  '/':dodiv;
  '-':domin;
  '*':domul;
  '=',#13:doeq;
  '%':doperc;
  end;

  if not (IsNumChar(key) or (key = DecimalSeparator) or (key = #8)) then
    key := #0;

  if ((Text = '0') or (newval)) and (IsNumChar(key)) and not ReadOnly then
  begin
    Text := Key;
    Key := #0;
    SelStart := 1;
    SelLength := 0;
    Newval := False;
    Exit;
  end;

  if (Length(Text) = 1) and (Key = #8) then
  begin
    Text := '0';
    Key := #0;
    SelStart := 1;
    SelLength := 0;
    Exit;
  end;

  if (Key = ThousandSeparator) then
  begin
    Key := #0;
  end;

  if (Key = DecimalSeparator) and (Pos(Key,Text)>0) then
  begin
    Key := #0;
  end;

  inherited;
end;

procedure TMoneyEdit.SetCalculatorLook(const Value: TCalculatorLook);
begin
  FCalculatorLook.Assign(Value);
end;

function TMoneyEdit.GetValue: extended;
var
  s:string;
begin
  s := Text;
  if s = '' then s := '0';
  try
    Result := StrToFloat(s);
  except
    Result := 0;
  end;
end;

procedure TMoneyEdit.SetValue(const Value: extended);
begin
 Text:=Format('%g',[value]);
end;

procedure TMoneyEdit.SetExtraButtons(const Value: TExtraButtons);
begin
  fExtraButtons.Assign(Value);
end;

procedure TMoneyEdit.CalcChange;
begin

end;

function TMoneyEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TMoneyEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TMoneyEdit.SetVersion(const Value: string);
begin

end;

{ TTabForm }

procedure TTabForm.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if msg.CharCode in [vk_up,vk_down,vk_left,vk_right,vk_tab] then msg.result:=1;
end;

constructor TTabForm.Create(aOwner: TComponent);
begin
  inherited;
end;

destructor TTabForm.Destroy;
begin
  inherited;
end;

procedure TTabForm.SetEx(i:integer;b:boolean);
begin
  if (i < 5) and (i > 0) then
    FEx[i] := b;
end;

function TTabForm.GetEx(i:integer):boolean;
begin
  Result := False;
  if (i < 5) and (i > 0) then
    Result := FEx[i];
end;


procedure TTabForm.Paint;
var
  i,j: Integer;
  r: TRect;
  oldColor: TColor;

begin
  inherited;

  with Canvas do
  begin
    Pen.Color := clBlack;
    r := GetClientRect;
    Rectangle(r.left,r.top,r.right,r.bottom);

    oldColor := Canvas.Brush.Color;
    Canvas.Brush.Color := ButtonColor;
    Canvas.Pen.color:=ButtonColor;

    for i := 1 to 6 do
      for j := 1 to 4 do
      begin
        if (i = 5) and (j < 2) then
          Continue;

        if (i = 6) and not FEx[j] then
          Continue;

        Rectangle(2 + (2 + FButtonWidth)*(i - 1),2 + (2 + FButtonHeight)*(j-1),
                 (2 + FButtonWidth)*(i),(2 + FButtonHeight)*(j));
      end;

    Canvas.Brush.Color:=oldColor;
  end;
end;

procedure TTabForm.WMClose(var Msg: TMessage);
begin
  inherited;
  self.Free;
end;

{ TCalculatorLook }

procedure TCalculatorLook.Assign(Source: TPersistent);
begin
 if Source is TCalculatorLook then
  begin
    ButtonBkg.Assign(TCalculatorLook(Source).ButtonBkg);
    ButtonWidth:=TCalculatorLook(Source).ButtonWidth;
    ButtonHeight:=TCalculatorLook(Source).ButtonHeight;
    ButtonColor:=TCalculatorLook(Source).ButtonColor;
    Color:=TCalculatorLook(Source).Color;
    Font.Assign(TCalculatorLook(Source).Font);
    Flat:=TCalculatorLook(Source).Flat;
  end;
end;

constructor TCalculatorLook.Create;
begin
  inherited;
  FFont:=TFont.Create;
  FButtonWidth:=24;
  FButtonHeight:=24;
  FButtonColor:=clSilver;
  FColor:=clWhite;
  FbuttonBkg:=TBitmap.Create;
end;

destructor TCalculatorLook.Destroy;
begin
  FFont.Free;
  FbuttonBkg.Free;
  inherited;
end;

procedure TCalculatorLook.SetButtonBkg(const Value: TBitmap);
begin
  FButtonBkg.Assign(Value);
end;

procedure TCalculatorLook.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ TCalculatorButton }

procedure TCalculatorButton.Paint;
var
  R: TRect;
begin
  inherited;

  R := GetClientRect;

  OffsetRect(R, 4, 0);

  if FState = bsDown then
    OffsetRect(R, 2, 2);

  DrawText(Canvas.Handle, PChar(FCaption), Length(FCaption), R, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
end;

procedure TCalculatorButton.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

end.


