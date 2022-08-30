{***************************************************************************}
{ TAdvMemoDropDown components                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2015                                        }
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

{$I TMSDEFS.INC}

unit AdvMemoDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, AdvDropDown, Menus, StdCtrls,
  SysUtils, Forms
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMemoDropDown = class(TAdvCustomDropDown)
  private
    FOnMemoChange: TNotifyEvent;
    FMemoPopup: TPopupMenu;
    FMemoText: TStringList;
    FMemo: TMemo;
    FMemoScrollBars: TScrollStyle;
    FMemoReadOnly: Boolean;
    FInternalUpdate: Boolean;
    FAutoDrop: boolean;
    procedure SetMemoPopup(const Value: TPopupMenu);
    procedure SetMemoText(const Value: TStringList);
    procedure SetMemoScrollBars(const Value: TScrollStyle);
    procedure MemoChanged(Sender: TObject);
    procedure OnMemoTextChanged(Sender: TObject);
    procedure OnMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetTextEx: string;
    procedure SetMemoReadOnly(const Value: Boolean);
    function GetMemoText: TStringList;
  protected
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyPress(var Key: Char); override;
    procedure Change; override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: string read GetTextEx;
    property Memo: TMemo read FMemo;
  published
    property AutoDrop: boolean read FAutoDrop write FAutoDrop default false;
    property MemoText: TStringList read GetMemoText write SetMemoText;
    property MemoPopup: TPopupMenu read FMemoPopup write SetMemoPopup;
    property MemoScrollBars: TScrollStyle read FMemoScrollBars write SetMemoScrollBars;
    property MemoReadOnly: Boolean read FMemoReadOnly write SetMemoReadOnly default False;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property BorderColor;
    property DisabledBorder;
    property FocusBorderColor;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownSizeable;
    property Enabled;
    property EditorEnabled;
    property Font;
    property DropDownButtonGlyph;
    property Images;
    property Version;
    property ReadOnly;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;
    
    property OnMemoChange: TNotifyEvent read FOnMemoChange write FOnMemoChange; //exposed OnChange event of the memo.
    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;
  end;

implementation

//------------------------------------------------------------------------------

{ TAdvMemoDropDown }

procedure TAdvMemoDropDown.Change;
begin
  if not FInternalUpdate then
  begin
    FMemoText.OnChange := nil;
    MemoText.Text := Text;
    FMemoText.Text := Text;
    FMemoText.OnChange := OnMemoTextChanged;
  end;

  inherited;
end;

constructor TAdvMemoDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FMemoText := TStringList.Create;
  FMemoText.OnChange := OnMemoTextChanged;
  FMemoScrollBars := ssNone;
  FMemoReadOnly := False;
  FInternalUpdate := False;
  FAutoDrop := False;
end;

//------------------------------------------------------------------------------

destructor TAdvMemoDropDown.Destroy;
begin
  FMemoText.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.CreateDropDownForm;
begin
  inherited;

  if not Assigned(FMemo) then
  begin
    FMemo := TMemo.Create(Self);
    FMemo.Parent := FDropDownForm;
    FMemo.Height := 100;
    FMemo.OnChange := MemoChanged;
    FMemo.OnKeyDown := OnMemoKeyDown;
    FMemo.BorderStyle := bsNone;
    FMemo.WantTabs := true;
    FMemo.TabStop := true;
  end;
  Control := FMemo;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.SetMemoPopup(const Value: TPopupMenu);
begin
  FMemoPopup := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.SetMemoReadOnly(const Value: Boolean);
begin
  FMemoReadOnly := Value;
  if DroppedDown and Assigned(FMemo) then
    FMemo.ReadOnly := FMemoReadOnly;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.SetMemoText(const Value: TStringList);
begin
  FMemoText.Assign(Value);
  if Assigned(FMemo) then
    FMemo.Lines.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.BeforeDropDown;
begin
  inherited;
  if Assigned(FDropDownForm) then
    FDropDownForm.CancelOnDeActivate := False;

  if Assigned(FMemo) then
  begin
    if EditorEnabled then
      FMemo.Lines.Text := FMemoText.Text;
    FMemo.PopupMenu := FMemoPopup;
    FMemo.MaxLength := MaxLength;
    FMemo.ScrollBars := MemoScrollBars;
    FMemo.ReadOnly := MemoReadOnly;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.SetMemoScrollBars(const Value: TScrollStyle);
begin
  FMemoScrollBars := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.MemoChanged(Sender: TObject);
begin
  FMemoText.Assign(FMemo.Lines);
  if Assigned(FOnMemoChange) then
    FOnMemoChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.OnHideDropDown;
begin
  inherited;

  FMemoText.Text := FMemo.Lines.Text;

  OnMemoTextChanged(Self);
  SelStart := 0;
  SelLength := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.DoEnter;
begin
  inherited;
  if AutoDrop then
    DoShowDropDown;
end;

procedure TAdvMemoDropDown.DoHideDropDown(Canceled: Boolean);
var
  oldDrop: boolean;
begin
  oldDrop := AutoDrop;
  AutoDrop := false;

  if Canceled then  // restoring old text
  begin
    FMemo.Lines.Text := FMemoText.Text;
  end;

  inherited;

  AutoDrop := oldDrop;
end;

//------------------------------------------------------------------------------

function TAdvMemoDropDown.GetMemoText: TStringList;
begin
  if Assigned(FMemo) then
    Result := TStringList(FMemo.Lines)
  else
    Result := FMemoText;
end;

function TAdvMemoDropDown.GetTextEx: string;
begin
  Result := inherited Text;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.OnDropDownControlKeyDown(var Key: Word;
  Shift: TShiftState);
var
  IsAlt: Boolean;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;

  if (Key = VK_TAB) then
  begin
    Key := 0;
    HideDropDown(false);
    SelectAll;
    Exit;
  end;

  if (Key in [VK_ESCAPE, VK_F4]) or IsAlt then
    inherited;

  if ((Key = ord('A')) or (key = ord('a'))) and (ssCtrl in Shift) then
  begin
    FMemo.SelectAll;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.OnDropDownControlKeyPress(var Key: Char);
var
  IsAlt: Boolean;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;

  if Key = #9 then
  begin
    Key := #0;
    SelectAll;
  end;

  if (Integer(Key) in [VK_ESCAPE{, VK_F4}]) or IsAlt then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.OnDropDownControlKeyUp(var Key: Word;
  Shift: TShiftState);
var
  IsAlt: Boolean;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  if (Key in [VK_ESCAPE, VK_F4]) or IsAlt then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.OnMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  IsAlt: Boolean;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  case Key of
    VK_ESCAPE: DoHideDropDown(True);
    VK_UP, VK_DOWN:
    begin
      if IsAlt then
      begin
        if DroppedDown then
          DoHideDropDown(True)
        else
          DoShowDropDown;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMemoDropDown.OnMemoTextChanged(Sender: TObject);
var
  s: string;
  ER: TRect;
  ts: TSize;
  DC: HDC;
  Canvas: TCanvas;
begin
  s := '';
  if (FMemoText.Count > 0) then
  begin
    if not EditorEnabled then
    begin
      s := FMemoText[0];
      ER := GetEditRect;
      DC := GetWindowDC(Handle);
      try
        Canvas := TCanvas.Create;
        Canvas.Handle := DC;
        Canvas.Font.Assign(Self.Font);
        ts := GetTextSize(Canvas, s);
        if (ts.cx >= (ER.Right - ER.Left)) then
          s := GetTextOfSize(Canvas, s, (ER.Right - ER.Left))
        else if (FMemoText.Count > 1) then
          s := s + '...';

        Canvas.Free;
      finally
        ReleaseDC(Handle,DC);
      end;
    end
    else
      s := FMemoText[0];
  end;

  FInternalUpdate := true;
  SetTextDirect(s);
  Change;
  FInternalUpdate := false;
end;

//------------------------------------------------------------------------------

end.
