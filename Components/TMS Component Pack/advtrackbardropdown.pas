{***************************************************************************}
{ TAdvTrackBarDropDown components                                           }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009                                               }
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

unit AdvTrackBarDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, AdvDropDown, ComCtrls, SysUtils;

type

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTrackBarDropDown = class(TAdvCustomDropDown)
  private
    FTrackBar: TTrackBar;
    FMin: Integer;
    FFrequency: Integer;
    FPageSize: Integer;
    FMax: Integer;
    FPosition: Integer;
    FMouseDraging: Boolean;
    FMouseX: Integer;
    FOnTrackChange: TNotifyEvent;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure SetFrequency(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure OnTrackBarChange(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure SetCenterControl; override;
    procedure SetText(Value: string); override;
    procedure Change; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
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
    property DropDownButtonGlyph;
    property DropDownSizeable;
    property Images;
    property Version;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DropDownEnabled;
    property EditorEnabled default False;
    property Enabled;
    property Font;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property ReadOnly;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property Position: Integer read FPosition write SetPosition default 0;
    property PageSize: Integer read FPageSize write SetPageSize default 2;
    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;
    
    property OnTrackChange: TNotifyEvent read FOnTrackChange write FOnTrackChange;
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
  end;

implementation

//------------------------------------------------------------------------------

{ TAdvTrackBarDropDown }

constructor TAdvTrackBarDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FMin := 0;
  FMax := 10;
  FPosition := 0;
  FPageSize := 2;
  FFrequency := 1;
  EditorEnabled := False;
  DropDownEnabled := True;
  EditType := etNumeric;
  DropDownHeader.Visible := False;
  DropDownFooter.Visible := False;
  ForceShadow := True;
end;

//------------------------------------------------------------------------------

destructor TAdvTrackBarDropDown.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.BeforeDropDown;
begin
  inherited;

  if Assigned(FDropDownForm) then
    FDropDownForm.CancelOnDeActivate := False;

  if Assigned(FTrackBar) then
  begin
    FTrackBar.Min := FMin;
    FTrackBar.Max := FMax;
    FTrackBar.Position := FPosition;
    FTrackBar.PageSize := FPageSize;
    FTrackBar.Frequency := FFrequency;
    FTrackBar.Width := Width;
  end;
  Control := FTrackBar;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.CreateDropDownForm;
begin
  inherited;
  if not Assigned(FTrackBar) then
  begin
    FTrackBar := TTrackBar.Create(Self);
    FTrackBar.Parent := FDropDownForm;
    FTrackBar.Left := 0;
    FTrackBar.Top := 0;
    FTrackBar.Height := 40;
  end;
  FTrackBar.OnChange := OnTrackBarChange;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetCenterControl;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.DoHideDropDown(Canceled: Boolean);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.OnHideDropDown;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetSelectionColorStyle(
  const Value: TSelectionColorStyle);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.UpdateDropDownSize;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.WMKeyDown(var Msg: TWMKeyDown);
var
  IsAlt: Boolean;
begin
  inherited;
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  if not EditorEnabled and not IsAlt then
  begin
    case Msg.CharCode of
      VK_UP:
      begin
        if (Position - Frequency >= Min) then
          Position := Position - Frequency;
      end;
      VK_DOWN:
      begin
        if (Position + Frequency <= Max) then
          Position := Position + Frequency;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetFrequency(const Value: Integer);
begin
  FFrequency := Value;
  if Assigned(FTrackBar) then
    FTrackBar.Frequency := FFrequency;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetMax(const Value: Integer);
begin
  FMax := Value;
  if Assigned(FTrackBar) then
  begin
    FTrackBar.Max := FMax;
    FMax := FTrackBar.Max;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetMin(const Value: Integer);
begin
  FMin := Value;
  if Assigned(FTrackBar) then
  begin
    FTrackBar.Min := FMin;
    FMin := FTrackBar.Min;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetPageSize(const Value: Integer);
begin
  FPageSize := Value;
  if Assigned(FTrackBar) then
    FTrackBar.PageSize := FPageSize;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if DropDownEnabled and not EditorEnabled then
  begin
    FMouseX := X;
    FMouseDraging := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  i: Integer;  
begin
  inherited;

  if DropDownEnabled and not EditorEnabled and FMouseDraging then
  begin
    i := (X - FMouseX) div PageSize;
    FMouseX := X;
    Position := Position + i;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FMouseDraging := False;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetText(Value: string);
begin
  inherited SetText(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  if Assigned(FTrackBar) then
  begin
    FTrackBar.Position := FPosition;
    FPosition := FTrackBar.Position;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.Change;
begin
  inherited;
  if (Text <> '') then
    Position := StrToInt(Text)
  else
    Position := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvTrackBarDropDown.OnTrackBarChange(Sender: TObject);
begin
  if Assigned(FTrackBar) then
  begin
    FPosition := FTrackBar.Position;
    SetTextDirect(InttoStr(FTrackBar.Position));

    if Assigned(OnTrackChange) then
      OnTrackChange(Self);
  end;
end;

//------------------------------------------------------------------------------


end.
