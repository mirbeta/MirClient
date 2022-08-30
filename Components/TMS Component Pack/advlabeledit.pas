{***************************************************************************}
{ TAdvLabelEdit component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013 - 2015                                        }
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

unit AdvLabelEdit;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Controls, StdCtrls, AdvEdit, Buttons, Dialogs, SysUtils,
  StrUtils, PNGImage, Messages, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with OnChange event
  // v1.1.0.0 : Fixed : Issue with OnEditStop event
  //          : New : Exposed EditAlign,EditType,EditMaxLength,EditPrecision,EditSuffix,EditPrefix,EditSigned properties
  // v1.1.1.0 : New : Event OnEditCancel added


type
  TPNGSpeedButton = class(TSpeedButton)
  private
    FPNGName: string;
    procedure SetPNGName(const Value: string);
  protected
    procedure Paint; override;
  published
    property PNGName: string read FPNGName write SetPNGName;
  end;

  TAdvLabelEditButtons = class(TPersistent)
  private
    FHintOK: string;
    FHintCancel: string;
    FHintEdit: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property HintOK: string read FHintOK write FHintOK;
    property HintCancel: string read FHintCancel write FHintCancel;
    property HintEdit: string read FHintEdit write FHintEdit;
  end;

  TEditEvent = procedure(Sender: TObject; var Value: string) of object;

  TEditCancelEvent = procedure(Sender: TObject; Value: string) of object;

  TAdvLabelEdit = class(TCustomControl)
  private
    FLabel: TLabel;
    FEditBtn: TPNGSpeedButton;
    FCancelBtn: TPNGSpeedButton;
    FOKBtn: TPNGSpeedButton;
    FEdit: TAdvEdit;
    FEditMode: boolean;
    FButtons: TAdvLabelEditButtons;
    FHoverColor: TColor;
    FOnEditStop: TEditEvent;
    FOnEditStart: TEditEvent;
    FEllipsisPosition: TEllipsisPosition;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnDblCLick: TNotifyEvent;
    FEmptyText: string;
    FEditType: TAdvEditType;
    FEditAlign: TEditAlign;
    FEditPrefix: string;
    FEditSuffix: string;
    FEditPrecision: integer;
    FEditMaxLength: integer;
    FEditSigned: boolean;
    FOnEditCancel: TEditCancelEvent;
    procedure SetText(const Value: string);
    procedure SetEditMode(const Value: boolean);
    function GetText: string;
    procedure SetButtons(const Value: TAdvLabelEditButtons);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetEllipsisPosition(const Value: TEllipsisPosition);
    function GetVersion: string;
    procedure SetEmptyText(const Value: string);
    procedure SetEditType(const Value: TAdvEditType);
    procedure SetEditAlign(const Value: TEditAlign);
    procedure SetEditPrefix(const Value: string);
    procedure SetEditSuffix(const Value: string);
    procedure SetEditPrecision(const Value: integer);
    procedure SetEditMaxLength(const Value: integer);
    procedure SetEditSigned(const Value: boolean);
  protected
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoClick(Sender: TObject); virtual;
    procedure DoDblClick(Sender: TObject); virtual;
    procedure DoChange(Sender: TObject); virtual;
    procedure DoEditClick(Sender: TObject);
    procedure DoEditChange(Sender: TObject);
    procedure DoEditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoOKClick(Sender: TObject);
    procedure DoCancelClick(Sender: TObject);
    procedure DoLabelEnter(Sender: TObject);
    procedure DoLabelLeave(Sender: TObject);
    procedure DoLabelClick(Sender: TObject);
    procedure DoEditExit(Sender: TObject);
    procedure CreateWnd; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetName(const Value: TComponentName); override;
    procedure DoEditStart(var Value: string); virtual;
    procedure DoEditStop(var Value: string); virtual;
    procedure DoEditCancel(Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EditMode: boolean read FEditMode write SetEditMode;
    property Edit: TAdvEdit read FEdit;
    function GetVersionNr: integer;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Buttons: TAdvLabelEditButtons read FButtons write SetButtons;
    property Color;
    property Constraints;
    property EditAlign: TEditAlign read FEditAlign write SetEditAlign default eaLeft;
    property EditMaxLength: integer read FEditMaxLength write SetEditMaxLength default 0;
    property EditPrefix: string read FEditPrefix write SetEditPrefix;
    property EditSuffix: string read FEditSuffix write SetEditSuffix;
    property EditPrecision: integer read FEditPrecision write SetEditPrecision default 0;
    property EditSigned: boolean read FEditSigned write SetEditSigned default false;
    property EditType: TAdvEditType read FEditType write SetEditType default etString;
    property EllipsisPosition: TEllipsisPosition read FEllipsisPosition write SetEllipsisPosition default epNone;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property Enabled;
    property Font;
    property Hint;
    property HoverColor: TColor read FHoverColor write FHoverColor default clGray;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Text: string read GetText write SetText;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Version: string read GetVersion;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnContextPopup;
    property OnDblClick: TNotifyEvent read FOnDblCLick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnEditCancel: TEditCancelEvent read FOnEditCancel write FOnEditCancel;
    property OnEditStart: TEditEvent read FOnEditStart write FOnEditStart;
    property OnEditStop: TEditEvent read FOnEditStop write FOnEditStop;
  end;


implementation

{ TAdvLabelEdit }

procedure TAdvLabelEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FLabel.Color := Color;
end;

procedure TAdvLabelEdit.CMEnabledChanged(var Message: TMessage);
begin
  if Assigned(FLabel) then
    FLabel.Enabled := Enabled;
end;

procedure TAdvLabelEdit.CMFontChanged(var Message: TMessage);
begin
  FLabel.Font.Assign(Font);
  FEdit.Font.Assign(Font);
end;

constructor TAdvLabelEdit.Create(AOwner: TComponent);
begin
  inherited;
  Height := 21;
  Width := 200;
  FEditBtn := TPNGSpeedButton.Create(Self);
  FCancelBtn := TPNGSpeedButton.Create(Self);
  FOKBtn := TPNGSpeedButton.Create(Self);
  FEdit := TAdvEdit.Create(Self);
  FLabel := TLabel.Create(Self);
  FEditMode := False;
  FLabel.Caption := Name;
  FButtons := TAdvLabelEditButtons.Create;
  FHoverColor := clGray;
  FEllipsisPosition := epNone;
  FEditType := etString;
  FEditAlign := eaLeft;
  FEditPrecision := 0;
end;

procedure TAdvLabelEdit.CreateWnd;
begin
  inherited;
  FEditBtn.Parent := Self;
  FCancelBtn.Parent := Self;
  FOKBtn.Parent := Self;
  FEdit.Parent := Self;
  FLabel.Parent := Self;
  FEdit.Left := 0;
  FOKBtn.Left := Width - FOKBtn.Width;
  FCancelBtn.Left := Width - FOKBtn.Width - FCancelBtn.Width;
  FOKBtn.Align := alRight;
  FOKBtn.Flat := true;
  FOKBtn.PNGName := 'tms_gl_accept';
  FOKBtn.OnClick := DoOKClick;
  FOKBtn.Hint := Buttons.HintOK;
  FOKBtn.ShowHint := FOKBtn.Hint <> '';
  FCancelBtn.Align := alRight;
  FCancelBtn.Flat := true;
  FCancelBtn.OnClick := DoCancelClick;
  FCancelBtn.PNGName := 'tms_gl_cancel';
  FCancelBtn.Hint := Buttons.HintCancel;
  FCancelBtn.ShowHint := FCancelBtn.Hint <> '';
  FEditBtn.Left:= Width - FEditBtn.Width;
  FEditBtn.Align := alRight;
  FEditBtn.Flat := true;
  FEditBtn.OnClick := DoEditClick;
  FEditBtn.PNGName := 'tms_gl_edit';
  FEditBtn.Hint := Buttons.HintEdit;
  FEditBtn.ShowHint := FEditBtn.Hint <> '';

  FEdit.Align := alClient;
  FLabel.Align := alClient;
  FLabel.Transparent := false;
  FLabel.OnClick := DoLabelClick;
  FLabel.OnDblClick := DoDblClick;
  FLabel.OnMouseEnter := DoLabelEnter;
  FLabel.OnMouseLeave := DoLabelLeave;
  FLabel.AlignWithMargins := true;
  FEdit.ReturnIsTab := true;

  FEdit.OnExit := DoEditExit;
  FEdit.OnChange := DoEditChange;
  FEdit.OnDblClick := DoDblClick;
  FEdit.OnKeyDown := DoEditKeyDown;
  FEdit.OnKeyUp := DoEditKeyUp;
  FEdit.OnKeyPress := DoEditKeyPress;
  FEdit.EmptyTextFocused := true;

  if (csDesigning in ComponentState) then
  begin
    FOKBtn.Width := 0;
    FCancelBtn.Width := 0;
    FEdit.Align := alNone;
    FEdit.Width := 0;
  end;

  SetEditMode(EditMode);
end;

destructor TAdvLabelEdit.Destroy;
begin
  FButtons.Free;
  inherited;
end;

procedure TAdvLabelEdit.DoCancelClick(Sender: TObject);
var
  s: string;
begin
  s := FLabel.Caption;
  FEditMode := false;
  EditMode := false;
  FLabel.Caption := s;
  DoEditCancel(s);
end;

procedure TAdvLabelEdit.DoChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvLabelEdit.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TAdvLabelEdit.DoDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TAdvLabelEdit.DoEditCancel(Value: string);
begin
  if Assigned(OnEditCancel) then
    OnEditCancel(Self, Value);
end;

procedure TAdvLabelEdit.DoEditChange(Sender: TObject);
begin
  DoChange(Self);
end;

procedure TAdvLabelEdit.DoEditClick(Sender: TObject);
begin
  DoClick(Sender);
  EditMode := true;
end;

procedure TAdvLabelEdit.DoEditExit(Sender: TObject);
begin
  EditMode := false;
end;

procedure TAdvLabelEdit.DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvLabelEdit.DoEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeypress) then
    OnKeyPress(Self, Key);
end;

procedure TAdvLabelEdit.DoEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvLabelEdit.DoEditStart(var Value: string);
begin
  if Assigned(OnEditStart) then
    OnEditStart(Self, Value);
end;

procedure TAdvLabelEdit.DoEditStop(var Value: string);
begin
  if Assigned(OnEditStop) then
    OnEditStop(Self, Value);
end;

procedure TAdvLabelEdit.DoEnter;
begin
  inherited;

  Invalidate;
end;

procedure TAdvLabelEdit.DoExit;
begin
  inherited;

  Invalidate;
end;

procedure TAdvLabelEdit.DoLabelClick(Sender: TObject);
begin
  DoClick(Sender);
  if Enabled then
    EditMode := true;
end;

procedure TAdvLabelEdit.DoLabelEnter(Sender: TObject);
begin
  if FHoverColor <> clNone then
    FLabel.Color := FHoverColor;
end;

procedure TAdvLabelEdit.DoLabelLeave(Sender: TObject);
begin
  FLabel.Color := Color;
end;

procedure TAdvLabelEdit.DoOKClick(Sender: TObject);
begin
  EditMode := false;
end;

function TAdvLabelEdit.GetText: string;
begin
  if EditMode then
    Result := FEdit.Text
  else
    Result := FLabel.Caption;
end;

function TAdvLabelEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvLabelEdit.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvLabelEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F2) then
    EditMode := not EditMode;
end;

procedure TAdvLabelEdit.KeyPress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
    EditMode := true
  else
  begin
    if CharInSet(Key, ['a'..'z','A'..'Z','0'..'9']) then
    begin
      EditMode := true;
      FEdit.Text := Key;
      FEdit.SelStart := 1;
    end;
  end;
end;

procedure TAdvLabelEdit.Paint;
var
  r: TRect;
begin
  inherited;
  if GetFocus = Handle then
  begin
    r := Rect(0,0,FLabel.Width + 4,Height - 1);
    DrawFocusRect(Canvas.Handle,r)
  end;
end;

procedure TAdvLabelEdit.SetButtons(const Value: TAdvLabelEditButtons);
begin
  FButtons := Value;
end;

procedure TAdvLabelEdit.SetEditAlign(const Value: TEditAlign);
begin
  FEditAlign := Value;
  if Assigned(FEdit) then
    FEdit.EditAlign := Value;
end;

procedure TAdvLabelEdit.SetEditMaxLength(const Value: integer);
begin
  if Value >= 0 then
  begin
    FEditMaxLength := Value;
    if Assigned(FEdit) then
      FEdit.MaxLength := Value;
  end;
end;

procedure TAdvLabelEdit.SetEditMode(const Value: boolean);
var
  s: string;
  ev: boolean;
begin
  ev := FEditMode <> Value;
  FEditMode := Value;

  if FEditMode then
  begin
    FLabel.Visible := false;
    FEdit.Visible := true;
    FEditBtn.Visible := false;
    FCancelBtn.Visible := true;
    FOKBtn.Visible := true;
    s := FLabel.Caption;
    if ev then
      DoEditStart(s);
    FEdit.Text := s;
    FEdit.SetFocus;
  end
  else
  begin
    FLabel.Visible := true;
    FEdit.Visible := false;
    FEditBtn.Visible := true;
    FCancelBtn.Visible := false;
    FOKBtn.Visible := false;
    s := FEdit.Text;
    if ev then
      DoEditStop(s);
    FLabel.Caption := s;
  end;
end;

procedure TAdvLabelEdit.SetEditPrecision(const Value: integer);
begin
  if Value >= 0 then
  begin
    FEditPrecision := Value;
    if Assigned(FEdit) then
      FEdit.Precision := Value;
  end;
end;

procedure TAdvLabelEdit.SetEditPrefix(const Value: string);
begin
  FEditPrefix := Value;
  if Assigned(FEdit) then
    FEdit.Prefix := Value;
end;

procedure TAdvLabelEdit.SetEditSigned(const Value: boolean);
begin
  FEditSigned := Value;
  if Assigned(FEdit) then
    FEdit.Signed := Value;
end;

procedure TAdvLabelEdit.SetEditSuffix(const Value: string);
begin
  FEditSuffix := Value;
  if Assigned(FEdit) then
    FEdit.Suffix := Value;
end;

procedure TAdvLabelEdit.SetEditType(const Value: TAdvEditType);
begin
  FEditType := Value;
  if Assigned(FEdit) then
    FEdit.EditType := Value;
end;

procedure TAdvLabelEdit.SetEllipsisPosition(const Value: TEllipsisPosition);
begin
  if (FEllipsisPosition <> Value) then
  begin
    FEllipsisPosition := Value;
    if Assigned(FLabel) then
      FLabel.EllipsisPosition := Value;
  end;
end;

procedure TAdvLabelEdit.SetEmptyText(const Value: string);
begin
  FEmptyText := Value;
  if Assigned(FEdit) then
    FEdit.EmptyText := Value;
end;

procedure TAdvLabelEdit.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := {(csSetCaption in ControlStyle) and}
    not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Text := Value;
end;

procedure TAdvLabelEdit.SetText(const Value: string);
begin
  FLabel.Caption := Value;
  FEdit.Text := Value;
end;


{ TPNGSpeedButton }

procedure TPNGSpeedButton.Paint;
var
  png: TPngImage;
  t,l: integer;
begin
  inherited;

  if PNGName <> '' then
  begin
    png := TPngImage.Create;
    try
      png.LoadFromResourceName(Hinstance,PNGName);
      t := (Height - png.Height) div 2;
      l := (Width - png.Width) div 2;
      png.Draw(Canvas, Rect(l,t,l + png.Width,t + png.Height));
    finally
      png.Free;
    end;
  end;
end;

procedure TPNGSpeedButton.SetPNGName(const Value: string);
begin
  if (FPNGName <> Value) then
  begin
    FPNGName := Value;
    Invalidate;
  end;
end;

{ TAdvLabelEditButtons }

procedure TAdvLabelEditButtons.Assign(Source: TPersistent);
begin
  if (Source is TAdvLabelEditButtons) then
  begin
    FHintOK := (Source as TAdvLabelEditButtons).HintOK;
    FHintCancel := (Source as TAdvLabelEditButtons).HintCancel;
    FHintEdit := (Source as TAdvLabelEditButtons).HintEdit;
  end;
end;

constructor TAdvLabelEditButtons.Create;
begin
  inherited;
  FHintOK := 'Accept';
  FHintCancel := 'Cancel';
  FHintEdit := 'Edit';
end;

end.
