{**************************************************************************}
{ TAdvMultiButtonEdit component                                            }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2013 - 2015                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit advmultibuttonedit;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Controls, StdCtrls, AdvEdit, Buttons, ImgList, Dialogs,
  PngImage, Forms, Messages, Graphics;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with setting Enabled at runtime
  // v1.0.0.2 : Fixed : Issue with OnEnter event
  // v1.0.0.3 : Fixed : Issue with displaying disabled buttons at design-time

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

  TAdvMultiButtonEdit = class;

  TButtonPosition = (bpLeft, bpRight);

  TButtonStyle = (bsClear,bsFind,bsOK,bsTrash,bsAccept,bsDeny,bsClose,bsCopy,bsPrevious,bsNext,bsUndo,bsAdd,bsSub,bsCustom);

  TEditButton = class(TCollectionItem)
  private
    FButtonPosition: TButtonPosition;
    FButton: TPNGSpeedButton;
    FEnabled: boolean;
    FFlat: boolean;
    FStyle: TButtonStyle;
    FImageIndex: integer;
    FHint: string;
    procedure SetButtonPosition(const Value: TButtonPosition);
    procedure SetEnabled(const Value: boolean);
    procedure SetFlat(const Value: boolean);
    procedure SetImageIndex(const Value: integer);
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetHint(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Button: TPNGSpeedButton read FButton;
  published
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Flat: boolean read FFlat write SetFlat default false;
    property Hint: string read FHint write SetHint;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Position: TButtonPosition read FButtonPosition write SetButtonPosition default bpRight;
    property Style: TButtonStyle read FStyle write SetStyle default bsCustom;
  end;

  TEditButtons = class(TOwnedCollection)
  private
    FEdit: TAdvMultiButtonEdit;
    function GetItem(Index: Integer): TEditButton;
    procedure SetItem(Index: Integer; const Value: TEditButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TEditButton read GetItem write SetItem; default;
    function Add: TEditButton;
    function Insert(Index: Integer): TEditButton;
    function FindButton(AStyle: TButtonStyle): TEditButton;
  end;

  TButtonClickEvent = procedure(Sender: TObject; ButtonIndex: integer) of object;

  TAdvCustomMultiButtonEdit = class(TCustomControl)
  private
    FEdit: TAdvEdit;
    FEditButtons: TEditButtons;
    FImages: TCustomImageList;
    FOnClickOK: TNotifyEvent;
    FOnClickCustom: TButtonClickEvent;
    FOnClickClear: TNotifyEvent;
    FOnClickFind: TNotifyEvent;
    FOnClickDeny: TNotifyEvent;
    FOnClickClose: TNotifyEvent;
    FOnClickTrash: TNotifyEvent;
    FOnClickAccept: TNotifyEvent;
    FOnClickCopy: TNotifyEvent;
    FOnClickPrevious: TNotifyEvent;
    FOnClickNext: TNotifyEvent;
    FOnClickUndo: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnClickAdd: TNotifyEvent;
    FOnClickSub: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    FOnDblClick: TNotifyEvent;
    FEditColor: TColor;
    FCharCase: TEditCharCase;
    FHideSelection: Boolean;
    FMaxLength: integer;
    FReadOnly: boolean;
    FEmptyText: string;
    FEmptyTextFocused: boolean;
    procedure SetText(const Value: string);
    procedure SetButtons(const Value: TEditButtons);
    function GetText: string;
    function GetVersion: string;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure SetEditColor(const Value: TColor);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMaxLength(const Value: integer);
    procedure SetReadOnly(const Value: boolean);
    procedure SetEmptyText(const Value: string);
    procedure SetEmptyTextFocused(const Value: boolean);
  protected
    procedure ButtonClick(Sender: TObject); virtual;
    procedure DoClickButton(ButtonIndex: integer); virtual;
    procedure DoClickSub; virtual;
    procedure DoClickAdd; virtual;
    procedure DoClickFind; virtual;
    procedure DoClickClear; virtual;
    procedure DoClickOK; virtual;
    procedure DoClickClose; virtual;
    procedure DoClickCopy; virtual;
    procedure DoClickAccept; virtual;
    procedure DoClickDeny; virtual;
    procedure DoClickTrash; virtual;
    procedure DoClickNext; virtual;
    procedure DoClickPrevious; virtual;
    procedure DoClickUndo; virtual;
    procedure DoEditChange(Sender: TObject); virtual;
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeypress(Sender: TObject; var Key: Char); virtual;
    procedure DoEditEnter(Sender: TObject); virtual;
    procedure DoEditExit(Sender: TObject); virtual;
    procedure DoEditDblClick(Sender: TObject); virtual;
    procedure DoEditClick(Sender: TObject); virtual;
    procedure CreateWnd; override;
    procedure DoEnter; override;
    function CreateButtons: TEditButtons; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure UpdateButtons;
    property Buttons: TEditButtons read FEditButtons write SetButtons;
    property OnClickAdd: TNotifyEvent read FOnClickAdd write FOnClickAdd;
    property OnClickSub: TNotifyEvent read FOnClickSub write FOnClickSub;
    property OnClickFind: TNotifyEvent read FOnClickFind write FOnClickFind;
    property OnClickClear: TNotifyEvent read FOnClickClear write FOnClickClear;
    property OnClickOK: TNotifyEvent read FOnClickOK write FOnClickOK;
    property OnClickTrash: TNotifyEvent read FOnClickTrash write FOnClickTrash;
    property OnClickAccept: TNotifyEvent read FOnClickAccept write FOnClickAccept;
    property OnClickDeny: TNotifyEvent read FOnClickDeny write FOnClickDeny;
    property OnClickClose: TNotifyEvent read FOnClickClose write FOnClickClose;
    property OnClickCopy: TNotifyEvent read FOnClickCopy write FOnClickCopy;
    property OnClickNext: TNotifyEvent read FOnClickNext write FOnClickNext;
    property OnClickPrevious: TNotifyEvent read FOnClickPrevious write FOnClickPrevious;
    property OnClickUndo: TNotifyEvent read FOnClickUndo write FOnClickUndo;
    property OnClickCustom: TButtonClickEvent read FOnClickCustom write FOnClickCustom;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: integer;
    property Edit: TAdvEdit read FEdit;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property DragCursor;
    property DragMode;
    property DragKind;
    property EditColor: TColor read FEditColor write SetEditColor default clWindow;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property EmptyTextFocused: boolean read FEmptyTextFocused write SetEmptyTextFocused;
    property Enabled;
    property Font;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default true;
    property Hint;
    property Images: TCustomImageList read FImages write FImages;
    property MaxLength: integer read FMaxLength write SetMaxLength default 0;
    property ParentCtl3D;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default false;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Text: string read GetText write SetText;
    property Version: string read GetVersion;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;

  TAdvMultiButtonEdit = class(TAdvCustomMultiButtonEdit)
  published
    property Buttons;
    property OnClickAdd;
    property OnClickFind;
    property OnClickClear;
    property OnClickOK;
    property OnClickTrash;
    property OnClickAccept;
    property OnClickDeny;
    property OnClickClose;
    property OnClickCopy;
    property OnClickNext;
    property OnClickPrevious;
    property OnClickUndo;
    property OnClickCustom;
  end;



implementation

uses
  SysUtils, Clipbrd;

type
  TSpeedButtonCrack = class(TPNGSpeedButton);

{ TAdvMultiButtonEdit }

procedure TAdvCustomMultiButtonEdit.ButtonClick(Sender: TObject);
var
  sp: TPNGSpeedButton;
begin
  if (Sender is TPNGSpeedButton) then
  begin
    sp := (Sender as TPNGSpeedButton);

    case Buttons[sp.Tag].Style of
    bsClear: DoClickClear;
    bsFind: DoClickFind;
    bsOK: DoClickOK;
    bsCopy: DoClickCopy;
    bsNext: DoClickNext;
    bsPrevious: DoClickPrevious;
    bsTrash: DoClickTrash;
    bsClose: DoClickClose;
    bsAccept: DoClickAccept;
    bsDeny: DoClickDeny;
    bsUndo: DoClickUndo;
    bsAdd: DoClickAdd;
    bsSub: DoClickSub;
    bsCustom: DoClickButton(sp.Tag);
    end;
  end;
end;

procedure TAdvCustomMultiButtonEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FEdit) then
    FEdit.BiDiMode := BidiMode;
end;

procedure TAdvCustomMultiButtonEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvCustomMultiButtonEdit.CMEnabledChanged(var Message: TMessage);
var
  i: integer;
begin
  inherited;
  FEdit.Enabled := Enabled;

  if not (csDesigning in ComponentState) then
  begin
    for i := 0 to Buttons.Count - 1 do
      Buttons[i].FButton.Enabled := Enabled;
  end;
end;

procedure TAdvCustomMultiButtonEdit.CMFontChanged(var Message: TMessage);
begin
  FEdit.Font.Assign(Font);
end;

constructor TAdvCustomMultiButtonEdit.Create(AOwner: TComponent);
begin
  inherited;
  FEdit := TAdvEdit.Create(Self);
  FEdit.OnChange := DoEditChange;
  FEdit.OnKeyDown:= DoEditKeyDown;
  FEdit.OnKeyUp := DoEditKeyUp;
  FEdit.OnKeyPress := DoEditKeypress;
  FEdit.OnEnter := DoEditEnter;
  FEdit.OnExit := DoEditExit;
  FEdit.OnDblClick := DoEditDblClick;
  FEdit.OnClick := DoEditClick;
  FEditButtons := CreateButtons;
  FEdit.TabStop := true;
  FEdit.TabOrder := 0;
  FBorderStyle := bsSingle;
  FEditColor := clWindow;
  FHideSelection := true;
  FMaxLength := 0;
  Height := 22;
  Width := 200;
end;

function TAdvCustomMultiButtonEdit.CreateButtons: TEditButtons;
begin
  Result := TEditButtons.Create(Self);
end;

procedure TAdvCustomMultiButtonEdit.CreateWnd;
begin
  inherited;
  UpdateButtons;
end;

destructor TAdvCustomMultiButtonEdit.Destroy;
begin
  FEditButtons.Free;
  inherited;
end;

procedure TAdvCustomMultiButtonEdit.DoClickAccept;
begin
  if Assigned(OnClickAccept) then
    OnClickAccept(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickAdd;
begin
  if Assigned(OnClickAdd) then
    OnClickAdd(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickSub;
begin
  if Assigned(OnClickSub) then
    OnClickSub(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickButton(ButtonIndex: integer);
begin
  if Assigned(OnClickCustom) then
    OnClickCustom(Self, ButtonIndex);
end;

procedure TAdvCustomMultiButtonEdit.DoClickClear;
begin
  FEdit.Text := '';
  if Assigned(OnClickClear) then
    OnClickClear(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickClose;
begin
  Visible := false;
  if Assigned(OnClickClose) then
    OnClickClose(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickDeny;
begin
  if Assigned(OnClickDeny) then
    OnClickDeny(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickCopy;
begin
  if Assigned(OnClickCopy) then
    OnClickCopy(Self);
  Clipboard.Open;
  Clipboard.AsText := FEdit.Text;
  Clipboard.Close;
end;

procedure TAdvCustomMultiButtonEdit.DoClickFind;
begin
  if Assigned(OnClickFind) then
    OnClickFind(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickNext;
begin
  if Assigned(OnClickNext) then
    OnClickNext(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickOK;
begin
  if Assigned(OnClickOK) then
    OnClickOK(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickPrevious;
begin
  if Assigned(OnClickPrevious) then
    OnClickPrevious(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoClickTrash;
begin
  if Assigned(OnClickTrash) then
    OnClickTrash(Self);
  FEdit.Text := '';
end;

procedure TAdvCustomMultiButtonEdit.DoClickUndo;
begin
  if Assigned(OnClickUndo) then
    OnClickUndo(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditEnter(Sender: TObject);
begin
  if Assigned(OnEnter) then
    OnEnter(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditExit(Sender: TObject);
begin
  if Assigned(OnExit) then
    OnExit(Self);
end;

procedure TAdvCustomMultiButtonEdit.DoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;

procedure TAdvCustomMultiButtonEdit.DoEditKeypress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TAdvCustomMultiButtonEdit.DoEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvCustomMultiButtonEdit.DoEnter;
begin
  FEdit.SetFocus;
end;

function TAdvCustomMultiButtonEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

function TAdvCustomMultiButtonEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomMultiButtonEdit.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomMultiButtonEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TAdvCustomMultiButtonEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
  if Assigned(FEdit) then
    FEdit.BorderStyle := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetButtons(const Value: TEditButtons);
begin
  FEditButtons.Assign(Value);
end;

procedure TAdvCustomMultiButtonEdit.SetCharCase(const Value: TEditCharCase);
begin
  FCharCase := Value;

  if Assigned(FEdit) then
    FEdit.CharCase := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEditColor(const Value: TColor);
begin
  FEditColor := Value;
  if Assigned(FEdit) then
    FEdit.Color := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEmptyText(const Value: string);
begin
  FEmptyText := Value;
  if Assigned(FEdit) then
    FEdit.EmptyText := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetEmptyTextFocused(const Value: boolean);
begin
  FEmptyTextFocused := Value;
  if Assigned(FEdit) then
    FEdit.EmptyTextFocused := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  if Assigned(FEdit) then
    FEdit.HideSelection := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetMaxLength(const Value: integer);
begin
  FMaxLength := Value;
  if Assigned(FEdit) then
    FEdit.MaxLength := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
  if Assigned(FEdit) then
    FEdit.ReadOnly := Value;
end;

procedure TAdvCustomMultiButtonEdit.SetText(const Value: string);
begin
  FEdit.Text := Value;
end;

procedure TAdvCustomMultiButtonEdit.UpdateButtons;
var
  i: integer;
  FromLeft,FromRight: integer;
begin
  if (csReading in ComponentState) then
    Exit;

  FromLeft := 0;
  FromRight := Width;
  for i := 0 to Buttons.Count - 1 do
  begin
    Buttons[i].FButton.Parent := Self;

    if Buttons[i].Position = bpLeft then
    begin
      Buttons[i].FButton.Left := FromLeft;
      Buttons[i].FButton.Align := alLeft;
      FromLeft := FromLeft + Buttons[i].FButton.Width;
    end;

    if Buttons[i].Position = bpRight then
    begin
      Buttons[i].FButton.Left := FromRight - Buttons[i].FButton.Width;
      Buttons[i].FButton.Align := alRight;
      FromRight := FromRight + Buttons[i].FButton.Width;
    end;

    if (Buttons[i].Style = bsCustom) and Assigned(FImages) and (Buttons[i].ImageIndex >= 0) then
    begin
      TSpeedButtonCrack(Buttons[i].FButton).CopyImage(FImages, Buttons[i].ImageIndex);
    end;

    case Buttons[i].Style of
    bsClear: Buttons[i].FButton.PNGName := 'tms_gl_cancel';
    bsFind: Buttons[i].FButton.PNGName := 'tms_gl_search';
    bsOK: Buttons[i].FButton.PNGName := 'tms_gl_ok';
    bsTrash: Buttons[i].FButton.PNGName := 'tms_gl_trash';
    bsCopy: Buttons[i].FButton.PNGName := 'tms_gl_copy';
    bsClose: Buttons[i].FButton.PNGName := 'tms_gl_close';
    bsDeny: Buttons[i].FButton.PNGName := 'tms_gl_deny';
    bsAccept: Buttons[i].FButton.PNGName := 'tms_gl_accept';
    bsPrevious: Buttons[i].FButton.PNGName := 'tms_gl_prev';
    bsNext: Buttons[i].FButton.PNGName := 'tms_gl_next';
    bsUndo: Buttons[i].FButton.PNGName := 'tms_gl_undo';
    bsAdd: Buttons[i].FButton.PNGName := 'tms_gl_add';
    bsSub: Buttons[i].FButton.PNGName := 'tms_gl_sub';
    end;

    Buttons[i].FButton.Hint := Buttons[i].Hint;
    Buttons[i].FButton.ShowHint := Buttons[i].Hint <> '';

    Buttons[i].FButton.Tag := i;
    Buttons[i].FButton.Top := 0;
    Buttons[i].FButton.Enabled := Buttons[i].Enabled;
    Buttons[i].FButton.Flat := Buttons[i].Flat;
    Buttons[i].FButton.OnClick := ButtonClick;
  end;

  FEdit.Parent := Self;
  FEdit.Left := FromLeft;
  FEdit.Align := alClient;
end;


{ TEditButtons }

function TEditButtons.Add: TEditButton;
begin
  Result := TEditButton(inherited Add);
end;

constructor TEditButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TEditButton);
  if AOwner is TAdvMultiButtonEdit then
    FEdit := AOwner as TAdvMultiButtonEdit
  else
    FEdit := nil;
end;

function TEditButtons.FindButton(AStyle: TButtonStyle): TEditButton;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Style = AStyle then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TEditButtons.GetItem(Index: Integer): TEditButton;
begin
  Result := TEditButton(inherited Items[Index]);
end;

function TEditButtons.Insert(Index: Integer): TEditButton;
begin
  Result := TEditButton(inherited Insert(Index));
end;

procedure TEditButtons.SetItem(Index: Integer; const Value: TEditButton);
begin
  inherited Items[Index] := Value;
end;

procedure TEditButtons.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FEdit) then
    FEdit.UpdateButtons;
end;

{ TEditButton }

procedure TEditButton.Assign(Source: TPersistent);
begin
  if (Source is TEditButton) then
  begin
    FButtonPosition := (Source as TEditButton).Position;
    FFlat := (Source as TEditButton).Flat;
    FEnabled := (Source as TEditButton).Enabled;
    FHint := (Source as TEditButton).Hint;
    FStyle := (Source as TEditButton).Style;
    FImageIndex := (Source as TEditButton).ImageIndex;
  end;
end;

constructor TEditButton.Create(Collection: TCollection);
begin
  Collection.BeginUpdate;
  inherited Create(Collection);
  FButton := TPNGSpeedButton.Create((Collection as TEditButtons).FEdit);
  FButtonPosition := bpRight;
  FFlat := false;
  FEnabled := true;
  FImageIndex := -1;
  FStyle := bsCustom;
  Collection.EndUpdate;
end;

destructor TEditButton.Destroy;
begin
  FButton.Free;
  inherited;
end;

procedure TEditButton.SetButtonPosition(const Value: TButtonPosition);
begin
  if (FButtonPosition <> Value) then
  begin
    FButtonPosition := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetFlat(const Value: boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetHint(const Value: string);
begin
  if (FHint <> Value) then
  begin
    FHint := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TEditButton.SetStyle(const Value: TButtonStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    Changed(False);
  end;
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

end.
