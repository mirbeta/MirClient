{***************************************************************************}
{ TMS Spell Check component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

unit TMSSpellCheckCorrectForm;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Variants, Classes, TMSSpellCheck
  {$IFNDEF FMXLIB}
  , Graphics, Controls, Forms, Dialogs, StdCtrls,  ExtCtrls
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.ListBox, FMX.Layouts, FMX.Types, System.UITypes, FMX.TMSBaseControl
  {$ENDIF}
  ;

type
  TAdvSpellCheckCorrectPanel = class;

  TErrorWordEvent = procedure(Sender: TObject; var ErrorWord: string) of object;
  TCorrectWordEvent = procedure(Sender: TObject; ErrorWord, CorrectedWord: string) of object;
  TIgnoreWordEvent = procedure(Sender: TObject; ErrorWord: string) of object;
  TAddWordEvent = procedure(Sender: TObject; NewWord: string) of object;

  TSpellCheckPanelUI = class(TPersistent)
  private
    FCaptionChange: string;
    FHintNext: string;
    FCaptionPrevious: string;
    FHintChangeAll: string;
    FHintAdd: string;
    FShowIgnoreAll: boolean;
    FShowChange: boolean;
    FCaptionIgnore: string;
    FShowPrevious: boolean;
    FCaptionNext: string;
    FHintIgnoreAll: string;
    FHintChange: string;
    FShowIgnore: boolean;
    FCaptionChangeAll: string;
    FCaptionAdd: string;
    FHintPrevious: string;
    FShowNext: boolean;
    FHintIgnore: string;
    FShowChangeAll: boolean;
    FShowAdd: boolean;
    FCaptionIgnoreAll: string;
    FOnChange: TNotifyEvent;
    FQueryWord: string;
    FQueryAdd: string;
    procedure SetCaptionAdd(const Value: string);
    procedure SetCaptionChange(const Value: string);
    procedure SetCaptionChangeAll(const Value: string);
    procedure SetCaptionIgnore(const Value: string);
    procedure SetCaptionIgnoreAll(const Value: string);
    procedure SetCaptionNext(const Value: string);
    procedure SetCaptionPrevious(const Value: string);
    procedure SetShowAdd(const Value: boolean);
    procedure SetShowChange(const Value: boolean);
    procedure SetShowChangeAll(const Value: boolean);
    procedure SetShowIgnore(const Value: boolean);
    procedure SetShowIgnoreAll(const Value: boolean);
    procedure SetShowNext(const Value: boolean);
    procedure SetShowPrevious(const Value: boolean);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ShowIgnore: boolean read FShowIgnore write SetShowIgnore;
    property ShowIgnoreAll: boolean read FShowIgnoreAll write SetShowIgnoreAll;
    property ShowAdd: boolean read FShowAdd write SetShowAdd;
    property ShowChange: boolean read FShowChange write SetShowChange;
    property ShowChangeAll: boolean read FShowChangeAll write SetShowChangeAll;
    property ShowNext: boolean read FShowNext write SetShowNext;
    property ShowPrevious: boolean read FShowPrevious write SetShowPrevious;

    property CaptionIgnore: string read FCaptionIgnore write SetCaptionIgnore;
    property CaptionIgnoreAll: string read FCaptionIgnoreAll write SetCaptionIgnoreAll;
    property CaptionAdd: string read FCaptionAdd write SetCaptionAdd;

    property CaptionChange: string read FCaptionChange write SetCaptionChange;
    property CaptionChangeAll: string read FCaptionChangeAll write SetCaptionChangeAll;
    property CaptionNext: string read FCaptionNext write SetCaptionNext;
    property CaptionPrevious: string read FCaptionPrevious write SetCaptionPrevious;

    property HintIgnore: string read FHintIgnore write FHintIgnore;
    property HintIgnoreAll: string read FHintIgnoreAll write FHintIgnoreAll;
    property HintAdd: string read FHintAdd write FHintAdd;

    property HintChange: string read FHintChange write FHintChange;
    property HintChangeAll: string read FHintChangeAll write FHintChangeAll;
    property HintNext: string read FHintNext write FHintNext;
    property HintPrevious: string read FHintPrevious write FHintPrevious;

    property QueryAdd: string read FQueryAdd write FQueryAdd;
    property QueryWord: string read FQueryWord write FQueryWord;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TAdvSpellCheckCorrectPanel = class(TCustomPanel)
  {$ENDIF}
  {$IFDEF FMXLIB}
  TAdvSpellCheckCorrectPanel = class(TPanel)
  {$ENDIF}
  private
    FSpellCheck: TAdvSpellCheck;
    btnIgnore: TButton;
    btnIgnoreAll: TButton;
    btnAdd: TButton;
    btnChange: TButton;
    btnChangeAll: TButton;
    lstSuggestions: TListBox;
    btnPrevious: TButton;
    btnNext: TButton;
    lblErrorWord: TLabel;
    FOnIgnoreWord: TIgnoreWordEvent;
    FOnCorrectWordAll: TCorrectWordEvent;
    FOnPreviousError: TNotifyEvent;
    FOnCorrectWord: TCorrectWordEvent;
    FOnIgnoreWordAll: TIgnoreWordEvent;
    FOnGetErrorWord: TErrorWordEvent;
    FOnAddWord: TAddWordEvent;
    FOnNextError: TNotifyEvent;
    FUI: TSpellCheckPanelUI;
    procedure HandleNextError(Sender: TObject);
    procedure HandlePreviousError(Sender: TObject);
    procedure HandleChangeWord(Sender: TObject);
    procedure HandleChangeWordAll(Sender: TObject);
    procedure HandleIgnore(Sender: TObject);
    procedure HandleIgnoreAll(Sender: TObject);
    procedure HandleAdd(Sender: TObject);
    procedure SetUI(const AUI: TSpellCheckPanelUI);
    procedure UIChanged(Sender: TObject);
    procedure UpdateUI;
  protected
    procedure DoNextError; virtual;
    procedure DoPreviousError; virtual;
    function DoGetCurrentError: string; virtual;
    procedure DoChangeWord(ErrorWord, CorrectWord: string); virtual;
    procedure DoChangeWordAll(ErrorWord, CorrectWord: string); virtual;
    procedure DoIgnore(ErrorWord: string); virtual;
    procedure DoIgnoreAll(ErrorWord: string); virtual;
    procedure DoAdd(NewWord: string); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoUpdate;
  published
    property Align;
    property Anchors;
    {$IFNDEF FMXLIB}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    {$ENDIF}
    property Enabled;
    property Hint;
    property Padding;
    {$IFNDEF FMXLIB}
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    {$ENDIF}
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    {$IFNDEF FMXLIB}
    property TabStop;
    {$ENDIF}
    property Touch;
    property UI: TSpellCheckPanelUI read FUI write SetUI;
    property Visible;

    property SpellCheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
    property OnAddWord: TAddWordEvent read FOnAddWord write FOnAddWord;
    property OnNextError: TNotifyEvent read FOnNextError write FOnNextError;
    property OnPreviousError: TNotifyEvent read FOnPreviousError write FOnPreviousError;
    property OnGetErrorWord: TErrorWordEvent read FOnGetErrorWord write FOnGetErrorWord;
    property OnCorrectWord: TCorrectWordEvent read FOnCorrectWord write FOnCorrectWord;
    property OnCorrectWordAll: TCorrectWordEvent read FOnCorrectWordAll write FOnCorrectWordAll;
    property OnIgnoreWord: TIgnoreWordEvent read FOnIgnoreWord write FOnIgnoreWord;
    property OnIgnoreWordAll: TIgnoreWordEvent read FOnIgnoreWordAll write FOnIgnoreWordAll;
  end;


  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TAdvSpellCheckCorrectDialog = class(TComponent)
  private
    FSpellCheck: TAdvSpellCheck;
    FSpellCheckCorrectFrm: TForm;
    FSpellCheckCorrectPnl: TAdvSpellCheckCorrectPanel;
    FOnNextError: TNotifyEvent;
    FOnPreviousError: TNotifyEvent;
    FOnGetErrorWord: TErrorWordEvent;
    FOnCorrectWordAll: TCorrectWordEvent;
    FOnCorrectWord: TCorrectWordEvent;
    FOnIgnoreWord: TIgnoreWordEvent;
    FOnIgnoreWordAll: TIgnoreWordEvent;
    FOnAddWord: TAddWordEvent;
    FUI: TSpellCheckPanelUI;
    FCaption: string;
    FSingleWord: boolean;
    FErrorWord: string;
    FCorrectWord: string;
    procedure HandleNextError(Sender: TObject);
    procedure HandlePreviousError(Sender: TObject);
    procedure HandleChangeWord(Sender: TObject; ErrorWord, CorrectedWord: string);
    procedure HandleChangeWordAll(Sender: TObject; ErrorWord, CorrectedWord: string);
    procedure HandleShow(Sender: TObject);
    procedure HandleIgnore(Sender: TObject; ErrorWord: string);
    procedure HandleIgnoreAll(Sender: TObject; ErrorWord: string);
    procedure HandleGetErrorWord(Sender: TObject; var ErrorWord: string);
    procedure HandleAdd(Sender: TObject; NewWord: string);
    procedure SetUI(const Value: TSpellCheckPanelUI);
  protected
    procedure DoNextError; virtual;
    procedure DoPreviousError; virtual;
    function DoGetCurrentError: string; virtual;
    procedure DoChangeWord(ErrorWord, CorrectWord: string); virtual;
    procedure DoChangeWordAll(ErrorWord, CorrectWord: string); virtual;
    procedure DoIgnore(ErrorWord: string); virtual;
    procedure DoIgnoreAll(ErrorWord: string); virtual;
    procedure DoGetErrorWord(var ErrorWord: string); virtual;
    procedure DoAddWord(NewWord: string); virtual;
    procedure DoUpdate;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: TModalResult; overload;
    function Execute(var s: string): TModalResult; overload;
    function Execute(X,Y: integer; var s: string): TModalResult; overload;
  published
    property Caption: string read FCaption write FCaption;
    property SpellCheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
    property UI: TSpellCheckPanelUI read FUI write SetUI;
    property OnNextError: TNotifyEvent read FOnNextError write FOnNextError;
    property OnPreviousError: TNotifyEvent read FOnPreviousError write FOnPreviousError;
    property OnAddWord: TAddWordEvent read FOnAddWord write FOnAddWord;
    property OnGetErrorWord: TErrorWordEvent read FOnGetErrorWord write FOnGetErrorWord;
    property OnCorrectWord: TCorrectWordEvent read FOnCorrectWord write FOnCorrectWord;
    property OnCorrectWordAll: TCorrectWordEvent read FOnCorrectWordAll write FOnCorrectWordAll;
    property OnIgnoreWord: TIgnoreWordEvent read FOnIgnoreWord write FOnIgnoreWord;
    property OnIgnoreWordAll: TIgnoreWordEvent read FOnIgnoreWordAll write FOnIgnoreWordAll;
  end;


implementation

{ TAdvSpellCheckCorrectDialog }

constructor TAdvSpellCheckCorrectDialog.Create(AOwner: TComponent);
begin
  inherited;
  FUI := TSpellCheckPanelUI.Create;
end;

destructor TAdvSpellCheckCorrectDialog.Destroy;
begin
  FUI.Free;
  inherited;
end;

procedure TAdvSpellCheckCorrectDialog.DoAddWord(NewWord: string);
begin
  if Assigned(OnAddWord) then
    OnAddWord(Self, NewWord);
end;

procedure TAdvSpellCheckCorrectDialog.DoChangeWord(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(OnCorrectWord) then
    OnCorrectWord(Self, ErrorWord, CorrectWord);
end;

procedure TAdvSpellCheckCorrectDialog.DoChangeWordAll(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(OnCorrectWordAll) then
    OnCorrectWordAll(Self, ErrorWord, CorrectWord);
end;

function TAdvSpellCheckCorrectDialog.DoGetCurrentError: string;
begin
  Result := '';
  if Assigned(OnGetErrorWord) then
    OnGetErrorWord(Self, Result);
end;

procedure TAdvSpellCheckCorrectDialog.DoGetErrorWord(var ErrorWord: string);
begin
  ErrorWord := DoGetCurrentError;
end;

procedure TAdvSpellCheckCorrectDialog.DoIgnore(ErrorWord: string);
begin
  if Assigned(OnIgnoreWord) then
    OnIgnoreWord(Self, ErrorWord);

  if FSingleWord then
    FSpellCheckCorrectFrm.ModalResult := mrOK
  else
    DoNextError;
end;

procedure TAdvSpellCheckCorrectDialog.DoIgnoreAll(ErrorWord: string);
begin
  if Assigned(OnIgnoreWordAll) then
    OnIgnoreWordAll(Self, ErrorWord);

  if FSingleWord then
    FSpellCheckCorrectFrm.ModalResult := mrOK
  else
    DoNextError;
end;

procedure TAdvSpellCheckCorrectDialog.DoNextError;
begin
  if Assigned(OnNextError) then
    OnNextError(Self);

  DoUpdate;
end;

procedure TAdvSpellCheckCorrectDialog.DoPreviousError;
begin
  if Assigned(OnPreviousError) then
    OnPreviousError(Self);

  DoUpdate;
end;


procedure TAdvSpellCheckCorrectDialog.HandleAdd(Sender: TObject;
  NewWord: string);
begin
  FCorrectWord := NewWord;
  if FSingleWord then
    FSpellCheckCorrectFrm.ModalResult := mrOK;
end;

procedure TAdvSpellCheckCorrectDialog.HandleChangeWord(Sender: TObject; ErrorWord, CorrectedWord: string);
begin
  DoChangeWord(ErrorWord, CorrectedWord);
  FCorrectWord := CorrectedWord;
  if FSingleWord then
    FSpellCheckCorrectFrm.ModalResult := mrOK;
end;

procedure TAdvSpellCheckCorrectDialog.HandleChangeWordAll(Sender: TObject; ErrorWord, CorrectedWord: string);
begin
  DoChangeWordAll(ErrorWord, CorrectedWord);
  FCorrectWord := CorrectedWord;
  if FSingleWord then
    FSpellCheckCorrectFrm.ModalResult := mrOK;
end;

procedure TAdvSpellCheckCorrectDialog.HandleGetErrorWord(Sender: TObject;
  var ErrorWord: string);
begin
  DoGetErrorWord(ErrorWord);
end;

procedure TAdvSpellCheckCorrectDialog.HandleIgnore(Sender: TObject; ErrorWord: string);
begin
  DoIgnore(ErrorWord);
end;

procedure TAdvSpellCheckCorrectDialog.HandleIgnoreAll(Sender: TObject; ErrorWord: string);
begin
  DoIgnoreAll(ErrorWord);
end;

procedure TAdvSpellCheckCorrectDialog.HandleNextError(Sender: TObject);
begin
  DoNextError;
end;

procedure TAdvSpellCheckCorrectDialog.HandlePreviousError(Sender: TObject);
begin
  DoPreviousError;
end;

procedure TAdvSpellCheckCorrectDialog.HandleShow(Sender: TObject);
begin
  FSpellCheckCorrectPnl.DoUpdate;
  DoUpdate;
end;

procedure TAdvSpellCheckCorrectDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FSpellCheck) then
    FSpellCheck := nil;
end;

procedure TAdvSpellCheckCorrectDialog.SetUI(const Value: TSpellCheckPanelUI);
begin
  FUI.Assign(Value);
end;

procedure TAdvSpellCheckCorrectDialog.DoUpdate;
var
  s: string;
begin
  if Assigned(OnGetErrorWord) then
  begin
    s := DoGetCurrentError;

    {$IFNDEF FMXLIB}
    FSpellCheckCorrectPnl.lblErrorWord.Caption := s;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FSpellCheckCorrectPnl.lblErrorWord.Text := s;
    {$ENDIF}
  end
  else
  begin
    {$IFNDEF FMXLIB}
    s := FSpellCheckCorrectPnl.lblErrorWord.Caption;
    {$ENDIF}
    {$IFDEF FMXLIB}
    s := FSpellCheckCorrectPnl.lblErrorWord.Text;
    {$ENDIF}
  end;

  if (s <> '') and Assigned(FSpellCheck) then
  begin
    s := FSpellCheck.Suggestions(s);
    FSpellCheckCorrectPnl.lstSuggestions.Items.Text := s;
    FSpellCheckCorrectPnl.lstSuggestions.ItemIndex := 0;
  end
  else
    FSpellCheckCorrectPnl.lstSuggestions.Items.Clear;
end;

function TAdvSpellCheckCorrectDialog.Execute(X, Y: integer;
  var s: string): TModalResult;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create(TMSSPELLERRMSG);

  FSingleWord := (s <> '');
  FErrorWord := s;
  FCorrectWord := s;

  FSpellCheckCorrectFrm := TForm.CreateNew(nil);
  try
    {$IFNDEF FMXLIB}
    FSpellCheckCorrectFrm.BorderStyle := bsToolWindow;
    {$ENDIF}
    {$IFDEF FMXLIB}
    {$IFDEF DELPHIXE6_LVL}
    FSpellCheckCorrectFrm.BorderStyle := TFmxFormBorderStyle.ToolWindow;
    {$ELSE}
    FSpellCheckCorrectFrm.BorderStyle := TFmxFormBorderStyle.bsToolWindow;
    {$ENDIF}
    {$ENDIF}

    FSpellCheckCorrectFrm.Height := 327;
    FSpellCheckCorrectFrm.Width := 305;
    FSpellCheckCorrectFrm.Caption := Caption;

    FSpellCheckCorrectPnl := TAdvSpellCheckCorrectPanel.Create(FSpellCheckCorrectFrm);
    FSpellCheckCorrectPnl.Parent := FSpellCheckCorrectFrm;

    FSpellCheckCorrectPnl.Visible := true;
    FSpellCheckCorrectPnl.SpellCheck := SpellCheck;
    FSpellCheckCorrectPnl.UI.Assign(FUI);

    {$IFNDEF FMXLIB}
    FSpellCheckCorrectPnl.Align := alClient;
    FSpellCheckCorrectPnl.lblErrorWord.Caption := s;
    {$ENDIF}
    {$IFDEF FMXLIB}
    {$IFDEF DELPHIXE6_LVL}
    FSpellCheckCorrectPnl.Align := TAlignLayout.Client;
    {$ELSE}
    FSpellCheckCorrectPnl.Align := TAlignLayout.alClient;
    {$ENDIF}
    FSpellCheckCorrectPnl.lblErrorWord.Text := s;
    {$ENDIF}

    if (x <> -1) and (y <> -1) then
    begin
      {$IFNDEF FMXLIB}
      FSpellCheckCorrectFrm.Position := poDesigned;
      {$ENDIF}
      FSpellCheckCorrectFrm.Left := x;
      FSpellCheckCorrectFrm.Top := y;
    end;

    FSpellCheckCorrectPnl.OnPreviousError := HandlePreviousError;
    FSpellCheckCorrectPnl.OnNextError := HandleNextError;
    FSpellCheckCorrectPnl.OnCorrectWord := HandleChangeWord;
    FSpellCheckCorrectPnl.OnCorrectWordAll := HandleChangeWordAll;
    FSpellCheckCorrectPnl.OnIgnoreWord := HandleIgnore;
    FSpellCheckCorrectPnl.OnIgnoreWordAll := HandleIgnoreAll;
    FSpellCheckCorrectPnl.OnGetErrorWord := HandleGetErrorWord;
    FSpellCheckCorrectPnl.OnAddWord := HandleAdd;

    FSpellCheckCorrectFrm.OnShow := HandleShow;
    Result := FSpellCheckCorrectFrm.ShowModal;
    s := FCorrectWord;
  finally
    FSpellCheckCorrectFrm.Free;
  end;


end;

function TAdvSpellCheckCorrectDialog.Execute: TModalResult;
var
  s: string;
begin
  s := '';
  Result := Execute(s);
end;

function TAdvSpellCheckCorrectDialog.Execute(var s: string): TModalResult;
begin
  Result := Execute(-1,-1,s);
end;

//------------------------------------------------------------------------------

{ TAdvSpellCheckCorrectPanel }

constructor TAdvSpellCheckCorrectPanel.Create(AOwner: TComponent);
{$IFDEF FMXLIB}
var
  i: integer;
{$ENDIF}

  {$IFNDEF FMXLIB}
  procedure InitControl(ctrl: TWinControl; x, y, ts: integer);
  {$ENDIF}
  {$IFDEF FMXLIB}
  procedure InitControl(ctrl: TControl; x, y, ts: integer);
  {$ENDIF}
  begin
    ctrl.Parent := Self;
    {$IFNDEF FMXLIB}
    ctrl.Left := x;
    ctrl.Top := y;
    ctrl.TabStop := true;
    {$ENDIF}
    {$IFDEF FMXLIB}
    ctrl.Position.x := x;
    ctrl.Position.y := y;
    {$ENDIF}
    ctrl.TabOrder := ts;
  end;

begin
  inherited;
  Width := 305;
  Height := 322;

  {$IFNDEF FMXLIB}
  Caption := '';
  {$ENDIF}

  FUI := TSpellCheckPanelUI.Create;
  FUI.OnChange := UIChanged;

  btnIgnore := TButton.Create(Self);
  btnIgnoreAll := TButton.Create(Self);
  btnAdd := TButton.Create(Self);
  lstSuggestions := TListBox.Create(Self);
  btnChange := TButton.Create(Self);
  btnChangeAll := TButton.Create(Self);
  btnNext := TButton.Create(Self);
  btnPrevious := TButton.Create(Self);
  lblErrorWord := TLabel.Create(Self);

  InitControl(btnIgnore, 16, 41, 0);
  InitControl(btnIgnoreAll, 110, 41, 1);
  InitControl(btnAdd, 207, 41, 2);
  InitControl(lstSuggestions, 16, 72, 3);

  lstSuggestions.Width := 266;
  lstSuggestions.Height := 153;
  lstSuggestions.Visible := true;

  InitControl(btnChange, 16, 231, 4);
  InitControl(btnChangeAll, 110, 231, 5);

  InitControl(btnPrevious, 16, 271, 6);

  InitControl(btnNext, 110, 271, 7);

  btnIgnore.Width := 75;
  btnIgnoreAll.Width := 75;
  btnAdd.Width := 75;

  lblErrorWord.Parent := Self;
  lblErrorWord.AutoSize := true;
  {$IFNDEF FMXLIB}
  lblErrorWord.Caption := '';
  lblErrorWord.Left := 16;
  lblErrorWord.Top := 4;
  lblErrorWord.Font.Name := 'Segoe UI';
  lblErrorWord.Font.Size := 12;
  lblErrorWord.Font.Style := [fsBold];
  {$ENDIF}
  {$IFDEF FMXLIB}
  lblErrorWord.Text := '';
  lblErrorWord.Position.X := 16;
  lblErrorWord.Position.Y := 4;
  {$ENDIF}
  lblErrorWord.Visible := true;

  UpdateUI;

  // event handlers
  btnPrevious.OnClick := HandlePreviousError;
  btnNext.OnClick := HandleNextError;
  btnChange.OnClick := HandleChangeWord;
  btnChangeAll.OnClick := HandleChangeWordAll;
  btnIgnore.OnClick := HandleIgnore;
  btnIgnoreAll.OnClick := HandleIgnoreAll;
  btnAdd.OnClick := HandleAdd;

  btnChange.TabOrder := 1;
//  btnChange.Default := true;
  btnChangeAll.TabOrder := 2;
  btnPrevious.TabOrder := 3;
  btnNext.TabOrder := 4;
  btnIgnore.TabOrder := 5;
  btnIgnoreAll.TabOrder := 6;
  btnAdd.TabOrder := 7;
  lstSuggestions.TabOrder := 0;

  {$IFDEF FMXLIB}
  for i := 0 to ControlsCount - 1 do
  begin
    Controls[i].Stored := false;
  end;
  {$ENDIF}
end;

destructor TAdvSpellCheckCorrectPanel.Destroy;
begin
  FUI.Free;
  inherited;
end;

procedure TAdvSpellCheckCorrectPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FSpellCheck) then
    FSpellCheck := nil;
end;

procedure TAdvSpellCheckCorrectPanel.SetUI(const AUI: TSpellCheckPanelUI);
begin
  FUI.Assign(AUI);
end;

procedure TAdvSpellCheckCorrectPanel.UIChanged(Sender: TObject);
begin
  UpdateUI;
end;

procedure TAdvSpellCheckCorrectPanel.UpdateUI;
begin
  {$IFNDEF FMXLIB}
  btnIgnore.Caption := UI.CaptionIgnore;
  btnIgnoreAll.Caption := UI.CaptionIgnoreAll;
  btnChange.Caption := UI.CaptionChange;
  btnChangeAll.Caption := UI.CaptionChangeAll;
  btnAdd.Caption := UI.CaptionAdd;
  btnNext.Caption := UI.CaptionNext;
  btnPrevious.Caption := UI.CaptionPrevious;
  {$ENDIF}
  {$IFDEF FMXLIB}
  btnIgnore.Text := UI.CaptionIgnore;
  btnIgnoreAll.Text := UI.CaptionIgnoreAll;
  btnChange.Text := UI.CaptionChange;
  btnChangeAll.Text := UI.CaptionChangeAll;
  btnAdd.Text := UI.CaptionAdd;
  btnNext.Text := UI.CaptionNext;
  btnPrevious.Text := UI.CaptionPrevious;
  {$ENDIF}

  {$IFNDEF FMXLIB}
  btnIgnore.Hint := UI.HintIgnore;
  btnIgnoreAll.Hint := UI.HintIgnoreAll;
  btnChange.Hint := UI.HintChange;
  btnChangeAll.Hint := UI.HintChangeAll;
  btnAdd.Hint := UI.HintAdd;
  btnNext.Hint := UI.HintNext;
  btnPrevious.Hint := UI.HintPrevious;

  btnIgnore.ShowHint := UI.HintIgnore <> '';
  btnIgnoreAll.ShowHint := UI.HintIgnoreAll <> '';
  btnChange.ShowHint := UI.HintChange <> '';
  btnChangeAll.ShowHint := UI.HintChangeAll <> '';
  btnAdd.ShowHint := UI.HintAdd <> '';
  btnNext.ShowHint := UI.HintNext <> '';
  btnPrevious.ShowHint := UI.HintPrevious <> '';
  {$ENDIF}

  btnIgnore.Visible := UI.ShowIgnore;
  btnIgnoreAll.Visible := UI.ShowIgnoreAll;
  btnChange.Visible := UI.ShowChange;
  btnChangeAll.Visible := UI.ShowChangeAll;
  btnAdd.Visible := UI.ShowAdd;
  btnNext.Visible := UI.ShowNext;
  btnPrevious.Visible := UI.ShowPrevious;
end;

procedure TAdvSpellCheckCorrectPanel.DoUpdate;
var
  s:string;
begin
  s := DoGetCurrentError;

  {$IFNDEF FMXLIB}
  lblErrorWord.Caption := s;
  {$ENDIF}

  {$IFDEF FMXLIB}
  lblErrorWord.Text := s;
  {$ENDIF}

  if (s <> '') and Assigned(FSpellCheck) then
  begin
    s := FSpellCheck.Suggestions(s);
    lstSuggestions.Items.Text := s;
    lstSuggestions.ItemIndex := 0;
  end
  else
    lstSuggestions.Items.Clear;
end;

procedure TAdvSpellCheckCorrectPanel.HandleAdd(Sender: TObject);
var
  v: string;
begin
  v := '';
  if InputQuery(UI.QueryAdd,UI.QueryWord,v) then
    DoAdd(v);
end;

procedure TAdvSpellCheckCorrectPanel.HandleChangeWord(Sender: TObject);
var
  idx: integer;
  s: string;
begin
  idx := lstSuggestions.ItemIndex;
  {$IFNDEF FMXLIB}
  s := lblErrorWord.Caption;
  {$ENDIF}
  {$IFDEF FMXLIB}
  s := lblErrorWord.Text;
  {$ENDIF}

  if idx <> -1 then
    DoChangeWord(s, lstSuggestions.Items[idx]);
end;

procedure TAdvSpellCheckCorrectPanel.HandleChangeWordAll(Sender: TObject);
var
  idx: integer;
  s: string;
begin
  idx := lstSuggestions.ItemIndex;
  {$IFNDEF FMXLIB}
  s := lblErrorWord.Caption;
  {$ENDIF}
  {$IFDEF FMXLIB}
  s := lblErrorWord.Text;
  {$ENDIF}

  if idx <> -1 then
    DoChangeWordAll(s, lstSuggestions.Items[idx]);
end;

procedure TAdvSpellCheckCorrectPanel.HandleIgnore(Sender: TObject);
begin
  {$IFNDEF FMXLIB}
  DoIgnore(lblErrorWord.Caption);
  {$ENDIF}
  {$IFDEF FMXLIB}
  DoIgnore(lblErrorWord.Text);
  {$ENDIF}
end;

procedure TAdvSpellCheckCorrectPanel.HandleIgnoreAll(Sender: TObject);
begin
  {$IFNDEF FMXLIB}
  DoIgnoreAll(lblErrorWord.Caption);
  {$ENDIF}
  {$IFDEF FMXLIB}
  DoIgnoreAll(lblErrorWord.Text);
  {$ENDIF}
end;

procedure TAdvSpellCheckCorrectPanel.HandleNextError(Sender: TObject);
begin
  DoNextError;
end;

procedure TAdvSpellCheckCorrectPanel.HandlePreviousError(Sender: TObject);
begin
  DoPreviousError;
end;

procedure TAdvSpellCheckCorrectPanel.DoAdd(NewWord: string);
begin
  if Assigned(OnAddWord) then
    OnAddWord(Self, NewWord);

  if Assigned(FSpellCheck) then
    FSpellCheck.AppendWords(FSpellCheck.ActiveLanguage, NewWord);
end;

procedure TAdvSpellCheckCorrectPanel.DoChangeWord(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(OnCorrectWord) then
    OnCorrectWord(Self, ErrorWord, CorrectWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectPanel.DoChangeWordAll(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(OnCorrectWordAll) then
    OnCorrectWordAll(Self, ErrorWord, CorrectWord);

  DoNextError;
end;

function TAdvSpellCheckCorrectPanel.DoGetCurrentError: string;
begin
  Result := '';
  if Assigned(OnGetErrorWord) then
    OnGetErrorWord(Self, Result);
end;

procedure TAdvSpellCheckCorrectPanel.DoIgnore(ErrorWord: string);
begin
  if Assigned(OnIgnoreWord) then
    OnIgnoreWord(Self, ErrorWord);

  if Assigned(FSpellCheck) then
    FSpellCheck.AddToIgnoreList(ErrorWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectPanel.DoIgnoreAll(ErrorWord: string);
begin
  if Assigned(OnIgnoreWordAll) then
    OnIgnoreWordAll(Self, ErrorWord);

  if Assigned(FSpellCheck) then
    FSpellCheck.AddToIgnoreList(ErrorWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectPanel.DoNextError;
begin
  if Assigned(OnNextError) then
    OnNextError(Self);

  DoUpdate;
end;

procedure TAdvSpellCheckCorrectPanel.DoPreviousError;
begin
  if Assigned(OnPreviousError) then
    OnPreviousError(Self);

  DoUpdate;
end;

{ TSpellCheckPanelUI }

procedure TSpellCheckPanelUI.Assign(Source: TPersistent);
begin
  if (Source is TSpellCheckPanelUI) then
  begin
    FShowIgnore := (Source as TSpellCheckPanelUI).ShowIgnore;
    FShowIgnoreAll := (Source as TSpellCheckPanelUI).ShowIgnoreAll;
    FShowAdd := (Source as TSpellCheckPanelUI).ShowAdd;
    FShowChange := (Source as TSpellCheckPanelUI).ShowChange;
    FShowChangeAll := (Source as TSpellCheckPanelUI).ShowChangeAll;
    FShowNext := (Source as TSpellCheckPanelUI).ShowNext;
    FShowPrevious := (Source as TSpellCheckPanelUI).ShowPrevious;

    FCaptionIgnore := (Source as TSpellCheckPanelUI).CaptionIgnore;
    FCaptionIgnoreAll := (Source as TSpellCheckPanelUI).CaptionIgnoreAll;
    FCaptionAdd := (Source as TSpellCheckPanelUI).CaptionAdd;

    FCaptionChange := (Source as TSpellCheckPanelUI).CaptionChange;
    FCaptionChangeAll := (Source as TSpellCheckPanelUI).CaptionChangeAll;
    FCaptionNext := (Source as TSpellCheckPanelUI).CaptionNext;
    FCaptionPrevious := (Source as TSpellCheckPanelUI).CaptionPrevious;

    FHintIgnore := (Source as TSpellCheckPanelUI).HintIgnore;
    FHintIgnoreAll := (Source as TSpellCheckPanelUI).HintIgnoreAll;
    FHintAdd := (Source as TSpellCheckPanelUI).HintAdd;

    FHintChange := (Source as TSpellCheckPanelUI).HintChange;
    FHintChangeAll := (Source as TSpellCheckPanelUI).HintChangeAll;
    FHintNext := (Source as TSpellCheckPanelUI).HintNext;
    FHintPrevious := (Source as TSpellCheckPanelUI).HintPrevious;

    FQueryAdd := (Source as TSpellCheckPanelUI).QueryAdd;
    FQueryWord := (Source as TSpellCheckPanelUI).QueryWord;

    DoChange;
  end;
end;

constructor TSpellCheckPanelUI.Create;
begin
  inherited Create;

  FCaptionIgnore := '&Ignore';
  FCaptionIgnoreAll := 'I&gnore all';
  FCaptionAdd := '&Add';
  FCaptionChange := '&Change';
  FCaptionChangeAll := 'C&hange all';
  FCaptionNext := '&Next';
  FCaptionPrevious := '&Previous';

  FHintIgnore := 'Ignore current spell check error';
  FHintIgnoreAll := 'Ignore all spell check error';
  FHintAdd := 'Add new word to word list';
  FHintChange := 'Change spelling of current word';
  FHintChangeAll := 'Change spelling of all words';
  FHintPrevious := 'Go to previous spell check error';
  FHintNext := 'Go to next spell check error';

  FQueryAdd := 'Add new word';
  FQueryWord := 'Word';

  FShowIgnoreAll := true;
  FShowIgnore := true;
  FShowAdd := true;
  FShowChange := true;
  FShowChangeAll := true;
  FShowNext := true;
  FShowPrevious := true;
end;

procedure TSpellCheckPanelUI.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TSpellCheckPanelUI.SetCaptionAdd(const Value: string);
begin
  FCaptionAdd := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetCaptionChange(const Value: string);
begin
  FCaptionChange := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetCaptionChangeAll(const Value: string);
begin
  FCaptionChangeAll := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetCaptionIgnore(const Value: string);
begin
  FCaptionIgnore := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetCaptionIgnoreAll(const Value: string);
begin
  FCaptionIgnoreAll := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetCaptionNext(const Value: string);
begin
  FCaptionNext := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetCaptionPrevious(const Value: string);
begin
  FCaptionPrevious := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowAdd(const Value: boolean);
begin
  FShowAdd := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowChange(const Value: boolean);
begin
  FShowChange := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowChangeAll(const Value: boolean);
begin
  FShowChangeAll := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowIgnore(const Value: boolean);
begin
  FShowIgnore := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowIgnoreAll(const Value: boolean);
begin
  FShowIgnoreAll := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowNext(const Value: boolean);
begin
  FShowNext := Value;
  DoChange;
end;

procedure TSpellCheckPanelUI.SetShowPrevious(const Value: boolean);
begin
  FShowPrevious := Value;
  DoChange;
end;

end.


