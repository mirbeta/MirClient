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

unit TMSSpellCheckCorrectLinesForm;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Variants, Classes, Types,
  TMSSpellCheck, TMSSpellParser, TMSSpellCheckCorrectForm
  {$IFNDEF FMXLIB}
  , Graphics, Controls, Forms, Dialogs, StdCtrls,  ExtCtrls, ComCtrls
  {$ENDIF}
  {$IFDEF FMXLIB}
  , FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.TMSBaseControl,
  FMX.ExtCtrls, FMX.ListBox, FMX.Layouts, FMX.Types, System.UITypes, FMX.TMSRichEditor,
  FMX.TMSScrollControl, System.UIConsts
  {$ENDIF}
  ;

type
  TAdvSpellCheckCorrectLinesPanel = class;

  TErrorWordEvent = procedure(Sender: TObject; var ErrorWord: string) of object;
  TCorrectWordEvent = procedure(Sender: TObject; ErrorWord, CorrectedWord: string) of object;
  TIgnoreWordEvent = procedure(Sender: TObject; ErrorWord: string) of object;
  TAddWordEvent = procedure(Sender: TObject; NewWord: string) of object;
  TCheckCompleteEvent = procedure(Sender: TObject; OriginalText, CorrectedText: string) of object;

  TSpellCheckLinesPanelUI = class(TSpellCheckPanelUI);

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  {$IFNDEF FMXLIB}
  TAdvSpellCheckCorrectLinesPanel = class(TCustomPanel)
  {$ENDIF}
  {$IFDEF FMXLIB}
  TAdvSpellCheckCorrectLinesPanel = class(TPanel)
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
    {$IFNDEF FMXLIB}
    reText: TRichEdit;
    {$ENDIF}
    {$IFDEF FMXLIB}
    reText: TTMSFMXRichEditor;
    {$ENDIF}
    FOnIgnoreWord: TIgnoreWordEvent;
    FOnCorrectWordAll: TCorrectWordEvent;
    FOnPreviousError: TNotifyEvent;
    FOnCorrectWord: TCorrectWordEvent;
    FOnIgnoreWordAll: TIgnoreWordEvent;
    FOnGetErrorWord: TErrorWordEvent;
    FOnNextError: TNotifyEvent;
    FErrors: TStringList;
    FWordIndex: integer;
    FOriginalText: string;
    FOnSpellCheckComplete: TCheckCompleteEvent;
    FUI: TSpellCheckLinesPanelUI;
    FOnAddWord: TAddWordEvent;
    procedure HandleNextError(Sender: TObject);
    procedure HandlePreviousError(Sender: TObject);
    procedure HandleChangeWord(Sender: TObject);
    procedure HandleChangeWordAll(Sender: TObject);
    procedure HandleIgnore(Sender: TObject);
    procedure HandleIgnoreAll(Sender: TObject);
    procedure HandleAdd(Sender: TObject);
    procedure UIChanged(Sender: TObject);
    procedure UpdateUI;
    procedure SetUI(const Value: TSpellCheckLinesPanelUI);
    function GetRichEditorText: string;
    procedure SetRichEditorText(const Value: string);
  protected
    procedure DoNextError; virtual;
    procedure DoPreviousError; virtual;
    function DoGetCurrentError: string; virtual;
    procedure DoChangeWord(ErrorWord, CorrectWord: string); virtual;
    procedure DoChangeWordAll(ErrorWord, CorrectWord: string); virtual;
    procedure DoIgnore(ErrorWord: string); virtual;
    procedure DoIgnoreAll(ErrorWord: string); virtual;
    procedure DoCheckComplete(OriginalText, CorrectedText: string); virtual;
    procedure DoAdd(NewWord: string); virtual;
    procedure DoUpdate;
    procedure DoInit;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    {$IFNDEF FMXLIB}
    procedure CreateWnd; override;
    {$ENDIF}
    property RichEditorText: string read GetRichEditorText write SetRichEditorText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(Text: string);
    function Text: string;
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
    property Enabled;
    property Font;
    {$ENDIF}
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
    property UI: TSpellCheckLinesPanelUI read FUI write SetUI;

    property Visible;

    property SpellCheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
    property OnNextError: TNotifyEvent read FOnNextError write FOnNextError;
    property OnPreviousError: TNotifyEvent read FOnPreviousError write FOnPreviousError;
    property OnGetErrorWord: TErrorWordEvent read FOnGetErrorWord write FOnGetErrorWord;
    property OnAddWord: TAddWordEvent read FOnAddWord write FOnAddWord;
    property OnCorrectWord: TCorrectWordEvent read FOnCorrectWord write FOnCorrectWord;
    property OnCorrectWordAll: TCorrectWordEvent read FOnCorrectWordAll write FOnCorrectWordAll;
    property OnIgnoreWord: TIgnoreWordEvent read FOnIgnoreWord write FOnIgnoreWord;
    property OnIgnoreWordAll: TIgnoreWordEvent read FOnIgnoreWordAll write FOnIgnoreWordAll;
    property OnSpellCheckComplete: TCheckCompleteEvent read FOnSpellCheckComplete write FOnSpellCheckComplete;
  end;

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TAdvSpellCheckCorrectLinesDialog = class(TComponent)
  private
    FSpellCheck: TAdvSpellCheck;
    FSpellCheckCorrectFrm: TForm;
    FSpellCheckCorrectPnl: TAdvSpellCheckCorrectLinesPanel;
    FOnNextError: TNotifyEvent;
    FOnPreviousError: TNotifyEvent;
    FOnGetErrorWord: TErrorWordEvent;
    FOnCorrectWordAll: TCorrectWordEvent;
    FOnCorrectWord: TCorrectWordEvent;
    FOnIgnoreWord: TIgnoreWordEvent;
    FOnIgnoreWordAll: TIgnoreWordEvent;
    FOnSpellCheckComplete: TCheckCompleteEvent;
    FModalResult: TModalResult;
    FUI: TSpellCheckLinesPanelUI;
    FCaption: string;
    FOnAddWord: TAddWordEvent;
    procedure HandleNextError(Sender: TObject);
    procedure HandlePreviousError(Sender: TObject);
    procedure HandleChangeWord(Sender: TObject; ErrorWord, CorrectedWord: string);
    procedure HandleChangeWordAll(Sender: TObject; ErrorWord, CorrectedWord: string);
    procedure HandleShow(Sender: TObject);
    procedure HandleIgnore(Sender: TObject; ErrorWord: string);
    procedure HandleIgnoreAll(Sender: TObject; ErrorWord: string);
    procedure HandleGetErrorWord(Sender: TObject; var ErrorWord: string);
    procedure HandleSpellCheckComplete(Sender: TObject; OriginalText, CorrectedText: string);
    procedure HandleCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure HandleAdd(Sender: TObject; NewWord: string);
    procedure SetUI(const Value: TSpellCheckLinesPanelUI);
  protected
    procedure DoNextError; virtual;
    procedure DoPreviousError; virtual;
    function DoGetCurrentError: string; virtual;
    procedure DoChangeWord(ErrorWord, CorrectWord: string); virtual;
    procedure DoChangeWordAll(ErrorWord, CorrectWord: string); virtual;
    procedure DoIgnore(ErrorWord: string); virtual;
    procedure DoIgnoreAll(ErrorWord: string); virtual;
    procedure DoGetErrorWord(var ErrorWord: string); virtual;
    procedure DoAdd(NewWord: string); virtual;
    procedure DoSpellCheckComplete(OriginalText, CorrectedText: string); virtual;

    procedure DoUpdate;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(var Lines: string): TModalResult; overload;
    function Execute(x,y: integer; var Lines: string): TModalResult; overload;
    function Execute(AControl: TControl; var Lines: string): TModalResult; overload;
  published
    property Caption: string read FCaption write FCaption;
    property SpellCheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
    property UI: TSpellCheckLinesPanelUI read FUI write SetUI;

    property OnNextError: TNotifyEvent read FOnNextError write FOnNextError;
    property OnPreviousError: TNotifyEvent read FOnPreviousError write FOnPreviousError;
    property OnGetErrorWord: TErrorWordEvent read FOnGetErrorWord write FOnGetErrorWord;
    property OnCorrectWord: TCorrectWordEvent read FOnCorrectWord write FOnCorrectWord;
    property OnCorrectWordAll: TCorrectWordEvent read FOnCorrectWordAll write FOnCorrectWordAll;
    property OnAddWord: TAddWordEvent read FOnAddWord write FOnAddWord;
    property OnIgnoreWord: TIgnoreWordEvent read FOnIgnoreWord write FOnIgnoreWord;
    property OnIgnoreWordAll: TIgnoreWordEvent read FOnIgnoreWordAll write FOnIgnoreWordAll;
    property OnSpellCheckComplete: TCheckCompleteEvent read FOnSpellCheckComplete write FOnSpellCheckComplete;
  end;

implementation

uses
  StrUtils;

//------------------------------------------------------------------------------

{ TAdvSpellCheckCorrectLinesPanel }

constructor TAdvSpellCheckCorrectLinesPanel.Create(AOwner: TComponent);
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
  Height := 390;
  {$IFNDEF FMXLIB}
  Caption := '';
  {$ENDIF}

  FUI := TSpellCheckLinesPanelUI.Create;
  FUI.OnChange := UIChanged;

  FErrors := TStringList.Create;

  btnIgnore := TButton.Create(Self);
  btnIgnoreAll := TButton.Create(Self);
  btnAdd := TButton.Create(Self);
  lstSuggestions := TListBox.Create(Self);
  btnChange := TButton.Create(Self);
  btnChangeAll := TButton.Create(Self);
  btnNext := TButton.Create(Self);
  btnPrevious := TButton.Create(Self);
  {$IFDEF FMXLIB}
  reText := TTMSFMXRichEditor.Create(Self);
  reText.Width := 266;
  reText.Height := 83;
  reText.ReadOnly := true;
  reText.Color := $FFF4F4F4;
  reText.BorderStyle := bsNone;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  reText := TRichEdit.Create(Self);
  {$ENDIF}


  InitControl(btnIgnore,16,90,0);
  InitControl(btnIgnoreAll,110,90,1);
  InitControl(btnAdd, 207, 90, 2);

  InitControl(lstSuggestions, 16, 121,3);

  lstSuggestions.Width := 266;
  lstSuggestions.Height := 153;
  lstSuggestions.Visible := true;

  InitControl(btnChange, 16, 280, 4);
  InitControl(btnChangeAll, 110, 280, 5);
  InitControl(btnPrevious,16,320,6);
  InitControl(btnNext,110,320,7);

  InitControl(reText, 16,4,-1);
  reText.Visible := true;

  UpdateUI;

  // event handlers
  btnPrevious.OnClick := HandlePreviousError;
  btnNext.OnClick := HandleNextError;
  btnChange.OnClick := HandleChangeWord;
  btnChangeAll.OnClick := HandleChangeWordAll;
  btnIgnore.OnClick := HandleIgnore;
  btnIgnoreAll.OnClick := HandleIgnoreAll;
  btnAdd.OnClick := HandleAdd;

  // tab order
  btnChange.TabOrder := 1;
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

{$IFNDEF FMXLIB}
procedure TAdvSpellCheckCorrectLinesPanel.CreateWnd;
begin
  inherited;

  reText.Left := 16;
  reText.Width := 266;
  reText.Height := 83;
  reText.Color := clBtnFace;
  reText.ReadOnly := true;
  reText.BorderStyle := bsNone;
end;
{$ENDIF}

destructor TAdvSpellCheckCorrectLinesPanel.Destroy;
begin
  FUI.Free;
  FErrors.Free;
  inherited;
end;

procedure TAdvSpellCheckCorrectLinesPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FSpellCheck) then
    FSpellCheck := nil;
end;

procedure TAdvSpellCheckCorrectLinesPanel.SetUI(
  const Value: TSpellCheckLinesPanelUI);
begin
  FUI.Assign(Value);
end;

function TAdvSpellCheckCorrectLinesPanel.GetRichEditorText: string;
begin
{$IFNDEF FMXLIB}
  Result := reText.Lines.Text;
{$ENDIF}
{$IFDEF FMXLIB}
  Result := reText.Text;
{$ENDIF}
end;

procedure TAdvSpellCheckCorrectLinesPanel.SetRichEditorText(const Value: string);
begin
{$IFNDEF FMXLIB}
  reText.Lines.Text := Value;
{$ENDIF}
{$IFDEF FMXLIB}
  reText.Clear;
  reText.AddText(Value);
{$ENDIF}
end;

function TAdvSpellCheckCorrectLinesPanel.Text: string;
begin
{$IFNDEF FMXLIB}
  Result := reText.Lines.Text;
{$ENDIF}
{$IFDEF FMXLIB}
  Result := reText.Text;
{$ENDIF}
end;

procedure TAdvSpellCheckCorrectLinesPanel.UIChanged(Sender: TObject);
begin
  UpdateUI;
end;

procedure TAdvSpellCheckCorrectLinesPanel.UpdateUI;
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

procedure TAdvSpellCheckCorrectLinesPanel.DoUpdate;
var
  s: string;
  offs: integer;

begin
  if not Assigned(FSpellCheck) then
    Exit;

  s := '';

  lstSuggestions.Items.Clear;

  if FErrors.Count > 0 then
  begin
    offs := integer(FErrors.Objects[FWordIndex]);
    s := FErrors.Strings[FWordIndex];

    if (s <> '') then
    begin
      RichEditorText := RichEditorText;

      reText.SelStart := offs;
      reText.SelLength := Length(s);
      {$IFNDEF FMXLIB}
      reText.SelAttributes.Style := [fsBold];
      reText.SelAttributes.Color := clRed;
      {$ENDIF}

      {$IFDEF FMXLIB}
      reText.SetSelectionAttribute(reText.Font, claRed);
      reText.SetSelectionBold(true);
      reText.ClearSelection;
      {$ENDIF}

      s := FSpellCheck.Suggestions(s);

      lstSuggestions.Items.Text := s;
      lstSuggestions.ItemIndex := 0;
    end
  end;
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandleAdd(Sender: TObject);
var
  v: string;
begin
  v := '';
  if InputQuery(UI.QueryAdd,UI.QueryWord,v) then
    DoAdd(v);
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandleChangeWord(Sender: TObject);
var
  idx: integer;
begin
  idx := lstSuggestions.ItemIndex;
  if (idx <> -1) then
    DoChangeWord(FErrors.Strings[FWordIndex], lstSuggestions.Items[idx]);
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandleChangeWordAll(Sender: TObject);
var
  idx: integer;
begin
  idx := lstSuggestions.ItemIndex;
  if idx <> -1 then
    DoChangeWordAll(FErrors.Strings[FWordIndex], lstSuggestions.Items[idx]);
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandleIgnore(Sender: TObject);
begin
  if FErrors.Count > 0 then
    DoIgnore(FErrors.Strings[FWordIndex]);
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandleIgnoreAll(Sender: TObject);
begin
  if FErrors.Count > 0 then
    DoIgnoreAll(FErrors.Strings[FWordIndex]);
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandleNextError(Sender: TObject);
begin
  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesPanel.HandlePreviousError(Sender: TObject);
begin
  DoPreviousError;
end;

procedure TAdvSpellCheckCorrectLinesPanel.Init(Text: string);
begin
  RichEditorText := Text;
  DoInit;
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoInit;
var
  s,t: string;
  wordlist: TStringList;
  res: boolean;
  i: integer;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create(TMSSPELLERRMSG);

  s := RichEditorText;

  wordlist := TStringList.Create;
  FErrors.Clear;

  try
    ParseStringToWords(s, wordlist);

    btnNext.Enabled := wordlist.Count > 0;
    btnPrevious.Enabled := wordlist.Count > 0;

    for i := 0 to wordlist.Count - 1 do
    begin
      t := LowerCase(wordlist.Strings[i]);
      res := FSpellCheck.Validate(t) <> TAdvWordValidationResult.wvrNotValidated;

      if not res then
        FErrors.AddObject(wordlist.Strings[i], wordlist.Objects[i]);
    end;
  finally
    wordlist.Free;
  end;

  FWordIndex := 0;
  DoUpdate;
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoAdd(NewWord: string);
begin
  if Assigned(OnAddWord) then
    OnAddWord(Self, NewWord);

  if Assigned(FSpellCheck) then
    FSpellCheck.AppendWords(FSpellCheck.ActiveLanguage, NewWord);
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoChangeWord(ErrorWord,
  CorrectWord: string);
var
  s: string;
  offs: integer;
begin
  if Assigned(OnCorrectWord) then
    OnCorrectWord(Self, ErrorWord, CorrectWord);


  s := RichEditorText;

  offs := integer(FErrors.Objects[FWordIndex]);

  s := Copy(s, 1, offs) + CorrectWord + Copy(s, offs + Length(ErrorWord) + 1, Length(s));

  RichEditorText := s;

  DoInit;
  DoUpdate;
//  DoNextError;

  if FErrors.Count = 0 then
    DoCheckComplete(FOriginalText, RichEditorText);
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoChangeWordAll(ErrorWord,
  CorrectWord: string);
var
  s: string;
begin
  if Assigned(OnCorrectWordAll) then
    OnCorrectWordAll(Self, ErrorWord, CorrectWord);

  s := RichEditorText;

  ReplaceStr(s, ErrorWord, CorrectWord);

  RichEditorText := s;

  DoInit;
  DoUpdate;
//  DoNextError;

  if FErrors.Count = 0 then
    DoCheckComplete(FOriginalText, RichEditorText);
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoCheckComplete(
  OriginalText, CorrectedText: string);
begin
  if Assigned(OnSpellCheckComplete) then
    OnSpellCheckComplete(Self, OriginalText, reText.Text);
end;

function TAdvSpellCheckCorrectLinesPanel.DoGetCurrentError: string;
begin
  Result := '';
  if Assigned(OnGetErrorWord) then
    OnGetErrorWord(Self, Result);
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoIgnore(ErrorWord: string);
begin
  if Assigned(OnIgnoreWord) then
    OnIgnoreWord(Self, ErrorWord);

  if Assigned(FSpellCheck) then
    FSpellCheck.AddToIgnoreList(ErrorWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoIgnoreAll(ErrorWord: string);
begin
  if Assigned(OnIgnoreWordAll) then
    OnIgnoreWordAll(Self, ErrorWord);

  if Assigned(FSpellCheck) then
    FSpellCheck.AddToIgnoreList(ErrorWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoNextError;
begin
  if Assigned(OnNextError) then
    OnNextError(Self);

  if FWordIndex < FErrors.Count - 1 then
    inc(FWordIndex)
  else
    FWordIndex := 0;

  DoUpdate;
end;

procedure TAdvSpellCheckCorrectLinesPanel.DoPreviousError;
begin
  if Assigned(OnPreviousError) then
    OnPreviousError(Self);

  if FWordIndex > 0 then
    dec(FWordIndex)
  else
    FWordIndex := FErrors.Count - 1;

  DoUpdate;
end;


{ TAdvSpellCheckCorrectLinesDialog }

constructor TAdvSpellCheckCorrectLinesDialog.Create(AOwner: TComponent);
begin
  inherited;
  FUI := TSpellCheckLinesPanelUI.Create;
  FCaption := 'Correct';
end;

destructor TAdvSpellCheckCorrectLinesDialog.Destroy;
begin
  FUI.Free;
  inherited;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoAdd(NewWord: string);
begin
  if Assigned(OnAddWord) then
    OnAddWord(Self, NewWord);
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoChangeWord(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(OnCorrectWord) then
    OnCorrectWord(Self, ErrorWord, CorrectWord);

//  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoChangeWordAll(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(OnCorrectWordAll) then
    OnCorrectWordAll(Self, ErrorWord, CorrectWord);

//  DoNextError;
end;

function TAdvSpellCheckCorrectLinesDialog.DoGetCurrentError: string;
begin
  Result := '';
  if Assigned(OnGetErrorWord) then
    OnGetErrorWord(Self, Result);
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoGetErrorWord(var ErrorWord: string);
begin
  ErrorWord := DoGetCurrentError;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoIgnore(ErrorWord: string);
begin
  if Assigned(OnIgnoreWord) then
    OnIgnoreWord(Self, ErrorWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoIgnoreAll(ErrorWord: string);
begin
  if Assigned(OnIgnoreWordAll) then
    OnIgnoreWordAll(Self, ErrorWord);

  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoNextError;
begin
  if Assigned(OnNextError) then
    OnNextError(Self);

  DoUpdate;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoPreviousError;
begin
  if Assigned(OnPreviousError) then
    OnPreviousError(Self);

  DoUpdate;
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoSpellCheckComplete(OriginalText,
  CorrectedText: string);
begin
  if Assigned(OnSpellCheckComplete) then
    OnSpellCheckComplete(Self, OriginalText, CorrectedText);
end;

function TAdvSpellCheckCorrectLinesDialog.Execute(x, y: integer; var Lines: string): TModalResult;
{$IFNDEF FMXLIB}
var
  i: integer;
{$ENDIF}
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create(TMSSPELLERRMSG);

  FModalResult := mrCancel;
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

    FSpellCheckCorrectFrm.Height := 390;
    FSpellCheckCorrectFrm.Width := 305;
    FSpellCheckCorrectFrm.OnCloseQuery := HandleCloseQuery;
    FSpellCheckCorrectFrm.Caption := Caption;

    FSpellCheckCorrectPnl := TAdvSpellCheckCorrectLinesPanel.Create(FSpellCheckCorrectFrm);
    FSpellCheckCorrectPnl.Parent := FSpellCheckCorrectFrm;
    {$IFNDEF FMXLIB}
    FSpellCheckCorrectPnl.Align := alClient;
    {$ENDIF}
    {$IFDEF FMXLIB}
    {$IFDEF DELPHIXE6_LVL}
    FSpellCheckCorrectPnl.Align := TAlignLayout.Client;
    {$ELSE}
    FSpellCheckCorrectPnl.Align := TAlignLayout.alClient;
    {$ENDIF}
    {$ENDIF}
    FSpellCheckCorrectPnl.Visible := true;
    FSpellCheckCorrectPnl.SpellCheck := SpellCheck;
    FSpellCheckCorrectPnl.UI.Assign(UI);

    FSpellCheckCorrectPnl.OnPreviousError := HandlePreviousError;
    FSpellCheckCorrectPnl.OnNextError := HandleNextError;
    FSpellCheckCorrectPnl.OnCorrectWord := HandleChangeWord;
    FSpellCheckCorrectPnl.OnCorrectWordAll := HandleChangeWordAll;
    FSpellCheckCorrectPnl.OnIgnoreWordAll := HandleIgnore;
    FSpellCheckCorrectPnl.OnIgnoreWordAll := HandleIgnoreAll;
    FSpellCheckCorrectPnl.OnGetErrorWord := HandleGetErrorWord;
    FSpellCheckCorrectPnl.OnSpellCheckComplete := HandleSpellCheckComplete;
    FSpellCheckCorrectPnl.OnAddWord := HandleAdd;

    FSpellCheckCorrectPnl.Init(Lines);

    if (x <> -1) and (y <> -1) then
    begin
      {$IFNDEF FMXLIB}
      FSpellCheckCorrectFrm.Position := poDesigned;
      {$ENDIF}
      FSpellCheckCorrectFrm.Left := x;
      FSpellCheckCorrectFrm.Top := y;
    end;

    FSpellCheckCorrectFrm.OnShow := HandleShow;

    Result := FSpellCheckCorrectFrm.ShowModal;

    Lines := '';

    {$IFNDEF FMXLIB}
    for i := 0 to FSpellCheckCorrectPnl.reText.Lines.Count - 1 do
    begin
      if i > 0 then
        Lines := Lines + #13#10;
      Lines := Lines + FSpellCheckCorrectPnl.reText.Lines[i];
    end;
    {$ENDIF}
    {$IFDEF FMXLIB}
    Lines := FSpellCheckCorrectPnl.reText.Text;
    {$ENDIF}

  finally
    FSpellCheckCorrectFrm.Free;
  end;
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleAdd(Sender: TObject;
  NewWord: string);
begin

end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleChangeWord(Sender: TObject; ErrorWord, CorrectedWord: string);
begin
  DoChangeWord(ErrorWord, CorrectedWord);
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleChangeWordAll(Sender: TObject; ErrorWord, CorrectedWord: string);
begin
  DoChangeWordAll(ErrorWord, CorrectedWord);
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := true;
  FSpellCheckCorrectFrm.ModalResult := FModalResult;
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleGetErrorWord(Sender: TObject;
  var ErrorWord: string);
begin
  DoGetErrorWord(ErrorWord);
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleIgnore(Sender: TObject; ErrorWord: string);
begin
  DoIgnore(ErrorWord);
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleIgnoreAll(Sender: TObject; ErrorWord: string);
begin
  DoIgnoreAll(ErrorWord);
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleNextError(Sender: TObject);
begin
  DoNextError;
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandlePreviousError(Sender: TObject);
begin
  DoPreviousError;
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleShow(Sender: TObject);
begin
  DoUpdate;
end;

procedure TAdvSpellCheckCorrectLinesDialog.HandleSpellCheckComplete(
  Sender: TObject; OriginalText, CorrectedText: string);
begin
  FModalResult := mrOK;
  FSpellCheckCorrectFrm.Close;
end;

procedure TAdvSpellCheckCorrectLinesDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FSpellCheck) then
    FSpellCheck := nil;
end;

procedure TAdvSpellCheckCorrectLinesDialog.SetUI(
  const Value: TSpellCheckLinesPanelUI);
begin
  FUI.Assign(Value);
end;

procedure TAdvSpellCheckCorrectLinesDialog.DoUpdate;
begin

end;

function TAdvSpellCheckCorrectLinesDialog.Execute(AControl: TControl;
  var Lines: string): TModalResult;
var
  pt: TPoint;
{$IFNDEF FMXLIB}
  mon: TMonitor;
{$ENDIF}
begin
  pt := Point(-1,-1);

  {$IFNDEF FMXLIB}
  if Assigned(AControl) and Assigned(AControl.Parent) then
  begin
    pt := Point(AControl.Left, AControl.Top + AControl.Height);
    pt := AControl.Parent.ClientToScreen(pt);
    pt.X := pt.X + 10;
    pt.Y := pt.Y + 10;

    mon := Screen.MonitorFromPoint(pt);

    if pt.Y + 390 > mon.BoundsRect.Bottom then
    begin
      pt := Point(AControl.Left, AControl.Top);
      pt := AControl.Parent.ClientToScreen(pt);
      pt.Y := pt.Y - 400;
      pt.X := pt.X + 10;
    end;
  end;
  {$ENDIF}

  Result := Execute(pt.X, pt.Y, Lines);
end;

function TAdvSpellCheckCorrectLinesDialog.Execute(var Lines: string): TModalResult;
begin
  Result := Execute(-1, -1, Lines);
end;

end.
