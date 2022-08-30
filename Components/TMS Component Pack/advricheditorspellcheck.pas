{*************************************************************************}
{ TMS TAdvRichEditor Spell Check                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorSpellCheck;

interface

uses
  Classes, SysUtils, Forms, Dialogs, Menus, AdvRichEditor, AdvRichEditorBase,
  AdvToolBar, AdvRichEditorToolBar, AdvGlowButton,
  TMSSpellCheck, TMSSpellCheckCorrectForm, TMSSpellCheckConfForm, Windows;

type
  TSpellCheckAction = (spcNone, spcMarkError, spcAutoCorrect);

  TAdvRichEditorSpellCheck = class(TAdvSpellCheck)
  private
    FRichEditor: TAdvRichEditor;
    FSpellCheckAction: TSpellCheckAction;
    FContextMenuSuggestions: boolean;
    procedure SetRichEditor(const ARichEditor: TAdvRichEditor);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SpellCheckCallback(Sender: TObject; CallBackContext: TAdvSpellCheckCallbackContext);
    procedure DoAutoCorrectWord(Sender: TObject; var AWord: string; var Error: boolean);
    procedure DoContextCorrectWord(Sender: TObject; MousePos: TPoint; var AWord: string; AElement: TREElement; var Handled: boolean);
  public
    procedure CheckDocument;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ContextMenuSuggestions: boolean read FContextMenuSuggestions write FContextMenuSuggestions default true;
    property RichEditor: TAdvRichEditor read FRichEditor write SetRichEditor;
    property SpellCheckAction: TSpellCheckAction read FSpellCheckAction write FSpellCheckAction default spcNone;
  end;

  TAdvRichEditorSpellCheckDialog = class(TAdvSpellCheckCorrectDialog)
  private
    FRichEditor: TAdvRichEditor;
  protected
    procedure DoNextError; override;
    procedure DoPreviousError; override;
    function DoGetCurrentError: string; override;
    procedure DoChangeWord(ErrorWord, CorrectWord: string); override;
    procedure DoChangeWordAll(ErrorWord, CorrectWord: string); override;
    procedure DoIgnore(ErrorWord: string); override;
    procedure DoIgnoreAll(ErrorWord: string); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  published
    property RichEditor: TAdvRichEditor read FRichEditor write FRichEditor;
  end;

  TAdvRichEditorSpellCheckPanel = class(TAdvSpellCheckCorrectPanel)
  private
    FRichEditor: TAdvRichEditor;
  protected
    procedure DoNextError; override;
    procedure DoPreviousError; override;
    function DoGetCurrentError: string; override;
    procedure DoChangeWord(ErrorWord, CorrectWord: string); override;
    procedure DoChangeWordAll(ErrorWord, CorrectWord: string); override;
    procedure DoIgnore(ErrorWord: string); override;
    procedure DoIgnoreAll(ErrorWord: string); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  published
    property RichEditor: TAdvRichEditor read FRichEditor write FRichEditor;
  end;


  TAdvRichEditorHighlight = class
    ss: integer;
    sl: integer;
  end;

  TAdvRichEditorProofingHints = class(TPersistent)
  private
    FCheckDocumentTitle: string;
    FCheckDocumentContent: string;
    FClearSpellCheckTitle: string;
    FClearSpellCheckContent: string;
    FSelectLanguageTitle: string;
    FSelectLanguageContent: string;
    FConfigSpellCheckTitle: string;
    FConfigSpellCheckContent: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

  published
    property CheckDocumentTitle: string read FCheckDocumentTitle write FCheckDocumentTitle;
    property CheckDocumentContent: string read FCheckDocumentContent write FCheckDocumentContent;

    property ClearSpellCheckTitle: string read FClearSpellCheckTitle write FClearSpellCheckTitle;
    property ClearSpellCheckContent: string read FClearSpellCheckContent write FClearSpellCheckContent;

    property ConfigSpellCheckTitle: string read FConfigSpellCheckTitle write FConfigSpellCheckTitle;
    property ConfigSpellCheckContent: string read FConfigSpellCheckContent write FConfigSpellCheckContent;

    property SelectLanguageTitle: string read FSelectLanguageTitle write FSelectLanguageTitle;
    property SelectLanguageContent: string read FSelectLanguageContent write FSelectLanguageContent;
  end;

  TAdvRichEditorProofingCaptions = class(TPersistent)
  private
    FLanguage: string;
    FConfig: string;
    FOnChange: TNotifyEvent;
    FClear: string;
    FCheck: string;
    procedure SetCheck(const Value: string);
    procedure SetClear(const Value: string);
    procedure SetConfig(const Value: string);
    procedure SetLanguage(const Value: string);
  protected
    procedure DoChanged;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

  published
    property Check: string read FCheck write SetCheck;
    property Clear: string read FClear write SetClear;
    property Config: string read FConfig write SetConfig;
    property Language: string read FLanguage write SetLanguage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TAdvRichEditorToolBarProofingButton = (btCheckDocument, btClearSpellCheck, btConfigureSpellCheck, btSelectLanguage);

  TAdvRichEditorToolBarProofingButtons = set of TAdvRichEditorToolBarProofingButton;

  TAdvRichEditorProofingToolBar = class(TAdvRichEditorToolBar)
  private
    FHints: TAdvRichEditorProofingHints;
    FOptions: TAdvRichEditorToolBarProofingButtons;
    FSpellCheck: TAdvRichEditorSpellCheck;
    procedure SetHints(const Value: TAdvRichEditorProofingHints);
  protected
    procedure CheckDocument(Sender: TObject);
    procedure ConfigureSpellCheck(Sender: TObject);
    procedure SelectLanguage(Sender: TObject);
    procedure ClearSpellCheck(Sender: TObject);

    procedure SetOptions(Value: TAdvRichEditorToolBarProofingButtons);
    procedure UpdateButtons;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Hints: TAdvRichEditorProofingHints read FHints write SetHints;
    property Options: TAdvRichEditorToolBarProofingButtons read FOptions write SetOptions;
    property SpellCheck: TAdvRichEditorSpellCheck read FSpellCheck write FSpellCheck;
  end;


  TAdvRichEditorProofingRibbonToolBar = class(TAdvRichEditorToolBar)
  private
    FCheck, FClear, FConfig, FLanguage: TAdvGlowButton;
    FHints: TAdvRichEditorProofingHints;
    FSpellCheck: TAdvRichEditorSpellCheck;
    FOptions: TAdvRichEditorToolBarProofingButtons;
    FCaptions: TAdvRichEditorProofingCaptions;
    procedure SetHints(const Value: TAdvRichEditorProofingHints);
    procedure SetOptions(const Value: TAdvRichEditorToolBarProofingButtons);
    procedure SetCaptions(const Value: TAdvRichEditorProofingCaptions);
  protected
    procedure CheckDocument(Sender: TObject);
    procedure ConfigureSpellCheck(Sender: TObject);
    procedure SelectLanguage(Sender: TObject);
    procedure ClearSpellCheck(Sender: TObject);
    procedure UpdateButtons;
    procedure CaptionsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Captions: TAdvRichEditorProofingCaptions read FCaptions write SetCaptions;
    property Hints: TAdvRichEditorProofingHints read FHints write SetHints;
    property Options: TAdvRichEditorToolBarProofingButtons read FOptions write SetOptions;
    property SpellCheck: TAdvRichEditorSpellCheck read FSpellCheck write FSpellCheck;
  end;


procedure Register;

implementation

type
  TAdvRichEditorEx = class(TAdvRichEditor);


procedure Register;
begin
  RegisterComponents('TMS Edits',[TAdvRichEditorSpellCheck, TAdvRichEditorSpellCheckDialog, TAdvRichEditorSpellCheckPanel]);

  RegisterComponents('TMS Edits',[TAdvRichEditorProofingToolBar]);

  RegisterComponents('TMS Edits',[TAdvRichEditorProofingRibbonToolBar]);
end;

{ TAdvRichEditorSpellCheckDialog }

procedure TAdvRichEditorSpellCheckDialog.DoChangeWord(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.CorrectSelectedError(CorrectWord);
  inherited;
end;

procedure TAdvRichEditorSpellCheckDialog.DoChangeWordAll(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.CorrectAllErrors(ErrorWord, CorrectWord);
  inherited;
end;

function TAdvRichEditorSpellCheckDialog.DoGetCurrentError: string;
begin
  Result := FRichEditor.SelectedText;
end;

procedure TAdvRichEditorSpellCheckDialog.DoIgnore(ErrorWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.IgnoreSelectedError(ErrorWord);
  inherited;
end;

procedure TAdvRichEditorSpellCheckDialog.DoIgnoreAll(ErrorWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.IgnoreAllErrors(ErrorWord);
  inherited;
end;

procedure TAdvRichEditorSpellCheckDialog.DoNextError;
begin
  if Assigned(FRichEditor) then
    FRichEditor.SelectError(esNext);
  inherited;
end;

procedure TAdvRichEditorSpellCheckDialog.DoPreviousError;
begin
  if Assigned(FRichEditor) then
    FRichEditor.SelectError(esPrevious);
  inherited;
end;

procedure TAdvRichEditorSpellCheckDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FRichEditor) then
    FRichEditor := nil;
end;



{ TAdvRichEditorSpellCheckPanel }

procedure TAdvRichEditorSpellCheckPanel.DoChangeWord(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.CorrectSelectedError(CorrectWord);
  inherited;
end;

procedure TAdvRichEditorSpellCheckPanel.DoChangeWordAll(ErrorWord,
  CorrectWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.CorrectAllErrors(ErrorWord, CorrectWord);
  inherited;
end;

function TAdvRichEditorSpellCheckPanel.DoGetCurrentError: string;
begin
  Result := FRichEditor.SelectedText;
end;

procedure TAdvRichEditorSpellCheckPanel.DoIgnore(ErrorWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.IgnoreSelectedError(ErrorWord);
  inherited;
end;

procedure TAdvRichEditorSpellCheckPanel.DoIgnoreAll(ErrorWord: string);
begin
  if Assigned(FRichEditor) then
    FRichEditor.IgnoreAllErrors(ErrorWord);
  inherited;
end;

procedure TAdvRichEditorSpellCheckPanel.DoNextError;
begin
  if Assigned(FRichEditor) then
    FRichEditor.SelectError(esNext);
  inherited;
end;

procedure TAdvRichEditorSpellCheckPanel.DoPreviousError;
begin
  if Assigned(FRichEditor) then
    FRichEditor.SelectError(esPrevious);
  inherited;
end;

procedure TAdvRichEditorSpellCheckPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FRichEditor) then
    FRichEditor := nil;
end;

{ TAdvRichEditorSpellCheck }

procedure TAdvRichEditorSpellCheck.CheckDocument;
var
  s: string;
  hl: TAdvRichEditorHighlight;
begin
  if not Assigned(FRichEditor) then
    raise Exception.Create('No rich editor assigned.');

  CancelAllRequests('1');
  BeginRequest('1');

  s := FRichEditor.CheckFirstWord;
  repeat
   hl := TAdvRichEditorHighlight.Create;
   hl.ss := RichEditor.GetSelStart(RichEditor.CheckWord);
   hl.sl := RichEditor.GetSelLength(RichEditor.CheckWord);

   AddValidationRequest(s, hl, SpellCheckCallBack);

   s := FRichEditor.CheckNextWord;
  until s = '';

  EndRequest;
end;

constructor TAdvRichEditorSpellCheck.Create(AOwner: TComponent);
begin
  inherited;
  FSpellCheckAction := spcNone;
  FRichEditor := nil;
  FContextMenuSuggestions := true;
end;

destructor TAdvRichEditorSpellCheck.Destroy;
begin
  if Assigned(FRichEditor) then
    TAdvRichEditorEx(FRichEditor).OnAutoCorrectWord := nil;
  inherited;
end;

procedure TAdvRichEditorSpellCheck.DoAutoCorrectWord(Sender: TObject;
  var AWord: string; var Error: boolean);
var
  s: string;
begin
  case SpellCheckAction of
    spcNone: Exit;
    spcMarkError:
      begin
        if Validate(AWord) <> wvrValidated then
          Error := true;
      end;
    spcAutoCorrect:
      begin
        s := FirstSuggestion(AWord);
        if (s <> '') and (AWord <> s) then
          AWord := s;
      end;
  end;
end;

procedure TAdvRichEditorSpellCheck.DoContextCorrectWord(Sender: TObject;
  MousePos: TPoint; var AWord: string; AElement: TREElement; var Handled: boolean);
var
  sl: TStringList;
  popup: THandle;
  i: integer;
  pt: TPoint;

begin
  if not FContextMenuSuggestions then
    Exit;

  if (Aword <> '') and Assigned(AElement) then
  begin
    if (AElement is TTextElement) then
    begin
      if (AElement as TTextElement).Error then
      begin
        AWord := (AElement as TTextElement).Text;
        sl := TStringList.Create;
        sl.Text := Suggestions(AWord);

        if sl.Count > 0 then
        begin
          popup := CreatePopupMenu;

          for i := 0 to sl.Count - 1 do
          begin
            InsertMenu(popup, $FFFFFFFF, MF_BYPOSITION, i + 1, pchar(sl.Strings[i]));
          end;

          pt := FRichEditor.ClientToScreen(MousePos);

          i := integer(TrackPopupMenu(popup, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RETURNCMD, pt.X, pt.Y, 0, FRichEditor.Handle, nil));

          if i > 0 then
          begin
            AWord := sl.Strings[i - 1];

            (AElement as TTextElement).Text := AWord;
            (AElement as TTextElement).Error := false;

            TAdvRichEditorEx(FRichEditor).Refresh;
          end;

          DestroyMenu(popup);
        end;
        Handled := true;
      end;
    end;
  end;
end;

procedure TAdvRichEditorSpellCheck.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FRichEditor) and (AOperation = opRemove) then
    FRichEditor := nil;
end;

procedure TAdvRichEditorSpellCheck.SetRichEditor(
  const ARichEditor: TAdvRichEditor);
begin
  FRichEditor := ARichEditor;
  if Assigned(FRichEditor) then
  begin
    TAdvRichEditorEx(FRichEditor).OnAutoCorrectWord := DoAutoCorrectWord;
    TAdvRichEditorEx(FRichEditor).OnContextCorrectWord := DoContextCorrectWord;
  end;
end;

procedure TAdvRichEditorSpellCheck.SpellCheckCallback(Sender: TObject;
  CallBackContext: TAdvSpellCheckCallbackContext);
var
  hl: TAdvRichEditorHighlight;
begin
  if CallBackContext.ValidationResult <> wvrValidated then
  begin
    hl := TAdvRichEditorHighlight(Callbackcontext.Data);
    FRichEditor.SelStart := hl.ss;
    FRichEditor.SelLength := hl.sl;
    FRichEditor.SetSelectionError(true);
    FRichEditor.ClearSelection;
  end;

  TAdvRichEditorHighlight(Callbackcontext.Data).Free;
end;

{ TAdvRichEditorProofingToolBar }

procedure TAdvRichEditorProofingToolBar.CheckDocument(Sender: TObject);
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');

  FSpellCheck.CheckDocument;
end;

procedure TAdvRichEditorProofingToolBar.ClearSpellCheck(Sender: TObject);
begin
  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');
  RichEditor.ClearErrors;
end;

procedure TAdvRichEditorProofingToolBar.ConfigureSpellCheck(Sender: TObject);
var
  dlg: TAdvSpellCheckConfigDialog;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');

  dlg := TAdvSpellCheckConfigDialog.Create(Application);
  dlg.Spellcheck := FSpellCheck;
  try
    dlg.Execute;
  finally
    dlg.Free;
  end;
end;

constructor TAdvRichEditorProofingToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;

  ShowRightHandle := false;

  FHints := TAdvRichEditorProofingHints.Create;
  Options := [btCheckDocument, btClearSpellCheck, btConfigureSpellCheck, btSelectLanguage];

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBSPELLCHECK','CheckDocument');
  atb.OfficeHint.Title := Hints.CheckDocumentTitle;
  atb.OfficeHint.Notes.Text := Hints.CheckDocumentContent;
  atb.Tag := integer(btCheckDocument);
  atb.OnClick := CheckDocument;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBSPELLCLEAR','ClearSpellCheck');
  atb.OfficeHint.Title := Hints.ClearSpellCheckTitle;
  atb.OfficeHint.Notes.Text := Hints.ClearSpellCheckContent;
  atb.Tag := integer(btClearSpellCheck);
  atb.OnClick := ClearSpellCheck;

  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBCONFIG','ConfigureSpellCheck');
  atb.OfficeHint.Title := Hints.ConfigSpellCheckTitle;
  atb.OfficeHint.Notes.Text := Hints.ConfigSpellCheckContent;
  atb.Tag := integer(btConfigureSpellCheck);
  atb.OnClick := ConfigureSpellCheck;


  atb := AddButton(HInstance,nil, bsButton, 'TMSRETBLANGUAGES','SelectLanguage');
  atb.OfficeHint.Title := Hints.SelectLanguageTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectLanguageContent;
  atb.Tag := integer(btSelectLanguage);
  atb.OnClick := SelectLanguage;
end;

destructor TAdvRichEditorProofingToolBar.Destroy;
begin
  FHints.Destroy;
  inherited;
end;

procedure TAdvRichEditorProofingToolBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FSpellcheck) and (AOperation = opRemove) then
    FSpellCheck := nil;
end;

procedure TAdvRichEditorProofingToolBar.SelectLanguage(Sender: TObject);
var
  dlg: TAdvSpellCheckLanguageSelectDialog;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');
  dlg := TAdvSpellCheckLanguageSelectDialog.Create(Application);
  dlg.Spellcheck := FSpellCheck;
  try
    dlg.Execute;
  finally
    dlg.Free;
  end;
end;

procedure TAdvRichEditorProofingToolBar.SetHints(
  const Value: TAdvRichEditorProofingHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorProofingToolBar.SetOptions(
  Value: TAdvRichEditorToolBarProofingButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvRichEditorProofingToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvRichEditorToolBarProofingButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvRichEditorToolBarProofingButton) to High(TAdvRichEditorToolBarProofingButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

{ TAdvRichEditorProofingHints }

procedure TAdvRichEditorProofingHints.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorProofingHints) then
  begin
    FCheckDocumentContent := (Source as TAdvRichEditorProofingHints).CheckDocumentContent;
    FCheckDocumentTitle := (Source as TAdvRichEditorProofingHints).CheckDocumentTitle;

    FConfigSpellCheckContent := (Source as TAdvRichEditorProofingHints).ConfigSpellCheckContent;
    FConfigSpellCheckTitle := (Source as TAdvRichEditorProofingHints).ConfigSpellCheckTitle;

    FSelectLanguageTitle := (Source as TAdvRichEditorProofingHints).SelectLanguageTitle;
    FSelectLanguageContent := (Source as TAdvRichEditorProofingHints).SelectLanguageContent;
  end;
end;

constructor TAdvRichEditorProofingHints.Create;
begin
  inherited;

  FCheckDocumentTitle := 'Spelling';
  FCheckDocumentContent := 'Check the spelling of the current document';

  FClearSpellCheckTitle:= 'Clear';
  FClearSpellCheckContent := 'Clear all marked spell check errors';

  FConfigSpellCheckTitle := 'Configuration';
  FConfigSpellCheckContent := 'Configure options and language for the spell check engine';

  FSelectLanguageTitle := 'Language';
  FSelectLanguageContent := 'Select language for the current document';
end;

{ TAdvRichEditorProofingRibbonToolBar }

procedure TAdvRichEditorProofingRibbonToolBar.CaptionsChanged(Sender: TObject);
begin
  FCheck.Caption := Captions.Check;
  FClear.Caption := Captions.Clear;
  FConfig.Caption := Captions.Clear;
  FLanguage.Caption := Captions.Language;
end;

procedure TAdvRichEditorProofingRibbonToolBar.CheckDocument(Sender: TObject);
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');

  FSpellCheck.CheckDocument;
end;

procedure TAdvRichEditorProofingRibbonToolBar.ClearSpellCheck(Sender: TObject);
begin
  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');
  RichEditor.ClearErrors;
end;

procedure TAdvRichEditorProofingRibbonToolBar.ConfigureSpellCheck(
  Sender: TObject);
var
  dlg: TAdvSpellCheckConfigDialog;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');

  dlg := TAdvSpellCheckConfigDialog.Create(Application);
  dlg.Spellcheck := FSpellCheck;
  try
    dlg.Execute;
  finally
    dlg.Free;
  end;
end;

constructor TAdvRichEditorProofingRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;
  FHints := TAdvRichEditorProofingHints.Create;
  FCaptions := TAdvRichEditorProofingCaptions.Create;
  FCaptions.OnChange := CaptionsChanged;
  FOptions := [btCheckDocument, btClearSpellCheck, btConfigureSpellCheck, btSelectLanguage];

  Caption:= 'Proofing';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;
  ShowOptionIndicator := false;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBSPELLCHECKL', 'Check Document');
  atb.OnClick := CheckDocument;
  atb.Caption := Hints.CheckDocumentTitle;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.OfficeHint.Title := Hints.CheckDocumentTitle;
  atb.OfficeHint.Notes.Text := Hints.CheckDocumentContent;
  atb.Tag := integer(btCheckDocument);
  atb.Left := 2;
  atb.Top := 2;
  atb.Width := 48;
  atb.Height := 64;
  FCheck := atb;

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBSPELLCLEARL', 'ClearSpellCheck');
  atb.OnClick := ClearSpellCheck;
  atb.Caption := Hints.ClearSpellCheckTitle;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.OfficeHint.Title := Hints.ClearSpellCheckTitle;
  atb.OfficeHint.Notes.Text := Hints.ClearSpellCheckContent;
  atb.Tag := integer(btClearSpellCheck);
  atb.Left := 50;
  atb.Top := 2;
  atb.Width := 48;
  atb.Height := 64;
  FClear := atb;

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBCONFIGL','Config SpellCheck');
  atb.OnClick := ConfigureSpellCheck;
  atb.Caption := 'Config';
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  atb.OfficeHint.Title := Hints.ConfigSpellCheckTitle;
  atb.OfficeHint.Notes.Text := Hints.ConfigSpellCheckContent;
  atb.Tag := integer(btConfigureSpellCheck);
  atb.Left := 98;
  atb.Top := 2;
  atb.Width := 48;
  atb.Height := 64;
  FConfig := atb;

  atb := AddButton(HInstance, nil, bsButton, 'TMSRETBLANGUAGESL','Select Language');
  atb.OnClick := SelectLanguage;
  atb.Caption := Hints.SelectLanguageTitle;
  atb.ShowCaption := true;
  atb.Layout := blGlyphTopAdjusted;
  FLanguage := atb;

  atb.OfficeHint.Title := Hints.SelectLanguageTitle;
  atb.OfficeHint.Notes.Text := Hints.SelectLanguageContent;
  atb.Tag := integer(btSelectLanguage);
  atb.Left := 146;
  atb.Top := 2;
  atb.Width := 52;
  atb.Height := 64;

  Width := 199;
  Height := 85;
end;

destructor TAdvRichEditorProofingRibbonToolBar.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  inherited;
end;

procedure TAdvRichEditorProofingRibbonToolBar.Notification(
  AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AComponent = FSpellCheck) and (AOperation = opRemove) then
    FSpellCheck := nil;
end;

procedure TAdvRichEditorProofingRibbonToolBar.SelectLanguage(Sender: TObject);
var
  dlg: TAdvSpellCheckLanguageSelectDialog;
begin
  if not Assigned(FSpellCheck) then
    raise Exception.Create('No spell check engine assigned');
  dlg := TAdvSpellCheckLanguageSelectDialog.Create(Application);
  dlg.Spellcheck := FSpellCheck;
  try
    dlg.Execute;
  finally
    dlg.Free;
  end;
end;

procedure TAdvRichEditorProofingRibbonToolBar.SetCaptions(
  const Value: TAdvRichEditorProofingCaptions);
begin
  FCaptions := Value;
end;

procedure TAdvRichEditorProofingRibbonToolBar.SetHints(
  const Value: TAdvRichEditorProofingHints);
begin
  FHints.Assign(Value);
end;

procedure TAdvRichEditorProofingRibbonToolBar.SetOptions(
  const Value: TAdvRichEditorToolBarProofingButtons);
begin
  FOptions := Value;
  UpdateButtons;
end;

procedure TAdvRichEditorProofingRibbonToolBar.UpdateButtons;
var
  i: integer;
  j: TAdvRichEditorToolBarProofingButton;
begin
  for i := 0 to ControlCount - 1 do
  begin
    for j := Low(TAdvRichEditorToolBarProofingButton) to High(TAdvRichEditorToolBarProofingButton) do
    begin
      if Controls[i].Tag = integer(j) then
        Controls[i].Visible := j in Options;
    end;
  end;
end;

{ TAdvRichEditorProofingCaptions }

procedure TAdvRichEditorProofingCaptions.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorProofingCaptions) then
  begin
    FCheck := (Source as TAdvRichEditorProofingCaptions).Check;
    FClear := (Source as TAdvRichEditorProofingCaptions).Clear;
    FConfig := (Source as TAdvRichEditorProofingCaptions).Config;
    FLanguage := (Source as TAdvRichEditorProofingCaptions).Language;
  end;
end;

constructor TAdvRichEditorProofingCaptions.Create;
begin
  inherited;
  FCheck := 'Check';
  FClear := 'Clear';
  FConfig := 'Config';
  FLanguage := 'Language';
end;

procedure TAdvRichEditorProofingCaptions.DoChanged;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvRichEditorProofingCaptions.SetCheck(const Value: string);
begin
  if (FCheck <> Value) then
  begin
    FCheck := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorProofingCaptions.SetClear(const Value: string);
begin
  if (FClear <> Value) then
  begin
    FClear := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorProofingCaptions.SetConfig(const Value: string);
begin
  if (FConfig <> Value) then
  begin
    FConfig := Value;
    DoChanged;
  end;
end;

procedure TAdvRichEditorProofingCaptions.SetLanguage(const Value: string);
begin
  if (FLanguage <> Value) then
  begin
    FLanguage := Value;
    DoChanged;
  end;
end;

end.
