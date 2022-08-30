unit NotepadMainForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ImgList, ActnList, cxGraphics,
  dxRibbonForm, cxLookAndFeels, dxBar, cxControls, cxLookAndFeelPainters, dxStatusBar, dxRibbonStatusBar, dxTabbedMDI,
  cxRichEdit, NotepadChildForm, dxBarExtItems, cxFontNameComboBox, cxBarEditItem, cxDropDownEdit, dxRibbonGallery,
  cxPC, dxSkinChooserGallery, IniFiles, cxClasses, dxColorDialog;

type

  { TRecentDocumentsController }

  TRecentDocumentsController = class(TObject)
  protected
    procedure DoLoad(AConfig: TCustomIniFile); virtual;
    procedure DoSave(AConfig: TCustomIniFile); virtual;
  public
    procedure Add(const AFileName: string); virtual;
    procedure SetCurrentFileName(const AFileName: string); virtual;
    //
    procedure LoadFromIniFile(const AFileName: string);
    procedure SaveToIniFile(const AFileName: string);
  end;

  { TdxNotepadForm }

  TfrmNotepadMain = class(TdxRibbonForm)
    acAlignCenter: TAction;
    acAlignLeft: TAction;
    acAlignRight: TAction;
    acBold: TAction;
    acBullets: TAction;
    acClear: TAction;
    acCopy: TAction;
    acCut: TAction;
    acExit: TAction;
    acFind: TAction;
    acFont: TAction;
    acFontColor: TAction;
    acItalic: TAction;
    acNew: TAction;
    acOpen: TAction;
    acPaste: TAction;
    acPrint: TAction;
    acRedo: TAction;
    acReplace: TAction;
    acSave: TAction;
    acSaveAs: TAction;
    acSelectAll: TAction;
    acUnderline: TAction;
    acUndo: TAction;
    alActions: TActionList;
    bbCursorColumn: TdxBarButton;
    bbCursorLine: TdxBarButton;
    bbLocked: TdxBarButton;
    bbModified: TdxBarButton;
    beFontName: TcxBarEditItem;
    beFontSize: TcxBarEditItem;
    cxLargeImages: TcxImageList;
    cxLookAndFeelController: TcxLookAndFeelController;
    cxSmallImages: TcxImageList;
    dxBarManager: TdxBarManager;
    dxTabbedMDIManager1: TdxTabbedMDIManager;
    FindDialog: TFindDialog;
    FontDialog1: TFontDialog;
    OpenDialog: TOpenDialog;
    PrintDialog1: TPrintDialog;
    ReplaceDialog: TReplaceDialog;
    scgiLookAndFeel: TdxSkinChooserGalleryItem;
    ColorDialog1: TdxColorDialog;
    procedure acAlignExecute(Sender: TObject);
    procedure acBoldExecute(Sender: TObject);
    procedure acBulletsExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acFontColorExecute(Sender: TObject);
    procedure acFontExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acUnderlineExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure bbLockedClick(Sender: TObject);
    procedure beFontNameChange(Sender: TObject);
    procedure beFontSizeChange(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure scgiLookAndFeelSkinChanged(Sender: TObject; const ASkinName: string);
    procedure dxTabbedMDIManager1GetTabHint(Sender: TdxTabbedMDIManager;
      APage: TdxTabbedMDIPage; var AHint: string; var ACanShow: Boolean);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRecentDocumentsController: TRecentDocumentsController;
    function GetActiveChild: TfrmNotepadChild;
    function GetEditor: TcxRichEdit;
    procedure EditorAddToRecentListHandler(Sender: TObject; const AFileName: string);
    procedure EditorChangeHandler(Sender: TObject);
    procedure EditorUndoListChangeHandler(Sender: TObject);
    procedure MDIStateChanged(Sender: TObject; const AEventArgs);
    procedure SetSkin(ASkinItem: TdxSkinChooserGalleryGroupItem); virtual;
  protected
    FUpdatingControls: Integer;
    function CreateChildForm: TfrmNotepadChild; virtual;
    function CreateRecentDocumentsController: TRecentDocumentsController; virtual;
    procedure DoUpdateControls(AActiveChild: TfrmNotepadChild); virtual;
    procedure InitializeLookAndFeel; virtual;
    procedure UpdateControls;
    procedure UpdateUndoRelatedControls; virtual;
  public
    function CreateNewChild: TfrmNotepadChild;
    function FindChild(const AFileName: string): TfrmNotepadChild;
    function OpenFile(const AFileName: string): TfrmNotepadChild; overload;
    function OpenFile: Boolean; overload;
    //
    property ActiveChild: TfrmNotepadChild read GetActiveChild;
    property Editor: TcxRichEdit read GetEditor;
    property RecentDocumentsController: TRecentDocumentsController read FRecentDocumentsController;
  end;

implementation

uses
{$IFDEF EXPRESSSKINS}
  {$I dxSkins.inc}
  dxSkinsForm, dxSkinsdxBarPainter, dxSkinscxPCPainter,
{$ENDIF}
  RichEdit, ComCtrls, dxCoreGraphics;

{$R *.dfm}

const
  sRichEditFoundResultCaption = 'Information';
  sRichEditTextNotFound = 'The search text is not found.';
  sRichEditReplaceAllResult = 'Replaced %d occurrences.';

function GetRecentDocumentsFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'RecentDocuments.ini';
end;

function GetBarsLayoutFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'BarsLayout.ini';
end;

{ TRecentDocumentsController }

procedure TRecentDocumentsController.Add(const AFileName: string);
begin
  // do nothing
end;

procedure TRecentDocumentsController.SetCurrentFileName(const AFileName: string);
begin
  // do nothing
end;

procedure TRecentDocumentsController.LoadFromIniFile(const AFileName: string);
var
  AIniFile: TIniFile;
begin
  AIniFile := TIniFile.Create(AFileName);
  try
    DoLoad(AIniFile);
  finally
    AIniFile.Free;
  end;
end;

procedure TRecentDocumentsController.SaveToIniFile(const AFileName: string);
var
  AIniFile: TIniFile;
begin
  AIniFile := TIniFile.Create(AFileName);
  try
    DoSave(AIniFile);
  finally
    AIniFile.Free;
  end;
end;

procedure TRecentDocumentsController.DoLoad(AConfig: TCustomIniFile);
begin
  // do nothing
end;

procedure TRecentDocumentsController.DoSave(AConfig: TCustomIniFile);
begin
  // do nothing
end;

{ TfrmNotepadMain }

function TfrmNotepadMain.CreateNewChild: TfrmNotepadChild;
begin
  Result := CreateChildForm;
  Result.OnChanged := EditorChangeHandler;
  Result.OnAddToRecent := EditorAddToRecentListHandler;
  Result.OnUndoListChanged := EditorUndoListChangeHandler;
end;

function TfrmNotepadMain.CreateRecentDocumentsController: TRecentDocumentsController;
begin
  Result := TRecentDocumentsController.Create;
end;

procedure TfrmNotepadMain.DoUpdateControls(AActiveChild: TfrmNotepadChild);
var
  ACursorPos: TPoint;
begin
  acAlignCenter.Enabled := AActiveChild <> nil;
  acAlignLeft.Enabled := AActiveChild <> nil;
  acAlignRight.Enabled := AActiveChild <> nil;
  acBold.Enabled := AActiveChild <> nil;
  acBullets.Enabled := AActiveChild <> nil;
  acClear.Enabled := AActiveChild <> nil;
  acFind.Enabled := AActiveChild <> nil;
  acFont.Enabled := AActiveChild <> nil;
  acFont.Enabled := AActiveChild <> nil;
  acFontColor.Enabled := AActiveChild <> nil;
  acItalic.Enabled := AActiveChild <> nil;
  acPrint.Enabled := AActiveChild <> nil;
  acReplace.Enabled := AActiveChild <> nil;
  acSave.Enabled := (AActiveChild <> nil) and AActiveChild.CanSave;
  acSaveAs.Enabled := AActiveChild <> nil;
  acSelectAll.Enabled := AActiveChild <> nil;
  acUnderline.Enabled := AActiveChild <> nil;

  acCopy.Enabled := (AActiveChild <> nil) and (AActiveChild.Editor.SelLength > 0);
  acPaste.Enabled := (AActiveChild <> nil) and AActiveChild.CanPaste;
  acClear.Enabled := acCopy.Enabled and AActiveChild.CanEdit;
  acCut.Enabled := acCopy.Enabled and AActiveChild.CanEdit;

  beFontName.Enabled := AActiveChild <> nil;
  beFontSize.Enabled := AActiveChild <> nil;

  bbLocked.Visible := VisibleTodxBarVisible(AActiveChild <> nil);
  bbCursorColumn.Visible := VisibleTodxBarVisible(AActiveChild <> nil);
  bbCursorLine.Visible := VisibleTodxBarVisible(AActiveChild <> nil);
  bbModified.Visible := VisibleTodxBarVisible((AActiveChild <> nil) and AActiveChild.Modified);

  if AActiveChild <> nil then
  begin
    ACursorPos.Y := SendMessage(Editor.InnerControl.Handle, EM_LINEFROMCHAR, Editor.SelStart, 0);
    ACursorPos.X := Editor.SelStart - SendMessage(Editor.InnerControl.Handle, EM_LINEINDEX, ACursorPos.Y, 0);
    bbCursorLine.Caption := Format(' Line: %3d ', [1 + ACursorPos.Y]);
    bbCursorColumn.Caption := Format(' Position: %3d ', [1 + ACursorPos.X]);
    bbLocked.Down := AActiveChild.Locked;

    beFontSize.EditValue := IntToStr(Editor.SelAttributes.Size);
    beFontName.EditValue := Editor.SelAttributes.Name;

    acBold.Checked := fsBold in Editor.SelAttributes.Style;
    acItalic.Checked := fsItalic in Editor.SelAttributes.Style;
    acUnderline.Checked := fsUnderline in Editor.SelAttributes.Style;

    acBullets.Checked := Editor.Paragraph.Numbering <> nsNone;

    case Ord(Editor.Paragraph.Alignment) of
      0: acAlignLeft.Checked := True;
      1: acAlignRight.Checked := True;
      2: acAlignCenter.Checked := True;
    end;
  end
  else
  begin
    acAlignCenter.Checked := False;
    acAlignLeft.Checked := False;
    acAlignRight.Checked := False;

    acBold.Checked := False;
    acBullets.Checked := False;
    acItalic.Checked := False;
    acUnderline.Checked := False;
  end;

  if (ActiveChild <> nil) and ActiveChild.Locked then
    bbLocked.Hint := 'Editing protection: Read only. Click for editing.'
  else
    bbLocked.Hint := 'Editing protection: Writable. Click for read-only mode.';
end;

procedure TfrmNotepadMain.InitializeLookAndFeel;
begin
  cxLookAndFeelController.NativeStyle := False;
  cxLookAndFeelController.SkinName := 'DevExpressStyle';
  scgiLookAndFeel.SelectedSkinName := RootLookAndFeel.Painter.LookAndFeelName;
end;

procedure TfrmNotepadMain.UpdateControls;
begin
  Inc(FUpdatingControls);
  try
    DoUpdateControls(ActiveChild);
    UpdateUndoRelatedControls;
  finally
    Dec(FUpdatingControls);
  end;
end;

procedure TfrmNotepadMain.UpdateUndoRelatedControls;
begin
  acRedo.Enabled := (ActiveChild <> nil) and ActiveChild.UndoController.CanRedo;
  acUndo.Enabled := (ActiveChild <> nil) and ActiveChild.UndoController.CanUndo;
end;

function TfrmNotepadMain.GetActiveChild: TfrmNotepadChild;
begin
  if ActiveMDIChild <> nil then
    Result := ActiveMDIChild as TfrmNotepadChild
  else
    Result := nil;
end;

function TfrmNotepadMain.CreateChildForm: TfrmNotepadChild;
begin
  Result := TfrmNotepadChild.Create(Self);
end;

function TfrmNotepadMain.GetEditor: TcxRichEdit;
begin
  Result := ActiveChild.Editor;
end;

procedure TfrmNotepadMain.MDIStateChanged(Sender: TObject; const AEventArgs);
var
  AEventData: TdxBarMDIStateChangeEventData;
begin
  AEventData := TdxBarMDIStateChangeEventData(AEventArgs);
  if AEventData.Change = scChildActivated then
  begin
    if ActiveChild <> nil then
      RecentDocumentsController.SetCurrentFileName(ActiveChild.FileName);
    UpdateControls;
  end;
end;

function TfrmNotepadMain.OpenFile: Boolean;
begin
  OpenDialog.FileName := '';
  Result := OpenDialog.Execute;
  if Result then
    OpenFile(OpenDialog.FileName)
end;

function TfrmNotepadMain.OpenFile(const AFileName: string): TfrmNotepadChild;
begin
  Result := FindChild(AFileName);
  if Result = nil then
  begin
    Result := CreateNewChild;
    Result.OpenFile(AFileName);
  end;
  Result.Show;
end;

procedure TfrmNotepadMain.ReplaceDialogReplace(Sender: TObject);
var
  AOldSelStart: Integer;
  APrevSelStart: Integer;
  AReplacedCount: Integer;
  AReplaceDialog: TReplaceDialog;
  S: string;
begin
  AReplacedCount := 0;
  AReplaceDialog := TReplaceDialog(Sender);

  AOldSelStart := Editor.SelStart;
  if frReplaceAll in AReplaceDialog.Options then
    Screen.Cursor := crHourglass;

  repeat
    if Editor.SelLength > 0 then
    begin
      if (Editor.SelText = AReplaceDialog.FindText) or not (frMatchCase in AReplaceDialog.Options) and
        (AnsiUpperCase(Editor.SelText) = AnsiUpperCase(AReplaceDialog.FindText)) then
      begin
        Editor.SelText := AReplaceDialog.ReplaceText;
        Inc(AReplacedCount);
      end;
    end;
    APrevSelStart := Editor.SelStart;
    FindDialogFind(Sender);
  until not (frReplaceAll in AReplaceDialog.Options) or (Editor.SelStart = APrevSelStart) and (Editor.SelLength = 0);

  if frReplaceAll in AReplaceDialog.Options then
  begin
    Screen.Cursor := crDefault;
    if AReplacedCount = 0 then
      S := sRichEditTextNotFound
    else
    begin
      Editor.SelStart := AOldSelStart;
      S := Format(sRichEditReplaceAllResult, [AReplacedCount]);
    end;
    Application.MessageBox(PChar(S), sRichEditFoundResultCaption, MB_ICONINFORMATION);
  end;
end;

procedure TfrmNotepadMain.scgiLookAndFeelSkinChanged(Sender: TObject; const ASkinName: string);
begin
  SetSkin(scgiLookAndFeel.SelectedGroupItem);
end;

procedure TfrmNotepadMain.SetSkin(ASkinItem: TdxSkinChooserGalleryGroupItem);
begin
  ASkinItem.ApplyToRootLookAndFeel;
end;

procedure TfrmNotepadMain.EditorAddToRecentListHandler(Sender: TObject; const AFileName: string);
begin
  RecentDocumentsController.Add(AFileName);
  if Sender = ActiveMDIChild then
    RecentDocumentsController.SetCurrentFileName(ActiveChild.FileName);
end;

procedure TfrmNotepadMain.EditorChangeHandler(Sender: TObject);
begin
  if Sender = ActiveMDIChild then
    UpdateControls;
end;

procedure TfrmNotepadMain.EditorUndoListChangeHandler(Sender: TObject);
begin
  if Sender = ActiveMDIChild then
    UpdateUndoRelatedControls;
end;

function TfrmNotepadMain.FindChild(const AFileName: string): TfrmNotepadChild;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to MDIChildCount - 1 do
    if SameText(AFileName, TfrmNotepadChild(MDIChildren[I]).FileName) then
    begin
      Result := TfrmNotepadChild(MDIChildren[I]);
      Break;
    end;
end;

procedure TfrmNotepadMain.FindDialogFind(Sender: TObject);
var
  ACaretR, R, AIntersectR: TRect;
  AFindDialog: TFindDialog;
  AFindLength: Integer;
  AFlags: TSearchTypes;
  AFoundAt: Integer;
  AStartPos: Integer;
  P: TPoint;
begin
  AFindDialog := Sender as TFindDialog;

  AStartPos := Editor.SelStart + Editor.SelLength;
  AFindLength := Length(Editor.Text) - AStartPos;

  AFlags := [];
  if frMatchCase in AFindDialog.Options then
    Include(AFlags, stMatchCase);
  if frWholeWord in AFindDialog.Options then
    Include(AFlags, stWholeWord);

  Screen.Cursor := crHourglass;
  AFoundAt := Editor.FindText(AFindDialog.FindText, AStartPos, AFindLength, AFlags);
  if not (frReplaceAll in AFindDialog.Options) then
    Screen.Cursor := crDefault;

  if AFoundAt < 0 then
  begin
    if not (frReplaceAll in AFindDialog.Options) then
      Application.MessageBox(sRichEditTextNotFound, sRichEditFoundResultCaption, MB_ICONINFORMATION);
  end
  else
    if frReplaceAll in AFindDialog.Options then
    begin
      Editor.SelStart := AFoundAt;
      Editor.SelLength := Length(AFindDialog.FindText);
    end
    else
    begin
      Editor.SetFocus;
      Editor.SelStart := AFoundAt;
      Editor.SelLength := Length(AFindDialog.FindText);

      Windows.GetCaretPos(P);
      P := ClientToScreen(P);
      ACaretR := Rect(P.X, P.Y, P.X + 2, P.Y + 20);
      GetWindowRect(Handle, R);
      if IntersectRect(AIntersectR, ACaretR, R) then
      begin
        if P.Y < Screen.Height div 2 then
          AFindDialog.Top := P.Y + 40
        else
          AFindDialog.Top := P.Y - (R.Bottom - R.Top + 20);
      end;
    end;
end;

procedure TfrmNotepadMain.FormCreate(Sender: TObject);
begin
  FRecentDocumentsController := CreateRecentDocumentsController;
  FRecentDocumentsController.LoadFromIniFile(GetRecentDocumentsFileName);
  dxBarManager.LoadFromIniFile(GetBarsLayoutFileName);
{$IFDEF EXPRESSSKINS}
  TdxSkinController.Create(Self);
{$ENDIF}
  OpenDialog.Filter := RTFFilter;
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  InitializeLookAndFeel;
  UpdateControls;

  dxBarManager.MDIStateChangedHandlers.Add(MDIStateChanged);
end;

procedure TfrmNotepadMain.FormDestroy(Sender: TObject);
begin
  dxBarManager.SaveToIniFile(GetBarsLayoutFileName);
  FRecentDocumentsController.SaveToIniFile(GetRecentDocumentsFileName);
  FreeAndNil(FRecentDocumentsController);
end;

procedure TfrmNotepadMain.FormShow(Sender: TObject);
var
  ADefaultDocument: string;
begin
  ADefaultDocument := ExtractFilePath(Application.ExeName) + 'Document1.rtf';
  if FileExists(ADefaultDocument) then
    OpenFile(ADefaultDocument)
  else
    CreateNewChild;
end;

procedure TfrmNotepadMain.acAlignExecute(Sender: TObject);
begin
  if TAction(Sender).Checked then
    Editor.Paragraph.Alignment := TAlignment(TAction(Sender).Tag)
  else
    Editor.Paragraph.Alignment := taLeftJustify;
end;

procedure TfrmNotepadMain.acBoldExecute(Sender: TObject);
begin
  if acBold.Checked then
    Editor.SelAttributes.Style := Editor.SelAttributes.Style + [fsBold]
  else
    Editor.SelAttributes.Style := Editor.SelAttributes.Style - [fsBold];
end;

procedure TfrmNotepadMain.acBulletsExecute(Sender: TObject);
begin
  Editor.Paragraph.Numbering := TNumberingStyle(acBullets.Checked);
end;

procedure TfrmNotepadMain.acClearExecute(Sender: TObject);
begin
  Editor.ClearSelection;
end;

procedure TfrmNotepadMain.acCopyExecute(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TfrmNotepadMain.acCutExecute(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TfrmNotepadMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmNotepadMain.acFindExecute(Sender: TObject);
begin
  Editor.SelLength := 0;
  FindDialog.Execute;
end;

procedure TfrmNotepadMain.acFontColorExecute(Sender: TObject);
begin
  ColorDialog1.Color := dxColorToAlphaColor(Editor.SelAttributes.Color);
  if ColorDialog1.Execute then
    Editor.SelAttributes.Color := dxAlphaColorToColor(ColorDialog1.Color);
end;

procedure TfrmNotepadMain.acFontExecute(Sender: TObject);
begin
  FontDialog1.Font.Assign(Editor.SelAttributes);
  if FontDialog1.Execute then
    Editor.SelAttributes.Assign(FontDialog1.Font);
end;

procedure TfrmNotepadMain.acItalicExecute(Sender: TObject);
begin
  if acItalic.Checked then
    Editor.SelAttributes.Style := Editor.SelAttributes.Style + [fsItalic]
  else
    Editor.SelAttributes.Style := Editor.SelAttributes.Style - [fsItalic];
end;

procedure TfrmNotepadMain.acNewExecute(Sender: TObject);
begin
  CreateNewChild;
end;

procedure TfrmNotepadMain.acOpenExecute(Sender: TObject);
begin
  OpenFile;
end;

procedure TfrmNotepadMain.acPasteExecute(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TfrmNotepadMain.acPrintExecute(Sender: TObject);
begin
  if PrintDialog1.Execute then
    Editor.Print(ActiveChild.FileName);
end;

procedure TfrmNotepadMain.acRedoExecute(Sender: TObject);
begin
  ActiveChild.UndoController.Redo;
end;

procedure TfrmNotepadMain.acReplaceExecute(Sender: TObject);
begin
  Editor.SelLength := 0;
  ReplaceDialog.Execute;
end;

procedure TfrmNotepadMain.acSaveAsExecute(Sender: TObject);
begin
  ActiveChild.SaveFile(True);
end;

procedure TfrmNotepadMain.acSaveExecute(Sender: TObject);
begin
  ActiveChild.SaveFile(False);
end;

procedure TfrmNotepadMain.acSelectAllExecute(Sender: TObject);
begin
  Editor.SelectAll;
end;

procedure TfrmNotepadMain.acUnderlineExecute(Sender: TObject);
begin
  if acUnderline.Checked then
    Editor.SelAttributes.Style := Editor.SelAttributes.Style + [fsUnderline]
  else
    Editor.SelAttributes.Style := Editor.SelAttributes.Style - [fsUnderline];
end;

procedure TfrmNotepadMain.acUndoExecute(Sender: TObject);
begin
  ActiveChild.UndoController.Undo(1);
end;

procedure TfrmNotepadMain.bbLockedClick(Sender: TObject);
begin
  ActiveChild.Locked := bbLocked.Down;
  UpdateControls;
end;

procedure TfrmNotepadMain.beFontNameChange(Sender: TObject);
begin
  if FUpdatingControls = 0 then
    Editor.SelAttributes.Name := beFontName.EditValue;
end;

procedure TfrmNotepadMain.beFontSizeChange(Sender: TObject);
begin
  if FUpdatingControls = 0 then
    Editor.SelAttributes.Size := StrToInt(beFontSize.EditValue);
end;

procedure TfrmNotepadMain.dxTabbedMDIManager1GetTabHint(Sender: TdxTabbedMDIManager;
  APage: TdxTabbedMDIPage; var AHint: string; var ACanShow: Boolean);
begin
  AHint := TfrmNotepadChild(APage.MDIChild).FileName;
  ACanShow := AHint <> '';
end;

end.
