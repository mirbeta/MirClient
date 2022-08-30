//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "NotepadMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxBarEditItem"
#pragma link "cxDropDownEdit"
#pragma link "cxFontNameComboBox"
#pragma link "cxPC"
#pragma link "dxBar"
#pragma link "dxRibbonGallery"
#pragma link "dxSkinChooserGallery"
#pragma link "dxTabbedMDI"
#pragma link "dxColorDialog"
#pragma link "dxCoreGraphics"
#pragma link "cxImageList"
#pragma link "dxBarBuiltInMenu"
#pragma resource "*.dfm"
TfrmNotepadMain *frmNotepadMain;

const String sRichEditFoundResultCaption = "Information";
const String sRichEditTextNotFound = "The search text is not found.";
const String sRichEditReplaceAllResult = "Replaced %d occurrences.";

int __fastcall Round(double const AVal)
{
  int res;
  if (AVal > 0)
	res = AVal + 0.5;
  else
	res = AVal - 0.5;
  return res;
}

String GetBarsLayoutFileName()
{
  return ExtractFilePath(Application->ExeName) + "BarsLayout.ini";
}

String GetRecentDocumentsFileName()
{
  return ExtractFilePath(Application->ExeName) + "RecentDocuments.ini";
}

//---------------------------------------------------------------------------

void TRecentDocumentsController::DoLoad(TCustomIniFile *AConfig)
{
  // do nothing
}
//---------------------------------------------------------------------------

void TRecentDocumentsController::DoSave(TCustomIniFile *AConfig)
{
  // do nothing
}
//---------------------------------------------------------------------------

void TRecentDocumentsController::Add(const String AFileName)
{
  // do nothing
}
//---------------------------------------------------------------------------

void TRecentDocumentsController::SetCurrentFileName(const String AFileName)
{
  // do nothing
}

//---------------------------------------------------------------------------
void TRecentDocumentsController::LoadFromIniFile(const String AFileName)
{
  TIniFile *AIniFile;
  AIniFile = new TIniFile(AFileName);
  try
  {
	DoLoad(AIniFile);
  }
  __finally
  {
	delete AIniFile;
  }
}
//---------------------------------------------------------------------------

void TRecentDocumentsController::SaveToIniFile(const String AFileName)
{
  TIniFile *AIniFile;
  AIniFile = new TIniFile(AFileName);
  try
  {
	DoSave(AIniFile);
  }
  __finally
  {
	delete AIniFile;
  }
}
//---------------------------------------------------------------------------

__fastcall TfrmNotepadMain::TfrmNotepadMain(TComponent* Owner)
	: TdxRibbonForm(Owner)
{
}
//---------------------------------------------------------------------------

TfrmNotepadChild* TfrmNotepadMain::GetActiveChild()
{
  if (ActiveMDIChild != NULL)
	return (TfrmNotepadChild*)ActiveMDIChild;
  else
	return NULL;
}
//---------------------------------------------------------------------------

TcxRichEdit* TfrmNotepadMain::GetEditor()
{
  return ActiveChild->Editor;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::EditorAddToRecentListHandler(TObject *Sender, const String AFileName)
{
  RecentDocumentsController->Add(AFileName);
  if (Sender == ActiveMDIChild)
	RecentDocumentsController->SetCurrentFileName(ActiveChild->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::EditorChangeHandler(TObject *Sender)
{
  if (Sender == ActiveMDIChild)
	UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::EditorUndoListChangeHandler(TObject *Sender)
{
  if (Sender == ActiveMDIChild)
	UpdateUndoRelatedControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::MDIStateChanged(TObject *Sender, const void* AEventArgs)
{
  TdxBarMDIStateChangeEventData *AEventData;
  AEventData = (TdxBarMDIStateChangeEventData*)AEventArgs;
  if (AEventData->Change == scChildActivated)
  {
	if (ActiveChild != NULL)
	  RecentDocumentsController->SetCurrentFileName(ActiveChild->FileName);
	UpdateControls();
  }
}
//---------------------------------------------------------------------------

void TfrmNotepadMain::SetSkin(TdxSkinChooserGalleryGroupItem *ASkinItem)
{
  ASkinItem->ApplyToRootLookAndFeel();
}
//---------------------------------------------------------------------------

TfrmNotepadChild* TfrmNotepadMain::CreateChildForm()
{
  return new TfrmNotepadChild(this);
}
//---------------------------------------------------------------------------

TRecentDocumentsController* TfrmNotepadMain::CreateRecentDocumentsController()
{
  return new TRecentDocumentsController();
}
//---------------------------------------------------------------------------

void TfrmNotepadMain::DoUpdateControls(TfrmNotepadChild *AActiveChild)
{
  TPoint ACursorPos;
  acAlignCenter->Enabled = AActiveChild != NULL;
  acAlignLeft->Enabled = AActiveChild != NULL;
  acAlignRight->Enabled = AActiveChild != NULL;
  acBold->Enabled = AActiveChild != NULL;
  acBullets->Enabled = AActiveChild != NULL;
  acClear->Enabled = AActiveChild != NULL;
  acFind->Enabled = AActiveChild != NULL;
  acFont->Enabled = AActiveChild != NULL;
  acFont->Enabled = AActiveChild != NULL;
  acFontColor->Enabled = AActiveChild != NULL;
  acItalic->Enabled = AActiveChild != NULL;
  acPrint->Enabled = AActiveChild != NULL;
  acReplace->Enabled = AActiveChild != NULL;
  acSave->Enabled = AActiveChild != NULL && AActiveChild->CanSave;
  acSaveAs->Enabled = AActiveChild != NULL;
  acSelectAll->Enabled = AActiveChild != NULL;
  acUnderline->Enabled = AActiveChild != NULL;

  acCopy->Enabled = (AActiveChild != NULL) && (AActiveChild->Editor->SelLength > 0);
  acPaste->Enabled = AActiveChild != NULL && AActiveChild->CanPaste;
  acClear->Enabled = acCopy->Enabled && AActiveChild->CanEdit;
  acCut->Enabled = acCopy->Enabled && AActiveChild->CanEdit;

  beFontName->Enabled = AActiveChild != NULL;
  beFontSize->Enabled = AActiveChild != NULL;

  bbLocked->Visible = VisibleTodxBarVisible(AActiveChild != NULL);
  bbCursorColumn->Visible = VisibleTodxBarVisible(AActiveChild != NULL);
  bbCursorLine->Visible = VisibleTodxBarVisible(AActiveChild != NULL);
  bbModified->Visible = VisibleTodxBarVisible(AActiveChild != NULL && AActiveChild->Modified);

  if (AActiveChild != NULL)
  {
	ACursorPos.y = SendMessage(Editor->InnerControl->Handle, EM_LINEFROMCHAR, Editor->SelStart, 0);
	ACursorPos.x = Editor->SelStart - SendMessage(Editor->InnerControl->Handle, EM_LINEINDEX, ACursorPos.y, 0);
	TVarRec V[] = {1 + ACursorPos.y};
	String AFormatTextMessage = " Line: %3d ";
	bbCursorLine->Caption = Format(AFormatTextMessage, V, 1);
	V[0] = 1 + ACursorPos.x;
	AFormatTextMessage = " Position: %3d ";
	bbCursorColumn->Caption = Format(AFormatTextMessage, V, 1);
	bbLocked->Down = AActiveChild->Locked;

	beFontSize->EditValue = IntToStr(Editor->SelAttributes->Size);
	beFontName->EditValue = Editor->SelAttributes->Name;

	acBold->Checked = Editor->SelAttributes->Style.Contains(fsBold);
	acItalic->Checked = Editor->SelAttributes->Style.Contains(fsItalic);
	acUnderline->Checked = Editor->SelAttributes->Style.Contains(fsUnderline);

	acBullets->Checked = Editor->Paragraph->Numbering != nsNone;

	acAlignCenter->Checked = False;
	acAlignLeft->Checked = False;
	acAlignRight->Checked = False;
	switch (int (Editor->Paragraph->Alignment))
	{
	  case 0:
		acAlignLeft->Checked = True; break;
	  case 1:
		acAlignRight->Checked = True; break;
	  case 2:
		acAlignCenter->Checked = True; break;
	}
  }
  else
  {
	acAlignCenter->Checked = False;
	acAlignLeft->Checked = False;
	acAlignRight->Checked = False;

	acBold->Checked = False;
	acBullets->Checked = False;
	acItalic->Checked = False;
	acUnderline->Checked = False;
  }

  if ((ActiveChild != NULL) && ActiveChild->Locked)
	bbLocked->Hint = "Editing protection: Read only. Click for editing->";
  else
	bbLocked->Hint = "Editing protection: Writable. Click for read-only mode->";
}
//---------------------------------------------------------------------------

void TfrmNotepadMain::InitializeLookAndFeel()
{
  cxLookAndFeelController->NativeStyle = False;
  cxLookAndFeelController->SkinName = "DevExpressStyle";
  scgiLookAndFeel->SelectedSkinName = RootLookAndFeel()->Painter->LookAndFeelName();
}
//---------------------------------------------------------------------------

void TfrmNotepadMain::UpdateControls()
{
  FUpdatingControls++;
  try
  {
	DoUpdateControls(ActiveChild);
	UpdateUndoRelatedControls();
  }
  __finally
  {
	FUpdatingControls--;
  }
}
//---------------------------------------------------------------------------

void TfrmNotepadMain::UpdateUndoRelatedControls()
{
  acRedo->Enabled = ActiveChild != NULL && ActiveChild->UndoController->CanRedo();
  acUndo->Enabled = ActiveChild != NULL && ActiveChild->UndoController->CanUndo();
}
//---------------------------------------------------------------------------

TfrmNotepadChild* TfrmNotepadMain::CreateNewChild()
{
  TfrmNotepadChild* AResult;
  AResult = CreateChildForm();
  AResult->OnChanged = EditorChangeHandler;
  AResult->OnAddToRecent = EditorAddToRecentListHandler;
  AResult->OnUndoListChanged = EditorUndoListChangeHandler;
  return AResult;
}
//---------------------------------------------------------------------------

TfrmNotepadChild* TfrmNotepadMain::FindChild(const String AFileName)
{
  TForm *AForm;
  for (int I = 0; I <= MDIChildCount - 1; I++)
	if (SameText(AFileName, ((TfrmNotepadChild*)MDIChildren[I])->FileName))
	  return (TfrmNotepadChild*)MDIChildren[I];
  return NULL;
}
//---------------------------------------------------------------------------

TfrmNotepadChild* TfrmNotepadMain::OpenFile(const String AFileName)
{
  TfrmNotepadChild* AResult;
  AResult = FindChild(AFileName);
  if (AResult == NULL)
  {
	AResult = CreateNewChild();
	AResult->OpenFile(AFileName);
  }
  AResult->Show();
  return AResult;
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadMain::OpenFile()
{
  Boolean AResult;
  OpenDialog->FileName = "";
  AResult = OpenDialog->Execute();
  if (AResult != NULL)
	OpenFile(OpenDialog->FileName);
  return AResult;
}

void __fastcall TfrmNotepadMain::FormCreate(TObject *Sender)
{
  FRecentDocumentsController = CreateRecentDocumentsController();
  FRecentDocumentsController->LoadFromIniFile(GetRecentDocumentsFileName());
  dxBarManager->LoadFromIniFile(GetBarsLayoutFileName());
#ifdef EXPRESSSKINS
  TdxSkinController->Create(this);
#endif
  OpenDialog->Filter = RTFFilter;
  OpenDialog->InitialDir = ExtractFilePath(Application->ExeName);
  InitializeLookAndFeel();
  UpdateControls();
  dxBarManager->MDIStateChangedHandlers->Add(MDIStateChanged);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::FormDestroy(TObject *Sender)
{
  dxBarManager->SaveToIniFile(GetBarsLayoutFileName());
  FRecentDocumentsController->SaveToIniFile(GetRecentDocumentsFileName());
  delete FRecentDocumentsController;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::FormShow(TObject *Sender)
{
  String ADefaultDocument;
  ADefaultDocument = ExtractFilePath(Application->ExeName) + "Document1.rtf";
  if (FileExists(ADefaultDocument))
	OpenFile(ADefaultDocument);
  else
	CreateNewChild();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::dxTabbedMDIManager1GetTabHint(TdxTabbedMDIManager *Sender,
		  TdxTabbedMDIPage *APage, UnicodeString &AHint, bool &ACanShow)
{
  AHint = ((TfrmNotepadChild*)(APage->MDIChild))->FileName;
  ACanShow = AHint != "";
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::FindDialogFind(TObject *Sender)
{
  TRect ACaretR, R, AIntersectR;
  TFindDialog *AFindDialog;
  int AFindLength;
  TSearchTypes AFlags;
  int AFoundAt;
  int AStartPos;
  TPoint P;

  AFindDialog = (TFindDialog*)Sender;

  AStartPos = Editor->SelStart + Editor->SelLength;
  AFindLength = Editor->Text.Length() - AStartPos;

  AFlags.Clear();
  if (AFindDialog->Options.Contains(frMatchCase))
	AFlags = AFlags << stMatchCase;
  if (AFindDialog->Options.Contains(frWholeWord))
	AFlags = AFlags << stWholeWord;

  Screen->Cursor = crHourGlass;
  AFoundAt = Editor->FindTexT(AFindDialog->FindText, AStartPos, AFindLength, AFlags);
  if (!AFindDialog->Options.Contains(frReplaceAll))
	Screen->Cursor = crDefault;

  if (AFoundAt < 0)
  {
	if (!AFindDialog->Options.Contains(frReplaceAll))
	  Application->MessageBox(sRichEditTextNotFound.w_str(), sRichEditFoundResultCaption.w_str(), MB_ICONINFORMATION);
  }
  else
	if (AFindDialog->Options.Contains(frReplaceAll))
	{
	  Editor->SelStart = AFoundAt;
	  Editor->SelLength = AFindDialog->FindText.Length();
	}
	else
	{
	  Editor->SetFocus();
	  Editor->SelStart = AFoundAt;
	  Editor->SelLength = AFindDialog->FindText.Length();

	  GetCaretPos(&P);
	  P = ClientToScreen(P);
	  ACaretR = Rect(P.x, P.y, P.x + 2, P.y + 20);
	  GetWindowRect(Handle, &R);
	  if (IntersectRect(AIntersectR, ACaretR, R))
	  {
		if (P.y < div(Screen->Height, 2).quot)
		  AFindDialog->Top = P.y + 40;
		else
		  AFindDialog->Top = P.y - (R.Bottom - R.Top + 20);
	  }
	}
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::ReplaceDialogReplace(TObject *Sender)
{
  int AOldSelStart;
  int APrevSelStart;
  int AReplacedCount;
  TReplaceDialog *AReplaceDialog;
  String S;

  AReplacedCount = 0;
  AReplaceDialog = (TReplaceDialog*)Sender;

  AOldSelStart = Editor->SelStart;
  if (AReplaceDialog->Options.Contains(frReplaceAll))
	Screen->Cursor = crHourGlass;

  do
  {
	if (Editor->SelLength > 0)
	{
	  if (((Editor->SelText == AReplaceDialog->FindText) || !(AReplaceDialog->Options.Contains(frMatchCase))) &
		(AnsiUpperCase(Editor->SelText) == AnsiUpperCase(AReplaceDialog->FindText)))
	  {
		Editor->SelText = AReplaceDialog->ReplaceTextA;
		AReplacedCount++;
	  }
	}
	APrevSelStart = Editor->SelStart;
	FindDialogFind(Sender);
  }
  while (AReplaceDialog->Options.Contains(frReplaceAll) | (Editor->SelStart == APrevSelStart) & (Editor->SelLength == 0));

  if (AReplaceDialog->Options.Contains(frReplaceAll))
  {
	Screen->Cursor = crDefault;
	if (AReplacedCount == 0)
	  S = sRichEditTextNotFound;
	else
	{
	  Editor->SelStart = AOldSelStart;
	  TVarRec V[] = {AReplacedCount};
	  S = Format(sRichEditReplaceAllResult, V, 1);
	}
	Application->MessageBox(S.w_str(), sRichEditFoundResultCaption.w_str(), MB_ICONINFORMATION);
  }
}
//---------------------------------------------------------------------------



void __fastcall TfrmNotepadMain::acAlignExecute(TObject *Sender)
{
  Editor->Paragraph->Alignment = (TAlignment)(((TAction*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acBoldExecute(TObject *Sender)
{
  if (acBold->Checked)
	Editor->SelAttributes->Style = Editor->SelAttributes->Style << fsBold;
  else
	Editor->SelAttributes->Style = Editor->SelAttributes->Style >> fsBold;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acBulletsExecute(TObject *Sender)
{
  Editor->Paragraph->Numbering = (TNumberingStyle)(acBullets->Checked);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acClearExecute(TObject *Sender)
{
  Editor->ClearSelection();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acCopyExecute(TObject *Sender)
{
  Editor->CopyToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acCutExecute(TObject *Sender)
{
  Editor->CutToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acFindExecute(TObject *Sender)
{
  Editor->SelLength = 0;
  FindDialog->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acFontExecute(TObject *Sender)
{
  FontDialog1->Font->Assign(Editor->SelAttributes);
  if (FontDialog1->Execute())
	Editor->SelAttributes->Assign(FontDialog1->Font);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acFontColorExecute(TObject *Sender)
{
  ColorDialog1->Color = dxColorToAlphaColor(Editor->SelAttributes->Color);
  if (ColorDialog1->Execute())
	Editor->SelAttributes->Color = dxAlphaColorToColor(ColorDialog1->Color);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acItalicExecute(TObject *Sender)
{
  if (acItalic->Checked)
	Editor->SelAttributes->Style = Editor->SelAttributes->Style << fsItalic;
  else
	Editor->SelAttributes->Style = Editor->SelAttributes->Style >> fsItalic;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acNewExecute(TObject *Sender)
{
  CreateNewChild();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acOpenExecute(TObject *Sender)
{
  OpenFile();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acPasteExecute(TObject *Sender)
{
  Editor->PasteFromClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acPrintExecute(TObject *Sender)
{
  if (PrintDialog1->Execute())
	Editor->Print(ActiveChild->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acRedoExecute(TObject *Sender)
{
  ActiveChild->UndoController->Redo();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acReplaceExecute(TObject *Sender)
{
  Editor->SelLength = 0;
  ReplaceDialog->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acSaveExecute(TObject *Sender)
{
  ActiveChild->SaveFile(False);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acSaveAsExecute(TObject *Sender)
{
  ActiveChild->SaveFile(True);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acSelectAllExecute(TObject *Sender)
{
  Editor->SelectAll();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acUnderlineExecute(TObject *Sender)
{
  if (acUnderline->Checked)
	Editor->SelAttributes->Style = Editor->SelAttributes->Style << fsUnderline;
  else
	Editor->SelAttributes->Style = Editor->SelAttributes->Style >> fsUnderline;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::acUndoExecute(TObject *Sender)
{
  ActiveChild->UndoController->Undo(1);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::bbLockedClick(TObject *Sender)
{
  ActiveChild->Locked = bbLocked->Down;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::beFontNameChange(TObject *Sender)
{
  if (FUpdatingControls == 0)
	Editor->SelAttributes->Name = beFontName->EditValue;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::beFontSizeChange(TObject *Sender)
{
  if (FUpdatingControls == 0)
	Editor->SelAttributes->Size = StrToInt(beFontSize->EditValue);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadMain::scgiLookAndFeelSkinChanged(TObject *Sender, const UnicodeString ASkinName)
{
  SetSkin(scgiLookAndFeel->SelectedGroupItem);
}
//---------------------------------------------------------------------------

