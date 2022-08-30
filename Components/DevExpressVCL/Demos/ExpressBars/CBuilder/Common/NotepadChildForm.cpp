//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "NotepadChildForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmNotepadChild *frmNotepadChild;

//---------------------------------------------------------------------------

__fastcall TRichEditUndoController::TRichEditUndoController(TcxRichEdit *AEditor)
{
  FEditor = AEditor;
  FActions = new TStringList;
}
//---------------------------------------------------------------------------

__fastcall TRichEditUndoController::~TRichEditUndoController()
{
  delete FActions;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::PopUndo(void)
{
  if (Actions->Count > 0)
	Actions->Delete(0);
}
//---------------------------------------------------------------------------

void TRichEditUndoController::PushUndo(const String AAction)
{
  Actions->Insert(0, AAction);
}
//---------------------------------------------------------------------------

void TRichEditUndoController::AddAction(int AActionID)
{
  String ARichEditAction[7] = {"Unknown", "Typing", "Delete", "Drag And Drop", "Cut", "Paste", "Color Change"};
  if ((AActionID != 6) || (FEditor->SelLength != 0))
	PushUndo(ARichEditAction[AActionID]);
  FLastMessageID = AActionID;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::AnalyseMessage()
{
  int AMessageID;
  if (FIsLocked)
	return;
  AMessageID = SendMessage(FEditor->InnerControl->Handle, EM_GETUNDONAME, 0, 0);
  if ((AMessageID > 1) || (AMessageID == 1) && (AMessageID != FLastMessageID))
	AddAction(AMessageID);
}
//---------------------------------------------------------------------------

Boolean TRichEditUndoController::CanUndo()
{
  return (SendMessage(FEditor->InnerControl->Handle, EM_CANUNDO, 0, 0) != 0) && (Actions->Count > 0);
}
//---------------------------------------------------------------------------

Boolean TRichEditUndoController::CanRedo()
{
  return SendMessage(FEditor->InnerControl->Handle, EM_CANREDO, 0, 0) != 0;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::Redo()
{
  SendMessage(FEditor->InnerControl->Handle, EM_REDO, 0, 0);
}
//---------------------------------------------------------------------------

void TRichEditUndoController::Undo(int ACount)
{
  Lock();
  try
  {
	while (ACount > 0)
	{
	  if (CanUndo())
	  {
		PopUndo();
		SendMessage(FEditor->InnerControl->Handle, EM_UNDO, 0, 0);
	  }
	  else
		break;

	  ACount--;
	}
  }
  __finally
  {
	Unlock();
  }
}
//---------------------------------------------------------------------------

void TRichEditUndoController::Lock()
{
  FIsLocked = True;
  FLastMessageID = 0;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::Unlock()
{
  FIsLocked = False;
}
//---------------------------------------------------------------------------

__fastcall TfrmNotepadChild::TfrmNotepadChild(TComponent* Owner): TForm(Owner)
{
}
//---------------------------------------------------------------------------

__fastcall TfrmNotepadChild::~TfrmNotepadChild()
{
  delete FUndoController;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadChild::EditorPropertiesChange(TObject *Sender)
{
  Modified = True;
  DoChanged();
  PostMessage(Handle, UM_UPDATEUNDO, 0, 0);
}

//---------------------------------------------------------------------------
Boolean TfrmNotepadChild::GetCanEdit()
{
  return !Locked;
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadChild::GetCanPaste()
{
  return SendMessage(Editor->InnerControl->Handle, EM_CANPASTE, 0, 0) != 0;
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadChild::GetCanSave()
{
  return Modified | (FileName == "");
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadChild::GetLocked()
{
  return Editor->Properties->ReadOnly;
}
//---------------------------------------------------------------------------

void TfrmNotepadChild::SetLocked(const Boolean AValue)
{
  if (Locked != AValue)
  {
	Editor->Properties->ReadOnly = AValue;
	DoChanged();
  }
}
//---------------------------------------------------------------------------

void TfrmNotepadChild::DoAddToRecent(const String AFileName)
{
  if (OnAddToRecent)
	OnAddToRecent(this, AFileName);;

}

//---------------------------------------------------------------------------

void TfrmNotepadChild::DoChanged()
{
  dxCallNotify(OnChanged, this);
}
//---------------------------------------------------------------------------

void TfrmNotepadChild::DoUndoListChanged()
{
  dxCallNotify(OnUndoListChanged, this);
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadChild::CheckSaveChanges()
{
  if (Modified)
	switch(QuerySaveFile())
	{
	  case ID_YES:
		return SaveFile(False);
	  case ID_CANCEL:
		return False;
	}
  return True;
}
//---------------------------------------------------------------------------

int TfrmNotepadChild::QuerySaveFile()
{
  const String FormatTextMessage = "Do you want to save the changes you made to '%s'?";
  TVarRec V[] = {Caption};
  PChar AMessage, ACaption;
  AMessage = (Application->Title).c_str();
  ACaption = Format(FormatTextMessage, V, 1).c_str();
  return Application->MessageBox(ACaption, AMessage, MB_ICONQUESTION | MB_YESNOCANCEL);
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadChild::UpdateCaption()
{
  if (FileName != "")
	Caption = ExtractFileName(FileName);
  else
	Caption = sDefaultDocName;
}
//---------------------------------------------------------------------------

void TfrmNotepadChild::UpdateUndo(TMessage &AMessage)
{
  UndoController->AnalyseMessage();
  DoUndoListChanged();
}
//---------------------------------------------------------------------------

void TfrmNotepadChild::OpenFile(const String AFileName)
{
  FFileName = AFileName;
  Editor->Lines->LoadFromFile(FileName);
  DoAddToRecent(FileName);
  FModified = False;
  UpdateCaption();
  DoChanged();
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadChild::ExportAsPlainText()
{
  Boolean AResult;
  SaveDialog->FileName = ChangeFileExt(ExtractFileName(FileName), "");
  SaveDialog->Filter = TXTFilter;
  AResult = SaveDialog->Execute();
  if (AResult)
  {
	Editor->ActiveProperties->PlainText = True;
	Editor->Lines->SaveToFile(SaveDialog->FileName);
	Editor->ActiveProperties->PlainText = False;
  }
  return AResult;
}
//---------------------------------------------------------------------------

Boolean TfrmNotepadChild::SaveFile(Boolean ASaveAs)
{
  Boolean AResult;
  AResult = !ASaveAs & (FileName != "");
  if (!AResult)
  {
	SaveDialog->FileName = ChangeFileExt(ExtractFileName(FileName), "");
	SaveDialog->Filter = RTFFilter;
	AResult = SaveDialog->Execute();
	if (AResult)
	{
	  FFileName = SaveDialog->FileName;
	  DoAddToRecent(FileName);
	  UpdateCaption();
	}
  }

  if (AResult)
  {
	Editor->Lines->SaveToFile(FileName);
	FModified = False;
	DoChanged();
  }
  return AResult;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadChild::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadChild::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = CheckSaveChanges();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadChild::FormCreate(TObject *Sender)
{
  FUndoController = new TRichEditUndoController(Editor);
  SaveDialog->InitialDir = ExtractFilePath(Application->ExeName);
  UpdateCaption();
}
//---------------------------------------------------------------------------

void __fastcall TfrmNotepadChild::EditorPropertiesSelectionChange(TObject *Sender)
{
  DoChanged();
}
//---------------------------------------------------------------------------

