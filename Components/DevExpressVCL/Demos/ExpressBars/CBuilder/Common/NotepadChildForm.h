//---------------------------------------------------------------------------

#ifndef NotepadChildFormH
#define NotepadChildFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "cxGraphics.hpp"
#include "cxControls.hpp"
#include "cxLookAndFeels.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "dxRibbonForm.hpp"
//---------------------------------------------------------------------------

const String sDefaultDocName = "New Document.rtf";
const String RTFFilter = "Rich Text Files (*.RTF)|*.RTF";
const String TXTFilter = "Plain text (*.TXT)|*.TXT";
const int UM_UPDATEUNDO = WM_USER + 1;

class TRichEditUndoController
{
private:
  TStringList *FActions;
  TcxRichEdit *FEditor;
  Boolean FIsLocked;
  int FLastMessageID;
protected:
  void PopUndo(void);
  void PushUndo(const String AAction);
public:
  __fastcall TRichEditUndoController(TcxRichEdit *AEditor);
  __fastcall ~TRichEditUndoController();
  void AddAction(int AActionID);
  void AnalyseMessage();
  Boolean CanUndo();
  Boolean CanRedo();
  void Redo();
  void Undo(int ACount);
  void Lock();
  void Unlock();

  __property TStringList *Actions = { read = FActions };
};

typedef void __fastcall (__closure *TNotepadAddToRecentList)(TObject *Sender, const String AFileName);

class TfrmNotepadChild : public TForm
{
__published:	// IDE-managed Components
  TBevel *bvSpacer4;
  TBevel *Bevel1;
  TBevel *Bevel2;
  TBevel *Bevel3;
  TcxRichEdit *Editor;
  TSaveDialog *SaveDialog;
  void __fastcall EditorPropertiesChange(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall EditorPropertiesSelectionChange(TObject *Sender);
private:	// User declarations
  String FFileName;
  Boolean FModified;
  TRichEditUndoController *FUndoController;

  TNotepadAddToRecentList FOnAddToRecent;
  TNotifyEvent FOnChanged;
  TNotifyEvent FOnUndoListChanged;

  Boolean GetCanEdit();
  Boolean GetCanPaste();
  Boolean GetCanSave();
  Boolean GetLocked();
  void SetLocked(const Boolean AValue);
protected:
  void DoAddToRecent(const String AFileName);
  void DoChanged();
  void DoUndoListChanged();

  Boolean CheckSaveChanges();
  int QuerySaveFile();

  void __fastcall UpdateCaption();
  void UpdateUndo(TMessage &AMessage);

  BEGIN_MESSAGE_MAP
  VCL_MESSAGE_HANDLER(UM_UPDATEUNDO, TMessage, UpdateUndo)
  END_MESSAGE_MAP(TForm)

public:		// User declarations
  __fastcall TfrmNotepadChild(TComponent* Owner);
  __fastcall ~TfrmNotepadChild();
  void OpenFile(const String AFileName);
  Boolean ExportAsPlainText();
  Boolean SaveFile(Boolean ASaveAs);

  __property Boolean CanEdit = {read = GetCanEdit};
  __property Boolean CanPaste = {read = GetCanPaste};
  __property Boolean CanSave = {read = GetCanSave};
  __property String FileName = {read = FFileName};
  __property Boolean Locked = {read = GetLocked, write = SetLocked};
  __property Boolean Modified = {read = FModified, write = FModified};
  __property TRichEditUndoController *UndoController = {read = FUndoController};
  //
  __property TNotepadAddToRecentList OnAddToRecent = {read = FOnAddToRecent, write = FOnAddToRecent};
  __property TNotifyEvent OnChanged = {read = FOnChanged, write = FOnChanged};
  __property TNotifyEvent OnUndoListChanged = {read = FOnUndoListChanged, write = FOnUndoListChanged};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmNotepadChild *frmNotepadChild;
//---------------------------------------------------------------------------
#endif
