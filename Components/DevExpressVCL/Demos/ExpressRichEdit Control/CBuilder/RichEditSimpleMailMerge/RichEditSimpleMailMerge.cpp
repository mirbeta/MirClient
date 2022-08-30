//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RichEditSimpleMailMerge.h"
//---------------------------------------------------------------------------
#pragma link "dxRichEdit.NativeApi"

#pragma package(smart_init)
#pragma link "dxRichEdit.Control.SpellChecker"
#pragma link "dxRichEdit.Dialogs.EventArgs"
#pragma link "dxRichEdit.NativeApi"
#pragma link "dxRichEdit.Types"
#pragma resource "*.dfm"

TfrmRichEditSimpleMailMerge *frmRichEditSimpleMailMerge;
//---------------------------------------------------------------------------
__fastcall TfrmRichEditSimpleMailMerge::TfrmRichEditSimpleMailMerge(TComponent* Owner)
	: TfrmRichEditControlBase(Owner)
{
}

void TfrmRichEditSimpleMailMerge::InitDataBase()
{
  UnicodeString ADatabaseName = "../../Data/Employees.cds";
  if (FileExists(ADatabaseName))
	cdsMail->LoadFromFile(ADatabaseName);
}

void TfrmRichEditSimpleMailMerge::InitDocument()
{
  UnicodeString ADocumentName = "../../Data/MailMerge.rtf";
  if (FileExists(ADocumentName))
	RichEditControl->Document->LoadDocument(ADocumentName, TdxRichEditDocumentFormat::Undefined);
}

void TfrmRichEditSimpleMailMerge::InitUriService()
{
  RegistrationDBUriStreamProvider(RichEditControl, cdsMail, "EmployeeID", "Photo", "dbimg://");
}

void __fastcall TfrmRichEditSimpleMailMerge::FormShow(TObject *Sender)
{
  InitDocument();
  InitDataBase();
  InitUriService();
  tvEmployees->Controller->FocusedRowIndex = 0;
  tvEmployees->Controller->FocusedRow->Expand(True);
}

void __fastcall TfrmRichEditSimpleMailMerge::bbExitClick(TObject *Sender)
{
  Close();
}

void __fastcall TfrmRichEditSimpleMailMerge::RichEditControlMailMergeGetTargetDocument(TObject *Sender,
	const TdxMailMergeGetTargetDocumentEventArgs *Args)
{
  TfrmRibbonRichEditMain *AForm = new TfrmRibbonRichEditMain(this);
  IdxRichEditDocument *document = AForm->RichEditControl->Document;
  TdxMailMergeGetTargetDocumentEventArgs *E = (TdxMailMergeGetTargetDocumentEventArgs*)Args;
  E->TargetDocument = document;
  AForm->Show();
}
