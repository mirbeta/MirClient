//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RichEditMasterDetailMailMerge.h"
//---------------------------------------------------------------------------
#pragma link "dxRichEdit.NativeApi"

#pragma package(smart_init)
#pragma resource "*.dfm"

TfrmRichEditMasterDetailMailMerge *frmRichEditMasterDetailMailMerge;
//---------------------------------------------------------------------------
__fastcall TfrmRichEditMasterDetailMailMerge::TfrmRichEditMasterDetailMailMerge(TComponent* Owner)
	: TfrmRichEditControlBase(Owner)
{
}

void TfrmRichEditMasterDetailMailMerge::CalculateMaxAndMin(Double &AMax, Double &AMin)
{
  TClientDataSet* ADataSet = new TClientDataSet(NULL);
  ADataSet->CloneCursor(cdsDetail, False);
  ADataSet->First();
  Variant AValue = ADataSet->FieldValues["UnitPrice"];
  AMax = AValue;
  AMin = AValue;
  while (!ADataSet->Eof)
  {
    ADataSet->Next();
    AValue = ADataSet->FieldValues["UnitPrice"];
    AMax = Max(AMax, AValue);
    AMin = Min(AMin, AValue);
  }
  delete ADataSet;
}

Integer TfrmRichEditMasterDetailMailMerge::GetID(const UnicodeString AValue)
{
  int Result;
  if (TryStrToInt(AValue, Result))
	return Result;
  return -1;
}

void __fastcall TfrmRichEditMasterDetailMailMerge::bMergeToNewDocumentClick(TObject *Sender)
{
  MergeToNewDocument();
  LayoutControlGroup_Root->ItemIndex = lgResultingDocument->Index;
}

void __fastcall TfrmRichEditMasterDetailMailMerge::DetailDocumentServerCalculateDocumentVariable(TObject *Sender, TdxCalculateDocumentVariableEventArgs *E)
{
  if (E->Arguments->Count == 0)
	return;
  int AProductId = GetID(E->Arguments->Items[0]->Value);
  if (AProductId == -1)
	return;
  if (E->VariableName == "UnitPrice")
  {
    Variant AValue = LookUp(cdsDetail, "ProductID", "UnitPrice", AProductId);
    E->Value = TValue::From(CurrToStrF(AValue, ffCurrency, 2));
    E->Handled = True;
  }
}

void TfrmRichEditMasterDetailMailMerge::FillTemplate()
{
  cdsTemplate->Append();
  cdsTemplate->Fields->Fields[0]->Value = 0;
  cdsTemplate->Post();
}

void TfrmRichEditMasterDetailMailMerge::InitDataBases()
{
  UnicodeString ACategoriesDatabaseName = "../../Data/Categories.cds";
  UnicodeString AProductsDatabaseName = "../../Data/Products.cds";
  UnicodeString AMasterDatabaseName = "../../Data/Master.cds";
  if (FileExists(ACategoriesDatabaseName))
	cdsCategories->LoadFromFile(ACategoriesDatabaseName);
  if (FileExists(AMasterDatabaseName))
	cdsMaster->LoadFromFile(AMasterDatabaseName);
  if (FileExists(AProductsDatabaseName))
	cdsDetail->LoadFromFile(AProductsDatabaseName);
  cdsTemplate->DisableControls();
  cdsMaster->DisableControls();
  cdsDetail->DisableControls();
  cdsCategories->DisableControls();
}

void TfrmRichEditMasterDetailMailMerge::InitDocuments()
{
  UnicodeString ADetailDocumentName = "../../Data/MasterDetailMailMergeDetail.rtf";
  UnicodeString AMasterDocumentName = "../../Data/MasterDetailMailMergeMaster.rtf";
  UnicodeString ATemplateDocumentName = "../../Data/MasterDetailMailMergeTemplate.rtf";
  if (FileExists(ATemplateDocumentName))
	recTemplate->Document->LoadDocument(ATemplateDocumentName, TdxRichEditDocumentFormat::Undefined);
  if (FileExists(AMasterDocumentName))
	recMaster->Document->LoadDocument(AMasterDocumentName, TdxRichEditDocumentFormat::Undefined);
  if (FileExists(ADetailDocumentName))
	recDetail->Document->LoadDocument(ADetailDocumentName, TdxRichEditDocumentFormat::Undefined);
  recTemplate->DocumentModelModified = False;
  recMaster->DocumentModelModified = False;
  recDetail->DocumentModelModified = False;
}

Variant TfrmRichEditMasterDetailMailMerge::LookUp(TClientDataSet* ASource,
  UnicodeString AKeyField, UnicodeString AField, Variant AKeyValue)
{
  TClientDataSet* ADataSet = new TClientDataSet(NULL);
  ADataSet->CloneCursor(ASource, True);
  Variant Result = ADataSet->Lookup(AKeyField, AKeyValue, AField);
  delete ADataSet;
  return Result;
}

void __fastcall TfrmRichEditMasterDetailMailMerge::LayoutControlGroup_RootTabChanged(TObject *Sender)
{
  bool AIsResultDocument = LayoutControlGroup_Root->ItemIndex == lgResultingDocument->Index;
  bmbMergeToNewDocument->Visible = !AIsResultDocument;
  if (AIsResultDocument)
    MergeToNewDocument();
}

void __fastcall TfrmRichEditMasterDetailMailMerge::MasterDocumentServerCalculateDocumentVariable(TObject *Sender,
	TdxCalculateDocumentVariableEventArgs *E)
{
  if (E->Arguments->Count == 0)
	return;
  int ACurrentCategoryID = GetID(E->Arguments->Items[0]->Value);
  if (ACurrentCategoryID == -1)
	return;
  cdsDetail->Filter = "CategoryID = " + IntToStr(ACurrentCategoryID);
  cdsDetail->Filtered = True;
  if (E->VariableName == "Products")
  {
	_di_IdxRichEditDocumentServer ADocumentServer = recDetail->CreateDocumentServer();
	_di_IdxRichEditMailMergeOptions AOptions = recDetail->CreateMailMergeOptions();
	AOptions->MergeMode = TdxRichEditMergeMode::JoinTables;
	ADocumentServer->AddCalculateDocumentVariableHandler(DetailDocumentServerCalculateDocumentVariable);
	_di_IdxRichEditDocument ADoc = ADocumentServer->Document;
	recDetail->MailMerge(AOptions, ADoc);
	ADocumentServer->RemoveCalculateDocumentVariableHandler(DetailDocumentServerCalculateDocumentVariable);
	E->Value = TValue::From(ADocumentServer);
	E->Handled = True;
  }
  Double AMax, AMin;
  CalculateMaxAndMin(AMax, AMin);
  if (E->VariableName == "ItemCount")
  {
    E->Value = TValue::From(cdsDetail->RecordCount);
    E->Handled = True;
  }
  if (E->VariableName == "LowestPrice")
  {
    E->Value = TValue::From(CurrToStrF(AMin, ffCurrency, 2));
    E->Handled = True;
  }
  if (E->VariableName == "HighestPrice")
  {
	E->Value = TValue::From(CurrToStrF(AMax, ffCurrency, 2));
	E->Handled = True;
  }
  if (E->VariableName == "TotalSales")
  {
    Variant AValue = LookUp(cdsMaster, "CategoryID", "TotalSales", ACurrentCategoryID);
    E->Value = TValue::From(CurrToStrF(AValue, ffCurrency, 2));
    E->Handled = True;
  }
}

void TfrmRichEditMasterDetailMailMerge::MergeToNewDocument()
{
  bool ANeedMerge = recTemplate->DocumentModelModified || recMaster->DocumentModelModified ||
	recDetail->DocumentModelModified;

  if (!ANeedMerge)
	return;
  _di_IdxRichEditDocument ADoc = recResultingDocument->Document;
  recTemplate->MailMerge(ADoc);
  recTemplate->DocumentModelModified = False;
  recMaster->DocumentModelModified = False;
  recDetail->DocumentModelModified = False;
}

void __fastcall TfrmRichEditMasterDetailMailMerge::ExitClick(TObject *Sender)
{
  Close();
}

void __fastcall TfrmRichEditMasterDetailMailMerge::FormShow(TObject *Sender)
{
  InitDocuments();
  InitDataBases();
  FillTemplate();
  MergeToNewDocument();
}

void __fastcall TfrmRichEditMasterDetailMailMerge::ResultingDocumentCalculateDocumentVariable(TObject *Sender,
	TdxCalculateDocumentVariableEventArgs *E)
{
  if (E->VariableName == "Categories")
  {
	_di_IdxRichEditDocumentServer ADocumentServer = recMaster->CreateDocumentServer();
	ADocumentServer->AddCalculateDocumentVariableHandler(MasterDocumentServerCalculateDocumentVariable);
	_di_IdxRichEditDocument ADoc = ADocumentServer->Document;
	recMaster->MailMerge(ADoc);
	ADocumentServer->RemoveCalculateDocumentVariableHandler(MasterDocumentServerCalculateDocumentVariable);
	E->Value = TValue::From(ADocumentServer);
	E->Handled = True;
  }
}

void __fastcall TfrmRichEditMasterDetailMailMerge::TemplateMailMergeStarted(TObject *Sender,
	const TdxMailMergeStartedEventArgs *Args)
{
   RegistrationDBUriStreamProvider(recMaster, cdsCategories, "CategoryID", "Picture", "dbimg://");
}
