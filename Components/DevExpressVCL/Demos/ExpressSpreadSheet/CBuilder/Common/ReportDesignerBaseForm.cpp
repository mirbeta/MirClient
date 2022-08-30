//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ReportDesignerBaseForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "ReportPreviewUnit"
#pragma link "cxCheckBox"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxFilterControl"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxSplitter"
#pragma link "dxSpreadSheetCore"
#pragma link "dxSpreadSheetReportDesigner"
#pragma resource "*.dfm"
TfrmReportDesignerBase *frmReportDesignerBase;
//---------------------------------------------------------------------------
__fastcall TfrmReportDesignerBase::TfrmReportDesignerBase(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::AfterConstruction()
{
  TfmBaseForm::AfterConstruction();
  FilterChanged(NULL);
}

//---------------------------------------------------------------------------

TdxSpreadSheet* TfrmReportDesignerBase::GetSpreadSheet()
{
  return (TdxSpreadSheet*)ReportDesigner;
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::FormCreate(TObject *Sender)
{
  Initialize();
  WindowState = wsMaximized;
  ReportDesigner->FieldChooser->Site = cxgFieldChooserSite;
  ReportDesigner->FieldChooser->Show();
  Preview = NULL;
  TfmBaseForm::FormCreate(Sender);
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::FormDestroy(TObject *Sender)
{
  if (FilterFileName != "") {
	Filter->SaveToFile(FilterFileName);
	if (Preview != NULL) Preview->Release();
  }
}


void TfrmReportDesignerBase::Initialize()
{
  // todo: need override
  if (ReportDesigner->DataBinding->DataController->DataSet != NULL ){
	FilterFileName = "..\\..\\Data\\" + ReportDesigner->DataBinding->DataController->DataSet->Name;
	if (FilterFileName != "") {
	  FilterFileName = ChangeFileExt(FilterFileName, ".flt");
	}
  }
  if (FileExists(FilterFileName)) {
	Filter->LoadFromFile(FilterFileName);
	cbxUseFilter->Checked = Filter->FilterText != "";
  }
}

//---------------------------------------------------------------------------

void TfrmReportDesignerBase::LoadDataset(TdxMemData *ADataSet, const UnicodeString AFileName)
{
  TFileStream *AFileStream =  new TFileStream(AFileName, fmOpenRead);
  try {
	ADataSet->DisableControls();
	try {
	  for (int I = ADataSet->Fields->Count - 1; I == 1; I--) {
		if (ADataSet->Fields->Fields[I]->FieldKind == fkData) {
		  ADataSet->Fields->Fields[I]->Free();
		}
	  }
	  ADataSet->CreateFieldsFromStream(AFileStream);
	  AFileStream->Position = 0;
	  ADataSet->LoadFromStream(AFileStream);
	  ADataSet->Active = true;
	}
	__finally {
	  ADataSet->EnableControls();
	}
  }
  __finally {
	AFileStream->Free();
  }
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::miDesignViewClick(TObject *Sender)
{
  ReportDesigner->Options->DesignView = miDesignView->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::miPreviewClick(TObject *Sender)
{
  FilterChanged(Sender);
  if (Preview == NULL) Preview = new TfrmPreview(Application);
  Preview->ssResult->ClearAll();
  ReportDesigner->Build(Preview->ssResult);
  Preview->Show();
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::miRemoveClick(TObject *Sender)
{
  int ARow, AColumn;
  TdxSpreadSheetReportSection *ASection;

  ARow = ReportDesigner->ActiveSheetAsTable->Selection->FocusedRow;
  AColumn = ReportDesigner->ActiveSheetAsTable->Selection->FocusedColumn;
  //
  if (ReportDesigner->FindSectionByCell(ARow, AColumn, ASection))
	ReportDesigner->RemoveSection(ASection);
  ReportDesignerSelectionChanged(NULL);
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::OnSectionClick(TObject *Sender)
{
  TRect R;
  R = ReportDesigner->ActiveSheetAsTable->Selection->Area;
  switch (((TComponent*)Sender)->Tag){
	case 0:
	  ReportDesigner->SetHeaderSection(R);
	  return;
	case 1:
	  ReportDesigner->SetDetailSection(R, -1);
	  return;
	case 2:
	  ReportDesigner->SetFooterSection(R);
	  return;
	default: return;
  }
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::FilterChanged(TObject *Sender)
{
  if (cbxUseFilter->Checked) {
	Filter->ApplyFilter();
  }
  ReportDesigner->DataBinding->DataController->Filter->Active = cbxUseFilter->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::btnApplyClick(TObject *Sender)
{
  Filter->ApplyFilter();
  cbxUseFilter->Checked = (Filter->FilterText != "");
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::btnClearClick(TObject *Sender)
{
  Filter->Clear();
  cbxUseFilter->Checked = (Filter->FilterText != "");
}

//---------------------------------------------------------------------------

void __fastcall TfrmReportDesignerBase::ReportDesignerSelectionChanged(TObject *Sender)
{
  int ARow, AColumn;
  TdxSpreadSheetReportSection *ASection;

  ARow = ReportDesigner->ActiveSheetAsTable->Selection->FocusedRow;
  AColumn = ReportDesigner->ActiveSheetAsTable->Selection->FocusedColumn;
  //
  if (ReportDesigner->FindSectionByCell(ARow, AColumn, ASection)) {
	miHeader->Checked = (ASection->SectionType == rstHeader);
	miDetail->Checked = (ASection->SectionType == rstDetail);
	miFooter->Checked = (ASection->SectionType == rstFooter);
  }
  else {
	miHeader->Checked = false;
	miDetail->Checked = false;
	miFooter->Checked = false;
  }
}
//---------------------------------------------------------------------------
