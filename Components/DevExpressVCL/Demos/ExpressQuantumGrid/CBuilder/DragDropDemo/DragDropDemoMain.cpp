//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DragDropDemoMain.h"
#include "DragDropDemoDictionaries.h"
#include "FilmsDemoData.h"
#include "AboutDemoForm.h"
#include "shellapi.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridCardView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxGridDBCardView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMemo"
#pragma resource "*.dfm"
TDragDropDemoMainForm *DragDropDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TDragDropDemoMainForm::TDragDropDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDragDropDemoMainForm::miShowDictionariesClick(TObject *Sender)
{
  DragDropDemoDictionariesForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::miTabPositionClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  Grid->RootLevelOptions->DetailTabsPosition =
    (TcxGridDetailTabsPosition)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

TcxCustomGridView* GetDragSourceGridView(TObject* Source)
{
  TcxGridSite* cxGridSite;
  if ((cxGridSite = dynamic_cast<TcxGridSite*> (((TDragControlObject*)Source)->Control))!= NULL)
    return(cxGridSite->GridView);
  else
    return(NULL);
}
//---------------------------------------------------------------------------

int GetMasterRecordFilmID(TcxCustomGridView* AView)
{
  return(((TcxDBDataRelation*)(AView->DataController->GetMasterRelation()))->GetMasterRecordID(AView->MasterGridRecordIndex));
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::cvPersonsDragOver(TObject *Sender, TObject *Source,
      int X, int Y, TDragState State, bool &Accept)
{
  Accept = ((TcxGridSite*)((TDragControlObject*)Source)->Control)->GridView ==
    DragDropDemoDictionariesForm->cvPersonsList;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tvCompaniesDragOver(TObject *Sender,
      TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
  Accept = ((TcxGridSite*)((TDragControlObject*)Source)->Control)->GridView == DragDropDemoDictionariesForm->tvCompaniesList;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::bvFilmsDragOver(TObject *Sender, TObject *Source,
      int X, int Y, TDragState State, bool &Accept)
{
  Accept = ((TcxGridSite*)((TDragControlObject*)Source)->Control)->GridView ==  DragDropDemoDictionariesForm->tvFilmsList;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::cvPersonsDragDrop(TObject *Sender, TObject *Source,
      int X, int Y)
{
  TcxCustomGridView* AGridView = GetDragSourceGridView(Source);
  if (AGridView == NULL) return;

  int FilmID = GetMasterRecordFilmID(((TcxGridSite*)Sender)->GridView);
  int PersonID = AGridView->DataController->Values[AGridView->DataController->FocusedRecordIndex][
	  DragDropDemoDictionariesForm->cvPersonsListID->Index];

  if (FilmsDemoDM->InsertPerson(FilmID, PersonID, 2))
	  SetFocus();
  else
	  MessageDlg("This person is already assigned to this film", mtInformation, TMsgDlgButtons() << mbOK, 0);
	}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tvCompaniesDragDrop(TObject *Sender,
      TObject *Source, int X, int Y)
{
  TcxCustomGridView* AGridView = GetDragSourceGridView(Source);
  if (AGridView == NULL) return;

  int FilmID = GetMasterRecordFilmID(((TcxGridSite*)Sender)->GridView);
  int CompanyID = AGridView->DataController->Values[AGridView->DataController->FocusedRecordIndex][
	  DragDropDemoDictionariesForm->tvCompaniesListID->Index];

  if (FilmsDemoDM->InsertCompany(FilmID, CompanyID))
	SetFocus();
  else
	MessageDlg("This company is already assigned to this film", mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::bvFilmsDragDrop(TObject *Sender, TObject *Source, int X, int Y)
{
  TcxCustomGridView* AGridView = GetDragSourceGridView(Source);
  if (AGridView == NULL) return;

  int FilmID = AGridView->DataController->Values[AGridView->DataController->FocusedRecordIndex][DragDropDemoDictionariesForm->tvFilmsListID->Index];

  if (FilmsDemoDM->InsertFilm(FilmID, Grid->ActiveLevel->Tag))
	SetFocus();
  else
	MessageDlg("This film already exists in this category", mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::GridRootLevelStylesGetTabStyle(
	TcxGridLevel* Sender, TcxGridLevel* ATabLevel, Cxstyles::TcxStyle* &AStyle)
{
	if (Grid->ActiveLevel == ATabLevel)
	{
		AStyle = cxStyle1;
	}
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::GridActiveTabChanged(TcxCustomGrid *Sender,
      TcxGridLevel *ALevel)
{
  if (ALevel->IsMaster)
	try
	{
		Grid->BeginUpdate();
		FilmsDemoDM->cdsGenres->Locate("ID", ALevel->Tag, TLocateOptions());

		ALevel->GridView = bvFilms;
		ALevel->Items[0]->GridView = cvPersons;
		ALevel->Items[1]->GridView = tvCompanies;
		ALevel->Items[2]->GridView = cvPhotos;
		bvFilms->DataController->ClearDetails();
    }
	__finally
	{
      	Grid->EndUpdate();
    }
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::FormCreate(TObject *Sender)
{
  	FilmsDemoDM->FilmsFiltered = False;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::FormShow(TObject *Sender)
{
  CreateLevels();
  if (Grid->Levels->Count > 0)
    GridActiveTabChanged(Grid, Grid->Levels->Items[0]);
  DragDropDemoDictionariesForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::CreateLevels()
{
  if (!FilmsDemoDM->cdsGenres->Active)
	FilmsDemoDM->cdsGenres->Open();
  FilmsDemoDM->cdsGenres->First();
  TcxGridLevel* Level;
  while (!FilmsDemoDM->cdsGenres->Eof)
  {
	Level = Grid->Levels->Add();
    Level->MaxDetailHeight = 600;
    Level->Caption = FilmsDemoDM->cdsGenresNAME->AsString;
    Level->Options->DetailTabsPosition = dtpTop;
	Level->Styles->Tab = cxStyle1;
    Level->Styles->TabsBackground = cxStyle1;
    Level->Add()->Caption = "People";
    Level->Add()->Caption = "Companies";
    Level->Add()->Caption = "Photos";
	Level->Tag = FilmsDemoDM->cdsGenresID->AsInteger;
	FilmsDemoDM->cdsGenres->Next();
  }
}
//---------------------------------------------------------------------------

