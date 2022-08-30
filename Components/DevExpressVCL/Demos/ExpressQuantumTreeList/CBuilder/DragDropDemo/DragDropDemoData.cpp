//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DragDropDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma resource "*.dfm"
TDragDropDemoDataDM *DragDropDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TDragDropDemoDataDM::TDragDropDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void TDragDropDemoDataDM::SetParentValue(Variant AValue)
{
  if (cdsDepartments->State == dsEdit || cdsDepartments->State == dsInsert)
    cdsDepartments->FindField("ParentID")->Value = AValue;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoDataDM::DataModuleCreate(TObject *Sender)
{
  cdsDeptData->Open();
  cdsPersData->Open();
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoDataDM::cdsDepartmentsAfterPost(TDataSet *DataSet)
{
  cdsDepartments->ApplyUpdates(-1);
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoDataDM::cdsPersonsAfterPost(TDataSet *DataSet)
{
  cdsPersons->ApplyUpdates(-1);
}
//---------------------------------------------------------------------------


void __fastcall TDragDropDemoDataDM::cdsDeptDictAfterPost(TDataSet *DataSet)
{
  cdsDeptDict->ApplyUpdates(-1);
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoDataDM::cdsPersDictAfterPost(TDataSet *DataSet)
{
  cdsPersDict->ApplyUpdates(-1);
}
//---------------------------------------------------------------------------

