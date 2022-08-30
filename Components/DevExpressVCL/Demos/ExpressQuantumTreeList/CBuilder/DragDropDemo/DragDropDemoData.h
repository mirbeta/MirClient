//---------------------------------------------------------------------------

#ifndef DragDropDemoDataH
#define DragDropDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxTL.hpp"
#include <DBClient.hpp>
#include <Provider.hpp>
//---------------------------------------------------------------------------
class TDragDropDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TClientDataSet *cdsDepartments;
  TAutoIncField *cdsDepartmentsID;
  TIntegerField *cdsDepartmentsPARENTID;
  TStringField *cdsDepartmentsNAME;
  TFloatField *cdsDepartmentsBUDGET;
  TStringField *cdsDepartmentsPHONE;
  TStringField *cdsDepartmentsFAX;
  TStringField *cdsDepartmentsEMAIL;
  TBooleanField *cdsDepartmentsVACANCY;
  TDataSource *dsDepartments;
  TClientDataSet *cdsPersons;
  TAutoIncField *cdsPersonsID;
  TStringField *cdsPersonsName;
  TStringField *cdsPersonsCountry;
  TStringField *cdsPersonsPostalCode;
  TStringField *cdsPersonsCity;
  TStringField *cdsPersonsAddress;
  TStringField *cdsPersonsPhone;
  TStringField *cdsPersonsFax;
  TStringField *cdsPersonsEMAIL;
  TStringField *cdsPersonsHOMEPAGE;
  TIntegerField *cdsPersonsDepartmentID;
  TDataSource *dsPersons;
  TcxStyleRepository *StyleRepository;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxStyle *cxStyle8;
  TcxStyle *cxStyle9;
  TcxStyle *cxStyle10;
  TcxStyle *cxStyle11;
  TcxStyle *cxStyle12;
  TcxStyle *cxStyle13;
  TcxStyle *stlGroupNode;
  TcxStyle *stlFixedBand;
  TcxTreeListStyleSheet *TreeListStyleSheetDevExpress;
  TClientDataSet *cdsDeptDict;
  TDataSource *dsDeptDict;
  TDataSource *dsPersDict;
  TClientDataSet *cdsPersDict;
  TDataSetProvider *dspDepartments;
  TDataSetProvider *dspPersons;
	TClientDataSet *cdsDeptData;
	TClientDataSet *cdsPersData;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall cdsDepartmentsAfterPost(TDataSet *DataSet);
	void __fastcall cdsPersonsAfterPost(TDataSet *DataSet);
	void __fastcall cdsDeptDictAfterPost(TDataSet *DataSet);
	void __fastcall cdsPersDictAfterPost(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
  __fastcall TDragDropDemoDataDM(TComponent* Owner);
  void SetParentValue(Variant AValue);
};
//---------------------------------------------------------------------------
extern PACKAGE TDragDropDemoDataDM *DragDropDemoDataDM;
//---------------------------------------------------------------------------
#endif
