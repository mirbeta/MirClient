//---------------------------------------------------------------------------

#ifndef CustomDrawDemoDataH
#define CustomDrawDemoDataH
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
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TCustomDrawDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TdxMemData *mdDepartments;
  TAutoIncField *mdDepartmentsID;
  TIntegerField *mdDepartmentsPARENTID;
  TStringField *mdDepartmentsNAME;
  TFloatField *mdDepartmentsBUDGET;
  TStringField *mdDepartmentsPHONE;
  TStringField *mdDepartmentsFAX;
  TStringField *mdDepartmentsEMAIL;
  TBooleanField *mdDepartmentsVACANCY;
  TIntegerField *mdDepartmentsMANAGERID;
  TDataSource *dsDepartments;
  TDataSource *dsPersons;
  TdxMemData *mdPersons;
	TAutoIncField *mdPersonsID;
  TStringField *mdPersonsName;
  TStringField *mdPersonsAddress;
  TStringField *mdPersonsPhone;
  TStringField *mdPersonsFax;
  TStringField *mdPersonsEMAIL;
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
  TcxStyle *styNoVacancy;
  TcxStyle *styVacancy;
  TcxTreeListStyleSheet *TreeListStyleSheetDevExpress;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TCustomDrawDemoDataDM(TComponent* Owner);
    void SetParentValue(Variant AValue);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawDemoDataDM *CustomDrawDemoDataDM;
//---------------------------------------------------------------------------
#endif
