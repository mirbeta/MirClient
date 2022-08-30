//---------------------------------------------------------------------------

#ifndef SummariesDemoDataH
#define SummariesDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxTL.hpp"
#include <ImgList.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TSummariesDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
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
  TdxMemData *mdDepartments;
  TAutoIncField	*mdDepartmentsID;
  TIntegerField	*mdDepartmentsPARENTID;
  TIntegerField	*mdDepartmentsMANAGERID;
  TStringField *mdDepartmentsNAME;
  TFloatField *mdDepartmentsBUDGET;
  TStringField *mdDepartmentsLOCATION;
  TStringField *mdDepartmentsPHONE;
  TStringField *mdDepartmentsFAX;
  TStringField *mdDepartmentsEMAIL;
  TBooleanField	*mdDepartmentsVACANCY;
  TDataSource *dsDepartments;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSummariesDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummariesDemoDataDM *SummariesDemoDataDM;
//---------------------------------------------------------------------------
#endif
