//---------------------------------------------------------------------------

#ifndef EditorsLookupDemoDataH
#define EditorsLookupDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include <ImgList.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TEditorsLookupDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
        TDataSource *dsProjects;
        TDataSource *dsItems;
		TClientDataSet *tblProjects;
        TClientDataSet *tblItems;
        TClientDataSet *tblUsers;
        TAutoIncField *tblUsersID;
        TStringField *tblUsersUserName;
        TStringField *tblUsersFNAME;
        TStringField *tblUsersMNAME;
        TStringField *tblUsersLNAME;
        TStringField *tblUsersCOUNTRY;
        TStringField *tblUsersPOSTALCODE;
        TStringField *tblUsersCITY;
        TStringField *tblUsersADDRESS;
        TStringField *tblUsersPHONE;
        TStringField *tblUsersFAX;
        TStringField *tblUsersEMAIL;
        TStringField *tblUsersHOMEPAGE;
        TIntegerField *tblUsersDEPARTMENTID;
        TDataSource *dsUsers;
        TDataSource *dsDepartments;
        TClientDataSet *tblDepartments;
        TcxStyleRepository *cxStyleRepository;
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
        TcxStyle *cxStyle14;
        TcxStyle *cxStyle15;
        TcxStyle *cxStyle16;
        TcxStyle *cxStyle17;
        TcxStyle *cxStyle18;
        TcxStyle *cxStyle19;
        TcxStyle *cxStyle20;
        TcxStyle *cxStyle21;
        TcxStyle *cxStyle22;
        TcxStyle *cxStyle23;
        TcxStyle *cxStyle24;
        TImageList *imStat;
        TcxEditStyleController *StyleController;
  void __fastcall tblUsersCalcFields(TDataSet *DataSet);
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsLookupDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsLookupDemoDataDM *EditorsLookupDemoDataDM;
//---------------------------------------------------------------------------
#endif
 