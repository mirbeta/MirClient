//---------------------------------------------------------------------------

#ifndef ColumnsShareDemoDataH
#define ColumnsShareDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxGridCardView.hpp"
#include <ImgList.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TColumnsShareDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TClientDataSet *tblProjects;
  TAutoIncField *tblProjectsID;
  TStringField *tblProjectsNAME;
  TIntegerField *tblProjectsMANAGERID;
  TDataSource *dsProjects;
  TDataSource *dsItems;
  TClientDataSet *tblItems;
  TAutoIncField *tblItemsID;
  TStringField *tblItemsNAME;
  TBooleanField *tblItemsTYPE;
  TIntegerField *tblItemsPROJECTID;
  TSmallintField *tblItemsPRIORITY;
  TSmallintField *tblItemsSTATUS;
  TIntegerField *tblItemsCREATORID;
  TDateTimeField *tblItemsCREATEDDATE;
  TIntegerField *tblItemsOWNERID;
  TDateTimeField *tblItemsLASTMODIFIEDDATE;
  TDateTimeField *tblItemsFIXEDDATE;
  TMemoField *tblItemsDESCRIPTION;
  TMemoField *tblItemsRESOLUTION;
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
  TStringField *tblUsersDepartment;
  TDataSource *dsUsers;
  TDataSource *dsProjectTeam;
  TClientDataSet *tblProjectTeam;
  TAutoIncField *tblProjectTeamID;
  TIntegerField *tblProjectTeamPROJECTID;
  TIntegerField *tblProjectTeamUSERID;
  TStringField *tblProjectTeamFUNCTION;
  TDataSource *dsDepartments;
  TClientDataSet *tblDepartments;
  TImageList *imStat;
  void __fastcall tblUsersCalcFields(TDataSet *DataSet);
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TColumnsShareDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnsShareDemoMainDM *ColumnsShareDemoMainDM;
//---------------------------------------------------------------------------
#endif
