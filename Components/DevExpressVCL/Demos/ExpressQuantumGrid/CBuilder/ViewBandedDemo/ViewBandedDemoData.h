//---------------------------------------------------------------------------

#ifndef ViewBandedDemoDataH
#define ViewBandedDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridBandedTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <Db.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TViewBandedDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsUSERS;
  TDataSource *dsPROJECTS;
  TDataSource *dsITEMS;
	TdxMemData *mdProjects;
	TAutoIncField *mdProjectsID;
	TStringField *mdProjectsNAME;
	TIntegerField *mdProjectsMANAGERID;
	TdxMemData *mdUsers;
	TAutoIncField *mdUsersID;
	TStringField *mdUsersFNAME;
	TStringField *mdUsersMNAME;
	TStringField *mdUsersLNAME;
	TStringField *mdUsersCOUNTRY;
	TStringField *mdUsersPOSTALCODE;
	TStringField *mdUsersCITY;
	TStringField *mdUsersADDRESS;
	TStringField *mdUsersPHONE;
	TStringField *mdUsersFAX;
	TStringField *mdUsersEMAIL;
	TStringField *mdUsersHOMEPAGE;
	TIntegerField *mdUsersDEPARTMENTID;
	TStringField *mdUsersName;
	TdxMemData *mdItems;
	TAutoIncField *mdItemsID;
	TStringField *mdItemsNAME;
	TBooleanField *mdItemsTYPE;
	TIntegerField *mdItemsPROJECTID;
	TSmallintField *mdItemsPRIORITY;
	TSmallintField *mdItemsSTATUS;
	TIntegerField *mdItemsCREATORID;
	TDateTimeField *mdItemsCREATEDDATE;
	TIntegerField *mdItemsOWNERID;
	TDateTimeField *mdItemsLASTMODIFIEDDATE;
	TDateTimeField *mdItemsFIXEDDATE;
	TMemoField *mdItemsDESCRIPTION;
	TMemoField *mdItemsRESOLUTION;
	void __fastcall mdUsersCalcFields(TDataSet *DataSet);
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TViewBandedDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewBandedDemoDataDM *ViewBandedDemoDataDM;
//---------------------------------------------------------------------------
#endif
