//---------------------------------------------------------------------------

#ifndef ViewCardSimpleDemoDataH
#define ViewCardSimpleDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <DBClient.hpp>
#include <Db.hpp>
//---------------------------------------------------------------------------
class TViewCardSimpleDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TClientDataSet *tlbDEPARTMENTS;
  TAutoIncField *tlbDEPARTMENTSID;
  TStringField *tlbDEPARTMENTSNAME;
  TDataSource *dsDEPARTMENTS;
  TDataSource *dsUSERS;
  TClientDataSet *tlbUSERS;
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TViewCardSimpleDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewCardSimpleDemoMainDM *ViewCardSimpleDemoMainDM;
//---------------------------------------------------------------------------
#endif
