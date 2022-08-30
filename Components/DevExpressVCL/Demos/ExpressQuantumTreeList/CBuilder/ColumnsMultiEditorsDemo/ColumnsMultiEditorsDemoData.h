//---------------------------------------------------------------------------

#ifndef ColumnsMultiEditorsDemoDataH
#define ColumnsMultiEditorsDemoDataH
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
class TColumnsMultiEditorsDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TdxMemData *mdPersons;
  TAutoIncField *mdPersonsID;
  TStringField *mdPersonsName;
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
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TColumnsMultiEditorsDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnsMultiEditorsDemoDataDM *ColumnsMultiEditorsDemoDataDM;
//---------------------------------------------------------------------------
#endif
