//---------------------------------------------------------------------------

#ifndef SimpleListDemoDataH
#define SimpleListDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxTL.hpp"
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TSimpleListDemoDataDM : public TDataModule
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
  TcxTreeListStyleSheet *TreeListStyleSheetDevExpress;
private:	// User declarations
public:		// User declarations
  __fastcall TSimpleListDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSimpleListDemoDataDM *SimpleListDemoDataDM;
//---------------------------------------------------------------------------
#endif
