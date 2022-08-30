//---------------------------------------------------------------------------

#ifndef FilterByCodeDemoDataH
#define FilterByCodeDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxDBEditRepository.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include <ImgList.hpp>
#include <Db.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TFilterByCodeDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TClientDataSet *tblCustomers;
  TDataSource *dsCustomers;
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
  TcxStyle *cxStyle14;
  TcxGridTableViewStyleSheet *GridTableViewStyleSheetDevExpress;
  TClientDataSet *tblProducts;
  TcxEditRepository *edrepMain;
  TcxEditRepositoryImageComboBoxItem *edrepDXPaymentTypeImageCombo;
  TcxEditRepositoryLookupComboBoxItem *edrepDXLookupProducts;
  TImageList *imPaytTypes;
  TDataSource *dsProducts;
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TFilterByCodeDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFilterByCodeDemoMainDM *FilterByCodeDemoMainDM;
//---------------------------------------------------------------------------
#endif
