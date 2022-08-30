//---------------------------------------------------------------------------

#ifndef ProviderModeDemoMainH
#define ProviderModeDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxInplaceContainer.hpp"
#include "cxNavigator.hpp"
#include "cxVGrid.hpp"
#include "ProviderModeDemoClasses.h"
//---------------------------------------------------------------------------
class TProviderModeDemoMainForm : public TDemoBasicMainForm
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
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
  TcxVirtualVerticalGrid *cxVirtualVerticalGrid;
  TcxNavigator *cxNavigator1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall miLinesClick(TObject *Sender);
  void __fastcall actCellAutoHeightExecute(TObject *Sender);
  void __fastcall actCellEndEllipsisExecute(TObject *Sender);
private:	// User declarations
  TCustomerList* CustomerList;
  TCustomerDataSource* CustomerDataSource;
  void __fastcall GenerateColumns();
  void __fastcall LoadData();
  void __fastcall SaveData();
public:		// User declarations
  __fastcall TProviderModeDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TProviderModeDemoMainForm *ProviderModeDemoMainForm;
//---------------------------------------------------------------------------
#endif
