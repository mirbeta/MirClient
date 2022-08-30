//---------------------------------------------------------------------------

#ifndef UnboundListDemoMainH
#define UnboundListDemoMainH
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
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridPopupMenu.hpp"
#include "UnboundListDemoClasses.h"
#include "cxDataStorage.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TUnboundListDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridTableView *tvCustomers;
  TcxGridLevel *lvCustomers;
  TcxGridPopupMenu *cxGridPopupMenu;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
  TCustomerList* CustomerList;
  TCustomerDataSource* CustomerDataSource;
  void __fastcall GenerateColumns();
  void __fastcall LoadData();
  void __fastcall CustomizeGrid();
  void __fastcall SaveData();
public:		// User declarations
  __fastcall TUnboundListDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundListDemoMainForm *UnboundListDemoMainForm;
//---------------------------------------------------------------------------
#endif
