//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoMainH
#define EditorsInPlaceDemoMainH
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
#include "cxDBVGrid.hpp"
#include "cxInplaceContainer.hpp"
#include "cxVGrid.hpp"
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBVerticalGrid *vgOrders;
  TcxCategoryRow *vgOrdersCompany;
  TcxDBEditorRow *vgOrdersCustomerID;
  TcxDBEditorRow *vgOrdersCustomerEmail;
  TcxCategoryRow *vgOrdersPurchaseInfo;
  TcxDBEditorRow *vgOrdersPaymentType;
  TcxDBEditorRow *vgOrdersPaymentAmount;
  TcxDBEditorRow *vgOrdersTime;
  TcxDBEditorRow *vgOrdersPurchaseDate;
  TcxDBEditorRow *vgOrdersQuantity;
  TcxCategoryRow *vgOrdersCar;
  TcxDBEditorRow *vgOrdersProductID;
  TcxDBEditorRow *vgOrdersCarInfo;
  void __fastcall vgOrdersCompanyEmailPropertiesButtonClick(TObject *Sender,
    int AButtonIndex);
  void __fastcall miShowEditBtnsClick(TObject *Sender);
  void __fastcall actAboutExecute(TObject *Sender);
  void __fastcall vgOrdersCarInfoPropertiesGetDisplayText(
    TcxCustomEditorRowProperties *Sender, int ARecord,
    String &AText);
  void __fastcall vgOrdersCarInfoEditPropertiesInitPopup(TObject *Sender);
  void __fastcall vgOrdersCarInfoEditPropertiesCloseUp(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsInPlaceDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoMainForm *EditorsInPlaceDemoMainForm;
//---------------------------------------------------------------------------
#endif
