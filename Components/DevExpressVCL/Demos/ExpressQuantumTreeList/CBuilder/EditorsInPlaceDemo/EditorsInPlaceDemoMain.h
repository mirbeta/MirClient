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
#include "cxButtonEdit.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxDropDownEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxInplaceContainer.hpp"
#include "cxSpinEdit.hpp"
#include "cxTimeEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxBlobEdit.hpp"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxMRUEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "CarsData.h"
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBTreeList *tlOrders;
  TcxDBTreeListColumn *tlOrdersCustomerID;
  TcxDBTreeListColumn *tlOrdersCompanyEmail;
  TcxDBTreeListColumn *tlOrdersProductID;
  TcxDBTreeListColumn *tlOrdersCarInfo;
  TcxDBTreeListColumn *tlOrdersPurchaseDate;
  TcxDBTreeListColumn *tlOrdersTime;
  TcxDBTreeListColumn *tlOrdersPaymentType;
  TcxDBTreeListColumn *tlOrdersPaymentAmount;
  TcxDBTreeListColumn *tlOrdersQuantity;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall tlOrdersCarInfoGetDisplayText(TcxTreeListColumn *Sender,
    TcxTreeListNode *ANode, String &Value);
  void __fastcall tlOrdersCompanyEmailPropertiesButtonClick(TObject *Sender,
    int AButtonIndex);
  void __fastcall tlOrdersCarInfoPropertiesInitPopup(TObject *Sender);
  void __fastcall tlOrdersCarInfoPropertiesCloseUp(TObject *Sender);
  void __fastcall miShowEditBtnsClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsInPlaceDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoMainForm *EditorsInPlaceDemoMainForm;
//---------------------------------------------------------------------------
#endif
