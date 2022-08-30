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
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxButtonEdit.hpp"
#include "cxCalc.hpp"
#include "cxCheckBox.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxMRUEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTimeEdit.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBCardView *cvCustomers;
  TcxGridDBCardViewRow *cvCustomersCompany;
  TcxGridDBCardViewRow *cvCustomersState;
  TcxGridDBCardViewRow *cvCustomersCity;
  TcxGridDBCardViewRow *cvCustomersPrefix;
  TcxGridDBCardViewRow *cvCustomersFirstName;
  TcxGridDBCardViewRow *cvCustomersLastName;
  TcxGridDBCardViewRow *cvCustomersCustomer;
  TcxGridDBCardViewRow *cvCustomersZipCode;
  TcxGridDBCardViewRow *cvCustomersAddress;
  TcxGridDBCardViewRow *cvCustomersFaxPhone;
  TcxGridDBCardViewRow *cvCustomersDescription;
  TcxGridDBCardView *cvOrders;
  TcxGridDBCardViewRow *cvOrdersCustomerID;
  TcxGridDBCardViewRow *cvOrdersProductID;
  TcxGridDBCardViewRow *cvOrdersPurchaseDate;
  TcxGridDBCardViewRow *cvOrdersTime;
  TcxGridDBCardViewRow *cvOrdersPaymentType;
  TcxGridDBCardViewRow *cvOrdersQuantity;
  TcxGridDBCardViewRow *cvOrdersPaymentAmount;
  TcxGridDBCardViewRow *cvOrdersDescription;
  TcxGridDBCardView *cvCars;
  TcxGridDBCardViewRow *cvCarsCategory;
  TcxGridDBCardViewRow *cvCarsCar;
  TcxGridDBCardViewRow *cvCarsPicture;
  TcxGridDBCardViewRow *cvCarsInfo;
  TcxGridDBCardViewRow *cvCarsHyperlink;
  TcxGridLevel *lvCustomers;
  TcxGridLevel *lvOrders;
  TcxGridLevel *lvCars;
  TMenuItem *miOptions;
  TMenuItem *miShowEditButtons;
  TMenuItem *miEditButtonsNever;
  TMenuItem *miEditButtonsForFocusedRecord;
  TMenuItem *miEditButtonsAlways;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall cvCustomersStatePropertiesButtonClick(TObject *Sender);
  void __fastcall cvCustomersCityPropertiesButtonClick(TObject *Sender, int AButtonIndex);
  void __fastcall GridFocusedViewChanged(TcxCustomGrid *Sender,
      TcxCustomGridView *APrevFocusedView, TcxCustomGridView *AFocusedView);
  void __fastcall miEditButtonsClick(TObject *Sender);
  void __fastcall cvCarsCustomDrawCell(TcxCustomGridTableView *Sender,
      TcxCanvas *ACanvas, TcxGridTableDataCellViewInfo *AViewInfo, bool &ADone);
private:	// User declarations
  void UpdateOptions();
public:		// User declarations
  __fastcall TEditorsInPlaceDemoMainForm(TComponent* Owner);
};

//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoMainForm *EditorsInPlaceDemoMainForm;
//---------------------------------------------------------------------------

class TcxPopupEditPropertiesAccess : public TcxPopupEditProperties
{
public:
  __property ImmediatePopup;
};
//---------------------------------------------------------------------------
#endif
