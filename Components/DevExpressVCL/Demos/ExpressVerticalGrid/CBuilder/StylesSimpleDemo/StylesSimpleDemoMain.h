//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoMainH
#define StylesSimpleDemoMainH
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
class TStylesSimpleDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TAction *actShowStyleDialog;
  TcxDBVerticalGrid *cxDBVerticalGrid;
  TcxCategoryRow *cxDBVerticalGridOrderInfo;
  TcxDBEditorRow *cxDBVerticalGridPurchaseDate;
  TcxDBEditorRow *cxDBVerticalGridTime;
  TcxDBEditorRow *cxDBVerticalGridPaymentType;
  TcxDBEditorRow *cxDBVerticalGridPaymentAmount;
  TcxDBEditorRow *cxDBVerticalGridQuantity;
  TcxCategoryRow *cxDBVerticalGridCustomerInfo;
  TcxCategoryRow *cxDBVerticalGridCommonCustomerInfo;
  TcxDBEditorRow *cxDBVerticalGridFirstName;
  TcxDBEditorRow *cxDBVerticalGridLastName;
  TcxDBEditorRow *cxDBVerticalGridSpouse;
  TcxDBEditorRow *cxDBVerticalGridPrefix;
  TcxDBEditorRow *cxDBVerticalGridTitle;
  TcxCategoryRow *cxDBVerticalGridCustomerContacts;
  TcxCategoryRow *cxDBVerticalGridPhonesAndFaxes;
  TcxDBEditorRow *cxDBVerticalGridFaxPhone;
  TcxDBEditorRow *cxDBVerticalGridHomePhone;
  TcxCategoryRow *cxDBVerticalGridCategoryAddress;
  TcxDBEditorRow *cxDBVerticalGridState;
  TcxDBEditorRow *cxDBVerticalGridCity;
  TcxDBEditorRow *cxDBVerticalGridAddress;
  TcxDBEditorRow *cxDBVerticalGridZipCode;
  TcxDBEditorRow *cxDBVerticalGridEmail;
  TcxDBEditorRow *cxDBVerticalGridOccupation;
  TcxDBEditorRow *cxDBVerticalGridCustomer;
  TcxDBEditorRow *cxDBVerticalGridCompany;
  TcxCategoryRow *cxDBVerticalGridCarInfo;
  TcxCategoryRow *cxDBVerticalGridCar;
  TcxDBEditorRow *cxDBVerticalGridTrademark;
  TcxDBEditorRow *cxDBVerticalGridModel;
  TcxCategoryRow *cxDBVerticalGridMPG;
  TcxDBEditorRow *cxDBVerticalGridMPG_City;
  TcxDBEditorRow *cxDBVerticalGridMPG_Highway;
  TcxCategoryRow *cxDBVerticalGridEngine;
  TcxDBEditorRow *cxDBVerticalGridHP;
  TcxDBEditorRow *cxDBVerticalGridLiter;
  TcxDBEditorRow *cxDBVerticalGridCyl;
  TcxCategoryRow *cxDBVerticalGridNotes;
  TcxDBEditorRow *cxDBVerticalGridCars_Description;
  TcxCategoryRow *cxDBVerticalGridTransmission;
  TcxDBEditorRow *cxDBVerticalGridTransmissSpeedCount;
  TcxDBEditorRow *cxDBVerticalGridTransmissAutomatic;
  TcxCategoryRow *cxDBVerticalGridOthers;
  TcxDBEditorRow *cxDBVerticalGridCategory;
  TcxDBEditorRow *cxDBVerticalGridHyperlink;
  TcxDBEditorRow *cxDBVerticalGridPrice;
  TcxDBEditorRow *cxDBVerticalGridPicture;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall actShowStyleDialogExecute(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
  void __fastcall RestoreDefaults(TObject *Sender);
public:		// User declarations
  __fastcall TStylesSimpleDemoMainForm(TComponent* Owner);
  void __fastcall StylesFormClosed(TObject *Sender, TCloseAction &Action);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoMainForm *StylesSimpleDemoMainForm;
//---------------------------------------------------------------------------
#endif
