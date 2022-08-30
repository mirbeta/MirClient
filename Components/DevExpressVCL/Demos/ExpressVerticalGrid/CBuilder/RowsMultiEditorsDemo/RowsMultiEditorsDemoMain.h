//---------------------------------------------------------------------------

#ifndef RowsMultiEditorsDemoMainH
#define RowsMultiEditorsDemoMainH
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
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxVGrid.hpp"
//---------------------------------------------------------------------------
class TRowsMultiEditorsDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBVerticalGrid *cxDBVerticalGrid;
  TcxCategoryRow *ctgOrder;
  TcxDBMultiEditorRow *merPurchaseDateTime;
  TcxDBMultiEditorRow *merPaymentDescr;
  TcxCategoryRow *ctgCustomer;
  TcxDBMultiEditorRow *merCustomer;
  TcxDBMultiEditorRow *merCustomerInfo;
  TcxDBMultiEditorRow *merWork;
  TcxDBMultiEditorRow *merMailData;
  TcxDBEditorRow *erAddress;
  TcxDBMultiEditorRow *merPhones;
  TcxDBEditorRow *erEmail;
  TcxCategoryRow *ctgCar;
  TcxDBMultiEditorRow *merCarName;
  TcxDBMultiEditorRow *merCarEngine;
  TcxDBMultiEditorRow *merTransmiss;
  TcxDBEditorRow *erCarDescr;
  TcxDBEditorRow *erCarImage;
  TcxDBEditorRow *erHyperlink;
  TcxDBEditorRow *erPrice;
  TImageList *ImageList;
  TcxEditRepository *EditRepository;
  TcxEditRepositoryBlobItem *erepCarPictEditing;
  TcxEditRepositoryImageItem *erepCarPictEdit;
  TcxEditRepositoryCalcItem *erepPaymentAmountEditing;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
  void __fastcall erCarImagePropertiesGetEditingProperties(
    TcxCustomEditorRowProperties *Sender, int ARecordIndex,
    TcxCustomEditProperties *&AProperties);
  void __fastcall erCarImagePropertiesGetEditProperties(
    TcxCustomEditorRowProperties *Sender, int ARecordIndex,
    TcxCustomEditProperties *&AProperties);
  void __fastcall merPaymentDescrEditors2GetEditingProperties(
    TcxCustomEditorRowProperties *Sender, int ARecordIndex,
    TcxCustomEditProperties *&AProperties);
  void __fastcall miLayoutStyleClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TRowsMultiEditorsDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TRowsMultiEditorsDemoMainForm *RowsMultiEditorsDemoMainForm;
//---------------------------------------------------------------------------
#endif
