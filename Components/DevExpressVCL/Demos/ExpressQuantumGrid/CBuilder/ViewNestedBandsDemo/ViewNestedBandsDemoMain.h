//---------------------------------------------------------------------------

#ifndef ViewNestedBandsDemoMainH
#define ViewNestedBandsDemoMainH
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
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include <ExtCtrls.hpp>
#include "cxLookAndFeels.hpp"
#include "cxGridBandedTableView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "BaseForm.h"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxImageComboBox.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TViewNestedBandsDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBBandedTableView *bvOrders;
  TcxGridDBBandedColumn *clnCustomerID;
  TcxGridDBBandedColumn *clnCustomerCompany;
  TcxGridDBBandedColumn *clnCustomerAddres;
  TcxGridDBBandedColumn *clnCustomerFax;
  TcxGridDBBandedColumn *clnCustomerPhone;
  TcxGridDBBandedColumn *clnCustomerOccupation;
  TcxGridDBBandedColumn *clnCustomerZipCode;
  TcxGridDBBandedColumn *clnOrdersProductID;
  TcxGridDBBandedColumn *clnCarTorque;
  TcxGridDBBandedColumn *clnCarCyl;
  TcxGridDBBandedColumn *clnCarHP;
  TcxGridDBBandedColumn *clnCarMPG_City;
  TcxGridDBBandedColumn *clnCarMPG_Highway;
  TcxGridDBBandedColumn *clnCarTransmissSpeedCount;
  TcxGridDBBandedColumn *clnCarTransMissAuto;
  TcxGridDBBandedColumn *clnPurchaseDate;
  TcxGridDBBandedColumn *clnPaymentType;
  TcxGridDBBandedColumn *clnQuantity;
  TcxGridDBBandedColumn *clnPaymentAmount;
  TcxGridLevel *lvOrders;
  TMenuItem *miOptions;
  TMenuItem *miNestedBands;
  TMenuItem *N1;
  TMenuItem *miBandsQuickCustomization;
  TMenuItem *miColumnsQuickCustomization;
  TMenuItem *N2;
  TMenuItem *miCellMerging;
  void __fastcall  miNestedBandsClick(TObject *Sender);
  void __fastcall  miBandsQuickCustomizationClick(TObject *Sender);
  void __fastcall  miColumnsQuickCustomizationClick(TObject *Sender);
  void __fastcall  miCellMergingClick(TObject *Sender);
private:  // User declarations
  TList *FNestedBands;
  void AddNestedBandInfo(TcxGridBand *ABand);
  void AdjustCellMerging(bool AUseCelMerging);
  void AdjustNestedBands(bool AUseNestedBands);
  void ChangeBandVisibility(int AIndex, bool AVisible);
  void HideNestedBands();
  void ReleaseNestedBandInfos();
  void ShowNestedBands();
public:   // User declarations
  __fastcall TViewNestedBandsDemoMainForm(TComponent* Owner);
  __fastcall ~TViewNestedBandsDemoMainForm(void);
};

class TcxNestedBandInfo
{
private:
  TcxGridBand *FBand;
  TcxGridBand *FParentBand;
  int FColumnIndex;
  TCaption FCaption;
public:
  __fastcall TcxNestedBandInfo(TcxGridBand *ABand);
  void  __fastcall RestoreBand();
  __property TcxGridBand *Band = {read=FBand};
  __property TCaption Caption = {read=FCaption};
  __property int ColumnIndex = {read=FColumnIndex};
  __property TcxGridBand *ParentBand = {read=FParentBand};
};

//---------------------------------------------------------------------------
extern PACKAGE TViewNestedBandsDemoMainForm *ViewNestedBandsDemoMainForm;
//---------------------------------------------------------------------------
#endif
