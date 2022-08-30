//---------------------------------------------------------------------------

#ifndef UnboundDesignDefinedDemoMainH
#define UnboundDesignDefinedDemoMainH
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
#include "BaseForm.h"
#include "cxCurrencyEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxImageComboBox.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TUnboundDesignDefinedDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridBandedTableView *bvCars;
  TcxGridBandedColumn *clnTrademark;
  TcxGridBandedColumn *clnCar;
  TcxGridBandedColumn *clnPrice;
  TcxGridBandedColumn *clnPicture;
  TcxGridBandedColumn *clnHP;
  TcxGridBandedColumn *clnTorque;
  TcxGridBandedColumn *clnCyl;
  TcxGridBandedColumn *clnTransmissSpeedCount;
  TcxGridBandedColumn *clnTransmissAutomatic;
  TcxGridBandedColumn *clnHyperlink;
  TcxGridLevel *lvCars;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *styCar;
  TcxStyle *styGroup;
private:  // User declarations
public:   // User declarations
  __fastcall TUnboundDesignDefinedDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundDesignDefinedDemoMainForm *UnboundDesignDefinedDemoMainForm;
//---------------------------------------------------------------------------
#endif
