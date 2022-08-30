//---------------------------------------------------------------------------

#ifndef ColumnsSimpleDemoCarsH
#define ColumnsSimpleDemoCarsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridBandedTableView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ExtCtrls.hpp>
#include "cxBlobEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxImageComboBox.hpp"
#include "cxMemo.hpp"
#include "cxRadioGroup.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TColumnSimpleDemoCarsForm : public TForm
{
__published:	// IDE-managed Components
        TPanel *pnlCarInfo;
        TcxGrid *GridCars;
        TcxGridDBBandedTableView *bvCars;
        TcxGridDBBandedColumn *bvCarsID;
        TcxGridDBBandedColumn *bvCarsTrademark;
        TcxGridDBBandedColumn *bvCarsModel;
        TcxGridDBBandedColumn *bvCarsPrice;
        TcxGridDBBandedColumn *bvCarsPicture;
        TcxGridDBBandedColumn *bvCarsLargePicture;
        TcxGridDBBandedColumn *bvCarsCategory;
        TcxGridDBBandedColumn *bvCarsHP;
        TcxGridDBBandedColumn *bvCarsCyl;
        TcxGridDBBandedColumn *bvCarsTransmissSpeedCount;
        TcxGridDBBandedColumn *bvCarsTransmissAutomatic;
        TcxGridDBBandedColumn *bvCarsMPG_City;
        TcxGridDBBandedColumn *bvCarsMPG_Highway;
        TcxGridDBBandedColumn *bvCarsDescription;
        TcxGridDBBandedColumn *bvCarsHyperlink;
        TcxGridLevel *lvCars;
  void __fastcall bvCarsTopRecordIndexChanged(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TColumnSimpleDemoCarsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnSimpleDemoCarsForm *ColumnSimpleDemoCarsForm;
//---------------------------------------------------------------------------
#endif
