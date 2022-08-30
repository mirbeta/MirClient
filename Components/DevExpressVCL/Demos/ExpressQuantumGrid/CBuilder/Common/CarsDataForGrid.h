//---------------------------------------------------------------------------

#ifndef CarsDataForGridH
#define CarsDataForGridH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <DB.hpp>
#include "CarsData.h"
#include "cxLookAndFeels.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxDBExtLookupComboBox.hpp"
#include "cxEdit.hpp"
#include "cxDBEditRepository.hpp"
#include "cxClasses.hpp" 
#include "dxmdaset.hpp"
#include "cxStyles.hpp"
#include "cxCustomData.hpp" 
#include "cxGraphics.hpp"
#include "cxFilter.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxNavigator.hpp"
#include "cxDBData.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridTableView.hpp"
#include "cxGridBandedTableView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxControls.hpp"
#include "cxGridCustomView.hpp"
#include "cxGrid.hpp"
  
//---------------------------------------------------------------------------
class TdmGridCars : public TdmCars
{
__published:  // IDE-managed Components
    TcxEditRepositoryExtLookupComboBoxItem* EditRepositoryTrademarkLookup;
    TcxGridViewRepository* GridViewRepository;
    TcxGridDBBandedTableView* GridViewRepositoryDBBandedTableView;
    TcxGridDBBandedColumn* GridViewRepositoryDBBandedTableViewDescription;
    TcxGridDBBandedColumn* GridViewRepositoryDBBandedTableViewID;
    TcxGridDBBandedColumn* GridViewRepositoryDBBandedTableViewLogo;
    TcxGridDBBandedColumn* GridViewRepositoryDBBandedTableViewName;
    TcxGridDBBandedColumn* GridViewRepositoryDBBandedTableViewRecId;
    TcxGridDBBandedColumn* GridViewRepositoryDBBandedTableViewSite;
public:   // User declarations
	__fastcall TdmGridCars(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmGridCars *dmGridCars;
//---------------------------------------------------------------------------
#endif
