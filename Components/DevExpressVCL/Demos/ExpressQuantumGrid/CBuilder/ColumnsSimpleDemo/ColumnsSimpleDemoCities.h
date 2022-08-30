//---------------------------------------------------------------------------

#ifndef ColumnsSimpleDemoCitiesH
#define ColumnsSimpleDemoCitiesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
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
#include "cxLookAndFeelPainters.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <ExtCtrls.hpp>
#include "cxDataStorage.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TColumnsSimpleDemoCitiesForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *lbDescription;
        TPanel *pnlCustomize;
        TcxButton *btnSet;
        TcxButton *btnAdd;
        TcxButton *btnDelete;
        TcxButton *btnCancel;
        TcxGrid *GridCities;
        TcxGridDBTableView *tvCities;
        TcxGridDBColumn *tvCitiesCity;
        TcxGridLevel *lvCities;
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnDeleteClick(TObject *Sender);
  void __fastcall tvCitiesKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
private:	// User declarations
  Variant __fastcall GetValue();
public:		// User declarations
  __fastcall TColumnsSimpleDemoCitiesForm(TComponent* Owner);
  __property Variant Value = {read = GetValue};
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnsSimpleDemoCitiesForm *ColumnsSimpleDemoCitiesForm;
//---------------------------------------------------------------------------
#endif
