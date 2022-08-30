//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoCitiesH
#define EditorsInPlaceDemoCitiesH
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
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoCitiesForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbDescription;
  TcxGrid *GridCities;
  TcxGridDBTableView *tvCities;
  TcxGridDBColumn *tvCitiesCity;
  TcxGridLevel *lvCities;
  TcxButton *btnCancel;
  TcxButton *btnOK;
  void __fastcall tvCitiesKeyDown(TObject *Sender, Word &Key,
      TShiftState Shift);
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnDeleteClick(TObject *Sender);
private:	// User declarations
  Variant GetValue();
public:		// User declarations
  __fastcall TEditorsInPlaceDemoCitiesForm(TComponent* Owner);
  __property Variant Value = {read=GetValue};
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoCitiesForm *EditorsInPlaceDemoCitiesForm;
//---------------------------------------------------------------------------
#endif
 