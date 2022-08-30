//---------------------------------------------------------------------------

#ifndef CancelReservationUnitH
#define CancelReservationUnitH
#include "..\cxDemosBCB.inc"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxLabel.hpp"
#include "cxListBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxSchedulerStorage.hpp"
//---------------------------------------------------------------------------
class TfrmCancelReservation : public TForm
{
__published:	// IDE-managed Components
    TcxListBox *lbxCustomers;
    TcxButton *btnCancelReserv;
    TcxButton *btnClose;
    TcxListBox *lbxEvents;
    TcxLabel *lbSelectCustomer;
    TcxLabel *lbCancelReservation;
    void __fastcall FormCreate(TObject* Sender);
    void __fastcall lbxCustomersClick(TObject* Sender);
    void __fastcall lbxEventsClick(TObject* Sender);
    void __fastcall btnCancelReservClick(TObject* Sender);
private:	// User declarations
public:		// User declarations
    TDateTime Date;
    int Index;
    __fastcall TfrmCancelReservation(TComponent* Owner);
    TcxCustomSchedulerStorage* __fastcall Storage();
    void __fastcall CheckButtonEnabled();
    void __fastcall FillCustomersList();
    void __fastcall FillEventsList(int ACustomerIndex);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCancelReservation *frmCancelReservation;
//---------------------------------------------------------------------------
#endif
