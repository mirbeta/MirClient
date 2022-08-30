//---------------------------------------------------------------------------

#ifndef RentUnitH
#define RentUnitH
#include "..\cxDemosBCB.inc"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDateNavigator.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxListBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmRentCar : public TForm
{
__published:	// IDE-managed Components
    TLabel *lbChoosePeriod;
    TLabel *lbChooseDate;
    TLabel *lbChooseCar;
    TLabel *lbCustomerName;
    TLabel *lbChooseTime;
    TcxListBox *lbxPeriod;
    TcxDateNavigator *DateNavigator;
    TListView *lvCars;
    TcxTextEdit *edtUserName;
    TcxScheduler *TimeScheduler;
    TcxButton *btnRent;
    TcxButton *btnCancel;
    TcxStyleRepository *cxStyleRepository;
    TcxStyle *cxBoldStyle;
    void __fastcall FormCreate(TObject* Sender);
    void __fastcall FormDestroy(TObject* Sender);
    void __fastcall FormShow(TObject* Sender);
    void __fastcall TimeSchedulerCustomDrawContent(TObject* Sender,
      TcxCanvas* ACanvas, TcxSchedulerContentCellViewInfo* AViewInfo, bool &ADone);
    void __fastcall DateNavigatorCustomDrawDayNumber(TObject* Sender,
      TcxCanvas* ACanvas, TcxSchedulerDateNavigatorDayNumberViewInfo* AViewInfo,
      bool &ADone);
    void __fastcall DateNavigatorCustomDrawContent(TObject* Sender,
      TcxCanvas* ACanvas, TcxSchedulerDateNavigatorMonthContentViewInfo* AViewInfo,
      bool &ADone);
    void __fastcall lvCarsSelectItem(TObject *Sender,
      TListItem *Item, bool Selected);
    void __fastcall lbxPeriodClick(TObject* Sender);
    void __fastcall TimeSchedulerKeyDown(TObject* Sender, WORD &Key,
      TShiftState Shift);
    void __fastcall btnRentClick(TObject* Sender);
        void __fastcall DateNavigatorSelectionChanged(TObject *Sender,
          const TDateTime AStart, const TDateTime AFinish);
private:	// User declarations
    TcxSchedulerEventList *FDayEvents;
    TcxSchedulerDateList *FDays;
    TcxSchedulerFilteredEventList *FEventsList;
    TcxSchedulerDateList *FPeriods;
    void __fastcall AddPeriodTime(TDateTime AStart, TDateTime AFinish);
    void __fastcall AddRent(TDateTime AStart, TDateTime AFinish);
      TDateTime __fastcall DateTimeToTime(TDateTime ATime);
    void __fastcall FillTimeList();
    bool __fastcall Intersect(TDateTime AStart1, TDateTime AFinish1,
      TDateTime AStart2, TDateTime AFinish2);
    bool __fastcall IntersectTime(TDateTime &AStart1,
      TDateTime &AFinish1, TDateTime &AStart2, TDateTime &AFinish2);
    void __fastcall ProcessSelectItem(int AIndex);
    void __fastcall RentCar();
    TcxSchedulerStorage* __fastcall Storage();
    TDateTime __fastcall TimeToDateTime(TDateTime ATime);
public:		// User declarations
        __fastcall TfrmRentCar(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmRentCar *frmRentCar;
//---------------------------------------------------------------------------
#endif
