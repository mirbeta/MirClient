//---------------------------------------------------------------------------

#ifndef ResourceMainUnitH
#define ResourceMainUnitH
#include "..\cxDemosBCB.inc"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxSchedulerAgendaView.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "cxSchedulerStorage.hpp"
#include "cxSchedulerUtils.hpp"
#include <Buttons.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TResourceDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
    TcxStyle *cxAutumnStyle;
    TcxStyle *cxContentStyle;
    TcxStyle *cxSelectionStyle;
    TcxStyle *cxSpringStyle;
    TcxStyle *cxSummerStyle;
    TcxStyle *cxTimeLineStyle;
    TcxStyle *cxTimeStyle;
    TcxStyle *cxWinterStyle;
    TcxStyleRepository *cxStyleRepository;
    TOpenDialog *dlgOpen;
    TListView *lvCarsBar;
    TImageList *imgCars;
    TImageList *imgGlyph;
    TMenuItem *miCancelReservation;
    TMenuItem *miOpen;
    TMenuItem *miRent;
    TMenuItem *miRentCar;
    TMenuItem *miSaveAs;
    TMenuItem *miUseColorScheme;
    TSpeedButton *btnCancelReserv;
    TSpeedButton *btnRent;
    TPanel *pnlTools;
    TcxSchedulerStorage *Storage;
    TStringGrid *TaskGrid;
    void __fastcall FormCreate(TObject* Sender);
    void __fastcall lvCarsBarSelectItem(TObject* Sender, TListItem* Item, bool Selected);
    void __fastcall OpenSaveClick(TObject* Sender);
    void __fastcall RentClick(TObject* Sender);
    void __fastcall SchedulerLayoutChanged(TObject* Sender);
    void __fastcall SchedulerDateNavigatorCustomDrawContent(TObject* Sender, TcxCanvas* ACanvas,
        TcxSchedulerDateNavigatorMonthContentViewInfo* AViewInfo, bool &ADone);
    void __fastcall SchedulerViewDayCustomDrawContainer(TObject* Sender, TcxCanvas* ACanvas,
        TcxSchedulerContainerCellViewInfo* AViewInfo, bool &ADone);
    void __fastcall SchedulerCustomDrawContent(TObject* Sender, TcxCanvas *ACanvas,
        TcxSchedulerContentCellViewInfo* AViewInfo, bool &ADone);
    void __fastcall SchedulerStylesGetContentStyle(TObject* Sender,
        TcxSchedulerStorageResourceItem* AResource, const TDateTime ADateTime, TcxStyle* &AStyle);
    void __fastcall miUseColorSchemeClick(TObject* Sender);
    void __fastcall SchedulerEventPopupMenuPopup(TcxSchedulerEventPopupMenu *Sender,
        TPopupMenu *ABuiltInMenu, bool &AHandled);
    void __fastcall SchedulerContentPopupMenuPopup(TcxSchedulerContentPopupMenu *Sender,
        TPopupMenu *ABuiltInMenu, bool &AHandled);
    void __fastcall SchedulerBeforeDeleting(TcxCustomScheduler *Sender,
        TcxSchedulerControlEvent *AEvent, bool &Allow);
    void __fastcall SchedulerSelectionChanged(TObject* Sender);
    void __fastcall TaskGridSelectCell(TObject* Sender, int ACol, int ARow, bool &CanSelect);
private:	// User declarations
    TcxSchedulerEventList *TaskEvents;
public:		// User declarations
    __fastcall TResourceDemoMainForm(TComponent* Owner);
    void __fastcall CreateRecurrenceEvent(int AResourceID, String ACaption, TDays AOccurDays);
    void __fastcall DrawDateNavigatorContent(TcxCanvas *ACanvas,
        TcxSchedulerDateNavigatorMonthContentViewInfo *AViewInfo, bool &ADone);
    void __fastcall FillTaskGrid();
    TColor __fastcall GetResourceColor(TcxSchedulerResourceViewInfo *AResource);
    void __fastcall SelectEvents(int AStartIndex);
    void __fastcall SyncVisibility(TDateTime ADate, int AIndex);

};

int __fastcall GetIndex(TListView *AListView);
void __fastcall SetIndex(TListView *AListView, int AIndex);

//---------------------------------------------------------------------------
extern PACKAGE TResourceDemoMainForm *ResourceDemoMainForm;
//---------------------------------------------------------------------------
#endif
