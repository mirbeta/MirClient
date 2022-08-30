//---------------------------------------------------------------------------

#ifndef Office11GroupRowStyleDemoMainH
#define Office11GroupRowStyleDemoMainH
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
#include "cxCalendar.hpp"
#include "cxDropDownEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxImageComboBox.hpp"
#include "BaseForm.h"
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TOffice11GroupRowStyleDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
        TcxGrid *Grid;
        TcxGridTableView *tvMail;
        TcxGridColumn *tvMailImportance;
        TcxGridColumn *tvMailIcon;
        TcxGridColumn *tvMailAttachment;
        TcxGridColumn *tvMailFrom;
        TcxGridColumn *tvMailSubject;
        TcxGridColumn *tvMailReceived;
        TcxGridColumn *tvMailSent;
        TcxGridLevel *lvMail;
        TMenuItem *miOptions;
        TMenuItem *miOffice11GroupRowStyle;
        TMenuItem *miGroupBySorting;
        TMenuItem *miAlwaysExpandedGroups;
        TMenuItem *miDateTimeGrouping;
        TMenuItem *miDateTimeGroupingByDateAndTime;
        TMenuItem *miDateTimeGroupingRelativeToToday;
        TMenuItem *miDateTimeGroupingByHour;
        TMenuItem *miDateTimeGroupingByDate;
        TMenuItem *miDateTimeGroupingByMonth;
        TMenuItem *miDateTimeGroupingByYear;
        TImageList *imgImportance;
        TDataSource *dsPersons;
        TcxStyle *UnreadStyle;
	TdxMemData *mdPersons;
	TStringField *mdPersonsFullName;
	TStringField *mdPersonsFNAME;
	TStringField *mdPersonsLNAME;
	TAutoIncField *mdPersonsID;
	TStringField *mdPersonsMNAME;
	TStringField *mdPersonsCOUNTRY;
	TStringField *mdPersonsPOSTALCODE;
	TStringField *mdPersonsCITY;
	TStringField *mdPersonsADDRESS;
	TStringField *mdPersonsPHONE;
	TStringField *mdPersonsFAX;
	TStringField *mdPersonsEMAIL;
	TStringField *mdPersonsHOMEPAGE;
	TIntegerField *mdPersonsDEPARTMENTID;
		void __fastcall tvMailStylesGetContentStyle(
                TcxCustomGridTableView *Sender, TcxCustomGridRecord *ARecord,
                TcxCustomGridTableItem *AItem, TcxStyle *&AStyle);
        void __fastcall miOffice11GroupRowStyleClick(TObject *Sender);
        void __fastcall miAlwaysExpandedGroupsClick(TObject *Sender);
        void __fastcall miGroupBySortingClick(TObject *Sender);
        void __fastcall miDateTimeGroupingClick(TObject *Sender);
	void __fastcall mdPersonsCalcFields(TDataSet *DataSet);
private:
        // record data random generating
        void __fastcall AddRecordIntoTable(int ARecordIndex);
        void __fastcall AddRecordsIntoTable();
        int GetImportance();
        int GetIcon();
        TDateTime GetSent();
        TDateTime GetReceived(TDateTime ASent);
        String GetSubject();
protected:
        void UpdateMenu();
public:   // User declarations
        __fastcall TOffice11GroupRowStyleDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TOffice11GroupRowStyleDemoMainForm *Office11GroupRowStyleDemoMainForm;
//---------------------------------------------------------------------------
#endif
