//---------------------------------------------------------------------------

#ifndef IssueListGridH
#define IssueListGridH
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
#include "cxGridBandedTableView.hpp"
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <ExtCtrls.hpp>

#include "IssueListForm.h"
#include "cxBlobEdit.hpp"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"

//---------------------------------------------------------------------------
class TIssueListGridForm : public TForm
{
__published:
        TSplitter *Splitter1;
        TcxGrid *cxGrid;
        TcxGridDBTableView *tvItems;
        TcxGridDBColumn *tvItemsNAME;
        TcxGridDBColumn *tvItemsPROJECTID;
        TcxGridDBColumn *tvItemsTYPE;
        TcxGridDBColumn *tvItemsOWNERID;
        TcxGridDBColumn *tvItemsCREATEDDATE;
        TcxGridDBColumn *tvItemsSTATUS;
        TcxGridDBColumn *tvItemsPRIORITY;
        TcxGridDBColumn *tvItemsCREATORID;
        TcxGridDBColumn *tvItemsLASTMODIFIEDDATE;
        TcxGridDBColumn *tvItemsFIXEDDATE;
        TcxGridDBColumn *tvItemsRESOLUTION;
        TcxGridDBColumn *tvItemsDESCRIPTION;
        TcxGridDBTableView *tvProjects;
        TcxGridDBColumn *tvProjectsNAME;
        TcxGridDBColumn *tvProjectsMANAGERID;
        TcxGridDBTableView *tvUsers;
        TcxGridDBColumn *tvUsersFNAME;
        TcxGridDBColumn *tvUsersMNAME;
        TcxGridDBColumn *tvUsersLNAME;
        TcxGridDBColumn *tvUsersEMAIL;
        TcxGridDBColumn *tvUsersPHONE;
        TcxGridDBColumn *tvUsersDEPARTMENTID;
        TcxGridDBTableView *tvTeams;
        TcxGridDBColumn *tvTeamsPROJECTID;
        TcxGridDBColumn *tvTeamsUSERID;
        TcxGridDBColumn *tvTeamsFUNCTION;
        TcxGridDBTableView *tvDepartments;
        TcxGridDBColumn *tvDepartmentsNAME;
        TcxGridDBBandedTableView *btnSchedule;
        TcxGridDBBandedColumn *btnScheduleID;
        TcxGridDBBandedColumn *btnSchedulePROJECTID;
        TcxGridDBBandedColumn *btnScheduleUSERID;
        TcxGridDBBandedColumn *btnScheduleSUNDAY;
        TcxGridDBBandedColumn *btnScheduleMONDAY;
        TcxGridDBBandedColumn *btnScheduleTUESDAY;
        TcxGridDBBandedColumn *btnScheduleWEDNESDAY;
        TcxGridDBBandedColumn *btnScheduleTHURSDAY;
        TcxGridDBBandedColumn *btnScheduleFRIDAY;
        TcxGridDBBandedColumn *btnScheduleSATURDAY;
        TcxGridDBBandedColumn *btnScheduleRowSum;
        TcxGridDBBandedColumn *btnScheduleRowAvg;
        TcxGridLevel *lvProjects;
        TcxGridLevel *lvProjectItems;
        TcxGridLevel *lvItems;
        TcxGridLevel *lvDepartments;
        TcxGridLevel *lvDepartmentUsers;
        TcxGridLevel *lvTeam;
        TcxGridLevel *lvUsers;
        TcxGridLevel *lvSchedule;
        TPanel *pnlForm;
		TcxGridPopupMenu *cxGridPopupMenu1;
		TLabel *lbDesciption;
        void __fastcall cxGridActiveTabChanged(TcxCustomGrid *Sender,
          TcxGridLevel *ALevel);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall cxGridFocusedViewChanged(TcxCustomGrid *Sender,
          TcxCustomGridView *APrevFocusedView,
          TcxCustomGridView *AFocusedView);
        void __fastcall FormActivate(TObject *Sender);
        void __fastcall cxGridRootLevelStylesGetTabStyle(
          TcxGridLevel *Sender, TcxGridLevel *ATabLevel,
          TcxStyle *&AStyle);
        void __fastcall tvItemsStylesGetContentStyle(
          TcxCustomGridTableView *Sender, TcxCustomGridRecord *ARecord,
          TcxCustomGridTableItem *AItem, TcxStyle *&AStyle);
private:
        bool FIsDependsOnData;

        void __fastcall GoToFrame(int AID);
        int __fastcall GetFrameIDByDataSetName(const AnsiString ADataSetName);
        TfrmBasic* __fastcall CreateFrameByID(int AID);
        void __fastcall ChangeUsersLevel(TcxGridLevel *ALevel);
        void __fastcall ChangeItemsLevel(TcxGridLevel *ALevel);
        void __fastcall ChangeDescription(int AIndex);
        TcxGridLevel* __fastcall GetGridLevelByTag(int ATag);
        TcxGridDBTableView* __fastcall GetFocusedView(void);
        void __fastcall RegisterFrames();
public:
        __fastcall TIssueListGridForm(TComponent* Owner);

        // operations
        void __fastcall DoGoProject(int AIndex);

        void __fastcall DoSetShowPictures(bool Value);
        void __fastcall DoSetShowDescription(bool Value);
        void __fastcall DoSetShowDependsOnData(bool Value);
        void __fastcall DoSetShowNewItemRow(bool Value);

        void __fastcall DoSetNativeStyle(bool Value);
        void __fastcall DoSetLookAndFeelKind(TcxLookAndFeelKind AKind);
        void __fastcall DoSetEditorsShadow(bool Value);

        void __fastcall DoSetShowIndicator(bool Value);
        void __fastcall DoSetShowGrouping(bool Value);
        void __fastcall DoSetShowHeader(bool Value);
        void __fastcall DoSetShowFooter(bool Value);
        void __fastcall DoSetAutoWidth(bool Value);
        void __fastcall DoSetInvertSelected(bool Value);
        void __fastcall DoSetAutoPreview(bool Value);
        void __fastcall DoSetShowEditButtons(bool Value);
        void __fastcall DoSetShowGridLines(bool Value);

        void __fastcall DoFullCollapse();
        void __fastcall DoFullExpand();
        void __fastcall DoColumnsCustomization();
        void __fastcall DoBestFit();

        __property TcxGridDBTableView* FocusedView = {read = GetFocusedView};
        __property bool IsDependsOnData = {read = FIsDependsOnData, write = FIsDependsOnData};

};
//---------------------------------------------------------------------------
extern PACKAGE TIssueListGridForm *IssueListGridForm;
//---------------------------------------------------------------------------
#endif
