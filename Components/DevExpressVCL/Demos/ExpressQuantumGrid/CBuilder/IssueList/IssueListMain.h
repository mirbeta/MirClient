//---------------------------------------------------------------------------

#ifndef IssueListMainH
#define IssueListMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "cxGridDBTableView.hpp"
#include "cxLookAndFeels.hpp"
#include "IssueListGrid.h"
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"

//---------------------------------------------------------------------------
class TIssueListMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
        TActionList *alMain;
        TAction *actColumnsCustomize;
        TAction *actShowPictures;
        TAction *actGrouping;
        TAction *actSummaryFooter;
        TAction *actHeader;
        TAction *actAutoWidth;
        TAction *actInvertSelected;
        TAction *actFullExpand;
        TAction *actFullCollapse;
        TAction *actBestFit;
        TAction *actShowEditButtons;
        TAction *actGridLines;
        TAction *actAutoPreview;
        TAction *actShowDependsOnData;
        TAction *actNewItemRow;
        TAction *actSelectStyleSheet;
        TAction *actEditorsShadow;
        TMenuItem *miView;
        TMenuItem *miGridPictures;
        TMenuItem *miShowDepentOnData;
        TMenuItem *miEditorsShadow;
        TMenuItem *miSeparator1;
        TMenuItem *miGoProjects;
        TMenuItem *miGoProjectItems;
        TMenuItem *miGoDepartments;
        TMenuItem *miGoTeams;
        TMenuItem *miGoUsers;
        TMenuItem *miGoSchedule;
        TMenuItem *miGridOptions;
        TMenuItem *miGridView;
        TMenuItem *miGrouping;
        TMenuItem *miacIndicator;
        TMenuItem *miHeaders;
        TMenuItem *miSummaryFooter;
        TMenuItem *miShowGrid;
        TMenuItem *mitAutoWidth;
        TMenuItem *miAlwaysDisplayButtons;
        TMenuItem *miInvertSelected;
        TMenuItem *actNewItemRow1;
        TMenuItem *miSeparator8;
        TMenuItem *actSelectStyleSheet1;
        TMenuItem *miGridActions;
        TMenuItem *miFullExpand;
        TMenuItem *miFullCollapse;
        TMenuItem *miSeparator2;
        TMenuItem *miColumnCustomization;
        TMenuItem *miBestFitallcolumns;
		void __fastcall actGoProjectExecute(TObject *Sender);
		void __fastcall FormCreate(TObject *Sender);
		void __fastcall actShowPicturesExecute(TObject *Sender);
		void __fastcall actShowDescriptionExecute(TObject *Sender);
		void __fastcall actGroupingExecute(TObject *Sender);
		void __fastcall acIndicatorExecute(TObject *Sender);
		void __fastcall actHeaderExecute(TObject *Sender);
		void __fastcall actSummaryFooterExecute(TObject *Sender);
		void __fastcall actAutoWidthExecute(TObject *Sender);
		void __fastcall actInvertSelectedExecute(TObject *Sender);
        void __fastcall acIndicatorUpdate(TObject *Sender);
        void __fastcall actGroupingUpdate(TObject *Sender);
        void __fastcall actSummaryFooterUpdate(TObject *Sender);
        void __fastcall actHeaderUpdate(TObject *Sender);
        void __fastcall actAutoWidthUpdate(TObject *Sender);
        void __fastcall actInvertSelectedUpdate(TObject *Sender);
        void __fastcall actFullCollapseExecute(TObject *Sender);
        void __fastcall actFullExpandExecute(TObject *Sender);
        void __fastcall actShowEditButtonsExecute(TObject *Sender);
        void __fastcall actGridLinesExecute(TObject *Sender);
        void __fastcall actShowEditButtonsUpdate(TObject *Sender);
        void __fastcall actGridLinesUpdate(TObject *Sender);
        void __fastcall actAutoPreviewUpdate(TObject *Sender);
        void __fastcall actAutoPreviewExecute(TObject *Sender);
        void __fastcall actBestFitExecute(TObject *Sender);
        void __fastcall actColumnsCustomizeExecute(TObject *Sender);
        void __fastcall actShowDependsOnDataExecute(TObject *Sender);
        void __fastcall actNewItemRowExecute(TObject *Sender);
        void __fastcall actNewItemRowUpdate(TObject *Sender);
        void __fastcall actSelectStyleSheetExecute(TObject *Sender);
        void __fastcall actEditorsShadowExecute(TObject *Sender);
private:
        TIssueListGridForm* FGridForm;
        TcxGridDBTableView* GetFocusedView(void);

public:
        __fastcall TIssueListMainForm(TComponent* Owner);
        __property TIssueListGridForm* GridForm = {read = FGridForm};
        __property TcxGridDBTableView* FocusedView = {read = GetFocusedView};


};
//---------------------------------------------------------------------------
extern PACKAGE TIssueListMainForm *IssueListMainForm;
//---------------------------------------------------------------------------
#endif
