//---------------------------------------------------------------------------

#ifndef SummaryFooterDemoEditSummaryH
#define SummaryFooterDemoEditSummaryH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxGridDBTableView.hpp"
#include "cxListBox.hpp"
#include <ExtCtrls.hpp>
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeels.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TSummaryFooterDemoEditSummaryForm : public TForm
{
__published:	// IDE-managed Components
	TcxLabel *Label1;
	TcxLabel *Label2;
	TcxLabel *Label3;
        TBevel *Bevel1;
        TcxButton *btnAdd;
        TcxButton *btnExit;
        TcxButton *btnDelete;
        TcxComboBox *cbCalculatedColumn;
        TcxComboBox *cbFooterSummaryColumn;
        TcxImageComboBox *cbSummaryKind;
		TcxGroupBox *gbSummaries;
		TcxListBox *lbSummaries;
		void __fastcall btnAddClick(TObject *Sender);
		void __fastcall FormCreate(TObject *Sender);
		void __fastcall FormShow(TObject *Sender);
		void __fastcall cbCalculatedColumnPropertiesChange(TObject *Sender);
		void __fastcall cbFooterSummaryColumnPropertiesChange(TObject &Sender);
		void __fastcall btnDeleteClick(TObject *Sender);
private:	// User declarations
		void AddSummaryKind(TcxSummaryKind AKind);
        void SetItemProperties(AnsiString ADescription, int AImageIndex,
          int AValue, TcxImageComboBoxItem* AItem);
        void UpdateSummaryKinds(TField* AField = NULL);
        void UpdateSummaryList(bool AFirstItemActive);
public:		// User declarations
        __fastcall TSummaryFooterDemoEditSummaryForm(TComponent* Owner);
        inline TcxDataFooterSummaryItems*  GetFooterSummaryItems();
        inline TcxGridDBTableView* GetFocusedView();
};
//---------------------------------------------------------------------------
typedef Set<TFieldType, ftUnknown, ftDataSet> TcxFieldTypes;
//---------------------------------------------------------------------------
extern PACKAGE TSummaryFooterDemoEditSummaryForm *SummaryFooterDemoEditSummaryForm;
//---------------------------------------------------------------------------
#endif
