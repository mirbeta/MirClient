//---------------------------------------------------------------------------

#ifndef IssueListStylesH
#define IssueListStylesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxGrid.hpp"
#include <Dialogs.hpp>
#include "IssueListStyleData.h"
#include "cxListBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeels.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TIssueListStylesForm : public TForm
{
__published:	// IDE-managed Components
        TcxListBox *lbPredefinedStyleSheets;
        TcxButton *btnEdit;
        TcxButton *cxButton1;
        TcxButton *cxButton2;
        TcxButton *btnClear;
        TOpenDialog *OpenDialog;
        TSaveDialog *SaveDialog;
        void __fastcall lbPredefinedStyleSheetsClick(TObject *Sender);
        void __fastcall btnClearClick(TObject *Sender);
        void __fastcall btnEditClick(TObject *Sender);
        void __fastcall cxButton1Click(TObject *Sender);
        void __fastcall cxButton2Click(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
        TdmStyles* FStyleData;
        void __fastcall PopulateStylesList();
        void __fastcall LoadStyleSheets(const TFileName AFileName);
        void __fastcall SaveStyleSheets(const TFileName AFileName);
        void __fastcall UpdateStyleAndController();
        void __fastcall SetStyleSheet();
        void __fastcall ResetStyleSheet();
        void __fastcall SelectStyleFromList();
        void __fastcall ResetStyleAndController();
public:		// User declarations
        TcxGridTableViewStyleSheet* SelectedStyleSheet;
        __fastcall TIssueListStylesForm(TComponent* Owner);

        __property TdmStyles* StyleData = { read = FStyleData };

};
//---------------------------------------------------------------------------
extern PACKAGE TIssueListStylesForm *IssueListStylesForm;
//---------------------------------------------------------------------------
#endif
