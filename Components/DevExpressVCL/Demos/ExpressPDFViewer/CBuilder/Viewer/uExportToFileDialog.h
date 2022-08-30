//---------------------------------------------------------------------------

#ifndef uExportToFileDialogH
#define uExportToFileDialogH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxCheckBox.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include "dxLayoutLookAndFeels.hpp"
//---------------------------------------------------------------------------
class TfrmExportToFileDialog : public TForm
{
__published:	// IDE-managed Components
	TdxLayoutControl *dxLayoutControl1;
	TButton *btnCancel;
	TcxCheckBox *cbOpenAfterExport;
	TcxSpinEdit *sePageZoom;
	TButton *btnOk;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutItem *dxLayoutItem5;
	TdxLayoutAutoCreatedGroup *dxLayoutAutoCreatedGroup1;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutItem *dxLayoutItem4;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutSkinLookAndFeel *dxLayoutSkinLookAndFeel1;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmExportToFileDialog(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmExportToFileDialog *frmExportToFileDialog;
//---------------------------------------------------------------------------
#endif
