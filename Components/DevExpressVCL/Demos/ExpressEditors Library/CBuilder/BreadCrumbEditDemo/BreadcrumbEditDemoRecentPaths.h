//---------------------------------------------------------------------------

#ifndef BreadcrumbEditDemoRecentPathsH
#define BreadcrumbEditDemoRecentPathsH
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <Messages.hpp>
#include <SysUtils.hpp> 
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ShlObj.hpp>
#include <ShellAPI.hpp>
#include <Menus.hpp>
#include <Buttons.hpp>
#include <Menus.hpp>
#include <ImgList.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <ActnList.hpp>
#include <Buttons.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>

#include "cxGraphics.hpp"
#include "cxControls.hpp"
#include "cxLookAndFeels.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "dxBreadcrumbEdit.hpp"
#include "cxTreeView.hpp"
#include "cxListView.hpp"
#include "cxSplitter.hpp"
#include "dxGDIPlusClasses.hpp"
#include "cxButtons.hpp"
#include "cxListBox.hpp"
#include "dxBevel.hpp"
#include "cxImageComboBox.hpp"
#include "cxLabel.hpp"
#include "cxDropDownEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"

//---------------------------------------------------------------------------
class TdxBreadcrumbEditDemoRecentPathsForm : public TForm
{
__published:  // IDE-managed Components
    TcxButton *btnAdd;
    TcxButton *btnCancel;
    TcxButton *btnDelete;
    TcxButton *btnOk;
    TcxButton *btnReplace;
    TdxBevel *bvSeparator;
    TcxImageComboBox *cbImage;
    TcxLabel *lbRecentPath;
    TcxListView *lvPaths;
    TcxTextEdit *tePath;
    void __fastcall btnAddClick(TObject *Sender);
	void __fastcall btnDeleteClick(TObject *Sender);
    void __fastcall btnReplaceClick(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall lvPathsSelectItem(TObject *Sender, TListItem *Item, Boolean Selected);
    void __fastcall tePathPropertiesChange(TObject *Sender);
private:
    void __fastcall InitializeImageComboBox();
    void __fastcall UpdateControlsState();
public:   // User declarations
  __fastcall TdxBreadcrumbEditDemoRecentPathsForm(TComponent* Owner);
    void __fastcall LoadPaths(TdxBreadcrumbEditRecentPaths *APaths);
    void __fastcall SavePaths(TdxBreadcrumbEditRecentPaths *APaths);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxBreadcrumbEditDemoRecentPathsForm *dxBreadcrumbEditDemoRecentPathsForm;
//---------------------------------------------------------------------------
#endif

