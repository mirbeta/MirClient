//---------------------------------------------------------------------------

#ifndef BreadcrumbEditDemoMainH
#define BreadcrumbEditDemoMainH
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
#include "cxSplitter.hpp"
#include "dxGDIPlusClasses.hpp"
#include "cxButtons.hpp"
#include "cxListBox.hpp"
#include "BaseForm.h"
#include "cxCheckBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"

//---------------------------------------------------------------------------
class TdxBreadcrumbEditDemoForm : public TfmBaseForm
{
__published:  // IDE-managed Components
    TdxBreadcrumbEdit *beNavigation;
    TcxButton *btnProgressStart;
    TcxButton *btnProgressStop;
    TcxComboBox *cbCancelEffect;
    TcxGroupBox *gbProgressBarOptions;
    TcxImageList *cxImageList1;
    TcxLabel *lbCancelEffect;
    TTimer *tmProgress;
    TcxTreeView *tvTree;
    TcxGroupBox *gbPathEditorOptions;
    TcxCheckBox *cbpeAutoComplete;
    TcxCheckBox *cbpeEnabled;
    TcxCheckBox *cbpeReadOnly;
    TcxGroupBox *gbpeRecents;
    TcxCheckBox *cbpeRecentsAutoPopulate;
    TcxButton *btnEditRecentPath;
    TcxGroupBox *gbMain;
    TBevel *bvSpacerLeft;
    TBevel *bvSpacerBottom;
    TBevel *bvSpacerTop;
    TBevel *bvSpacerRight;
    TBevel *Bevel1;
    void __fastcall beNavigationPathSelected(TObject *Sender);
    void __fastcall beNavigationPopulateChildren(TObject *Sender, TdxBreadcrumbEditNode *ANode);
    void __fastcall btnProgressStartClick(TObject *Sender);
    void __fastcall btnProgressStopClick(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall PathEditorOptionsChanged(TObject *Sender);
    void __fastcall ProgressBarPropertiesChange(TObject *Sender);
    void __fastcall tmProgressTimer(TObject *Sender);
    void __fastcall tvTreeChange(TObject *Sender, TTreeNode *Node);
    void __fastcall btnEditRecentPathClick(TObject *Sender);
private:
	Boolean FInitializingControls;
	String GetPath(TTreeNode *ATreeNode);
    void __fastcall EnableControls(TWinControl *AContainer, TcxCheckBox *ASender);
    void __fastcall InitializeControls();
    void __fastcall SynchronizeNodes(TdxBreadcrumbEditNode *ANode, TTreeNode *ATreeNode);
public:   // User declarations
  __fastcall TdxBreadcrumbEditDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxBreadcrumbEditDemoForm *dxBreadcrumbEditDemoForm;
//---------------------------------------------------------------------------
#endif

